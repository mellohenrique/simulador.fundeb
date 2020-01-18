#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com codigo ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param base_socioeconomica data.frame com  o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param base_financas data.frame com o codigo do ibge e os valores do fundeb e das demais receitas
#' @param auxilio_federal percentual do fundo que a União complementará
#' @param var_fundo parametro com o nome da variavel do fundo a ser considerado
#' @param var_alunos parametro com o nome da variavel alunos a ser considerada
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

modelo_fundeb <- function(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = 0.1, var_fundo = fundeb, var_alunos = alunos, ...) {
  dados <-
    simulador.fundeb:::pondera_geral(base_alunos, ponderador, base_socioeconomica, ...) %>%
    dplyr::left_join(base_financas)

  aporte_federal <-
    auxilio_federal * simulador.fundeb:::calcula_fundo_total(dados, {{var_fundo}})

  dados_modelo <-
    simulador.fundeb:::calcula_fundo_estadual(dados, var_fundo = {{var_fundo}}) %>%
    dplyr::left_join(simulador.fundeb:::calcula_alunos_estadual(dados, var_alunos = {{var_alunos}}), by = "codigo_estado") %>%
    dplyr::mutate(vaa = fundo_estadual / alunos_estado) %>%
    dplyr::arrange(vaa) %>%
    dplyr::mutate(
      cumulativo_alunos = cumsum(alunos_estado),
      gasto_necessario =  vaa * cumulativo_alunos,
      gasto_realizado =  cumsum(vaa * alunos_estado),
      total_equalizacao = gasto_necessario - gasto_realizado
    )
  dados_modelo <- dados_modelo %>%
    dplyr::filter(total_equalizacao < aporte_federal) %>%
    dplyr::mutate(vaa = (sum(fundo_estadual) + aporte_federal) / sum(alunos_estado)) %>%
    dplyr::bind_rows(dados_modelo %>% filter(total_equalizacao > aporte_federal)) %>%
    dplyr::mutate(auxilio_federal = total_equalizacao < aporte_federal) %>%
    dplyr::select(codigo_estado, vaa_fundo = vaa, fundo_estadual, auxilio_federal)

  dados %>%
    dplyr::mutate(codigo_estado = substring(ibge, 1, 2) %>% as.numeric()) %>%
    dplyr::left_join(dados_modelo) %>%
    dplyr::left_join(base_financas) %>%
    dplyr::mutate(vaa_final = vaa_fundo + demais_receitas / {{var_alunos}})

}
