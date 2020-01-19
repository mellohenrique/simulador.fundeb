#' @title Simulação do modelo Fundeb de financiamento da educação
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação
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

simular_modelo_fundeb <- function(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = 0.1, var_fundo = fundeb, var_alunos = alunos, ...) {
  dados <-
    pondera_geral(base_alunos, ponderador, base_socioeconomica, base_financas, ...)

  aporte_federal <-
    auxilio_federal * calcula_fundo_total(dados, {{var_fundo}})

  dados_modelo <-
    calcula_fundo_estadual(dados, var_fundo = {{var_fundo}}) %>%
    dplyr::left_join(calcula_alunos_estadual(dados, var_alunos = {{var_alunos}}), by = "codigo_estado") %>%
    dplyr::mutate(vaa = fundo_estadual / alunos_estado) %>%
    prepara_equalizacao()

  dados_modelo <- dados_modelo %>%
    equaliza_modelo(aporte = aporte_federal, codigo = codigo_estado) %>%
    dplyr::rename(vaa_fundeb = vaa)

  dados %>%
    dplyr::mutate(codigo_estado = substring(ibge, 1, 2) %>% as.numeric()) %>%
    dplyr::left_join(dados_modelo) %>%
    dplyr::left_join(base_financas) %>%
    dplyr::mutate(vaa_final = vaa_fundeb + demais_receitas / {{var_alunos}})

}
