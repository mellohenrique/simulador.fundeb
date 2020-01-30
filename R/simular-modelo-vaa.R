#' @title Simulação do modelo VAA de financiamento da educação
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

simular_modelo_vaa <- function(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = 0.1, var_fundo = fundeb, var_alunos = alunos, distribuicao_fundo_estadual_socio = FALSE, equalizacao_vaa_socio = FALSE,...){
  dados <- pondera_geral(base_alunos, ponderador_alunos, base_socioeconomica, base_financas)
  dados_estaduais <- gera_dados_estaduais(dados)
  aporte_federal <- auxilio_federal * calcula_fundo_total(dados)
  financiamento_estado <- dados_estaduais %>%
      dplyr::select(fundo_estadual, codigo_estado)

    dados <- dados %>%
      dplyr::left_join(financiamento_estado) %>%
      dplyr::group_by(codigo_estado) %>%
      dplyr::mutate(
        fundeb_recebido = dplyr::case_when(
          distribuicao_fundo_estadual_socio ~ fundo_estadual * (alunos_socioeco / sum(alunos_socioeco)),
          TRUE ~ fundo_estadual * (alunos / sum(alunos)))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        recursos_totais = fundeb_recebido + demais_receitas,
        vaa_final = dplyr::case_when(
          distribuicao_fundo_estadual_socio ~ recursos_totais / alunos_socioeco,
          TRUE ~ recursos_totais / alunos))

    if(equalizacao_vaa_socio){
      fundo_equalizado <- equaliza_modelo(fundo = recursos_totais, aporte = aporte_federal, var_alunos = alunos_socio, codigo = ibge) %>%
        rename(recursos_complementados = fundo_equalizado)
      dados <- left_join(dados, fundo_equalizado) %>%
        mutate(vaa_complementado = recursos_+vaa_complementados / alunos_socio)
    } else {
      fundo_equalizado <- equaliza_modelo(fundo = recursos_totais, aporte = aporte_federal, var_alunos = alunos, codigo = ibge) %>%
        rename(recursos_complementados = fundo_equalizado)
      dados <- left_join(dados, fundo_equalizado) %>%
        mutate(vaa_complementado = recursos_+vaa_complementados / alunos)
    }
    dados
}
