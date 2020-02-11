#' @title Simulação do modelo híbrido de financiamento da educação
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo híbrido de financiamento da educação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param base_socioeconomica data.frame com  o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param base_financas data.frame com o codigo do ibge e os valores do fundeb e das demais receitas
#' @param auxilio_federal_fundeb percentual do fundo que a União complementará segundo o modelo fundeb
#' @param auxilio_federal_vaa percentual do fundo que a União complementará segundo o modelo vaa
#' @param equalizacao_socio parametro lógico que controla se a equalização do fundo considerara o vetor de alunos ou de alunos socioeconomico
#' @param distribuicao_fundo_estadual_socio parametro logico que controla se a distribuicao do fundo estadual considerara o vetor de alunos ou de alunos socioeconomico
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

simular_modelo_hibrido <- function(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = 0.1, auxilio_federal_vaa = 0.05, equalizacao_socio = FALSE, distribuicao_fundo_estadual_socio = FALSE, ...){

  dados <- simular_modelo_fundeb(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = auxilio_federal, distribuicao_fundo_estadual_socio= distribuicao_fundo_estadual_socio, equalizacao_socio = equalizacao_socio, ...)

  aporte_federal <-
    auxilio_federal_vaa * calcula_fundo_total(dados, fundeb)

  if(distribuicao_fundo_estadual_socio) {
    financiamento <- equaliza_modelo(dados, recursos_totais, aporte = aporte_federal, var_alunos = alunos_socioeco, codigo = ibge)
  } else {
    financiamento <- equaliza_modelo(dados, recursos_totais, aporte = aporte_federal, var_alunos = alunos, codigo = ibge)
  }

  dados %>%
    dplyr::left_join(financiamento %>%
                       dplyr::rename(recursos_vat = fundo_equalizado)) %>%
    dplyr::rename(vaa_intermediario = vaa_final) %>%
    dplyr::mutate(
      vaa_final = dplyr::case_when(
        distribuicao_fundo_estadual_socio ~ recursos_vat / alunos_socioeco,
        TRUE ~ recursos_vat / alunos))
}
