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
#' @param var_fundo parametro com o nome da variavel do fundo a ser considerado
#' @param var_alunos parametro com o nome da variavel alunos a ser considerada
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

simular_modelo_hibrido <- function(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = 0.1, auxilio_federal_vaa = 0.05, var_fundo = fundeb, var_alunos = alunos, equalizacao_socio = FALSE, distribuicao_fundo_estadual_socio = FALSE, ...){

  dados <- simular_modelo_fundeb(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = auxilio_federal, ...)

  aporte_federal <-
    auxilio_federal_vaa * calcula_fundo_total(dados, {{var_fundo}})

  if(distribuicao_fundo_estadual_socio) {
    financiamento <- equaliza_modelo(dados, recursos_totais, aporte = aporte_federal, var_alunos = var_alunos_socio, var_vaa = vaa_final, codigo = ibge)
  } else {
    financiamento <- equaliza_modelo(dados, recursos_totais, aporte = aporte_federal,var_vaa = vaa_final, var_alunos = alunos, codigo = ibge)
  }

  dados %>%
    dplyr::left_join(financiamento %>% rename(fundo_etapa_vaa = fundo_equalizado)) %>%
    dplyr::rename(vaa_intermediario = vaa_final) %>%
    dplyr::mutate(
      vaa_final = dplyr::case_when(
        distribuicao_fundo_estadual_socio ~ recursos_totais / alunos_socioeco,
        TRUE ~ recursos_totais / alunos))
}
