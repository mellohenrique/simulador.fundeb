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

simular_modelo_hibrido <- function(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal_fundeb = 0.1, auxilio_federal_vaa = 0.05, var_fundo = fundeb, var_alunos = alunos, ...) {
  dados <- simular_modelo_fundeb(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = auxilio_federal_fundeb, ...)

  aporte_federal <-
    auxilio_federal_vaa * calcula_fundo_total(dados, {{var_fundo}})
}
