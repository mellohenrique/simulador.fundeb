#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com código ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param codigo coluna numerica com o codigo do estado
#' @param etapa_ensino coluna de fator ou caractere com a etapa de ensino
#' @param var_alunos coluna numerica com o numero de alunos
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)

pondera_geral <- function(base_alunos, ponderador, base_socioeconomica, condicao_rede = TRUE, condicao_etapa = TRUE, condicao_socio = FALSE){
  base_alunos %>%
    {ifelse(condicao_rede, pondera_alunos_rede(., ...), . )} %>%
    {ifelse(condicao_etapa, pondera_alunos_etapa(., ponderador, ...), . )}  %>%
    {ifelse(condicao_socio, pondera_socioeconomico(., ...), . )}
}
