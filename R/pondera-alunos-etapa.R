#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com codigo ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o numero de alunos ponderados por etapa para cada entidade do grupo considerado
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param codigo coluna numerica com o codigo do ente federativo ou estado
#' @param var_alunos nomeia a coluna numerica com o numero de alunos que sera utilizada
#' @param variavel_peso nomeia coluna que sera utilizada como peso da etapa de ensino
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @import data.table
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)


pondera_alunos_etapa <- function(base_alunos, ponderador, codigo = ibge, var_alunos = alunos, variavel_peso = peso){
  DT <- data.table(base_alunos)

  DT[data.table(ponderador), on = "etapa"]

  DT %>%
    group_by({{codigo}}) %>%
    summarise(alunos = sum({{var_alunos}} * {{variavel_peso}}))
}
