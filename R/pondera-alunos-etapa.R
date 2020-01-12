#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com código ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)


pondera_alunos_etapa <- function(base_alunos, ponderador, codigo = ibge, etapa_ensino = etapa, var_alunos = alunos){
  base_alunos %>%
    select({{codigo}}, {{etapa_ensino}}, {{var_alunos}}) %>%
    left_join(ponderador) %>%
    group_by(ibge) %>%
    summarise(alunos_ponderado = sum({{var_alunos}} * peso))
}
