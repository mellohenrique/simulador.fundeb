#' @title Calcula valor do fundo total
#'
#' @description Recebe uma base com código ibge, alunos e
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param codigo coluna numerica com o codigo do estado
#' @param etapa_ensino coluna de fator ou caractere com a etapa de ensino
#' @param var_alunos coluna numerica com o numero de alunos
#' @param busca_creche caractere com expressao que identifica a etapa referente a creche
#' @param busca_em caractere com expressao que identifica a etapa referente ao ensino medio
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#'
pondera_alunos_rede <- function(base_alunos, codigo = ibge, etapa_ensino = etapa, var_alunos = alunos, busca_creche = "Creche|Pr\u00E9-escola", busca_em = "EM"){
  base_alunos %>%
    mutate(alunos = dplyr::case_when(
      {{codigo}} == 53 ~ {{var_alunos}},
      {{codigo}} < 100 & stringr::str_detect({{etapa_ensino}}, busca_creche) ~ 0,
      {{codigo}} > 100 & stringr::str_detect({{etapa_ensino}}, busca_em) ~ 0,
      TRUE ~ alunos
    ))
}

