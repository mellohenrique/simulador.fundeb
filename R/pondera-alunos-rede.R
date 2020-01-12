#' @title Calcula valor do fundo total
#'
#' @description Recebe uma base com código ibge, alunos e
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::pondera_alunos_rede(test)
#'
pondera_alunos_rede <- function(base_alunos, codigo = ibge, etapa_ensino = etapa, var_alunos = alunos, busca_creche = "Creche|Pr\u00E9-escola", busca_em = "EM"){
  base_alunos %>%
    mutate(alunos_ponderados = dplyr::case_when(
      {{codigo}} == 53 ~ {{var_alunos}},
      {{codigo}} < 100 & stringr::str_detect({{etapa_ensino}}, busca_creche) ~ 0,
      {{codigo}} > 100 & stringr::str_detect({{etapa_ensino}}, busca_em) ~ 0,
      TRUE ~ alunos
    ))
}

