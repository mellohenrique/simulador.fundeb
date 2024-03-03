#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto
#'
#' @import data.table
#'


pondera_alunos_etapa <- function(dados_alunos, dados_peso){

  matriz_alunms = as.matrix(matriculas[order(matriculas$ibge), !names(matriculas) %in% c('ibge')])
  alunos_vaaf = as.matrix(matriculas[, pesos$etapa]) %*% pesos$peso_vaaf
  alunos_vaat = as.matrix(matriculas[, pesos$etapa]) %*% pesos$peso_vaat

  df_alunos = data.frame(ibge = matriculas$ibge[order(matriculas$ibge)],
             alunos_vaaf = alunos_vaaf,
             alunos_vaat = alunos_vaat)

  return(df_alunos)

}
