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

  dados_alunos_sem_ibge = dados_alunos[order(dados_alunos$ibge), !names(dados_alunos) %in% c('ibge')]
  matriz_alunos = as.matrix(dados_alunos_sem_ibge[, pesos$etapa])
  alunos_vaaf = matriz_alunos %*% pesos$peso_vaaf
  alunos_vaat = matriz_alunos %*% pesos$peso_vaat

  df_alunos = data.frame(ibge = dados_alunos$ibge[order(dados_alunos$ibge)],
             alunos_vaaf = alunos_vaaf,
             alunos_vaat = alunos_vaat)

  return(df_alunos)

}
