#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os fatores sociais e financeiros
#'
#' @import data.table
#'


pondera_alunos_sociofiscal <- function(dados_alunos, dados_complementar){

  df_geral = merge(dados_alunos, dados_complementar, by = 'ibge')

  df_geral$alunos_vaaf = df_geral$alunos_vaaf * df_geral$nse
  df_geral$alunos_vaat = df_geral$alunos_vaat * df_geral$nse

  return(df_geral)
}
