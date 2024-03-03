#' @title Pondera alunos por nivelsocioeconomico
#'
#' @description Une os dados de alunos com os dados complementares e atribui o peso do fator socioeconomico
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame com os dados de alunos considerando o nivel socioeconomico
#'
#' @import data.table
#'


pondera_alunos_sociofiscal <- function(dados_alunos, dados_complementar){

  # Une dados de alunos com dados complementares
  df_geral = merge(dados_alunos, dados_complementar, by = 'ibge')

  # Multiplica pelo fator socioeconomico
  df_geral$alunos_vaaf = df_geral$alunos_vaaf * df_geral$nse
  df_geral$alunos_vaat = df_geral$alunos_vaat * df_geral$nse

  # Retorna resultado
  return(df_geral)
}
