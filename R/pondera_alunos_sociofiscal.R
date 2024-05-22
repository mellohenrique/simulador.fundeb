#' @title Pondera matriculas por nivelsocioeconomico
#'
#' @description Une os dados de matriculas com os dados complementares e atribui o peso do fator socioeconomico
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame com os dados de matriculas considerando o nivel socioeconomico


pondera_matriculas_sociofiscal <- function(dados_matriculas, dados_complementar){

  # Une dados de matriculas com dados complementares
  df = merge(dados_matriculas, dados_complementar, by = 'ibge')

  # Multiplica pelo fator socioeconomico
  df$matriculas_vaaf = df$matriculas_vaaf * df$nse
  df$matriculas_vaat = df$matriculas_vaat * df$nse

  # Multiplica pelo fator fiscal
  df$matriculas_vaaf = df$matriculas_vaaf * df$nf

  # Retorna resultado
  return(df)
}
