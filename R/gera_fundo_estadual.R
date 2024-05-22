#' @title Gera Fundo estadual
#'
#' @description Pondera dados de matriculas do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @param dados_entes data.frame com os dados dos entes federativos considerados na funcao
#'
#' @return Um data.frame com os dados de recursos e matriculas dos fundos estaduais
#'
#' @importFrom stats aggregate

gera_fundo_estadual <- function(dados_entes){

  # Agrega dados por estado
  df = stats::aggregate(list(matriculas_estado_vaaf = dados_entes$matriculas_vaaf,
                      recursos_estado_vaaf = dados_entes$recursos_vaaf),
                 by = list(uf = dados_entes$uf),
                 FUN=sum)

  # Calcula vaaf pre complementacao estadual
  df$vaaf_estado_inicial = df$recursos_estado_vaaf / df$matriculas_estado_vaaf

  # Retorna dados dos fundos estaduais
  return(df)
}
