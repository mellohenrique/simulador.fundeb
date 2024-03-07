#' @title Gera Fundo estadual
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame com os dados de recursos e alunos dos fundos estaduais

gera_fundo_estadual <- function(dados_entes){

  # Agrega dados por estado
  df = aggregate(list(alunos_estado_vaaf = dados_entes$alunos_vaaf,
                      recursos_estado_vaaf = dados_entes$recursos_vaaf),
                 by = list(uf = dados_entes$uf),
                 FUN=sum)

  # Calcula vaaf pre complementacao estadual
  df$vaaf_estado_inicial = df$recursos_estado_vaaf / df$alunos_estado_vaaf

  # Retorna dados dos fundos estaduais
  return(df)
}
