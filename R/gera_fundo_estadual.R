#' @title Gera Fundo estadual
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto
#'
#' @import data.table
#'
#'

gera_fundo_estadual <- function(dados_entes){

  # Agrega dados por estado
  fundo_estadual = aggregate(list(alunos_vaaf = df_entes$alunos_vaaf,
                                  recursos_vaaf = df_entes$recursos_vaaf),
                             by = list(uf = df_entes$uf), FUN=sum)

  # Calcula vaaf pre complementacao estadual
  fundo_estadual$vaaf_inicial = fundo_estadual$recursos_vaaf / fundo_estadual$alunos_vaaf

  # Retorna dados dos fundos estaduais
  return(fundo_estadual)
}
