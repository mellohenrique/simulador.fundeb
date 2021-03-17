#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams pondera_alunos_etapa
#' @param dados_alunos data.frame ou data.table com os dados de alunos ponderados em formato tidy
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#'
#'


gera_fundo_estadual <- function(dados_alunos, produto_dt = TRUE ){
  dados_alunos = checa_transforma_dt(dados_alunos)

  fundo_estadual = dados_alunos[, .(alunos_ponderados = sum(alunos_ponderados),
                   alunos = sum(alunos),
                   receitas = sum(`Estimativa de Receitas`)),
               by = UF]

  fundo_estadual[, vaa := receitas/alunos_ponderados]

  retorna_dt_df(fundo_estadual, produto_dt)
}
