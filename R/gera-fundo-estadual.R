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

gera_fundo_estadual <- function(dados_alunos, dados_financas, produto_dt = TRUE ){
  dados_alunos = checa_transforma_dt(dados_alunos)

  dados_financas = checa_transforma_dt(dados_financas)

  alunos_estadual = dados_alunos[, .(alunos_ponderados = sum(alunos_ponderados),
                   alunos = sum(alunos)),
               by = uf]

  dados_financas = dados_financas[, .(receitas = sum(estimativa_de_receitas)),
                                  by = uf]

  fundo_estadual = alunos_estadual[dados_financas, receitas := receitas, on = .(uf)]

  fundo_estadual[, vaa := receitas/alunos_ponderados]

  retorna_dt_df(fundo_estadual, produto_dt)
}
