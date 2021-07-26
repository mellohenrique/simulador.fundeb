#' @title Gera Fundo estadual
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams pondera_alunos_etapa
#' @param dados_alunos data.frame ou data.table com os dados de alunos ponderados em formato tidy
#' @param dados_complementar data.frame ou data.table com os dados de receitas dos entes
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto
#'
#' @import data.table
#'
#'

gera_fundo_estadual <- function(dados_alunos, dados_complementar, produto_dt = TRUE ){
  # Binding variables para NULL
  alunos_ponderados = . = alunos = uf = estimativa_de_receitas = receitas = vaa = NULL

  dados_alunos = checa_transforma_dt(dados_alunos)

  dados_complementar = checa_transforma_dt(dados_complementar)

  alunos_estadual = dados_alunos[, .(alunos_ponderados = sum(alunos_ponderados),
                   alunos = sum(alunos)),
               by = uf]

  dados_financas = dados_complementar[, .(fundeb = sum(fundeb)),
                                  by = uf]

  fundo_estadual = alunos_estadual[dados_financas, fundeb := fundeb, on = .(uf)]

  fundo_estadual[, vaa := fundeb/alunos_ponderados]

  retorna_dt_df(fundo_estadual, produto_dt)
}
