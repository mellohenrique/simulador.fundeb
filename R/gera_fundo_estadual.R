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

gera_fundo_estadual <- function(dados_alunos, dados_complementar, produto_dt = TRUE ){
  # Binding variables para NULL
  alunos_ponderados = . = alunos = uf = estimativa_de_receitas = receitas = vaa = NULL

  dados_alunos = checa_transforma_dt(dados_alunos)

  dados_complementar = checa_transforma_dt(dados_complementar)

  alunos_estadual = dados_alunos[, .(
    alunos_vaaf = sum(alunos_vaaf),
    alunos = sum(alunos)
  ), by = uf]

  dados_financas = dados_complementar[, .(fundeb_estado = sum(fundeb_vaaf)),
                                  by = uf]

  fundo_estadual = alunos_estadual[dados_financas, fundeb_estado := fundeb_estado, on = .(uf)]

  fundo_estadual[, vaa := fundeb_estado/alunos_vaaf]

  return(retorna_dt_df(fundo_estadual, produto_dt))
}
