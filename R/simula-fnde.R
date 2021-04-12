#' @title Simula FNDE
#'
#' @description Simula a distribuição dos fundos do FNDE e as etapas de equalização
#'
#' @inheritParams pondera_alunos_etapa
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE
#'
#' @import data.table
#'
#' @export

simula_fundeb <- function(dados_fnde, peso_etapas = peso, aporte, produto_dt = TRUE){
  fnde = checa_transforma_dt(dados_fnde)

  # Tabelas iniciais
  alunos <- pondera_alunos_etapa(fnde, peso_etapas = peso_etapas)
  financas <- financas_fnde(fnde)
  estados <- gera_fundo_estadual(alunos, financas)

  # Etapa 1
  ## Equalziacao VAAF
  fundo_estadual_equalizado <-
    equaliza_fundo(estados,
                   var_ordem = "vaa",
                   var_alunos = "alunos_ponderados",
                   var_receitas = "receitas",
                   aporte = aporte)

  vaaf= alunos[financas, receitas := estimativa_de_receitas, on = .(uf,  municipio)]
  vaaf[fundo_estadual_equalizado, fundo_estadual := receitas_etapa, on = .(uf)]
  vaaf[, vaaf := fundo_estadual/sum(alunos_ponderados), by = .(uf)]

  retorna_dt_df(vaaf, produto_dt = produto_dt)
}
