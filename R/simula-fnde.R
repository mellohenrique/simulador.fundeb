#' @title Simula FNDE
#'
#' @description Simula a distribuição dos fundos do FNDE e as etapas de equalização
#'
#' @inheritParams pondera_alunos_etapa
#' @param aporte_vaaf valor numerico com o montante a ser complementado pela uniao na etapa VAAF
#' @param aporte_vaat valor numerico com o montante a ser complementado pela uniao na etapa VAAT
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE
#'
#' @import data.table
#'
#' @export

simula_fundeb <- function(dados_fnde, peso_etapas = peso, aporte_vaaf, aporte_vaat, produto_dt = TRUE){
  fnde = checa_transforma_dt(dados_fnde)

  # Tabelas iniciais
  alunos = pondera_alunos_etapa(fnde, peso_etapas = peso_etapas)
  financas = financas_fnde(fnde)
  estados = gera_fundo_estadual(alunos, financas)
  entes = alunos[financas, receitas := estimativa_de_receitas, on = .(uf, municipio)]

  # Etapa 1 ####
  ## Equalziacao VAAF
  fundo_estadual_equalizado =
    equaliza_fundo(estados,
                   var_ordem = "vaa",
                   var_alunos = "alunos_ponderados",
                   var_receitas = "receitas",
                   aporte = aporte_vaaf)

  ## Unindo bases
  entes = une_vaaf(entes, fundo_estadual_equalizado)

  # Etapa 2
  fundo_ente =
    equaliza_fundo(entes,
                   var_ordem = "vaaf",
                   var_alunos = "alunos_ponderados",
                   var_receitas = "fundo_vaaf",
                   aporte = aporte_vaat)

  ## Unindo bases
  entes = une_vaat(entes, fundo_ente)

  # Retorno
  return(retorna_dt_df(entes, produto_dt = produto_dt))
}
