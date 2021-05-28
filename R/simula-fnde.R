#' @title Simula FNDE
#'
#' @description Simula a distribuição dos fundos do FNDE e as etapas de equalização
#'
#' @inheritParams pondera_alunos_etapa
#' @inheritParams pondera_alunos_sociofiscal
#' @param aporte_vaaf valor numerico com o montante a ser complementado pela uniao na etapa VAAF
#' @param aporte_vaat valor numerico com o montante a ser complementado pela uniao na etapa VAAT
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE
#'
#' @import data.table
#'
#' @export

simula_fundeb <- function(dados_fnde, dados_complementar, peso_etapas = peso, chao_socio = 1, teto_socio = 1.3, chao_fiscal = 1, teto_fiscal = 1.3, aporte_vaaf, aporte_vaat, produto_dt = TRUE){
  fnde = checa_transforma_dt(dados_fnde)

  # Tabelas iniciais
  alunos = pondera_alunos_etapa(fnde, peso_etapas = peso_etapas)
  alunos = pondera_alunos_sociofiscal(dados_alunos = alunos, dados_complementar = dados_complementar)
  financas = financas_fnde(fnde)
  estados = gera_fundo_estadual(alunos, financas)
  entes = alunos[financas, receitas := estimativa_de_receitas, on = .(uf, ibge)]

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

  ## Adicionando medida para Var
  entes[,fundo_vaaf_extra := fundo_vaaf + impostos_extra ]
  entes[,vaaf_extra := fundo_vaaf_extra/alunos_ponderados]

  # Etapa 2
  fundo_ente =
    equaliza_fundo(entes,
                   var_ordem = "vaaf_extra",
                   var_alunos = "alunos_ponderados",
                   var_receitas = "fundo_vaaf_extra",
                   aporte = aporte_vaat)

  ## Unindo bases
  entes = une_vaat(entes, fundo_ente)

  # Retorno
  return(retorna_dt_df(entes, produto_dt = produto_dt))
}
