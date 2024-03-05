#' @title Simula Fundeb
#'
#' @description Simula a distribuição dos fundos do Fundeb e as etapas de equalização
#'
#' @param dados_alunos data.frame com os dados de alunos
#' @param dados_complementar data.frame com os dados de peso das etapas
#' @param dados_peso data.frame com os dados de peso das etapas
#' @param complementacao_vaaf valor numerico com o montante a ser complementado pela uniao na etapa VAAF
#' @param complementacao_vaat valor numerico com o montante a ser complementado pela uniao na etapa VAAT
#' @param teto valor maximo do nivel socioeconomico na ponderacao de alunos
#' @param chao valor minimo do nivel socioeconomico na ponderacao de alunos
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE
#'
#' @import data.table
#'
#' @export

simula_fundeb <- function(dados_alunos, dados_complementar, dados_peso, peso_vaar = NULL, entes_excluidos_vaat = NULL, teto = 1.05, chao = .95, complementacao_vaaf, complementacao_vaat, produto_dt = TRUE){


  # Checando dados ----

  # Agregacoes iniciais ----
  ## Pondera aluno por peso das etapas  ----
  df_alunos = pondera_alunos_etapa(dados_alunos = dados_alunos, peso_etapas = peso_etapas)

  ## Reescala vetor socioeconomico ----
  dados_complementar$nse = reescala_vetor(dados_complementar$nse, teto = teto, piso = piso)

  ## Pondera alunos por nivel socioeconomico ----
  df_entes = pondera_alunos_sociofiscal(
    dados_alunos = df_alunos,
    dados_complementar = dados_complementar)

  ## Gera fundo estadual ----
  df_estados = gera_fundo_estadual(df_entes)

  # Etapa 1 VAAF ----
  ## Equalizacao VAAF ----
  df_fundo_estadual = equaliza_fundo(df_estados, complemento = complemento_vaaf, var_ordem = 'vaaf_estado_inicial', var_alunos = 'alunos_estado_vaaf', var_recursos = 'recursos_estado_vaaf', entes_excluidos = NULL)


  ## Unindo bases ----
  df_entes = une_vaaf(df_entes, df_estados, df_fundo_estadual)

  ## Calculando medidas necessarias ----


  ## Calculando VAAT ----
  entes[, fundo_base := alunos_vaaf * sum(fundeb_vaaf)/sum(alunos_vaaf), by = uf]
  entes[, vaat_pre := fundeb_vaat/alunos_vaat]

  # Etapa 2 ----
  fundo_vaat =  equaliza_fundo(df_entes, complemento = complemento_vaat, var_ordem = 'vaat_inicial', var_alunos = 'alunos_vaaft', var_recursos = 'recursos_vaat', entes_excluidos = df_entes$inabilitados_vaat)

    equaliza_fundo(entes,
                   var_ordem = "vaat_pre",
                   var_alunos = "alunos_vaat",
                   var_receitas = "fundeb_vaat",
                   entes_excluidos = entes_excluidos_vaat,
                   complementacao = complementacao_vaat)

  ##

  ## Unindo bases  ----
  df_entes = une_vaat(entes, fundo_ente)

  ## Etapa 3 VAAR ----
  # Retorno
  return(retorna_dt_df(entes, produto_dt = produto_dt))
}
