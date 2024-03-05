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

simula_fundeb <- function(dados_alunos, dados_complementar, dados_peso, teto = 1.05, chao = .95, complementacao_vaaf, complementacao_vaat, complementacao_vaar){


  # Checando dados ----

  # Agregacoes iniciais ----
  ## Pondera aluno por peso das etapas  ----
  df_alunos = pondera_alunos_etapa(dados_alunos = dados_alunos, dados_peso = dados_peso)

  ## Reescala vetor socioeconomico ----
  dados_complementar$nse = reescala_vetor(dados_complementar$nse, teto = teto, chao = chao)

  ## Pondera alunos por nivel socioeconomico ----
  df_entes = pondera_alunos_sociofiscal(
    dados_alunos = df_alunos,
    dados_complementar = dados_complementar)

  ## Gera fundo estadual ----
  df_estados = gera_fundo_estadual(df_entes)

  # Etapa 1 VAAF ----
  ## Equalizacao VAAF ----
  df_fundo_estadual = equaliza_fundo(df_estados, complementacao_uniao = complementacao_vaaf, var_ordem = 'vaaf_estado_inicial', var_alunos = 'alunos_estado_vaaf', var_recursos = 'recursos_estado_vaaf', identificador = 'uf', entes_excluidos = NULL)


  ## Unindo bases ----
  df_entes = une_vaaf(df_entes, df_estados, df_fundo_estadual)

  ## Calculando medidas necessarias ----
  df_entes$vaat_pre = df_entes$recursos_vaat / df_entes$alunos_vaat

  # Etapa 2 ----
  fundo_vaat =  equaliza_fundo(df_entes, complementacao_uniao = complementacao_vaat, var_ordem = 'vaat_pre', var_alunos = 'alunos_vaat', var_recursos = 'recursos_vaat', identificador = 'ibge', entes_excluidos = df_entes$inabilitados_vaat)

  ## Unindo bases  ----
  df_entes = une_vaat(df_entes, fundo_vaat)

  ## Etapa 3 VAAR ----
  df_entes$recursos_vaar = df_entes$peso_vaar * complementacao_vaar

  # Gera colunas de interesse
  df_entes$complemento_vaaf = df_entes$recursos_vaaf_final - df_entes$recursos_vaaf
  df_entes$complemento_vaat = df_entes$recursos_vaat_final - df_entes$recursos_vaat
  df_entes$complemento_uniao = df_entes$recursos_vaar + df_entes$complemento_vaat + df_entes$complemento_vaaf
  df_entes$recursos_fundeb = df_entes$recursos_vaaf + df_entes$complemento_uniao

  # Retorno
  return(df_entes)
}
