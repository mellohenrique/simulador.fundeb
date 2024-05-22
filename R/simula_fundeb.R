#' @title Simula Fundeb
#'
#' @description Simula a distribuição dos fundos do Fundeb e as etapas de equalização
#'
#' @param dados_matriculas data.frame com os dados de matriculas
#' @param dados_complementar data.frame com os dados de peso das etapas
#' @param dados_peso data.frame com os dados de peso das etapas
#' @param complementacao_vaaf valor numerico com o montante a ser complementado pela uniao na etapa VAAF
#' @param complementacao_vaat valor numerico com o montante a ser complementado pela uniao na etapa VAAT
#' @param complementacao_vaar valor numerico com o montante a ser complementado pela uniao na etapa VAAR
#' @param teto valor maximo do nivel socioeconomico na ponderacao de matriculas
#' @param chao valor minimo do nivel socioeconomico na ponderacao de matriculas
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE
#'
#'
#' @examples
#'
#' data("teste_matriculas")
#' data("teste_complementar")
#' data("teste_peso")
#'
#' df_teste = simulador.fundeb::simula_fundeb(
#'   dados_matriculas = teste_matriculas,
#'   dados_complementar = teste_complementar,
#'   dados_peso = teste_peso,
#'   complementacao_vaaf = 4e5,
#'   complementacao_vaat = 1e5,
#'   complementacao_vaar = 1e5)
#'
#' head(df_teste)
#'
#' @export

simula_fundeb <- function(dados_matriculas, dados_complementar, dados_peso, teto = 1.05, chao = .95, complementacao_vaaf, complementacao_vaat, complementacao_vaar){

  # Checando dados ----

  # Gera parametros
  entes_excluidos = dados_complementar[dados_complementar[,'inabilitados_vaat'] == TRUE,]$ibge

  # Agregacoes iniciais ----
  ## Pondera aluno por peso das etapas  ----
  df_matriculas = pondera_matriculas_etapa(dados_matriculas = dados_matriculas, dados_peso = dados_peso)

  ## Reescala vetor socioeconomico ----
  dados_complementar$nse = reescala_vetor(dados_complementar$nse, teto = teto, chao = chao)

  ## Pondera matriculas por nivel socioeconomico ----
  df_entes = pondera_matriculas_sociofiscal(
    dados_matriculas = df_matriculas,
    dados_complementar = dados_complementar)

  ## Gera fundo estadual ----
  df_estados = gera_fundo_estadual(df_entes)

  # Etapa 1 VAAF ----
  ## Equalizacao VAAF ----
  df_fundo_estadual = equaliza_fundo(df_estados, complementacao_uniao = complementacao_vaaf, var_ordem = 'vaaf_estado_inicial', var_matriculas = 'matriculas_estado_vaaf', var_recursos = 'recursos_estado_vaaf', identificador = 'uf', entes_excluidos = NULL)

  ## Unindo bases ----
  df_entes = une_vaaf(df_entes, df_estados, df_fundo_estadual)

  ## Calculando medidas necessarias ----
  df_entes$vaat_pre = df_entes$recursos_vaat / df_entes$matriculas_vaat

  # Etapa 2 ----
  fundo_vaat =  equaliza_fundo(df_entes, complementacao_uniao = complementacao_vaat, var_ordem = 'vaat_pre', var_matriculas = 'matriculas_vaat', var_recursos = 'recursos_vaat', identificador = 'ibge', entes_excluidos = entes_excluidos)

  ## Unindo bases  ----
  df_entes = une_vaat(df_entes, fundo_vaat)

  ## Etapa 3 VAAR ----
  df_entes$complemento_vaar = df_entes$peso_vaar * complementacao_vaar

  # Gera colunas de interesse
  df_entes$complemento_vaaf = df_entes$recursos_vaaf_final - df_entes$recursos_vaaf
  df_entes$complemento_vaat = df_entes$recursos_vaat_final - df_entes$recursos_vaat
  df_entes$complemento_uniao = df_entes$complemento_vaar + df_entes$complemento_vaat + df_entes$complemento_vaaf
  df_entes$recursos_fundeb = df_entes$recursos_vaaf + df_entes$complemento_uniao

  # Retorno
  return(df_entes)
}
