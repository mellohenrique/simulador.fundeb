#' @title Simula Fundeb
#'
#' @description Simula a distribuição dos fundos do Fundeb e as etapas de equalização
#'
#' @param dados_alunos data.frame com os dados de alunos
#' @param dados_complementar data.frame com os dados de peso das etapas
#' @param peso_etapas data.frame com os dados de peso das etapas
#' @param complementacao_vaaf valor numerico com o montante a ser complementado pela uniao na etapa VAAF
#' @param complementacao_vaat valor numerico com o montante a ser complementado pela uniao na etapa VAAT
#' @param difere_etapas_complementacao variavel em caractere com as opcoes de diferenciacao dos pesos das etapas. Caso escolha-se vaaf_vaat a etapa vaaf e a etapa vaat consideram pesos diferentes, caso escolha-se mesmos pesos as etapas tem os mesmos pesos
#' @param produto_dt valor logico que determina se o resultado sera em data.table (caso produto_dt = TRUE) ou data.frame (caso produto_dt = FALSE)
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE
#'
#' @import data.table
#'
#' @export

simula_fundeb <- function(dados_alunos, dados_complementar, peso_etapas = peso, difere_etapas_complementacao = c("vaaf_vaat", "mesmos_pesos"), entes_excluidos_vaat = NULL, complementacao_vaaf, complementacao_vaat, produto_dt = TRUE){

  # Checa o argumento de diferenciacao de etapas
  difere_etapas_complementacao = match.arg(difere_etapas_complementacao)

  # Checando dados
  complementar = checa_transforma_dt(dados_complementar)
  alunos = checa_transforma_dt(dados_alunos)
  peso_etapas = checa_transforma_dt(peso_etapas)

  # Binding variables para NULL
  . = uf = ibge = fundo_vaaf_extra = fundo_vaaf = impostos_extra = vaaf_extra = peso = alunos_ponderados = estimativa_de_receitas = receitas = NULL

  # Tabelas iniciais
  alunos = pondera_alunos_etapa(alunos, peso_etapas = peso_etapas, difere_etapas_complementacao = difere_etapas_complementacao)

  entes = pondera_alunos_sociofiscal(
    dados_alunos = alunos,
    dados_complementar = dados_complementar)

  estados = gera_fundo_estadual(entes, complementar)

    # Etapa 1 ####
  ## Equalizacao VAAF
  fundo_estadual_equalizado =
    equaliza_fundo(estados,
                   var_ordem = "vaa",
                   var_alunos = "alunos_vaaf",
                   var_receitas = "fundeb_estado",
                   complementacao = complementacao_vaaf)

  ## Unindo bases
  entes = une_vaaf(entes, fundo_estadual_equalizado)

  # Calculando medidas necessarias
  # Calculando VAAT
  entes[, fundo_base := alunos_vaaf * sum(fundeb_vaaf)/sum(alunos_vaaf), by = uf]
  entes[, vaat_pre := fundeb_vaat/alunos_vaat]

  # Etapa 2
  fundo_ente =
    equaliza_fundo(entes,
                   var_ordem = "vaat_pre",
                   var_alunos = "alunos_vaat",
                   var_receitas = "fundeb_vaat",
                   entes_excluidos = entes_excluidos_vaat,
                   complementacao = complementacao_vaat)

  ## Unindo bases
  entes = une_vaat(entes, fundo_ente)


  # Retorno
  return(retorna_dt_df(entes, produto_dt = produto_dt))
}
