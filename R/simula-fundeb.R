#' @title Simula Fundeb
#'
#' @description Simula a distribuição dos fundos do Fundeb e as etapas de equalização
#'
#' @param dados_alunos data.frame com os dados de alunos
#' @param dados_complementar data.frame com os dados de peso das etapas
#' @param peso_etapas data.frame com os dados de peso das etapas
#' @param chao_socio valor minimo do peso da variavel social na ponderacao de alunos
#' @param teto_socio valor maximo do peso da variavel social na ponderacao de alunos
#' @param chao_fiscal valor minimo do peso da variavel fiscal na ponderacao de alunos
#' @param teto_fiscal valor maximo do peso da variavel fiscal na ponderacao de alunos
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

simula_fundeb <- function(dados_alunos, dados_complementar, peso_etapas = peso, chao_socio = 1, teto_socio = 1.3, chao_fiscal = 1, teto_fiscal = 1.3, entes_excluidos_vaaf = NULL, complementacao_vaaf, complementacao_vaat, produto_dt = TRUE){

   # Checando dados
  complementar = checa_transforma_dt(dados_complementar)
  alunos = checa_transforma_dt(dados_alunos)
  peso_etapas = checa_transforma_dt(peso_etapas)

  # Binding variables para NULL
  . = uf = ibge = fundo_vaaf_extra = fundo_vaaf = impostos_extra = vaaf_extra = peso = alunos_ponderados = estimativa_de_receitas = receitas = NULL

  # Tabelas iniciais
  alunos = pondera_alunos_etapa(alunos, peso_etapas = peso_etapas)
  entes = pondera_alunos_sociofiscal(
    dados_alunos = alunos,
    dados_complementar = dados_complementar,
    chao_socio = chao_socio,
    teto_socio = teto_socio,
    chao_fiscal = chao_fiscal,
    teto_fiscal = teto_fiscal)

  estados = gera_fundo_estadual(entes, complementar)

  # Etapa 1 ####
  ## Equalziacao VAAF
  fundo_estadual_equalizado =
    equaliza_fundo(estados,
                   var_ordem = "vaa",
                   var_alunos = "alunos_ponderados_vaaf",
                   var_receitas = "fundeb",
                   complementacao = complementacao_vaaf)

  ## Unindo bases
  entes = une_vaaf(entes, fundo_estadual_equalizado)

  ## Adicionando medida para Var
  entes[,fundo_vaaf_extra := fundo_vaaf + recursos_extra ]
  entes[,vaaf_extra := fundo_vaaf_extra/alunos_ponderados_vaat]

  # Etapa 2
  fundo_ente =
    equaliza_fundo(entes,
                   var_ordem = "vaaf_extra",
                   var_alunos = "alunos_ponderados_vaat",
                   var_receitas = "fundo_vaaf_extra",
                   entes_excluidos = entes_excluidos_vaaf,
                   complementacao = complementacao_vaat)

  ## Unindo bases
  entes = une_vaat(entes, fundo_ente)
  entes[, vaa := fundo_vaat/alunos]

  # Retorno
  return(retorna_dt_df(entes, produto_dt = produto_dt))
}
