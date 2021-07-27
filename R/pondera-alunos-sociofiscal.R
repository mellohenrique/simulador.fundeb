#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams limpa_fnde
#' @param dados_alunos data.frame com os dados de peso das etapas
#' @param dados_complementar data.frame com os dados de peso das etapas
#' @param chao_socio valor minimo do peso da variavel social na ponderacao de alunos
#' @param teto_socio valor maximo do peso da variavel social na ponderacao de alunos
#' @param chao_fiscal valor minimo do peso da variavel fiscal na ponderacao de alunos
#' @param teto_fiscal valor maximo do peso da variavel fiscal na ponderacao de alunos
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os fatores sociais e financeiros
#'
#' @import data.table
#'


pondera_alunos_sociofiscal <- function(dados_alunos, dados_complementar, chao_socio = 1, teto_socio = 1.3, chao_fiscal = 1, teto_fiscal = 1.3, produto_dt = TRUE){
  # Binding variables para NULL
  impostos_extra = imposto_cap = fator_social = impostos_cap = alunos_ponderados = fator_fiscal = fator_socio = NULL

  dados_alunos = checa_transforma_dt(dados_alunos)
  dados_complementar = checa_transforma_dt(dados_complementar)

  dados_alunos[dados_complementar,
         `:=`(fator_social = fator_social,
              fator_fiscal = fator_fiscal,
              fundeb = fundeb,
              recursos_extra = recursos_extra),
         on = "ibge"]

  dados_alunos[, `:=`(
    fator_socio = reescala_vetor(fator_social, chao = chao_socio, teto = teto_socio),
    fator_fiscal = reescala_vetor(fator_fiscal, chao = chao_fiscal, teto = teto_fiscal))]

  dados_alunos[,
    alunos_ponderados := alunos_ponderados * fator_fiscal * fator_socio]

    return(retorna_dt_df(dados_alunos, produto_dt = produto_dt))
}
