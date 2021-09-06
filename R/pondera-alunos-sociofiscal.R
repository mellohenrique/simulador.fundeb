#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams simula_fundeb
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
         `:=`(nome = nome,
              fator_social = fator_social,
              fator_fiscal = fator_fiscal,
              fundeb = fundeb,
              recursos_extra = recursos_extra),
         on = "ibge"]

  dados_alunos[, `:=`(
    fator_social = reescala_vetor(fator_social, chao = chao_socio, teto = teto_socio),
    fator_fiscal = reescala_vetor(fator_fiscal, chao = chao_fiscal, teto = teto_fiscal))]

  dados_alunos[,
               `:=`(alunos_ponderados_vaaf = alunos_ponderados_vaaf * fator_fiscal * fator_social,
                    alunos_ponderados_vaat = alunos_ponderados_vaat * fator_fiscal * fator_social)
    ]

    return(retorna_dt_df(dados_alunos, produto_dt = produto_dt))
}
