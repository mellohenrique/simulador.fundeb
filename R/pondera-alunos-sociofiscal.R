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


pondera_alunos_sociofiscal <- function(dados_alunos, dados_complementar, produto_dt = TRUE){
  # Binding variables para NULL
  impostos_extra = imposto_cap = fator_social = impostos_cap = alunos_ponderados = fator_fiscal = fator_socio = NULL

  dados_alunos = checa_transforma_dt(dados_alunos)
  dados_complementar = checa_transforma_dt(dados_complementar)

  dados_alunos[dados_complementar,
         `:=`(nome = nome,
              nse = nse,
              fundeb_vaaf  = fundeb_vaaf ,
              fundeb_vaat = fundeb_vaat),
         on = "ibge"]

  dados_alunos[,
               `:=`(alunos_vaaf = alunos_vaaf * nse,
                    alunos_vaat = alunos_vaat * nse)
    ]

    return(retorna_dt_df(dados_alunos, produto_dt = produto_dt))
}
