#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto
#'
#' @import data.table
#'


pondera_alunos_etapa <- function(dados_fnde, peso_etapas = peso, difere_etapas_complementacao = c("vaaf_vaat", "mesmos_pesos"), produto_dt = TRUE){
  # Binding variables para NULL
  alunos = alunos_ponderados = . = uf = ibge = NULL

  difere_etapas_complementacao = match.arg(difere_etapas_complementacao)
  dados_fnde = checa_transforma_dt(dados_fnde)

  suppressWarnings({
  dados_fnde[,`:=`(nome = NULL)]
  })

  alunos_tidy = suppressWarnings(melt(dados_fnde, id.vars = c("uf", "ibge"),
                                      variable.name = "etapa",
                                      value.name = "alunos",
                                      variable.factor = FALSE,
                                      verbose = FALSE))

  alunos_tidy[, `:=`(alunos = ifelse(is.na(alunos), 0, alunos))]

  if (difere_etapas_complementacao == "vaaf_vaat"){
    alunos_tidy[peso_etapas, `:=`(peso_vaaf = peso_vaaf,
                                  peso_vaat = peso_vaat), on = "etapa"]
  } else if (difere_etapas_complementacao == "mesmos_pesos") {

    alunos_tidy[peso_etapas, `:=`(peso_vaaf = peso,
                                  peso_vaat = peso), on = "etapa"]
  }

  alunos_tidy[,
              `:=`(alunos_vaaf = alunos * peso_vaaf,
                   alunos_vaat = alunos * peso_vaat)]

  alunos_tidy = alunos_tidy[,.(alunos = sum(alunos),
                               alunos_vaaf = sum(alunos_vaaf),
                 alunos_vaat = sum(alunos_vaat)), by = .(uf, ibge)]

  return(retorna_dt_df(alunos_tidy, produto_dt = produto_dt))

}
