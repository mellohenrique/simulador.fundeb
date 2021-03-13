#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @param dados_mor_let base dados no formato mortalidade letalidade
#'
#' @return Um data.frame com os dados de alunos de FNDE em formato longo ou curto
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#'

pondera_alunos_etapa <- function(dados_fnde, tidy = FALSE, produto_dt = TRUE){

  dados = checa_transforma_dt(dados_fnde)

  alunos = dados[, `:=`(`Estimativa de Receitas` = NULL,
                         `Coeficiente de Distribuição` = NULL)]

  alunos_long = suppressWarnings(melt(alunos, id.vars = c("UF", "Município"),
                                      variable.name = "etapa",
                                      value.name = "alunos",
                                      variable.factor = FALSE,
                                      verbose = FALSE))

  alunos_long[, `:=`(alunos = ifelse(is.na(alunos), 0, alunos))]

  alunos_long[peso, peso := peso, on = "etapa"]

  alunos_long[, alunos_ponderados := alunos * peso]

  if(tidy){
    retorna_dt_df(alunos_long)
  } else {
    retorna_dt_df(dcast(alunos_long, UF + `Município` ~ etapa, value.var = "alunos_ponderados"))
  }
  }
