#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams limpa_fnde
#' @param peso_etapas data.frame com os dados de peso das etapas
#' @param tidy variavel binaria se a tabela final da funcao sera no formato tidy (se Verdadeiro) ou long (se Falso)
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto
#'
#' @import data.table
#'
#' @export

pondera_alunos_etapa <- function(dados_fnde, peso_etapas = peso, tidy = TRUE, produto_dt = TRUE){

  dados_fnde = checa_transforma_dt(dados_fnde)

  dados_fnde[, `Estimativa de Receitas` := NULL]
  alunos_long = suppressWarnings(melt(dados_fnde, id.vars = c("UF", "Município"),
                                      variable.name = "etapa",
                                      value.name = "alunos",
                                      variable.factor = FALSE,
                                      verbose = FALSE))

  alunos_long[, `:=`(alunos = ifelse(is.na(alunos), 0, alunos))]

  alunos_long[peso_etapas, peso := peso, on = "etapa"]

  alunos_long[, alunos_ponderados := alunos * peso]

  if(tidy){
    retorna_dt_df(alunos_long, produto_dt)
  } else {
    retorna_dt_df(dcast(alunos_long, UF + `Município` ~ etapa, value.var = "alunos_ponderados"), produto_dt)
  }
}
