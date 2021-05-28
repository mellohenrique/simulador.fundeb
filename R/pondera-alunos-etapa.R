#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams limpa_fnde
#' @param peso_etapas data.frame com os dados de peso das etapas
#' @param retorno variavel em caractere com a opcao de retorno da funcao. As opcoes tidy se apresenta o retorno os valores de alunos e alunos ponderados, etapa_tidy se apresenta valores de alunos e alunos ponderados para todas as etapas em formato tidy e etapa_long se se apresenta valores de alunos e alunos ponderados para todas as etapas em formato long
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto
#'
#' @import data.table
#'
#' @export

pondera_alunos_etapa <- function(dados_fnde, peso_etapas = peso, retorno = c("tidy", "etapa_tidy", "etapa_long"), produto_dt = TRUE){

  retorno = match.arg(retorno)
  dados_fnde = checa_transforma_dt(dados_fnde)

  suppressWarnings({
  dados_fnde[,`:=`(estimativa_de_receitas = NULL,
                   nome = NULL,
                   municipio = NULL,
                   coeficiente_de_distribuicao = NULL)]
  })

  alunos_tidy = suppressWarnings(melt(dados_fnde, id.vars = c("uf", "ibge"),
                                      variable.name = "etapa",
                                      value.name = "alunos",
                                      variable.factor = FALSE,
                                      verbose = FALSE))

  alunos_tidy[, `:=`(alunos = ifelse(is.na(alunos), 0, alunos))]

  alunos_tidy[peso_etapas, peso := peso, on = "etapa"]

  alunos_tidy[, alunos_ponderados := alunos * peso]

  if (retorno == "tidy") {
    alunos_tidy = alunos_tidy[,.(
      alunos_ponderados = sum(alunos_ponderados),
      alunos = sum(alunos)), by = .(uf, ibge)]
    retorna_dt_df(alunos_tidy, produto_dt = produto_dt)

  } else if (retorno == "etapa_tidy") {
    retorna_dt_df(alunos_tidy, produto_dt = produto_dt)

  } else if (retorno == "etapa_long") {
    retorna_dt_df(dcast(alunos_tidy, uf + ibge ~ etapa, value.var = "alunos_ponderados"), produto_dt= produto_dt)

  }
}
