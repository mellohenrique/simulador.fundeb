#' @title Limpa dados do FNDE
#'
#' @description Limpa os dados do FNDE removendo coeficiente de distribuicao e o municipio "TOTAL GERAL"
#'
#' @param dados_fnde data.frame ou data.table com os dados do FNDE
#' @param produto_dt Variavel binaria que determina se a tabela produto da funcao sera da classe data.table (se Verdadeiro) ou data.frame (se Falso)
#'
#' @return Um data.frame ou data.table com os dados do FNDE
#'
#' @import data.table


limpa_fnde <- function(dados_fnde, produto_dt = TRUE){
  dados_fnde = checa_transforma_dt(dados_fnde)

  dados_fnde = dados_fnde[nome != "TOTAL GERAL",]

  dados_fnde[, coeficiente_de_distribuicao := NULL]

  retorna_dt_df(dados_fnde, produto_dt = produto_dt)
}

#' @title Retira dados de Financas do FNDE
#'
#' @description Limpa os dados do FNDE selecionando os dados de Financas
#'
#' @param dados_fnde data.frame ou data.table com os dados do FNDE limpos
#' @param produto_dt Variavel binaria que determina se a tabela produto da funcao sera da classe data.table (se Verdadeiro) ou data.frame (se Falso)
#'
#' @return Um data.frame ou data.table com os dados de financas do FNDE por ente
#'
#' @import data.table

financas_fnde <- function(dados_fnde, produto_dt = TRUE){
  dados_fnde = checa_transforma_dt(dados_fnde)

  dados_fnde = dados_fnde[, .(uf, ibge, estimativa_de_receitas)]

  retorna_dt_df(dados_fnde, produto_dt = produto_dt)
}
