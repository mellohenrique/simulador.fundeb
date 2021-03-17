#' @title Retorna data.frame ou data.table
#'
#' @description Recebe um data.table e determina se transforma em data.frame ou continua dt
#'
#' @param dados Um objeto da classe data.table
#'
#' @return Um data.frame ou data.table
#'
#' @import data.table


retorna_dt_df <- function(dados, produto_dt){
  if(produto_dt) {
    dados
  } else {
    as.data.frame(dados)
  }
}

#' @title Checa e caso nao transforma em data.table
#'
#' @description Checa se um objeto e data.table e caso nao seja transforma em data.table
#'
#' @param dados um objeto data.frame ou data.table
#'
#' @return Um objeto em data.table
#'
#' @import data.table


checa_transforma_dt <- function(dados){
  if (!is.data.table(dados)){
    dados = as.data.table(dados)
  }
  dados
}

