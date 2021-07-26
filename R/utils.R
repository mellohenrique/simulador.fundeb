#' @title Retorna data.frame ou data.table
#'
#' @description Recebe um data.table e determina se transforma em data.frame ou continua dt
#'
#' @inheritParams limpa_fnde
#' @param dados Um objeto da classe data.table
#'
#' @return Um data.frame ou data.table
#'
#' @import data.table

retorna_dt_df <- function(dados, produto_dt = TRUE){

  if(produto_dt) {
    dados
  } else {
    as.data.frame(dados)
  }
}

#' @title Checa e caso nao transforma em data.table
#'
#' @description Checa se um objeto e data.table e caso nao seja transforma em data.table. Caso seja faz uma copia para nao gerar problemas com avaliacoes intermediarias
#'
#' @param dados um objeto data.frame ou data.table
#'
#' @return Um objeto em data.table
#'
#' @import data.table


checa_transforma_dt <- function(dados){
  if (!is.data.table(dados)){
    dados = as.data.table(dados)
  } else {
    copy(dados)
  }
}

#' @title Reescala uma vetor
#'
#' @description Checa se um objeto e data.table e caso nao seja transforma em data.table. Caso seja faz uma copia para nao gerar problemas com avaliacoes intermediarias
#'
#' @param var um vetor numerico
#' @param teto valor maximo do vetor apos o rescalonamento
#' @param chao valor minimo do vetor apois o rescalonamento
#'
#' @return Um vetor numerico reescalado

reescala_vetor = function(var, teto = 1.3, chao = 1){
  maximo = max(var)
  minimo = min(var)
  diferenca = teto - chao

  if (maximo == minimo){
    return(1)
  } else {
    return(teto - diferenca * (var - minimo)/(maximo - minimo))
    }
  }

