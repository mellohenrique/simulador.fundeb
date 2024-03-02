
#' @title Reescala uma vetor
#'
#' @description Checa se um objeto e data.table e caso nao seja transforma em data.table. Caso seja faz uma copia para nao gerar problemas com avaliacoes intermediarias
#'
#' @param var um vetor numerico
#' @param teto valor maximo do vetor apos o rescalonamento
#' @param chao valor minimo do vetor apois o rescalonamento
#'
#' @return Um vetor numerico reescalado

reescala_vetor = function(var, teto = 1.05, chao = .95){
  maximo = max(var)
  minimo = min(var)
  diferenca = teto - chao

  if (maximo == minimo){
    return(1)
  } else {
    return(chao + diferenca * (var - minimo)/(maximo - minimo))
  }
}

