
#' @title Reescala uma vetor
#'
#' @description Checa se um objeto e data.table e caso nao seja transforma em data.table. Caso seja faz uma copia para nao gerar problemas com avaliacoes intermediarias
#'
#' @param var um vetor numerico
#' @param teto valor maximo do vetor apos o rescalonamento
#' @param chao valor minimo do vetor apois o rescalonamento
#'
#' @return Um vetor numerico reescalado

reescala_vetor = function(var, maximo = 1.05, minimo = .95){
  maior_valor = max(var)
  menor_valor = min(var)
  diferenca = maximo - minimo

  if (maior_valor == menor_valor){
    return(1)
  } else {
    return(minimo + diferenca * (var - menor_valor)/(maior_valor - menor_valor))
  }
}

#' @title Checa numerico
#'
#' @description Testa se objeto e um objeto numerico de dimensao 1
#'
#' @param x um objeto a ser testado
#' @param nome nome do objeto para ser passado para a chamada de erro se necessaria
#'
#' @return Lanca um erro ou retorna um vetor logico

checa_numerico = function(x, nome){
  if(!is.numeric(x)) {
    stop(paste(nome, 'não é numerico'), call. = FALSE)
  } else if (length(x) != 1){
    stop(paste(nome, 'tem comprimento diferente de 1'), call. = FALSE)
  } else {
    TRUE
  }
}

#' @title Checa NAs
#'
#' @description Checa se um objeto possui algum valor NA
#'
#' @inheritParams checa_numerico
#'
#' @return Lanca um erro ou retorna um vetor logico

checa_na = function(x, nome){
  if(any(is.na(x)) | any(is.null(x))) {
    stop(paste(nome, 'possui valores faltantes ou nulos'), call. = FALSE)
  } else {
    TRUE
  }
}

#' @title Checa data.frame
#'
#' @description Checa se um objeto e um data.frame
#'
#' @inheritParams checa_numerico
#'
#' @return Lanca um erro ou retorna um vetor logico

checa_data_frame = function(x, nome){
  if(!is.data.frame(x)) {
    stop(paste(nome, 'não é um data.frame'), call. = FALSE)
  } else {
    TRUE
  }
}
