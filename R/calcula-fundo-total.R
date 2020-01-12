#' @title Calcula valor do fundo total
#'
#' @description Essa função calcula o valor total de um fundo
#'
#' @param fundo data.frame
#' @param var coluna numerica de fundo
#'
#' @return O valor do fundo total como vetor numerico de tamanho 1
#'
#' @import tidyverse
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::calcula_fundo_total(test)

calcula_fundo_total <- function(fundo, var = valor){
  fundo %>%
    summarise(fundo_total = sum({{var}}, na.rm = TRUE)) %>%
    pull(fundo_total)
}

