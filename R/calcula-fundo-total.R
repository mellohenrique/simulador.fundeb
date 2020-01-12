#' @title Calcula valor do fundo total
#'
#' @description Essa função calcula o valor total de um fundo
#'
#' @param fundo data.frame
#' @param var_fundo coluna numerica de fundo
#'
#' @return O valor do fundo total como vetor numerico de tamanho 1
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::calcula_fundo_total(test)

calcula_fundo_total <- function(fundo, var_fundo = valor){
  fundo %>%
    dplyr::summarise(fundo_total = sum({{var_fundo}}, na.rm = TRUE)) %>%
    dplyr::pull(fundo_total)
}

