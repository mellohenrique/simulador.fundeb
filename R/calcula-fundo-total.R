#' @title Calcula valor do fundo total
#'
#' @description Essa função calcula o valor total do fundo selecionado
#'
#' @param fundo data.frame com o valor do fundo de cada entede federativo
#' @param var_fundo nomeia a coluna numerica com o valor do fundo
#'
#' @return O valor do fundo total como vetor numerico de tamanho 1
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::calcula_fundo_total(test, var_fundo = valor)

calcula_fundo_total <- function(fundo, var_fundo = fundeb){
  fundo %>%
    dplyr::summarise(fundo_total = sum({{var_fundo}}, na.rm = TRUE)) %>%
    dplyr::pull(fundo_total)
}

