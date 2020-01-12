#' @title Calcula valor do fundo total
#'
#' @description Essa função calcula o valor total de um fundo
#'
#' @param fundo data.frame
#' @param var_fundo coluna numerica com o valor do fundo
#' @param codigo coluna numerica com o codigo do estado
#'
#' @return Um data.frame com o valor do fundo estadual em um coluna e o codigo do respectivo estado em outra
#'
#' @import tidyverse
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::calcula_fundo_estadual(test)

calcula_fundo_estadual <- function(fundo, codigo = ibge, var_fundo = valor){
  # Calcula o valor total do fundo por estado
  fundo %>% 
    mutate(codigo_estado = str_extract({{codigo}}, "^[:digit:]{2}") %>% as.numeric()) %>% 
    group_by(codigo_estado) %>% 
    summarise(fundo_estadual = sum({{var_fundo}}, na.rm = TRUE))
}
