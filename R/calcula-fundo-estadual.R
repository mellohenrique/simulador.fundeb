#' @title Calcula valor do fundo total
#'
#' @description Essa função calcula o valor total de um fundo
#'
#' @param fundo data.frame com o valor do fundo de cada entede federativo e o codigo ibge do ente
#' @param codigo coluna numerica com o codigo do estado
#'
#' @return Um data.frame com o valor do fundo estadual em um coluna e o codigo do respectivo estado em outra
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::calcula_fundo_estadual(test)

calcula_fundo_estadual <- function(fundo, codigo = ibge, var_fundo = valor){
  fundo %>% 
    dplyr::mutate(codigo_estado = substring({{codigo}}, 1, 2) %>% as.numeric()) %>% 
    dplyr::group_by(codigo_estado) %>% 
    dplyr::summarise(fundo_estadual = sum({{var_fundo}}, na.rm = TRUE))
}
