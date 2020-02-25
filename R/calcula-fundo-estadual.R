#' @title Calcula valor do fundo estadual
#'
#' @description Essa função calcula o valor total dos fundo estaduais, a funcao considera que os dois primeiros digitos do codigo correspondem ao estado
#'
#' @param fundo data.frame com o valor do fundo de cada entede federativo e o codigo ibge do ente
#' @param codigo coluna numerica com o codigo do estado
#' @param var_fundo coluna numerica com o valor do fundo
#'
#' @return Um data.frame com o valor do fundo estadual em um coluna e o codigo do respectivo estado em outra
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::calcula_fundo_estadual(test, var_fundo = valor)

calcula_fundo_estadual <- function(fundo, codigo = ibge, var_fundo = fundeb){
  fundo %>%
    dplyr::mutate(codigo_estado = substring({{codigo}}, 1, 2) %>% as.numeric()) %>%
    dplyr::group_by(codigo_estado) %>%
    dplyr::summarise(fundo_estadual = sum({{var_fundo}}, na.rm = TRUE))
}
