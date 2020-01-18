#' @title Calcula numero de alunos por estado
#'
#' @description Essa função calcula o valor total de um fundo. Essa função supõe que os dois primeiros digitos são referentes ao estado do ente federativo
#'
#' @param fundo data.frame com o valor do fundo de cada entede federativo e o codigo ibge do ente
#' @param codigo coluna numerica com o codigo identificador do ente
#' @param var_alunos coluna numerica com o numero de alunos por ente
#'
#' @return Um data.frame com o valor do fundo estadual em um coluna e o codigo do respectivo estado em outra
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::calcula_alunos_estadual(test)

calcula_alunos_estadual <- function(fundo, codigo = ibge, var_alunos = alunos){
  fundo %>%
    dplyr::mutate(codigo_estado = substring({{codigo}}, 1, 2) %>% as.numeric()) %>%
    dplyr::group_by(codigo_estado) %>%
    dplyr::summarise(alunos_estado = sum({{var_alunos}}, na.rm = TRUE))
}
