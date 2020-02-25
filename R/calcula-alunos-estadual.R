#' @title Calcula numero de alunos por estado
#'
#' @description Calcula o numero de alunos por ente federativo. Essa funcao supoe que os dois primeiros digitos sao referentes ao estado do ente federativo
#'
#' @param dados data.frame com o numero de alunos por ente federativo
#' @param codigo coluna numerica com o codigo identificador do ente
#' @param var_alunos nomeia a coluna numerica com o numero de alunos por ente
#'
#' @return Um data.frame com o numero de alunos por estado
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#' simulador.fundeb:::calcula_alunos_estadual(test)

calcula_alunos_estadual <- function(dados, codigo = ibge, var_alunos = alunos){
  dados %>%
    dplyr::mutate(codigo_estado = substring({{codigo}}, 1, 2) %>% as.numeric()) %>%
    dplyr::group_by(codigo_estado) %>%
    dplyr::summarise(alunos_estado = sum({{var_alunos}}, na.rm = TRUE))
}
