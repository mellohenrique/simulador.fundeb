#' @title Equaliza um modelo
#'
#' @description Função intermediária que equaliza o valor do fundo por aluno
#'
#' @param dados data.frame que serao equalizados
#' @param fundo coluna com o valor do fundo a ser equalizado
#' @param aporte quantidade de verba que será usada na equalização
#' @param var_alunos coluna numerica com o numero de alunos
#' @param codigo codigo do ente ou estado
#'
#' @return Um data.frame com o fundo avaliado equalizado
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)

equaliza_modelo <- function(dados, fundo, aporte = aporte_federal,  var_alunos  = alunos_estado, codigo) {
  dados <- dados %>%
    dplyr::arrange({{fundo}}/{{var_alunos}}) %>%
    dplyr::mutate(auxilio = aporte > cumsum({{var_alunos}}) * {{fundo}}/{{var_alunos}} - cumsum({{fundo}}/{{var_alunos}} * {{var_alunos}}))
  dados %>%
    dplyr::filter(auxilio) %>%
    dplyr::mutate(fundo_equalizado = (aporte + sum({{fundo}}))*{{var_alunos}}/sum({{var_alunos}})) %>%
    dplyr::bind_rows(dados %>%
                       dplyr::filter(!auxilio) %>%
                       dplyr::mutate(fundo_equalizado = {{fundo}})) %>%
    dplyr::select({{codigo}}, fundo_equalizado)
  }
