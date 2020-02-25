#' @title Equaliza um modelo
#'
#' @description Função intermediária que equaliza o valor do fundo por aluno de acordo com o tamanho do aporte federal. A equalizacao e realizada para melhorar a situacao dos entes com o menor valor de recursos por aluno.
#'
#' @param dados data.frame que com informacoes de fundo, alunos e aporte federal
#' @param fundo coluna com o valor do fundo a ser equalizado
#' @param aporte quantidade de verba que será usada na equalização
#' @param var_alunos coluna numerica com o numero de alunos a ser considerado no calculo de recursos por aluno
#' @param codigo codigo dos entes que serao considerados na equalziacao
#'
#' @return Um data.frame com o fundo avaliado equalizado entre os entes
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)

equaliza_modelo <- function(dados, fundo, aporte = aporte_federal, var_alunos  = alunos_estado, codigo) {
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
