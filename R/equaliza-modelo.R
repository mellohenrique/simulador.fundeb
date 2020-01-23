#' @title Equaliza um modelo
#'
#' @description Função intermediária que equaliza o valor do fundo por aluno
#'
#' @param dados data.frame que serao equalizados
#' @param var_necessidade_equalizacao variavel que indica a necessidade de equalizacao
#' @param aporte quantidade de verba que será usada na equalização
#' @param fundo_equalizado coluna numerica com o fundo a ser equalizado entre os entes
#'
#' @return Um data.frame com as seguintes colunas adicionas cumulativo de alunos e gasto necessario para equalizar
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)

equaliza_modelo <- function(dados, var_necessidade_equalizacao = necessario_equalizacao, aporte = aporte_federal, fundo_equalizado = fundo_estadual, var_alunos  = alunos_estado, codigo) {
  dados %>%
    dplyr::filter({{var_necessidade_equalizacao}} < {{aporte}}) %>%
    dplyr::mutate(vaa = (sum({{fundo_equalizado}}) + {{aporte}}) / sum({{var_alunos}})) %>%
    dplyr::bind_rows(dados %>% dplyr::filter({{var_necessidade_equalizacao}} > {{aporte}})) %>%
    dplyr::mutate(auxilio_federal = {{var_necessidade_equalizacao}} < {{aporte}},
                  total_fundo_estado = vaa * {{var_alunos}}) %>%
    dplyr::select({{codigo}}, total_fundo_estado, vaa_equalizado = vaa, auxilio_federal)
  }
