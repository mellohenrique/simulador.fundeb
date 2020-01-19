#' @title Prepara um modelo para equalizacao
#'
#' @description Função intermediária que prepara um conjunto de dados para equalização gerando variaveis de interesse intermediárias
#'
#' @param dados data.frame que sera preparado para equalizacao
#' @param var_vaa variavel de valor as ser equalizada entre entes federativos
#' @param var_alunos_grupos coluna numerica com o numero de alunos por ente
#'
#' @return Um data.frame com as seguintes colunas adicionas cumulativo de alunos e gasto necessario para equalizar
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)

prepara_equalizacao <- function(dados, var_vaa = vaa, var_alunos_grupo = alunos_estado){
  dados %>%
    dplyr::arrange({{var_vaa}}) %>%
    dplyr::mutate(
      cumulativo_alunos = cumsum({{var_alunos_grupo}}),
      necessario_equalizacao = ({{var_vaa}} * cumulativo_alunos) - cumsum({{var_vaa}} * {{var_alunos_grupo}})
    )
  }
