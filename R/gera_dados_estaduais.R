#' @title Gera tabela com dados dos estados
#'
#' @description Função intermediária que gera dados dos estados que será usada nas simulações
#'
#' @param dados data.frame preparado por pondera_geral
#'
#' @return Um data.frame com as seguintes colunas adicionas cumulativo de alunos e gasto necessario para equalizar
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#'
#' <- dplyr::left_join(

gera_dados_estaduais <- function(dados){
  dplyr::left_join(
    calcula_alunos_estadual(dados),
    calcula_alunos_estadual(dados, var_alunos = alunos_socioeco) %>%
      dplyr::rename(alunos_estado_socio = alunos_estado)) %>%
      dplyr::left_join(calcula_fundo_estadual(dados)) %>%
      dplyr::mutate(vaa = fundo_estadual / alunos_estado,
                    vaa_socio = fundo_estadual / alunos_estado_socio)
  }
