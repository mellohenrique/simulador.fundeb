#' @title Gera tabela com dados dos estados
#'
#' @description Funcao intermediaria que agrega dados de ente federativo por estado
#'
#' @param dados data.frame preparado por pondera_geral
#'
#' @return Um data.frame com dados de alunos por estado, pela ponderacao por etapa e por dados socioeconomicos, fundo estadual, vaa, pelas duas ponderacoes de aluno
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)

gera_dados_estaduais <- function(dados){
  dplyr::left_join(
    calcula_alunos_estadual(dados),
    calcula_alunos_estadual(dados, var_alunos = alunos_socioeco) %>%
      dplyr::rename(alunos_estado_socio = alunos_estado)) %>%
      dplyr::left_join(calcula_fundo_estadual(dados)) %>%
      dplyr::mutate(vaa = fundo_estadual / alunos_estado,
                    vaa_socio = fundo_estadual / alunos_estado_socio)
  }
