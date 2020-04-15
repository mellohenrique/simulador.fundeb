#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com codigo ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @inheritParams pondera_socioeconomico
#' @param condicao_rede valor binario que condiciona a ponderacao por rede
#' @param condicao_etapa valor binario que condiciona a ponderacao por etapa
#'
#' @return Data.frame com alunos ponderados de acordo com as especificacoes providas pelo usuario, dados de financas e dados socioeconomicos
#'
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' library(simulador.fundeb)

pondera_geral <- function(base_alunos,
                          ponderador,
                          base_socioeconomica,
                          base_financas,
                          condicao_rede = TRUE,
                          min_social = 1,
                          max_social = 1.2,
                          min_disp_fiscal = 1,
                          max_disp_fiscal = 1.2,
                          var_socioeconomica = nse,
                          considerar = "ambos",
                          desconsidera_estados = FALSE,
                          ...
){
  matriculas <- base_alunos %>%
    dplyr::group_by(ibge) %>%
    dplyr::summarise(alunos_imponderado = sum(alunos))


  if(condicao_rede) {
    resultado <- pondera_alunos_rede(base_alunos, ...) %>%
      pondera_alunos_etapa(ponderador = ponderador)
  } else {
    resultado <- pondera_alunos_etapa(base_alunos, ponderador, ...)
  }

  resultado <- pondera_socioeconomico(
      resultado,
      base_socioeconomica,
      base_financas,
      min_disp_fiscal = min_disp_fiscal,
      max_disp_fiscal = max_disp_fiscal,
      min_social = min_social,
      max_social = max_social,
      var_socioeconomica = {{var_socioeconomica}},
      considerar = considerar,
      desconsidera_estados = desconsidera_estados
      )

    data.table(resultado)[data.table(matriculas), on = "ibge"]


}
