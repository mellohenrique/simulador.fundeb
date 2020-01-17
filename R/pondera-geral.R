#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com codigo ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param base_socioeconomica data.frame o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param condicao_rede valor binario que condiciona a ponderacao por rede
#' @param condicao_etapa valor binario que condiciona a ponderacao por etapa
#' @param condicao_socio valor binario que condiciona a ponderacao por informacao socioeconomica
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' library(simulador.fundeb)

pondera_geral <- function(base_alunos, ponderador, base_socioeconomica, condicao_rede = TRUE, condicao_socio = FALSE, ...){
  if(condicao_rede) {
    resultado <- pondera_alunos_rede(base_alunos, ...) %>%
      pondera_alunos_etapa(ponderador = ponderador)
  } else {
    resultado <- pondera_alunos_etapa(base_alunos, ponderador, ...)
  }

  if(condicao_socio) {
    pondera_socioeconomico(resultado, base_socioeconomica, ...)
  } else {
    if(is.data.frame(base_socioeconomica) & condicao_socio == FALSE){
      dplyr::left_join(resultado, base_socioeconomica)
    } else {
      resultado
    }
  }
}
