#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com codigo ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param base_socioeconomica data.frame o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param base_financas data.frame com o codigo do ibge e os valores do fundeb e das demais receitas
#' @param condicao_rede valor binario que condiciona a ponderacao por rede
#' @param condicao_etapa valor binario que condiciona a ponderacao por etapa
#' @param condicao_socio valor binario que condiciona a ponderacao por informacao socioeconomica
#' @param min_social peso minimo dado a informacao socioeconomica
#' @param max_social peso maximo dado a informacao socioeconomica
#' @param min_financas peso minimo dado a informacao de financas
#' @param max_financas peso maximo dado a informacao de financas
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
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
                          condicao_socio = TRUE,
                          min_social = 1,
                          max_social = 1.3,
                          min_financas = 1,
                          max_financas = 1.3,
                          ...
){
  if(condicao_rede) {
    resultado <- pondera_alunos_rede(base_alunos, ...) %>%
      pondera_alunos_etapa(ponderador = ponderador)
  } else {
    resultado <- pondera_alunos_etapa(base_alunos, ponderador, ...)
  }

  if(condicao_socio) {
    pondera_socioeconomico(resultado, base_socioeconomica, base_financas, min_financas = min_financas, max_financas = max_financas, min_social = min_social, max_social = max_social)
  } else {
    if(is.data.frame(base_socioeconomica) & is.data.frame(financas) & condicao_socio == FALSE){
      dplyr::left_join(resultado, base_socioeconomica) %>%
        dplyr::left_join(base_financas)
    } else {
      resultado
    }
  }
}
