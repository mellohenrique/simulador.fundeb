#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com código ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @param base_socioeconomica data.frame o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param variavel_socioeconomica variavel numerica socioeconomica de um ente federativo
#' @param min_social peso minimo dado a informacao socioeconomica
#' @param max_social peso maximo dado a informacao socioeconomica
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#'
pondera_socioeconomico <-
  function(base_socioeconomica,
           variavel_socioeconomica = nse,
           min_social = 1,
           max_social = 1.3) {
    base_socioeconomica %>%
      dplyr::mutate(socioeco = (({{variavel_socioeconomica}} - min({{variavel_socioeconomica}}, na.rm = TRUE)) / (max({{variavel_socioeconomica}}, na.rm = TRUE) - min({{variavel_socioeconomica}}, na.rm = TRUE))) %>%
               scales::rescale(c(max_social, min_social), c(0, 1)))
  }
