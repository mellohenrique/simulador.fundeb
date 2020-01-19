#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com código ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param base_socioeconomica data.frame o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param base_financas data.frame o codigo do ibge e valores do fundeb e das demais receitas
#' @param var_socioeconomica variavel numerica socioeconomica de um ente federativo
#' @param var_fundo_pond variavel numerica com o valor do fundo a ser considerado na ponderacao financeira
#' @param codigo coluna numerica com o codigo do estado, deve ter o mesmo nome em base_alunos e base_socioeconomica
#' @param min_social peso minimo dado a informacao socioeconomica
#' @param max_social peso maximo dado a informacao socioeconomica
#' @param codigo parametro com o nome do codigo a ser usado como identificador dos entes federativos, recomedanda-se o codigo ibge
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#'
pondera_socioeconomico <-
  function(base_alunos,
           base_socioeconomica,
           base_financas,
           var_socioeconomica = nse,
           var_fundo_pond = demais_receitas,
           min_social = 1,
           max_social = 1.3,
           codigo = ibge) {
    base_alunos %>%
      dplyr::left_join(base_socioeconomica) %>%
      dplyr::left_join(base_financas) %>%
      dplyr::mutate(codigo_estado = substring({{codigo}}, 1, 2) %>% as.numeric()) %>%
      dplyr::group_by(codigo_estado) %>%
      dplyr::mutate(
        socioeco = (({{var_socioeconomica}} - min({{var_socioeconomica}}, na.rm = TRUE)) / (max({{var_socioeconomica}}, na.rm = TRUE) - min({{var_socioeconomica}}, na.rm = TRUE))) %>%
               scales::rescale(c(max_social, min_social), c(0, 1)),
        financas = (({{var_fundo_pond}} - min({{var_fundo_pond}}, na.rm = TRUE)) / (max({{var_fundo_pond}}, na.rm = TRUE) - min({{var_fundo_pond}}, na.rm = TRUE))),
        peso_socio_eco = financas * socioeco,
        alunos_socioeco_fin = alunos * peso_socio_eco) %>%
      dplyr::ungroup()
  }
