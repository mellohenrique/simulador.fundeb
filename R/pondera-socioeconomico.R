#' @title Pondera alunos por etapa de ensino
#'
#' @description Recebe uma base com código ibge, etapa e alunos e outra com pesos de ponderador por etapa e gera uma tabela com o número de alunos ponderados por entidade da federação
#'
#' @param base_alunos data.frame com numero de alunos por etapa e ente federativo
#' @param base_socioeconomica data.frame com o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param base_financas data.frame com o codigo do ibge e valores do fundeb e das demais receitas
#' @param var_socioeconomica variavel numerica socioeconomica de um ente federativo a ser considerada na ponderacao socioeconomica do fundo
#' @param var_fundo_pond variavel numerica com o valor do fundo a ser considerado na ponderacao financeira
#' @param codigo coluna numerica com o codigo do estado, deve ter o mesmo nome em base_alunos e base_socioeconomica
#' @param min_social peso minimo dado a informacao socioeconomica
#' @param max_social peso maximo dado a informacao socioeconomica
#' @param min_disp_fiscal peso minimo dado a informacao de financas
#' @param max_disp_fiscal peso maximo dado a informacao de financas
#' @param codigo parametro com o nome do codigo a ser usado como identificador dos entes federativos, recomedanda-se o codigo ibge
#' @param considerar nome que indica que parametro sera considarado na criacao do peso socioeconomico, se selecionado social se considerara apenas as informacoes sociais, se selecionado financas se considerara apenas as informacoes de financas e se selecionado ambos se considerara ambas as dimensoes
#' @param desconsidera_estados variavel logica que indica se os estados serao considerados na ponderacao socioeconomica, se verdadeiro o valor que os estados receberao nao e alterado pelo seu peso socioeconomico, se falso o valor que os estados receberao e alterado pelo seu peso socioeconomico
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#' @examples
#' library(simulador.fundeb)
#'
#'

pondera_socioeconomico <-
  function(base_alunos,
           base_socioeconomica,
           base_financas,
           var_socioeconomica = nse,
           var_fundo_pond = demais_receitas,
           min_social = 1,
           max_social = 1.2,
           min_disp_fiscal = 1,
           max_disp_fiscal = 1.2,
           considerar = c("social", "financas", "ambos"),
           desconsidera_estados = FALSE,
           codigo = ibge) {
    if(desconsidera_estados){
      estados <- base_alunos %>%
        dplyr::filter(ibge < 100) %>%
        dplyr::left_join(base_socioeconomica) %>%
        dplyr::left_join(base_financas) %>%
        dplyr::mutate(codigo_estado = substring({{codigo}}, 1, 2) %>% as.numeric())
      base_alunos <- base_alunos %>%
        dplyr::filter(ibge > 100)
    }

    dados <- base_alunos %>%
      dplyr::left_join(base_socioeconomica) %>%
      dplyr::left_join(base_financas) %>%
      dplyr::mutate(codigo_estado = substring({{codigo}}, 1, 2) %>% as.numeric()) %>%
      dplyr::group_by(codigo_estado) %>%
      dplyr::mutate(
        socioeco = (({{var_socioeconomica}} - min({{var_socioeconomica}}, na.rm = TRUE)) / (max({{var_socioeconomica}}, na.rm = TRUE) - min({{var_socioeconomica}}, na.rm = TRUE))) %>%
               scales::rescale(c(max_social, min_social), c(0, 1)),
        financas = (({{var_fundo_pond}} - min({{var_fundo_pond}}, na.rm = TRUE)) / (max({{var_fundo_pond}}, na.rm = TRUE) - min({{var_fundo_pond}}, na.rm = TRUE))) %>%
          scales::rescale(c(max_disp_fiscal, min_disp_fiscal), c(0, 1)),
        socioeco = dplyr::if_else(is.nan(socioeco), min_social, socioeco),
        financas = dplyr::if_else(is.nan(financas), min_disp_fiscal, financas),
        peso_socio_eco = dplyr::case_when(
          considerar == "social" ~ socioeco,
          considerar == "financas" ~ financas,
          considerar == "ambos" ~ financas * socioeco),
        alunos_socioeco = alunos * peso_socio_eco) %>%
      dplyr::ungroup()

    if(desconsidera_estados){
      dados <- dados %>%
        dplyr::bind_rows(estados) %>%
        dplyr::group_by(codigo_estado) %>%
        dplyr::mutate(peso_socio_eco = dplyr::case_when(
          ibge <= 100 ~ weighted.mean(peso_socio_eco, alunos),
          TRUE ~ peso_socio_eco
        )) %>%
          dplyr::ungroup()
    }

    dados
  }
