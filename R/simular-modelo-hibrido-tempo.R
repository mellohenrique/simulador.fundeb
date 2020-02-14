#' @title Simulação do modelo hibrido de financiamento da educação no tempo
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação ao longo do tempo. Considera o crescimento demográfico e econômico ao longo do tempo.
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param base_socioeconomica data.frame com  o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param base_financas data.frame com o codigo do ibge e os valores do fundeb e das demais receitas
#' @param auxilio_federal percentual do fundo que a União complementará
#' @param auxilio_federal_vaa percentual do fundo que a União complementará segundo o modelo vaa
#' @param equalizacao_socio parametro lógico que controla se a equalização do fundo considerara o vetor de alunos ou de alunos socioeconomico
#' @param distribuicao_fundo_estadual_socio parametro logico que controla se a distribuicao do fundo estadual considerara o vetor de alunos ou de alunos socioeconomico
#' @param crescimento_economico vetor númerico de crescimento economico, especificamente do fundeb e das demais receitas
#' @param crescimento_demografico vetor númerico de crescimento demográfico para alunos
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

simular_modelo_hibrido_tempo <- function(base_alunos,
                                         ponderador,
                                         base_socioeconomica,
                                         base_financas,
                                         auxilio_federal = 0.1,
                                         auxilio_federal_vaa = 0.05,
                                         equalizacao_socio = FALSE,
                                         distribuicao_fundo_estadual_socio = FALSE,
                                         crescimento_economico,
                                         crescimento_demografico,
                                         min_social = 1,
                                         max_social = 1.3,
                                         min_financas = 1,
                                         max_financas = 1.3,
                                         ...
){
  lista_fundos <- purrr::map(cumprod(crescimento_economico), ~dplyr::mutate(base_financas, fundeb = fundeb * .x, demais_receitas = demais_receitas))
  lista_alunos <- purrr::map(cumprod(crescimento_demografico), ~dplyr::mutate(base_alunos, alunos = alunos * .x))

  purrr::map2_dfr(lista_alunos,
                  lista_fundos,
                  ~simular_modelo_hibrido(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = auxilio_federal, auxilio_federal_vaa = auxilio_federal_vaa, equalizacao_socio = FALSE, distribuicao_fundo_estadual_socio = FALSE, , min_social = min_social, max_social = max_social, min_financas = min_financas, max_financas = max_financas,...),
                  .id = "ano")
}
