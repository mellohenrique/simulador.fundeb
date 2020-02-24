#' @title Simulação do modelo hibrido de financiamento da educação no tempo
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação ao longo do tempo. Considera o crescimento demográfico e econômico ao longo do tempo.
#'
#' @inheritParams simular_modelo_fundeb
#' @param crescimento_economico vetor númerico de crescimento economico, especificamente do fundeb e das demais receitas
#' @param crescimento_demografico vetor númerico de crescimento demográfico para alunos
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
                                         auxilio_federal_vat = 0.05,
                                         equalizacao_socio = FALSE,
                                         distribuicao_fundo_estadual_socio = FALSE,
                                         crescimento_economico,
                                         crescimento_demografico,
                                         min_social = 1,
                                         max_social = 1.3,
                                         min_financas = 1,
                                         max_financas = 1.3,
                                         var_socioeconomica = nse,
                                         considerar = "ambos",
                                         ...
){
  lista_fundos <- purrr::map(cumprod(crescimento_economico), ~dplyr::mutate(base_financas, fundeb = fundeb * .x, demais_receitas = demais_receitas * .x))
  lista_alunos <- purrr::map(cumprod(crescimento_demografico), ~dplyr::mutate(base_alunos, alunos = alunos * .x))

  purrr::pmap_dfr(
    .l = list(
      ls_alunos = lista_alunos,
      ls_fundos = lista_fundos,
      ls_auxilio = auxilio_federal,
      ls_auxilio_vat = auxilio_federal_vat
    ),
    .f = function(ls_alunos, ls_fundos, ls_auxilio, ls_auxilio_vat)
      {simular_modelo_hibrido(
      ls_alunos,
      ponderador,
      base_socioeconomica,
      ls_fundos,
      auxilio_federal = ls_auxilio,
      auxilio_federal_vat = ls_auxilio_vat,
      equalizacao_socio = FALSE,
      distribuicao_fundo_estadual_socio = FALSE,
      min_social = min_social,
      max_social = max_social,
      min_financas = min_financas,
      max_financas = max_financas,
      var_socioeconomica = {{var_socioeconomica}},
      considerar = considerar,
      ...
    )},
    .id = "ano"
  )
}
