#' @title Simulação do modelo vat de financiamento da educação no tempo
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação ao longo do tempo. Considera o crescimento demográfico e econômico ao longo do tempo.
#'
#' @inheritParams simular_modelo_fundeb_tempo
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export

simular_modelo_vaat_tempo <- function(base_alunos,
                                     ponderador,
                                     base_socioeconomica,
                                     base_financas,
                                     complem_uniao = 0.1,
                                     condicao_rede = TRUE,
                                     equalizacao_socio = FALSE,
                                     fatores_intra_equidade = FALSE,
                                     crescimento_economico,
                                     crescimento_demografico,
                                     min_social = 1,
                                     max_social = 1.3,
                                     min_disp_fiscal = 1,
                                     max_disp_fiscal = 1.3,
                                     var_socioeconomica = nse,
                                     considerar = "ambos",
                                     ...
){
  lista_fundos <- purrr::map(cumprod(crescimento_economico + 1), ~dplyr::mutate(base_financas, fundeb = fundeb * .x, demais_receitas = demais_receitas * .x))
  lista_alunos <- purrr::map(cumprod(crescimento_demografico + 1), ~dplyr::mutate(base_alunos, alunos = alunos * .x))

  purrr::pmap_dfr(
    .l = list(
      ls_alunos = lista_alunos,
      ls_fundos = lista_fundos,
      ls_auxilio = complem_uniao
    ),
    .f = function(ls_alunos, ls_fundos, ls_auxilio){simular_modelo_vaat(
      ls_alunos,
      ponderador,
      base_socioeconomica,
      ls_fundos,
      complem_uniao = ls_auxilio,
      equalizacao_socio = equalizacao_socio,
      fatores_intra_equidade = fatores_intra_equidade,
      min_social = min_social,
      max_social = max_social,
      min_disp_fiscal = min_disp_fiscal,
      max_disp_fiscal = max_disp_fiscal,
      var_socioeconomica = {{var_socioeconomica}},
      considerar = considerar,
      condicao_rede = condicao_rede,
      ...
    )},
    .id = "ano"
  )
}
