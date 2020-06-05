#' @title Simulação do modelo hibrido de financiamento da educação no tempo
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação ao longo do tempo. Considera o crescimento demográfico e econômico ao longo do tempo.
#'
#' @inheritParams simular_modelo_fundeb
#' @param crescimento_economico vetor númerico de crescimento economico, especificamente do fundeb e das demais receitas
#' @param crescimento_demografico vetor númerico de crescimento demográfico para alunos
#' @param complem_uniao_vaat percentual do fundo que a União complementará na segunda etapa, na equalizacao dos entes pelo valor vaa
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
                                         complem_uniao = 0.1,
                                         complem_uniao_vaat = 0.05,
                                         condicao_rede = TRUE,
                                         equalizacao_socio = FALSE,
                                         fatores_intra_equidade = FALSE,
                                         crescimento_economico,
                                         crescimento_demografico,
                                         min_social = 1,
                                         max_social = 1.2,
                                         min_disp_fiscal = 1,
                                         max_disp_fiscal = 1.2,
                                         var_socioeconomica = nse,
                                         considerar = "ambos",
                                         desconsidera_estados = FALSE,
                                         ...
){
  lista_fundos <- purrr::map(cumprod(crescimento_economico + 1), ~dplyr::mutate(base_financas, fundeb = fundeb * .x, demais_receitas = demais_receitas * .x))
  lista_alunos <- purrr::map(cumprod(crescimento_demografico + 1), ~dplyr::mutate(base_alunos, alunos = alunos * .x))

  purrr::pmap_dfr(
    .l = list(
      ls_alunos = lista_alunos,
      ls_fundos = lista_fundos,
      ls_auxilio = complem_uniao,
      ls_auxilio_vat = complem_uniao_vaat
    ),
    .f = function(ls_alunos, ls_fundos, ls_auxilio, ls_auxilio_vat)
      {simular_modelo_hibrido(
      ls_alunos,
      ponderador,
      base_socioeconomica,
      ls_fundos,
      complem_uniao = ls_auxilio,
      complem_uniao_vaat = ls_auxilio_vat,
      equalizacao_socio = equalizacao_socio,
      fatores_intra_equidade = fatores_intra_equidade,
      min_social = min_social,
      max_social = max_social,
      min_disp_fiscal = min_disp_fiscal,
      max_disp_fiscal = max_disp_fiscal,
      var_socioeconomica = {{var_socioeconomica}},
      considerar = considerar,
      condicao_rede = condicao_rede,
      desconsidera_estados = desconsidera_estados,
      ...
    )},
    .id = "ano"
  )
}
