#' @title Simulação do modelo Fundeb de financiamento da educação
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação
#'
#' @inheritParams pondera_geral
#' @param complem_uniao percentual do fundo que a União complementará
#' @param equalizacao_socio parametro lógico que controla se a equalização do fundo considerara o vetor de alunos ou de alunos socioeconomico
#' @param fatores_intra_equidade parametro logico que controla se a distribuicao do fundo estadual considerara o vetor de alunos ou de alunos socioeconomico
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

simular_modelo_fundeb <- function(base_alunos,
                                  ponderador,
                                  base_socioeconomica,
                                  base_financas,
                                  condicao_rede = TRUE,
                                  complem_uniao = 0.1,
                                  equalizacao_socio = FALSE,
                                  fatores_intra_equidade = FALSE,
                                  min_social = 1,
                                  max_social = 1.3,
                                  min_disp_fiscal = 1,
                                  max_disp_fiscal = 1.3,
                                  var_socioeconomica = nse,
                                  considerar = "ambos",
                                  ...
){

  dados <- pondera_geral(
    base_alunos,
    ponderador_alunos,
    base_socioeconomica,
    base_financas,
    min_social = min_social,
    max_social = max_social,
    min_disp_fiscal = min_disp_fiscal,
    max_disp_fiscal = max_disp_fiscal,
    var_socioeconomica = {{var_socioeconomica}},
    considerar = considerar,
    condicao_rede = condicao_rede,
  )
  dados_estaduais <- gera_dados_estaduais(dados)
  aporte_federal <- complem_uniao * calcula_fundo_total(dados)

  if (equalizacao_socio) {
    financiamento_estado <-
      equaliza_modelo(
        dados_estaduais,
        fundo = fundo_estadual,
        aporte = aporte_federal,
        var_alunos = alunos_estado_socio,
        codigo = codigo_estado
      )
  } else {
    financiamento_estado <-
      equaliza_modelo(
        dados_estaduais,
        fundo = fundo_estadual,
        aporte = aporte_federal,
        codigo = codigo_estado
      )
  }

  dados %>%
    dplyr::left_join(financiamento_estado) %>%
    dplyr::group_by(codigo_estado) %>%
    dplyr::mutate(
      fundeb_recebido = dplyr::case_when(
        fatores_intra_equidade ~ fundo_equalizado * (alunos_socioeco / sum(alunos_socioeco)),
        TRUE ~ fundo_equalizado * (alunos / sum(alunos))
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      recursos_totais = fundeb_recebido + demais_receitas,
      vaa_final = dplyr::case_when(
        fatores_intra_equidade ~ recursos_totais / alunos_socioeco,
        TRUE ~ recursos_totais / alunos
      )
    )
}
