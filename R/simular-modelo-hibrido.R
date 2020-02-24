#' @title Simulação do modelo híbrido de financiamento da educação
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo híbrido de financiamento da educação
#'
#' @inheritParams simular_modelo_fundeb
#' @param auxilio_federal_vat percentual do fundo que a União complementará na segunda etapa, na equalizacao dos entes pelo valor vaa
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

simular_modelo_hibrido <-
  function(base_alunos,
           ponderador,
           base_socioeconomica,
           base_financas,
           auxilio_federal = 0.1,
           auxilio_federal_vat = 0.05,
           equalizacao_socio = FALSE,
           distribuicao_fundo_estadual_socio = FALSE,
           min_social = 1,
           max_social = 1.3,
           min_financas = 1,
           max_financas = 1.3,
           var_socioeconomica = nse,
           considerar = "ambos",
           ...) {
    dados <-
      simular_modelo_fundeb(
        base_alunos,
        ponderador,
        base_socioeconomica,
        base_financas,
        auxilio_federal = auxilio_federal,
        distribuicao_fundo_estadual_socio = distribuicao_fundo_estadual_socio,
        equalizacao_socio = equalizacao_socio,
        min_social = min_social,
        max_social = max_social,
        min_financas = min_financas,
        max_financas = max_financas,
        var_socioeconomica = {{var_socioeconomica}},
        considerar = considerar,
        ...
      )

    aporte_federal <-
      auxilio_federal_vat * calcula_fundo_total(dados, fundeb)

    if (distribuicao_fundo_estadual_socio) {
      financiamento <-
        equaliza_modelo(
          dados,
          recursos_totais,
          aporte = aporte_federal,
          var_alunos = alunos_socioeco,
          codigo = ibge
        )
    } else {
      financiamento <-
        equaliza_modelo(
          dados,
          recursos_totais,
          aporte = aporte_federal,
          var_alunos = alunos,
          codigo = ibge
        )
    }

    dados %>%
      dplyr::left_join(financiamento %>%
                         dplyr::rename(recursos_vat = fundo_equalizado)) %>%
      dplyr::rename(vaa_intermediario = vaa_final) %>%
      dplyr::mutate(
        vaa_final = dplyr::case_when(
          distribuicao_fundo_estadual_socio ~ recursos_vat / alunos_socioeco,
          TRUE ~ recursos_vat / alunos
        )
      )
  }
