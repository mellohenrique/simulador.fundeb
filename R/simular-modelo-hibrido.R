#' @title Simulação do modelo híbrido de financiamento da educação
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo híbrido de financiamento da educação
#'
#' @inheritParams simular_modelo_fundeb
#' @param complem_uniao_vat percentual do fundo que a União complementará na segunda etapa, na equalizacao dos entes pelo valor vaa
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
           complem_uniao = 0.1,
           complem_uniao_vat = 0.05,
           condicao_rede = TRUE,
           equalizacao_socio = FALSE,
           fatores_intra_equidade = FALSE,
           min_social = 1,
           max_social = 1.2,
           min_disp_fiscal = 1,
           max_disp_fiscal = 1.2,
           var_socioeconomica = nse,
           considerar = "ambos",
           desconsidera_estados = FALSE,
           ...) {
    dados <-
      simular_modelo_fundeb(
        base_alunos,
        ponderador,
        base_socioeconomica,
        base_financas,
        complem_uniao = complem_uniao,
        fatores_intra_equidade = fatores_intra_equidade,
        equalizacao_socio = equalizacao_socio,
        min_social = min_social,
        max_social = max_social,
        min_disp_fiscal = min_disp_fiscal,
        max_disp_fiscal = max_disp_fiscal,
        var_socioeconomica = {{var_socioeconomica}},
        considerar = considerar,
        condicao_rede = condicao_rede,
        desconsidera_estados = desconsidera_estados,
        ...
      )

    aporte_federal <-
      complem_uniao_vat * calcula_fundo_total(dados, fundeb)

    if (fatores_intra_equidade) {
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
        vaa_final = recursos_vat / aluno_imponderado
        )

  }
