#' @title Simulação do modelo VAT de financiamento da educação
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação
#'
#' @inheritParams simular_modelo_fundeb
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

simular_modelo_vaat <-
  function(base_alunos,
           ponderador,
           base_socioeconomica,
           base_financas,
           complem_uniao = 0.1,
           var_fundo = fundeb,
           var_alunos = alunos,
           condicao_rede = TRUE,
           fatores_intra_equidade = FALSE,
           equalizacao_socio = FALSE,
           min_social = 1,
           max_social = 1.2,
           min_disp_fiscal = 1,
           max_disp_fiscal = 1.2,
           var_socioeconomica = nse,
           considerar = "ambos",
           desconsidera_estados = FALSE,
           ...) {
    dados <-
      pondera_geral(
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
        desconsidera_estados = desconsidera_estados
      )
    dados_estaduais <- gera_dados_estaduais(dados)
    aporte_federal <- complem_uniao * calcula_fundo_total(dados)
    financiamento_estado <- dados_estaduais %>%
      dplyr::select(fundo_estadual, codigo_estado)

    dados <- dados %>%
      dplyr::left_join(financiamento_estado) %>%
      dplyr::group_by(codigo_estado) %>%
      dplyr::mutate(
        fundeb_recebido = dplyr::case_when(
          fatores_intra_equidade ~ fundo_estadual * (alunos_socioeco / sum(alunos_socioeco)),
          TRUE ~ fundo_estadual * (alunos / sum(alunos))
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        recursos_totais = fundeb_recebido + demais_receitas,
        vaa_intermediario = dplyr::case_when(
          fatores_intra_equidade ~ recursos_totais / alunos_socioeco,
          TRUE ~ recursos_totais / alunos
        )
      )

    if (equalizacao_socio) {
      fundo_equalizado <-
        equaliza_modelo(
          dados,
          fundo = recursos_totais,
          aporte = aporte_federal,
          var_alunos = alunos_socioeco,
          codigo = ibge
        ) %>%
        dplyr::rename(recursos_complementados = fundo_equalizado)
      dados <- dplyr::left_join(dados, fundo_equalizado) %>%
        dplyr::mutate(vaa_final = recursos_complementados / alunos_imponderado)
    } else {
      fundo_equalizado <-
        equaliza_modelo(
          dados,
          fundo = recursos_totais,
          aporte = aporte_federal,
          var_alunos = alunos,
          codigo = ibge
        ) %>%
        dplyr::rename(recursos_complementados = fundo_equalizado)
      dados <- dplyr::left_join(dados, fundo_equalizado) %>%
        dplyr::mutate(vaa_final = recursos_complementados / alunos_imponderado)
    }
    dados
  }
