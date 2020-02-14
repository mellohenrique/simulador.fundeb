#' @title Simulação do modelo Fundeb de financiamento da educação
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param base_socioeconomica data.frame com  o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param base_financas data.frame com o codigo do ibge e os valores do fundeb e das demais receitas
#' @param auxilio_federal percentual do fundo que a União complementará
#' @param equalizacao_socio parametro lógico que controla se a equalização do fundo considerara o vetor de alunos ou de alunos socioeconomico
#' @param distribuicao_fundo_estadual_socio parametro logico que controla se a distribuicao do fundo estadual considerara o vetor de alunos ou de alunos socioeconomico
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
#'

simular_modelo_fundeb <- function(base_alunos,
                                  ponderador,
                                  base_socioeconomica,
                                  base_financas,
                                  auxilio_federal = 0.1,
                                  equalizacao_socio = FALSE,
                                  distribuicao_fundo_estadual_socio = FALSE,
                                  min_social = 1,
                                  max_social = 1.3,
                                  min_financas = 1,
                                  max_financas = 1.3,
                                  ...
){

  dados <- pondera_geral(base_alunos, ponderador_alunos, base_socioeconomica, base_financas, min_social = min_social, max_social = max_social, min_financas = min_financas, max_financas = max_financas)
  dados_estaduais <- gera_dados_estaduais(dados)
  aporte_federal <- auxilio_federal * calcula_fundo_total(dados)

  if(equalizacao_socio) {
    financiamento_estado <- equaliza_modelo(dados_estaduais, fundo_estadual, aporte = aporte_federal, var_alunos = alunos_estado_socio, codigo = codigo_estado)
  } else {
    financiamento_estado <- financiamento_estado <- equaliza_modelo(dados_estaduais, aporte = aporte_federal, fundo_estadual, codigo = codigo_estado)
  }

  dados %>%
    dplyr::left_join(financiamento_estado) %>%
    dplyr::group_by(codigo_estado) %>%
    dplyr::mutate(
      fundeb_recebido = dplyr::case_when(
        distribuicao_fundo_estadual_socio ~ fundo_equalizado * (alunos_socioeco / sum(alunos_socioeco)),
        TRUE ~ fundo_equalizado * (alunos / sum(alunos)))) %>%
  dplyr::ungroup() %>%
    dplyr::mutate(
      recursos_totais = fundeb_recebido + demais_receitas,
        vaa_final = dplyr::case_when(
          distribuicao_fundo_estadual_socio ~ recursos_totais / alunos_socioeco,
          TRUE ~ recursos_totais / alunos))
}
