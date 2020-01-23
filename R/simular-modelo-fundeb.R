#' @title Simulação do modelo Fundeb de financiamento da educação
#'
#' @description Recebe uma base com numero de alunos por ente e por etapa, ponderador por etapa, dados socioeconomicos por ente e dados financeiros por ente e simula o modelo fundeb de financiamento da educação
#'
#' @param base_alunos data.frame de numero de alunos por etapa e ente federativo
#' @param ponderador data.frame de peso de aluno por etapa
#' @param base_socioeconomica data.frame com  o codigo do ibge, o numero de alunos ponderado e uma variavel socioeconomia a ponderar
#' @param base_financas data.frame com o codigo do ibge e os valores do fundeb e das demais receitas
#' @param auxilio_federal percentual do fundo que a União complementará
#' @param var_fundo parametro com o nome da variavel do fundo a ser considerado
#' @param var_alunos parametro com o nome da variavel alunos a ser considerada
#'
#' @return Data.frame com alunos ponderador por ente federativo
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

simular_modelo_fundeb <- function(base_alunos, ponderador, base_socioeconomica, base_financas, auxilio_federal = 0.1, var_fundo = fundeb, var_alunos = alunos, equalizacao_socio = FALSE, distribuicao_fundo_estadual_socio = FALSE, ...) {
  dados <- simulador.fundeb::pondera_geral(alunos, ponderador_alunos, info_socioeco, financas)
  dados_estaduais <- gera_dados_estaduais(dados)
  aporte_federal <- auxilio_federal * calcula_fundo_total(dados)

  if(equalizacao_socio) {
    financiamento_estado <-
      prepara_equalizacao(dados_estaduais) %>%
      equaliza_modelo(var_alunos = alunos_estado_socio, codigo = codigo_estado, aporte = aporte_federal)
  } else {
    financiamento_estado <-
      prepara_equalizacao(dados_estaduais) %>%
      equaliza_modelo(codigo = codigo_estado, aporte = aporte_federal)
  }

  dados %>%
    dplyr::left_join(financiamento_estado) %>%
    dplyr::group_by(codigo_estado) %>%
    dplyr::mutate(
      vaa_fundeb = ifelse(distribuicao_fundo_estadual_socio,
        (total_fundo_estado * (alunos_socioeco / sum(alunos_socioeco)))/alunos_socioeco,
        (total_fundo_estado * (alunos / sum(alunos)))/alunos),
        vaa_final = ifelse(distribuicao_fundo_estadual_socio,
        vaa_fundeb + demais_receitas / alunos_socioeco,
        vaa_fundeb + demais_receitas / alunos)) %>%
    ungroup()
}
