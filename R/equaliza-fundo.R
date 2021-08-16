#' @title Equaliza fundo
#' @description Recebe dados dos entes do fnde, divididos em entes federativos ou fundos estaduais, e equaliza o valor recebido de acordo com o vaa em questao. A equalizacao e feita de baixo para cima.
#'
#' @inheritParams limpa_fnde
#' @param dados Um objeto da classe data.table ou data.frame com os dados do fundo a serem equalizados
#' @param complementacao Numero com a quantidade de fundos a serem utilizados na equalizacao
#' @param var_ordem Variavel que sera utilizada para ordenar os entes
#' @param var_alunos Variavel que sera utilizada como numero de alunos a serem considerados do ente
#' @param var_receitas Variavel com as receitas que seram equalizadas
#'
#' @return Um data.frame ou data.table
#'
#' @import data.table
#'

equaliza_fundo <- function(dados, complementacao, var_ordem, var_alunos, var_receitas, entes_excluidos = NULL, produto_dt = TRUE){
  # Binding variables para NULL
  equalizacao = receitas_etapa = vaa_etapa = NULL

  dados = checa_transforma_dt(dados)

  setorderv(dados, var_ordem)

  if (complementacao == 0){
  dados[, `:=`(equalizacao = round(get(var_ordem) * cumsum(get(var_alunos)) - cumsum(get(var_receitas)), digits = 2) < complementacao)] } else {
  dados[, `:=`(equalizacao = get(var_ordem) * cumsum(get(var_alunos)) - cumsum(get(var_receitas)) <= complementacao)]
  }

  if (!is.null(entes_excluidos)){
    dados[, equalizacao = ifelse(ibge %in% entes_excluidos, FALSE, equalizacao)]
  }

  equalizado = dados[equalizacao == TRUE,]
  nao_equalizado = dados[equalizacao == FALSE,]

  equalizado[, receitas_etapa := ((sum(get(var_receitas)) + complementacao)*get(var_alunos))/sum(get(var_alunos))]
  nao_equalizado[, receitas_etapa := get(var_receitas)]

  dados_etapa = rbind(equalizado, nao_equalizado)

  dados_etapa[, vaa_etapa := receitas_etapa/get(var_alunos)]

  retorna_dt_df(dados_etapa, produto_dt)
}
