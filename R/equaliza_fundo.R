#' @title Equaliza fundo
#' @description Recebe dados dos entes do fnde, divididos em entes federativos ou fundos estaduais, e equaliza o valor recebido de acordo com o vaa em questao. A equalizacao e feita de baixo para cima.
#'
#' @param dados Um objeto da classe data.frame com os dados do fundo a serem equalizados
#' @param complemento Numero com a quantidade de fundos a serem utilizados na equalizacao
#' @param var_ordem Variavel que sera utilizada para ordenar os entes
#' @param var_alunos Variavel que sera utilizada como numero de alunos a serem considerados do ente
#' @param var_recursos Variavel com as receitas que seram equalizadas
#'
#' @return Um data.frame

equaliza_fundo <- function(dados, complemento, var_ordem, var_alunos, var_recursos, identificador, entes_excluidos = NULL){

  ## Remove entes que nao fazem parte da equalizacao
  if (!is.null(entes_excluidos)){
    dados_entes_excluidos = dados[ibge %in% entes_excluidos,]
    dados = dados[!ibge %in% entes_excluidos,]
  }

  ## Limpeza
  ### Ordena pela variavel de ordem
  dados = dados[order(dados[,var_ordem]),]

  ### Gera dados de alunos acumulados, recursos acumulados e acomplementacao necessaria para equalizacao
  dados$alunos_acumulados = cumsum(dados[,var_alunos])
  dados$recursos_acumulados = cumsum(dados[,var_recursos])
  dados$complementacao_necessaria = dados$alunos_acumulados * dados[,var_ordem] - dados$recursos_acumulados

  # Define entes que serao complementados e os que nao seres
  ## Define vetor com identificadores de complementacao
  entes_complementados = dados['complementacao_necessaria']  < complemento
  complementar = dados[entes_complementados,]

  ## Remove entes nao complementados
  if (!is.null(entes_excluidos)){
    nao_complementar = rbind(
      dados[!entes_complementados,],
      dados_entes_excluidos)
  } else {
    nao_complementar = dados[!entes_complementados,]
  }

  # Complementacao ----
  complementar$recursos_pos = complementar[,var_alunos] * (sum(complementar[,var_recursos]) + complemento)/ sum(complementar[,var_alunos])

  # Define recursos pos complementacao
  complementar$recursos_pos / complementar$alunos_vaaf
  nao_complementar$recursos_pos = nao_complementar$recursos_vaaf

  # Une tabelas
  dados_unidos = rbind(nao_complementar,
        complementar[, names(nao_complementar)])

  # Seleciona colunas
  dados_final = dados_unidos[, c(identificador, 'recursos_pos')]

  # Retorna resultados
  return(dados_final)
}
