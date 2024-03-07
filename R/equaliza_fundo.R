#' @title Equaliza fundo
#' @description Recebe dados dos entes do fnde, divididos em entes federativos ou fundos estaduais, e equaliza o valor recebido de acordo com o vaa em questao. A equalizacao e feita de baixo para cima.
#'
#' @param dados Um objeto da classe data.frame com os dados do fundo a serem equalizados
#' @param complementacao_uniao Numero com a quantidade de fundos a serem utilizados na equalizacao
#' @param var_ordem Variavel que sera utilizada para ordenar os entes
#' @param var_alunos Variavel que sera utilizada como numero de alunos a serem considerados do ente
#' @param var_recursos Variavel com as receitas que seram equalizadas
#' @param identificador Identificador unico para cada ente que estara sendo equalizado
#' @param entes_excluidos Vetor com o identificador de cada entre que sera removido da equalizacao
#'
#' @return Um data.frame

equaliza_fundo <- function(dados, complementacao_uniao, var_ordem, var_alunos, var_recursos, identificador, entes_excluidos = NULL){

  ## Remove entes que nao fazem parte da equalizacao
  if (!is.null(entes_excluidos)){
    df_entes_excluidos = dados[dados$ibge %in% entes_excluidos,]
    df = dados[!dados$ibge %in% entes_excluidos,]
  }

  ## Limpeza
  ### Ordena pela variavel de ordem
  df = dados[order(dados[,var_ordem]),]

  ### Gera dados de alunos acumulados, recursos acumulados e acomplementacao necessaria para equalizacao
  df$alunos_acumulados = cumsum(df[,var_alunos])
  df$recursos_acumulados = cumsum(df[,var_recursos])
  df$complementacao_necessaria = df$alunos_acumulados * df[,var_ordem] - df$recursos_acumulados

  # Define entes que serao complementados e os que nao seres
  ## Define vetor com identificadores de complementacao
  entes_complementados = df['complementacao_necessaria']  < complementacao_uniao
  df_complementar = df[entes_complementados,]

  ## Remove entes nao complementados
  if (!is.null(entes_excluidos)){
    df_nao_complementar = rbind(
      df[!entes_complementados,],
      df_entes_excluidos)
  } else {
    df_nao_complementar = df[!entes_complementados,]
  }

  # Complementacao ----
  df_complementar$recursos_pos = df_complementar[,var_alunos] * (sum(df_complementar[,var_recursos]) + complementacao_uniao)/ sum(df_complementar[,var_alunos])

  # Define recursos pos complementacao
  df_nao_complementar$recursos_pos = df_nao_complementar[,var_recursos]

  # Une tabelas
  df_final = rbind(df_nao_complementar, df_complementar[, names(df_nao_complementar)])

  # Seleciona colunas
  df_final = df_final[, c(identificador, 'recursos_pos')]

  # Retorna resultados
  return(df_final)
}
