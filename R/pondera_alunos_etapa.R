#' @title Pondera alunos por etapa
#'
#' @description Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto


pondera_alunos_etapa <- function(dados_alunos, dados_peso){

  # Remove ibge e transforma em matriz
  dados_alunos_sem_ibge = dados_alunos[order(dados_alunos$ibge), !names(dados_alunos) %in% c('ibge')]
  matriz_alunos = as.matrix(dados_alunos_sem_ibge[, dados_peso$etapa])

  # Calcula alunos ponderados por peso vaaf e vaat
  alunos_vaaf = matriz_alunos %*% dados_peso$peso_vaaf
  alunos_vaat = matriz_alunos %*% dados_peso$peso_vaat

  # Gera tabela resultado
  df_alunos = data.frame(ibge = dados_alunos$ibge[order(dados_alunos$ibge)],
             alunos_vaaf = alunos_vaaf,
             alunos_vaat = alunos_vaat)

  # Retorna resultado
  return(df_alunos)

}
