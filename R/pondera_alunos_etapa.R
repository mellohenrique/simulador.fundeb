#' @title Pondera matriculas por etapa
#'
#' @description Pondera dados de matriculas do FNDE por ente e etapa e retorna os valores ponderados por etapa
#'
#' @inheritParams simula_fundeb
#'
#' @return Um data.frame ou data.table com os dados de matriculas considerando os pesos dados de FNDE em formato longo ou curto


pondera_matriculas_etapa <- function(dados_matriculas, dados_peso){

  # Remove ibge e transforma em matriz
  dados_matriculas_sem_ibge = dados_matriculas[order(dados_matriculas$ibge), !names(dados_matriculas) %in% c('ibge')]
  matriz_matriculas = as.matrix(dados_matriculas_sem_ibge[, dados_peso$etapa])

  # Calcula matriculas ponderados por peso vaaf e vaat
  matriculas_vaaf = matriz_matriculas %*% dados_peso$peso_vaaf
  matriculas_vaat = matriz_matriculas %*% dados_peso$peso_vaat

  # Gera tabela resultado
  df_matriculas = data.frame(ibge = dados_matriculas$ibge[order(dados_matriculas$ibge)],
             matriculas_vaaf = matriculas_vaaf,
             matriculas_vaat = matriculas_vaat)

  # Retorna resultado
  return(df_matriculas)

}
