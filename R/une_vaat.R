#' @title Une a equalizacao vaat com a tabela de entes
#'
#' @description Une a equalização do fundo etapa vaat com a tabela dos entes e calcula o valor aluno ano da etapa
#'
#' @description Une a equalização do fundo com a tabela dos entes e calcula o valor aluno ano da etapa
#'
#' @inheritParams une_vaaf
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE

une_vaat <- function(dados_entes,
                     dados_complementacao_vaat){

  # Une tabelas
  df = merge(dados_entes, dados_complementacao_vaat, all = TRUE)

  # Corrige nomes
  names(df)[names(df) == 'recursos_pos'] <- 'recursos_vaat_final'

  # Gera calculos necessarios
  df$vaat_final = df$recursos_vaat_final / df$alunos_vaat

  # Retorna resultado
  return(df)
}
