#' @title Une a equalizacao vaaf com a tabela de entes
#'
#' @description Une a equalização do fundo etapa vaaf com a tabela dos entes e calcula o valor aluno ano da etapa
#'
#' @inheritParams gera_fundo_estadual
#' @param dados_estados data.frame com os dados dos fundos estaduais
#' @param dados_fundos_estaduais data.frame com os dados da equalizacao da etapa dos fundos estaduais
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE

une_vaaf <- function(dados_entes,
                     dados_estados,
                     dados_fundos_estaduais){

  # Une bases
  df = merge(dados_entes, dados_estados[, c('uf', 'matriculas_estado_vaaf')], all.x = TRUE, by = 'uf')
  df = merge(df, dados_fundos_estaduais, all.x = TRUE, by = 'uf')

  # Calcula valores
  df$recursos_vaaf_final = df$matriculas_vaaf * df$recursos_pos / df$matriculas_estado_vaaf
  df$vaaf_final = df$recursos_vaaf_final / df$matriculas_vaaf

  # Remove colunas indesejadas
  df$matriculas_estado_vaaf  = NULL
  df$recursos_pos = NULL

  # Retorna resultados
  return(df)
}
