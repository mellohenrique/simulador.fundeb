#' @title Une a equalizacao vaaf com a tabela de entes
#'
#' @description Une a equalização do fundo etapa vaaf com a tabela dos entes e calcula o valor aluno ano da etapa
#'
#' @param df_entes data.frame com os dados dos entes federativos
#' @param df_estados data.frame com os dados dos fundos estaduais
#' @param df_fundos_estaduais data.frame com os dados da equalizacao da etapa dos fundos estaduais
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE

une_vaaf <- function(df_entes,
                     df_estados,
                     df_fundos_estaduais){

  # Une bases
  df = merge(df_entes, df_estados[, c('uf', 'alunos_estado_vaaf')], all.x = TRUE, by = 'uf')
  df = merge(df, df_fundos_estaduais, all.x = TRUE, by = 'uf')

  # Calcula valores
  df$recursos_vaaf_final = df$alunos_vaaf * df$recursos_pos / df$alunos_estado_vaaf
  df$vaaf_final = df$recursos_vaaf_final / df$alunos_vaaf

  # Remove colunas indesejadas
  df$alunos_estado_vaaf  = NULL
  df$recursos_pos = NULL

  # Retorna resultados
  return(df)
}
