#' @title Une a equalizacao vaaf com a tabela de entes
#'
#' @description Une a equalização do fundo etapa vaaf com a tabela dos entes e calcula o valor aluno ano da etapa
#'
#' @inheritParams pondera_alunos_etapa
#' @param dados_entes data.frame ou data.table com os dados dos entes federativos
#' @param dados_fundo data.frame ou data.table com os dados da equalizacao da etapa
#' @param etapa variavel em caracteres que pode ter os valores "vaat" ou "vaaf" a depender da etapa do calculo
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE
#'
#' @import data.table
#'


une_vaaf <- function(dados_entes,
                     dados_etapa,
                     produto_dt = TRUE){

  dados_entes = checa_transforma_dt(dados_entes)
  dados_etapa = checa_transforma_dt(dados_etapa)

  dados_entes[dados_etapa,
              `:=`(fundo_estadual_vaaf = receitas_etapa,
                   equalizacao_vaaf = equalizacao),
              on = .(uf)]


   dados_entes[,`:=`(
     vaaf = fundo_estadual_vaaf/sum(alunos_ponderados),
     fundo_vaaf = fundo_estadual_vaaf*alunos_ponderados/sum(alunos_ponderados)),
                by = .(uf)]

  return(dados_entes)
}
