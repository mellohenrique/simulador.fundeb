#' @title Une a equalizacao vaaf com a tabela de entes
#'
#' @description Une a equalização do fundo etapa vaaf com a tabela dos entes e calcula o valor aluno ano da etapa
#'
#' @inheritParams simula_fundeb
#' @param dados_entes data.frame ou data.table com os dados dos entes federativos
#' @param dados_etapa data.frame ou data.table com os dados da equalizacao da etapa
#'
#' @return Um data.frame ou data.table com a simulacao dos dados do FNDE

une_vaaf <- function(dados_entes,
                     dados_etapa){

  dados_entes[dados_etapa,
              `:=`(fundo_estadual_vaaf = receitas_etapa,
                   equalizacao_vaaf = equalizacao),
              on = .(uf)]


   dados_entes[,`:=`(
     vaaf = fundo_estadual_vaaf/sum(alunos_vaaf),
     fundo_vaaf = fundo_estadual_vaaf*alunos_vaaf/sum(alunos_vaaf)),
                by = .(uf)]

  return(dados_entes)
}
