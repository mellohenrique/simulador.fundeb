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
                     dados_etapa){

  dados_entes[dados_etapa,
              `:=`(fundo_vaat = receitas_etapa,
                   equalizacao_vaat = equalizacao),
              on = .(uf, ibge)]

  dados_entes[, vaat := fundo_vaat/alunos_vaat]

  return(dados_entes)
}
