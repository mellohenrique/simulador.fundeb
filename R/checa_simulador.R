#' @title Checa dados
#'
#' @description Checa se os objetos usados na simulacao sao adequados para realiza-la
#'
#' @inheritParams simula_fundeb
#'

checa_dados_simulador = function(dados_matriculas, dados_complementar, dados_peso, complementacao_vaaf, complementacao_vaat, complementacao_vaar, teto, chao){


  checa_data_frame(dados_matriculas, 'dados_matriculas')
  checa_data_frame(dados_complementar, 'dados_matriculas')
  checa_data_frame(dados_peso, 'dados_matriculas')

  checa_numerico(complementacao_vaaf, 'complementacao_vaaf')
  checa_numerico(complementacao_vaat, 'complementacao_vaat')
  checa_numerico(complementacao_vaar, 'complementacao_vaar')
  checa_numerico(max_nse, 'max_nse')
  checa_numerico(min_nse, 'min_nse')
  checa_numerico(max_nf, 'max_nf')
  checa_numerico(min_nf, 'min_nf')

  checa_data_frame(dados_matriculas, 'dados_matriculas')
  checa_data_frame(dados_complementar, 'dados_matriculas')
  checa_data_frame(dados_peso, 'dados_matriculas')

  checa_na(complementacao_vaaf, 'complementacao_vaaf')
  checa_na(complementacao_vaat, 'complementacao_vaat')
  checa_na(complementacao_vaar, 'complementacao_vaar')
  checa_na(max_nse, 'max_nse')
  checa_na(min_nse, 'min_nse')
  checa_na(max_nf, 'max_nf')
  checa_na(min_nf, 'min_nf')

}
