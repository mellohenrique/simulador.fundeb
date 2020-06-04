teste_equalizacao_ente <- function(social = FALSE){
  if(social) {
    simular_modelo_vaat(alunos_2015, ponderador_alunos, socioeco_2015, financas_2015, equalizacao_socio = TRUE) %>%
      dplyr::filter(recursos_complementados != recursos_totais) %>%
      dplyr::mutate(vaa = recursos_complementados / alunos_socioeco) %>%
      dplyr::pull(vaa)
  } else {
    simular_modelo_vaat(alunos_2015, ponderador_alunos, socioeco_2015, financas_2015) %>%
      dplyr::filter(recursos_complementados != recursos_totais) %>%
      dplyr::mutate(vaa = recursos_complementados / alunos) %>%
      dplyr::pull(vaa)
  }
}

test_that("Todos os entes equalizados recebem o mesmo valor na funcao simula_modelo_fundeb", {
    expect_equal(
      all(teste_equalizacao_ente() - teste_equalizacao_ente()[1] < 0.00001), TRUE)
    expect_equal(
      all(teste_equalizacao_ente(social = TRUE) - teste_equalizacao_ente(social = TRUE)[1]  < 0.00001), TRUE)
  })

