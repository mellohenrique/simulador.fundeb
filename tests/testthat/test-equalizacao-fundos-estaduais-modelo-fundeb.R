teste_equalizacao_fundos_estaduais <- function(social = FALSE){
  if(social) {
    simular_modelo_fundeb(alunos_2015, ponderador_alunos, socioeco_2015, financas_2015, equalizacao_socio = TRUE) %>%
      dplyr::group_by(estado) %>%
      dplyr::summarise(alunos = sum(alunos_socioeco),
                       vaa = sum(fundeb_recebido) / alunos,
                       recebeu = sum(fundeb_recebido) - sum(fundeb) > 0.00001) %>%
      dplyr::filter(recebeu) %>%
      dplyr::pull(vaa)
  } else {
    simular_modelo_fundeb(alunos_2015, ponderador_alunos, socioeco_2015, financas_2015) %>%
    dplyr::group_by(estado) %>%
    dplyr::summarise(alunos = sum(alunos),
              vaa = sum(fundeb_recebido) / alunos,
              recebeu = sum(fundeb_recebido) - sum(fundeb) > 0.00001) %>%
    dplyr::filter(recebeu) %>%
    dplyr::pull(vaa)}
}

test_that("Todos os estados equalizados recebem o mesmo valor na funcao simula_modelo_fundeb", {
          expect_equal(
            all(teste_equalizacao_fundos_estaduais() - teste_equalizacao_fundos_estaduais()[1] < 0.00001), TRUE)
          expect_equal(
            all(teste_equalizacao_fundos_estaduais(social = TRUE) - teste_equalizacao_fundos_estaduais(social = TRUE)[1]  < 0.00001), TRUE)
          })
