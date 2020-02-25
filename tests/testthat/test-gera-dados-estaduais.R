test_that("gera_dados_estaduais funciona", {
  expect_equal(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% gera_dados_estaduais() %>% dplyr::pull(alunos_estado),
               c(48.05, 71.55, 47))
  expect_equal(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% gera_dados_estaduais() %>% dplyr::pull(alunos_estado_socio), c(64.2650, 88.4895, 47))
  expect_equal(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% gera_dados_estaduais() %>% dplyr::pull(fundo_estadual), c(30, 70, 50))
})

test_that("gera_dados_estaduais gera data.frame", {
  expect_output(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% gera_dados_estaduais() %>% str(), "data\\.frame")
})
