test_that("pondera_alunos_etapa funciona", {
  expect_equal(pondera_alunos_etapa(alunos_teste, ponderador_alunos) %>% dplyr::pull(alunos), c(32.3, 64.6, 64.6, 32.3, 32.3))
})

test_that("pondera_alunos_etapa gera data.frame", {
  expect_output(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% equaliza_modelo(fundeb, aporte = 30, var_alunos = alunos) %>% str(), "data\\.frame")
})

