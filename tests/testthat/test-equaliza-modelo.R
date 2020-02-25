test_that("equaliza_modelo funciona", {
  expect_equal(
    pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>%
      equaliza_modelo(fundeb, aporte = 30, var_alunos = alunos) %>%
      dplyr::pull(fundo_equalizado) %>%
      sum,
    180)
  expect_equal(
    pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>%
      equaliza_modelo(fundeb, aporte = 30, var_alunos = alunos) %>%
      dplyr::pull(fundo_equalizado),
    c(24.7238296, 25.8285113, 49.4476591, 50.00000, 30.00000))
})

test_that("equaliza_modelo gera data.frame", {
  expect_output(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% equaliza_modelo(fundeb, aporte = 30, var_alunos = alunos) %>% str(), "data\\.frame")
})
