test_that("pondera_geral funciona", {
  expect_equal(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% dplyr::pull(alunos), c(23.5, 47, 47, 24.55, 24.55))

  expect_equal(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% dplyr::pull(socioeco), c(1.3, 1, 1, 1, 1.3))

  expect_equal(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, max_disp_fiscal = 2, min_disp_fiscal = 0.2) %>% dplyr::pull(financas), c(2, 0.2, 0.2, 0.2, 2))
})

test_that("pondera_geral gera data.frame", {
  expect_output(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% str, "data\\.frame")
  expect_output(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, condicao_rede = FALSE) %>% str, "data\\.frame")
  expect_output(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, considerar = "social") %>% str, "data\\.frame")
  expect_output(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, considerar = "financas") %>% str, "data\\.frame")
  expect_output(pondera_geral(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, min_social = 0.5, max_social = 2, min_disp_fiscal = 3, max_disp_fiscal = 4) %>% str, "data\\.frame")
})
