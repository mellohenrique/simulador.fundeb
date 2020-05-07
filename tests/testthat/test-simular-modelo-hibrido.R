test_that("simular_modelo_hibrido gera data.frame", {
  expect_output(simular_modelo_hibrido(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste) %>% str, "data\\.frame")
  expect_output(simular_modelo_hibrido(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, condicao_rede = FALSE) %>% str, "data\\.frame")
  expect_output(simular_modelo_hibrido(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, considerar = "social") %>% str, "data\\.frame")
  expect_output(simular_modelo_hibrido(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, considerar = "financas") %>% str, "data\\.frame")
  expect_output(simular_modelo_hibrido(alunos_teste, ponderador_alunos, socioeco_teste, financas_teste, min_social = 0.5, max_social = 2, min_disp_fiscal = 3, max_disp_fiscal = 4) %>% str, "data\\.frame")
})
