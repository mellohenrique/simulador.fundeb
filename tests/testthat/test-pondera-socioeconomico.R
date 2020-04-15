test_that("pondera_socioeconomico funciona", {
  expect_equal(
    pondera_socioeconomico(alunos_teste, socioeco_teste, financas_teste, considerar = "ambos") %>% dplyr::pull(socioeco),
    c(rep(1.2, 28), rep(1, 28), rep(1.2, 28), rep(1, 56))
  )
  expect_equal(
    pondera_socioeconomico(alunos_teste, socioeco_teste, financas_teste, considerar = "financas") %>% dplyr::pull(socioeco),
    c(rep(1.2, 28), rep(1, 28), rep(1.2, 28), rep(1, 56))
  )
})


test_that("pondera_socioeconomico gera data.frame", {
  expect_output(
    pondera_socioeconomico(alunos_teste, socioeco_teste, financas_teste, considerar = "ambos") %>% str,
    "data\\.frame"
  )
  expect_output(
    pondera_socioeconomico(alunos_teste, socioeco_teste, financas_teste, considerar = "financas") %>% str,
    "data\\.frame"
  )
  expect_output(
    pondera_socioeconomico(alunos_teste, socioeco_teste, financas_teste, considerar = "social") %>% str,
    "data\\.frame"
  )
  expect_output(
    pondera_socioeconomico(
      alunos_teste,
      socioeco_teste,
      financas_teste,
      min_social = 0.5,
      max_social = 2,
      min_disp_fiscal = 3,
      max_disp_fiscal = 4,
      considerar = "ambos"
    ) %>% str,
    "data\\.frame"
  )
})
