test_that("calcula-alunos-estadual funciona", {
  expect_equal(calcula_alunos_estadual(alunos_teste)$alunos_estado, c(56, 84, 56))
})
