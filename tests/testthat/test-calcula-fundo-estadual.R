test_that("multiplication works", {
  expect_equal(calcula_fundo_estadual(financas_teste)$fundo_estadual, c(30, 70, 50))
})
