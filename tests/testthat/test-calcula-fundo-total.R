test_that("calcula_fundo_total funciona", {
  expect_equal(calcula_fundo_total(financas_teste), 150)
  expect_equal(calcula_fundo_total(financas_teste, demais_receitas), 100)
})

test_that("calcula_fundo_total gera vetor de tamanho 1", {
  expect_length(calcula_fundo_total(financas_teste), 1)
})
