# Teste das bases de dados

## Teste de estrutura
expect_equal(class(dados_teste),
             "data.frame")
expect_equal(class(peso),
             "data.frame")

## Teste de dimensoes
expect_equal(dim(dados_teste),
             c(78,33))
expect_equal(dim(peso),
             c(29,4))

## Teste de caracteristicas
expect_true(all(peso$etapa %in% names(dados_teste)))
expect_equal(unique(dados_teste$uf),
             c("AC", "RO"))
