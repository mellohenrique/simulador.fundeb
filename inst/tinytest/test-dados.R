# Teste das bases de dados

## Teste de estrutura
expect_equal(class(dados_teste),
             "data.frame")
expect_equal(class(peso),
             "data.frame")
expect_equal(class(complementar_teste),
             "data.frame")

## Teste de dimensoes
expect_equal(dim(dados_teste),
             c(76,32))
expect_equal(dim(peso),
             c(29,5))
expect_equal(dim(complementar_teste),
             c(76,7))

## Teste de caracteristicas
expect_true(all(peso$etapa %in% names(dados_teste)))
expect_equal(unique(dados_teste$uf),
             c("AC", "RO"))
expect_equal(sapply(complementar_teste, class, simplify = TRUE),
             c("integer", "numeric", "numeric", "character", "character", "numeric", "numeric"))
