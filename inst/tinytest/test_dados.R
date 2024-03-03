# Teste das bases de dados

## Teste de estrutura
expect_equal(class(dados_teste),
             "data.frame")
expect_equal(class(peso),
             "data.frame")
expect_equal(class(dados_complementar),
             "data.frame")

## Teste de dimensoes
expect_equal(dim(dados_teste),
             c(76,32))
expect_equal(dim(peso),
             c(29,5))
expect_equal(dim(dados_complementar),
             c(76,8))

## Teste de caracteristicas
expect_true(all(peso$etapa %in% names(dados_teste)))
expect_equal(unique(dados_teste$uf),
             c("AC", "RO"))
expect_equal(sapply(dados_complementar, class, simplify = TRUE),
             c(ibge = "integer",  recursos_extra = "numeric", fator_fiscal = "numeric", nome = "character", uf = "character", fator_social = "numeric", fundeb_vaaf = "numeric", fundeb_vaat = "numeric"))
