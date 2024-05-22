# Teste das bases de dados

## Teste de estrutura
expect_equal(class(teste_matriculas),
             "data.frame")
expect_equal(class(teste_peso),
             "data.frame")
expect_equal(class(teste_complementar),
             "data.frame")

## Teste de dimensoes
expect_equal(dim(teste_matriculas),
             c(76,30))
expect_equal(dim(teste_peso),
             c(29,5))
expect_equal(dim(teste_complementar),
             c(76,9))

## Teste de caracteristicas
expect_true(all(teste_peso$etapa %in% names(teste_matriculas)))
expect_equal(sort(unique(as.integer(substr(teste_matriculas$ibge, 1, 2)))),
             11:12)
expect_equal(sapply(teste_complementar, class, simplify = TRUE),
             c(ibge = "integer",  uf = "character", nome = "character", recursos_vaaf = "numeric", recursos_vaat = "numeric", nse = "numeric", nf = 'numeric', peso_vaar = "numeric", inabilitados_vaat = "logical"))
