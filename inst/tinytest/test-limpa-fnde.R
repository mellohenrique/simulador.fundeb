# Teste da funcao limpa FNDE
dados_teste_limpo <- simulador.fundeb2::limpa_fnde(dados_teste, produto_dt = FALSE)
dados_teste_limpo_dt <- simulador.fundeb2::limpa_fnde(dados_teste, produto_dt = TRUE)

## Testes de estrutura
expect_equal(class(dados_teste_limpo),
             c("data.frame"))
expect_equal(class(dados_teste_limpo_dt),
             c("data.table", "data.frame"))
expect_equal(dim(dados_teste_limpo),
             c(76,32))
expect_equal(dim(dados_teste_limpo_dt),
             c(76,32))

## Teste observando se tem a coluna Coeficiente de Distribuicao
expect_equal(any(names(dados_teste_limpo) == "Coeficiente de Distribuição"),
             FALSE)
expect_equal(any(names(dados_teste_limpo_dt) == "Coeficiente de Distribuição"),
             FALSE)

## Teste observando se tem a linha Total Geral
expect_equal(any(dados_teste_limpo$`Município` == "TOTAL GERAL"),
             FALSE)
expect_equal(any(dados_teste_limpo_dt$`Município` == "TOTAL GERAL"),
             FALSE)

# Teste da funcao financas_fnde
financas <- financas_fnde(dados_teste_limpo, produto_dt = FALSE)
financas_dt <- financas_fnde(dados_teste_limpo_dt, produto_dt = TRUE)

## Testes de estrutura
expect_equal(class(financas),
             c("data.frame"))
expect_equal(class(financas_dt),
             c("data.table", "data.frame"))
expect_equal(dim(financas),
             c(76,3))
expect_equal(dim(financas_dt),
             c(76,3))

## Teste observando os nomes das colunas
expect_equal(names(financas),
             c("UF", "Município", "Estimativa de Receitas"))
expect_equal(names(financas_dt),
             enc2utf8(c("UF", "Município", "Estimativa de Receitas")))

