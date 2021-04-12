# Teste da funcao simula fnde

## Preparação
fnde <- limpa_fnde(dados_teste)

df_teste <- simula_fundeb(fnde, aporte = 1e5, produto_dt = FALSE)
dt_teste <- simula_fundeb(fnde, aporte = 1e5)

## Testes de estrutura
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(class(dt_teste),
             c("data.table", "data.frame"))
expect_equal(dim(df_teste),
             c(76,7))
expect_equal(dim(dt_teste),
             c(76,7))

