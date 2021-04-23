# Teste da funcao simula fnde ----
# Autor: Henrique de Assunção
# Data: 16/04/2021
## Testes para a função simula fnde

## Preparação
fnde <- limpa_fnde(dados_teste)

df_teste <- simula_fundeb(fnde, aporte = 1e5, produto_dt = FALSE)
dt_teste <- simula_fundeb(fnde, aporte = 1e5)

### Preparação para teste de casos extremos
dt_teste_zero <- simula_fundeb(fnde, aporte = 0, produto_dt = FALSE)
dt_teste_super <- simula_fundeb(fnde, aporte = 1e9, produto_dt = FALSE)

teste_sem_aporte <- as.vector(cbind(by(df_teste$receitas, df_teste$uf, sum))/ cbind(by(df_teste$alunos_ponderados, df_teste$uf, sum)))

teste_super_aporte <- (sum(df_teste$receitas) + 1e9)/sum(df_teste$alunos_ponderados)


# Testes ----
## Testes de estrutura ####
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(class(dt_teste),
             c("data.table", "data.frame"))
expect_equal(dim(df_teste),
             c(76,8))
expect_equal(dim(dt_teste),
             c(76,8))

## Teste de resultados da funcao ####
### Casos extremos
expect_equal(dt_teste_zero$vaaf,
             c(rep(teste_sem_aporte[1], 23),
             rep(teste_sem_aporte[2], 53)),)
expect_equal(dt_teste_super$vaaf,
             rep(teste_super_aporte, 76))
expect_equal(all(dt_teste_zero$equalizacao_vaaf),
             FALSE)
expect_equal(all(dt_teste_super$equalizacao_vaaf),
             TRUE)
