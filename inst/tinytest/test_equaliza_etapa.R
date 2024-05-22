# Teste para função equaliza fundo ----
## Testes para a função de equalizar fundo

# Configuração ----
## Simulação de dados
set.seed(23)
teste <- data.frame(pop = sample(50:100, 32, replace = TRUE),
                    renda = sample(1000:2000, 32, replace = TRUE),
                    id = 1:32)

teste$renda_pc = teste$renda/teste$pop

## Roda exemplos

df_fundo <- simulador.fundeb:::equaliza_fundo(teste,
                           2000,
                           var_ordem = "renda_pc",
                           var_matriculas = "pop",
                           var_recursos = "renda",
                           identificador = 'id')

super_fundo <- simulador.fundeb:::equaliza_fundo(teste,
                              200000,
                              var_ordem = "renda_pc",
                              var_matriculas = "pop",
                              var_recursos = "renda",
                              identificador = 'id')

zero_fundo <- simulador.fundeb:::equaliza_fundo(teste,
                              complementacao_uniao = 0,
                              var_ordem = "renda_pc",
                              var_matriculas = "pop",
                              var_recursos = "renda",
                              identificador = 'id')
# Testes ----
## Testes estrutura
expect_equal(class(df_fundo),
             "data.frame")
expect_equal(class(super_fundo),
             c("data.frame"))
expect_equal(class(zero_fundo),
             c("data.frame"))
expect_equal(dim(df_fundo),
             c(32,2))
expect_equal(dim(super_fundo),
             c(32,2))
expect_equal(dim(zero_fundo),
             c(32,2))

## Testes de resultados extremos
### Zero Fundo
df_teste_zero = merge(zero_fundo, teste)
df_teste_zero$renda_pc_teste = df_teste_zero$recursos_pos / df_teste_zero$pop

expect_equal(df_teste_zero$renda_pc_teste,
             df_teste_zero$renda_pc)
expect_equal(df_teste_zero$recursos_pos,
             df_teste_zero$renda)

### Super Fundo
df_teste_super = merge(super_fundo, teste)
df_teste_super$renda_pc_teste = df_teste_super$recursos_pos / df_teste_super$pop

expect_equal(all((df_teste_super$renda_pc_teste - df_teste_super$renda_pc_teste[1]) < 0.00001),
             TRUE)

### Fundo Normal
df_teste_normal = merge(df_fundo, teste)
df_teste_normal$renda_pc_teste = df_teste_normal$recursos_pos / df_teste_normal$pop

expect_equal(sum(df_teste_normal$recursos_pos) - sum(df_teste_normal$renda) ,
             2000)

expect_equal(sort(df_teste_normal$recursos_pos != df_teste_normal$renda),
             c(rep(FALSE, 20), rep(TRUE, 12)))

expect_equal(sort(df_teste_normal$renda_pc_teste),
             c(rep(15.4384191176471, 12), sort(df_teste_normal$renda_pc_teste)[13:32]))
