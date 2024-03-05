# Teste para função equaliza fundo ----
## Autor: Henrique de Assunção
## Data: 25/05/2021
## Testes para a função de equalizar fundo:

# Configuração ----
## Simulação de dados
set.seed(23)
teste <- data.frame(pop = sample(50:100, 32, replace = TRUE),
                    renda = sample(1000:2000, 32, replace = TRUE),
                    id = 1:32)

teste$renda_pc = teste$renda/teste$pop

## Roda exemplos

df_fundo <- simulador.fundeb2:::equaliza_fundo(teste,
                           2000,
                           var_ordem = "renda_pc",
                           var_alunos = "pop",
                           var_recursos = "renda",
                           identificador = 'id')

super_fundo <- simulador.fundeb2:::equaliza_fundo(teste,
                              200000,
                              var_ordem = "renda_pc",
                              var_alunos = "pop",
                              var_recursos = "renda",
                              identificador = 'id')

zero_fundo <- simulador.fundeb2:::equaliza_fundo(teste,
                              complementacao_uniao = 0,
                              var_ordem = "renda_pc",
                              var_alunos = "pop",
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
             c(32,5))
expect_equal(dim(df_fundo),
             c(32,6))
expect_equal(dim(df_fundo),
             c(32,6))

## Testes de resultados extremos
### Zero Fundo
expect_equal(zero_fundo$vaa_etapa,
             sort(teste$renda_pc))
expect_equal(zero_fundo$receitas_etapa,
             teste$renda[order(teste$renda_pc)])
expect_equal(any(zero_fundo$equalizacao),
             FALSE)

### Super Fundo
expect_equal(all((super_fundo$vaa_etapa - super_fundo$vaa_etapa[1]) < 0.00001),
             TRUE)
expect_equal(all(super_fundo$equalizacao),
             TRUE)

### Fundo Normal
expect_equal(sum(df_fundo$receitas_etapa) - sum(df_fundo$renda) ,
             2000)

expect_equal(df_fundo$equalizacao,
             c(rep(TRUE, 12), rep(FALSE, 20)))

expect_equal(df_fundo$vaa_etapa,
             c(rep(15.4384191176471, 12), df_fundo$vaa_etapa[13:32]))
