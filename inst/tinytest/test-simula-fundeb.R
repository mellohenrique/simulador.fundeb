# Teste da funcao simula fnde ----
# Autor: Henrique de Assunção
# Data: 16/04/2021
## Testes para a função simula fnde

## Preparação ----
fnde <- limpa_fnde(dados_teste)

df_teste <- simula_fundeb(dados_fnde = fnde,
                          peso_etapas = peso,
                          aporte_vaaf = 1e5,
                          aporte_vaat = 1e5,
                          produto_dt = FALSE)
dt_teste <- simula_fundeb(dados_fnde = fnde,
                          peso_etapas = peso,
                          aporte_vaaf = 1e5,
                          aporte_vaat = 1e5,
                          produto_dt = TRUE)

### Preparação para teste de casos extremos ####
dt_teste_zero <- simula_fundeb(dados_fnde = fnde,
                               peso_etapas = peso,
                               aporte_vaaf = 0,
                               aporte_vaat = 0,
                               produto_dt = TRUE)
dt_teste_super_vaat <- simula_fundeb(dados_fnde = fnde,
                                     peso_etapas = peso,
                                     aporte_vaaf = 0,
                                     aporte_vaat = 1e10,
                                     produto_dt = TRUE)
dt_teste_super_vaaf <- simula_fundeb(dados_fnde = fnde,
                                peso_etapas = peso,
                                aporte_vaaf = 1e10,
                                aporte_vaat = 0,
                                produto_dt = TRUE)

teste_sem_aporte <- as.vector(cbind(by(df_teste$receitas, df_teste$uf, sum))/ cbind(by(df_teste$alunos_ponderados, df_teste$uf, sum)))

teste_super_aporte <- (sum(df_teste$receitas) + 1e10)/sum(df_teste$alunos_ponderados)


# Testes ----
## Testes de estrutura ####
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(class(dt_teste),
             c("data.table", "data.frame"))
expect_equal(dim(df_teste),
             c(76,12))
expect_equal(dim(dt_teste),
             c(76,12))

## Teste de resultados da funcao ####
### Casos extremos
#### Valores
expect_equal(dt_teste_super_vaaf$vaaf,
             rep(teste_super_aporte, 76))
expect_equal(dt_teste_super_vaaf$vaat,
             rep(teste_super_aporte, 76))
expect_equal(dt_teste_super_vaaf$vaaf,
             rep(teste_super_aporte, 76))

expect_equal(unique(round(dt_teste_zero$vaat, digits = 2)),
             round(teste_sem_aporte, digits = 2))
expect_equal(unique(round(dt_teste_zero$vaaf, digits = 2)),
             round(teste_sem_aporte, digits = 2))

#### Equalizacao
expect_equal(any(dt_teste_zero$equalizacao_vaaf),
             FALSE)
expect_equal(any(dt_teste_zero$equalizacao_vaat),
             FALSE)
expect_equal(all(dt_teste_super_vaaf$equalizacao_vaaf),
             TRUE)
expect_equal(all(dt_teste_super_vaat$equalizacao_vaat),
             TRUE)

expect_equal(any(dt_teste_super_vaat$equalizacao_vaaf),
             FALSE)
expect_equal(any(dt_teste_super_vaaf$equalizacao_vaat),
             FALSE)
