# Teste da funcao simula fnde ----
# Autor: Henrique de Assunção
# Data: 25/05/2021
## Testes para a função simula fnde

## Preparação ----
df_teste = simula_fundeb(dados_alunos = dados_teste,
                         dados_complementar = complementar_teste,
                         peso_etapas = peso,
                         complementacao_vaaf = 1e5,
                         complementacao_vaat = 1e5,
                         produto_dt = FALSE)

dt_teste = simula_fundeb(dados_alunos = dados_teste,
                          dados_complementar = complementar_teste,
                          peso_etapas = peso,
                          complementacao_vaaf = 1e5,
                          complementacao_vaat = 1e5,
                          produto_dt = TRUE)

### Preparação para teste de casos extremos ####
dt_teste_zero = simula_fundeb(dados_alunos = dados_teste,
                               dados_complementar = complementar_teste,
                               peso_etapas = peso,
                               complementacao_vaaf = 0,
                               complementacao_vaat = 0,
                               produto_dt = TRUE)

dt_teste_super_vaat = simula_fundeb(dados_alunos = dados_teste,
                                     dados_complementar = complementar_teste,
                                     peso_etapas = peso,
                                     complementacao_vaaf = 0,
                                     complementacao_vaat = 1e10,
                                     produto_dt = TRUE)

dt_teste_super_vaaf = simula_fundeb(dados_alunos = dados_teste,
                                    dados_complementar = complementar_teste,
                                peso_etapas = peso,
                                complementacao_vaaf = 1e10,
                                complementacao_vaat = 0,
                                produto_dt = TRUE)

#
teste_sem_complementacao = as.vector(cbind(by(df_teste$fundeb_vaaf, df_teste$uf, sum))/ cbind(by(df_teste$alunos_vaaf, df_teste$uf, sum)))

teste_super_complementacao = (sum(df_teste$fundeb_vaaf) + 1e10)/sum(df_teste$alunos_vaaf)
teste_super_complementacao_extra = (sum(df_teste$fundeb_vaat +df_teste$recursos_extra) + 1e10)/sum(df_teste$alunos_vaat)

# Testes ----
## Testes de estrutura ####
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(class(dt_teste),
             c("data.table", "data.frame"))
expect_equal(dim(df_teste),
             c(76, 23))
expect_equal(dim(dt_teste),
             c(76, 23))

## Teste de resultados da funcao ####
### Casos extremos
#### Valores
expect_equal(dt_teste_super_vaaf$vaaf,
             rep(teste_super_complementacao, 76))
expect_equal(dt_teste_super_vaat$vaat,
             rep(teste_super_complementacao_extra, 76))

expect_equal(unique(round(dt_teste_zero$vaaf, digits = 2)),
             round(teste_sem_complementacao, digits = 2))

expect_equal(dt_teste_zero$vaat,
             dt_teste_zero$vaat_pre)

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
