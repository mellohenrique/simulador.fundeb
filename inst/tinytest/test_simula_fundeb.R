# Teste da funcao simula fnde ----
# Autor: Henrique de Assunção
## Testes para a função simula fnde

## Preparação ----
df_teste = simulador.fundeb2:::simula_fundeb(
  dados_alunos = teste_alunos,
  dados_complementar = teste_complementar,
  dados_peso = teste_peso,
  complementacao_vaaf = 4e5,
  complementacao_vaat = 1e5,
  complementacao_vaar = 1e5)

### Preparação para teste de casos extremos ####

df_teste_zero = simulador.fundeb2:::simula_fundeb(
  dados_alunos = teste_alunos,
  dados_complementar = teste_complementar,
  dados_peso = teste_peso,
  complementacao_vaaf = 0,
  complementacao_vaat = 0,
  complementacao_vaar = 0)

df_teste_super_vaaf = simulador.fundeb2:::simula_fundeb(
  dados_alunos = teste_alunos,
  dados_complementar = teste_complementar,
  dados_peso = teste_peso,
  complementacao_vaaf = 1e8,
  complementacao_vaat = 0,
  complementacao_vaar = 0)

df_teste_super_vaat = simulador.fundeb2:::simula_fundeb(
  dados_alunos = teste_alunos,
  dados_complementar = teste_complementar,
  dados_peso = teste_peso,
  complementacao_vaaf = 0,
  complementacao_vaat = 1e8,
  complementacao_vaar = 0)


#
teste_sem_complementacao = as.vector(cbind(by(df_teste$fundeb_vaaf, df_teste$uf, sum))/ cbind(by(df_teste$alunos_vaaf, df_teste$uf, sum)))

teste_super_complementacao = (sum(df_teste$fundeb_vaaf) + 1e10)/sum(df_teste$alunos_vaaf)
teste_super_complementacao_extra = (sum(df_teste$fundeb_vaat +df_teste$recursos_extra) + 1e10)/sum(df_teste$alunos_vaat)

# Testes ----
## Testes de estrutura ####
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(dim(df_teste),
             c(76, 20))

## Teste de resultados da funcao ####
### Casos extremos
#### Valores
expect_equal(df_teste_super_vaaf$vaaf,
             rep(teste_super_complementacao, 76))
expect_equal(df_teste_super_vaat$vaat,
             rep(teste_super_complementacao_extra, 76))

expect_equal(unique(round(df_teste_zero$vaaf, digits = 2)),
             round(teste_sem_complementacao, digits = 2))

expect_equal(df_teste_zero$vaat,
             df_teste_zero$vaat_pre)

#### Equalizacao
expect_equal(any(df_teste_zero$equalizacao_vaaf),
             FALSE)
expect_equal(any(df_teste_zero$equalizacao_vaat),
             FALSE)
expect_equal(all(df_teste_super_vaaf$equalizacao_vaaf),
             TRUE)
expect_equal(all(df_teste_super_vaat$equalizacao_vaat),
             TRUE)

expect_equal(any(df_teste_super_vaat$equalizacao_vaaf),
             FALSE)
expect_equal(any(df_teste_super_vaaf$equalizacao_vaat),
             FALSE)
