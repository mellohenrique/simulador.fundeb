# Teste da funcao simula fnde ----
# Autor: Henrique de Assunção
## Testes para a função simula fnde

## Preparação ----

### Entes habilitados
ibge_habilitados = teste_complementar$ibge[teste_complementar$inabilitados_vaat == FALSE]

### Teste original

df_teste = simulador.fundeb::simula_fundeb(
  dados_matriculas = teste_matriculas,
  dados_complementar = teste_complementar,
  dados_peso = teste_peso,
  complementacao_vaaf = 4e5,
  complementacao_vaat = 1e5,
  complementacao_vaar = 1e5)

### Preparação para teste de casos extremos ####

df_teste_zero = simulador.fundeb::simula_fundeb(
  dados_matriculas = teste_matriculas,
  dados_complementar = teste_complementar,
  dados_peso = teste_peso,
  complementacao_vaaf = 0,
  complementacao_vaat = 0,
  complementacao_vaar = 0)

df_teste_super_vaaf = simulador.fundeb::simula_fundeb(
  dados_matriculas = teste_matriculas,
  dados_complementar = teste_complementar,
  dados_peso = teste_peso,
  complementacao_vaaf = 1e10,
  complementacao_vaat = 0,
  complementacao_vaar = 0)

df_teste_super_vaat = simulador.fundeb::simula_fundeb(
  dados_matriculas = teste_matriculas,
  dados_complementar = teste_complementar,
  dados_peso = teste_peso,
  complementacao_vaaf = 0,
  complementacao_vaat = 1e10,
  complementacao_vaar = 0)


### Parametros para teste ----
teste_sem_complementacao = as.vector(cbind(by(df_teste$recursos_vaaf, df_teste$uf, sum))/ cbind(by(df_teste$matriculas_vaaf, df_teste$uf, sum)))

teste_super_complementacao_vaaf = (sum(df_teste$recursos_vaaf) + 1e10)/sum(df_teste$matriculas_vaaf)

df_teste_vaat = df_teste[df_teste$ibge %in% ibge_habilitados,]

teste_super_complementacao_vaat = (sum(df_teste_vaat$recursos_vaat) + 1e10)/sum(df_teste_vaat$matriculas_vaat)

# Testes ----
## Testes de estrutura ----
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(dim(df_teste),
             c(76, 21))

## Teste de resultados da funcao ----
### Casos extremos
#### Valores
expect_equal(df_teste_super_vaaf$vaaf_final,
             rep(teste_super_complementacao_vaaf, 76))
expect_equal(df_teste_super_vaat[df_teste_super_vaat$ibge %in% ibge_habilitados,]$vaat_final,
             rep(teste_super_complementacao_vaat, 75))

expect_equal(sort(unique(round(df_teste_zero$vaaf, digits = 2))),
             sort(round(teste_sem_complementacao, digits = 2)))

expect_equal(df_teste_zero$vaat_pre,
             df_teste_zero$vaat_final)

#### Equalizacao ----
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

