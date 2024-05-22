# Teste para função gera_fundo_estadual----
## Testes para função de gerar fundo estadual

# Configuração ----
## Carregando dados
dados_matriculas = simulador.fundeb:::pondera_matriculas_etapa(dados_matriculas = teste_matriculas,  dados_peso =  teste_peso)

dados_entes = simulador.fundeb:::pondera_matriculas_sociofiscal(dados_matriculas = dados_matriculas,  dados_complementar = teste_complementar)

df_teste = simulador.fundeb:::gera_fundo_estadual(dados_entes)

## Testes de estrutura
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(dim(df_teste),
             c(2, 4))

## Testando resultados
expect_equal(df_teste$uf,
             c("AC", "RO"))
expect_equal(df_teste$matriculas_estado_vaaf,
             c(277685.55, 398168.35))
expect_equal(df_teste$recursos_estado_vaaf,
             c(1150404470.33, 1665184818.25))
expect_equal(df_teste$vaaf_estado_inicial,
             c(4142.83159613455, 4182.11246134958))
