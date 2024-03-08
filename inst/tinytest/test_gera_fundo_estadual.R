# Teste para função gera_fundo_estadual----
## Testes para função de gerar fundo estadual

# Configuração ----
## Carregando dados
dados_alunos = simulador.fundeb:::pondera_alunos_etapa(dados_alunos = teste_alunos,  dados_peso =  teste_peso)

dados_entes = simulador.fundeb:::pondera_alunos_sociofiscal(dados_alunos = dados_alunos,  dados_complementar = teste_complementar)

df_teste = simulador.fundeb:::gera_fundo_estadual(dados_entes)

## Testes de estrutura
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(dim(df_teste),
             c(2, 4))

## Testando resultados
expect_equal(df_teste$uf,
             c("AC", "RO"))
expect_equal(df_teste$alunos_estado_vaaf,
             c(277685.55, 398168.35))
expect_equal(df_teste$recursos_estado_vaaf,
             c(1150404470.33, 1665184818.25))
expect_equal(df_teste$vaaf_estado_inicial,
             c(4142.83159613455, 4182.11246134958))
