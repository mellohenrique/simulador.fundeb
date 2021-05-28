# Teste para função gera_fundo_estadual----
## Autor: Henrique de Assunção
## Data: 25/05/2021
## Testes para função de gerar fundo estadual

# Configuração ----
## Carregando dados
dados = limpa_fnde(dados_teste)
dados_financas = financas_fnde(dados)
dados$municipio = NULL
dados_alunos = pondera_alunos_etapa(dados)

df_teste = gera_fundo_estadual(dados_alunos, dados_financas, produto_dt = FALSE)
dt_teste = gera_fundo_estadual(dados_alunos, dados_financas, produto_dt = TRUE)

## Testes de estrutura
expect_equal(class(df_teste),
             c("data.frame"))
expect_equal(class(dt_teste),
             c("data.table", "data.frame"))
expect_equal(dim(df_teste),
             c(2,5))
expect_equal(dim(dt_teste),
             c(2, 5))

## Testando resultados
expect_equal(df_teste$uf,
             c("AC", "RO"))
expect_equal(dt_teste$uf,
             c("AC", "RO"))
expect_equal(df_teste$alunos_ponderados,
             c(277685.55, 398168.35))
expect_equal(dt_teste$alunos_ponderados,
             c(277685.55, 398168.35))
expect_equal(df_teste$alunos,
             c(252544, 365712))
expect_equal(dt_teste$alunos,
             c(252544, 365712))
expect_equal(df_teste$receitas,
             c(1150404470.33, 1665184818.25))
expect_equal(dt_teste$receitas,
             c(1150404470.33, 1665184818.25))
expect_equal(df_teste$vaa,
             c(4142.83159613455, 4182.11246134958))
expect_equal(dt_teste$vaa,
             c(4142.83159613455, 4182.11246134958))
