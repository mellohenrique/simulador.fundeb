# Teste para função pondera_alunos_etapa----
## Autor: Henrique de Assunção
## Data: 25/05/2021
## Testes para função de pnderar alunos por etapa

# Configuração ----

## Cria bases de teste
df_teste = pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = FALSE)
dt_teste = pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = TRUE)

df_teste_etapa = pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = FALSE, retorno = "etapa_tidy")
dt_teste_etapa = pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = TRUE, retorno = "etapa_tidy")

df_teste_etapa_long = pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = FALSE, retorno = "etapa_long")
dt_teste_etapa_long = pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = TRUE, retorno = "etapa_long")

## Teste de estrutura
expect_equal(class(df_teste),
             "data.frame")
expect_equal(class(dt_teste),
             c("data.table", "data.frame"))
expect_equal(class(df_teste_etapa),
             "data.frame")
expect_equal(class(dt_teste_etapa),
             c("data.table", "data.frame"))
expect_equal(class(df_teste_etapa_long),
             "data.frame")
expect_equal(class(dt_teste_etapa_long),
             c("data.table", "data.frame"))

## Teste de dimensoes
expect_equal(dim(df_teste),
             c(76, 4))
expect_equal(dim(dt_teste),
             c(76, 4))

expect_equal(dim(df_teste_etapa),
             c(2204, 6))
expect_equal(dim(dt_teste_etapa),
             c(2204, 6))

expect_equal(dim(df_teste_etapa_long),
             c(76, 31))
expect_equal(dim(dt_teste_etapa_long),
             c(76, 31))

## Testando ponderacao de alunos entre tipos diferentes de teste
expect_equal(
  data.table::setorder(dt_teste_etapa[etapa == "ed_especial",], ibge)$alunos_ponderados,
  data.table::setorder(dt_teste_etapa_long,ibge)$ed_especial)

expect_equal(
  data.table::setorder(dt_teste_etapa[etapa == "ensino_medio_urbano",], ibge)$alunos_ponderados,
  data.table::setorder(dt_teste_etapa_long,ibge)$ensino_medio_urbano)

expect_equal(
  data.table::setorder(dt_teste_etapa[etapa == "ensino_fundamental_tempo_integral",], ibge)$alunos_ponderados,
  data.table::setorder(dt_teste_etapa_long,ibge)$ensino_fundamental_tempo_integral)

expect_equal(
  data.table::setorder(dt_teste_etapa[etapa == "ensino_fundamental_ser_iniciais_urbana",], uf, ibge)$alunos_ponderados,
  c(24101, 781, 579, 1216, 709, 641, 2403, 789, 703, 229, 1171, 892, 754, 423, 371, 6545, 575, 416, 927, 2143, 2058, 354, 262, 16410, 1030, 4826, 416, 2406, 973, 813, 275, 678, 1228, 3177, 2740, 3136, 1859, 687, 2257, 2066, 23242, 813, 349, 2933, 537, 5972, 989, 1566, 690, 487, 368, 1808, 631, 311, 447, 1194, 0, 553, 1054, 268, 775, 337, 587, 597, 342,  281, 229, 190, 371, 853, 513, 207, 0, 538, 559, 358))
