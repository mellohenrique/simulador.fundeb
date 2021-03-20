# Testando funcao limpa_fnde

## Cria bases de teste
df_teste <- pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = FALSE)
dt_teste <- pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = TRUE)

df_teste_untidy <- pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = FALSE, tidy = FALSE)
dt_teste_untidy <- pondera_alunos_etapa(limpa_fnde(dados_teste), produto_dt = TRUE, tidy = FALSE)

## Teste de estrutura
expect_equal(class(df_teste),
             "data.frame")
expect_equal(class(dt_teste),
             c("data.table", "data.frame"))
expect_equal(class(df_teste_untidy),
             "data.frame")
expect_equal(class(dt_teste_untidy),
             c("data.table", "data.frame"))

## Teste de dimensoes
expect_equal(dim(df_teste),
             c(2204, 6))
expect_equal(dim(dt_teste),
             c(2204, 6))

expect_equal(dim(df_teste_untidy),
             c(76, 31))
expect_equal(dim(dt_teste_untidy),
             c(76, 31))

## Testando ponderacao de alunos entre tipos diferentes de teste
expect_equal(
  setorder(dt_teste[etapa == "Ed. Especial",], `Município`)$alunos_ponderados,
  setorder(dt_teste_untidy,`Município`)$`Ed. Especial`)

expect_equal(
  setorder(dt_teste[etapa == "Ensino Médio Urbano",], `Município`)$alunos_ponderados,
  setorder(dt_teste_untidy,`Município`)$`Ensino Médio Urbano`)

expect_equal(
  setorder(dt_teste[etapa == "Ensino Fundamental Tempo Integral",], `Município`)$alunos_ponderados,
  setorder(dt_teste_untidy,`Município`)$`Ensino Fundamental Tempo Integral`)

expect_equal(
  setorder(dt_teste[etapa == "Ensino Fundamental Sér. Iniciais Urbana",], UF, `Município`)$alunos_ponderados,
  c(781, 579, 1216, 709, 641, 2403, 789, 703,  24101, 229, 1171, 892, 754, 423, 262, 371, 6545, 575, 416, 2143, 927, 2058, 354, 1030, 487, 368, 690, 4826, 1808, 416, 311, 2406, 447, 1194, 0, 973, 553, 813, 275, 678, 1054, 1228, 268, 16410,  3177, 775, 2740, 3136, 1859, 337, 587, 597, 687, 1566, 342, 631, 2257, 281, 2066, 229, 23242, 813, 190, 349, 2933, 537, 371, 853, 989, 513, 207, 0, 538, 559, 358, 5972))
