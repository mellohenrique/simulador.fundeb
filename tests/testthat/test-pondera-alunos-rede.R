test_that("pondera_alunos_rede funciona", {
  expect_equal(pondera_alunos_rede(alunos_teste) %>% dplyr::filter(ibge < 100, stringr::str_detect(etapa, "Creche")) %>% dplyr::select(alunos) %>% dplyr::pull(alunos) %>% sum, 0)
  expect_equal(pondera_alunos_rede(alunos_teste) %>% dplyr::filter(ibge > 100, stringr::str_detect(etapa, "EM")) %>% dplyr::select(alunos) %>% dplyr::pull(alunos) %>% sum, 0)
})

test_that("pondera_alunos_rede gera data.frame", {
  expect_output(pondera_alunos_rede(alunos_teste) %>% str(), "data\\.frame")
})
