# Teste das funcoes no arquivo utils

## Teste da funcao reescala vetor
expect_equal(class(simulador.fundeb:::reescala_vetor(1:10, 1, 1.5)),
             c("numeric"))
expect_equal(simulador.fundeb:::reescala_vetor(1:11, minimo =  1, maximo = 1.5),
             seq(1, 1.5, by = .05))

## Testa funcoes de mensagens de erro
expect_error(simulador.fundeb:::checa_numerico('a', 'a'))
expect_error(simulador.fundeb:::checa_data_frame('a', 'a'))
expect_error(simulador.fundeb:::checa_na(c('a', NA_character_), 'a'))
expect_true(simulador.fundeb:::checa_numerico(1, 'a'))
expect_true(simulador.fundeb:::checa_data_frame(simulador.fundeb:::teste_matriculas, 'a'))
expect_true(simulador.fundeb:::checa_na(c('a', 'b'), 'a'))
