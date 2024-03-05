# Teste das funcoes no arquivo utils

## Teste da funcao reescala vetor
expect_equal(class(simulador.fundeb2:::reescala_vetor(1:10, 1, 1.5)),
             c("numeric"))
expect_equal(simulador.fundeb2:::reescala_vetor(1:11, chao =  1, teto = 1.5),
             seq(1, 1.5, by = .05))
