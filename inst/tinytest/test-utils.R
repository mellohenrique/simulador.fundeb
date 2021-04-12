# Teste das funcoes no arquivo utils

## Teste da funcao checa_tranforma_dt
expect_equal(class(simulador.fundeb2:::checa_transforma_dt(mtcars)),
             c("data.table", "data.frame"))
expect_equal(class(simulador.fundeb2:::checa_transforma_dt(data.table::data.table(a = 1:2))),
             c("data.table", "data.frame"))

## Teste da funcao retorna_dt_df
expect_equal(class(simulador.fundeb2:::retorna_dt_df(data.table::data.table(mtcars), produto_dt = TRUE)),
             c("data.table", "data.frame"))
expect_equal(class(simulador.fundeb2:::retorna_dt_df(mtcars, produto_dt = FALSE)),
             c("data.frame"))
