% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gera-fundo-estadual.R
\name{gera_fundo_estadual}
\alias{gera_fundo_estadual}
\title{Pondera alunos por etapa}
\usage{
gera_fundo_estadual(dados_alunos, dados_financas, produto_dt = TRUE)
}
\arguments{
\item{dados_alunos}{data.frame ou data.table com os dados de alunos ponderados em formato tidy}

\item{produto_dt}{Variavel binaria que determina se a tabela produto da funcao sera da classe data.table (se Verdadeiro) ou data.frame (se Falso)}
}
\value{
Um data.frame ou data.table com os dados de alunos considerando os pesos dados de FNDE em formato longo ou curto
}
\description{
Pondera dados de alunos do FNDE por ente e etapa e retorna os valores ponderados por etapa
}