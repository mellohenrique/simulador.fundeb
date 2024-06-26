#' Peso
#'
#' Uma base de dados com os pesos por etapa
#'
#' @format Uma tabela com 5 colunas e 29 linhas.
#' \describe{
#'   \item{nome}{Variavel em caractere com o nome das etapas formatado para apresentacao}
#'   \item{etapa}{Variavel em caractere com o nome das etapas segundo as portarias do FNDE}
#'   \item{peso_vaaf}{Variavel numerica com o peso de cada etapa para a etapa de complementacao VAAF segunda o regulamentacao do FNDE}
#'   \item{peso_vaat}{Variavel numerica com o peso de cada etapa para a etapa de complementacao VAAT}
#' @source Tabela Criada a partir dos dados do TCU
"pesos"

#' Dados Teste
#'
#' Uma base de dados com as matriculas por etapa, coeficiente de distribuicao, estimativas de receitas e estado por ente federativo
#'
#' @format Uma tabela com 30 colunas e 76 linhas.
#' \describe{
#'   \item{ibge}{Variavel numerica com o codigo ibge do ente}
#'   \item{creche_integral_rede_publica}{Variavel numerica com o numero de matriculas no ensino infantil creche integral da rede publica}
#'   \item{creche_parcial_rede_publica}{Variavel numerica com o numero de matriculas no ensino infantil creche parcial da rede publica}
#'   \item{pre_escola_integral_rede_publica}{Variavel numerica com o numero de matriculas no ensino infantil pre escola integral da rede publica}
#'   \item{pre_escola_parcial_rede_publica}{Variavel numerica com o numero de matriculas no ensino infantil pre escola parcial da rede publica}
#'   \item{ens_fundamental_series_iniciais_urbano_rede_publica}{Variavel numerica com o numero de matriculas no ensino fundamental series iniciais urbano da rede publica}
#'   \item{ens_fundamental_series_iniciais_rural_rede_publica}{Variavel numerica com o numero de matriculas no ensino fundamental series iniciais rural da rede publica}
#'   \item{ens_fundamental_series_finais_urbano_rede_publica}{Variavel numerica com o numero de matriculas no ensino fundamental anos finais urbano rede publica}
#'   \item{ens_fundamental_series_finais_rural_rede_publica}{Variavel numerica com o numero de matriculas no ensino fundalmental anos finais rural rede publica}
#'   \item{"ens_fundamental_integral_rede_publica}{Variavel numerica com o numero de matriculas no ensino fundalmental integral rede publica}
#'   \item{"ensino_medio_urbano_rede_publica}{Variavel numerica com o numero de matriculas no ensino medio urbano rede publica}
#'   \item{"ensino_medio_rural_rede_publica}{Variavel numerica com o numero de matriculas no ensino medio rural rede publica}
#'   \item{"ensino_medio_integral_rede_publica}{Variavel numerica com o numero de matriculas no ensino medio integral rede publica}
#'   \item{"ensino_medio_integrado_a_educacao_profissional_rede_publica}{Variavel numerica com o numero de matriculas no ensino medio integrado a educacao profissional rede publica}
#'   \item{"itinerario_de_formacao_tecnica_e_profissional_rede_publica}{Variavel numerica com o numero de matriculas no itinerario de formacao tecnica e profissional rede publica}
#'   \item{"educacao_profissional_concomitante_ao_ensino_medio_rede_publica}{Variavel numerica com o numero de matriculas na educacao profissional concomitante ao ensino medio rede publica}
#'   \item{"educacao_especial_rede_publica}{Variavel numerica com o numero de matriculas na educacao especial rede publica}
#'   \item{"atendimento_educacional_especializado_aee}{Variavel numerica com o numero de matriculas no atendimento educacional especializado AEE}
#'   \item{"educacao_de_jovens_e_adultos_com_avaliacao_no_processo_rede_publica}{Variavel numerica com o numero de matriculas na educacao de jovens e adultos EJA com avaliacao no processo rede publica}
#'   \item{"educacao_de_jovens_e_adultos_integrada_a_ed_profissional_de_nivel_medio_rede_publica}{Variavel numerica com o numero de matriculas na educacao de jovens e adultos EJA integrada a educacao profissional de nivel medio rede publica}
#'   \item{"educacao_indigena_e_quilombola_rede_publica}{Variavel numerica com o numero de matriculas na educacao indigena e quilombola, com excecao de creche e pre escola, rede publica}
#'   \item{"creche_integral_rede_conveniada}{Variavel numerica com o numero de matriculas na educacao infantil creche integral rede conveniada}
#'   \item{"creche_parcial_rede_conveniada}{Variavel numerica com o numero de matriculas na educacao infantil creche parcial rede conveniada}
#'   \item{"pre_escola_integral_rede_conveniada}{Variavel numerica com o numero de matriculas na educacao infantil pre escola integral rede conveniada}
#'   \item{"pre_escola_parcial_rede_conveniada}{Variavel numerica com o numero de matriculas na educacao infantil pre escola parcial rede conveniada}
#'   \item{"educacao_especial_rede_conveniada}{Variavel numerica com o numero de matriculas na educacao especial rede conveniada}
#'   \item{"ens_fundamental_series_finais_rural_rede_conveniada_de_formacao_por_alternancia}{Variavel numerica com o numero de matriculas no ensino fundamental series finais rural rede conveniada de formacao por alternancia}
#'   \item{"ens_medio_rural_rede_conveniada_de_formacao_por_alternancia}{Variavel numerica com o numero de matriculas no ensino medio rural rede conveniada de formacao por alternancia}
#'   \item{"ens_medio_integrado_a_ed_profisional_rede_conveniada_de_formacao_por_alternancia}{Variavel numerica com o numero de matriculas no ensino medio integrado a educacao profissional rede conveniada de formacao por alternancia}
#'   \item{"educacao_indigena_e_quilombola_rede_conveniada_de_formacao_por_alternancia}{Variavel numerica com o numero de matriculas na educacao indignea e quilombola rede conveniada de formacao por alternancia}
#'   \item{"eja_com_avaliacao_no_processo_rede_conveniada_de_formacao_por_alternancia}{Variavel numerica com o numero de matriculas na educacao de jovens e adultos EJA com avaliacao no processo rede conveniada de formacao por alternancia}
#'   \item{"eja_integrada_a_ed_profissional_de_nivel_medio_rede_conveniada_de_formacao_por_alternancia}{Variavel numerica com o numero de matriculas na educacao de jovens e adultos EJA integrado a educacao profissional de nivel medio rede conveniada de formacao por alternancia}
#'   \item{"itinerario_de_formacao_tecnica_e_profissional_rede_conveniada_de_formacao_por_alternancia}{Variavel numerica com o numero de matriculas no itinerario de formacao tecnica e profissional rede conveniada de formacao pro alternancia}
#'   \item{"educacao_profissional_concomitante_ao_ensino_medio_rede_conveniada_de_formacao_por_alternancia}{Variavel numerica com o numero de matriculas na educacao profissional concomitante ao ensino medio na rede conveniada de formacao por alternancia}
#'   \item{"ens_medio_integrado_a_ed_profissional_rede_conveniada_instituicoes_de_ed_profissional}{Variavel numerica com o numero de matriculas no ensino medio integrado a educacao profissional rede conveniada instituicoes de educacao profissional}
#'   \item{"eja_integrada_a_ed_profissional_de_nivel_medio_rede_conveniada_instituicoes_de_ed_profissional}{Variavel numerica com o numero de matriculas na educacao de jovens e adultos EJA integrada a educacao profissional de nivel medio rede conveniada de instituicoes de educacao profissional}
#'   \item{"itinerario_de_formacao_tecnica_e_profissional_rede_conveniada_instituicoes_de_ed_profissional}{Variavel numerica com o numero de matriculas no itinerario de formacao tecnica e profissional rede conveniada de instituicoes de educacao profissional}
#'   \item{"educacao_profissional_concomitante_ao_ensino_medio_rede_conveniada_instituicoes_de_ed_profissional}{Variavel numerica com o numero de matriculas na educacao profissional concomitante ao ensino medio rede conveniado em instituicoes de educacao profissional}
#'   \item{"ed_ind_quil_creche}{Variavel numerica com o numero de matriculas na educacao indigena e quilombolas em creche, rede publica}
#'   \item{"ed_ind_quil_pre_escola}{Variavel numerica com o numero de matriculas na educacao indigena e quilombolas em pre escola, rede publica}
#'   \item{"ed_esp_creche}{Variavel numerica com o numero de matriculas na educacao especial em creche, rede publica}
#'   \item{"ed_esp_pre_escola}{Variavel numerica com o numero de matriculas na educacao especial em pre escola, rede publica}
#' }
#' @source Tabela Criada a partir dos dados do TCU
"matriculas"

#' Dados Teste
#'
#' Uma base de dados com as matriculas por etapa, coeficiente de distribuicao, estimativas de receitas e estado por ente federativo
#'
#' @format Uma tabela com 9 colunas e 76 linhas.
#' \describe{
#'   \item{ibge}{Variavel numerica com o codigo ibge do ente}
#'   \item{uf}{Variavel em caractere com o nome da unidades da federacao do ente}
#'   \item{nome}{Variavel em caracteres com o nome do ente}
#'   \item{recursos_vaaf}{Variaval numerica receitas do fundeb cada ente federativo utilizadas na etapa VAAF}
#'   \item{recursos_vaat}{Variaval numerica  com receitas do fundeb cada ente federativo utilizadas na etapa VAAT}
#'   \item{nse}{Variaval numerica com o indice socioeconomico, no exemplo do teste todos os valores sao um}
#'   \item{nf}{Variaval numerica com o indice de nivel fiscal, no exemplo do teste todos os valores sao um}
#'   \item{peso_vaar}{Variavel com a proporcao de complementacao VAAR que o ente recebe}
#'   \item{inabilitados_vaat}{Variavel logica com o identificador dos entes que nao participarao da complementacao vaat}
#'   \item{nf}{Variaval com o indice de nivel fiscal, no exemplo do teste todos os valores sao um}
#'   }
#'
"complementar"

