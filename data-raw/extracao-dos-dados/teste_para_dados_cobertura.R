# Esse código testa uma estimativa para  Cobertura populacional com equipes de
# saúde da familia, utilizando um fator de cálculo encontrado em
# https://www.proadess.icict.fiocruz.br/index.php?pag=fic_mu&cod=A55&tab=1

library(dplyr)
library(readr)
# Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres)

# Ler o arquivo CSV baixado em https://sisaps.saude.gov.br/painelsaps/cobertura_pot_aps

cobertura_aps_mun <- read.csv('data-raw/csv/cobertura_potencial_aps_municipio.csv',
                              sep = ';',
                              encoding = 'ISO-8859-1',
                              stringsAsFactors = FALSE) |>
  select(qt_esf_mun,qt_pop_municipio,
         qt_capacidade_equipe_mun,mes_ano,municipio,co_municipio_ibge,sg_uf) |>

  mutate(ano =  as.numeric(substr(mes_ano, nchar(mes_ano) - 3, nchar(mes_ano)))) |>
  rename(codmunres = co_municipio_ibge) |>
  select(-mes_ano) |>
  filter(codmunres %in% codigos_municipios)

# Função para substituir códigos de escape por letras acentuadas
substituir_acentos <- function(texto) {
  texto <- iconv(texto, to = "UTF-8", from = "ISO-8859-1", sub = "byte")
  return(texto)
}

# Aplicar a função a todas as colunas do dataframe
cobertura_aps_mun[] <- lapply(cobertura_aps_mun, substituir_acentos)
cobertura_aps_mun$media_cobertura_esf <- (as.numeric(cobertura_aps_mun$qt_esf_mun )* 3450)

# Corrigindo formato de algumas variáveis para juncao ao bloco1
cobertura_aps_mun$ano <- cobertura_aps_mun$ano |> as.numeric()
cobertura_aps_mun$codmunres <- cobertura_aps_mun$codmunres |> as.numeric()
cobertura_aps_mun$qt_pop_municipio <- cobertura_aps_mun$qt_pop_municipio |> as.numeric()
bloco1 <- read_csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2022.csv")

bloco1 <- bloco1 |>
  left_join(cobertura_aps_mun %>% select(ano, codmunres, media_cobertura_esf, qt_pop_municipio),
            by = c("ano", "codmunres"))
#Substituindo os dados de 2021 e 2022 anteriores
bloco1[is.na(bloco1$media_cobertura_esf.x),'media_cobertura_esf.x'] <- bloco1[is.na(bloco1$media_cobertura_esf.x),'media_cobertura_esf.y']
bloco1[is.na(bloco1$populacao_total),'populacao_total'] <- bloco1[is.na(bloco1$populacao_total),'qt_pop_municipio']

bloco1 <- bloco1 |>
  select(-qt_pop_municipio, -media_cobertura_esf.y) |>
  rename(media_cobertura_esf = media_cobertura_esf.x)

#Duas cidades nao existem dados para 2021
bloco1[bloco1$ano == 2021 & bloco1$media_cobertura_esf |>  is.na(),] |> View()

