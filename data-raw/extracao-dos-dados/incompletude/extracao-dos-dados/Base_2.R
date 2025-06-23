## pacotes
library(readr)
library(janitor)
library(dplyr)
library(lubridate)
library(forcats)
library(pander)
library(skimr)
require(survival)
require(truncnorm)
require(LaplacesDemon)
require(TeachingDemos)
require(coda)
library(foreign)
library("reshape2")
library(tidyr)
# IMPORTACAO --------------------------------------------------------------


IDADEMAE <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/IDADEMAE_muni.csv"),
                       delim = ",", escape_double = FALSE, trim_ws = TRUE)
ESCMAE <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/ESCMAE_muni.csv"),
                     delim = ",", escape_double = FALSE, trim_ws = TRUE)
PARTO <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/PARTO_muni.csv"),
                    delim = ",", escape_double = FALSE, trim_ws = TRUE)
TPROBSON <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/TPROBSON_muni.csv"),
                       delim = ",", escape_double = FALSE, trim_ws = TRUE)
PESO <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/PESO_muni.csv"),
                   delim = ",", escape_double = FALSE, trim_ws = TRUE)
RACACORMAE <- read_delim("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/RACACORMAE_muni.csv",
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)

# CRIACAO TABELA BASE MUNICIPIOS COMPARACAO -------------------------------
#COMPARANDO COM BASE ANTIGA
dados_nasc <- read_delim("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/total_nascidos_CODMUNRES.csv",
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)

dados_nasc_agr <- dados_nasc %>%
  rename(nasc = total_de_nascidos_vivos)

janitor::get_dupes(dados_nasc_agr, codmunres)

dados <- dplyr::distinct(dados_nasc_agr, ano, codmunres, .keep_all = TRUE)
dados_nasc_antigo <- read_delim("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/total_nascidos_CODMUNRES.csv",
                                delim = ",", escape_double = FALSE, trim_ws = TRUE)
dados_nasc_agr_antigo <- dados_nasc_antigo %>% select(-...1) |>
  rename(nasc = TOTAL_DE_NASCIDOS_VIVOS)
sum(dados_nasc_agr |> filter(ano < 2021 ) |> pull(nasc)) - sum(dados_nasc_agr_antigo$nasc)
#DEU DIFERENCA, TESTANDO AGORA PEGAR SOMENTE OS MUNICIPIOS UTILIZADO NO PAINEL DE VIGILANCIA

codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres)
#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2022)), ano = 2012:2022)

df_verificacao <- left_join(df_aux_municipios, dados_nasc_agr)
df_verificacao$nasc[is.na(df_verificacao$nasc)] <- 0

sum(df_verificacao |> filter(ano < 2021 ) |> pull(nasc)) - sum(dados_nasc_agr_antigo$nasc)


dados_nasc_agr_antigo$ANO |> unique()
#AGORA FOI, USAR ESSE. E COMO SABEMOS QUE OS NUMERO DE NASCIDOS CONFERE,
#PODEMOS UTILIZAR ESSE BANCO DE DADOS PARA VERIFICACAO AO INVES DA BASE ANTIGA DE
#CADA VARIAVEL
dados_nasc_agr <- df_verificacao
rm(df_verificacao,dados_nasc_agr_antigo,codigos_municipios,dados_nasc_antigo)
names(dados_nasc_agr) <- names(dados_nasc_agr) |> toupper()
# IDADEMAE ----------------------------------------------------------------
IGNORADOS <- IDADEMAE %>%
  filter(IDADEMAE  == 99)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(IGNORADOS = sum(NASCIDOS))

NULOS <- IDADEMAE %>%
  filter( IDADEMAE  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(NULOS = sum(NASCIDOS))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_IDADEMAE <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_IDADEMAE$variavel <- 'IDADEMAE'

# IDADEMAE2 ---------------------------------------------------------------
IGNORADOS <- IDADEMAE %>%
  filter(IDADEMAE  == 99 | IDADEMAE > 55)

# IGNORADOS <- IDADEMAE %>%
#   filter(IDADEMAE > 55)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(IGNORADOS = sum(NASCIDOS))

NULOS <- IDADEMAE %>%
  filter( IDADEMAE  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(NULOS = sum(NASCIDOS))



dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_IDADEMAE2 <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_IDADEMAE2$variavel <- 'IDADEMAE2'

# ESCMAE ------------------------------------------------------------------
IGNORADOS <- ESCMAE %>%
  filter(ESCMAE  == 9)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(IGNORADOS = sum(NASCIDOS))

NULOS <- ESCMAE %>%
  filter( ESCMAE  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(NULOS = sum(NASCIDOS))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_ESCMAE <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_ESCMAE$variavel <- 'ESCMAE'

# PARTO -------------------------------------------------------------------
IGNORADOS <- PARTO %>%
  filter(PARTO  == 9)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(IGNORADOS = sum(NASCIDOS))

NULOS <- PARTO %>%
  filter( PARTO  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(NULOS = sum(NASCIDOS))



dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_PARTO <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_PARTO$variavel <- 'PARTO'

# TPROBSON ----------------------------------------------------------------
## CORRIGINDO O ERRO DE 2013
TPROBSON_antigo <-  read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/TPROBSON_muni.csv"),
                               delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
  rename(CODMUNRES = Municipio,
         ANO = Ano,
         NASC = Nascidos)
TPROBSON <- TPROBSON |>
  rbind(TPROBSON_antigo[TPROBSON_antigo$ANO == 2013,c('CODMUNRES', 'ANO', 'NASC','TPROBSON')])

IGNORADOS <- TPROBSON %>%
  filter(TPROBSON  == 11 | TPROBSON == 12)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(IGNORADOS = sum(NASC))

NULOS <- TPROBSON %>%
  filter( TPROBSON  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_TPROBSON <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_TPROBSON$variavel <- 'TPROBSON'
# PESO --------------------------------------------------------------------
IGNORADOS <- PESO %>%
  filter(PESO  == 9999)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(IGNORADOS = sum(NASCIDOS))

NULOS <- PESO %>%
  filter( PESO  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(NULOS = sum(NASCIDOS))



dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_PESO <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_PESO$variavel <- 'PESO'

# RACACORMAE -----------------------------------------------------------------
IGNORADOS <- RACACORMAE %>%
  filter(RACACORMAE  == 9)


IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(IGNORADOS = sum(NASCIDOS))

NULOS <- RACACORMAE %>%
  filter( RACACORMAE  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES   , ANO ) %>%
  summarise(NULOS = sum(NASCIDOS))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_RACACORMAE <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_RACACORMAE$variavel <- 'RACACORMAE'



# JUNCAO ------------------------------------------------------------------


dados_final <- bind_rows(dados_PARTO ,dados_TPROBSON,
                         dados_RACACORMAE,dados_IDADEMAE2,
                         dados_ESCMAE ,dados_IDADEMAE ,dados_PESO)
dados_final[dados_final$UF %>% is.na() & round(dados_final$CODMUNRES/10000)== 36,'CODMUNRES']
dados_final$aux <- dados_final$CODMUNRES %>% substr(start = 1,stop =2)
UFS <- data.frame(
  COD = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
  UF = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA',
            'MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF'))
dados_final$CODMUNRES <- dados_final$CODMUNRES %>% as.character()
dados_final <-merge(dados_final, UFS, by.x = "aux", by.y = "COD", all.x = TRUE)
dados_final_2 <- dados_final %>% select(-c(aux))

write.csv(dados_final_2,'data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/Base_2_2012-2023.csv')
dados_final_2[dados_final_2$TOTAIS %>% is.na(),]
