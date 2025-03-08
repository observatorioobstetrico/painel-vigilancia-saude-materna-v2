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

dados_nasc <- read_delim("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/total_nascidos_CODMUNRES.csv",
                         delim = ",", escape_double = FALSE, trim_ws = TRUE) |> select(-...1)

dados_nasc_agr <- dados_nasc %>%
  rename(nasc = total_de_nascidos_vivos)

janitor::get_dupes(dados_nasc_agr, codmunres)

# dados_nasc_agr_antigo <- dados_nasc_antigo %>% select(-...1) |>
#   rename(nasc = TOTAL_DE_NASCIDOS_VIVOS)
# sum(dados_nasc_agr |> filter(ano < 2021 ) |> pull(nasc)) - sum(dados_nasc_agr_antigo$nasc)
#DEU DIFERENCA, TESTANDO AGORA PEGAR SOMENTE OS MUNICIPIOS UTILIZADO NO PAINEL DE VIGILANCIA

codigos_municipios <- read.csv("data-raw/csv/tabela_aux_municipios.csv") |>
  pull(codmunres)
#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios,
                                                each = length(2012:2023)),
                                                ano = 2012:2023)

df_verificacao <- left_join(df_aux_municipios, dados_nasc_agr)
df_verificacao$nasc[is.na(df_verificacao$nasc)] <- 0

# sum(df_verificacao |> filter(ano < 2021 ) |> pull(nasc)) - sum(dados_nasc_agr_antigo$nasc)


# dados_nasc_agr_antigo$ANO |> unique()
#AGORA FOI, USAR ESSE. E COMO SABEMOS QUE OS NUMERO DE NASCIDOS CONFERE,
#PODEMOS UTILIZAR ESSE BANCO DE DADOS PARA VERIFICACAO AO INVES DA BASE ANTIGA DE
#CADA VARIAVEL
dados_nasc_agr <- df_verificacao
rm(df_verificacao,codigos_municipios #,dados_nasc_antigo,dados_nasc_agr_antigo
   )
names(dados_nasc_agr) <- names(dados_nasc_agr) |> toupper()

# TPROBSON-PARTO ----------------------------------------------------------
## CORRIGINDO O ERRO DE 2013
TPROBSON_PARTO <-  read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/TPROBSON_PARTO_muni.csv"),
                               delim = ",", escape_double = FALSE, trim_ws = TRUE) #|>

  #rename(CODMUNRES = Municipio,
  #       ANO = Ano,
  #       NASC = Nascidos)
# TPROBSON_PARTO <- TPROBSON_PARTO |>
#   rbind(TPROBSON_PARTO_antigo[TPROBSON_PARTO_antigo$ANO == 2013,c('CODMUNRES', 'ANO', 'NASC','TPROBSON', 'PARTO')])

INCOMPLETUDE <- TPROBSON_PARTO %>%
  filter(PARTO  == 9 | TPROBSON %in% c(11,12) | PARTO %>% is.na() | TPROBSON %>% is.na())

INCOMPLETUDE <- INCOMPLETUDE |>
group_by(CODMUNRES, ANO) %>%
  summarise(INCOMPLETUDE = sum(NASC))


dados <- right_join(INCOMPLETUDE, dados_nasc_agr, by = c("CODMUNRES","ANO"))
dados_TPROBSON_PARTO <- dados %>%
  mutate(INCOMPLETUDE =  ifelse(is.na(INCOMPLETUDE), 0, INCOMPLETUDE)) %>% rename(TOTAIS = NASC)


dados_TPROBSON_PARTO$aux <- dados_TPROBSON_PARTO$CODMUNRES %>% substr(start = 1,stop =2)
UFS <- data.frame(
  COD = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
  UF= c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA',
            'MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF'))
dados_TPROBSON_PARTO$CODMUNRES <- dados_TPROBSON_PARTO$CODMUNRES %>% as.character()
dados_TPROBSON_PARTO <-merge(dados_TPROBSON_PARTO, UFS, by.x = "aux", by.y = "COD", all.x = TRUE)
dados_final_2 <- dados_TPROBSON_PARTO %>% select(-c(aux))
write.csv(dados_final_2,"data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/Base_3_2012-2023_v2.csv")
