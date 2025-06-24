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
library(readr)


## carregando dados

dados <- read_csv("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/Base_1_2012-2023_v2.csv") |>
  select(-...1) %>% rename(VARIAVEL = variavel)

# teste <- janitor::get_dupes(dados, c("CODMUNNASC", "ANO", "VARIAVEL"))


dados1 <- dados %>%
  group_by(CODMUNRES, ANO, VARIAVEL) %>%
  summarise(nulos = sum(NULOS),
            ignorados = sum(IGNORADOS),
            totais = sum(TOTAIS)) |>
  ungroup()
dados1 <- dados1 %>%
  rename(NULOS = nulos,
         IGNORADOS = ignorados,
         TOTAIS = totais)
teste <- janitor::get_dupes(dados1, c("CODMUNRES", "ANO", "VARIAVEL"))

# write.csv(dados1, "SINASC_Incompletude_v2_casos-unicos.csv")


dados2 <- dados1 %>%
  filter(ANO >= 2012 & ANO <= 2023)

dados3 <- dados2 %>%
  filter(VARIAVEL == "IDADEMAE" | VARIAVEL == "RACACORMAE" | VARIAVEL == "ESCMAE" |
           VARIAVEL == "QTDPARTNOR" | VARIAVEL == "QTDPARTCES" |
           VARIAVEL == "CONSPRENAT" | VARIAVEL == "MESPRENAT" |
           VARIAVEL == "PARTO" |  VARIAVEL == "TPROBSON" |
           VARIAVEL == "PESO" | VARIAVEL == "GESTACAO" | VARIAVEL == "SEMAGESTAC" |
           VARIAVEL == "IDANOMAL"
  ) %>%
  mutate(INCOMPLETOS = NULOS + IGNORADOS) %>%
  select(CODMUNRES, ANO, VARIAVEL, INCOMPLETOS, TOTAIS) # VARIAVEL1,

dados_aux <- read_csv("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/Base_3_2012-2023_v2.csv") |>
  select(-...1) |> mutate(VARIAVEL = 'PARTO_TPROBSON') |> rename(INCOMPLETOS = INCOMPLETUDE)
dados_aux$UF <- NULL

dados3<- rbind(dados3,dados_aux)
dadosa <- dcast(melt(dados3, id.vars=c("CODMUNRES", "ANO", "VARIAVEL")),
                CODMUNRES +  ANO ~ VARIAVEL + variable)

# dadosa <- dadosa |> select(-c(IDANOMAL_INCOMPLETOS, IDANOMAL_TOTAIS))

write.csv(dadosa, "data-raw/csv/incompletude_SINASC_2012-2023.csv")

