library(microdatasus)
library(dplyr)
library(readr)
library(data.table)
library(janitor)
library(tidyr)
library(readxl)

####### INCOMPLETUDE FETAL

codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando data.frames que irão receber os dados dos indicadores de causas evitáveis e grupos de causa
df_incompletude_bloco7 <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

df_dofet_consolidado <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  vars = c("CODMUNRES", "DTOBITO", "PESO", "GESTACAO", "SEMAGESTAC", "OBITOPARTO"),
  information_system = "SIM-DOFET"
) |>
  clean_names() |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    peso = as.numeric(peso),
    semagestac = as.numeric(semagestac),
    gestacao = as.numeric(gestacao),
    obitoparto = as.numeric(obitoparto)
  )

df_sim_dofet_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv") |>
  clean_names() |>
  filter(tipobito == 1) |>
  select(codmunres, dtobito, peso, gestacao, semagestac, obitoparto)|>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    peso = as.numeric(peso),
    semagestac = as.numeric(semagestac),
    gestacao = as.numeric(gestacao),
    obitoparto = as.numeric(obitoparto)
  )

df_sim_dofet <- rbind(df_dofet_consolidado, df_sim_dofet_2024)

df_fetal_incompletude <- df_sim_dofet |>
  mutate(
    incompletude_fetal_peso_ig = case_when(
      (is.na(peso) & (is.na(gestacao) | gestacao==9) &
         (is.na(semagestac)| semagestac == 99)) ~ 1,
      TRUE ~0
    ),
    incompletude_fetal_momento = case_when(
       (obitoparto == 9 | is.na(obitoparto)) ~ 1,
      TRUE ~0
    ),
    incompletude_fetal_peso = case_when(
      is.na(peso) ~1,
      TRUE ~0
    ),
    total_dofet = 1
  ) |>
  group_by(ano, codmunres) |>
  summarise(
    incompletude_fetal_peso_ig = sum(incompletude_fetal_peso_ig),
    incompletude_fetal_momento = sum(incompletude_fetal_momento),
    incompletude_fetal_peso = sum(incompletude_fetal_peso),
    total_dofet = sum(total_dofet)
  )

df_incompletude_bloco7 <- left_join(df_incompletude_bloco7, df_fetal_incompletude, by = c("ano", "codmunres"))


########### INCOMPLETUDE NEONATAL

df_doinf_consolidado <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  vars = c("CODMUNRES", "DTOBITO", "IDADE", "PESO"),
  information_system = "SIM-DOINF"
) |>
  clean_names() |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    idade = as.numeric(idade),
    peso = as.numeric(peso)
  )


df_sim_doinf_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv") |>
  clean_names() |>
  filter(idade <= 400) |>
  select(codmunres, dtobito, peso, idade)|>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    peso = as.numeric(peso),
    idade = as.numeric(idade)
  )

df_sim_doinf <- rbind(df_doinf_consolidado, df_sim_doinf_2024)

df_neonatal_incompletude <- df_sim_doinf |>
  mutate(
    incompletude_neonatal_idade = case_when(
      idade == 000 | is.na(idade) ~ 1,
      TRUE ~ 0
    ),
    incompletude_neonatal_peso = case_when(
       is.na(peso) ~ 1,
       TRUE ~ 0
    ),
    total_doinf = 1
  ) |>
  group_by(ano, codmunres) |>
  summarise(
    incompletude_neonatal_idade = sum(incompletude_neonatal_idade),
    incompletude_neonatal_peso = sum(incompletude_neonatal_peso),
    total_doinf = sum(total_doinf)
  )

df_incompletude_bloco7 <- left_join(df_incompletude_bloco7, df_neonatal_incompletude, by = c("ano", "codmunres"))


write.csv(df_incompletude_bloco7, 'data-raw/csv/indicadores_incompletude_bloco7_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)
