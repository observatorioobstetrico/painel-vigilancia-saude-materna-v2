library(microdatasus)
library(dplyr)
library(readr)
library(data.table)

tabela_aux_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv")


############## INDICADORES DA ABA FETAL

df_fetais_totais1 <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  vars = c("CODMUNRES", "DTOBITO",
                  "PESO",
                  "GESTACAO",
                  "SEMAGESTAC",
                  "OBITOPARTO"),
  information_system = "SIM-DOFET"
)

options(timeout = 600)

sim_2023 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv")

sim_dofet2023 <- sim_2023 |> filter(TIPOBITO == 1) |>
  select(c(CODMUNRES, DTOBITO, PESO, GESTACAO, SEMAGESTAC, OBITOPARTO))

sim_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN+(2).csv")

sim_dofet2024 <- sim_2024 |> filter(TIPOBITO == 1) |>
  select(c(CODMUNRES, DTOBITO, PESO, GESTACAO, SEMAGESTAC, OBITOPARTO))

df_fetais_totais <- rbind(df_fetais_totais1, sim_dofet2023) |>
  rbind(sim_dofet2024)

df_fetais_totais2 <- process_sim(df_fetais_totais, municipality_data = TRUE)

#write.csv(df_fetais_totais2, file="dados_sim_fetais.csv")

########### Filtrando para maior ou igual a 22 semanas ou peso>=500g

dados_fetais <- df_fetais_totais2 |>
  mutate(SEMAGESTAC = as.numeric(SEMAGESTAC), PESO = as.numeric(PESO)) |>
  filter(
    ((GESTACAO == "28 a 31 semanas" | GESTACAO == "32 a 36 semanas"  | GESTACAO == "37 a 41 semanas" | GESTACAO == "42 semanas e mais" | GESTACAO == "22 a 27 semanas") | (is.na(GESTACAO) & SEMAGESTAC >= 22 & SEMAGESTAC != 99))
    | (PESO >= 500)
  )

### Criando colunas

df_fetais <- dados_fetais |>
  mutate(
    ano1 = substr(DTOBITO, 1, 4),
    codmunres = CODMUNRES
  ) |>
  mutate(
    ano = case_when(
      ano1 == "23" ~ "2023",
      ano1 != "23" ~ ano1
    ),
    obitos= 1,
    peso_menos_1500 = case_when(
      PESO < 1500 ~ 1,
      !(PESO < 1500) ~ 0
    ),
    peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000) ~ 1,
      !(PESO >= 1500 & PESO < 2000) ~ 0
    ),
    peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500) ~ 1,
      !(PESO >= 2000 & PESO < 2500) ~ 0
    ),
    peso_mais_2500 = case_when(
      (PESO >= 2500) ~ 1,
      !(PESO >=2500) ~ 0
    ),
    sem_info_parto = case_when(
      is.na(OBITOPARTO) ~ 1,
      !is.na(OBITOPARTO) ~ 0
    ),
    antes = case_when(
      OBITOPARTO == "Antes" ~ 1,
      !(OBITOPARTO == "Antes") ~ 0
    ),
    durante = case_when(
      (OBITOPARTO == "Durante") ~ 1,
      !(OBITOPARTO == "Durante") ~ 0
    ),
    depois = case_when(
      (OBITOPARTO == "Depois") ~ 1,
      !(OBITOPARTO == "Depois") ~ 0
    ),
    antes_peso_menos_1500 = case_when(
      (PESO < 1500 & OBITOPARTO == "Antes") ~ 1,
      !(PESO < 1500 & OBITOPARTO == "Antes") ~ 0
    ),
    antes_peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000 & OBITOPARTO == "Antes") ~ 1,
      !(PESO >= 1500 & PESO < 2000 & OBITOPARTO == "Antes") ~ 0
    ),
    antes_peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500 & OBITOPARTO == "Antes") ~ 1,
      !(PESO >= 2000 & PESO < 2500 & OBITOPARTO == "Antes") ~ 0
    ),
    antes_peso_mais_2500 = case_when(
      (PESO >= 2500 & OBITOPARTO == "Antes") ~ 1,
      !(PESO >=2500 & OBITOPARTO == "Antes") ~ 0
    ),
    durante_peso_menos_1500 = case_when(
      (PESO < 1500 & OBITOPARTO == "Durante") ~ 1,
      !(PESO < 1500 & OBITOPARTO == "Durante") ~ 0
    ),
    durante_peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000 & OBITOPARTO == "Durante") ~ 1,
      !(PESO >= 1500 & PESO < 2000 & OBITOPARTO == "Durante") ~ 0
    ),
    durante_peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500 & OBITOPARTO == "Durante") ~ 1,
      !(PESO >= 2000 & PESO < 2500 & OBITOPARTO == "Durante") ~ 0
    ),
    durante_peso_mais_2500 = case_when(
      (PESO >= 2500 & OBITOPARTO == "Durante") ~ 1,
      !(PESO >=2500 & OBITOPARTO == "Durante") ~ 0
    ),
    depois_peso_menos_1500 = case_when(
      (PESO < 1500 & OBITOPARTO == "Depois") ~ 1,
      !(PESO < 1500 & OBITOPARTO == "Depois") ~ 0
    ),
    depois_peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000 & OBITOPARTO == "Depois") ~ 1,
      !(PESO >= 1500 & PESO < 2000 & OBITOPARTO == "Depois") ~ 0
    ),
    depois_peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500 & OBITOPARTO == "Depois") ~ 1,
      !(PESO >= 2000 & PESO < 2500 & OBITOPARTO == "Depois") ~ 0
    ),
    depois_peso_mais_2500 = case_when(
      (PESO >= 2500 & OBITOPARTO == "Depois") ~ 1,
      !(PESO >=2500 & OBITOPARTO == "Depois") ~ 0
    )
    # peso_menos_1500_sem_menos28 = case_when(
    #   (PESO < 1500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO < 1500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_1500_1999_sem_menos28 = case_when(
    #   (PESO >= 1500 & PESO < 2000) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 1500 & PESO < 2000) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_2000_2499_sem_menos28 = case_when(
    #   (PESO >= 2000 & PESO < 2500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2000 & PESO < 2500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_mais_2500_sem_menos28 = case_when(
    #   (PESO >= 2500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)) ~ 0
    # ),
    #
    # peso_menos_1500_sem_28_32 = case_when(
    #   (PESO < 1500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO < 1500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_1500_1999_sem_28_32 = case_when(
    #   (PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_2000_2499_sem_28_32 = case_when(
    #   (PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_mais_2500_sem_28_32 = case_when(
    #   (PESO >= 2500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_menos_1500_sem_33_34 = case_when(
    #   (PESO < 1500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO < 1500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_1500_1999_sem_33_34 = case_when(
    #   (PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_2000_2499_sem_33_34 = case_when(
    #   (PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_mais_2500_sem_33_34 = case_when(
    #   (PESO >= 2500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_menos_1500_sem_35_36 = case_when(
    #   (PESO < 1500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO < 1500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_1500_1999_sem_35_36 = case_when(
    #   (PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_2000_2499_sem_35_36 = case_when(
    #   (PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_mais_2500_sem_35_36 = case_when(
    #   (PESO >= 2500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)) ~ 0
    # ),
    # fetal_sem_menos28 = case_when(
    #   (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !(SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 0
    # ),
    # fetal_sem_28_32 = case_when(
    #   (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99) ~ 1,
    #   !(SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99) ~ 0
    # ),
    # fetal_sem_33_34 = case_when(
    #   (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99) ~ 1,
    #   !(SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99) ~ 0
    # ),
    # fetal_sem_35_36 = case_when(
    #   (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99) ~ 1,
    #   !(SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99) ~ 0
    # )

  ) |>
  select(-c(ano1))|>
  group_by(codmunres, ano) |>
  summarise(

    obitos_fetais_mais_22sem = sum(obitos),
    fetal_peso_menos_1500 = sum(peso_menos_1500, na.rm = T),
    fetal_peso_1500_1999 = sum(peso_1500_1999, na.rm = T),
    fetal_peso_2000_2499 = sum(peso_2000_2499, na.rm = T),
    fetal_peso_mais_2500 = sum(peso_mais_2500, na.rm = T),
    fetal_sem_info_parto = sum(sem_info_parto, na.rm = T),
    fetal_antes = sum(antes, na.rm = T),
    fetal_durante = sum(durante, na.rm = T),
    fetal_depois = sum(depois, na.rm = T),
    fetal_antes_peso_menos_1500 = sum(antes_peso_menos_1500, na.rm = T),
    fetal_antes_peso_1500_1999 = sum(antes_peso_1500_1999, na.rm = T),
    fetal_antes_peso_2000_2499 = sum(antes_peso_2000_2499, na.rm = T),
    fetal_antes_peso_mais_2500 = sum(antes_peso_mais_2500, na.rm = T),
    fetal_durante_peso_menos_1500 = sum(durante_peso_menos_1500, na.rm = T),
    fetal_durante_peso_1500_1999 = sum(durante_peso_1500_1999, na.rm = T),
    fetal_durante_peso_2000_2499 = sum(durante_peso_2000_2499, na.rm = T),
    fetal_durante_peso_mais_2500 = sum(durante_peso_mais_2500, na.rm = T),
    fetal_depois_peso_menos_1500 = sum(depois_peso_menos_1500, na.rm = T),
    fetal_depois_peso_1500_1999 = sum(depois_peso_1500_1999, na.rm = T),
    fetal_depois_peso_2000_2499 = sum(depois_peso_2000_2499, na.rm = T),
    fetal_depois_peso_mais_2500 = sum(depois_peso_mais_2500, na.rm = T)

    # fetal_peso_menos_1500_sem_menos28 = sum(peso_menos_1500_sem_menos28, na.rm=T),
    # fetal_peso_1500_1999_sem_menos28 = sum(peso_1500_1999_sem_menos28, na.rm=T),
    # fetal_peso_2000_2499_sem_menos28 = sum(peso_2000_2499_sem_menos28, na.rm=T),
    # fetal_peso_mais_2500_sem_menos28  = sum(peso_mais_2500_sem_menos28, na.rm=T),
    # fetal_peso_menos_1500_sem_28_32 = sum(peso_menos_1500_sem_28_32, na.rm=T),
    # fetal_peso_1500_1999_sem_28_32 = sum(peso_1500_1999_sem_28_32, na.rm=T),
    # fetal_peso_2000_2499_sem_28_32= sum(peso_2000_2499_sem_28_32, na.rm=T),
    # fetal_peso_mais_2500_sem_28_32  = sum(peso_mais_2500_sem_28_32, na.rm=T),
    # fetal_peso_menos_1500_sem_33_34 = sum(peso_menos_1500_sem_33_34, na.rm=T),
    # fetal_peso_1500_1999_sem_33_34 = sum(peso_1500_1999_sem_33_34, na.rm=T),
    # fetal_peso_2000_2499_sem_33_34= sum(peso_2000_2499_sem_33_34, na.rm=T),
    # fetal_peso_mais_2500_sem_33_34  = sum(peso_mais_2500_sem_33_34, na.rm=T),
    # fetal_peso_menos_1500_sem_35_36 = sum(peso_menos_1500_sem_35_36, na.rm=T),
    # fetal_peso_1500_1999_sem_35_36 = sum(peso_1500_1999_sem_35_36, na.rm=T),
    # fetal_peso_2000_2499_sem_35_36= sum(peso_2000_2499_sem_35_36, na.rm=T),
    # fetal_peso_mais_2500_sem_35_36  = sum(peso_mais_2500_sem_35_36, na.rm=T),
    # fetal_sem_menos28 = sum(fetal_sem_menos28, na.rm=T),
    # fetal_sem_28_32 = sum(fetal_sem_28_32, na.rm=T),
    # fetal_sem_33_34 = sum(fetal_sem_33_34, na.rm=T),
    # fetal_sem_35_36 = sum(fetal_sem_35_36, na.rm=T)

    ) |>
  ungroup()

codigos_municipios <- read_csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(municipio)

#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2022:2024)), ano = 2022:2024)

df_fetais <- df_fetais |> mutate_if(is.character, as.numeric)

df_obitos_fetais <- left_join(df_aux_municipios, df_fetais, by=c("codmunres", "ano")) |>
  select(
    codmunres,
    ano,
    obitos_fetais_mais_22sem,
    fetal_peso_menos_1500,
    fetal_peso_1500_1999,
    fetal_peso_2000_2499,
    fetal_peso_mais_2500,
    fetal_antes,
    fetal_durante,
    fetal_depois,
    fetal_antes_peso_menos_1500,
    fetal_antes_peso_1500_1999,
    fetal_antes_peso_2000_2499,
    fetal_antes_peso_mais_2500,
    fetal_durante_peso_menos_1500,
    fetal_durante_peso_1500_1999,
    fetal_durante_peso_2000_2499,
    fetal_durante_peso_mais_2500,
    fetal_depois_peso_menos_1500,
    fetal_depois_peso_1500_1999,
    fetal_depois_peso_2000_2499,
    fetal_depois_peso_mais_2500
    # fetal_peso_menos_1500_sem_menos28,
    # fetal_peso_1500_1999_sem_menos28,
    # fetal_peso_2000_2499_sem_menos28,
    # fetal_peso_mais_2500_sem_menos28,
    # fetal_peso_menos_1500_sem_28_32,
    # fetal_peso_1500_1999_sem_28_32,
    # fetal_peso_2000_2499_sem_28_32,
    # fetal_peso_mais_2500_sem_28_32,
    # fetal_peso_menos_1500_sem_33_34,
    # fetal_peso_1500_1999_sem_33_34,
    # fetal_peso_2000_2499_sem_33_34,
    # fetal_peso_mais_2500_sem_33_34,
    # fetal_peso_menos_1500_sem_35_36,
    # fetal_peso_1500_1999_sem_35_36,
    # fetal_peso_2000_2499_sem_35_36,
    # fetal_peso_mais_2500_sem_35_36,
    # fetal_sem_menos28,
    # fetal_sem_28_32,
    # fetal_sem_33_34,
    # fetal_sem_35_36
  )

df_obitos_fetais[is.na(df_obitos_fetais)] <- 0


df_obitos_fetais$obitos_fetais_mais_22sem[is.na(df_obitos_fetais$obitos_fetais_mais_22sem)] <- 0
df_obitos_fetais$fetal_peso_menos_1500[is.na(df_obitos_fetais$fetal_peso_menos_1500)] <- 0
df_obitos_fetais$fetal_peso_1500_1999[is.na(df_obitos_fetais$fetal_peso_1500_1999)] <- 0
df_obitos_fetais$fetal_peso_2000_2499[is.na(df_obitos_fetais$fetal_peso_2000_2499)] <- 0
df_obitos_fetais$fetal_peso_mais_2500[is.na(df_obitos_fetais$fetal_peso_mais_2500)] <- 0
df_obitos_fetais$fetal_antes[is.na(df_obitos_fetais$fetal_antes)] <- 0
df_obitos_fetais$fetal_durante[is.na(df_obitos_fetais$fetal_durante)] <- 0
df_obitos_fetais$fetal_depois[is.na(df_obitos_fetais$fetal_depois)] <- 0
df_obitos_fetais$fetal_antes_peso_menos_1500[is.na(df_obitos_fetais$fetal_antes_peso_menos_1500)] <- 0
df_obitos_fetais$fetal_antes_peso_1500_1999[is.na(df_obitos_fetais$fetal_antes_peso_1500_1999)] <- 0
df_obitos_fetais$fetal_antes_peso_2000_2499[is.na(df_obitos_fetais$fetal_antes_peso_2000_2499)] <- 0
df_obitos_fetais$fetal_antes_peso_mais_2500[is.na(df_obitos_fetais$fetal_antes_peso_mais_2500)] <- 0
df_obitos_fetais$fetal_durante_peso_menos_1500[is.na(df_obitos_fetais$fetal_durante_peso_menos_1500)] <- 0
df_obitos_fetais$fetal_durante_peso_1500_1999[is.na(df_obitos_fetais$fetal_durante_peso_1500_1999)] <- 0
df_obitos_fetais$fetal_durante_peso_2000_2499[is.na(df_obitos_fetais$fetal_durante_peso_2000_2499)] <- 0
df_obitos_fetais$fetal_durante_peso_mais_2500[is.na(df_obitos_fetais$fetal_durante_peso_mais_2500)] <- 0
df_obitos_fetais$fetal_depois_peso_menos_1500[is.na(df_obitos_fetais$fetal_depois_peso_menos_1500)] <- 0
df_obitos_fetais$fetal_depois_peso_1500_1999[is.na(df_obitos_fetais$fetal_depois_peso_1500_1999)] <- 0
df_obitos_fetais$fetal_depois_peso_2000_2499[is.na(df_obitos_fetais$fetal_depois_peso_2000_2499)] <- 0
df_obitos_fetais$fetal_depois_peso_mais_2500[is.na(df_obitos_fetais$fetal_depois_peso_mais_2500)] <- 0
# df_obitos_fetais$fetal_peso_menos_1500_sem_menos28[is.na(df_obitos_fetais$fetal_peso_menos_1500_sem_menos28)] <- 0
# df_obitos_fetais$fetal_peso_1500_1999_sem_menos28[is.na(df_obitos_fetais$fetal_peso_1500_1999_sem_menos28)] <- 0
# df_obitos_fetais$fetal_peso_2000_2499_sem_menos28[is.na(df_obitos_fetais$fetal_peso_2000_2499_sem_menos28)] <- 0
# df_obitos_fetais$fetal_peso_mais_2500_sem_menos28[is.na(df_obitos_fetais$fetal_peso_mais_2500_sem_menos28)] <- 0
# df_obitos_fetais$fetal_peso_menos_1500_sem_28_32[is.na(df_obitos_fetais$fetal_peso_menos_1500_sem_28_32)] <- 0
# df_obitos_fetais$fetal_peso_1500_1999_sem_28_32[is.na(df_obitos_fetais$fetal_peso_1500_1999_sem_28_32)] <- 0
# df_obitos_fetais$fetal_peso_2000_2499_sem_28_32[is.na(df_obitos_fetais$fetal_peso_2000_2499_sem_28_32)] <- 0
# df_obitos_fetais$fetal_peso_mais_2500_sem_28_32[is.na(df_obitos_fetais$fetal_peso_mais_2500_sem_28_32)] <- 0
# df_obitos_fetais$fetal_peso_menos_1500_sem_33_34[is.na(df_obitos_fetais$fetal_peso_menos_1500_sem_33_34)] <- 0
# df_obitos_fetais$fetal_peso_1500_1999_sem_33_34[is.na(df_obitos_fetais$fetal_peso_1500_1999_sem_33_34)] <- 0
# df_obitos_fetais$fetal_peso_2000_2499_sem_33_34[is.na(df_obitos_fetais$fetal_peso_2000_2499_sem_33_34)] <- 0
# df_obitos_fetais$fetal_peso_mais_2500_sem_33_34[is.na(df_obitos_fetais$fetal_peso_mais_2500_sem_33_34)] <- 0
# df_obitos_fetais$fetal_peso_menos_1500_sem_35_36[is.na(df_obitos_fetais$fetal_peso_menos_1500_sem_35_36)] <- 0
# df_obitos_fetais$fetal_peso_1500_1999_sem_35_36[is.na(df_obitos_fetais$fetal_peso_1500_1999_sem_35_36)] <- 0
# df_obitos_fetais$fetal_peso_2000_2499_sem_35_36[is.na(df_obitos_fetais$fetal_peso_2000_2499_sem_35_36)] <- 0
# df_obitos_fetais$fetal_peso_mais_2500_sem_35_36[is.na(df_obitos_fetais$fetal_peso_mais_2500_sem_35_36)] <- 0
# df_obitos_fetais$fetal_sem_menos28[is.na(df_obitos_fetais$fetal_sem_menos28)] <- 0
# df_obitos_fetais$fetal_sem_28_32[is.na(df_obitos_fetais$fetal_sem_28_32)] <- 0
# df_obitos_fetais$fetal_sem_33_34[is.na(df_obitos_fetais$fetal_sem_33_34)] <- 0
# df_obitos_fetais$fetal_sem_35_36[is.na(df_obitos_fetais$fetal_sem_35_36)] <- 0

# Juntando dados antigos e novos

df_obitos_fetais_antigo <- read_csv("data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2023.csv") |>
  filter(ano <= 2021) |>
  select(-c(`...1`))

df_obitos_fetais_novo <- rbind(df_obitos_fetais_antigo, df_obitos_fetais)

write.csv(df_obitos_fetais_novo, 'data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


######### INDICADORES DA ABA PERINATAL

# Óbitos fetais de idade gestacional maior ou igual a 28 semanas por ano e município

df_fetais_28sem <- df_fetais_totais2 |>
  mutate(SEMAGESTAC = as.numeric(SEMAGESTAC), PESO = as.numeric(PESO)) |>
  filter(
    ((GESTACAO == "28 a 31 semanas" | GESTACAO == "32 a 36 semanas" | GESTACAO == "37 a 41 semanas" | GESTACAO == "42 semanas e mais") | (is.na(GESTACAO) & SEMAGESTAC >= 28 & SEMAGESTAC != 99)) | (PESO >= 1000)
  )|>
  mutate(
    ano = substr(DTOBITO, 1, 4),
    codmunres = as.numeric(CODMUNRES)
  ) |>
  mutate(
    peso_menos_1500 = case_when(
      PESO < 1500 ~ 1,
      !(PESO < 1500) ~ 0
    ),
    peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000) ~ 1,
      !(PESO >= 1500 & PESO < 2000) ~ 0
    ),
    peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500) ~ 1,
      !(PESO >= 2000 & PESO < 2500) ~ 0
    ),
    peso_mais_2500 = case_when(
      (PESO >= 2500) ~ 1,
      !(PESO >=2500) ~ 0
    ),
    obitos = 1,
    .after = PESO
  )|>
  group_by(ano, codmunres) |>
  summarise(obitos_fetais_mais_28sem = sum(obitos),
            peso_menos_1500_mais_28sem = sum(peso_menos_1500, na.rm=T),
            peso_1500_1999_mais_28sem = sum(peso_1500_1999, na.rm=T),
            peso_2000_2499_mais_28sem = sum(peso_2000_2499, na.rm=T),
            peso_mais_2500_mais_28sem = sum(peso_mais_2500, na.rm=T)) |>
  ungroup()

############ Juntando as bases de dados
df_fetais_28sem <- df_fetais_28sem |> mutate_if(is.character, as.numeric)

juncao <- left_join(df_aux_municipios, df_fetais_28sem, by=c("codmunres", "ano"))

juncao[is.na(juncao)] <- 0


# juncao$obitos_fetais_mais_28sem[is.na(juncao$obitos_fetais_mais_28sem)] <- 0
# juncao$peso_menos_1500_mais_28sem[is.na(juncao$peso_menos_1500_mais_28sem)] <- 0
# juncao$peso_1500_1999_mais_28sem[is.na(juncao$peso_1500_1999_mais_28sem)] <- 0
# juncao$peso_2000_2499_mais_28sem[is.na(juncao$peso_2000_2499_mais_28sem)] <- 0
# juncao$peso_mais_2500_mais_28sem[is.na(juncao$peso_mais_2500_mais_28sem)] <- 0

df_obitos_perinatais <- juncao %>%
  select(
    codmunres,
    ano,
    obitos_fetais_mais_28sem,
    peso_menos_1500_mais_28sem,
    peso_1500_1999_mais_28sem,
    peso_2000_2499_mais_28sem,
    peso_mais_2500_mais_28sem
  )

df_obitos_perinatais_antigo <- read_csv("data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2023.csv") |>
  filter(ano <= 2021) |>
  select(-c(`...1`))

df_obitos_perinatais_novo <- rbind(df_obitos_perinatais_antigo, df_obitos_perinatais)

write.table(df_obitos_perinatais_novo, 'data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


######### INDICADORES DA ABA A NEONATAL

df_neonat_total <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  #vars = c("CODMUNRES", "DTOBITO", "IDADE", "PESO"),
  information_system = "SIM-DOINF"
)

sim_2023 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv")

sim23 <- sim_2023 |> select(-c(contador, OPOR_DO, TP_ALTERA, CB_ALT))

sim_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN+(2).csv")

sim24 <- sim_2024 |> select(-c(contador, OPOR_DO, TP_ALTERA, CB_ALT))

df_neonat_total <- df_neonat_total |> select(-c(ESTABDESCR, NUDIASOBIN,
                                                              NUDIASINF, FONTESINF,
                                                              CONTADOR))

preliminares <- rbind(sim23, sim24)

# df_sim_total2 <- process_sim(rbind(df_neonat_total, preliminares), municipality_data = T) |>
#   select(
#     CODMUNRES, DTOBITO, IDADEminutos, IDADEhoras, IDADEdias, PESO
#   )

df_sim_total2 <- rbind(df_neonat_total, preliminares) |>
  select(
    CODMUNRES, DTOBITO, IDADE, PESO
  )

df_neonat_total3 <- df_sim_total2  |>
  mutate(
    ano1 = substr(DTOBITO, 5, 8),
    codmunres = as.numeric(CODMUNRES),
    IDADE = as.numeric(IDADE),
    PESO = as.numeric(PESO)
  ) |>
  mutate(
     ano = case_when(
       ano1 == "023" ~ "2023",
       ano1 == "024" ~ "2024",
       TRUE ~ ano1
     ),
    obitos_27dias = case_when(
      (IDADE <= 227) ~ 1,
      !(IDADE <= 227) ~ 0
    ),
    obitos_27dias_menos1500 = case_when(
      ((IDADE <= 227) & PESO <1500)  ~ 1,
      !((IDADE <= 227) & PESO < 1500) ~ 0
    ),

    obitos_27dias_1500_1999 = case_when(
      ((IDADE <= 227) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 227) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_27dias_2000_2499 = case_when(
      ((IDADE <= 227) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 227) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_27dias_mais2500 = case_when(
      ((IDADE <= 227) & PESO >= 2500)  ~ 1,
      !((IDADE <= 227) & PESO >= 2500) ~ 0
    ),

    obitos_6dias = case_when(

      (IDADE <= 206) ~ 1,
      !(IDADE <= 206) ~ 0
    ),

    obitos_6dias_menos1500 = case_when(
      ((IDADE <= 206) & PESO <1500)  ~ 1,
      !((IDADE <= 206) & PESO < 1500) ~ 0
    ),

    obitos_6dias_1500_1999 = case_when(
      ((IDADE <= 206) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 206) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_6dias_2000_2499 = case_when(
      ((IDADE <= 206) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 206) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_6dias_mais2500 = case_when(
      ((IDADE <= 206) & PESO >= 2500)  ~ 1,
      !((IDADE <= 206) & PESO >= 2500) ~ 0
    ),

    obitos_0dias = case_when(

      (IDADE <= 200 ) ~ 1,
      !(IDADE <= 200 ) ~ 0
    ),

    obitos_0dias_menos1500 = case_when(
      ((IDADE <= 200 ) & PESO <1500)  ~ 1,
      !((IDADE <= 200 ) & PESO < 1500) ~ 0
    ),

    obitos_0dias_1500_1999 = case_when(
      ((IDADE <= 200 ) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 200 ) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_0dias_2000_2499 = case_when(
      ((IDADE <= 200 ) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 200 ) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_0dias_mais2500 = case_when(
      ((IDADE <= 200 ) & PESO >= 2500)  ~ 1,
      !((IDADE <= 200 ) & PESO >= 2500) ~ 0
    ),

    obitos_1_6dias = case_when(

      (IDADE <= 206 & IDADE >= 201) ~ 1,
      !(IDADE <= 206 & IDADE >= 201) ~ 0
    ),

    obitos_1_6dias_menos1500 = case_when(
      ((IDADE <= 206 & IDADE >= 201) & PESO <1500)  ~ 1,
      !((IDADE <= 206 & IDADE >= 201) & PESO < 1500) ~ 0
    ),

    obitos_1_6dias_1500_1999 = case_when(
      ((IDADE <= 206 & IDADE >= 201) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 206 & IDADE >= 201) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_1_6dias_2000_2499 = case_when(
      ((IDADE <= 206 & IDADE >= 201) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 206 & IDADE >= 201) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_1_6dias_mais2500 = case_when(
      ((IDADE <= 206 & IDADE >= 201) & PESO >= 2500)  ~ 1,
      !((IDADE <= 206 & IDADE >= 201) & PESO >= 2500) ~ 0
    ),

    obitos_7_27dias = case_when(

      (IDADE <= 227 & IDADE >= 207) ~ 1,
      !(IDADE <= 227 & IDADE >= 207) ~ 0
    ),

    obitos_7_27dias_menos1500 = case_when(
      ((IDADE <= 227 & IDADE >= 207) & PESO <1500)  ~ 1,
      !((IDADE <= 227 & IDADE >= 207) & PESO < 1500) ~ 0
    ),

    obitos_7_27dias_1500_1999 = case_when(
      ((IDADE <= 227 & IDADE >= 207) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 227 & IDADE >= 207) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_7_27dias_2000_2499 = case_when(
      ((IDADE <= 227 & IDADE >= 207) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 227 & IDADE >= 207) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_7_27dias_mais2500 = case_when(
      ((IDADE <= 227 & IDADE >= 207) & PESO >= 2500)  ~ 1,
      !((IDADE <= 227 & IDADE >= 207) & PESO >= 2500) ~ 0
    )

  ) |>
  select(-c(ano1)) |>
  group_by(codmunres, ano) |>
  summarise(

    obitos_27dias = sum( obitos_27dias,na.rm=T),
    obitos_27dias_menos1500 = sum( obitos_27dias_menos1500,na.rm=T),
    obitos_27dias_1500_1999 = sum( obitos_27dias_1500_1999,na.rm=T),
    obitos_27dias_2000_2499 = sum(obitos_27dias_2000_2499 ,na.rm=T),
    obitos_27dias_mais2500 = sum(obitos_27dias_mais2500 ,na.rm=T),
    obitos_6dias = sum(obitos_6dias ,na.rm=T),
    obitos_6dias_menos1500 = sum(obitos_6dias_menos1500 ,na.rm=T),
    obitos_6dias_1500_1999 = sum(obitos_6dias_1500_1999 ,na.rm=T),
    obitos_6dias_2000_2499 = sum(obitos_6dias_2000_2499 ,na.rm=T),
    obitos_6dias_mais2500 = sum(obitos_6dias_mais2500 ,na.rm=T),
    obitos_0dias = sum(obitos_0dias ,na.rm=T),
    obitos_0dias_menos1500 = sum(obitos_0dias_menos1500 ,na.rm=T),
    obitos_0dias_1500_1999 = sum(obitos_0dias_1500_1999 ,na.rm=T),
    obitos_0dias_2000_2499 = sum(obitos_0dias_2000_2499 ,na.rm=T),
    obitos_0dias_mais2500 = sum(obitos_0dias_mais2500 ,na.rm=T),
    obitos_1_6dias = sum(obitos_1_6dias ,na.rm=T),
    obitos_1_6dias_menos1500 = sum(obitos_1_6dias_menos1500 ,na.rm=T),
    obitos_1_6dias_1500_1999 = sum(obitos_1_6dias_1500_1999 ,na.rm=T),
    obitos_1_6dias_2000_2499 = sum(obitos_1_6dias_2000_2499 ,na.rm=T),
    obitos_1_6dias_mais2500 = sum(obitos_1_6dias_mais2500 ,na.rm=T),
    obitos_7_27dias = sum(obitos_7_27dias ,na.rm=T),
    obitos_7_27dias_menos1500 = sum(obitos_7_27dias_menos1500 ,na.rm=T),
    obitos_7_27dias_1500_1999 = sum(obitos_7_27dias_1500_1999 ,na.rm=T),
    obitos_7_27dias_2000_2499 = sum(obitos_7_27dias_2000_2499 ,na.rm=T),
    obitos_7_27dias_mais2500 = sum(obitos_7_27dias_mais2500 ,na.rm=T)
  )|>
  ungroup()


df_nascidos_total1_aux <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  vars = c("CODMUNRES", "DTNASC", "PESO", "GESTACAO", "SEMAGESTAC", "APGAR5"),
  information_system = "SINASC"
)

df_nascidos_total1 <- df_nascidos_total1_aux |>
  select(CODMUNRES, DTNASC, PESO)

options(timeout = 600)
sinasc23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";")
sinasc23_aux <- sinasc23 |>
  select(CODMUNRES, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5)

sinasc23 <- sinasc23 |>
  select(CODMUNRES, DTNASC, PESO)

sinasc24 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN24.csv", sep = ";")
sinasc24_aux <- sinasc24 |>
  select(CODMUNRES, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5)

sinasc24 <- sinasc24 |>
  select(CODMUNRES, DTNASC, PESO)

df_nascidos_total <- rbind(df_nascidos_total1, sinasc23) |>
  rbind(sinasc24)

df_nascidos_total2 <- process_sinasc(df_nascidos_total, municipality_data = T)

#write.csv(df_nascidos_total2, file="Bloco_7/Databases/dados_sinasc_neonat.csv")

df_nascidos_total3 <- df_nascidos_total2 |>
  mutate(
    ano = substr(DTNASC, 1, 4),
    codmunres = as.numeric(CODMUNRES),
    PESO = as.numeric(PESO)
  ) |>
  mutate(
    nascidos = 1,
    nascidos_menos1500 = case_when(
      (PESO < 1500) ~ 1,
      !(PESO < 1500) ~ 0
    ),
    nascidos_1500_1999 = case_when(
      (PESO >=  1500 & PESO<=1999) ~ 1,
      !((PESO >=  1500 & PESO<=1999)) ~ 0
    ),

    nascidos_2000_2499 = case_when(
      (PESO >=  2000 & PESO<=2499) ~ 1,
      !(PESO >=  2000 & PESO<=2499) ~ 0
    ),

    nascidos_mais2500 = case_when(
      (PESO >=  2500) ~ 1,
      !(PESO >=  2500) ~ 0
    )
  ) |>
  group_by(codmunres, ano) |>
  summarise(

    nascidos = sum(nascidos, na.rm=T),
    nascidos_menos1500 = sum(nascidos_menos1500, na.rm = T),
    nascidos_1500_1999 = sum(nascidos_1500_1999, na.rm = T),
    nascidos_2000_2499 = sum(nascidos_2000_2499, na.rm = T),
    nascidos_mais2500 = sum(nascidos_mais2500, na.rm = T)
  ) |>
  ungroup()

df_juncao <- left_join(df_nascidos_total3, df_neonat_total3, by = c("codmunres", "ano"))

df_juncao <- df_juncao |> mutate_if(is.character, as.numeric)

df_juncao[is.na(df_juncao)] <- 0

df_neonat <- left_join(df_aux_municipios, df_juncao, by=c("codmunres", "ano")) |>
  select(
    codmunres,
    ano,
    nascidos,
    nascidos_menos1500,
    nascidos_1500_1999,
    nascidos_2000_2499,
    nascidos_mais2500,
    obitos_27dias,
    obitos_6dias,
    obitos_7_27dias,
    obitos_27dias_menos1500,
    obitos_27dias_1500_1999,
    obitos_27dias_2000_2499,
    obitos_27dias_mais2500,
    obitos_6dias_menos1500,
    obitos_6dias_1500_1999,
    obitos_6dias_2000_2499,
    obitos_6dias_mais2500,
    obitos_7_27dias_menos1500,
    obitos_7_27dias_1500_1999,
    obitos_7_27dias_2000_2499,
    obitos_7_27dias_mais2500,
    obitos_0dias,
    obitos_0dias_menos1500,
    obitos_0dias_1500_1999,
    obitos_0dias_2000_2499,
    obitos_0dias_mais2500,
    obitos_1_6dias,
    obitos_1_6dias_menos1500,
    obitos_1_6dias_1500_1999,
    obitos_1_6dias_2000_2499,
    obitos_1_6dias_mais2500
  )

df_neonat_antigo <- read_csv("data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2023.csv") |>
  filter(ano <= 2021) |>
  select(-c(`...1`))

df_neonat_novo <- rbind(df_neonat_antigo, df_neonat)

write.csv(df_neonat_novo, 'data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


######### INDICADORES DE MORBIDADE NEONATAL

library(janitor)
library(RSQLite)
library(glue)
library(tidyr)
library(data.table)
library(readr)

df_nascidos_total_aux <- rbind(df_nascidos_total1_aux, sinasc23_aux) |>
  rbind(sinasc24_aux)


df_ameacadoras <- df_nascidos_total_aux |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    PESO = as.numeric(PESO),
    GESTACAO = as.numeric(GESTACAO),
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    APGAR5 = as.numeric(APGAR5),
    .keep = "unused",
  ) |>
  mutate(
    nascidos_condicoes_ameacadoras = if_else(PESO < 1500 | (GESTACAO < 4 | SEMAGESTAC < 32) | APGAR5 < 7, 1, 0, missing = 0),
    .keep = "unused"
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise_at(vars(contains("nascidos")), sum) |>
  ungroup()

# Para os indicadores provenientes do SIH ---------------------------------
## Criando um vetor com os anos considerados
anos <- c(2022:2024)

## Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

procedimentos_parto <- c("0310010012", "0310010039", "0310010047",
                         "0310010055", "0411010026", "0411010034",
                         "0411010042")

## Baixando os dados do SIH-RD
for (estado in estados) {
  # Criando data.frames que guardarão as bases do estado
  df_sih_rd_menores_28_uf <- data.frame()
  df_sih_rd_partos_uf <- data.frame()

  for (ano in anos) {

    erro_rd <- TRUE
    while (erro_rd) {
      erro_rd <- tryCatch({
        # Baixando os dados do SIH-RD para o dado ano e UF
        df_sih_rd_aux <- fetch_datasus(
          year_start = ano,
          year_end = ano,
          uf = estado,
          month_start = 1,
          month_end = 12,
          information_system = "SIH-RD",
          timeout = 500,
          stop_on_error = TRUE,
          vars = c(
            "CNES", "CEP", "MUNIC_RES", "MUNIC_MOV", "ANO_CMPT", "COD_IDADE", "IDADE", "NASC",
            "DT_INTER", "DT_SAIDA", "COBRANCA", "N_AIH", "DIAG_PRINC", "PROC_REA",
            "US_TOT", "UTI_MES_TO"
          )
        )

        # Criando um data.frame que contém apenas as internações de menores de 28 dias
        df_sih_rd_aux_menores_28 <- df_sih_rd_aux |>
          mutate(idade_dias = as.numeric(as.Date(DT_INTER, format = "%Y%m%d") - as.Date(NASC, format = "%Y%m%d"))) |>
          dplyr::filter(
            idade_dias < 28
          )

        # Criando um data.frame que contém apenas os partos
        df_sih_rd_aux_partos <- df_sih_rd_aux |>
          dplyr::filter(
            ((DIAG_PRINC >= "O32" & DIAG_PRINC <= "O36") | (DIAG_PRINC >= "O60" & DIAG_PRINC <= "O69") |
               (DIAG_PRINC >= "O75" & DIAG_PRINC < "O76") | (DIAG_PRINC >= "O80" & DIAG_PRINC <= "O84") |
               DIAG_PRINC == "P95") | (PROC_REA %in% procedimentos_parto)
          )

        erro_rd <- FALSE
      },
      warning = function(cond) return(TRUE)
      )
    }

    # Juntando com os dados dos anos anteriores para a dada UF
    df_sih_rd_menores_28_uf <- bind_rows(df_sih_rd_menores_28_uf, df_sih_rd_aux_menores_28)
    df_sih_rd_partos_uf <- bind_rows(df_sih_rd_partos_uf, df_sih_rd_aux_partos)


    # Limpando a memória
    rm(df_sih_rd_aux_menores_28, df_sih_rd_aux_partos)
    gc()
  }

  # Salvando as bases da UF
  write.csv2(
    df_sih_rd_menores_28_uf,
    gzfile(glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  write.csv2(
    df_sih_rd_partos_uf,
    gzfile(glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  # Limpando a memória
  rm(df_sih_rd_uf)
  gc()
}

## Criando os data.frames que guardarão as bases finais
df_sih_rd_menores_28 <- data.frame()
df_sih_rd_partos <- data.frame()

for (estado in estados) {
  df_sih_rd_menores_28_aux <- fread(
    glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_2022_2024.csv.gz"),
    sep = ";"
  )
  df_sih_rd_menores_28 <- bind_rows(df_sih_rd_menores_28, df_sih_rd_menores_28_aux)

  rm(df_sih_rd_menores_28_aux)
  gc()
}

for (estado in estados) {
  df_sih_rd_partos_aux <- fread(
    glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_2022_2024.csv.gz"),
    sep = ";"
  )
  df_sih_rd_partos <- bind_rows(df_sih_rd_partos, df_sih_rd_partos_aux)

  rm(df_sih_rd_partos_aux)
  gc()
}

## Salvando as bases completas
write.csv2(
  df_sih_rd_menores_28,
  glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)

write.csv2(
  df_sih_rd_partos,
  glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)


## Para os numeradores dos indicadores (número de internações/internações em UTI em menores de 28 dias) ----
### Lendo uma base com informações auxiliares dos municípios
df_infos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_aux_municipios.csv") |>
  mutate_if(is.numeric, as.character)

### Rodando o algoritmo da Claudia na base completa de internações em menores de 28 dias
#### Criando um vetor que contém o diretório original do projeto
diretorio_original <- getwd()

#### Criando um vetor que contém o diretório das bases brutas do SIH-RD
diretorio_bases_brutas <- glue("{getwd()}/data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH")

#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de internações
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_menores_28_dias_2022_2024.csv"))

#### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

#### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih_internacoes_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_internacoes <- left_join(
  df_aih_internacoes_aux,
  df_sih_rd_menores_28 |>
    select(ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV, idade_dias) |>
    mutate_at(vars(c(DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV)), as.character)
)

### Passando os casos para o formato wide (cada linha corresponderá a uma pessoa única)
df_aih_internacoes_wide <- df_aih_internacoes |>
  mutate(
    DT_INTER = as.Date(DT_INTER, format = "%Y%m%d"),
    DT_SAIDA = as.Date(DT_SAIDA, format = "%Y%m%d"),
    NASC = as.Date(NASC, format = "%Y%m%d")
  ) |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    CNES = first(CNES),  # CNES do estabelecimento da primeira internação
    MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
    MUNIC_MOV = first(MUNIC_MOV),  # Município do estabelecimento da primeira internação
    idade_dias = first(idade_dias),  # Idade, em dias, na data da primeira internação
    SOMA_UTI = sum(as.integer(UTI_MES_TO))  # Total de dias na UTI
  ) |>
  ungroup() |>
  select(ano = ANO_CMPT, codmunres = MUNIC_RES, codmunocor = MUNIC_MOV, cnes = CNES, aihref = AIHREF, idade_dias, soma_uti_mes_to = SOMA_UTI) |>
  # Filtrando apenas pelos casos em que os municípios de residência e ocorrência são considerados no painel
  filter(codmunres %in% df_infos_municipios$codmunres & codmunocor %in% df_infos_municipios$codmunres)

### Adicionando as indicadores de internação em UTI e de internação na macrorregião de residência
df_aih_internacoes_wide_macros <- df_aih_internacoes_wide |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_res = macro_r_saude), by = join_by(codmunres == codmunres)) |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_ocor = macro_r_saude), by = join_by(codmunocor == codmunres)) |>
  mutate(
    idade_cat = case_when(
      idade_dias == 0 ~ "0_dias",
      idade_dias >= 1 & idade_dias < 7 ~ "1_a_6_dias",
      idade_dias >= 7 & idade_dias <= 27 ~ "7_a_27_dias",
      TRUE ~ NA_character_
    )        #ifelse(idade_dias < 7, "menores_7_dias", "7_a_27_dias"),
,
    indicadora_mesma_macro = ifelse(macro_r_saude_res == macro_r_saude_ocor, "na_macro", "fora_macro"),
    indicadora_uti = ifelse(soma_uti_mes_to > 0, "internado_uti", "nao_internado_uti")
  ) |>
  select(!c(cnes, codmunocor, aihref, idade_dias, soma_uti_mes_to, macro_r_saude_res, macro_r_saude_ocor))

### Passando a base para o formato wide (um município por linha) e criando as variáveis necessárias
df_bloco5_sih_internacoes <- df_aih_internacoes_wide_macros |>
  group_by_all() |>
  summarise(num_internacoes = n()) |>
  ungroup() |>
  pivot_wider(
    names_from = c(indicadora_mesma_macro, idade_cat, indicadora_uti),
    values_from = num_internacoes,
    values_fill = 0,
    names_prefix = "internacoes_"
  ) |>
  right_join(data.frame(codmunres = rep(df_infos_municipios$codmunres, each = length(2022:2024)), ano = 2022:2024)) |>
  arrange(codmunres, ano) |>
  mutate(across(.cols = -c(codmunres, ano), .fns = ~ replace_na(., 0))) |>
  rowwise() |>
  mutate(
    # Para o indicador de internações geral, os nomes das variáveis seguem o padrão "internacoes_local-do-parto_idade-do-bebe"
    internacoes_na_macro_7_a_27_dias = sum(c_across(contains("na_macro_7_a_27_dias"))),
    internacoes_fora_macro_7_a_27_dias = sum(c_across(contains("fora_macro_7_a_27_dias"))),
    internacoes_na_macro_1_a_6_dias = sum(c_across(contains("na_macro_1_a_6_dias"))),
    internacoes_fora_macro_1_a_6_dias = sum(c_across(contains("fora_macro_1_a_6_dias"))),
    internacoes_na_macro_0_dias = sum(c_across(contains("na_macro_0_dias"))),
    internacoes_fora_macro_0_dias = sum(c_across(contains("fora_macro_0_dias"))),
    internacoes_geral_7_a_27_dias = internacoes_na_macro_7_a_27_dias + internacoes_fora_macro_7_a_27_dias,
    internacoes_geral_1_a_6_dias = internacoes_na_macro_1_a_6_dias + internacoes_fora_macro_1_a_6_dias,
    internacoes_geral_0_dias = internacoes_na_macro_0_dias + internacoes_fora_macro_0_dias,
    internacoes_na_macro_geral = internacoes_na_macro_7_a_27_dias + internacoes_na_macro_1_a_6_dias + internacoes_na_macro_0_dias,
    internacoes_fora_macro_geral = internacoes_fora_macro_7_a_27_dias + internacoes_fora_macro_1_a_6_dias + internacoes_fora_macro_0_dias,
    internacoes_geral_geral = internacoes_geral_7_a_27_dias + internacoes_geral_1_a_6_dias + internacoes_geral_0_dias,
    # Para o indicador de internações em UTI, os nomes das variáveis seguem o padrão "internacoes_local-do-parto_idade-do-bebe_internado_uti"
    internacoes_na_macro_geral_internado_uti = sum(c_across(contains("na_macro") & contains("dias_internado"))),
    internacoes_fora_macro_geral_internado_uti = sum(c_across(contains("fora_macro") & contains("dias_internado"))),
    internacoes_geral_7_a_27_dias_internado_uti = sum(c_across(contains("7_a_27_dias_internado"))),
    internacoes_geral_1_a_6_dias_internado_uti = sum(c_across(contains("1_a_6_dias_internado"))),
    internacoes_geral_0_dias_internado_uti = sum(c_across(contains("0_dias_internado"))),
    internacoes_geral_geral_internado_uti = internacoes_geral_7_a_27_dias_internado_uti + internacoes_geral_1_a_6_dias_internado_uti + internacoes_geral_0_dias_internado_uti
  ) |>
  select(ano, codmunres, ends_with("_6_dias"), ends_with("_27_dias"), ends_with("_0_dias"), ends_with("_geral"), ends_with("internado_uti") & !ends_with("nao_internado_uti"))

### Verificando se o total de internações equivale ao número de linhas das bases df_aih_wide/df_aih_wide_macros
sum(df_bloco5_sih_internacoes$internacoes_geral_geral) == nrow(df_aih_internacoes_wide_macros)

sum(df_bloco5_sih_internacoes$internacoes_geral_geral_internado_uti) == nrow(df_aih_internacoes_wide[df_aih_internacoes_wide$soma_uti_mes_to > 0, ])

sum(df_bloco5_sih_internacoes$internacoes_na_macro_geral_internado_uti, df_bloco5_sih_internacoes$internacoes_fora_macro_geral_internado_uti) ==
  sum(df_bloco5_sih_internacoes$internacoes_geral_7_a_27_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_1_a_6_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_0_dias_internado_uti)

sum(df_bloco5_sih_internacoes$internacoes_geral_7_a_27_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_1_a_6_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_0_dias_internado_uti) ==
  sum(df_bloco5_sih_internacoes$internacoes_geral_geral_internado_uti)


## Para o denominador dos indicadores (total de partos públicos) -----------
### Rodando o algoritmo da Claudia na base completa de partos
#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de partos
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_partos_2022_2024.csv"))

 #### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

#### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih_partos_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_partos <- left_join(
  df_aih_partos_aux,
  df_sih_rd_partos |>
    select(ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH) |>
    mutate_at(vars(c(DT_INTER, DT_SAIDA, N_AIH)), as.character)
)

### Passando as casos para o formato wide (cada linha corresponderá a uma pessoa única)
df_bloco5_sih_partos <- df_aih_partos |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
  ) |>
  ungroup() |>
  select(ano = ANO_CMPT, codmunres = MUNIC_RES, aihref = AIHREF) |>
  filter(codmunres %in% df_infos_municipios$codmunres) |>
  mutate(nascidos_estabelecimentos_publicos_sih = 1) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  # Criando a variável que contém o número nascimentos em hospitais públicos em cada município
  summarise(nascidos_estabelecimentos_publicos_sih = sum(nascidos_estabelecimentos_publicos_sih)) |>
  ungroup()

### Removendo arquivos já utilizados e que são maiores que o limite de 100 mb
file.remove(c(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_2022_2024.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_2022_2024.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/aihperm.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/aihpermtransf.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite"

))

# Juntandos os dados para a aba de morbidade

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2022:2024)), ano = 2022:2024)

df_bloco7_morbidade_neonatal <- left_join(df_aux_municipios, df_ameacadoras, by = c("codmunres", "ano")) |>
  left_join(df_bloco5_sih_partos) |>
  left_join(df_bloco5_sih_internacoes)

## Preenchendo os valores NAs, gerados após o left_join, com 0 (MENOS PARA 2023 PARA AS COLUNAS QUE VEM DO SIH, QUE AINDA NÃO FORAM ATUALIZADAS)
internacoes_cols <- grep("^internacoes|sih$", names(df_bloco7_morbidade_neonatal), value = TRUE)

for (col in names(df_bloco7_morbidade_neonatal)) {
  if (col %in% internacoes_cols) {
    # Nas colunas que começam com "internacoes" ou que terminam com "sih", substituir os NAs por 0 apenas se o ano não for 2023
    df_bloco7_morbidade_neonatal[[col]][is.na(df_bloco7_morbidade_neonatal[[col]])] <- 0 #& df_bloco7_morbidade_neonatal$ano != 2023
  } else {
    # Nas outras colunas, substituir todos os NAs por 0
    df_bloco7_morbidade_neonatal[[col]][is.na(df_bloco7_morbidade_neonatal[[col]])] <- 0
  }
}

df_bloco7_morbidade_neonatal_antigo <- read_csv("data-raw/csv/indicadores_bloco7_morbidade_neonatal_2012-2023.csv") |>
  filter(ano <= 2021)

df_bloco7_morbidade_neonatal_novo <- rbind(df_bloco7_morbidade_neonatal_antigo, df_bloco7_morbidade_neonatal)

write.csv(df_bloco7_morbidade_neonatal_novo, 'data-raw/csv/indicadores_bloco7_morbidade_neonatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)








