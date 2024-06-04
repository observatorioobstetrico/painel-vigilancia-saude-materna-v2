library(microdatasus)
library(dplyr)
library(readr)

tabela_aux_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv")


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

sim_2023 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv")

sim_dofet2023 <- sim_2023 |> filter(TIPOBITO == 1) |>
  select(c(CODMUNRES, DTOBITO, PESO, GESTACAO, SEMAGESTAC, OBITOPARTO))

df_fetais_totais <- rbind(df_fetais_totais1, sim_dofet2023)

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

codigos_municipios <- read_csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(municipio)

#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2022:2023)), ano = 2022:2023)

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



#write.table(df_obitos_fetais, 'Bloco_7/indicadores_bloco7_mortalidade_fetal_2021-2022.csv', sep = ",", dec = ".", row.names = FALSE)


write.csv(df_obitos_fetais, 'data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2022.csv', sep = ",", dec = ".", row.names = FALSE)


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


write.table(df_obitos_perinatais, 'data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2023.csv', sep = ",", dec = ".", row.names = FALSE)


######### INDICADORES DA ABA A NEONATAL

df_neonat_total <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  #vars = c("CODMUNRES", "DTOBITO", "IDADE", "PESO"),
  information_system = "SIM-DOINF"
)

sim_2023 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv")

sim23 <- sim_2023 |> select(-c(contador, OPOR_DO, TP_ALTERA, CB_ALT))

df_neonat_total <- df_neonat_total |> select(-c(ESTABDESCR, NUDIASOBIN,
                                                              NUDIASINF, FONTESINF,
                                                              CONTADOR))

df_sim_total2 <- process_sim(rbind(df_neonat_total, sim23), municipality_data = T) |>
  select(
    CODMUNRES, DTOBITO, IDADEminutos, IDADEhoras, IDADEdias, PESO
  )

#write.csv(df_neonat_total2, file="Bloco_7/Databases/dados_sim_neonatal.csv")

df_neonat_total3 <- df_sim_total2  |>
  mutate(
    ano1 = substr(DTOBITO, 1, 4),
    codmunres = as.numeric(CODMUNRES),
    IDADEminutos = as.numeric(IDADEminutos),
    IDADEhoras = as.numeric(IDADEhoras),
    IDADEdias = as.numeric(IDADEdias),
    PESO = as.numeric(PESO)
  ) |>
  mutate(
    ano = case_when(
      ano1 == "23" ~ "2023",
      TRUE ~ ano1
    ),
    obitos_27dias = case_when(
      (IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) ~ 1,
      !(IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) ~ 0
    ),
    obitos_27dias_menos1500 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) & PESO <1500)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) & PESO < 1500) ~ 0
    ),

    obitos_27dias_1500_1999 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_27dias_2000_2499 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_27dias_mais2500 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) & PESO >= 2500)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 27) & PESO >= 2500) ~ 0
    ),

    obitos_6dias = case_when(

      (IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) ~ 1,
      !(IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) ~ 0
    ),

    obitos_6dias_menos1500 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) & PESO <1500)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) & PESO < 1500) ~ 0
    ),

    obitos_6dias_1500_1999 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_6dias_2000_2499 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_6dias_mais2500 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) & PESO >= 2500)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 | IDADEdias <= 6) & PESO >= 2500) ~ 0
    ),

    obitos_0dias = case_when(

      (IDADEminutos <= 59| IDADEhoras <= 23 ) ~ 1,
      !(IDADEminutos <= 59| IDADEhoras <= 23 ) ~ 0
    ),

    obitos_0dias_menos1500 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 ) & PESO <1500)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 ) & PESO < 1500) ~ 0
    ),

    obitos_0dias_1500_1999 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 ) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 ) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_0dias_2000_2499 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 ) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 ) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_0dias_mais2500 = case_when(
      ((IDADEminutos <= 59| IDADEhoras <= 23 ) & PESO >= 2500)  ~ 1,
      !((IDADEminutos <= 59| IDADEhoras <= 23 ) & PESO >= 2500) ~ 0
    ),

    obitos_1_6dias = case_when(

      (IDADEdias <= 6 & IDADEdias >= 1) ~ 1,
      !(IDADEdias <= 6 & IDADEdias >= 1) ~ 0
    ),

    obitos_1_6dias_menos1500 = case_when(
      ((IDADEdias <= 6 & IDADEdias >= 1) & PESO <1500)  ~ 1,
      !((IDADEdias <= 6 & IDADEdias >= 1) & PESO < 1500) ~ 0
    ),

    obitos_1_6dias_1500_1999 = case_when(
      ((IDADEdias <= 6 & IDADEdias >= 1) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADEdias <= 6 & IDADEdias >= 1) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_1_6dias_2000_2499 = case_when(
      ((IDADEdias <= 6 & IDADEdias >= 1) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADEdias <= 6 & IDADEdias >= 1) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_1_6dias_mais2500 = case_when(
      ((IDADEdias <= 6 & IDADEdias >= 1) & PESO >= 2500)  ~ 1,
      !((IDADEdias <= 6 & IDADEdias >= 1) & PESO >= 2500) ~ 0
    ),

    obitos_7_27dias = case_when(

      (IDADEdias <= 27 & IDADEdias >= 7) ~ 1,
      !(IDADEdias <= 27 & IDADEdias >= 7) ~ 0
    ),

    obitos_7_27dias_menos1500 = case_when(
      ((IDADEdias <= 27 & IDADEdias >= 7) & PESO <1500)  ~ 1,
      !((IDADEdias <= 27 & IDADEdias >= 7) & PESO < 1500) ~ 0
    ),

    obitos_7_27dias_1500_1999 = case_when(
      ((IDADEdias <= 27 & IDADEdias >= 7) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADEdias <= 27 & IDADEdias >= 7) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_7_27dias_2000_2499 = case_when(
      ((IDADEdias <= 27 & IDADEdias >= 7) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADEdias <= 27 & IDADEdias >= 7) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_7_27dias_mais2500 = case_when(
      ((IDADEdias <= 27 & IDADEdias >= 7) & PESO >= 2500)  ~ 1,
      !((IDADEdias <= 27 & IDADEdias >= 7) & PESO >= 2500) ~ 0
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


df_nascidos_total1 <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  vars = c("CODMUNRES", "DTNASC", "PESO"),
  information_system = "SINASC"
)

sinasc23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";")
sinasc23 <- sinasc23 |>
  select(CODMUNRES, DTNASC, PESO)

df_nascidos_total <- rbind(df_nascidos_total1, sinasc23)

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

write.csv(df_neonat, 'data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2023.csv', sep = ",", dec = ".", row.names = FALSE)










