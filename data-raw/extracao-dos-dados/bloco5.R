library("microdatasus")
library("dplyr")
library("janitor")
library("readr")

df <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  vars = c("CODMUNRES",
           "DTNASC",
           "PESO",
           "GESTACAO",
           "SEMAGESTAC"),
  information_system = "SINASC"
)

df_proces <- process_sinasc(df, municipality_data = T) 

#write.csv(df_proces, "Bloco_5/Databases/dados_sinasc_bloco5.csv")

df2 <- df_proces |>
  mutate(
    ano = substr(DTNASC, 1, 4),
    codmunres = as.numeric(CODMUNRES),
    PESO = as.numeric(PESO),
    SEMAGESTAC = as.numeric(SEMAGESTAC)
    
  ) |>  
  mutate(
    nascidos= 1,
    nascidos_vivos_com_baixo_peso = case_when(
      (PESO < 2500) ~ 1,
      !(PESO < 2500) ~ 0
    ),
    nascidos_vivos_prematuros = case_when(
      (GESTACAO != "37 a 41 semanas" & GESTACAO != "42 semanas ou mais" & !is.na(GESTACAO)) ~ 1,
      !(GESTACAO != "37 a 41 semanas" & GESTACAO != "42 semanas ou mais" & !is.na(GESTACAO)) ~ 0
    ),
    nascidos_vivos_termo_precoce = case_when(
      (SEMAGESTAC == 37 | SEMAGESTAC == 38) ~ 1,
      !(SEMAGESTAC == 37 | SEMAGESTAC == 38) ~ 0
    ),
    nascidos_vivos_peso_menor_1500 = case_when(
      (PESO < 1500) ~ 1,
      !(PESO < 1500) ~ 0
    ),
    
    nascidos_vivos_peso_1500_a_1999 = case_when(
      (PESO >= 1500  & PESO <= 1999) ~ 1,
      !(PESO >= 1500  & PESO <= 1999) ~ 0
    ),
    
    nascidos_vivos_peso_2000_a_2499 = case_when(
      (PESO <= 2499 & PESO >= 2000) ~ 1,
      !(PESO <= 2499 & PESO >= 2000) ~ 0
    ),
    
    nascidos_vivos_menos_de_28_semanas = case_when(
      (SEMAGESTAC < 28) ~ 1,
      !(SEMAGESTAC < 28) ~ 0
    ),
    
    nascidos_vivos_28_a_32_semanas = case_when(
      (SEMAGESTAC <= 32 & SEMAGESTAC >= 28) ~ 1,
      !(SEMAGESTAC <= 32 & SEMAGESTAC >= 28) ~ 0
    ),
    nascidos_vivos_33_a_34_semanas = case_when(
      (SEMAGESTAC <= 34 & SEMAGESTAC >= 33) ~ 1,
      !(SEMAGESTAC <= 34 & SEMAGESTAC >= 33) ~ 0
    ),
    nascidos_vivos_35_a_36_semanas = case_when(
      (SEMAGESTAC <= 36 & SEMAGESTAC >= 35) ~ 1,
      !(SEMAGESTAC <= 36 & SEMAGESTAC >= 35) ~ 0
    )
    
  ) |>
  #select(codmunres, ano, nascidos) |>
  group_by(codmunres, ano) |>
  summarise(
    total_de_nascidos_vivos = sum(nascidos),
    nascidos_vivos_com_baixo_peso = sum(nascidos_vivos_com_baixo_peso, na.rm=T),
    nascidos_vivos_prematuros =  sum(nascidos_vivos_prematuros, na.rm = T),
    nascidos_vivos_termo_precoce = sum(nascidos_vivos_termo_precoce, na.rm = T),
    nascidos_vivos_peso_menor_1500 = sum(nascidos_vivos_peso_menor_1500, na.rm = T),
    nascidos_vivos_peso_1500_a_1999 = sum(nascidos_vivos_peso_1500_a_1999, na.rm = T),
    nascidos_vivos_peso_2000_a_2499 = sum(nascidos_vivos_peso_2000_a_2499, na.rm = T),
    nascidos_vivos_menos_de_28_semanas = sum(nascidos_vivos_menos_de_28_semanas, na.rm = T),
    nascidos_vivos_28_a_32_semanas = sum(nascidos_vivos_28_a_32_semanas, na.rm = T),
    nascidos_vivos_33_a_34_semanas = sum(nascidos_vivos_33_a_34_semanas, na.rm = T),
    nascidos_vivos_35_a_36_semanas = sum(nascidos_vivos_35_a_36_semanas , na.rm = T)
    
  ) |>
  ungroup()

#Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres)

#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2022)), ano = 2012:2022)

df2 <- df2 |> mutate_if(is.character, as.numeric)

##Fazendo um left_join da base auxiliar de municípios com o data.frame que contém o total de nascidos vivos
df_bloco5 <- left_join(df_aux_municipios, df2, by = c("codmunres", "ano"))

df_bloco5[is.na(df_bloco5)] <- 0

write.csv(df_bloco5, "data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2022.csv", row.names = FALSE)
