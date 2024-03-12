library(microdatasus)
library(dplyr)
library(janitor)
library(tidyr)

# Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/databases-antigas/tabela_aux_municipios.csv") |>
  pull(codmunres)

# Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(as.character(codigos_municipios), each = length(2012:2022)), ano = 2012:2022)

# Criando o data.frame que irá receber todos os dados dos gráficos do bloco 8
df_bloco8_graficos <- df_aux_municipios


# Garbage codes para óbitos maternos --------------------------------------
## Baixando os dados do SIM-DOMAT de 2012 a 2022
df_sim_domat_aux <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  information_system = "SIM-DOMAT"
) |>
  clean_names()

## Criando a variável de ano e limitando a variável 'causabas' a três caracteres
df_sim_domat <- df_sim_domat_aux |>
  mutate_if(is.numeric, as.character) |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    causabas = substr(causabas, 1, 3)
  )

## Criando um vetor que recebe as CIDs consideradas garbage codes
garbage_codes_materno <- c("D39", "F53", "O08", "O94", "O95")

## Criando um data.frame com os óbitos maternos totais
df_maternos_totais <- df_sim_domat |>
  select(codmunres, ano) |>
  mutate(obitos_maternos_totais = 1) |>
  group_by(across(!obitos_maternos_totais)) |>
  summarise(obitos_maternos_totais = sum(obitos_maternos_totais)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_maternos_totais[is.na(df_maternos_totais)] <- 0

## Criando um data.frame com os óbitos maternos preenchidos com garbage codes
df_maternos_garbage <- df_sim_domat |>
  filter(grepl(paste(garbage_codes_materno, collapse = "|"), causabas)) |>
  select(codmunres, ano, causabas) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = causabas,
    values_from = obitos,
    values_fill = 0
  ) |>
  clean_names() |>
  rename_with(.fn = ~ paste0('garbage_materno_', .), .cols = -c(ano, codmunres)) |>
  right_join(df_aux_municipios) |>
  right_join(df_maternos_totais) |>
  arrange(codmunres)

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_maternos_garbage)

## Para a tabela de garbage codes em óbitos maternos
df_cid10 <- read.csv("data-raw/extracao-dos-dados/databases-antigas/df_cid10.csv")

df_maternos_garbage_tabela <- df_sim_domat |>
  filter(grepl(paste(garbage_codes_materno, collapse = "|"), causabas)) |>
  select(codmunres, ano, causabas) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  left_join(df_cid10 |> select(causabas, causabas_categoria)) |>
  select(!causabas) |>
  pivot_wider(
    names_from = causabas_categoria,
    values_from = obitos,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) |>
  pivot_longer(
    cols = !c(codmunres, ano),
    names_to = "causabas_categoria",
    values_to = "obitos"
  ) |>
  left_join(df_cid10 |> select(!causabas)) |>
  select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_categoria, obitos) |>
  arrange(codmunres)

write.csv(df_maternos_garbage_tabela, "data-raw/csv/garbage_materno_2012_2022.csv", row.names = FALSE)


## Removendo objetos já utilizados
rm(df_maternos_garbage, df_maternos_totais, df_sim_domat, df_sim_domat_aux, garbage_codes_materno)


# Garbage codes para óbitos fetais ----------------------------------------
## Baixando os dados do SIM-DOFET de 2012 a 2022
df_sim_dofet_aux <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  information_system = "SIM-DOFET"
) |>
  clean_names()

## Criando a variável de ano, limitando a variável 'causabas' a três caracteres e filtrando apenas pelos óbitos fetais que consideramos
df_sim_dofet <- df_sim_dofet_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    causabas = substr(causabas, 1, 3)
  ) |>
  filter(
    ((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 22 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 500)
  )

## Criando um data.frame com os óbitos fetais totais
df_fetais_totais <- df_sim_dofet |>
  select(codmunres, ano) |>
  mutate(obitos_fetais_totais = 1) |>
  group_by(across(!obitos_fetais_totais)) |>
  summarise(obitos_fetais_totais = sum(obitos_fetais_totais)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_fetais_totais[is.na(df_fetais_totais)] <- 0

## Criando um data.frame com os óbitos maternos preenchidos com garbage codes
df_fetais_garbage <- df_sim_dofet |>
  mutate(
    grupo_cid = case_when(
      causabas >= "P90" & causabas <= "P96" ~ "garbage_fetal_p90_p96",
      causabas >= "Q10" & causabas <= "Q18" ~ "garbage_fetal_q10_q18",
      causabas >= "Q35" & causabas <= "Q37" ~ "garbage_fetal_q35_q37",
      causabas >= "Q80" & causabas <= "Q89" ~ "garbage_fetal_q80_q89",
      causabas >= "Q90" & causabas <= "Q99" ~ "garbage_fetal_q90_q99"
    )
  ) |>
  filter(!is.na(grupo_cid)) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = obitos,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  right_join(df_fetais_totais) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_fetais_garbage[is.na(df_fetais_garbage)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_garbage)

## Removendo objetos já utilizados
rm(df_fetais_garbage, df_fetais_totais, df_sim_dofet, df_sim_dofet_aux)


# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco8_graficos, "data-raw/csv/indicadores_bloco8_graficos_2012-2022.csv", row.names = FALSE)
