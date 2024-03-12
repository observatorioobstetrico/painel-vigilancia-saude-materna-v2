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

## Substituindo todos os NAs por 0 (gerados após o right join)
df_maternos_garbage[is.na(df_maternos_garbage)] <- 0

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

write.csv(df_maternos_garbage_tabela, gzfile("data-raw/csv/garbage_materno_2012_2022.csv.gz"), row.names = FALSE)


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
rm(df_fetais_garbage)

# Principais causas para óbitos fetais -------------------------------------------------------

df_fetais_causas <- df_sim_dofet |>
  mutate(
    causabas = substr(causabas, 1, 3),
    grupo_cid = case_when(
      causabas >= "P00" & causabas <= "P04" ~ "fetal_causas_p00_p04",
      causabas >= "P05" & causabas <= "P08" ~ "fetal_causas_p05_p08",
      causabas >= "P10" & causabas <= "P15" ~ "fetal_causas_p10_p15",
      causabas >= "P20" & causabas <= "P29" ~ "fetal_causas_p20_p29",
      causabas >= "P35" & causabas <= "P39" ~ "fetal_causas_p35_p39",
      causabas >= "P50" & causabas <= "P61" ~ "fetal_causas_p50_p61",
      causabas >= "P70" & causabas <= "P74" ~ "fetal_causas_p70_p74",
      causabas >= "P75" & causabas <= "P78" ~ "fetal_causas_p75_p78",
      causabas >= "P80" & causabas <= "P83" ~ "fetal_causas_p80_p83",
      causabas >= "P90" & causabas <= "P96" ~ "fetal_causas_p90_p96",
      causabas >= "Q00" & causabas <= "Q99" ~ "fetal_causas_q00_q99",
      causabas >= "J00" & causabas <= "J99" ~ "fetal_causas_j00_j99",
      causabas >= "A00" & causabas <= "A99" |
        causabas >= "B00" & causabas <= "B99" ~ "fetal_causas_a00_b99",
      TRUE ~ "fetal_causas_outros"
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
df_fetais_causas[is.na(df_fetais_causas)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_causas)

## Removendo objetos já utilizados
rm(df_fetais_causas)


# Principais causas para óbitos neonatais ---------------------------------
###Baixando os dados óbitos SIM-DOINF de 2012 a 2022
df_sim_doinf_aux <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  information_system = "SIM-DOINF",
) |>
  clean_names()

## Criando a variável de ano, limitando a variável 'causabas' a três caracteres e filtrando apenas os óbitos neonatais
df_sim_doinf <- df_sim_doinf_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito)))
  ) |>
  filter(
    idade < 228
  )

## Criando um data.frame com os óbitos neonatais totais
df_neonatais_totais <- df_sim_doinf |>
  select(codmunres, ano) |>
  mutate(obitos_neonatais_totais = 1) |>
  group_by(across(!obitos_neonatais_totais)) |>
  summarise(obitos_neonatais_totais = sum(obitos_neonatais_totais)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_totais[is.na(df_neonatais_totais)] <- 0

df_neonatais_causas <- df_sim_doinf |>
  mutate(
    causabas = substr(causabas, 1, 3),
    grupo_cid = case_when(
      causabas >= "P00" & causabas <= "P04" ~ "neonatal_causas_p00_p04",
      causabas >= "P05" & causabas <= "P08" ~ "neonatal_causas_p05_p08",
      causabas >= "P10" & causabas <= "P15" ~ "neonatal_causas_p10_p15",
      causabas >= "P20" & causabas <= "P29" ~ "neonatal_causas_p20_p29",
      causabas >= "P35" & causabas <= "P39" ~ "neonatal_causas_p35_p39",
      causabas >= "P50" & causabas <= "P61" ~ "neonatal_causas_p50_p61",
      causabas >= "P70" & causabas <= "P74" ~ "neonatal_causas_p70_p74",
      causabas >= "P75" & causabas <= "P78" ~ "neonatal_causas_p75_p78",
      causabas >= "P80" & causabas <= "P83" ~ "neonatal_causas_p80_p83",
      causabas >= "P90" & causabas <= "P96" ~ "neonatal_causas_p90_p96",
      causabas >= "Q00" & causabas <= "Q99" ~ "neonatal_causas_q00_q99",
      causabas >= "J00" & causabas <= "J99" ~ "neonatal_causas_j00_j99",
      causabas >= "A00" & causabas <= "A99" |
        causabas >= "B00" & causabas <= "B99" ~ "neonatal_causas_a00_b99",
      TRUE ~ "neonatal_causas_outros"

    )
    # tipo_do_obito_neonatal = case_when(
    #   idade < 207 ~ "Neonatal precoce",
    #   idade >= 207 & idade < 228 ~ "Neonatal tardio",
    #   idade >= 228 ~ "Pós-neonatal"
    # )
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
  right_join(df_neonatais_totais) |>
  arrange(codmunres)


## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_causas[is.na(df_neonatais_causas)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_causas)

## Removendo objetos já utilizados
rm(df_neonatais_causas)



# Vetores para causas evitáveis -------------------------------------------

imunoprevencao <- c("A17", "A19", "A33", "A35", "A36", "A37", "A80", "B05", "B06",
                    "B16", "B260", "G000", "P350", "P353")

mulher_gestacao <- c("A50", sprintf("B2%d", 0:4), "P022", "P023", "P027", "P028",
                     "P029", "P00", "P04", "P01", "P05", "P07", "P220", "P26",
                     "P52", "P550", "P551", sprintf("B5%d", 58:79), "P77")

evitaveis_parto <- c("P020", "P021", "P024", "P025", "P026", "P03", "P08", sprintf("P1%d", 0:5),
                     "P20", "P21", "P24")

recem_nascido <- c("P221", "P228", "P229", "P23", "P25", "P27", "P28",
                   sprintf("P3%d", 51:99), sprintf("P5%d", 0:4), "P58", "P59",
                   sprintf("P7%d", 0:4), "P60", "P61",  sprintf("P7%d", 5:8),
                   sprintf("P8%d", 0:3),  sprintf("P9%d", 0:6),
                   sprintf("P9%d", 61:68))

tratamento <- c("A15", "A16", "A18", sprintf("G0%d", 0:4), sprintf("J0%d", 0:6),
                sprintf("J1%d", 2:8), sprintf("J1%d", 2:8), sprintf("J2%d", 0:2),
                "J384", sprintf("J4%d", 0:2), sprintf("J4%d", 5:7), sprintf("J6%d", 8:9),
                sprintf("A7%d", 0:4), "A30", "A31", "A32", "A38", "A39", "A40", "A41",
                "A46", "A49", "E030", "E031", sprintf("E1%d", 0:4), "E700", "E730",
                "G40", "G41", "Q90", "N390", sprintf("I0%d", 0:9))

saude <- c(sprintf("A0%d", 0:9), sprintf("A2%d", 0:8), sprintf("A9%d", 0:9),
           sprintf("A7%d", 5:9), "A82", sprintf("B5%d", 0:9), sprintf("B6%d", 0:4),
           sprintf("B6%d", 5:9), sprintf("B7%d", 0:9), sprintf("B8%d", 0:3),
           "B99", sprintf("D5%d", 0:3), sprintf("E4%d", 0:9), sprintf("E6%d", 0:9),
           sprintf("E6%d", 0:4), "E86", c(sprintf("V%02d", 1:99)), sprintf("X4%d", 0:4),
           sprintf("X4%d", 5:9), "R95", c(sprintf("W%02d", 0:19)), sprintf("X0%d", 0:9),
           sprintf("X3%d", 0:9), c(sprintf("W%02d", 65:74)), c(sprintf("W%02d", 75:84)),
           c(sprintf("W%02d", 85:99)), c(sprintf("X%02d", 85:99)),
           c(sprintf("Y%02d", 00:09)), c(sprintf("Y%02d", 10:34)), c(sprintf("W%02d", 20:49)),
           c(sprintf("Y%02d", 60:69)), c(sprintf("Y%02d", 83:84)), c(sprintf("Y%02d", 40:59))
)

mal_definidas <- c(c(sprintf("R%02d", 00:94)), c(sprintf("R%02d", 96:99)),
                   "P95", "P969")

# causas evitáveis para óbitos fetais -------------------------------------------------------

df_fetais_evitaveis <- df_sim_dofet |>
  mutate(
    causabas2 = substr(causabas, 1, 3),
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "fetal_evitaveis_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao~ "fetal_evitaveis_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "fetal_evitaveis_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "fetal_evitaveis_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "fetal_evitaveis_tratamento",
      causabas %in% saude | causabas2 %in% saude ~ "fetal_evitaveis_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "fetal_evitaveis_mal_definidas",
      TRUE ~ "fetal_evitaveis_outros"
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
df_fetais_evitaveis[is.na(df_fetais_evitaveis)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_evitaveis)

# causas evitáveis para óbitos neonatais -------------------------------------------------------

df_neonatais_evitaveis <- df_sim_doinf |>
  mutate(
    causabas2 = substr(causabas, 1, 3),
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "neonatal_evitaveis_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao~ "neonatal_evitaveis_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "neonatal_evitaveis_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "neonatal_evitaveis_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "neonatal_evitaveis_tratamento",
      causabas %in% saude | causabas2 %in% saude ~ "neonatal_evitaveis_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "neonatal_evitaveis_mal_definidas",
      TRUE ~ "neonatal_evitaveis_outros"
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
df_neonatais_evitaveis[is.na(df_neonatais_evitaveis)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_evitaveis)


# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco8_graficos, "data-raw/csv/indicadores_bloco8_graficos_2012-2022.csv", row.names = FALSE)

