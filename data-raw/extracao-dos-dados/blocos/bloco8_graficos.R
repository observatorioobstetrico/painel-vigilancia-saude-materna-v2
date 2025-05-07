library(microdatasus)
library(dplyr)
library(janitor)
library(tidyr)
library(readxl)
library(data.table)
library(readr)

# Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres)

# Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(as.character(codigos_municipios), each = length(2020:2024)), ano = 2020:2024)

# Criando o data.frame que irá receber todos os dados dos gráficos do bloco 8
df_bloco8_graficos <- df_aux_municipios


# Garbage codes para óbitos maternos --------------------------------------
## Baixando os dados do SIM-DOMAT de 2012 a 2022
df_sim_domat_aux1 <- fetch_datasus(
  year_start = 2020,
  year_end = 2023,
  information_system = "SIM-DOMAT"
) |>
  clean_names()

df_sim_domat_aux1 <- df_sim_domat_aux1 |> select(-c(estabdescr, nudiasobin,
                                                    nudiasinf, fontesinf))

options(timeout = 600)

sim_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv")

 sim_mat2024 <- sim_2024 |>
   filter(
     SEXO == "2",
     ((CAUSABAS >= "O000"  &  CAUSABAS <= "O959") |
        (CAUSABAS >= "O980"  &  CAUSABAS <= "O999") |
        (CAUSABAS == "A34" & OBITOPUERP != "2") |
        ((CAUSABAS >= "B200"  &  CAUSABAS <= "B249") & (OBITOGRAV == "1" | OBITOPUERP == "1")) |
        (CAUSABAS == "D392" & (OBITOGRAV == "1" | OBITOPUERP == "1")) |
        (CAUSABAS == "E230" & (OBITOGRAV == "1" | OBITOPUERP == "1")) |
        ((CAUSABAS >= "F530"  &  CAUSABAS <= "F539") & (OBITOPUERP != "2")) |
        (CAUSABAS == "M830" & OBITOPUERP != "2"))
   ) |>
   clean_names()

 sim_mat2024 <- sim_mat2024 |> select(-c(opor_do, tp_altera, cb_alt))

 df_sim_domat_aux1 <- df_sim_domat_aux1 |>
   select(-c(#codmuncart,
     #codcart, numregcart, dtregcart,
             #dtrecorig, expdifdata, crm
     ))

 df_sim_domat_aux <- rbind(df_sim_domat_aux1, sim_mat2024)

## Criando a variável de ano e limitando a variável 'causabas' a três caracteres
 df_sim_domat <- df_sim_domat_aux |>
   mutate_if(is.numeric, as.character) |>
   mutate(
     ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito)))
   )

# ## Lendo o dataframe que recebe as CIDs consideradas garbage codes
 df_garbage_codes <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_garbage_codes.csv")

# ## Criando um data.frame com os óbitos maternos totais
 df_maternos_totais <- df_sim_domat |>
   select(codmunres, ano) |>
   mutate(obitos_maternos_totais = 1) |>
  group_by(across(!obitos_maternos_totais)) |>
   summarise(obitos_maternos_totais = sum(obitos_maternos_totais)) |>
   ungroup() |>
   right_join(df_aux_municipios) |>
   arrange(codmunres)

# ## Substituindo todos os NAs por 0 (gerados após o right join)
 df_maternos_totais[is.na(df_maternos_totais)] <- 0

# ## Criando um data.frame com os óbitos maternos preenchidos com garbage codes
 df_maternos_garbage <- df_sim_domat |>
   filter(causabas %in% df_garbage_codes$causabas) |>
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

 #Total de garbage codes
 df_maternos_garbage_frequencia <- df_sim_domat |>
   filter(causabas %in% df_garbage_codes$causabas) |>
   select(codmunres, ano, causabas) |>
   mutate(obitos = 1) |>
   group_by(ano, codmunres) |>
   summarise(total_garbage_codes_maternos = sum(obitos))

 df_maternos_garbage <- left_join(df_maternos_garbage, df_maternos_garbage_frequencia)

# ## Substituindo todos os NAs por 0 (gerados após o right join)
 df_maternos_garbage[is.na(df_maternos_garbage)] <- 0

# ## Juntando com o restante da base do bloco 8
 df_bloco8_graficos <- left_join(df_bloco8_graficos, df_maternos_garbage)

## Para a tabela de garbage codes em óbitos maternos
 df_cid10 <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_cid10_completo.csv")

#  df_maternos_garbage_tabela_aux <- df_sim_domat |>
#    filter(causabas %in% df_garbage_codes$causabas) |>
#    select(codmunres, ano, causabas) |>
#    mutate(obitos = 1) |>
#    group_by(across(!obitos)) |>
#    summarise(obitos = sum(obitos)) |>
#    ungroup() |>
#    left_join(df_cid10 |> select(causabas, causabas_subcategoria)) |>
#    select(!causabas) |>
#    pivot_wider(
#      names_from = causabas_subcategoria,
#      values_from = obitos,
#      values_fill = 0
#    )
#
#  df_maternos_garbage_tabela <- df_maternos_garbage_tabela_aux |>
#    right_join(df_aux_municipios |> filter(codmunres %in% df_maternos_garbage_tabela_aux$codmunres)) |>
#    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) |>
#    pivot_longer(
#      cols = !c(codmunres, ano),
#      names_to = "causabas_subcategoria",
#      values_to = "obitos"
#    ) |>
#    left_join(df_cid10 |> select(capitulo_cid10, grupo_cid10, causabas_subcategoria)) |>
#    select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
#    arrange(codmunres, ano)
#
#  get_dupes(df_maternos_garbage_tabela)
#
#  df_maternos_garbage_tabela_antigo <-read_csv("data-raw/csv/garbage_materno_2012_2022.csv.gz") |>
#    filter(ano <= 2021)
#
#  df_materno_garbage_tabela_novo <- rbind(df_maternos_garbage_tabela_antigo, df_maternos_garbage_tabela)
#
#  write.csv(df_maternos_garbage_tabela_novo, gzfile("data-raw/csv/garbage_materno_2022_2024.csv.gz"), row.names = FALSE)
#
# # Removendo objetos já utilizados
# rm(df_maternos_garbage, df_maternos_totais, df_maternos_garbage_tabela_aux, df_maternos_garbage_tabela)


# Garbage codes para óbitos fetais ----------------------------------------
## Baixando os dados do SIM-DOFET de 2012 a 2022
df_sim_dofet_aux1 <- fetch_datasus(
  year_start = 2020,
  year_end = 2023,
  information_system = "SIM-DOFET"
) |>
  clean_names()

df_sim_dofet_aux1 <- df_sim_dofet_aux1 |>
  select(-c(codmuncart, numregcart, codcart, dtregcart, estabdescr, medico, linhaa_o, linhab_o,
            linhac_o, linhad_o, linhaii_o, dtrecorig, nudiasobin, nudiasinf, fontesinf #, crm
            ))

sim_dofet2024 <- sim_2024 |> filter(TIPOBITO == 1) |>
  clean_names() |>
  select(-c(opor_do, tp_altera, cb_alt))


df_sim_dofet_aux <- rbind(df_sim_dofet_aux1, sim_dofet2024)

## Criando a variável de ano, limitando a variável 'causabas' a três caracteres e filtrando apenas pelos óbitos fetais que consideramos
df_sim_dofet <- df_sim_dofet_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito)))
  ) |>
  filter(
    ((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 22 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 500)
  )

df_sim_dofet_antes <- df_sim_dofet_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    obitoparto = as.numeric(obitoparto)
  ) |>
  filter(
    (((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 22 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 500)) & obitoparto == 1
  )

df_sim_dofet_durante <- df_sim_dofet_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    obitoparto = as.numeric(obitoparto)
  ) |>
  filter(
    (((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 22 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 500)) & obitoparto == 2
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

## Criando um data.frame com os óbitos fetais totais antes do parto
df_fetais_totais_antes <- df_sim_dofet_antes |>
  select(codmunres, ano) |>
  mutate(obitos_fetais_totais_antes = 1) |>
  group_by(across(!obitos_fetais_totais_antes)) |>
  summarise(obitos_fetais_totais_antes = sum(obitos_fetais_totais_antes)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_fetais_totais_antes[is.na(df_fetais_totais_antes)] <- 0

## Criando um data.frame com os óbitos fetais totais durante o parto
df_fetais_totais_durante <- df_sim_dofet |>
  select(codmunres, ano) |>
  mutate(obitos_fetais_totais_durante = 1) |>
  group_by(across(!obitos_fetais_totais_durante)) |>
  summarise(obitos_fetais_totais_durante = sum(obitos_fetais_totais_durante)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_fetais_totais_durante[is.na(df_fetais_totais_durante)] <- 0

## Criando um data.frame com os óbitos fetais preenchidos com garbage codes
df_fetais_garbage <- df_sim_dofet |>
   filter(causabas %in% df_garbage_codes$causabas) |>
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
   rename_with(.fn = ~ paste0('garbage_fetal_', .), .cols = -c(ano, codmunres)) |>
   right_join(df_aux_municipios) |>
   right_join(df_fetais_totais) |>
   arrange(codmunres)

# Agrupando a categoria "outros"

df_fetais_garbage_filtro <- df_fetais_garbage |>
  select(-c(ano, codmunres, obitos_fetais_totais)) |>
  summarise(across(everything(), sum, na.rm = TRUE)) |>
  mutate(
    garbage_5_porc = rowSums(across(everything()), na.rm = TRUE)*0.05
  )

df_fetais_garbage_filtro <- df_fetais_garbage_filtro|>
  select(where(~ . > df_fetais_garbage_filtro$garbage_5_porc))

colunas_para_agrupar <- setdiff(names(df_fetais_garbage), c("ano", "codmunres", "obitos_fetais_totais", names(df_fetais_garbage_filtro)))

colunas_para_manter <- c("ano", "codmunres", "obitos_fetais_totais", names(df_fetais_garbage_filtro))

df_fetais_garbage <- df_fetais_garbage |>
  mutate(garbage_fetal_outros = rowSums(select(df_fetais_garbage, all_of(colunas_para_agrupar)), na.rm = TRUE)) |>
  select(ano, codmunres, all_of(colunas_para_manter), garbage_fetal_outros)

#Total de garbage codes
df_fetais_garbage_frequencia <- df_sim_dofet |>
  filter(causabas %in% df_garbage_codes$causabas) |>
  select(codmunres, ano, causabas) |>
  mutate(obitos = 1) |>
  group_by(ano, codmunres) |>
  summarise(total_garbage_codes_fetais = sum(obitos))

df_fetais_garbage <- left_join(df_fetais_garbage, df_fetais_garbage_frequencia)

# ## Substituindo todos os NAs por 0 (gerados após o right join)
 df_fetais_garbage[is.na(df_fetais_garbage)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_garbage)

## Removendo objetos já utilizados
rm(df_fetais_garbage)

## Para a tabela de garbage codes em óbitos fetais
#  df_garbage_fetais_tabela_aux <- df_sim_dofet |>
#    filter(causabas %in% df_garbage_codes$causabas) |>
#    select(codmunres, ano, causabas) |>
#    mutate(obitos = 1) |>
#    group_by(across(!obitos)) |>
#    summarise(obitos = sum(obitos)) |>
#    ungroup() |>
#    left_join(df_cid10 |> select(causabas, causabas_subcategoria)) |>
#    select(!causabas) |>
#    pivot_wider(
#      names_from = causabas_subcategoria,
#      values_from = obitos,
#      values_fill = 0
#    )
#
#  df_garbage_fetais_tabela <- df_garbage_fetais_tabela_aux |>
#    right_join(df_aux_municipios |> filter(codmunres %in% df_garbage_fetais_tabela_aux$codmunres)) |>
#    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) |>
#    pivot_longer(
#      cols = !c(codmunres, ano),
#      names_to = "causabas_subcategoria",
#      values_to = "obitos"
#    ) |>
#    left_join(df_cid10 |> select(capitulo_cid10, grupo_cid10, causabas_subcategoria)) |>
#    select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
#    arrange(codmunres, ano)
#
#  get_dupes(df_garbage_fetais_tabela)
#
# write.csv(df_garbage_fetais_tabela, gzfile("data-raw/csv/garbage_fetal_2022_2023.csv.gz"), row.names = FALSE)
#
# ## Removendo objetos já utilizados
# rm(df_garbage_fetais_tabela_aux, df_garbage_fetais_tabela)



# Garbage codes para óbitos neonatais -------------------------------------
df_sim_doinf_aux1 <- fetch_datasus(
  year_start = 2020,
  year_end = 2023,
  information_system = "SIM-DOINF",
) |>
  clean_names()

df_sim_doinf_aux1 <- df_sim_doinf_aux1|>
  select(-c(estabdescr, nudiasobin, nudiasinf, fontesinf#, crm,
            #codmuncart, codcart,
            #numregcart, dtregcart, dtrecorig, expdifdata
            ))

sim_2024 <- sim_2024 |>
  clean_names() |>
  select(-c(opor_do, tp_altera, cb_alt))


df_sim_doinf_aux <- rbind(df_sim_doinf_aux1, sim_2024)


## Criando a variável de ano, limitando a variável 'causabas' a três caracteres e filtrando apenas os óbitos neonatais
df_sim_doinf <- df_sim_doinf_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
  ) |>
  filter(
    idade < 228
  )

df_sim_doinf_0_dias <- df_sim_doinf_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
  ) |>
  filter(
    idade <= 200
  )

df_sim_doinf_1_6_dias <- df_sim_doinf_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
  ) |>
  filter(
    idade > 200 & idade < 207
  )

df_sim_doinf_7_27_dias <- df_sim_doinf_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
  ) |>
  filter(
    idade > 206 & idade < 228
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

## Criando um data.frame com os óbitos neonatais precoces totais
df_neonatais_totais_precoce <- df_sim_doinf |>
  filter(idade < 207 | as.numeric(peso) < 1000) |>
  select(codmunres, ano) |>
  mutate(obitos_neonatais_precoce_totais = 1) |>
  group_by(across(!obitos_neonatais_precoce_totais)) |>
  summarise(obitos_neonatais_precoce_totais = sum(obitos_neonatais_precoce_totais)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_totais_precoce[is.na(df_neonatais_totais_precoce)] <- 0

## Criando um data.frame com os óbitos neonatais totais com 0 dias de vida
df_neonatais_totais_0_dias <- df_sim_doinf |>
  select(codmunres, ano) |>
  mutate(obitos_neonatais_totais_0_dias = 1) |>
  group_by(across(!obitos_neonatais_totais_0_dias)) |>
  summarise(obitos_neonatais_totais_0_dias = sum(obitos_neonatais_totais_0_dias)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_totais_0_dias[is.na(df_neonatais_totais_0_dias)] <- 0

## Criando um data.frame com os óbitos neonatais totais com 0 dias de vida
df_neonatais_totais_1_6_dias <- df_sim_doinf |>
  select(codmunres, ano)|>
  mutate(obitos_neonatais_totais_1_6_dias = 1) |>
  group_by(across(!obitos_neonatais_totais_1_6_dias)) |>
  summarise(obitos_neonatais_totais_1_6_dias = sum(obitos_neonatais_totais_1_6_dias)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_totais_1_6_dias[is.na(df_neonatais_totais_1_6_dias)] <- 0

## Criando um data.frame com os óbitos neonatais totais com 0 dias de vida
df_neonatais_totais_7_27_dias <- df_sim_doinf |>
  select(codmunres, ano)|>
  mutate(obitos_neonatais_totais_7_27_dias = 1) |>
  group_by(across(!obitos_neonatais_totais_7_27_dias)) |>
  summarise(obitos_neonatais_totais_7_27_dias = sum(obitos_neonatais_totais_7_27_dias)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_totais_7_27_dias[is.na(df_neonatais_totais_7_27_dias)] <- 0


## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_totais_precoce)

## Criando um data.frame com os óbitos neonatais preenchidos com garbage codes
  # df_sim_doinf_garbage <- df_sim_doinf |>
  #  filter(causabas %in% df_garbage_codes$causabas) |>
  #  select(codmunres, ano, causabas) |>
  #  left_join(df_cid10 |> select(causabas, capitulo_cid10)) |>
  #  pull(capitulo_cid10) |>
  #  unique()

# df_fetais_garbage <- df_sim_dofet |>
#   filter(causabas %in% df_garbage_codes$causabas) |>
#   select(codmunres, ano, causabas) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = causabas,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   clean_names() |>
#   rename_with(.fn = ~ paste0('garbage_fetal_', .), .cols = -c(ano, codmunres)) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais) |>
#   arrange(codmunres)

 df_neonatais_garbage <- df_sim_doinf |>
   filter(causabas %in% df_garbage_codes$causabas) |>
   select(codmunres, ano, causabas) |>
   #left_join(df_cid10 |> select(causabas, capitulo_cid10)) |>
   mutate(obitos = 1) |>
   group_by(across(!c(obitos))) |>
   summarise(obitos = sum(obitos)) |>
   ungroup() |>
   pivot_wider(
     names_from = causabas,
     values_from = obitos,
     values_fill = 0
   ) |>
   clean_names() |>
   rename_with(.fn = ~ paste0('garbage_neonatal_', .), .cols = -c(ano, codmunres)) |>
   right_join(df_aux_municipios) |>
   right_join(df_neonatais_totais) |>
   arrange(codmunres)

 # Agrupando a categoria "outros"

 df_neonatais_garbage_filtro <- df_neonatais_garbage |>
   select(-c(ano, codmunres, obitos_neonatais_totais)) |>
   summarise(across(everything(), sum, na.rm = TRUE)) |>
   mutate(
     garbage_5_porc = rowSums(across(everything()), na.rm = TRUE)*0.05
   )

 df_neonatais_garbage_filtro <- df_neonatais_garbage_filtro|>
   select(where(~ . > df_neonatais_garbage_filtro$garbage_5_porc))

 colunas_para_agrupar <- setdiff(names(df_neonatais_garbage), c("ano", "codmunres", "obitos_neonatais_totais", names(df_neonatais_garbage_filtro)))

 colunas_para_manter <- c("ano", "codmunres", "obitos_neonatais_totais", names(df_neonatais_garbage_filtro))

 df_neonatais_garbage <- df_neonatais_garbage |>
   mutate(garbage_neonatal_outros = rowSums(select(df_neonatais_garbage, all_of(colunas_para_agrupar)), na.rm = TRUE)) |>
   select(ano, codmunres, all_of(colunas_para_manter), garbage_neonatal_outros)

 #Total de garbage codes
 df_neonatais_garbage_frequencia <- df_sim_doinf |>
   filter(causabas %in% df_garbage_codes$causabas) |>
   select(codmunres, ano, causabas) |>
   mutate(obitos = 1) |>
   group_by(ano, codmunres) |>
   summarise(total_garbage_codes_neonatais = sum(obitos))

 df_neonatais_garbage <- left_join(df_neonatais_garbage, df_neonatais_garbage_frequencia)

# ## Substituindo todos os NAs por 0 (gerados após o right join)
 df_neonatais_garbage[is.na(df_neonatais_garbage)] <- 0

# ## Juntando com o restante da base do bloco 8
 df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_garbage)

# ## Removendo objetos já utilizados
 rm(df_neonatais_garbage)



 # Garbage codes para óbitos perinatais

 ## Criando a variável de ano, limitando a variável 'causabas' a três caracteres e filtrando apenas pelos óbitos fetais que consideramos
 df_sim_perinatal_fetal <- df_sim_dofet_aux |>
   mutate(
     ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito)))
   ) |>
   filter(
     ((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 22 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 500)
   )

 df_sim_perinatal_antes <- df_sim_dofet_aux |>
   mutate(
     ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
     obitoparto = as.numeric(obitoparto)
   ) |>
   filter(
     (((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 22 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 500)) & obitoparto == 1
   )

 df_sim_perinatal_durante <- df_sim_dofet_aux |>
   mutate(
     ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
     obitoparto = as.numeric(obitoparto)
   ) |>
   filter(
     (((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 22 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 500)) & obitoparto == 2
   )

 df_sim_perinatal <- rbind(df_sim_perinatal_fetal, df_sim_doinf_0_dias) |>
   rbind(df_sim_doinf_1_6_dias)


 ## Criando um data.frame com os óbitos perinatais totais
 df_perinatais_totais <- df_sim_perinatal |>
   select(codmunres, ano) |>
   mutate(obitos_perinatais_totais = 1) |>
   group_by(across(!obitos_perinatais_totais)) |>
   summarise(obitos_perinatais_totais = sum(obitos_perinatais_totais)) |>
   ungroup() |>
   right_join(df_aux_municipios) |>
   arrange(codmunres)

 ## Substituindo todos os NAs por 0 (gerados após o right join)
 df_perinatais_totais[is.na(df_perinatais_totais)] <- 0

 ## Criando um data.frame com os óbitos perinatais totais antes do parto
 df_perinatais_totais_antes <- df_sim_perinatal_antes |>
   select(codmunres, ano) |>
   mutate(obitos_perinatais_totais_antes = 1) |>
   group_by(across(!obitos_perinatais_totais_antes)) |>
   summarise(obitos_perinatais_totais_antes = sum(obitos_perinatais_totais_antes)) |>
   ungroup() |>
   right_join(df_aux_municipios) |>
   arrange(codmunres)

 ## Substituindo todos os NAs por 0 (gerados após o right join)
 df_perinatais_totais_antes[is.na(df_perinatais_totais_antes)] <- 0

 ## Criando um data.frame com os óbitos perinatais totais durante o parto
 df_perinatais_totais_durante <- df_sim_perinatal_durante |>
   select(codmunres, ano) |>
   mutate(obitos_perinatais_totais_durante = 1) |>
   group_by(across(!obitos_perinatais_totais_durante)) |>
   summarise(obitos_perinatais_totais_durante = sum(obitos_perinatais_totais_durante)) |>
   ungroup() |>
   right_join(df_aux_municipios) |>
   arrange(codmunres)

 ## Substituindo todos os NAs por 0 (gerados após o right join)
 df_perinatais_totais_durante[is.na(df_perinatais_totais_durante)] <- 0

 ## Criando um data.frame com os óbitos perinatais preenchidos com garbage codes
 df_perinatais_garbage <- df_sim_perinatal |>
   filter(causabas %in% df_garbage_codes$causabas) |>
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
   rename_with(.fn = ~ paste0('garbage_perinatal_', .), .cols = -c(ano, codmunres)) |>
   right_join(df_aux_municipios) |>
   right_join(df_perinatais_totais) |>
   arrange(codmunres)

 # Agrupando a categoria "outros"

 df_perinatais_garbage_filtro <- df_perinatais_garbage |>
   select(-c(ano, codmunres, obitos_perinatais_totais)) |>
   summarise(across(everything(), sum, na.rm = TRUE)) |>
   mutate(
     garbage_5_porc = rowSums(across(everything()), na.rm = TRUE)*0.05
   )

 df_perinatais_garbage_filtro <- df_perinatais_garbage_filtro|>
   select(where(~ . > df_perinatais_garbage_filtro$garbage_5_porc))

 colunas_para_agrupar <- setdiff(names(df_perinatais_garbage), c("ano", "codmunres", "obitos_perinatais_totais", names(df_perinatais_garbage_filtro)))

 colunas_para_manter <- c("ano", "codmunres", "obitos_perinatais_totais", names(df_perinatais_garbage_filtro))

 df_perinatais_garbage <- df_perinatais_garbage |>
   mutate(garbage_perinatal_outros = rowSums(select(df_perinatais_garbage, all_of(colunas_para_agrupar)), na.rm = TRUE)) |>
   select(ano, codmunres, all_of(colunas_para_manter), garbage_perinatal_outros)

 #Total de garbage codes
 df_perinatais_garbage_frequencia <- df_sim_perinatal |>
   filter(causabas %in% df_garbage_codes$causabas) |>
   select(codmunres, ano, causabas) |>
   mutate(obitos = 1) |>
   group_by(ano, codmunres) |>
   summarise(total_garbage_codes_perinatais = sum(obitos))

 df_perinatais_garbage <- left_join(df_perinatais_garbage, df_perinatais_garbage_frequencia)

 # ## Substituindo todos os NAs por 0 (gerados após o right join)
 df_perinatais_garbage[is.na(df_perinatais_garbage)] <- 0

 ## Juntando com o restante da base do bloco 8
 df_bloco8_graficos <- left_join(df_bloco8_graficos, df_perinatais_garbage)

 ## Removendo objetos já utilizados
 rm(df_perinatais_garbage)


 # Salvando a base de dados completa na pasta data-raw/csv -----------------

 df_bloco8_graficos_2023 <- df_bloco8_graficos

 df_bloco8_graficos_antigo <- read_csv("data-raw/csv/indicadores_bloco8_graficos_garbage_code_2012-2024.csv") |>
   filter(ano <= 2019) #|>
   #select(all_of(names(df_bloco8_graficos_2023)))


 df_bloco8_graficos_2023$codmunres <- as.numeric(df_bloco8_graficos_2023$codmunres)
 df_bloco8_graficos_total <- full_join(df_bloco8_graficos_antigo, df_bloco8_graficos_2023)
 df_bloco8_graficos_total[is.na(df_bloco8_graficos_total)] <- 0

 write.csv(df_bloco8_graficos_total, "data-raw/csv/indicadores_bloco8_graficos_garbage_code_2012-2024.csv", row.names = FALSE)






 ################### CÓDIGOS NÃO MAIS USADOS

## Para a tabela de garbage codes em óbitos neonatais
#  df_garbage_neonatais_tabela_aux <- df_sim_doinf |>
#    filter(causabas %in% df_garbage_codes$causabas) |>
#    mutate(
#      idade = as.numeric(idade),
#      peso = as.numeric(peso),
#      faixa_de_peso = #factor(
#        case_when(
#          is.na(peso) ~ "Sem informação",
#          peso < 1500 ~ "< 1500 g",
#          peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
#          peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
#          peso >= 2500 ~ "\U2265 2500 g"
#        ),
#        levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
#      ),
#      faixa_de_idade = factor(
#        case_when(
#          idade <= 206 ~ "0 a 6 dias",
#          idade > 206 & idade <= 227 ~ "7 a 27 dias"
#        ),
#  levels = c("0 a 6 dias", "7 a 27 dias")
# ),
#      obitos = 1
#    ) |>
#    select(codmunres, ano, causabas, faixa_de_peso, faixa_de_idade, obitos) |>
#    group_by(across(!obitos)) |>
#    summarise(obitos = sum(obitos)) |>
#      ungroup() |>
#   filter(!is.na(causabas)) |>
#    left_join(df_cid10 |> select(causabas, causabas_subcategoria)) |>
#    select(!causabas) |>
#    pivot_wider(
#      names_from = causabas_subcategoria,
#      values_from = obitos,
#      values_fill = 0
#    )

# df_garbage_neonatais_tabela <- df_garbage_neonatais_tabela_aux |>
#   right_join(df_aux_municipios |> filter(codmunres %in% df_garbage_neonatais_tabela_aux$codmunres)) |>
#   mutate(
#     faixa_de_peso = ifelse(is.na(faixa_de_peso), "apagar", faixa_de_peso),
#     faixa_de_idade = ifelse(is.na(faixa_de_idade), "apagar", faixa_de_idade)
#   ) |>
#   mutate(across(everything(), ~ifelse(is.na(.), 0, .))) |>
#   pivot_longer(
#     cols = !c(codmunres, ano, faixa_de_idade, faixa_de_peso),
#     names_to = "causabas_subcategoria",
#     values_to = "obitos"
#   ) |>
#   arrange(codmunres, ano)
#   left_join(df_cid10 |> select(capitulo_cid10, grupo_cid10, causabas_subcategoria)) |>
#   select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
#   arrange(codmunres, ano)
#
# write.csv(df_garbage_neonatais_tabela, gzfile("data-raw/csv/garbage_neonatal_2012_2022.csv.gz"), row.names = FALSE)

## Removendo objetos já utilizados
#rm(df_garbage_neonatais_tabela_aux, df_garbage_neonatais_tabela)

# df_garbage_neonatais_tabela <- df_sim_doinf |>
#   filter(causabas %in% df_garbage_codes$causabas, codmunres %in% codigos_municipios) |>
#   left_join(df_cid10) |>
#   mutate(
#     idade = as.numeric(idade),
#     peso = as.numeric(peso),
#     faixa_de_peso = factor(
#       case_when(
#         is.na(peso) ~ "Sem informação",
#         peso < 1500 ~ "< 1500 g",
#         peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
#         peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
#         peso >= 2500 ~ "\U2265 2500 g"
#       ),
#       levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
#     ),
#     faixa_de_idade = factor(
#       case_when(
#         idade <= 206 ~ "0 a 6 dias",
#         idade > 206 & idade <= 227 ~ "7 a 27 dias"
#       ),
#       levels = c("0 a 6 dias", "7 a 27 dias")
#     ),
#     obitos = 1
#   ) |>
#   select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   arrange(codmunres, ano)
#
# get_dupes(df_garbage_neonatais_tabela)
#
# write.csv(df_garbage_neonatais_tabela, gzfile("data-raw/csv/garbage_neonatal_2022_2023.csv.gz"), row.names = FALSE)

## Removendo objetos já utilizados
#rm(df_garbage_neonatais_tabela)


# Causas principais para óbitos fetais -------------------------------------------------------
# df_principais_fetal <- df_sim_dofet |>
#   mutate(
#     causabas = substr(causabas, 1, 3),
#     grupo_cid = case_when(
#       causabas >= "P00" & causabas <= "P04" ~ "principais_fetal_p00_p04",
#       causabas >= "P05" & causabas <= "P08" ~ "principais_fetal_p05_p08",
#       causabas >= "P10" & causabas <= "P15" ~ "principais_fetal_p10_p15",
#       causabas >= "P20" & causabas <= "P29" ~ "principais_fetal_p20_p29",
#       causabas >= "P35" & causabas <= "P39" ~ "principais_fetal_p35_p39",
#       causabas >= "P50" & causabas <= "P61" ~ "principais_fetal_p50_p61",
#       causabas >= "P70" & causabas <= "P74" ~ "principais_fetal_p70_p74",
#       causabas >= "P75" & causabas <= "P78" ~ "principais_fetal_p75_p78",
#       causabas >= "P80" & causabas <= "P83" ~ "principais_fetal_p80_p83",
#       causabas >= "P90" & causabas <= "P96" ~ "principais_fetal_p90_p96",
#       causabas >= "Q00" & causabas <= "Q99" ~ "principais_fetal_q00_q99",
#       #causabas >= "J00" & causabas <= "J99" ~ "principais_fetal_j00_j99",
#       causabas >= "A00" & causabas <= "A99" |
#         causabas >= "B00" & causabas <= "B99" ~ "principais_fetal_a00_b99"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "principais_fetal_outros", grupo_cid)
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_principais_fetal[is.na(df_principais_fetal)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_principais_fetal)
#
# ## Removendo objetos já utilizados
# rm(df_principais_fetal)

## Para a tabela de causas principais em óbitos fetais
# df_principais_fetais_tabela_aux <- df_sim_dofet |>
#   select(codmunres, ano, causabas) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   left_join(df_cid10 |> select(causabas, causabas_subcategoria)) |>
#   select(!causabas) |>
#   pivot_wider(
#     names_from = causabas_subcategoria,
#     values_from = obitos,
#     values_fill = 0
#   )
#
# df_principais_fetais_tabela <- df_principais_fetais_tabela_aux |>
#   right_join(df_aux_municipios |> filter(codmunres %in% df_principais_fetais_tabela_aux$codmunres)) |>
#   mutate(across(everything(), ~ifelse(is.na(.), 0, .))) |>
#   pivot_longer(
#     cols = !c(codmunres, ano),
#     names_to = "causabas_subcategoria",
#     values_to = "obitos"
#   ) |>
#   left_join(df_cid10) |>
#   mutate(
#     causabas = substr(causabas, 1, 3),
#     grupo_cid10 = case_when(
#       causabas >= "Q00" & causabas <= "Q99" ~ "(Q00-Q99) Anomalias congênitas",
#       causabas >= "J00" & causabas <= "J99" ~ "(J00-J99) Respiratórias",
#       causabas >= "A00" & causabas <= "A99" |
#         causabas >= "B00" & causabas <= "B99" ~ "(A00-B99) Infecciosas",
#       TRUE ~ grupo_cid10
#     )
#   ) |>
#   select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
#   arrange(codmunres, ano)

# df_principais_fetais_tabela <- df_sim_dofet |>
#   filter(codmunres %in% codigos_municipios) |>
#   left_join(df_cid10) |>
#   mutate(
#     causabas = substr(causabas, 1, 3),
#     grupo_cid10 = case_when(
#       causabas >= "Q00" & causabas <= "Q99" ~ "(Q00-Q99) Anomalias congênitas",
#       #causabas >= "J00" & causabas <= "J99" ~ "(J00-J99) Respiratórias",
#       causabas >= "A00" & causabas <= "A99" |
#         causabas >= "B00" & causabas <= "B99" ~ "(A00-B99) Infecciosas",
#       causabas >= "P00" & causabas <= "P96" ~ grupo_cid10,
#       TRUE ~ "Outros"
#     ),
#     obitos = 1
#   ) |>
#   select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   arrange(codmunres, ano)
#
# get_dupes(df_principais_fetais_tabela)
#
# write.csv(df_principais_fetais_tabela, gzfile("data-raw/csv/principais_fetal_2022_2024.csv.gz"), row.names = FALSE)

## Removendo objetos já utilizados
#rm(df_principais_fetais_tabela)


# Causas principais para óbitos neonatais ---------------------------------
# df_principais_neonatal <- df_sim_doinf |>
#   mutate(
#     causabas = substr(causabas, 1, 3),
#     grupo_cid = case_when(
#       causabas >= "P00" & causabas <= "P04" ~ "principais_neonatal_p00_p04",
#       causabas >= "P05" & causabas <= "P08" ~ "principais_neonatal_p05_p08",
#       causabas >= "P10" & causabas <= "P15" ~ "principais_neonatal_p10_p15",
#       causabas >= "P20" & causabas <= "P29" ~ "principais_neonatal_p20_p29",
#       causabas >= "P35" & causabas <= "P39" ~ "principais_neonatal_p35_p39",
#       causabas >= "P50" & causabas <= "P61" ~ "principais_neonatal_p50_p61",
#       causabas >= "P70" & causabas <= "P74" ~ "principais_neonatal_p70_p74",
#       causabas >= "P75" & causabas <= "P78" ~ "principais_neonatal_p75_p78",
#       causabas >= "P80" & causabas <= "P83" ~ "principais_neonatal_p80_p83",
#       causabas >= "P90" & causabas <= "P96" ~ "principais_neonatal_p90_p96",
#       causabas >= "Q00" & causabas <= "Q99" ~ "principais_neonatal_q00_q99",
#       #causabas >= "J00" & causabas <= "J99" ~ "principais_neonatal_j00_j99",
#       causabas >= "A00" & causabas <= "A99" |
#         causabas >= "B00" & causabas <= "B99" ~ "principais_neonatal_a00_b99"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "principais_neonatal_outros", grupo_cid)
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_neonatais_totais) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_principais_neonatal[is.na(df_principais_neonatal)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_principais_neonatal)
#
# # Causas principais para óbitos neonatais precoces ---------------------------------
# df_principais_neonatal_precoce <- df_sim_doinf |>
#   mutate(
#     causabas = substr(causabas, 1, 3),
#     grupo_cid = case_when(
#       causabas >= "P00" & causabas <= "P04" ~ "principais_neonatal_precoce_p00_p04",
#       causabas >= "P05" & causabas <= "P08" ~ "principais_neonatal_precoce_p05_p08",
#       causabas >= "P10" & causabas <= "P15" ~ "principais_neonatal_precoce_p10_p15",
#       causabas >= "P20" & causabas <= "P29" ~ "principais_neonatal_precoce_p20_p29",
#       causabas >= "P35" & causabas <= "P39" ~ "principais_neonatal_precoce_p35_p39",
#       causabas >= "P50" & causabas <= "P61" ~ "principais_neonatal_precoce_p50_p61",
#       causabas >= "P70" & causabas <= "P74" ~ "principais_neonatal_precoce_p70_p74",
#       causabas >= "P75" & causabas <= "P78" ~ "principais_neonatal_precoce_p75_p78",
#       causabas >= "P80" & causabas <= "P83" ~ "principais_neonatal_precoce_p80_p83",
#       causabas >= "P90" & causabas <= "P96" ~ "principais_neonatal_precoce_p90_p96",
#       causabas >= "Q00" & causabas <= "Q99" ~ "principais_neonatal_precoce_q00_q99",
#       #causabas >= "J00" & causabas <= "J99" ~ "principais_neonatal_precoce_j00_j99",
#       causabas >= "A00" & causabas <= "A99" |
#         causabas >= "B00" & causabas <= "B99" ~ "principais_neonatal_precoce_a00_b99"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "principais_neonatal_precoce_outros", grupo_cid)
#   ) |>
#   filter(idade < 207 | as.numeric(peso) < 1000) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_neonatais_totais_precoce) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_principais_neonatal_precoce[is.na(df_principais_neonatal_precoce)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_principais_neonatal_precoce)
#
#
# ## Para a tabela de causas principais em óbitos neonatais
# df_principais_neonatais_tabela <- df_sim_doinf |>
#   filter(codmunres %in% codigos_municipios) |>
#   left_join(df_cid10) |>
#   mutate(
#     idade = as.numeric(idade),
#     peso = as.numeric(peso),
#     faixa_de_peso = factor(
#       case_when(
#         is.na(peso) ~ "Sem informação",
#         peso < 1500 ~ "< 1500 g",
#         peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
#         peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
#         peso >= 2500 ~ "\U2265 2500 g"
#       ),
#       levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
#     ),
#     faixa_de_idade = factor(
#       case_when(
#         idade <= 206 ~ "0 a 6 dias",
#         idade > 206 & idade <= 227 ~ "7 a 27 dias"
#       ),
#       levels = c("0 a 6 dias", "7 a 27 dias")
#     ),
#     causabas = substr(causabas, 1, 3),
#     grupo_cid10 = case_when(
#       causabas >= "Q00" & causabas <= "Q99" ~ "(Q00-Q99) Anomalias congênitas",
#       #causabas >= "J00" & causabas <= "J99" ~ "(J00-J99) Respiratórias",
#       causabas >= "A00" & causabas <= "A99" |
#         causabas >= "B00" & causabas <= "B99" ~ "(A00-B99) Infecciosas",
#       causabas >= "P00" & causabas <= "P96" ~ grupo_cid10,
#       TRUE ~ "Outros"
#     ),
#     obitos = 1
#   ) |>
#   select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   arrange(codmunres, ano)
#
# get_dupes(df_principais_neonatais_tabela)
#
# write.csv(df_principais_neonatais_tabela, gzfile("data-raw/csv/principais_neonatal_2022_2024.csv.gz"), row.names = FALSE)
#
# ## Removendo objetos já utilizados
# rm(df_principais_neonatais_tabela)


# Causas evitáveis --------------------------------------------------------
# ## Criando um vetor com as cids
# imunoprevencao <- c(
#     "A17", "A19", "A33", "A35", "A36", "A37", "A80", "B05", "B06",
#     "B16", "B260", "G000", "P350", "P353"
#   )
#
# mulher_gestacao <- c(
#     "A50", sprintf("B2%d", 0:4), "P022", "P023", "P027", "P028",
#     "P029", "P00", "P04", "P01", "P05", "P07", "P220", "P26",
#     "P52", "P550", "P551", "P558", "P559", "P56", "P57", "P77"
#   )
#
# evitaveis_parto <- c(
#     "P020", "P021", "P024", "P025", "P026", "P03", "P08", sprintf("P1%d", 0:5),
#     "P20", "P21", "P24"
#   )
#
# recem_nascido <- c(
#     "P221", "P228", "P229", "P23", "P25", "P27", "P28",
#     sprintf("P3%d", 51:53), sprintf("P3%d", 58:59), sprintf("P3%d", 6:9), sprintf("P5%d", 0:1), sprintf("P5%d", 3:4), "P58", "P59",
#     sprintf("P7%d", 0:4), "P60", "P61",  sprintf("P7%d", 5:6), "P78",
#     sprintf("P8%d", 0:3),  sprintf("P9%d", 0:4),
#     sprintf("P9%d", 60:68)
#   )
#
# tratamento <- c(
#     "A15", "A16", "A18", sprintf("G0%d", 0:4), sprintf("J0%d", 0:6),
#     sprintf("J1%d", 2:8), sprintf("J1%d", 2:8), sprintf("J2%d", 0:2),
#     "J384", sprintf("J4%d", 0:2), sprintf("J4%d", 5:7), sprintf("J6%d", 8:9),
#     sprintf("A7%d", 0:4), "A30", "A31", "A32", "A38", "A39", "A40", "A41",
#     "A46", "A49", "E030", "E031", sprintf("E1%d", 0:4), "E700", "E730",
#     "G40", "G41", "Q90", "N390", sprintf("I0%d", 0:9)
#   )
#
# saude <- c(
#     sprintf("A0%d", 0:9), sprintf("A2%d", 0:8), sprintf("A9%d", 0:9),
#     sprintf("A7%d", 5:9), "A82", sprintf("B5%d", 0:9), sprintf("B6%d", 0:4),
#     sprintf("B6%d", 5:9), sprintf("B7%d", 0:9), sprintf("B8%d", 0:3),
#     "B99", sprintf("D5%d", 0:3), sprintf("E4%d", 0:9), sprintf("E5%d", 0:9),
#     sprintf("E6%d", 0:4), "E86", c(sprintf("V%02d", 1:99)), sprintf("X4%d", 0:4),
#     sprintf("X4%d", 5:9), "R95", c(sprintf("W%02d", 0:19)), sprintf("X0%d", 0:9),
#     sprintf("X3%d", 0:9), c(sprintf("W%02d", 65:74)), c(sprintf("W%02d", 75:84)),
#     c(sprintf("W%02d", 85:99)), c(sprintf("X%02d", 85:99)),
#     c(sprintf("Y%02d", 00:09)), c(sprintf("Y%02d", 10:34)), c(sprintf("W%02d", 20:49)),
#     c(sprintf("Y%02d", 60:69)), c(sprintf("Y%02d", 83:84)), c(sprintf("Y%02d", 40:59))
#   )
#
# mal_definidas <- c(
#     c(sprintf("R%02d", 00:94)), c(sprintf("R%02d", 96:99)),
#     "P95", "P969"
#   )
#
#
# ## Causas evitáveis para óbitos fetais ------------------------------------
# df_evitaveis_fetal <- df_sim_dofet |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_fetal_imunoprevencao",
#       causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao~ "evitaveis_fetal_mulher_gestacao",
#       causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_fetal_parto",
#       causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_fetal_recem_nascido",
#       causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_fetal_tratamento",
#       causabas %in% saude | causabas2 %in% saude ~ "evitaveis_fetal_saude",
#       causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_fetal_mal_definidas"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_outros", grupo_cid)
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal[is.na(df_evitaveis_fetal)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_evitaveis_fetal)
#
# df_evitaveis_fetal_antes <- df_sim_dofet_antes |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_fetal_antes_imunoprevencao",
#       causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao~ "evitaveis_fetal_antes_mulher_gestacao",
#       causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_fetal_antes_parto",
#       causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_fetal_antes_recem_nascido",
#       causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_fetal_antes_tratamento",
#       causabas %in% saude | causabas2 %in% saude~ "evitaveis_fetal_antes_saude",
#       causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_fetal_antes_mal_definidas"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_antes_outros", grupo_cid)
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais_antes) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal_antes[is.na(df_evitaveis_fetal_antes)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_evitaveis_fetal_antes)
#
# df_evitaveis_fetal_durante <- df_sim_dofet_durante |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_fetal_durante_imunoprevencao",
#       causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao~ "evitaveis_fetal_durante_mulher_gestacao",
#       causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_fetal_durante_parto",
#       causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_fetal_durante_recem_nascido",
#       causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_fetal_durante_tratamento",
#       causabas %in% saude | causabas2 %in% saude~ "evitaveis_fetal_durante_saude",
#       causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_fetal_durante_mal_definidas"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_durante_outros", grupo_cid)
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais_durante) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal_durante[is.na(df_evitaveis_fetal_durante)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_evitaveis_fetal_durante)
#
# ## Para a tabela de causas evitáveis em óbitos fetais
# df_evitaveis_fetal_tabela <- df_sim_dofet |>
#   filter(codmunres %in% codigos_municipios) |>
#   left_join(df_cid10) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao ~ "Reduzível pelas ações de imunização",
#       causabas %in% mulher_gestacao ~ "Reduzíveis por adequada atenção à mulher na gestação",
#       causabas %in% evitaveis_parto ~ "Reduzíveis por adequada atenção à mulher no parto",
#       causabas %in% recem_nascido ~ "Reduzíveis por adequada atenção ao recém-nascido",
#       causabas %in% tratamento ~ "Reduzíveis por ações de diagnóstico e tratamento adequado",
#       causabas %in% saude ~ "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção",
#       causabas %in% mal_definidas ~ "Causas mal definidas",
#       TRUE ~ "Demais causas (não claramente evitáveis)"
#     ),
#     obitos = 1
#   ) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   select(codmunres, ano, capitulo_cid10, grupo_cid, causabas_subcategoria, obitos) |>
#   arrange(codmunres)
#
# write.csv(df_evitaveis_fetal_tabela, gzfile("data-raw/csv/evitaveis_fetal_2022_2024.csv.gz"), row.names = FALSE)
#
#
# ## Causas evitáveis para óbitos neonatais ------------------------------------------------------
 # df_neonatais_totais_peso <- df_sim_doinf |>
 #   mutate(
 #     faixa_de_peso = case_when(
 #       as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
 #       as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
 #       as.numeric(peso) > 1500 ~ "> 1500 g"
 #     )
 #   ) |>
 #   select(codmunres, ano, faixa_de_peso) |>
 #   mutate(obitos_neonatais_totais = 1) |>
 #   group_by(across(!obitos_neonatais_totais)) |>
 #   summarise(obitos_neonatais_totais = sum(obitos_neonatais_totais)) |>
 #   ungroup() |>
 #   right_join(df_aux_municipios) |>
 #   arrange(codmunres)

# df_neonatais_precoce_totais_peso <- df_sim_doinf |>
#   mutate(
#     faixa_de_peso = case_when(
#       as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
#       as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
#       as.numeric(peso) > 1500 ~ "> 1500 g"
#     )
#   ) |>
#   filter(idade < 207 | as.numeric(peso) < 1000) |>
#   select(codmunres, ano, faixa_de_peso) |>
#   mutate(obitos_neonatais_precoce_totais = 1) |>
#   group_by(across(!obitos_neonatais_precoce_totais)) |>
#   summarise(obitos_neonatais_precoce_totais = sum(obitos_neonatais_precoce_totais)) |>
#   ungroup() |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres)
#

 # Causas evitáveis --------------------------------------------------------
 # ## Criando um vetor com as cids
#  imunoprevencao <- c(
#    "A17", "A19", "A33", "A35", "A36", "A37", "A80", "B05", "B06",
#    "B16", "B260", "G000", "P350", "P353"
#  )
#
#  mulher_gestacao <- c(
#    "A50", sprintf("B2%d", 0:4), "P022", "P023", "P027", "P028",
#    "P029", "P00", "P04", "P01", "P05", "P07", "P220", "P26",
#    "P52", "P550", "P551", "P558", "P559", "P56", "P57", "P77"
#  )
#
#  evitaveis_parto <- c(
#    "P020", "P021", "P024", "P025", "P026", "P03", "P08", sprintf("P1%d", 0:5),
#    "P20", "P21", "P24"
#  )
#
#  recem_nascido <- c(
#    "P221", "P228", "P229", "P23", "P25", "P27", "P28",
#    sprintf("P3%d", 51:53), sprintf("P3%d", 58:59), sprintf("P3%d", 6:9), sprintf("P5%d", 0:1), sprintf("P5%d", 3:4), "P58", "P59",
#    sprintf("P7%d", 0:4), "P60", "P61",  sprintf("P7%d", 5:6), "P78",
#    sprintf("P8%d", 0:3),  sprintf("P9%d", 0:4),
#    sprintf("P9%d", 60:68)
#  )
#
#  tratamento <- c(
#    "A15", "A16", "A18", sprintf("G0%d", 0:4), sprintf("J0%d", 0:6),
#    sprintf("J1%d", 2:8), sprintf("J1%d", 2:8), sprintf("J2%d", 0:2),
#    "J384", sprintf("J4%d", 0:2), sprintf("J4%d", 5:7), sprintf("J6%d", 8:9),
#    sprintf("A7%d", 0:4), "A30", "A31", "A32", "A38", "A39", "A40", "A41",
#    "A46", "A49", "E030", "E031", sprintf("E1%d", 0:4), "E700", "E730",
#    "G40", "G41", "Q90", "N390", sprintf("I0%d", 0:9)
#  )
#
#  saude <- c(
#    sprintf("A0%d", 0:9), sprintf("A2%d", 0:8), sprintf("A9%d", 0:9),
#    sprintf("A7%d", 5:9), "A82", sprintf("B5%d", 0:9), sprintf("B6%d", 0:4),
#    sprintf("B6%d", 5:9), sprintf("B7%d", 0:9), sprintf("B8%d", 0:3),
#    "B99", sprintf("D5%d", 0:3), sprintf("E4%d", 0:9), sprintf("E5%d", 0:9),
#    sprintf("E6%d", 0:4), "E86", c(sprintf("V%02d", 1:99)), sprintf("X4%d", 0:4),
#    sprintf("X4%d", 5:9), "R95", c(sprintf("W%02d", 0:19)), sprintf("X0%d", 0:9),
#    sprintf("X3%d", 0:9), c(sprintf("W%02d", 65:74)), c(sprintf("W%02d", 75:84)),
#    c(sprintf("W%02d", 85:99)), c(sprintf("X%02d", 85:99)),
#    c(sprintf("Y%02d", 00:09)), c(sprintf("Y%02d", 10:34)), c(sprintf("W%02d", 20:49)),
#    c(sprintf("Y%02d", 60:69)), c(sprintf("Y%02d", 83:84)), c(sprintf("Y%02d", 40:59))
#  )
#
#  mal_definidas <- c(
#    c(sprintf("R%02d", 00:94)), c(sprintf("R%02d", 96:99)),
#    "P95", "P969"
#  )
#
#  df_neonatais_evitaveis <- df_sim_doinf |>
#    mutate(
#      causabas = causabas,
#      causabas2 = substr(causabas, 1 , 3)
#    ) |>
#    mutate(
#      grupo_cid = case_when(
#        causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_neonatal_imunoprevencao",
#        causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_mulher_gestacao",
#        causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_neonatal_parto",
#        causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_recem_nascido",
#        causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_neonatal_tratamento",
#        causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_saude",
#        causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_neonatal_mal_definidas"
#      ),
#      grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_outros", grupo_cid),
#      faixa_de_peso = case_when(
#        as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
#        as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
#        as.numeric(peso) > 1500 ~ "> 1500 g"
#      )
#    ) |>
#    select(codmunres, ano, grupo_cid, faixa_de_peso) |>
#    mutate(obitos = 1) |>
#    group_by(across(!obitos)) |>
#    summarise(obitos = sum(obitos)) |>
#    ungroup() |>
#    pivot_wider(
#      names_from = grupo_cid,
#      values_from = obitos,
#      values_fill = 0
#    ) |>
#    right_join(df_neonatais_totais_peso) |>
#    right_join(df_aux_municipios) |>
#    arrange(codmunres) |>
#    mutate(faixa_de_peso = ifelse(is.na(faixa_de_peso), "não se aplica", faixa_de_peso))
#
#  ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_evitaveis[is.na(df_neonatais_evitaveis)] <- 0
#
#
# write.csv(df_neonatais_evitaveis, "data-raw/csv/indicadores_bloco8_grafico_evitaveis_neonatal_2012-2024.csv", row.names = FALSE)
#
# #
# #
# # df_neonatais_precoce_evitaveis <- df_sim_doinf |>
# #   mutate(
# #     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao  | causabas2 %in% imunoprevencao~ "evitaveis_neonatal_precoce_imunoprevencao",
#       causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_precoce_mulher_gestacao",
#       causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto~ "evitaveis_neonatal_precoce_parto",
#       causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_precoce_recem_nascido",
#       causabas %in% tratamento | causabas2 %in% tratamento~ "evitaveis_neonatal_precoce_tratamento",
#       causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_precoce_saude",
#       causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "evitaveis_neonatal_precoce_mal_definidas"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_precoce_outros", grupo_cid),
#   ) |>
#   filter(idade < 207 | as.numeric(peso) < 1000) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_neonatais_totais_precoce) |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_precoce_evitaveis[is.na(df_neonatais_precoce_evitaveis)] <- 0
#
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_precoce_evitaveis)
#
#
# df_neonatais_evitaveis_0_dias <- df_sim_doinf_0_dias |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_neonatal_0_dias_imunoprevencao",
#       causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_0_dias_mulher_gestacao",
#       causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_neonatal_0_dias_parto",
#       causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_0_dias_recem_nascido",
#       causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_neonatal_0_dias_tratamento",
#       causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_0_dias_saude",
#       causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_neonatal_0_dias_mal_definidas"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_0_dias_outros", grupo_cid)#,
#     # faixa_de_peso = case_when(
#     #   as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
#     #   as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
#     #   as.numeric(peso) > 1500 ~ "> 1500 g"
#     # )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_neonatais_totais_0_dias) |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres) #|>
#   #mutate(faixa_de_peso = ifelse(is.na(faixa_de_peso), "não se aplica", faixa_de_peso))
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_evitaveis_0_dias[is.na(df_neonatais_evitaveis_0_dias)] <- 0
#
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_evitaveis_0_dias)
#
# df_neonatais_evitaveis_1_6_dias <- df_sim_doinf_1_6_dias |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_neonatal_1_6_dias_imunoprevencao",
#       causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_1_6_dias_mulher_gestacao",
#       causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_neonatal_1_6_dias_parto",
#       causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_1_6_dias_recem_nascido",
#       causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_neonatal_1_6_dias_tratamento",
#       causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_1_6_dias_saude",
#       causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_neonatal_1_6_dias_mal_definidas"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_1_6_dias_outros", grupo_cid)#,
#     # faixa_de_peso = case_when(
#     #   as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
#     #   as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
#     #   as.numeric(peso) > 1500 ~ "> 1500 g"
#     # )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_neonatais_totais_1_6_dias) |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres) #|>
#   #mutate(faixa_de_peso = ifelse(is.na(faixa_de_peso), "não se aplica", faixa_de_peso))
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_evitaveis_1_6_dias[is.na(df_neonatais_evitaveis_1_6_dias)] <- 0
#
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_evitaveis_1_6_dias)
#
# df_neonatais_evitaveis_7_27_dias <- df_sim_doinf_7_27_dias |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_neonatal_7_27_dias_imunoprevencao",
#       causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_7_27_dias_mulher_gestacao",
#       causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_neonatal_7_27_dias_parto",
#       causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_7_27_dias_recem_nascido",
#       causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_neonatal_7_27_dias_tratamento",
#       causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_7_27_dias_saude",
#       causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_neonatal_7_27_dias_mal_definidas"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_7_27_dias_outros", grupo_cid)#,
#     # faixa_de_peso = case_when(
#     #   as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
#     #   as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
#     #   as.numeric(peso) > 1500 ~ "> 1500 g"
#     # )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_neonatais_totais_7_27_dias) |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres) #|>
#   #mutate(faixa_de_peso = ifelse(is.na(faixa_de_peso), "não se aplica", faixa_de_peso))
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_evitaveis_7_27_dias[is.na(df_neonatais_evitaveis_7_27_dias)] <- 0
#
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_evitaveis_7_27_dias)
#
#

 # Causas evitáveis --------------------------------------------------------
 # ## Criando um vetor com as cids
#   imunoprevencao <- c(
#       "A17", "A19", "A33", "A35", "A36", "A37", "A80", "B05", "B06",
#       "B16", "B260", "G000", "P350", "P353"
#     )
#
#   mulher_gestacao <- c(
#       "A50", sprintf("B2%d", 0:4), "P022", "P023", "P027", "P028",
#       "P029", "P00", "P04", "P01", "P05", "P07", "P220", "P26",
#       "P52", "P550", "P551", "P558", "P559", "P56", "P57", "P77"
#     )
#
#   evitaveis_parto <- c(
#       "P020", "P021", "P024", "P025", "P026", "P03", "P08", sprintf("P1%d", 0:5),
#       "P20", "P21", "P24"
#     )
#
#   recem_nascido <- c(
#       "P221", "P228", "P229", "P23", "P25", "P27", "P28",
#       sprintf("P3%d", 51:53), sprintf("P3%d", 58:59), sprintf("P3%d", 6:9), sprintf("P5%d", 0:1), sprintf("P5%d", 3:4), "P58", "P59",
#       sprintf("P7%d", 0:4), "P60", "P61",  sprintf("P7%d", 5:6), "P78",
#       sprintf("P8%d", 0:3),  sprintf("P9%d", 0:4),
#       sprintf("P9%d", 60:68)
#     )
#
#   tratamento <- c(
#       "A15", "A16", "A18", sprintf("G0%d", 0:4), sprintf("J0%d", 0:6),
#       sprintf("J1%d", 2:8), sprintf("J1%d", 2:8), sprintf("J2%d", 0:2),
#       "J384", sprintf("J4%d", 0:2), sprintf("J4%d", 5:7), sprintf("J6%d", 8:9),
#      sprintf("A7%d", 0:4), "A30", "A31", "A32", "A38", "A39", "A40", "A41",
#    "A46", "A49", "E030", "E031", sprintf("E1%d", 0:4), "E700", "E730",
#       "G40", "G41", "Q90", "N390", sprintf("I0%d", 0:9)
#     )
#
#   saude <- c(
#       sprintf("A0%d", 0:9), sprintf("A2%d", 0:8), sprintf("A9%d", 0:9),
#       sprintf("A7%d", 5:9), "A82", sprintf("B5%d", 0:9), sprintf("B6%d", 0:4),
#       sprintf("B6%d", 5:9), sprintf("B7%d", 0:9), sprintf("B8%d", 0:3),
#       "B99", sprintf("D5%d", 0:3), sprintf("E4%d", 0:9), sprintf("E5%d", 0:9),
#       sprintf("E6%d", 0:4), "E86", c(sprintf("V%02d", 1:99)), sprintf("X4%d", 0:4),
#       sprintf("X4%d", 5:9), "R95", c(sprintf("W%02d", 0:19)), sprintf("X0%d", 0:9),
#       sprintf("X3%d", 0:9), c(sprintf("W%02d", 65:74)), c(sprintf("W%02d", 75:84)),
#       c(sprintf("W%02d", 85:99)), c(sprintf("X%02d", 85:99)),
#     c(sprintf("Y%02d", 00:09)), c(sprintf("Y%02d", 10:34)), c(sprintf("W%02d", 20:49)),
#       c(sprintf("Y%02d", 60:69)), c(sprintf("Y%02d", 83:84)), c(sprintf("Y%02d", 40:59))
#     )
#
#   mal_definidas <- c(
#       c(sprintf("R%02d", 00:94)), c(sprintf("R%02d", 96:99)),
#       "P95", "P969"
#     )
# ### Para a tabela de causas evitáveis em óbitos neonatais
#  df_evitaveis_neonatal_tabela <- df_sim_doinf |>
#    filter(codmunres %in% codigos_municipios) |>
#    left_join(df_cid10) |>
#    mutate(
#      idade = as.numeric(idade),
#      peso = as.numeric(peso),
#      faixa_de_peso = factor(
#        case_when(
#          is.na(peso) ~ "Sem informação",
#          peso < 1500 ~ "< 1500 g",
#          peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
#          peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
#          peso >= 2500 ~ "\U2265 2500 g"
#        ),
#        levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
#      ),
#      faixa_de_idade = factor(
#        case_when(
#          idade <= 206 ~ "0 a 6 dias",
#          idade > 206 & idade <= 227 ~ "7 a 27 dias"
#      ),
#        levels = c("0 a 6 dias", "7 a 27 dias")
#      ),
#      grupo_cid = case_when(
#        causabas %in% imunoprevencao ~ "Reduzível pelas ações de imunização",
#        causabas %in% mulher_gestacao ~ "Reduzíveis por adequada atenção à mulher na gestação",
#        causabas %in% evitaveis_parto ~ "Reduzíveis por adequada atenção à mulher no parto",
#        causabas %in% recem_nascido ~ "Reduzíveis por adequada atenção ao recém-nascido",
#        causabas %in% tratamento ~ "Reduzíveis por ações de diagnóstico e tratamento adequado",
#        causabas %in% saude ~ "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção",
#        causabas %in% mal_definidas ~ "Causas mal definidas",
#        TRUE ~ "Demais causas (não claramente evitáveis)"
#      ),
#      obitos = 1
#    ) |>
#    group_by(across(!obitos)) |>
#    summarise(obitos = sum(obitos)) |>
#    ungroup() |>
#    select(codmunres, ano, capitulo_cid10, grupo_cid, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
#    arrange(codmunres)
#
#  write.csv(df_evitaveis_neonatal_tabela, gzfile("data-raw/csv/evitaveis_neonatal_2012_2024.csv.gz"), row.names = FALSE)
#
#
#
#
# # Nova Referência de causas evitáveis --------------------------------------------------------
# ## Criando um vetor com as cids
#
# df_cids_fetal_evitaveis2 <- read_excel("data-raw/extracao-dos-dados/blocos/databases_auxiliares/evitabilidade_fetal.xlsx", sheet = "Fetal")
# colnames(df_cids_fetal_evitaveis2) <- c("nome", "cid")
#
#
# imunoprevencao2 <- filter(df_cids_fetal_evitaveis2, nome == "Imunoprevenção")$cid
#
# mulher_gestacao2 <- filter(df_cids_fetal_evitaveis2, nome == "Reduzíveis por adequada atenção à mulher na gestação")$cid
#
# evitaveis_parto2 <- filter(df_cids_fetal_evitaveis2, nome == "Reduzíveis por adequada atenção à mulher no parto")$cid
#
# mal_definidas2 <- filter(df_cids_fetal_evitaveis2, nome == "Causas de morte mal-definidas")$cid
#
# nao_aplica2 <- filter(df_cids_fetal_evitaveis2, nome == "Não se aplicam ao óbito fetal")$cid
#
# ## Causas evitáveis para óbitos fetais ------------------------------------
# df_evitaveis_fetal2 <- df_sim_dofet |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao2 | causabas2 %in% imunoprevencao2 ~ "evitaveis_fetal_imunoprevencao2",
#       causabas %in% mulher_gestacao2 | causabas2 %in% mulher_gestacao2~ "evitaveis_fetal_mulher_gestacao2",
#       causabas %in% evitaveis_parto2 | causabas2 %in% evitaveis_parto2 ~ "evitaveis_fetal_parto2",
#       causabas %in% nao_aplica2 | causabas2 %in% nao_aplica2 ~ "evitaveis_fetal_nao_aplica2",
#       causabas %in% mal_definidas2 | causabas2 %in% mal_definidas2~ "evitaveis_fetal_mal_definidas2"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_outros2", grupo_cid)
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal2[is.na(df_evitaveis_fetal2)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_evitaveis_fetal2)
#
# df_evitaveis_fetal_antes2 <- df_sim_dofet_antes |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao2 | causabas2 %in% imunoprevencao2 ~ "evitaveis_fetal_antes_imunoprevencao2",
#       causabas %in% mulher_gestacao2 | causabas2 %in% mulher_gestacao2~ "evitaveis_fetal_antes_mulher_gestacao2",
#       causabas %in% evitaveis_parto2 | causabas2 %in% evitaveis_parto2 ~ "evitaveis_fetal_antes_parto2",
#       causabas %in% nao_aplica2 | causabas2 %in% nao_aplica2 ~ "evitaveis_fetal_antes_nao_aplica2",
#       causabas %in% mal_definidas2 | causabas2 %in% mal_definidas2~ "evitaveis_fetal_antes_mal_definidas2"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_antes_outros2", grupo_cid)
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais_antes) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal_antes2[is.na(df_evitaveis_fetal_antes2)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_evitaveis_fetal_antes2)
#
# df_evitaveis_fetal_durante2 <- df_sim_dofet_durante |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% imunoprevencao2 | causabas2 %in% imunoprevencao2 ~ "evitaveis_fetal_durante_imunoprevencao2",
#       causabas %in% mulher_gestacao2 | causabas2 %in% mulher_gestacao2~ "evitaveis_fetal_durante_mulher_gestacao2",
#       causabas %in% evitaveis_parto2 | causabas2 %in% evitaveis_parto2 ~ "evitaveis_fetal_durante_parto2",
#       causabas %in% nao_aplica2 | causabas2 %in% nao_aplica2 ~ "evitaveis_fetal_durante_nao_aplica2",
#       causabas %in% mal_definidas2 | causabas2 %in% mal_definidas2~ "evitaveis_fetal_durante_mal_definidas2"
#     ),
#     grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_durante_outros2", grupo_cid)
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais_durante) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal_durante2[is.na(df_evitaveis_fetal_durante2)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_evitaveis_fetal_durante2)
#
#
# # Grupos de causas para óbitos fetais -------------------------------------------------------
# ## Vetores com as cids de alguns grupos
# grupos_prematuridade <- c("P07", "P220", "P25", "P26", "P52", "P77")
#
# grupos_infeccoes <- c("P35", "P36", "P37", "P38", "P39", "A40", "A41", "P23",
#                       "J12", "J13", "J14", "J15", "J16", "J17", "J18", "A00", "A01",
#                       "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A33",
#                       "A50", "B20", "B21", "B22", "B23", "B24", "G00", "G03", "G04")
#
# grupos_asfixia <- c("P017", "P020", "P021", "P024", "P025", "P026", "P03",
#                     "P10", "P11", "P12", "P13", "P14", "P15", "P20", "P21", "P24")
#
# grupos_respiratorias <- c("P221", "P228", "P229", "P28")
#
# grupos_gravidez <- c("P00", "P010", "P011", "P012", "P013", "P014", "P015", "P016",
#                      "P018", "P019", "P022", "P023", "P027", "P028", "P029", "P04",
#                      "P05", "P964")
#
# grupos_cardiorrespiratoria <- c("P221", "P228", "P229", "P28")
#
# grupos_afeccoes_perinatal <- c("P969")
#
# grupos_ma_formacao <- c(paste0("Q", sprintf("%02d", 0:99)))
#
# grupos_mal_definida <- c(paste0("R", sprintf("%02d", 0:99)))
#
#
# grupos_todas_subcategorias <- c("P017", "P020", "P021", "P024", "P025", "P026", "P221", "P228", "P229",
#                                 "P00", "P010", "P011", "P012", "P013", "P014", "P015", "P016",
#                                 "P018", "P019", "P022", "P023", "P027", "P028", "P029", "P964", "P969")
#
#
# ## Para a tabela de grupos de causa em óbitos fetais
#
# df_cid10 <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_cid10_completo.csv")
#
# df_grupos_fetais_tabela <- df_sim_dofet |>
#   filter(codmunres %in% codigos_municipios) |>
#   left_join(df_cid10) |>
#   mutate(
#     causabas_categoria = causabas,
#     causabas = substr(causabas, 1, 3),
#     grupo_cid10 = case_when(
#       causabas %in% grupos_prematuridade | causabas_categoria %in% grupos_prematuridade ~ "Prematuridade",
#       causabas %in% grupos_infeccoes | causabas_categoria %in% grupos_infeccoes ~ "Infecções",
#       causabas %in% grupos_asfixia | causabas_categoria %in% grupos_asfixia ~ "Asfixia/Hipóxia",
#       causabas %in% grupos_respiratorias | causabas_categoria %in% grupos_respiratorias ~ "Afecções respiratórias do recém-nascido",
#       causabas %in% grupos_gravidez | causabas_categoria %in% grupos_gravidez ~ "Fatores maternos relacionados à gravidez",
#       #causabas %in% grupos_cardiorrespiratoria | causabas_categoria %in% grupos_cardiorrespiratoria ~ "Transtornos cardiorrespiratórios originados do período perinatal",
#       causabas %in% grupos_afeccoes_perinatal | causabas_categoria %in% grupos_afeccoes_perinatal ~ "Afecções ariginais no período perinatal",
#       causabas %in% grupos_ma_formacao ~ "Má formação congênita",
#       causabas %in% grupos_mal_definida ~ "Causas mal definidas",
#       TRUE ~ "Outras causas"
#     ),
#     obitos = 1
#   ) |>
#   select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   arrange(codmunres, ano)
#
# get_dupes(df_grupos_fetais_tabela)
#
# write.csv(df_grupos_fetais_tabela, gzfile("data-raw/csv/grupos_fetal_2022_2024.csv.gz"), row.names = FALSE)
#
# # dados organizados para os gráficos de grupos de causa fetal
#
# df_fetais_grupos <- df_sim_dofet |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_grupos_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_grupos_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_grupos_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_grupos_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_grupos_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_grupos_afeccoes_perinatal",
#       causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao ~ "fetal_grupos_ma_formacao",
#       causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida ~ "fetal_grupos_mal_definida",
#       TRUE ~ "fetal_grupos_outros"
#     )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_fetais_grupos[is.na(df_fetais_grupos)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_grupos)
#
# df_fetais_grupos_antes <- df_sim_dofet_antes |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_grupos_antes_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_grupos_antes_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_grupos_antes_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_grupos_antes_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_grupos_antes_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_grupos_antes_afeccoes_perinatal",
#       causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao~ "fetal_grupos_antes_ma_formacao",
#       causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida~ "fetal_grupos_antes_mal_definida",
#       TRUE ~ "fetal_grupos_antes_outros"
#     )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais_antes) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_fetais_grupos_antes[is.na(df_fetais_grupos_antes)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_grupos_antes)
#
#
# df_fetais_grupos_durante <- df_sim_dofet_durante |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_grupos_durante_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_grupos_durante_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_grupos_durante_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_grupos_durante_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_grupos_durante_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_grupos_durante_afeccoes_perinatal",
#       causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao~ "fetal_grupos_durante_ma_formacao",
#       causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida~ "fetal_grupos_durante_mal_definida",
#       TRUE ~ "fetal_grupos_durante_outros"
#     )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_fetais_totais_durante) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_fetais_grupos_durante[is.na(df_fetais_grupos_durante)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_grupos_durante)
#
#
# # Grupos de causas para óbitos neonatais -------------------------------------------------------
#
#
# #tabela grupos de causa para óbitos neonatais
#
# df_grupos_neonatais_tabela <- df_sim_doinf |>
#   filter(codmunres %in% codigos_municipios) |>
#   left_join(df_cid10) |>
#   mutate(
#     idade = as.numeric(idade),
#     peso = as.numeric(peso),
#     faixa_de_peso = factor(
#       case_when(
#         is.na(peso) ~ "Sem informação",
#         peso < 1500 ~ "< 1500 g",
#         peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
#         peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
#         peso >= 2500 ~ "\U2265 2500 g"
#       ),
#       levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
#     ),
#     faixa_de_idade = factor(
#       case_when(
#         idade <= 206 ~ "0 a 6 dias",
#         idade > 206 & idade <= 227 ~ "7 a 27 dias"
#       ),
#       levels = c("0 a 6 dias", "7 a 27 dias")
#     ),
#     causabas_categoria = causabas,
#     causabas = substr(causabas, 1, 3),
#     grupo_cid10 = case_when(
#       causabas %in% grupos_prematuridade | causabas_categoria %in% grupos_prematuridade ~ "Prematuridade",
#       causabas %in% grupos_infeccoes | causabas_categoria %in% grupos_infeccoes ~ "Infecções",
#       causabas %in% grupos_asfixia | causabas_categoria %in% grupos_asfixia ~ "Asfixia/Hipóxia",
#       causabas %in% grupos_respiratorias | causabas_categoria %in% grupos_respiratorias ~ "Afecções respiratórias do recém-nascido",
#       causabas %in% grupos_gravidez | causabas_categoria %in% grupos_gravidez ~ "Fatores maternos relacionados à gravidez",
#       #causabas %in% grupos_cardiorrespiratoria | causabas_categoria %in% grupos_cardiorrespiratoria ~ "Transtornos cardiorrespiratórios originados do período perinatal",
#       causabas %in% grupos_afeccoes_perinatal | causabas_categoria %in% grupos_afeccoes_perinatal ~ "Afecções ariginais no período perinatal",
#       causabas %in% grupos_ma_formacao ~ "Má formação congênita",
#       causabas %in% grupos_mal_definida ~ "Causas mal definidas",
#       TRUE ~ "Outras causas"
#     ),
#     obitos = 1
#   ) |>
#   select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   arrange(codmunres, ano)
#
# get_dupes(df_grupos_neonatais_tabela)
#
# write.csv(df_grupos_neonatais_tabela, gzfile("data-raw/csv/grupos_neonatal_2022_2024.csv.gz"), row.names = FALSE)
#
#
# #gráfico grupos de causa para óbitos neonatais
#
# df_neonatais_grupos <- df_sim_doinf |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_grupos_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_grupos_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "neonat_grupos_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "neonat_grupos_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_grupos_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_grupos_afeccoes_perinatal",
#       causabas >= "Q00" & causabas <= "Q99" ~ "neonat_grupos_ma_formacao",
#       causabas >= "R00" & causabas <= "R99" ~ "neonat_grupos_mal_definida",
#       TRUE ~ "neonat_grupos_outros"
#     )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_neonatais_totais) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_grupos[is.na(df_neonatais_grupos)] <- 0
#
# df_neonatais_precoce_grupos <- df_sim_doinf |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_precoce_grupos_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_precoce_grupos_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "neonat_precoce_grupos_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias~ "neonat_precoce_grupos_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_precoce_grupos_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "neonat_precoce_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_precoce_grupos_afeccoes_perinatal",
#       causabas >= "Q00" & causabas <= "Q99" ~ "neonat_precoce_grupos_ma_formacao",
#       causabas >= "R00" & causabas <= "R99" ~ "neonat_precoce_grupos_mal_definida",
#       TRUE ~ "neonat_precoce_grupos_outros"
#     )
#   ) |>
#   filter(idade < 207 | as.numeric(peso) < 1000) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_neonatais_totais_precoce) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_precoce_grupos[is.na(df_neonatais_precoce_grupos)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_precoce_grupos)
#
# df_neonatais_grupos_0_dias <- df_sim_doinf_0_dias |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_grupos_0_dias_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_grupos_0_dias_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "neonat_grupos_0_dias_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "neonat_grupos_0_dias_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_grupos_0_dias_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_grupos_0_dias_afeccoes_perinatal",
#       causabas >= "Q00" & causabas <= "Q99" ~ "neonat_grupos_0_dias_ma_formacao",
#       causabas >= "R00" & causabas <= "R99" ~ "neonat_grupos_0_dias_mal_definida",
#       TRUE ~ "neonat_grupos_0_dias_outros"
#     )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_neonatais_totais_0_dias) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_grupos_0_dias[is.na(df_neonatais_grupos_0_dias)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_grupos_0_dias)
#
#
# df_neonatais_grupos_1_6_dias <- df_sim_doinf_1_6_dias |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_grupos_1_6_dias_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_grupos_1_6_dias_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "neonat_grupos_1_6_dias_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "neonat_grupos_1_6_dias_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_grupos_1_6_dias_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_grupos_1_6_dias_afeccoes_perinatal",
#       causabas >= "Q00" & causabas <= "Q99" ~ "neonat_grupos_1_6_dias_ma_formacao",
#       causabas >= "R00" & causabas <= "R99" ~ "neonat_grupos_1_6_dias_mal_definida",
#       TRUE ~ "neonat_grupos_1_6_dias_outros"
#     )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_neonatais_totais_1_6_dias) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_grupos_1_6_dias[is.na(df_neonatais_grupos_1_6_dias)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_grupos_1_6_dias)
#
#
# df_neonatais_grupos_7_27_dias <- df_sim_doinf_7_27_dias |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_grupos_7_27_dias_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_grupos_7_27_dias_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "neonat_grupos_7_27_dias_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "neonat_grupos_7_27_dias_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_grupos_7_27_dias_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_grupos_7_27_dias_afeccoes_perinatal",
#       causabas >= "Q00" & causabas <= "Q99" ~ "neonat_grupos_7_27_dias_ma_formacao",
#       causabas >= "R00" & causabas <= "R99" ~ "neonat_grupos_7_27_dias_mal_definida",
#       TRUE ~ "neonat_grupos_7_27_dias_outros"
#     )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(obitos = 1) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = obitos,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_neonatais_totais_7_27_dias) |>
#   arrange(codmunres)
#
# ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_grupos_7_27_dias[is.na(df_neonatais_grupos_7_27_dias)] <- 0
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_grupos_7_27_dias)
#
#
# ## Juntando com o restante da base do bloco 8
# df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_grupos)
#
# df_bloco8_graficos[is.na(df_bloco8_graficos)] <- 0
#
# df_bloco8_graficos <- df_bloco8_graficos |>
#   mutate(
#     obitos_perinatais_totais = obitos_neonatais_precoce_totais + obitos_fetais_totais,
#     # principais_perinatal_a00_b99 = principais_neonatal_precoce_a00_b99 + principais_fetal_a00_b99,
#     # #principais_perinatal_j00_j99 = principais_neonatal_precoce_j00_j99, #não temos essa categoria para fetal,
#     # principais_perinatal_p00_p04 = principais_neonatal_precoce_p00_p04 + principais_fetal_p00_p04,
#     # principais_perinatal_p05_p08 = principais_neonatal_precoce_p05_p08 + principais_fetal_p05_p08,
#     # principais_perinatal_p10_p15 = principais_neonatal_precoce_p10_p15 + principais_fetal_p10_p15,
#     # principais_perinatal_p20_p29 = principais_neonatal_precoce_p20_p29 + principais_fetal_p20_p29,
#     # principais_perinatal_p35_p39 = principais_neonatal_precoce_p35_p39 + principais_fetal_p35_p39,
#     # principais_perinatal_p50_p61 = principais_neonatal_precoce_p50_p61 + principais_fetal_p50_p61,
#     # principais_perinatal_p70_p74 = principais_neonatal_precoce_p70_p74 + principais_fetal_p70_p74,
#     # principais_perinatal_p75_p78 = principais_neonatal_precoce_p75_p78 + principais_fetal_p75_p78,
#     # principais_perinatal_p80_p83 = principais_neonatal_precoce_p80_p83 + principais_fetal_p80_p83,
#     # principais_perinatal_p90_p96 = principais_neonatal_precoce_p90_p96 + principais_fetal_p90_p96,
#     # principais_perinatal_q00_q99 = principais_neonatal_precoce_q00_q99 + principais_fetal_q00_q99,
#     # principais_perinatal_outros = principais_neonatal_precoce_outros,
#     #evitaveis_perinatal_imunoprevencao = evitaveis_neonatal_precoce_imunoprevencao + evitaveis_fetal_imunoprevencao,
#     evitaveis_perinatal_mulher_gestacao = evitaveis_neonatal_precoce_mulher_gestacao + evitaveis_fetal_mulher_gestacao,
#     evitaveis_perinatal_parto = evitaveis_neonatal_precoce_parto + evitaveis_fetal_parto,
#     evitaveis_perinatal_recem_nascido = evitaveis_neonatal_precoce_recem_nascido + evitaveis_fetal_recem_nascido,
#     evitaveis_perinatal_tratamento = evitaveis_neonatal_precoce_tratamento,
#     evitaveis_perinatal_saude = evitaveis_neonatal_precoce_saude ,
#     evitaveis_perinatal_mal_definidas = evitaveis_neonatal_precoce_mal_definidas + evitaveis_fetal_mal_definidas,
#     evitaveis_perinatal_outros = evitaveis_neonatal_precoce_outros + evitaveis_fetal_outros,
#     perinatal_grupos_prematuridade = neonat_precoce_grupos_prematuridade + fetal_grupos_prematuridade,
#     perinatal_grupos_infeccoes = neonat_precoce_grupos_infeccoes,
#     perinatal_grupos_asfixia = neonat_precoce_grupos_asfixia + fetal_grupos_asfixia,
#     perinatal_grupos_ma_formacao = neonat_precoce_grupos_ma_formacao + fetal_grupos_ma_formacao,
#     perinatal_grupos_respiratorias = neonat_precoce_grupos_respiratorias,
#     perinatal_grupos_gravidez = neonat_precoce_grupos_gravidez + fetal_grupos_gravidez,
#     #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
#     perinatal_grupos_afeccoes_perinatal = neonat_precoce_grupos_afeccoes_perinatal + fetal_grupos_afeccoes_perinatal,
#     perinatal_grupos_mal_definida = neonat_precoce_grupos_mal_definida,
#     perinatal_grupos_outros = neonat_precoce_grupos_outros + fetal_grupos_outros,
#
#     #evitaveis_perinatal_antes_imunoprevencao = evitaveis_fetal_antes_imunoprevencao,
#     evitaveis_perinatal_antes_mulher_gestacao = evitaveis_fetal_antes_mulher_gestacao,
#     evitaveis_perinatal_antes_parto = evitaveis_fetal_antes_parto,
#     evitaveis_perinatal_antes_recem_nascido = evitaveis_fetal_antes_recem_nascido,
#     #evitaveis_perinatal_antes_tratamento = evitaveis_neonatal_precoce_tratamento,
#     #evitaveis_perinatal_antes_saude = evitaveis_neonatal_precoce_saude ,
#     evitaveis_perinatal_antes_mal_definidas = evitaveis_fetal_antes_mal_definidas,
#     evitaveis_perinatal_antes_outros = evitaveis_fetal_antes_outros,
#     perinatal_grupos_antes_prematuridade = fetal_grupos_antes_prematuridade,
#     #perinatal_grupos_antes_infeccoes = neonat_precoce_grupos_infeccoes,
#     perinatal_grupos_antes_asfixia = fetal_grupos_antes_asfixia,
#     perinatal_grupos_antes_ma_formacao = fetal_grupos_antes_ma_formacao,
#     #perinatal_grupos_antes_respiratorias = neonat_precoce_grupos_respiratorias,
#     perinatal_grupos_antes_gravidez = fetal_grupos_antes_gravidez,
#     #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
#     perinatal_grupos_antes_afeccoes_perinatal = fetal_grupos_antes_afeccoes_perinatal,
#     #perinatal_grupos_antes_mal_definida = neonat_precoce_grupos_mal_definida,
#     perinatal_grupos_antes_outros = fetal_grupos_antes_outros,
#
#     #evitaveis_perinatal_durante_imunoprevencao = evitaveis_fetal_durante_imunoprevencao,
#     evitaveis_perinatal_durante_mulher_gestacao = evitaveis_fetal_durante_mulher_gestacao,
#     evitaveis_perinatal_durante_parto = evitaveis_fetal_durante_parto,
#     evitaveis_perinatal_durante_recem_nascido = evitaveis_fetal_durante_recem_nascido,
#     #evitaveis_perinatal_antes_tratamento = evitaveis_neonatal_precoce_tratamento,
#     #evitaveis_perinatal_antes_saude = evitaveis_neonatal_precoce_saude ,
#     evitaveis_perinatal_durante_mal_definidas = evitaveis_fetal_durante_mal_definidas,
#     evitaveis_perinatal_durante_outros = evitaveis_fetal_durante_outros,
#     perinatal_grupos_durante_prematuridade = fetal_grupos_durante_prematuridade,
#     #perinatal_grupos_antes_infeccoes = neonat_precoce_grupos_infeccoes,
#     perinatal_grupos_durante_asfixia = fetal_grupos_durante_asfixia,
#     perinatal_grupos_durante_ma_formacao = fetal_grupos_durante_ma_formacao,
#     #perinatal_grupos_antes_respiratorias = neonat_precoce_grupos_respiratorias,
#     perinatal_grupos_durante_gravidez = fetal_grupos_durante_gravidez,
#     #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
#     perinatal_grupos_durante_afeccoes_perinatal = fetal_grupos_durante_afeccoes_perinatal,
#     #perinatal_grupos_antes_mal_definida = neonat_precoce_grupos_mal_definida,
#     perinatal_grupos_durante_outros = fetal_grupos_durante_outros,
#
#
#     #evitaveis_perinatal_0_dias_imunoprevencao = evitaveis_neonatal_0_dias_imunoprevencao,
#     evitaveis_perinatal_0_dias_mulher_gestacao = evitaveis_neonatal_0_dias_mulher_gestacao ,
#     evitaveis_perinatal_0_dias_parto = evitaveis_neonatal_0_dias_parto,
#     evitaveis_perinatal_0_dias_recem_nascido = evitaveis_neonatal_0_dias_recem_nascido,
#     evitaveis_perinatal_0_dias_tratamento = evitaveis_neonatal_0_dias_tratamento,
#     evitaveis_perinatal_0_dias_saude = evitaveis_neonatal_0_dias_saude ,
#     evitaveis_perinatal_0_dias_mal_definidas = evitaveis_neonatal_0_dias_mal_definidas,
#     evitaveis_perinatal_0_dias_outros = evitaveis_neonatal_0_dias_outros,
#     perinatal_grupos_0_dias_prematuridade = neonat_grupos_0_dias_prematuridade,
#     perinatal_grupos_0_dias_infeccoes = neonat_grupos_0_dias_infeccoes,
#     perinatal_grupos_0_dias_asfixia = neonat_grupos_0_dias_asfixia,
#     perinatal_grupos_0_dias_ma_formacao = neonat_grupos_0_dias_ma_formacao,
#     perinatal_grupos_0_dias_respiratorias = neonat_grupos_0_dias_respiratorias,
#     perinatal_grupos_0_dias_gravidez = neonat_grupos_0_dias_gravidez,
#     #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
#     perinatal_grupos_0_dias_afeccoes_perinatal = neonat_grupos_0_dias_afeccoes_perinatal,
#     perinatal_grupos_0_dias_mal_definida = neonat_grupos_0_dias_mal_definida,
#     perinatal_grupos_0_dias_outros = neonat_grupos_0_dias_outros,
#
#     evitaveis_perinatal_1_6_dias_imunoprevencao = evitaveis_neonatal_1_6_dias_imunoprevencao,
#     evitaveis_perinatal_1_6_dias_mulher_gestacao = evitaveis_neonatal_1_6_dias_mulher_gestacao ,
#     evitaveis_perinatal_1_6_dias_parto = evitaveis_neonatal_1_6_dias_parto,
#     evitaveis_perinatal_1_6_dias_recem_nascido = evitaveis_neonatal_1_6_dias_recem_nascido,
#     evitaveis_perinatal_1_6_dias_tratamento = evitaveis_neonatal_1_6_dias_tratamento,
#     evitaveis_perinatal_1_6_dias_saude = evitaveis_neonatal_1_6_dias_saude ,
#     evitaveis_perinatal_1_6_dias_mal_definidas = evitaveis_neonatal_1_6_dias_mal_definidas,
#     evitaveis_perinatal_1_6_dias_outros = evitaveis_neonatal_1_6_dias_outros,
#     perinatal_grupos_1_6_dias_prematuridade = neonat_grupos_1_6_dias_prematuridade,
#     perinatal_grupos_1_6_dias_infeccoes = neonat_grupos_1_6_dias_infeccoes,
#     perinatal_grupos_1_6_dias_asfixia = neonat_grupos_1_6_dias_asfixia,
#     perinatal_grupos_1_6_dias_ma_formacao = neonat_grupos_1_6_dias_ma_formacao,
#     perinatal_grupos_1_6_dias_respiratorias = neonat_grupos_1_6_dias_respiratorias,
#     perinatal_grupos_1_6_dias_gravidez = neonat_grupos_1_6_dias_gravidez,
#     #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
#     perinatal_grupos_afeccoes_perinatal = neonat_grupos_1_6_dias_afeccoes_perinatal,
#     perinatal_grupos_1_6_dias_mal_definida = neonat_grupos_1_6_dias_mal_definida,
#     perinatal_grupos_1_6_dias_outros = neonat_grupos_1_6_dias_outros
#
#     )




# df_neonatais_evitaveis_2023 <- df_neonatais_evitaveis
#
# df_neonatais_evitaveis_antigo <- read_csv("data-raw/csv/indicadores_bloco8_grafico_evitaveis_neonatal_2012-2024.csv") |>
#   filter(ano <= 2021) |>
#   select(all_of(names(df_neonatais_evitaveis_2023)))
#
# df_neonatais_evitaveis_2023$codmunres <- as.numeric(df_neonatais_evitaveis_2023$codmunres)
# df_neonatais_evitaveis_total <- rbind(df_neonatais_evitaveis_antigo, df_neonatais_evitaveis_2023)
#
# write.csv(df_neonatais_evitaveis_total, "data-raw/csv/indicadores_bloco8_grafico_evitaveis_neonatal_2012-2024.csv", row.names = FALSE)


#write.csv(df_neonatais_precoce_evitaveis, "data-raw/csv/indicadores_bloco8_grafico_evitaveis_neonatal_precoce_2012-2022.csv", row.names = FALSE)

################ GARBAGE CODES PARA MORBIDADE NEONATAL

# library(janitor)
# library(RSQLite)
# library(glue)
# library(tidyr)
# library(data.table)
# library(readr)
#
# # Para os indicadores provenientes do SIH ---------------------------------
# ## Criando um vetor com os anos considerados
# anos <- c(2012:2024)
#
# ## Criando um vetor com as siglas de todos os estados do Brasil
# estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
#              "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
#              "RS", "RO" , "RR", "SC","SP", "SE", "TO")
#
#
# ## Baixando os dados do SIH-RD
# for (estado in estados) {
#   # Criando data.frames que guardarão as bases do estado
#   df_sih_rd_menores_28_uf <- data.frame()
#
#   for (ano in anos) {
#
#     erro_rd <- TRUE
#     while (erro_rd) {
#       erro_rd <- tryCatch({
#         # Baixando os dados do SIH-RD para o dado ano e UF
#         df_sih_rd_aux <- fetch_datasus(
#           year_start = ano,
#           year_end = ano,
#           uf = estado,
#           month_start = 1,
#           month_end = 6,
#           information_system = "SIH-RD",
#           timeout = 500,
#           stop_on_error = TRUE,
#           vars = c(
#             "CNES", "CEP", "MUNIC_RES", "MUNIC_MOV", "ANO_CMPT", "COD_IDADE", "IDADE", "NASC",
#             "DT_INTER", "DT_SAIDA", "COBRANCA", "N_AIH", "DIAG_PRINC", "PROC_REA",
#             "US_TOT", "UTI_MES_TO"
#           )
#         )
#
#         # Criando um data.frame que contém apenas as internações de menores de 28 dias
#         df_sih_rd_aux_menores_28 <- df_sih_rd_aux |>
#           mutate(idade_dias = as.numeric(as.Date(DT_INTER, format = "%Y%m%d") - as.Date(NASC, format = "%Y%m%d"))) |>
#           dplyr::filter(
#             idade_dias < 28
#           )
#
#         erro_rd <- FALSE
#       },
#       warning = function(cond) return(TRUE)
#       )
#     }
#
#     # Juntando com os dados dos anos anteriores para a dada UF
#     df_sih_rd_menores_28_uf <- bind_rows(df_sih_rd_menores_28_uf, df_sih_rd_aux_menores_28)
#
#
#     # Limpando a memória
#     rm(df_sih_rd_aux_menores_28
#     )
#     gc()
#   }
#
#   # Salvando as bases da UF
#   write.csv2(
#     df_sih_rd_menores_28_uf,
#     gzfile(glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/{estado}_sih_rd_menores_28_dias_2024.csv.gz")),
#     row.names = FALSE
#   )
#
# }
#
# ## Criando os data.frames que guardarão as bases finais
# df_sih_rd_menores_28 <- data.frame()
#
# for (estado in estados) {
#   df_sih_rd_menores_28_aux <-
#     fread(
#     glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/{estado}_sih_rd_menores_28_dias_2024.csv.gz"),
#     sep = ";"
#   )
#
#   print(paste0("O ESTADO ", estado, " FOI LIDO"))
#   df_sih_rd_menores_28 <- bind_rows(df_sih_rd_menores_28, df_sih_rd_menores_28_aux)
#
#   print(paste0("O ESTADO ", estado, " FOI ADICIONADO À BASE"))
#
#
#   rm(df_sih_rd_menores_28_aux)
#   gc()
# }
#
# ## Salvando as bases completas
# write.csv2(
#   df_sih_rd_menores_28,
#   glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/BR_sih_rd_menores_28_dias_2024.csv"),
#   row.names = FALSE
# )
#
#
# ## Para os numeradores dos indicadores (número de internações/internações em UTI em menores de 28 dias) ----
# ### Lendo uma base com informações auxiliares dos municípios
# df_infos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_aux_municipios.csv") |>
#   mutate_if(is.numeric, as.character)
#
# ### Rodando o algoritmo da Claudia na base completa de internações em menores de 28 dias
# #### Criando um vetor que contém o diretório original do projeto
# diretorio_original <- getwd()
#
# #### Criando um vetor que contém o diretório das bases brutas do SIH-RD
# diretorio_bases_brutas <- glue("{getwd()}/data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias")
#
# #### Mudando o diretório para a pasta que contém o algoritmo em C++
# setwd("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")
#
# #### Rodando o algoritmo em C++ na base de internações
# system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_menores_28_dias_2012-2024.csv"))
#
# #### Voltando para o diretório original do projeto
# setwd(diretorio_original)
#
# #### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
# con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")
#
# #### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
# df_aih_internacoes_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
# dbDisconnect(con)
#
# ### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
# df_aih_internacoes <- left_join(
#   df_aih_internacoes_aux,
#   df_sih_rd_menores_28 |>
#     select(ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV, idade_dias) |>
#     mutate_at(vars(c(DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV)), as.character)
# )
#
# ### Passando os casos para o formato wide (cada linha corresponderá a uma pessoa única)
# df_aih_internacoes_wide <- df_aih_internacoes |>
#   mutate(
#     DT_INTER = as.Date(DT_INTER, format = "%Y%m%d"),
#     DT_SAIDA = as.Date(DT_SAIDA, format = "%Y%m%d"),
#     NASC = as.Date(NASC, format = "%Y%m%d")
#   ) |>
#   group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
#   summarise(
#     ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
#     CNES = first(CNES),  # CNES do estabelecimento da primeira internação
#     MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
#     MUNIC_MOV = first(MUNIC_MOV),  # Município do estabelecimento da primeira internação
#     idade_dias = first(idade_dias),  # Idade, em dias, na data da primeira internação
#     SOMA_UTI = sum(as.integer(UTI_MES_TO)),  # Total de dias na UTI
#     PDIAG = first(DIAG_PRINC),  # Diagnóstico principal da primeira internação
#     PPROC = first(PROC_REA)
#   ) |>
#   ungroup() |>
#   select(ano = ANO_CMPT, codmunres = MUNIC_RES, causabas = PDIAG, codmunocor = MUNIC_MOV, cnes = CNES, aihref = AIHREF, idade_dias, soma_uti_mes_to = SOMA_UTI) |>
#   # Filtrando apenas pelos casos em que os municípios de residência e ocorrência são considerados no painel
#   filter(codmunres %in% df_infos_municipios$codmunres & codmunocor %in% df_infos_municipios$codmunres)
#
# ############## data frame de garbage code
#
# df_internacoes_neonatais_totais <- df_aih_internacoes_wide |>
#   select(codmunres, ano) |>
#   mutate(internacoes_neonatais_totais = 1) |>
#   group_by(across(!internacoes_neonatais_totais)) |>
#   summarise(internacoes_neonatais_totais = sum(internacoes_neonatais_totais)) |>
#   ungroup() |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres)
#
# df_neonatais_morbidade_garbage <- df_aih_internacoes_wide |>
#   filter(causabas %in% df_garbage_codes$causabas) |>
#   select(codmunres, ano, causabas) |>
#   #left_join(df_cid10 |> select(causabas, capitulo_cid10)) |>
#   mutate(internacoes = 1) |>
#   group_by(across(!c(internacoes))) |>
#   summarise(internacoes = sum(internacoes)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = causabas,
#     values_from = internacoes,
#     values_fill = 0
#   ) |>
#   clean_names() |>
#   rename_with(.fn = ~ paste0('garbage_morbidade_neonatal_', .), .cols = -c(ano, codmunres)) |>
#   right_join(df_aux_municipios) |>
#   right_join(df_internacoes_neonatais_totais) |>
#   arrange(codmunres)
#
# # ## Substituindo todos os NAs por 0 (gerados após o right join)
# df_neonatais_morbidade_garbage[is.na(df_neonatais_morbidade_garbage)] <- 0
#
# write.csv(df_neonatais_morbidade_garbage_antigo, gzfile("data-raw/csv/indicadores_bloco8_graficos_garbage_code_morbidade_2012-2024.csv.gz"), row.names = FALSE)
# df_neonatais_morbidade_garbage_antigo <- read.csv("data-raw/csv/indicadores_bloco8_graficos_garbage_code_morbidade_2012-2023.csv")
#
# df_neonatais_morbidade_garbage_novo <- rbind(df_neonatais_morbidade_garbage_antigo, df_neonatais_morbidade_garbage)
#
# write.csv(df_neonatais_morbidade_garbage_novo, gzfile("data-raw/csv/indicadores_bloco8_graficos_garbage_code_morbidade_2012-2024.csv.gz", row.names = FALSE))

