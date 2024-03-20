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
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito)))
  )

## Lendo o dataframe que recebe as CIDs consideradas garbage codes
df_garbage_codes <- read.csv("data-raw/extracao-dos-dados/databases-antigas/df_garbage_codes.csv")

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

## Substituindo todos os NAs por 0 (gerados após o right join)
df_maternos_garbage[is.na(df_maternos_garbage)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_maternos_garbage)

## Para a tabela de garbage codes em óbitos maternos
df_cid10 <- read.csv("data-raw/extracao-dos-dados/databases-antigas/df_cid10_completo.csv")

df_maternos_garbage_tabela_aux <- df_sim_domat |>
  filter(causabas %in% df_garbage_codes$causabas) |>
  select(codmunres, ano, causabas) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  left_join(df_cid10 |> select(causabas, causabas_subcategoria)) |>
  select(!causabas) |>
  pivot_wider(
    names_from = causabas_subcategoria,
    values_from = obitos,
    values_fill = 0
  )

df_maternos_garbage_tabela <- df_maternos_garbage_tabela_aux |>
  right_join(df_aux_municipios |> filter(codmunres %in% df_maternos_garbage_tabela_aux$codmunres)) |>
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) |>
  pivot_longer(
    cols = !c(codmunres, ano),
    names_to = "causabas_subcategoria",
    values_to = "obitos"
  ) |>
  left_join(df_cid10 |> select(capitulo_cid10, grupo_cid10, causabas_subcategoria)) |>
  select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
  arrange(codmunres, ano)

get_dupes(df_maternos_garbage_tabela)

write.csv(df_maternos_garbage_tabela, gzfile("data-raw/csv/garbage_materno_2012_2022.csv.gz"), row.names = FALSE)

## Removendo objetos já utilizados
rm(df_maternos_garbage, df_maternos_totais, df_maternos_garbage_tabela_aux, df_maternos_garbage_tabela)


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
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito)))
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

## Substituindo todos os NAs por 0 (gerados após o right join)
df_fetais_garbage[is.na(df_fetais_garbage)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_garbage)

## Removendo objetos já utilizados
rm(df_fetais_garbage)

## Para a tabela de garbage codes em óbitos fetais
df_garbage_fetais_tabela_aux <- df_sim_dofet |>
  filter(causabas %in% df_garbage_codes$causabas) |>
  select(codmunres, ano, causabas) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  left_join(df_cid10 |> select(causabas, causabas_subcategoria)) |>
  select(!causabas) |>
  pivot_wider(
    names_from = causabas_subcategoria,
    values_from = obitos,
    values_fill = 0
  )

df_garbage_fetais_tabela <- df_garbage_fetais_tabela_aux |>
  right_join(df_aux_municipios |> filter(codmunres %in% df_garbage_fetais_tabela_aux$codmunres)) |>
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) |>
  pivot_longer(
    cols = !c(codmunres, ano),
    names_to = "causabas_subcategoria",
    values_to = "obitos"
  ) |>
  left_join(df_cid10 |> select(capitulo_cid10, grupo_cid10, causabas_subcategoria)) |>
  select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
  arrange(codmunres, ano)

get_dupes(df_garbage_fetais_tabela)

write.csv(df_garbage_fetais_tabela, gzfile("data-raw/csv/garbage_fetal_2012_2022.csv.gz"), row.names = FALSE)

## Removendo objetos já utilizados
rm(df_garbage_fetais_tabela_aux, df_garbage_fetais_tabela)


# Garbage codes para óbitos neonatais -------------------------------------
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

## Criando um data.frame com os óbitos neonatais preenchidos com garbage codes
df_sim_doinf |>
  filter(causabas %in% df_garbage_codes$causabas) |>
  select(codmunres, ano, causabas) |>
  left_join(df_cid10 |> select(causabas, capitulo_cid10)) |>
  pull(capitulo_cid10) |>
  unique()

df_neonatais_garbage <- df_sim_doinf |>
  filter(causabas %in% df_garbage_codes$causabas) |>
  select(codmunres, ano, causabas) |>
  left_join(df_cid10 |> select(causabas, capitulo_cid10)) |>
  mutate(obitos = 1) |>
  group_by(across(!c(causabas, obitos))) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = capitulo_cid10,
    values_from = obitos,
    values_fill = 0
  ) |>
  clean_names() |>
  rename_with(.fn = ~ paste0('garbage_neonatal_', .), .cols = -c(ano, codmunres)) |>
  right_join(df_aux_municipios) |>
  right_join(df_neonatais_totais) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_garbage[is.na(df_neonatais_garbage)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_garbage)

## Removendo objetos já utilizados
rm(df_neonatais_garbage)

## Para a tabela de garbage codes em óbitos neonatais
# df_garbage_neonatais_tabela_aux <- df_sim_doinf |>
#   filter(causabas %in% df_garbage_codes$causabas) |>
#   mutate(
#     idade = as.numeric(idade),
#     peso = as.numeric(peso),
#     faixa_de_peso = #factor(
#       case_when(
#         is.na(peso) ~ "Sem informação",
#         peso < 1500 ~ "< 1500 g",
#         peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
#         peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
#         peso >= 2500 ~ "\U2265 2500 g"
#       ),
#       #levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
#     #),
#     faixa_de_idade = #factor(
#       case_when(
#         idade <= 206 ~ "0 a 6 dias",
#         idade > 206 & idade <= 227 ~ "7 a 27 dias"
#       ),
#       #levels = c("0 a 6 dias", "7 a 27 dias")
#     #),
#     obitos = 1
#   ) |>
#   select(codmunres, ano, causabas, faixa_de_peso, faixa_de_idade, obitos) |>
#   group_by(across(!obitos)) |>
#   summarise(obitos = sum(obitos)) |>
#   ungroup() |>
#   filter(!is.na(causabas)) |>
#   left_join(df_cid10 |> select(causabas, causabas_subcategoria)) |>
#   select(!causabas) |>
#   pivot_wider(
#     names_from = causabas_subcategoria,
#     values_from = obitos,
#     values_fill = 0
#   )
#
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

df_garbage_neonatais_tabela <- df_sim_doinf |>
  filter(causabas %in% df_garbage_codes$causabas, codmunres %in% codigos_municipios) |>
  left_join(df_cid10) |>
  mutate(
    idade = as.numeric(idade),
    peso = as.numeric(peso),
    faixa_de_peso = factor(
      case_when(
        is.na(peso) ~ "Sem informação",
        peso < 1500 ~ "< 1500 g",
        peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
        peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
        peso >= 2500 ~ "\U2265 2500 g"
      ),
      levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
    ),
    faixa_de_idade = factor(
      case_when(
        idade <= 206 ~ "0 a 6 dias",
        idade > 206 & idade <= 227 ~ "7 a 27 dias"
      ),
      levels = c("0 a 6 dias", "7 a 27 dias")
    ),
    obitos = 1
  ) |>
  select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  arrange(codmunres, ano)

get_dupes(df_garbage_neonatais_tabela)

write.csv(df_garbage_neonatais_tabela, gzfile("data-raw/csv/garbage_neonatal_2012_2022.csv.gz"), row.names = FALSE)

## Removendo objetos já utilizados
rm(df_garbage_neonatais_tabela)


# Causas principais para óbitos fetais -------------------------------------------------------
df_principais_fetal <- df_sim_dofet |>
  mutate(
    causabas = substr(causabas, 1, 3),
    grupo_cid = case_when(
      causabas >= "P00" & causabas <= "P04" ~ "principais_fetal_p00_p04",
      causabas >= "P05" & causabas <= "P08" ~ "principais_fetal_p05_p08",
      causabas >= "P10" & causabas <= "P15" ~ "principais_fetal_p10_p15",
      causabas >= "P20" & causabas <= "P29" ~ "principais_fetal_p20_p29",
      causabas >= "P35" & causabas <= "P39" ~ "principais_fetal_p35_p39",
      causabas >= "P50" & causabas <= "P61" ~ "principais_fetal_p50_p61",
      causabas >= "P70" & causabas <= "P74" ~ "principais_fetal_p70_p74",
      causabas >= "P75" & causabas <= "P78" ~ "principais_fetal_p75_p78",
      causabas >= "P80" & causabas <= "P83" ~ "principais_fetal_p80_p83",
      causabas >= "P90" & causabas <= "P96" ~ "principais_fetal_p90_p96",
      causabas >= "Q00" & causabas <= "Q99" ~ "principais_fetal_q00_q99",
      causabas >= "J00" & causabas <= "J99" ~ "principais_fetal_j00_j99",
      causabas >= "A00" & causabas <= "A99" |
        causabas >= "B00" & causabas <= "B99" ~ "principais_fetal_a00_b99"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "principais_fetal_outros", grupo_cid)
  ) |>
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
df_principais_fetal[is.na(df_principais_fetal)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_principais_fetal)

## Removendo objetos já utilizados
rm(df_principais_fetal)

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

df_principais_fetais_tabela <- df_sim_dofet |>
  filter(codmunres %in% codigos_municipios) |>
  left_join(df_cid10) |>
  mutate(
    causabas = substr(causabas, 1, 3),
    grupo_cid10 = case_when(
      causabas >= "Q00" & causabas <= "Q99" ~ "(Q00-Q99) Anomalias congênitas",
      causabas >= "J00" & causabas <= "J99" ~ "(J00-J99) Respiratórias",
      causabas >= "A00" & causabas <= "A99" |
        causabas >= "B00" & causabas <= "B99" ~ "(A00-B99) Infecciosas",
      causabas >= "P00" & causabas <= "P96" ~ grupo_cid10,
      TRUE ~ "Outros"
    ),
    obitos = 1
  ) |>
  select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  arrange(codmunres, ano)

get_dupes(df_principais_fetais_tabela)

write.csv(df_principais_fetais_tabela, gzfile("data-raw/csv/principais_fetal_2012_2022.csv.gz"), row.names = FALSE)

## Removendo objetos já utilizados
rm(df_principais_fetais_tabela)


# Causas principais para óbitos neonatais ---------------------------------
df_principais_neonatal <- df_sim_doinf |>
  mutate(
    causabas = substr(causabas, 1, 3),
    grupo_cid = case_when(
      causabas >= "P00" & causabas <= "P04" ~ "principais_neonatal_p00_p04",
      causabas >= "P05" & causabas <= "P08" ~ "principais_neonatal_p05_p08",
      causabas >= "P10" & causabas <= "P15" ~ "principais_neonatal_p10_p15",
      causabas >= "P20" & causabas <= "P29" ~ "principais_neonatal_p20_p29",
      causabas >= "P35" & causabas <= "P39" ~ "principais_neonatal_p35_p39",
      causabas >= "P50" & causabas <= "P61" ~ "principais_neonatal_p50_p61",
      causabas >= "P70" & causabas <= "P74" ~ "principais_neonatal_p70_p74",
      causabas >= "P75" & causabas <= "P78" ~ "principais_neonatal_p75_p78",
      causabas >= "P80" & causabas <= "P83" ~ "principais_neonatal_p80_p83",
      causabas >= "P90" & causabas <= "P96" ~ "principais_neonatal_p90_p96",
      causabas >= "Q00" & causabas <= "Q99" ~ "principais_neonatal_q00_q99",
      causabas >= "J00" & causabas <= "J99" ~ "principais_neonatal_j00_j99",
      causabas >= "A00" & causabas <= "A99" |
        causabas >= "B00" & causabas <= "B99" ~ "principais_neonatal_a00_b99"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "principais_neonatal_outros", grupo_cid)
  ) |>
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
df_principais_neonatal[is.na(df_principais_neonatal)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_principais_neonatal)

## Removendo objetos já utilizados
rm(df_principais_neonatal)

## Para a tabela de causas principais em óbitos neonatais
df_principais_neonatais_tabela <- df_sim_doinf |>
  filter(codmunres %in% codigos_municipios) |>
  left_join(df_cid10) |>
  mutate(
    idade = as.numeric(idade),
    peso = as.numeric(peso),
    faixa_de_peso = factor(
      case_when(
        is.na(peso) ~ "Sem informação",
        peso < 1500 ~ "< 1500 g",
        peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
        peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
        peso >= 2500 ~ "\U2265 2500 g"
      ),
      levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
    ),
    faixa_de_idade = factor(
      case_when(
        idade <= 206 ~ "0 a 6 dias",
        idade > 206 & idade <= 227 ~ "7 a 27 dias"
      ),
      levels = c("0 a 6 dias", "7 a 27 dias")
    ),
    causabas = substr(causabas, 1, 3),
    grupo_cid10 = case_when(
      causabas >= "Q00" & causabas <= "Q99" ~ "(Q00-Q99) Anomalias congênitas",
      causabas >= "J00" & causabas <= "J99" ~ "(J00-J99) Respiratórias",
      causabas >= "A00" & causabas <= "A99" |
        causabas >= "B00" & causabas <= "B99" ~ "(A00-B99) Infecciosas",
      causabas >= "P00" & causabas <= "P96" ~ grupo_cid10,
      TRUE ~ "Outros"
    ),
    obitos = 1
  ) |>
  select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  arrange(codmunres, ano)

get_dupes(df_principais_neonatais_tabela)

write.csv(df_principais_neonatais_tabela, gzfile("data-raw/csv/principais_neonatal_2012_2022.csv.gz"), row.names = FALSE)

## Removendo objetos já utilizados
rm(df_principais_neonatais_tabela)


# Causas evitáveis --------------------------------------------------------
## Criando um vetor com as cids
imunoprevencao <- c(
    "A17", "A19", "A33", "A35", "A36", "A37", "A80", "B05", "B06",
    "B16", "B260", "G000", "P350", "P353"
  )

mulher_gestacao <- c(
    "A50", sprintf("B2%d", 0:4), "P022", "P023", "P027", "P028",
    "P029", "P00", "P04", "P01", "P05", "P07", "P220", "P26",
    "P52", "P550", "P551", "P558", "P559", "P56", "P57", "P77"
  )

evitaveis_parto <- c(
    "P020", "P021", "P024", "P025", "P026", "P03", "P08", sprintf("P1%d", 0:5),
    "P20", "P21", "P24"
  )

recem_nascido <- c(
    "P221", "P228", "P229", "P23", "P25", "P27", "P28",
    sprintf("P3%d", 51:53), sprintf("P3%d", 58:59), sprintf("P3%d", 6:9), sprintf("P5%d", 0:1), sprintf("P5%d", 3:4), "P58", "P59",
    sprintf("P7%d", 0:4), "P60", "P61",  sprintf("P7%d", 5:6), "P78",
    sprintf("P8%d", 0:3),  sprintf("P9%d", 0:4),
    sprintf("P9%d", 60:68)
  )

tratamento <- c(
    "A15", "A16", "A18", sprintf("G0%d", 0:4), sprintf("J0%d", 0:6),
    sprintf("J1%d", 2:8), sprintf("J1%d", 2:8), sprintf("J2%d", 0:2),
    "J384", sprintf("J4%d", 0:2), sprintf("J4%d", 5:7), sprintf("J6%d", 8:9),
    sprintf("A7%d", 0:4), "A30", "A31", "A32", "A38", "A39", "A40", "A41",
    "A46", "A49", "E030", "E031", sprintf("E1%d", 0:4), "E700", "E730",
    "G40", "G41", "Q90", "N390", sprintf("I0%d", 0:9)
  )

saude <- c(
    sprintf("A0%d", 0:9), sprintf("A2%d", 0:8), sprintf("A9%d", 0:9),
    sprintf("A7%d", 5:9), "A82", sprintf("B5%d", 0:9), sprintf("B6%d", 0:4),
    sprintf("B6%d", 5:9), sprintf("B7%d", 0:9), sprintf("B8%d", 0:3),
    "B99", sprintf("D5%d", 0:3), sprintf("E4%d", 0:9), sprintf("E5%d", 0:9),
    sprintf("E6%d", 0:4), "E86", c(sprintf("V%02d", 1:99)), sprintf("X4%d", 0:4),
    sprintf("X4%d", 5:9), "R95", c(sprintf("W%02d", 0:19)), sprintf("X0%d", 0:9),
    sprintf("X3%d", 0:9), c(sprintf("W%02d", 65:74)), c(sprintf("W%02d", 75:84)),
    c(sprintf("W%02d", 85:99)), c(sprintf("X%02d", 85:99)),
    c(sprintf("Y%02d", 00:09)), c(sprintf("Y%02d", 10:34)), c(sprintf("W%02d", 20:49)),
    c(sprintf("Y%02d", 60:69)), c(sprintf("Y%02d", 83:84)), c(sprintf("Y%02d", 40:59))
  )

mal_definidas <- c(
    c(sprintf("R%02d", 00:94)), c(sprintf("R%02d", 96:99)),
    "P95", "P969"
  )


## Causas evitáveis para óbitos fetais ------------------------------------
df_evitaveis_fetal <- df_sim_dofet |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao ~ "evitaveis_fetal_imunoprevencao",
      causabas %in% mulher_gestacao ~ "evitaveis_fetal_mulher_gestacao",
      causabas %in% evitaveis_parto ~ "evitaveis_fetal_parto",
      causabas %in% recem_nascido ~ "evitaveis_fetal_recem_nascido",
      causabas %in% tratamento ~ "evitaveis_fetal_tratamento",
      causabas %in% saude ~ "evitaveis_fetal_saude",
      causabas %in% mal_definidas ~ "evitaveis_fetal_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_outros", grupo_cid)
  ) |>
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
df_evitaveis_fetal[is.na(df_evitaveis_fetal)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_evitaveis_fetal)

## Para a tabela de causas evitáveis em óbitos fetais
df_evitaveis_fetal_tabela <- df_sim_dofet |>
  filter(codmunres %in% codigos_municipios) |>
  left_join(df_cid10) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao ~ "Reduzível pelas ações de imunização",
      causabas %in% mulher_gestacao ~ "Reduzíveis por adequada atenção à mulher na gestação",
      causabas %in% evitaveis_parto ~ "Reduzíveis por adequada atenção à mulher no parto",
      causabas %in% recem_nascido ~ "Reduzíveis por adequada atenção ao recém-nascido",
      causabas %in% tratamento ~ "Reduzíveis por ações de diagnóstico e tratamento adequado",
      causabas %in% saude ~ "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção",
      causabas %in% mal_definidas ~ "Causas mal definidas",
      TRUE ~ "Demais causas (não claramente evitáveis)"
    ),
    obitos = 1
  ) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  select(codmunres, ano, capitulo_cid10, grupo_cid, causabas_subcategoria, obitos) |>
  arrange(codmunres)

write.csv(df_evitaveis_fetal_tabela, gzfile("data-raw/csv/evitaveis_fetal_2012_2022.csv.gz"), row.names = FALSE)


## Causas evitáveis para óbitos neonatais ------------------------------------------------------
df_neonatais_totais_peso <- df_sim_doinf |>
  mutate(
    faixa_de_peso = case_when(
      as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
      as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
      as.numeric(peso) > 1500 ~ "> 1500 g"
    )
  ) |>
  select(codmunres, ano, faixa_de_peso) |>
  mutate(obitos_neonatais_totais = 1) |>
  group_by(across(!obitos_neonatais_totais)) |>
  summarise(obitos_neonatais_totais = sum(obitos_neonatais_totais)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

df_neonatais_evitaveis <- df_sim_doinf |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao ~ "evitaveis_neonatal_imunoprevencao",
      causabas %in% mulher_gestacao ~ "evitaveis_neonatal_mulher_gestacao",
      causabas %in% evitaveis_parto ~ "evitaveis_neonatal_parto",
      causabas %in% recem_nascido ~ "evitaveis_neonatal_recem_nascido",
      causabas %in% tratamento ~ "evitaveis_neonatal_tratamento",
      causabas %in% saude ~ "evitaveis_neonatal_saude",
      causabas %in% mal_definidas ~ "evitaveis_neonatal_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_outros", grupo_cid),
    faixa_de_peso = case_when(
      as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
      as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
      as.numeric(peso) > 1500 ~ "> 1500 g"
    )
  ) |>
  select(codmunres, ano, grupo_cid, faixa_de_peso) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = obitos,
    values_fill = 0
  ) |>
  right_join(df_neonatais_totais_peso) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres) |>
  mutate(faixa_de_peso = ifelse(is.na(faixa_de_peso), "não se aplica", faixa_de_peso))

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_evitaveis[is.na(df_neonatais_evitaveis)] <- 0

## Para a tabela de causas evitáveis em óbitos neonatais
df_evitaveis_neonatal_tabela <- df_sim_doinf |>
  filter(codmunres %in% codigos_municipios) |>
  left_join(df_cid10) |>
  mutate(
    idade = as.numeric(idade),
    peso = as.numeric(peso),
    faixa_de_peso = factor(
      case_when(
        is.na(peso) ~ "Sem informação",
        peso < 1500 ~ "< 1500 g",
        peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
        peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
        peso >= 2500 ~ "\U2265 2500 g"
      ),
      levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
    ),
    faixa_de_idade = factor(
      case_when(
        idade <= 206 ~ "0 a 6 dias",
        idade > 206 & idade <= 227 ~ "7 a 27 dias"
      ),
      levels = c("0 a 6 dias", "7 a 27 dias")
    ),
    grupo_cid = case_when(
      causabas %in% imunoprevencao ~ "Reduzível pelas ações de imunização",
      causabas %in% mulher_gestacao ~ "Reduzíveis por adequada atenção à mulher na gestação",
      causabas %in% evitaveis_parto ~ "Reduzíveis por adequada atenção à mulher no parto",
      causabas %in% recem_nascido ~ "Reduzíveis por adequada atenção ao recém-nascido",
      causabas %in% tratamento ~ "Reduzíveis por ações de diagnóstico e tratamento adequado",
      causabas %in% saude ~ "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção",
      causabas %in% mal_definidas ~ "Causas mal definidas",
      TRUE ~ "Demais causas (não claramente evitáveis)"
    ),
    obitos = 1
  ) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  select(codmunres, ano, capitulo_cid10, grupo_cid, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
  arrange(codmunres)

write.csv(df_evitaveis_neonatal_tabela, gzfile("data-raw/csv/evitaveis_neonatal_2012_2022.csv.gz"), row.names = FALSE)


# Grupos de causas para óbitos fetais -------------------------------------------------------
## Vetores com as cids de alguns grupos
grupos_prematuridade <- c("P07", "P220", "P25", "P26", "P52", "P77")

grupos_infeccoes <- c("P35", "P36", "P37", "P38", "P39", "A40", "A41", "P23",
                      "J12", "J13", "J14", "J15", "J16", "J17", "J18", "A00", "A01",
                      "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A33",
                      "A50", "B20", "B21", "B22", "B23", "B24", "G00", "G03", "G04")

grupos_asfixia <- c("P017", "P020", "P021", "P024", "P025", "P026", "P03",
                    "P10", "P11", "P12", "P13", "P14", "P15", "P20", "P21", "P24")

grupos_respiratorias <- c("P221", "P228", "P229", "P28")

grupos_gravidez <- c("P00", "P010", "P011", "P012", "P013", "P014", "P015", "P016",
                     "P018", "P019", "P022", "P023", "P027", "P028", "P029", "P04",
                     "P05", "P964")

grupos_cardiorrespiratoria <- c("P221", "P228", "P229", "P28")

grupos_afeccoes_perinatal <- c("P969")

grupos_ma_formacao <- c(paste0("Q", sprintf("%02d", 0:99)))

grupos_mal_definida <- c(paste0("R", sprintf("%02d", 0:99)))


grupos_todas_subcategorias <- c("P017", "P020", "P021", "P024", "P025", "P026", "P221", "P228", "P229",
                                "P00", "P010", "P011", "P012", "P013", "P014", "P015", "P016",
                                "P018", "P019", "P022", "P023", "P027", "P028", "P029", "P964", "P969")


## Para a tabela de grupos de causa em óbitos fetais

df_cid10 <- read.csv("data-raw/extracao-dos-dados/databases-antigas/df_cid10_completo.csv")

df_grupos_fetais_tabela <- df_sim_dofet |>
  filter(codmunres %in% codigos_municipios) |>
  left_join(df_cid10) |>
  mutate(
    causabas_categoria = causabas,
    causabas = substr(causabas, 1, 3),
    grupo_cid10 = case_when(
      causabas %in% grupos_prematuridade | causabas_categoria %in% grupos_prematuridade ~ "Prematuridade",
      causabas %in% grupos_infeccoes | causabas_categoria %in% grupos_infeccoes ~ "Infecções",
      causabas %in% grupos_asfixia | causabas_categoria %in% grupos_asfixia ~ "Asfixia/Hipóxia",
      causabas %in% grupos_respiratorias | causabas_categoria %in% grupos_respiratorias ~ "Afecções respiratórias do recém-nascido",
      causabas %in% grupos_gravidez | causabas_categoria %in% grupos_gravidez ~ "Fatores maternos relacionados à gravidez",
      causabas %in% grupos_cardiorrespiratoria | causabas_categoria %in% grupos_cardiorrespiratoria ~ "Transtornos cardiorrespiratórios originados do período perinatal",
      causabas %in% grupos_afeccoes_perinatal | causabas_categoria %in% grupos_afeccoes_perinatal ~ "Afecções ariginais no período perinatal",
      causabas %in% grupos_ma_formacao ~ "Má formação congênita",
      causabas %in% grupos_mal_definida ~ "Causas mal definidas",
      TRUE ~ "Outras causas"
    ),
    obitos = 1
  ) |>
  select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, obitos) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  arrange(codmunres, ano)

get_dupes(df_grupos_fetais_tabela)

write.csv(df_grupos_fetais_tabela, gzfile("data-raw/csv/grupos_fetal_2012_2022.csv.gz"), row.names = FALSE)

# dados organizados para os gráficos de grupos de causa fetal

df_fetais_grupos <- df_sim_dofet |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade ~ "fetal_grupos_prematuridade",
      causabas %in% grupos_infeccoes ~ "fetal_grupos_infeccoes",
      causabas %in% grupos_asfixia ~ "fetal_grupos_asfixia",
      causabas %in% grupos_respiratorias ~ "fetal_grupos_respiratorias",
      causabas %in% grupos_gravidez ~ "fetal_grupos_gravidez",
      causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal ~ "fetal_grupos_afeccoes_perinatal",
      causabas %in% grupos_ma_formacao ~ "fetal_grupos_ma_formacao",
      causabas %in% grupos_mal_definida ~ "fetal_grupos_mal_definida",
      TRUE ~ "fetal_grupos_outros"
    )
  ) |>
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
df_fetais_grupos[is.na(df_fetais_grupos)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_fetais_grupos)


# Grupos de causas para óbitos neonatais -------------------------------------------------------


#tabela grupos de causa para óbitos neonatais

df_grupos_neonatais_tabela <- df_sim_doinf |>
  filter(codmunres %in% codigos_municipios) |>
  left_join(df_cid10) |>
  mutate(
    idade = as.numeric(idade),
    peso = as.numeric(peso),
    faixa_de_peso = factor(
      case_when(
        is.na(peso) ~ "Sem informação",
        peso < 1500 ~ "< 1500 g",
        peso >= 1500 & peso < 2000 ~ "1500 a 1999 g",
        peso >= 2000 & peso < 2500 ~ "2000 a 2499 g",
        peso >= 2500 ~ "\U2265 2500 g"
      ),
      levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")
    ),
    faixa_de_idade = factor(
      case_when(
        idade <= 206 ~ "0 a 6 dias",
        idade > 206 & idade <= 227 ~ "7 a 27 dias"
      ),
      levels = c("0 a 6 dias", "7 a 27 dias")
    ),
    causabas_categoria = causabas,
    causabas = substr(causabas, 1, 3),
    grupo_cid10 = case_when(
      causabas %in% grupos_prematuridade | causabas_categoria %in% grupos_prematuridade ~ "Prematuridade",
      causabas %in% grupos_infeccoes | causabas_categoria %in% grupos_infeccoes ~ "Infecções",
      causabas %in% grupos_asfixia | causabas_categoria %in% grupos_asfixia ~ "Asfixia/Hipóxia",
      causabas %in% grupos_respiratorias | causabas_categoria %in% grupos_respiratorias ~ "Afecções respiratórias do recém-nascido",
      causabas %in% grupos_gravidez | causabas_categoria %in% grupos_gravidez ~ "Fatores maternos relacionados à gravidez",
      causabas %in% grupos_cardiorrespiratoria | causabas_categoria %in% grupos_cardiorrespiratoria ~ "Transtornos cardiorrespiratórios originados do período perinatal",
      causabas %in% grupos_afeccoes_perinatal | causabas_categoria %in% grupos_afeccoes_perinatal ~ "Afecções ariginais no período perinatal",
      causabas %in% grupos_ma_formacao ~ "Má formação congênita",
      causabas %in% grupos_mal_definida ~ "Causas mal definidas",
      TRUE ~ "Outras causas"
    ),
    obitos = 1
  ) |>
  select(codmunres, ano, capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_peso, faixa_de_idade, obitos) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  arrange(codmunres, ano)

get_dupes(df_grupos_neonatais_tabela)

write.csv(df_grupos_neonatais_tabela, gzfile("data-raw/csv/grupos_neonatal_2012_2022.csv.gz"), row.names = FALSE)


#gráfico grupos de causa para óbitos neonatais

df_neonatais_grupos <- df_sim_doinf |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade ~ "neonat_grupos_prematuridade",
      causabas %in% grupos_infeccoes ~ "neonat_grupos_infeccoes",
      causabas %in% grupos_asfixia ~ "neonat_grupos_asfixia",
      causabas %in% grupos_respiratorias ~ "neonat_grupos_respiratorias",
      causabas %in% grupos_gravidez ~ "neonat_grupos_gravidez",
      causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal ~ "neonat_grupos_afeccoes_perinatal",
      causabas >= "Q00" & causabas <= "Q99" ~ "neonat_grupos_ma_formacao",
      causabas >= "R00" & causabas <= "R99" ~ "neonat_grupos_mal_definida",
      TRUE ~ "neonat_grupos_outros"
    )
  ) |>
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
df_neonatais_grupos[is.na(df_neonatais_grupos)] <- 0

## Juntando com o restante da base do bloco 8
df_bloco8_graficos <- left_join(df_bloco8_graficos, df_neonatais_grupos)

# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco8_graficos, "data-raw/csv/indicadores_bloco8_graficos_2012-2022.csv", row.names = FALSE)
write.csv(df_neonatais_evitaveis, "data-raw/csv/indicadores_bloco8_grafico_evitaveis_neonatal_2012-2022.csv", row.names = FALSE)


