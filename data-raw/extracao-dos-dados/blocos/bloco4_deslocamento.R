library(microdatasus)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)

#### Baixando os dados do sinasc de 2012-2022

anos <- c(2012, 2014:2023)
dados <- data.frame()

for (i in anos){

  df <- fetch_datasus(
    year_start = i,
    year_end = i,
    information_system = "SINASC",
    vars = c("CODMUNRES", "CODMUNNASC", 'CODESTAB', "LOCNASC",
             "PARTO", "IDADEMAE", "ESCMAE", "RACACORMAE",
             "TPROBSON", "PESO")
  )

  df <- df |> mutate(ANO = i)
  # data.table::fwrite(df, paste0("data-raw/csv/sinasc_", i, ".csv"))
  dados <- rbind(dados, df)
}

rm(df,anos,i)

## Não há a variável TPROBSON para 2013

df13 <- fetch_datasus(
  year_start = 2013,
  year_end = 2013,
  information_system = "SINASC",
  vars = c("CODMUNRES", "CODMUNNASC", 'CODESTAB', "LOCNASC",
           "PARTO", "IDADEMAE", "ESCMAE", "RACACORMAE", "PESO")
) |> mutate(ANO = 2013)

df13$TPROBSON <- rep("NA", nrow(df13))
# data.table::fwrite(df13, paste0("data-raw/csv/sinasc_2013.csv"))

## Juntando os dados do sinasc de 2013 aos dados dos outros anos

df_sinasc_consolidados <- rbind(dados, df13)
rm(df13)

#### Baixando os dados preliminares do sinasc 2023

options(timeout=99999)

#### Baixando os dados res do sinasc 2024

df24 <- data.table::fread(
  "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2024.csv",
  sep= ";") |>
  select("CODMUNRES", "CODMUNNASC", 'CODESTAB', "LOCNASC",
         "PARTO", "IDADEMAE", "ESCMAE", "RACACORMAE",
         "TPROBSON", "PESO") |> mutate(ANO = 2024)


#### Juntando os preliminares aos consolidados

df_sinasc_consolidados <- rbind(df_sinasc_consolidados, df24)

data.table::fwrite(df_sinasc_consolidados, "data-raw/csv/sinasc_2012_2024.csv",
                   row.names = FALSE)

#### Baixando dados do cnes leitos

anos <- c(2012:2024)
cnes_leitos <- data.frame()

for (i in anos){

  df <- fetch_datasus(
    year_start = i,
    year_end = i,
    month_start = 1,
    month_end = 12,
    information_system = "CNES-LT")
  df <- df |> mutate(ano = i)

  # data.table::fwrite(df, paste0("data-raw/csv/cnes_leitos_", i, ".csv"))
  cnes_leitos <- rbind(cnes_leitos, df)
}
rm(df,anos,i)

data.table::fwrite(cnes_leitos, "data-raw/csv/cnes_leitos_2012_2024.csv",
                   row.names = FALSE)


################################################################################
#### INDICADOR DESLOCAMENTO PESO < 1500g
################################################################################

rm(list = ls())

sinasc <- data.table::fread("data-raw/csv/sinasc_2012_2024.csv") |>
  filter(PESO < 1500) |>
  select(CODMUNRES, CODMUNNASC, ANO, CODESTAB, PESO) |>
  janitor::clean_names()

cnes <- data.table::fread("data-raw/csv/cnes_leitos_2012_2024.csv") |>
  janitor::clean_names() |>
  rename(codestab = cnes)

# criando mes e ano -------------------------------------------------------

cnes[, c("ano", "mes") := list(substr(competen, 1, 4), substr(competen, 5, 6))]
cnes$ano <- as.numeric(cnes$ano)

# realizando filtros para o indicador de leitos neonatal ------------------
cod_neonat <- c(80, 81, 82)

cnes1 <- cnes |>
  mutate(codleito = as.numeric(codleito)) |>
  mutate(leito_neonat = ifelse(codleito %in% cod_neonat, 1, 0)) |>
  group_by(codestab, codufmun, ano, competen) |>
  summarise(num_leitos_neonat = sum(qt_exist[leito_neonat == 1])) |>
  ungroup() |>
  group_by(codestab, codufmun, ano) |>
  summarise(num_leitos_neonat = mean(num_leitos_neonat)) |>
  select(codestab, codufmun, ano, num_leitos_neonat) |>
  ungroup()

rm(cnes)

# unindo sinasc e cnes ----------------------------------------------------

cnes1$ano <- as.numeric(cnes1$ano)

sinasc <- sinasc |>
  group_by(codmunres, codestab, codmunnasc, ano) |>
  summarise(nascimentos = n()) |>
  drop_na() |>
  ungroup()

sinasc_cnes <- left_join(sinasc, cnes1, by =
                           c("codestab", "ano", "codmunnasc" = "codufmun")) |>
  mutate(num_leitos_neonat = ifelse(is.na(num_leitos_neonat), NA, num_leitos_neonat))

rm(sinasc,cnes1)


# unindo com macro ---------------------------------------------------------

mun_res <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  janitor::clean_names() |>
  rename(macro_r_saude_res = macro_r_saude) |>
  select(codmunres, macro_r_saude_res)

mun_nasc <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  janitor::clean_names() |>
  rename(codmunnasc = codmunres,
         macro_r_saude_nasc = macro_r_saude) |>
  select(codmunnasc, macro_r_saude_nasc)


# temos poucos estabelecimentos sem as informações de macro
# é possível que sejam os mesmos que ficam de fora no painel, pois a base
# auxiliar de municipios é a do painel, e realmente alguns muni ficam de fora.

sinasc_cnes_macro <- inner_join(sinasc_cnes, mun_res, by = "codmunres") |>
  inner_join(mun_nasc, by = "codmunnasc")

rm(sinasc_cnes,mun_nasc,mun_res)

# criando indicadores -----------------------------------------------------

dados <- sinasc_cnes_macro |>
  mutate(partos_na_macro_com_1mais_uti = if_else(macro_r_saude_res == macro_r_saude_nasc &
                                                   num_leitos_neonat >= 1, nascimentos, 0, missing = 0),
         partos_na_macro_com_4mais_uti = if_else(macro_r_saude_res == macro_r_saude_nasc &
                                                   num_leitos_neonat >= 4, nascimentos, 0, missing = 0),
         partos_na_macro_sem_1mais_uti = if_else(macro_r_saude_res == macro_r_saude_nasc &
                                                   num_leitos_neonat < 1, nascimentos, 0, missing = 0),
         partos_na_macro_sem_4mais_uti = if_else(macro_r_saude_res == macro_r_saude_nasc &
                                                   num_leitos_neonat < 4, nascimentos, 0, missing = 0),
         partos_fora_macro_com_1mais_uti = if_else(macro_r_saude_res != macro_r_saude_nasc &
                                                     num_leitos_neonat >= 1, nascimentos, 0, missing = 0),
         partos_fora_macro_com_4mais_uti = if_else(macro_r_saude_res != macro_r_saude_nasc &
                                                     num_leitos_neonat >= 4, nascimentos, 0, missing = 0),
         partos_fora_macro_sem_1mais_uti = if_else(macro_r_saude_res != macro_r_saude_nasc &
                                                     num_leitos_neonat < 1, nascimentos, 0, missing = 0),
         partos_fora_macro_sem_4mais_uti = if_else(macro_r_saude_res != macro_r_saude_nasc &
                                                     num_leitos_neonat < 4, nascimentos, 0, missing = 0),
         partos_na_macro_sem_inf = if_else(macro_r_saude_res == macro_r_saude_nasc &
                                             is.na(num_leitos_neonat), nascimentos, 0),
         partos_fora_macro_sem_inf = if_else(macro_r_saude_res != macro_r_saude_nasc &
                                               is.na(num_leitos_neonat), nascimentos, 0)

  ) |>
  group_by(codmunres, ano) |>
  summarise(
    nascimentos = sum(nascimentos),
    partos_na_macro_com_1mais_uti = sum(partos_na_macro_com_1mais_uti),
    partos_na_macro_com_4mais_uti = sum(partos_na_macro_com_4mais_uti),
    partos_na_macro_sem_1mais_uti = sum(partos_na_macro_sem_1mais_uti),
    partos_na_macro_sem_4mais_uti = sum(partos_na_macro_sem_4mais_uti),
    partos_fora_macro_com_1mais_uti = sum(partos_fora_macro_com_1mais_uti),
    partos_fora_macro_com_4mais_uti = sum(partos_fora_macro_com_4mais_uti),
    partos_fora_macro_sem_1mais_uti = sum(partos_fora_macro_sem_1mais_uti),
    partos_fora_macro_sem_4mais_uti = sum(partos_fora_macro_sem_4mais_uti),
    partos_na_macro_sem_inf = sum(partos_na_macro_sem_inf),
    partos_fora_macro_sem_inf = sum(partos_fora_macro_sem_inf)
  ) |>
  ungroup()

## Dados municipios

mun <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  janitor::clean_names() |>
  select(codmunres)

anos <- data.frame(ano = c(2012:2024))

mun_anos <- cross_join(mun, anos)

rm(mun, anos)

## Juntandos dados

dados1 <- left_join(mun_anos, dados)

## Substituindo valores NA por zero

dados1[is.na(dados1)] <- 0


write.csv(dados1, "data-raw/csv/indicador_deslocamento_1500_2012_2024.csv",
          row.names = FALSE)

rm(dados, dados1, sinasc_cnes_macro, cod_neonat, mun_anos)

################################################################################
#### INDICADOR DESLOCAMENTO MEDIANA E NASCIDOS
################################################################################

rm(list = ls())

# Carregando e tratando o cnes

cnes_leitos <- data.table::fread("data-raw/csv/cnes_leitos_2012_2024.csv")

cnes_leitos <- cnes_leitos |>
  mutate(CODLEITO = as.numeric(CODLEITO))|>
  filter((CODLEITO == 74 | CODLEITO == 75 | CODLEITO == 76)) |>
  group_by(CNES, ano) |>
  summarise(LT_UTI_TOTAL = mean(QT_EXIST)) |>
  select(CODESTAB = CNES, LT_UTI_TOTAL, ANO = ano)

# Carregando e tratando o sinasc

dados <- data.table::fread("data-raw/csv/sinasc_2012_2024.csv")

# Tratando os níveis

dados$LOCNASC <- factor(dados$LOCNASC,
                        levels = c(1,2,3,4,5,9),
                        labels = c("Hospital",
                                   "Outro estab saude",
                                   "Domicílio",
                                   "Outros",
                                   "Aldeia indígena",
                                   "Ignorado"))

dados$PARTO <- factor(dados$PARTO,
                      levels = c(1,2,9),
                      labels = c("Vaginal",
                                 "Cesáreo",
                                 "Ignorado"))

dados$RACACORMAE <- factor(dados$RACACORMAE,
                           levels = c(1,2,3,4,5,9),
                           labels = c("Branca",
                                      "Preta",
                                      "Amarela",
                                      "Parda",
                                      "Indígena",
                                      "Ignorado"))
dados$ESCMAE <- factor(dados$ESCMAE,
                       levels = c(0,1,2,3,4,5,9),
                       labels = c("Ign", "Nenhuma",
                                  "01-03", "04-07",
                                  "08-11", "12 e+",
                                  "Ign"))

dados$TPROBSONac <- factor(dados$TPROBSON,
                           levels = c("01", "02", "03",
                                      "04", "05", "06-09",
                                      "10", "11", "12", NA))

dados$UFr <- floor(as.numeric(dados$CODMUNRES)/10000)
dados$UFn <- floor(as.numeric(dados$CODMUNNASC)/10000)
dados$TPROBSON <- factor(dados$TPROBSON)

dados$CODMUNRES <- as.numeric(dados$CODMUNRES)
dados$CODMUNNASC <- as.numeric(dados$CODMUNNASC)

# # Informações dos municípios, macro e micro de nascimento
# regioes_nasc <- data.table::fread("databases/rmr_nasc.csv")
# # Informações dos municípios, macro e micro de residência
# regioes_res <- data.table::fread("databases/rmr_res.csv")

# Informações dos municípios, macro e micro de residência
regioes_res <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  janitor::clean_names() |>
  dplyr::select(CODMUNRES = codmunres,
                MUNRES = municipio,
                CODRSRES = cod_r_saude,
                RSRES = r_saude,
                CODMRRES = cod_macro_r_saude,
                MRRES = macro_r_saude)

# Informações dos municípios, macro e micro de nascimento
regioes_nasc <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  janitor::clean_names() |>
  dplyr::select(CODMUNNASC = codmunres,
                MUNNASC = municipio,
                CODRSNASC = cod_r_saude,
                RSNASC = r_saude,
                CODMRNASC = cod_macro_r_saude,
                MRNASC = macro_r_saude)


# Matriz de distâncias entre os municípios, disponivel em:
# https://drive.google.com/uc?export=download&id=1VLQZBOccbbvIrC33WHFrZI6_H_gGr4ZK

dist_mun <- data.table::fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/matriz_distancias.csv")

# Juntando as bases
dados2 <- dplyr::left_join(dados, regioes_res, by = c("CODMUNRES"))
dados3 <- dplyr::left_join(dados2, regioes_nasc, by = c("CODMUNNASC"))
rm(dados2,regioes_res,regioes_nasc)

# Tratando a matriz de distâncias
dist_mun$CODMUNRES <- as.numeric(dist_mun$origem)
dist_mun$CODMUNNASC <- as.numeric(dist_mun$destino)

dist_mun <- dist_mun |>
  dplyr::select(c(distancia, CODMUNRES, CODMUNNASC))

# Juntando as bases
dados4 <- dplyr::left_join(dados3, dist_mun, by=c("CODMUNRES", "CODMUNNASC"))
rm(dados3, dist_mun)

# Juntando os dados
dados5 <- dplyr::left_join(dados4, cnes_leitos, by = c("CODESTAB", "ANO"))
rm(dados4, cnes_leitos)

dados5$complexidade <- "baixa"
dados5$complexidade[dados5$LT_UTI_TOTAL >= 1] <- "alta"
dados5$complexidade <- factor(dados5$complexidade)

##################### CÁLCULO DAS MEDIANAS E NASCIDOS PARA MUNICÍPIO ###########

#Criando classificação pelo município de nascimento em relação ao de residência

# Total de nascidos vivos com base no município de residência

nascidos_res <- dados |>
  select(c(ANO, CODMUNRES)) |>
  mutate(
    nascidos = 1,
    ano = as.numeric(ANO),
    codmunres = as.numeric(CODMUNRES)
  )|> group_by(codmunres, ano) |>
  summarise(
    destino_total = sum(nascidos)
  )

dados5_mun <- dados5 |>
  mutate(
    ano = as.numeric(ANO),
    codmunres = as.numeric(CODMUNRES),
    dest = case_when(

      is.na(CODMUNNASC) ~ NA_character_,

      CODMUNRES == CODMUNNASC ~ "local",

      (CODMUNRES != CODMUNNASC) &
        (CODRSRES == CODRSNASC) ~ "dentro_regiao_saude",

      (CODMRRES == CODMRNASC) &
        (CODMUNRES != CODMUNNASC) &
        (CODRSRES != CODRSNASC) ~  "dentro_macrorregiao_saude",

      (CODMRRES != CODMRNASC) &
        (CODMUNRES != CODMUNNASC) &
        (CODRSRES != CODRSNASC) &
        UFr == UFn ~ "fora_macrorregiao_saude",

      UFn != UFr ~ "outra_uf",

      TRUE ~ NA_character_
    )
  ) |>
  select(
    c(ano, codmunres, dest, distancia, complexidade)
  )

# Calculando número de nascidos e mediana

dados6 <- dados5_mun |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, codmunres, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

dados6_alta <- dados5_mun |>
  filter(
    complexidade == "alta"
  ) |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, codmunres, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

dados6_baixa <- dados5_mun |>
  filter(
    complexidade == "baixa"
  ) |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, codmunres, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

# Data frame para número de nascidos por classificação

dados_nascidos <- dados6 |>
  pivot_wider(id_cols = c(ano, codmunres), names_from = dest, values_from = nascidos_dest) |>
  select(c(ano, codmunres, local, outra_uf, dentro_macrorregiao_saude, dentro_regiao_saude,
           fora_macrorregiao_saude))

#Data frame para mediana de deslocamento por classificação
dados_mediana <- dados6 |>
  pivot_wider(id_cols = c(ano, codmunres), names_from = dest, values_from = mediana_km) |>
  select(c(ano, codmunres, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana) <- c("ano","codmunres", "km_partos_fora_macrorregiao",
                             'km_partos_na_macrorregiao', "km_partos_na_regiao",
                             "km_partos_fora_uf")

#Data frame para mediana de deslocamento de alta complexidade por classificação
dados_mediana_alta <- dados6_alta |>
  pivot_wider(id_cols = c(ano, codmunres), names_from = dest, values_from = mediana_km) |>
  select(c(ano, codmunres, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana_alta) <- c("ano","codmunres",
                                  "km_partos_fora_macrorregiao_alta_complexidade",
                                  'km_partos_na_macrorregiao_alta_complexidade',
                                  "km_partos_na_regiao_alta_complexidade",
                                  "km_partos_fora_uf_alta_complexidade")

#Data frame para mediana de deslocamento de baixa complexidade por classificação
dados_mediana_baixa <- dados6_baixa |>
  pivot_wider(id_cols = c(ano, codmunres), names_from = dest, values_from = mediana_km) |>
  select(c(ano, codmunres, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana_baixa) <- c("ano","codmunres",
                                   "km_partos_fora_macrorregiao_baixa_complexidade",
                                   'km_partos_na_macrorregiao_baixa_complexidade',
                                   "km_partos_na_regiao_baixa_complexidade",
                                   "km_partos_fora_uf_baixa_complexidade")

rm(dados6,dados6_alta,dados6_baixa)

#Data frame com número de nascimentos fora do município e mediana de deslocamento para esses nascimentos
dados_nao_local <- dados5_mun |>
  filter(dest != "local")|>
  mutate(nascidos = 1) |>
  group_by(ano, codmunres) |>
  summarise(
    nao_local = sum(nascidos, na.rm =T),
    km_partos_fora_municipio = round(median(distancia, na.rm=T), 4)
  )

#Data frame com  mediana de deslocamento para nascimentos fora do município de alta complexidade
dados_nao_local_alta <- dados5_mun |>
  filter(dest != "local" & complexidade == "alta")|>
  group_by(ano, codmunres) |>
  summarise(
    km_partos_fora_municipio_alta_complexidade = round(median(distancia, na.rm=T), 4)
  )

#Data frame com  mediana de deslocamento para nascimentos fora do município de baixa complexidade
dados_nao_local_baixa <- dados5_mun |>
  filter(dest != "local" & complexidade == "baixa")|>
  group_by(ano, codmunres) |>
  summarise(
    km_partos_fora_municipio_baixa_complexidade = round(median(distancia, na.rm=T), 4)
  )

rm(dados5_mun)


# Juntando todos os dados de local nascimentos e mediana de deslocamento por município
dados7 <- full_join(dados_nascidos, dados_mediana, by=c("ano", "codmunres"))
rm(dados_nascidos, dados_mediana)
dados11 <- full_join(dados7,dados_mediana_alta, by = c("ano", "codmunres"))
rm(dados7,dados_mediana_alta)
dados12 <- full_join(dados11,dados_mediana_baixa, by = c("ano", "codmunres"))
rm(dados11,dados_mediana_baixa)
dados8 <- full_join(dados12, dados_nao_local, by = c("ano", "codmunres"))
rm(dados12, dados_nao_local)
dados13 <- full_join(dados8, dados_nao_local_alta, by = c("ano", "codmunres"))
rm(dados8, dados_nao_local_alta)
dados13 <- full_join(dados13, dados_nao_local_baixa, by = c("ano", "codmunres"))
rm(dados_nao_local_baixa)
dados9 <- full_join(dados13, nascidos_res, by = c("ano", "codmunres"))
rm(dados13, nascidos_res)

dados9[is.na(dados9)] <- 0

#### Classificação "se nascido fora do municipio" primeiros tres municipios onde
#### esses nascimentos ocorreram

## dado CODMUNRES aqui tomamos o top tres dos
## mucipios de nascimento, onde CODMUNRES != CODMUNNASC, ou seja NAO_LOCAL

dados2 <- dados |> filter(CODMUNRES != CODMUNNASC) |>
  group_by(CODMUNRES, CODMUNNASC, ANO) |>
  summarise(obs=n())

dados2b <- dados2 |> group_by(CODMUNRES, ANO) |>
  summarize(total_n_local_mun = sum(obs))

dados3 <- dados2 |>
  group_by(CODMUNRES, ANO) |>
  arrange(desc(obs)) |>
  slice_head(n = 3) |>
  mutate(row_number = row_number()) |>
  pivot_wider(names_from = row_number, values_from = obs) |>
  rename("n_nasc1" = "1", "n_nasc2" = "2", "n_nasc3" = "3") |>
  mutate(row_number = row_number()) |>
  pivot_wider(names_from = row_number, values_from = CODMUNNASC) |>
  rename("codmunnasc1" = "1", "codmunnasc2" = "2", "codmunnasc3" = "3") |>
  group_by(CODMUNRES, ANO) |>
  summarize(n_nasc1 = sum(n_nasc1, na.rm=T),
            n_nasc2 = sum(n_nasc2, na.rm=T),
            n_nasc3 = sum(n_nasc3, na.rm=T),
            codmunnasc1 = sum(codmunnasc1, na.rm=T),
            codmunnasc2 = sum(codmunnasc2, na.rm=T),
            codmunnasc3 = sum(codmunnasc3, na.rm=T))

dados3[dados3 == 0] <- NA

dados4 <- full_join(dados3, dados2b, by = c("CODMUNRES", "ANO"))

dados5b <- dados4 |>
  mutate(x_1 = round(n_nasc1/total_n_local_mun*100, 1),
         x_2 = round(n_nasc2/total_n_local_mun*100, 1),
         x_3 = round(n_nasc3/total_n_local_mun*100, 1)) |>
  select(c(ANO, CODMUNRES,
           total_n_local_mun,
           codmunnasc1, n_nasc1, x_1,
           codmunnasc2, n_nasc2, x_2,
           codmunnasc3, n_nasc3, x_3))

rm(dados4,dados2b,dados3,dados2)

## encontrando para cada municipio de resisdencia o estabelecimento em que
## ocorerreram mais nascimentos, onde CODMUNRES != CODMUNNASC, ou seja NAO_LOCAL

dados6 <- dados |> filter(CODMUNRES != CODMUNNASC) |>
  group_by(CODMUNRES, ANO, CODESTAB) |>
  summarise(nasc_estab=n()) |>
  arrange(desc(nasc_estab)) |>
  slice_head(n = 1) |>
  rename(cnes = CODESTAB)

## Juntando dados

dados8 <- left_join(dados5b, dados6, by = c("ANO", "CODMUNRES")) |>
  rename(codmunres = CODMUNRES, ano = ANO)

rm(dados6, dados5b)

# Juntando os dois conjuntos de dados para os municipios

dados10 <- full_join(dados9, dados8, by = c("ano", "codmunres"))

rm(dados8, dados9)

## Dados dos estabelecimentos do CNES, disponivel em:
## https://opendatasus.saude.gov.br/dataset/cnes-cadastro-nacional-de-estabelecimentos-de-saude

dados_cnes_estabelecimento <- data.table::fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/cnes_estabelecimentos.csv")

# dados_cnes_estabelecimento <- dados_cnes_estabelecimento |>
#   select(CO_CNES, NO_FANTASIA) |>
#   rename(cnes = CO_CNES,
#          nome_estabelecimento_fantasia = NO_FANTASIA)
#
# data.table::fwrite(dados_cnes_estabelecimento, "data-raw/extracao-dos-dados/blocos/databases_auxiliares/cnes_estabelecimentos.csv",
#                    row.names = FALSE)

## Juntando os dados

dados11 <- left_join(dados10, dados_cnes_estabelecimento, by = c("cnes"))

rm(dados10, dados_cnes_estabelecimento)

## Dados dos municipios

dados_muni2 <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  select(codmunres) |> unique()

anos <- data.frame(ano = c(2012:2024))

dados_muni4 <- cross_join(dados_muni2, anos)

dados12 <- left_join(dados_muni4,dados11,  by = c("codmunres", "ano"))

rm(dados11, dados_muni4, dados_muni2, anos)

# Exportando os dados
# data.table::fwrite(dados12, "databases/indicadores_bloco4_deslocamento_parto_municipio_2012_2023.csv")

################################################################################
######### Unindo os dois indicadores para municipios em um arquivo so ##########

# dados12 <- data.table::fread("databases/indicadores_bloco4_deslocamento_parto_municipio_2012_2023.csv")
dados_1500 <- data.table::fread("data-raw/csv/indicador_deslocamento_1500_2012_2024.csv")

dados_municipio <- full_join(dados12, dados_1500, by = c("codmunres","ano"))

dados_municipio$km_partos_fora_uf <- as.numeric(dados_municipio$km_partos_fora_uf)

data.table::fwrite(dados_municipio,"data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2012-2024.csv")

rm(dados12,dados_1500,dados_municipio)

##################### CÁLCULO DAS MEDIANAS E NASCIDOS PARA UF ##################

# Total de nascidos vivos com base na UF de residência
nascidos_res_uf <- dados |>
  select(c(ANO, UFr)) |>
  mutate(
    nascidos = 1,
    ano = as.numeric(ANO),
    uf = as.numeric(UFr)
  )|> group_by(uf, ano) |>
  summarise(
    destino_total = sum(nascidos)
  )

dados5_uf <- dados5 |>
  mutate(
    ano = as.numeric(ANO),
    uf = as.numeric(UFr),
    dest = case_when(
      is.na(CODMUNNASC) ~ NA_character_,

      CODMUNRES == CODMUNNASC ~ "local",

      (CODMUNRES != CODMUNNASC) &
        (CODRSRES == CODRSNASC) ~ "dentro_regiao_saude",

      (CODMRRES == CODMRNASC) &
        (CODMUNRES != CODMUNNASC) &
        (CODRSRES != CODRSNASC) ~  "dentro_macrorregiao_saude",

      (CODMRRES != CODMRNASC) &
        (CODMUNRES != CODMUNNASC) &
        (CODRSRES != CODRSNASC) &
        UFr == UFn ~ "fora_macrorregiao_saude",

      UFn != UFr ~ "outra_uf",

      TRUE ~ NA_character_
    ),
  ) |>
  select(
    c(ano, uf, dest, distancia, complexidade)
  )

# Calculando número de nascidos e mediana

dados6_uf <- dados5_uf |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, uf, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

dados6_uf_alta <- dados5_uf |>
  filter(complexidade == "alta") |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, uf, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

dados6_uf_baixa <- dados5_uf |>
  filter(complexidade == "baixa") |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, uf, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

# Data frame para número de nascidos por classificação

dados_nascidos_uf <- dados6_uf |>
  pivot_wider(id_cols = c(ano, uf), names_from = dest, values_from = nascidos_dest) |>
  select(c(ano, uf, local, outra_uf, dentro_macrorregiao_saude, dentro_regiao_saude,
           fora_macrorregiao_saude))

#Data frame para mediana de deslocamento por classificação
dados_mediana_uf <- dados6_uf |>
  pivot_wider(id_cols = c(ano, uf), names_from = dest, values_from = mediana_km) |>
  select(c(ano, uf, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana_uf) <- c("ano","uf", "km_partos_fora_macrorregiao",
                                'km_partos_na_macrorregiao', "km_partos_na_regiao",
                                "km_partos_fora_uf")

#Data frame para mediana de deslocamento por classificação
dados_mediana_uf_alta <- dados6_uf_alta |>
  pivot_wider(id_cols = c(ano, uf), names_from = dest, values_from = mediana_km) |>
  select(c(ano, uf, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana_uf_alta) <- c("ano","uf",
                                     "km_partos_fora_macrorregiao_alta_complexidade",
                                     'km_partos_na_macrorregiao_alta_complexidade',
                                     "km_partos_na_regiao_alta_complexidade",
                                     "km_partos_fora_uf_alta_complexidade")


dados_mediana_uf_baixa <- dados6_uf_baixa |>
  pivot_wider(id_cols = c(ano, uf), names_from = dest, values_from = mediana_km) |>
  select(c(ano, uf, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana_uf_baixa) <- c("ano","uf",
                                      "km_partos_fora_macrorregiao_baixa_complexidade",
                                      'km_partos_na_macrorregiao_baixa_complexidade',
                                      "km_partos_na_regiao_baixa_complexidade",
                                      "km_partos_fora_uf_baixa_complexidade")

rm(dados6_uf, dados6_uf_alta, dados6_uf_baixa)

#Data frame com número de nascimentos fora do município e mediana de deslocamento para esses nascimentos
dados_nao_local_uf <- dados5_uf |>
  filter(dest != "local")|>
  mutate(nascidos = 1) |>
  group_by(ano, uf) |>
  summarise(
    nao_local = sum(nascidos, na.rm =T),
    km_partos_fora_municipio = round(median(distancia, na.rm=T), 4)
  )

dados_nao_local_uf_alta <- dados5_uf |>
  filter(dest != "local" & complexidade == "alta")|>
  group_by(ano, uf) |>
  summarise(
    km_partos_fora_municipio_alta_complexidade = round(median(distancia, na.rm=T), 4)
  )

dados_nao_local_uf_baixa <- dados5_uf |>
  filter(dest != "local" & complexidade == "baixa")|>
  group_by(ano, uf) |>
  summarise(
    km_partos_fora_municipio_baixa_complexidade = round(median(distancia, na.rm=T), 4)
  )

rm(dados5_uf)

# Juntando todos os dados de local nascimentos e mediana de deslocamento por município
dados7_uf <- full_join(dados_nascidos_uf, dados_mediana_uf, by=c("ano", "uf"))
rm(dados_nascidos_uf, dados_mediana_uf)
dados14_uf <- full_join(dados7_uf, dados_mediana_uf_alta, by=c("ano", "uf"))
rm(dados7_uf, dados_mediana_uf_alta)
dados15_uf <- full_join(dados14_uf, dados_mediana_uf_baixa, by=c("ano", "uf"))
rm(dados14_uf, dados_mediana_uf_baixa)
dados8_uf <- full_join(dados15_uf, dados_nao_local_uf, by = c("ano", "uf"))
rm(dados15_uf, dados_nao_local_uf)
dados16_uf <- full_join(dados8_uf, dados_nao_local_uf_alta, by = c("ano", "uf"))
rm(dados8_uf, dados_nao_local_uf_alta)
dados17_uf <- full_join(dados16_uf, dados_nao_local_uf_baixa, by = c("ano", "uf"))
rm(dados16_uf, dados_nao_local_uf_baixa)
dados9_uf <- full_join(dados17_uf, nascidos_res_uf, by = c("ano", "uf"))
rm(dados17_uf, nascidos_res_uf)

dados9_uf <- dados9_uf |> rename(cod_uf = uf)

# Códigos das UFs

codigos_uf2 <- data.table::fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_aux_municipios.csv") |>
  select(uf, codmunres) |>
  mutate(cod_uf = floor(codmunres/10000)) |>
  select(!codmunres) |>
  unique()

anos <- data.frame(ano = c(2012:2024))

codigos_uf3 <- cross_join(codigos_uf2, anos)

rm(codigos_uf2, anos)


dados10_uf <- left_join(codigos_uf3, dados9_uf, by = c("cod_uf", "ano"))

dados10_uf[is.na(dados10_uf)] <- 0

rm(codigos_uf3, dados9_uf)

##################### CÁLCULO DAS MEDIANAS E NASCIDOS O BRASIL #################


# Total de nascidos vivos
nascidos_res_br <- dados |>
  select(ANO) |>
  mutate(
    nascidos = 1,
    ano = as.numeric(ANO)
  )|> group_by(ano) |>
  summarise(
    destino_total = sum(nascidos)
  )

dados5_br <- dados5 |>
  mutate(
    ano = as.numeric(ANO),
    uf = as.numeric(UFr),
    dest = case_when(
      is.na(CODMUNNASC) ~ NA_character_,

      CODMUNRES == CODMUNNASC ~ "local",

      (CODMUNRES != CODMUNNASC) &
        (CODRSRES == CODRSNASC) ~ "dentro_regiao_saude",

      (CODMRRES == CODMRNASC) &
        (CODMUNRES != CODMUNNASC) &
        (CODRSRES != CODRSNASC) ~  "dentro_macrorregiao_saude",

      (CODMRRES != CODMRNASC) &
        (CODMUNRES != CODMUNNASC) &
        (CODRSRES != CODRSNASC) &
        UFr == UFn ~ "fora_macrorregiao_saude",

      UFn != UFr ~ "outra_uf",

      TRUE ~ NA_character_
    ),
  ) |>
  select(
    c(ano, uf, dest, distancia, complexidade)
  )

# Calculando número de nascidos e mediana

dados6_br <- dados5_br |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

dados6_br_alta <- dados5_br |>
  filter(complexidade == "alta") |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

dados6_br_baixa <- dados5_br |>
  filter(complexidade == "baixa") |>
  mutate(
    nascidos = 1
  ) |>
  group_by(ano, dest) |>
  summarise(
    nascidos_dest = sum(nascidos, na.rm = T),
    mediana_km = round(median(distancia, na.rm =T), 4)
  )

# Data frame para número de nascidos por classificação

dados_nascidos_br <- dados6_br |>
  pivot_wider(id_cols = c(ano), names_from = dest, values_from = nascidos_dest) |>
  select(c(ano, local, outra_uf, dentro_macrorregiao_saude, dentro_regiao_saude,
           fora_macrorregiao_saude))

#Data frame para mediana de deslocamento por classificação
dados_mediana_br <- dados6_br |>
  pivot_wider(id_cols = c(ano), names_from = dest, values_from = mediana_km) |>
  select(c(ano, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana_br) <- c("ano", "km_partos_fora_macrorregiao",
                                'km_partos_na_macrorregiao', "km_partos_na_regiao",
                                "km_partos_fora_uf")

#Data frame para mediana de deslocamento por classificação
dados_mediana_br_alta <- dados6_br_alta |>
  pivot_wider(id_cols = c(ano), names_from = dest, values_from = mediana_km) |>
  select(c(ano, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana_br_alta) <- c("ano",
                                     "km_partos_fora_macrorregiao_alta_complexidade",
                                     'km_partos_na_macrorregiao_alta_complexidade',
                                     "km_partos_na_regiao_alta_complexidade",
                                     "km_partos_fora_uf_alta_complexidade")


dados_mediana_br_baixa <- dados6_br_baixa |>
  pivot_wider(id_cols = c(ano), names_from = dest, values_from = mediana_km) |>
  select(c(ano, fora_macrorregiao_saude, dentro_macrorregiao_saude,
           dentro_macrorregiao_saude, dentro_regiao_saude, outra_uf))

colnames(dados_mediana_br_baixa) <- c("ano",
                                      "km_partos_fora_macrorregiao_baixa_complexidade",
                                      'km_partos_na_macrorregiao_baixa_complexidade',
                                      "km_partos_na_regiao_baixa_complexidade",
                                      "km_partos_fora_uf_baixa_complexidade")

rm(dados6_br, dados6_br_alta, dados6_br_baixa)


# Data frame com número de nascimentos fora do município e mediana de deslocamento para esses nascimentos
dados_nao_local_br <- dados5_br |>
  filter(dest != "local")|>
  mutate(nascidos = 1) |>
  group_by(ano) |>
  summarise(
    nao_local = sum(nascidos, na.rm =T),
    km_partos_fora_municipio = round(median(distancia, na.rm=T), 4)
  )

dados_nao_local_br_alta <- dados5_br |>
  filter(dest != "local" & complexidade == "alta")|>
  group_by(ano) |>
  summarise(
    km_partos_fora_municipio_alta_complexidade = round(median(distancia, na.rm=T), 4)
  )

dados_nao_local_br_baixa <- dados5_br |>
  filter(dest != "local" & complexidade == "baixa")|>
  group_by(ano) |>
  summarise(
    km_partos_fora_municipio_baixa_complexidade = round(median(distancia, na.rm=T), 4)
  )

rm(dados5_br)

# Juntando todos os dados de local nascimentos e mediana de deslocamento por município
dados7_br <- full_join(dados_nascidos_br, dados_mediana_br, by=c("ano"))
rm(dados_nascidos_br, dados_mediana_br)
dados14_br <- full_join(dados7_br, dados_mediana_br_alta, by=c("ano"))
rm(dados7_br, dados_mediana_br_alta)
dados15_br <- full_join(dados14_br, dados_mediana_br_baixa, by=c("ano"))
rm(dados14_br, dados_mediana_br_baixa)
dados8_br <- full_join(dados15_br, dados_nao_local_br, by = c("ano"))
rm(dados15_br, dados_nao_local_br)
dados16_br <- full_join(dados8_br, dados_nao_local_br_alta, by = c("ano"))
rm(dados8_br, dados_nao_local_br_alta)
dados17_br <- full_join(dados16_br, dados_nao_local_br_baixa, by = c("ano"))
rm(dados16_br, dados_nao_local_br_baixa)
dados9_br <- full_join(dados17_br, nascidos_res_br, by = c("ano"))
rm(dados17_br, nascidos_res_br)

dados9_br$cod_uf <- 55

##

codigos_uf2 <- data.frame(cod_uf = 55, uf = "Brasil")

anos <- data.frame(ano = c(2012:2024))

codigos_uf3 <- cross_join(codigos_uf2, anos)

rm(codigos_uf2, anos)

dados10_br <- left_join(codigos_uf3, dados9_br, by = c("ano", "cod_uf"))

rm(codigos_uf3)

dados10_br[is.na(dados10_br)] <- 0

# Juntando dados para Ufs e Brasil
dados11_uf <- rbind(dados10_uf, dados10_br)

rm(dados9_br, dados10_uf, dados10_br)

# Exportando os dados
data.table::fwrite(dados11_uf, "data-raw/csv/indicadores_bloco4_deslocamento_parto_UF_2012-2024.csv")

rm(dados11_uf, dados, dados5)


