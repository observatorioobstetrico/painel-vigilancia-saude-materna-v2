# Carregando os pacotes necessários
library(microdatasus)
library(dplyr)
library(janitor)
library(readr)
library(openxlsx)
library(readxl)
library(stringr)
library(tidyr)
library(data.table)

# Criando alguns objetos auxiliares ---------------------------------------
## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read_csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(municipio)

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(
  codmunres = rep(
    codigos_municipios,
    each = length(2012:2026)
  ),
  ano = 2012:2026
)

# Para os indicadores provenientes do SINASC ------------------------------
## Baixando os dados consolidados do SINASC de 2012 a 2024 e selecionando as variáveis de interesse

### Baixando SINASC 2012
df_2012 <- fetch_datasus(
  year_start = 2012,
  year_end = 2012,
  information_system = "SINASC",
  vars = c("CODMUNRES", "DTNASC", "CONSPRENAT", "MESPRENAT", "SEMAGESTAC"))

### Baixando SINASC 2013
df_2013 <- fetch_datasus(
  year_start = 2013,
  year_end = 2013,
  information_system = "SINASC",
  vars = c("CODMUNRES", "DTNASC", "MESPRENAT", "SEMAGESTAC")) |>
  # CONSPRENAT não é definada para o ano de 2013
  mutate(CONSPRENAT = NA)

### Baixando SINASC 2014-2024
df_2014_2024 <- fetch_datasus(
  year_start = 2014,
  year_end = 2024,
  information_system = "SINASC",
  vars = c("CODMUNRES", "DTNASC", "CONSPRENAT", "MESPRENAT", "SEMAGESTAC"))

## Baixando os dados preliminares do SINASC de 2025 e 2026
options(timeout = 99999)

### Baixando SINASC 2025
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file(
  "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2025_csv.zip",
  temp_zip,
  mode = "wb"
)

files <- unzip(temp_zip, exdir = temp_dir)

df_sinasc_preliminares_2025 <- fread(files[1], sep = ";") |>
  select(CODMUNRES, DTNASC, CONSPRENAT, MESPRENAT, SEMAGESTAC)


### Baixando SINASC 2026
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file(
  "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2026_csv.zip",
  temp_zip,
  mode = "wb"
)

files <- unzip(temp_zip, exdir = temp_dir)

df_sinasc_preliminares_2026 <- fread(files[1], sep = ";") |>
  select(CODMUNRES, DTNASC, CONSPRENAT, MESPRENAT, SEMAGESTAC)

## Juntando os dataframes
df <- rbind(
  df_2012,
  df_2013,
  df_2014_2024,
  df_sinasc_preliminares_2025,
  df_sinasc_preliminares_2026
) |>
  clean_names() |>
  mutate(
    ano = as.numeric(substr(dtnasc, nchar(dtnasc) - 3, nchar(dtnasc)))
  )

rm(
  df_2012,
  df_2013,
  df_2014_2024,
  df_sinasc_preliminares_2025,
  df_sinasc_preliminares_2026
)

# Tratando os dados e calculando indicadores ------------------------------
df2 <- df |>
  mutate(
    codmunres = as.numeric(CODMUNRES),
    CONSPRENAT = as.numeric(CONSPRENAT),
    MESPRENAT = as.numeric(MESPRENAT),
    SEMAGESTAC = as.numeric(SEMAGESTAC)
  ) |>
  mutate(
    nascidos = 1,

    pelo_menos_uma_consulta_prenatal = case_when(
      CONSPRENAT >= 1 ~ 1,
      !(CONSPRENAT >= 1) ~ 0
    ),
    inicio_precoce_do_prenatal = case_when(
      (MESPRENAT ==  1 | MESPRENAT == 2 | MESPRENAT ==  3) ~ 1,
      !(MESPRENAT ==  1 | MESPRENAT == 2 | MESPRENAT ==  3) ~ 0
    ),
    mais_de_sete_consultas_prenatal = case_when(
      (CONSPRENAT > 7 & CONSPRENAT < 99) ~ 1,
      !(CONSPRENAT > 7 & CONSPRENAT < 99) ~ 0
    ),
    mulheres_com_consultas_prenatal_adequadas = case_when(
      ((SEMAGESTAC < 20 & CONSPRENAT >= 1) |
         (SEMAGESTAC >= 20 & SEMAGESTAC < 26 & CONSPRENAT >= 2) |
         (SEMAGESTAC >= 26 & SEMAGESTAC < 30 & CONSPRENAT >= 3) |
         (SEMAGESTAC >= 30 & SEMAGESTAC < 34 & CONSPRENAT >= 4) |
         (SEMAGESTAC >= 34 & SEMAGESTAC < 36 & CONSPRENAT >= 5) |
         (SEMAGESTAC >= 36 & SEMAGESTAC < 38 & CONSPRENAT >= 6) |
         (SEMAGESTAC >= 38 & SEMAGESTAC < 40 & CONSPRENAT >= 7) |
         (SEMAGESTAC >= 40 & SEMAGESTAC < 99 & CONSPRENAT >= 8 & CONSPRENAT < 99)) ~ 1,

      !((SEMAGESTAC < 20 & CONSPRENAT >= 1) |
          (SEMAGESTAC >= 20 & SEMAGESTAC < 26 & CONSPRENAT >= 2) |
          (SEMAGESTAC >= 26 & SEMAGESTAC < 30 & CONSPRENAT >= 3) |
          (SEMAGESTAC >= 30 & SEMAGESTAC < 34 & CONSPRENAT >= 4) |
          (SEMAGESTAC >= 34 & SEMAGESTAC < 36 & CONSPRENAT >= 5) |
          (SEMAGESTAC >= 36 & SEMAGESTAC < 38 & CONSPRENAT >= 6) |
          (SEMAGESTAC >= 38 & SEMAGESTAC < 40 & CONSPRENAT >= 7) |
          (SEMAGESTAC >= 40 & SEMAGESTAC < 99 & CONSPRENAT >= 8 & CONSPRENAT < 99)) ~ 0
    )
  ) |>
  group_by(codmunres, ano) |>
  summarise(
    total_de_nascidos_vivos = sum(nascidos),
    mulheres_com_pelo_menos_uma_consulta_prenatal = sum(pelo_menos_uma_consulta_prenatal, na.rm = T),
    mulheres_com_inicio_precoce_do_prenatal = sum(inicio_precoce_do_prenatal, na.rm = T),
    mulheres_com_mais_de_sete_consultas_prenatal = sum(mais_de_sete_consultas_prenatal, na.rm = T),
    mulheres_com_consultas_prenatal_adequadas = sum(mulheres_com_consultas_prenatal_adequadas, na.rm = T)
  ) |>
  ungroup()

rm(df)

### Transformando as colunas que estão em caracter para numéricas
df2 <- df2 |> mutate_if(is.character, as.numeric)

### Fazendo um left_join da base auxiliar de municípios com o data.frame que contém o total de nascidos vivos
df_bloco3 <- left_join(df_aux_municipios, df2)

df_bloco3[is.na(df_bloco3)] <- 0

rm(df2)

# Incidência de sífilis congênita por mil nascidos vivos ------------------
## Lendo a base de dados obtida pelo site http://indicadoressifilis.aids.gov.br/
df_sifilis_excel1 <- read_excel("data-raw/extracao-dos-dados/blocos/databases_auxiliares/dados_painel_sifilis_2022.xlsx",
                               sheet = "DADOS CONTINUAÇÃO 2"
)

df_sifilis_excel2 <- read_excel("data-raw/extracao-dos-dados/blocos/databases_auxiliares/dados_painel_sifilis_2013_2024.xlsx",
                                sheet = "DADOS CONTINUAÇÃO 2"
)

### Corrigindo os nomes das colunas e filtrando pelos municípios que utilizamos no painel
names(df_sifilis_excel1) <- as.character(df_sifilis_excel1[1,])
names(df_sifilis_excel2) <- as.character(df_sifilis_excel2[1,])

df_sifilis1 <- df_sifilis_excel1[-1, ] |>
  clean_names() |>
  select(
    codmunres = codigo,
    starts_with("sifilis_congenita_em_menores_de_um_ano_2")
  ) |>
  rename_with(
    str_sub, start = -4, starts_with("sifilis_congenita_em_menores_de_um_ano_2")
  ) |>
  filter(codmunres %in% df_aux_municipios$codmunres) |>
  mutate_if(is.character, as.numeric)

df_sifilis2 <- df_sifilis_excel2[-1, ] |>
  clean_names() |>
  select(
    codmunres = codigo,
    starts_with("sifilis_congenita_em_menores_de_um_ano_2")
  ) |>
  rename_with(
    str_sub, start = -4, starts_with("sifilis_congenita_em_menores_de_um_ano_2")
  ) |>
  filter(codmunres %in% df_aux_municipios$codmunres) |>
  mutate_if(is.character, as.numeric)

### Passando o data.frame para o formato long
df_sifilis_long1 <- df_sifilis1 |>
  pivot_longer(
    cols = !codmunres,
    names_to = "ano",
    values_to = "casos_sc"
  ) |>
  filter(ano <= 2020) |>
  mutate_if(is.character, as.numeric)

df_sifilis_long2 <- df_sifilis2 |>
  pivot_longer(
    cols = !codmunres,
    names_to = "ano",
    values_to = "casos_sc"
  ) |>
  filter(ano <= 2024 & ano > 2020) |>
  mutate_if(is.character, as.numeric)

df_sifilis_long <- rbind(df_sifilis_long1, df_sifilis_long2)

## Juntando com o restante da base do bloco 3
df_bloco3 <- left_join(df_bloco3, df_sifilis_long)

### Substituindo os NA's da coluna 'casos_sc' por 0 (gerados após o left_join)
df_bloco3$casos_sc[is.na(df_bloco3$casos_sc)] <- 0

# Não temos dados para a Incidência de sífilis congênita para o ano de 2024 -- (provisorio remover quando a base for atualizada para 2024) --

### Substituindo os 0 da coluna 'casos_sc' por NA para o ano de 2024
df_bloco3$casos_sc[df_bloco3$ano == 2024] <- NA

# Salvando a base de dados completa -----------------
write.csv(df_bloco3, "data-raw/csv/indicadores_bloco3_assistencia_pre-natal_2012-2026.csv", row.names = FALSE)
