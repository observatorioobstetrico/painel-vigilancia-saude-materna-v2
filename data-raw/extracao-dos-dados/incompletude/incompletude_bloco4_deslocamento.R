# Carregando os pacotes necessários
library(dplyr)
library(tidyr)
library(janitor)
library(microdatasus)
library(glue)
library(data.table)

# Criando alguns objetos auxiliares ------------------------------------------
## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2023)), ano = 2012:2023)


# Baixando os dados do SINASC ---------------------------------------------
## Baixando os dados consolidados do SINASC e selecionando as variáveis de interesse
df_sinasc_aux <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  vars = c("CODMUNRES", "CODMUNNASC", "DTNASC", "IDADEMAE", "CODESTAB"),
  information_system = "SINASC"
)

## Criando a variável de ano e filtrando apenas nascimentos de mães em idade fértil
df_sinasc <- df_sinasc_aux |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    mes = as.numeric(substr(DTNASC, 3, 4)),
    .keep = "unused",
    .after = "CODMUNNASC"
  ) |>
  mutate(
    IDADEMAE = as.numeric(IDADEMAE)
  ) |>
  filter(
    IDADEMAE >= 10 & IDADEMAE <= 49
  ) |>
  clean_names()


# Baixando os dados do CNES -----------------------------------------------
## Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

### Criando uma pasta temporária para armazenar os arquivos baixados
dir.create("data-raw/extracao-dos-dados/incompletude/databases_auxiliares/cnes_st", showWarnings = FALSE, recursive = TRUE)

## Baixando os dados do CNES estabelecimentos e selecionando as variáveis de interesse
for (estado in estados) {
  ## Baixando os dados do CNES-ST para o dado estado
  df_cnes_st_uf_aux <- fetch_datasus(
    year_start = 2012,
    year_end = 2023,
    month_start = 1,
    month_end = 12,
    timeout = 1000,
    uf = estado,
    information_system = "CNES-ST",
    vars = c("CNES", "CODUFMUN", "COMPETEN")
  )

  ## Criando a variável de ano
  df_cnes_st_uf <- df_cnes_st_uf_aux |>
    mutate(
      ano = as.numeric(substr(COMPETEN, 1, 4)),
      mes = as.numeric(substr(COMPETEN, 5, 6)),
      .keep = "unused"
    ) |>
    unique() |>
    clean_names()

  ## Exportando os dados da UF
  write.csv(df_cnes_st_uf, glue("data-raw/extracao-dos-dados/incompletude/databases_auxiliares/cnes_st/df_cnes_st_{estado}_2012_2023.csv"), row.names = FALSE)

  ## Limpando a memória
  rm(df_cnes_st_uf_aux, df_cnes_st_uf)
  gc()
}

## Juntando todos os dados baixados
df_cnes_st <- data.frame()

for (estado in estados) {
  ## Lendo os dados do CNES-ST da dada UF
  df_cnes_st_aux <- fread(
    glue("data-raw/extracao-dos-dados/incompletude/databases_auxiliares/cnes_st/df_cnes_st_{estado}_2012_2023.csv"),
    colClasses = c("character", "character", "numeric", "numeric")
  )

  ## Juntando com o restante dos dados
  df_cnes_st <- bind_rows(df_cnes_st, df_cnes_st_aux)

  ## Limpando a memória
  rm(df_cnes_st_aux)
  gc()
}

## Salvando a base completa do CNES-ST
write.csv(df_cnes_st, glue("data-raw/extracao-dos-dados/incompletude/databases_auxiliares/cnes_st/df_cnes_st_BR_2012_2023.csv"), row.names = FALSE)


# Fazendo as manipulações necessárias -------------------------------------
df_sinasc <- fread("data-raw/extracao-dos-dados/incompletude/databases_auxiliares/df_sinasc_2012_2023.csv", colClasses = c("character", "character", "numeric", "numeric", "numeric", "character"))

## Criando um dataframe contendo as variáveis necessárias para o cálculo da incompletude
df_incompletude <- df_sinasc |>
  mutate(
    dn_hospital_id_fertil = 1,
    dn_hosp_id_fertil_cnes_preenchido = ifelse(is.na(codestab), 0, 1),
    dn_hosp_id_fertil_cnes_valido = ifelse(
      is.na(codestab) |
        !(paste(codmunnasc, codestab, ano, mes) %in% paste(df_cnes_st$codufmun, df_cnes_st$cnes, df_cnes_st$ano, df_cnes_st$mes)),
      0,
      1
    )
  ) |>
  group_by(ano, codmunres) |>
  summarise(
    dn_hospital_id_fertil = sum(dn_hospital_id_fertil),
    dn_hosp_id_fertil_cnes_preenchido = sum(dn_hosp_id_fertil_cnes_preenchido),
    dn_hosp_id_fertil_cnes_valido = sum(dn_hosp_id_fertil_cnes_valido)
  ) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  arrange(codmunres, ano)

## Exportando a base de incompletude
#write.csv(df_incompletude, "data-raw/csv/incompletude_indicadores_deslocamento.csv", row.names = FALSE)
write.csv(df_incompletude, "data-raw/extracao-dos-dados/incompletude/databases_auxiliares/incompletude_indicadores_deslocamento.csv", row.names = FALSE)

## Excluindo os arquivos do CNES
unlink("data-raw/extracao-dos-dados/incompletude/databases_auxiliares/cnes_st", recursive = TRUE)
