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
df_sinasc_aux <- microdatasus::fetch_datasus(year_start = 2013, year_end = 2023,
                                             information_system = "SINASC",
                                             vars = c("CODMUNRES",
                                                      "DTNASC",
                                                      "TPNASCASSI",
                                                      "LOCNASC",
                                                      "PARTO")
)

write.csv(df_sinasc_aux, "data-raw/extracao-dos-dados/incompletude/df_sinasc_aux.csv")

df_sinasc <- df_sinasc_aux |>
  # Selecionando somente parto vaginal, PARTO == 1
  filter(PARTO == "1") |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    tpnascassi = as.numeric(TPNASCASSI),
    locnasc = as.numeric(LOCNASC),
    .keep = "unused",
  ) |> clean_names()


# Criando as variáveis de incompletude -----------------------------------------
## Checando quais os possíveis valores incompletos para cada variável
### Para TPNASCASSI
sort(unique(df_sinasc$tpnascassi), na.last = FALSE)  # Existem NAs
sort(unique(df_sinasc$tpnascassi), decreasing = TRUE)
length(df_sinasc$tpnascassi[which(df_sinasc$tpnascassi == 9999)]) # Existem

### Para LOCNASC
sort(unique(df_sinasc$locnasc), na.last = FALSE)  # Existem NAs
sort(unique(df_sinasc$locnasc), decreasing = TRUE)
length(df_sinasc$locnasc[which(df_sinasc$locnasc == 9999)]) # Existem

# Criando data.frame de incompletude -------------------------------------------

df_incompletude_bloco4_profissional <- df_sinasc |>
  mutate(
    locnasc_incompletos = if_else(is.na(locnasc), 1, 0),
    tpnascassi_incompletos = if_else(is.na(tpnascassi), 1, 0),
    locnasc_totais = 1,
    tpnascassi_totais = 1
  ) |>
  group_by(ano, codmunres) |>
  summarise(
    locnasc_incompletos = sum(locnasc_incompletos),
    locnasc_totais = sum(locnasc_totais),
    tpnascassi_incompletos = sum(tpnascassi_incompletos),
    tpnascassi_totais = sum(tpnascassi_totais)
  )

write.csv(df_incompletude_bloco4_profissional, 'data-raw/csv/indicadores_incompletude_bloco4_profissional_2013-2023.csv', row.names = FALSE)





