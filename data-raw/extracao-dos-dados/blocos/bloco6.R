library(tidyverse)
library(janitor)
library(data.table)
library(readr)
library(microdatasus)

# Criando alguns objetos auxiliares ---------------------------------------
## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv"
) |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(
  codmunres = rep(codigos_municipios, each = length(2012:2024)),
  ano = 2012:2024
)


# Para os dados de mortalidade materna ------------------------------------
## Baixando os dados consolidados do SIM-DOMAT
df_sim_domat_consolidados <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  information_system = "SIM-DOMAT"
)

## Baixando os dados preliminares do SIM
df_sim_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv") |>
  mutate_if(is.numeric, as.character)

## Filtrando, nos dados preliminares, apenas os óbitos maternos
df_sim_domat_preliminares <- df_sim_2024 |>
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
  )

## Juntando os dados preliminares e consolidados
df_sim_domat <- full_join(
  df_sim_domat_consolidados,
  df_sim_domat_preliminares
)

## Baixando os dados consolidados do SINASC
df_sinasc_consolidados <- fetch_datasus(
  year_start = 2012,
  year_end = 2023,
  information_system = "SINASC",
  vars = c("CODMUNRES", "DTNASC")
)

## Baixando os dados preliminares do SINASC
df_sinasc_preliminares <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2024.csv") |>
  select(CODMUNRES, DTNASC) |>
  mutate_if(is.numeric, as.character)

## Juntando os dados preliminares e consolidados
df_sinasc <- full_join(
  df_sinasc_consolidados,
  df_sinasc_preliminares
)

## Removendo arquivos já utilizados e limpando a memória
rm(df_sim_2024, df_sim_domat_consolidados, df_sim_domat_preliminares, df_sinasc_consolidados, df_sinasc_preliminares)
gc()

## Criando as variáveis necessárias
### Para o SIM
df_bloco6_sim <- df_sim_domat |>
  mutate(
    ano = as.numeric(substr(DTOBITO, nchar(DTOBITO) - 3, nchar(DTOBITO))),
    obitos_mat_totais = 1,
    obitos_mat_diretos = if_else(
      !((CAUSABAS >= "B200" & CAUSABAS <= "B249") |
          (CAUSABAS >= "O100" & CAUSABAS <= "O109") |
          ((CAUSABAS >= "O240" & CAUSABAS != "O244") & CAUSABAS <= "O259") |
          (CAUSABAS == "O94") |
          (CAUSABAS >= "O980" & CAUSABAS <= "O999")) & !(CAUSABAS == "O95"),
      1, 0, missing = 0
    ),
    obitos_mat_aborto = if_else(
      obitos_mat_diretos == 1 & (CAUSABAS >= "O030"  &  CAUSABAS <= "O079"),
      1, 0, missing = 0
    ),
    obitos_mat_hipertensao = if_else(
      obitos_mat_diretos == 1 & (CAUSABAS == "O11" | (CAUSABAS >= "O13" & CAUSABAS <= "O16")),
      1, 0, missing = 0
    ),
    obitos_mat_hemorragia = if_else(
      obitos_mat_diretos == 1 & ((CAUSABAS >= "O200" & CAUSABAS <= "O209") |
                                   (CAUSABAS >= "O440" & CAUSABAS <= "O469") |
                                   (CAUSABAS >= "O670" & CAUSABAS <= "O679") |
                                   (CAUSABAS >= "O710" & CAUSABAS <= "O711") |
                                   (CAUSABAS >= "O720" & CAUSABAS <= "O723")),
      1, 0, missing = 0
    ),
    obitos_mat_infec_puerperal = if_else(
      obitos_mat_diretos == 1 & (CAUSABAS >= "O85" & CAUSABAS <= "O868"),
      1, 0, missing = 0
    )
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise_at(vars(starts_with("obitos")), sum) |>
  ungroup() |>
  right_join(df_aux_municipios)

### Substituindo todos os NAs, gerados após o right_join, por 0
df_bloco6_sim[is.na(df_bloco6_sim)] <- 0

### Para o SINASC
df_bloco6_sinasc <- df_sinasc |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    nascidos = 1
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise(nascidos = sum(nascidos)) |>
  ungroup() |>
  right_join(df_aux_municipios)

### Substituindo todos os NAs, gerados após o right_join, por 0
df_bloco6_sinasc[is.na(df_bloco6_sinasc)] <- 0

## Juntando os dados do SIM e do SINASC
df_bloco6_mortalidade <- left_join(
  df_bloco6_sim,
  df_bloco6_sinasc
)

## Exportando os dados
write.csv(df_bloco6_mortalidade, 'data-raw/csv/indicadores_bloco6_mortalidade_materna_2012-2024.csv', row.names = FALSE)


################## DADOS DE RMM CORRIGIDOS

library(readxl)
library(dplyr)
library(readr)

#Carregando as tabelas pde 2012 a 2022

rmm_brasil <- read_excel("data-raw/extracao-dos-dados/databases_auxiliares/analises_obitos_RMM_1996_2021.xlsx") %>%
  filter(Ano >= 2012)

rmm_regioes <- read_excel("data-raw/extracao-dos-dados/databases_auxiliares/RMM_Regioes.xlsx") %>%
  filter(Ano>=2012)

rmm_estados <- read_excel("data-raw/extracao-dos-dados/databases_auxiliares/RMM_Estados.xlsx") %>%
  filter(Ano>=2012)

dicionario_estados <- data.frame(
  sigla = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
  localidade = c(
    "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo",
    "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba",
    "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul",
    "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"
  )
)

rmm_estados <- left_join(rmm_estados, dicionario_estados, by = c("Estado" = "sigla"))

rmm_regioes$localidade <- rmm_regioes$Regiao
rmm_brasil$localidade <- "Brasil"

rmm_brasil <- rmm_brasil %>% rename(ano = Ano)
rmm_estados <- rmm_estados %>% rename(ano = Ano) %>%
  select(ano, RMM, localidade)
rmm_regioes <- rmm_regioes %>% rename(ano = Ano) %>%
  select(ano, RMM, localidade)
rmm_brasil <- rmm_brasil %>% rename(RMM = RMM_C) %>%
  select(ano, RMM, localidade)

rmm_regioes <- rmm_regioes %>%
  mutate(localidade = ifelse(localidade == "Centro_Oeste", "Centro-Oeste", localidade))

rmm_corrigida <- rbind(rmm_brasil, rmm_estados, rmm_regioes)
rmm_corrigida$RMM <- round(rmm_corrigida$RMM, 2)


write.table(rmm_corrigida, 'data-raw/csv/rmm_corrigida_2012-2021.csv', sep = ",", dec = ".", row.names = FALSE)
