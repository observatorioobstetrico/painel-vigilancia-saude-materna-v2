library(tidyverse)
library(janitor)
library(data.table)
library(readr)

############ DADOS DE MORTALIDADE MATERNA

#setwd("./Databases")

# remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)

#Criando função soma_var para poder fazer tratamento dos dados ----
soma_var <- function(vars,dados) {
  out <- rowSums(dados[,vars],na.rm=TRUE)
  return(out)
}

#Carregando os dados do SIM de 2021 a 2023

df_causas_especif_aux1 <- fetch_datasus(
  year_start = 2021,
  year_end = 2023,
  information_system = "SIM-DOMAT"
)

options(timeout = 600)

sim_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv")

sim_2024$OBITOPUERP <- as.character(sim_2024$OBITOPUERP)
sim_2024$OBITOGRAV <- as.character(sim_2024$OBITOGRAV)
sim_2024$SEXO <- as.character(sim_2024$SEXO)

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
  )

sim_mat2024 <- sim_mat2024 |> select(-c(contador, OPOR_DO, TP_ALTERA, CB_ALT))

df_causas_especif_aux1 <- df_causas_especif_aux1 |> select(-c(ESTABDESCR, NUDIASOBIN,
                                                              NUDIASINF, FONTESINF,
                                                              CONTADOR))

df_causas_especif_aux <- rbind(df_causas_especif_aux1, sim_mat2024)


df_causas_especif_aux <- df_causas_especif_aux |>
  mutate(
    tipo_de_morte_materna = if_else(
      condition = (CAUSABAS >= "B200" & CAUSABAS <= "B249") |
        (CAUSABAS >= "O100" & CAUSABAS <= "O109") |
        ((CAUSABAS >= "O240" & CAUSABAS != "O244") & CAUSABAS <= "O259") |
        (CAUSABAS == "O94") |
        (CAUSABAS >= "O980" & CAUSABAS <= "O999"),
      true = "Indireta",
      false = if_else(CAUSABAS == "O95", true = "Não especificada", false = "Direta")
    ),
    causa_especifica = case_when(
      CAUSABAS >= "O030"  &  CAUSABAS <= "O079" ~ "aborto",
      CAUSABAS == "O11" | (CAUSABAS >= "O13" & CAUSABAS <= "O16") ~ "causas_hipertensivas",
      (CAUSABAS >= "O200" & CAUSABAS <= "O209") |
        (CAUSABAS >= "O440" & CAUSABAS <= "O469") |
        (CAUSABAS >= "O670" & CAUSABAS <= "O679") |
        (CAUSABAS >= "O710" & CAUSABAS <= "O711") |
        (CAUSABAS >= "O720" & CAUSABAS <= "O723") ~ "causas_hemorragicas",
      CAUSABAS >= "O85" & CAUSABAS <= "O868" ~ "infeccao_puerperal"
    ),
    ano = as.numeric(sub('.*(\\d{4}).*', '\\1', DTOBITO)),
    obitos = 1
  ) |>
  group_by(CODMUNRES, ano, causa_especifica, tipo_de_morte_materna) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(names_from = c(causa_especifica, tipo_de_morte_materna), values_from = obitos, values_fill = 0)

df_causas_especificas <- df_causas_especif_aux |>
  mutate(
    obitos_mat_diretos = soma_var(c("causas_hipertensivas_Direta",
                                    "aborto_Direta",
                                    "causas_hemorragicas_Direta",
                                    "NA_Direta",
                                    "infeccao_puerperal_Direta"),
                                  df_causas_especif_aux),
    obitos_mat_totais = soma_var(c("NA_Indireta",
                                   "causas_hipertensivas_Direta",
                                   "aborto_Direta",
                                   "causas_hemorragicas_Direta",
                                   "NA_Não especificada",
                                   "NA_Direta",
                                   "infeccao_puerperal_Direta"),
                                 df_causas_especif_aux)
  ) |>
  select(
    codigo = CODMUNRES,
    ano,
    obitos_mat_totais,
    obitos_mat_diretos,
    obitos_mat_aborto = aborto_Direta,
    obitos_mat_hipertensao = causas_hipertensivas_Direta,
    obitos_mat_hemorragia = causas_hemorragicas_Direta,
    obitos_mat_infec_puerperal = infeccao_puerperal_Direta
  )

#Obtendo dados de nascidos vivos do SINASC de 2021 (foram usados os dados do github do painel de indicadores obstétricos)
# df_nascimentos <- read_delim("data-raw/extracao-dos-dados/databases_auxiliares/dados_sinasc.csv",
#                              delim = ",", escape_double = FALSE, trim_ws = TRUE) |>
#   clean_names()

df_nascimentos <- read_csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/dados_oobr_indicadores_obstetricos_sinasc_1996_2024.csv") |>
  clean_names()

df_nascimentos$codigo <- as.character(df_nascimentos$codigo)

df_nascimentos_2021 <- filter(df_nascimentos, ano >= 2021)

df_completo <- left_join(df_nascimentos_2021, df_causas_especificas, by = c("codigo", "ano")) |>
  select(
    codmunres = codigo,
    uf,
    ano,
    nascidos,
    obitos_mat_totais,
    obitos_mat_diretos,
    obitos_mat_aborto,
    obitos_mat_hipertensao,
    obitos_mat_hemorragia,
    obitos_mat_infec_puerperal
  )

get_dupes(df_completo, codmunres, uf, ano)

aux_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  select(codmunres, municipio, uf, regiao)

aux_municipios$codmunres <- as.character(aux_municipios$codmunres)

df_completo_plus <- left_join(aux_municipios, df_completo, by=c("codmunres"))  |>
  select(
    codmunres,
    municipio,
    uf=uf.x,
    regiao,
    ano,
    nascidos,
    obitos_mat_totais,
    obitos_mat_diretos,
    obitos_mat_aborto,
    obitos_mat_hipertensao,
    obitos_mat_hemorragia,
    obitos_mat_infec_puerperal
  )

df_completo_plus[is.na(df_completo_plus)] <- 0

get_dupes(df_completo_plus, codmunres, municipio, uf, regiao, ano)

# write.table(df_completo_plus, 'data-raw/csv/indicadores_bloco6_mortalidade_materna_2023.csv', sep = ",", dec = ".", row.names = FALSE) |>
#   filter(ano == 2023)

indicadores_bloco6_mortalidade_materna_2012_2023 <- read_csv("data-raw/csv/indicadores_bloco6_mortalidade_materna_2012-2024.csv") |>
  filter(ano <= 2020)

bloco6_mortalidade_materna_2012_2024 <- rbind(indicadores_bloco6_mortalidade_materna_2012_2023, df_completo_plus)

write.table(bloco6_mortalidade_materna_2012_2024, 'data-raw/csv/indicadores_bloco6_mortalidade_materna_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


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
