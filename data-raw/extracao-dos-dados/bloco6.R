library(tidyverse)
library(janitor)

############ DADOS DE MORTALIDADE MATERNA

#setwd("./Databases")

# remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)

#Criando função soma_var para poder fazer tratamento dos dados ----
soma_var <- function(vars,dados) {
  out <- rowSums(dados[,vars],na.rm=TRUE)
  return(out)
}

#Carregando os dados do SIM de 2021

df_causas_especif_aux <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  information_system = "SIM-DOMAT"
) |>
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
df_nascimentos <- read_delim("databases-antigas/dados_sinasc.csv",
                             delim = ",", escape_double = FALSE, trim_ws = TRUE) |>
  clean_names()

df_nascimentos$codigo <- as.character(df_nascimentos$codigo)

df_nascimentos_2021 <- filter(df_nascimentos, ano==2022)

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

aux_municipios <- read.csv("databases-antigas/tabela_aux_municipios.csv") |>
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

write.table(df_completo_plus, 'indicadores_bloco6_mortalidade_materna_2022.csv', sep = ",", dec = ".", row.names = FALSE)

indicadores_bloco6_mortalidade_materna_2012_2021 <- read_csv("databases-antigas/indicadores_bloco6_mortalidade_materna_2012-2021.csv")

bloco6_mortalidade_materna_2012_2022 <- rbind(indicadores_bloco6_mortalidade_materna_2012_2021, df_completo_plus)

write.table(bloco6_mortalidade_materna_2012_2022, 'indicadores_bloco6_mortalidade_materna_2012-2022.csv', sep = ",", dec = ".", row.names = FALSE)


################## DADOS DE RMM CORRIGIDOS

library(readxl)
library(dplyr)
library(readr)

#Carregando as tabelas pde 2012 a 2021

rmm_brasil <- read_excel("data-raw/extracao-dos-dados/databases-antigas/analises_obitos_RMM_1996_2021.xlsx") %>%
  filter(Ano >= 2012)

rmm_regioes <- read_excel("data-raw/extracao-dos-dados/databases-antigas/RMM_Regioes.xlsx") %>%
  filter(Ano>=2012)

rmm_estados <- read_excel("data-raw/extracao-dos-dados/databases-antigas/RMM_Estados.xlsx") %>%
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


write.table(rmm_corrigida, 'rmm_corrigida_2012-2021.csv', sep = ",", dec = ".", row.names = FALSE)
