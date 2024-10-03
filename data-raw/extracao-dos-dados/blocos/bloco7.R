# Carregando os pacotes necessários
library(microdatasus)
library(dplyr)
library(readr)
library(data.table)
library(janitor)
library(tidyr)
library(readxl)

# Criando alguns objetos auxiliares ------------------------------------------
## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

## Criando data.frames que irão receber os dados dos indicadores de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_fetal <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)
df_bloco7_distribuicao_cids_neonatal <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)
df_bloco7_distribuicao_cids_perinatal <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

# Para os óbitos fetais ---------------------------------------------------
## Baixando os dados consolidados do SIM-DOFET e filtrando apenas pelos óbitos fetais que consideramos
df_sim_dofet_consolidados <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  vars = c("CODMUNRES", "DTOBITO", "PESO", "GESTACAO", "SEMAGESTAC", "OBITOPARTO", "CAUSABAS"),
  information_system = "SIM-DOFET",
  timeout = 600
) |>
  mutate(
    ano = as.numeric(substr(DTOBITO, nchar(DTOBITO) - 3, nchar(DTOBITO))),
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    PESO = as.numeric(PESO),
    GESTACAO = as.character(GESTACAO)
  ) |>
  filter(
    ((GESTACAO != "1" & !is.na(GESTACAO) & GESTACAO != "9") | (SEMAGESTAC >= 22 & SEMAGESTAC != 99)) | (PESO >= 500)
  )

## Baixando os dados preliminares do SIM de 2023 e filtrando pelos óbitos fetais
df_sim_dofet_2023 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv") |>
  mutate(
    ano = as.numeric(substr(DTOBITO, nchar(DTOBITO) - 3, nchar(DTOBITO))),
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    PESO = as.numeric(PESO),
    TIPOBITO = as.numeric(PESO),
    GESTACAO = as.character(GESTACAO)
  ) |>
  filter(
    TIPOBITO == 1, ((GESTACAO != "1" & !is.na(GESTACAO) & GESTACAO != "9") | (SEMAGESTAC >= 22 & SEMAGESTAC != 99)) | (PESO >= 500)
  ) |>
  select(c(CODMUNRES, ano, DTOBITO, PESO, GESTACAO, SEMAGESTAC, OBITOPARTO, CAUSABAS))

## Baixando os dados preliminares do SIM de 2024 e filtrando pelos óbitos fetais
df_sim_dofet_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN+(2).csv") |>
  mutate(
    ano = as.numeric(substr(DTOBITO, nchar(DTOBITO) - 3, nchar(DTOBITO))),
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    PESO = as.numeric(PESO),
    TIPOBITO = as.numeric(PESO),
    GESTACAO = as.character(GESTACAO)
  ) |>
  filter(
    TIPOBITO == 1, ((GESTACAO != "1" & !is.na(GESTACAO) & GESTACAO != "9") | (SEMAGESTAC >= 22 & SEMAGESTAC != 99)) | (PESO >= 500)
  ) |>
  select(c(CODMUNRES, ano, DTOBITO, PESO, GESTACAO, SEMAGESTAC, OBITOPARTO, CAUSABAS))

## Juntando os dados consolidados e os dados preliminares e arrumando algumas colunas
df_fetais_totais <- rbind(
  df_sim_dofet_consolidados,
  rbind(df_sim_dofet_2023, df_sim_dofet_2024)
)

## Para os indicadores originais ------------------------------------------
### Criando algumas colunas e criando as variáveis utilizadas no painel
df_bloco7_fetais_aux <- df_fetais_totais |>
  mutate(
    codmunres = CODMUNRES
  ) |>
  mutate(
    obitos = 1,
    peso_menos_1500 = case_when(
      PESO < 1500 ~ 1,
      !(PESO < 1500) ~ 0
    ),
    peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000) ~ 1,
      !(PESO >= 1500 & PESO < 2000) ~ 0
    ),
    peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500) ~ 1,
      !(PESO >= 2000 & PESO < 2500) ~ 0
    ),
    peso_mais_2500 = case_when(
      (PESO >= 2500) ~ 1,
      !(PESO >=2500) ~ 0
    ),
    sem_info_parto = case_when(
      is.na(OBITOPARTO) ~ 1,
      !is.na(OBITOPARTO) ~ 0
    ),
    antes = case_when(
      OBITOPARTO == "1" ~ 1,
      !(OBITOPARTO == "1") ~ 0
    ),
    durante = case_when(
      (OBITOPARTO == "2") ~ 1,
      !(OBITOPARTO == "2") ~ 0
    ),
    depois = case_when(
      (OBITOPARTO == "3") ~ 1,
      !(OBITOPARTO == "3") ~ 0
    ),
    antes_peso_menos_1500 = case_when(
      (PESO < 1500 & OBITOPARTO == "1") ~ 1,
      !(PESO < 1500 & OBITOPARTO == "1") ~ 0
    ),
    antes_peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000 & OBITOPARTO == "1") ~ 1,
      !(PESO >= 1500 & PESO < 2000 & OBITOPARTO == "1") ~ 0
    ),
    antes_peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500 & OBITOPARTO == "1") ~ 1,
      !(PESO >= 2000 & PESO < 2500 & OBITOPARTO == "1") ~ 0
    ),
    antes_peso_mais_2500 = case_when(
      (PESO >= 2500 & OBITOPARTO == "1") ~ 1,
      !(PESO >=2500 & OBITOPARTO == "1") ~ 0
    ),
    durante_peso_menos_1500 = case_when(
      (PESO < 1500 & OBITOPARTO == "2") ~ 1,
      !(PESO < 1500 & OBITOPARTO == "2") ~ 0
    ),
    durante_peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000 & OBITOPARTO == "2") ~ 1,
      !(PESO >= 1500 & PESO < 2000 & OBITOPARTO == "2") ~ 0
    ),
    durante_peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500 & OBITOPARTO == "2") ~ 1,
      !(PESO >= 2000 & PESO < 2500 & OBITOPARTO == "2") ~ 0
    ),
    durante_peso_mais_2500 = case_when(
      (PESO >= 2500 & OBITOPARTO == "2") ~ 1,
      !(PESO >=2500 & OBITOPARTO == "2") ~ 0
    ),
    depois_peso_menos_1500 = case_when(
      (PESO < 1500 & OBITOPARTO == "3") ~ 1,
      !(PESO < 1500 & OBITOPARTO == "3") ~ 0
    ),
    depois_peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000 & OBITOPARTO == "3") ~ 1,
      !(PESO >= 1500 & PESO < 2000 & OBITOPARTO == "3") ~ 0
    ),
    depois_peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500 & OBITOPARTO == "3") ~ 1,
      !(PESO >= 2000 & PESO < 2500 & OBITOPARTO == "3") ~ 0
    ),
    depois_peso_mais_2500 = case_when(
      (PESO >= 2500 & OBITOPARTO == "3") ~ 1,
      !(PESO >=2500 & OBITOPARTO == "3") ~ 0
    )
    # peso_menos_1500_sem_menos28 = case_when(
    #   (PESO < 1500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO < 1500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_1500_1999_sem_menos28 = case_when(
    #   (PESO >= 1500 & PESO < 2000) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 1500 & PESO < 2000) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_2000_2499_sem_menos28 = case_when(
    #   (PESO >= 2000 & PESO < 2500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2000 & PESO < 2500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_mais_2500_sem_menos28 = case_when(
    #   (PESO >= 2500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2500) & (SEMAGESTAC < 28 & SEMAGESTAC != 99)) ~ 0
    # ),
    #
    # peso_menos_1500_sem_28_32 = case_when(
    #   (PESO < 1500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO < 1500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_1500_1999_sem_28_32 = case_when(
    #   (PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_2000_2499_sem_28_32 = case_when(
    #   (PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_mais_2500_sem_28_32 = case_when(
    #   (PESO >= 2500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2500) & (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_menos_1500_sem_33_34 = case_when(
    #   (PESO < 1500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO < 1500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_1500_1999_sem_33_34 = case_when(
    #   (PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_2000_2499_sem_33_34 = case_when(
    #   (PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_mais_2500_sem_33_34 = case_when(
    #   (PESO >= 2500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_menos_1500_sem_35_36 = case_when(
    #   (PESO < 1500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO < 1500) & (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_1500_1999_sem_35_36 = case_when(
    #   (PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 1500 & PESO < 2000) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_2000_2499_sem_35_36 = case_when(
    #   (PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2000 & PESO < 2500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)) ~ 0
    # ),
    # peso_mais_2500_sem_35_36 = case_when(
    #   (PESO >= 2500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)  ~ 1,
    #   !((PESO >= 2500) & (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99)) ~ 0
    # ),
    # fetal_sem_menos28 = case_when(
    #   (SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 1,
    #   !(SEMAGESTAC < 28 & SEMAGESTAC != 99)  ~ 0
    # ),
    # fetal_sem_28_32 = case_when(
    #   (SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99) ~ 1,
    #   !(SEMAGESTAC >= 28 & SEMAGESTAC <= 32 & SEMAGESTAC != 99) ~ 0
    # ),
    # fetal_sem_33_34 = case_when(
    #   (SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99) ~ 1,
    #   !(SEMAGESTAC >= 33 & SEMAGESTAC <= 34 & SEMAGESTAC != 99) ~ 0
    # ),
    # fetal_sem_35_36 = case_when(
    #   (SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99) ~ 1,
    #   !(SEMAGESTAC >= 35 & SEMAGESTAC <= 36 & SEMAGESTAC != 99) ~ 0
    # )
  ) |>
  group_by(codmunres, ano) |>
  summarise(
    obitos_fetais_mais_22sem = sum(obitos),
    fetal_peso_menos_1500 = sum(peso_menos_1500, na.rm = T),
    fetal_peso_1500_1999 = sum(peso_1500_1999, na.rm = T),
    fetal_peso_2000_2499 = sum(peso_2000_2499, na.rm = T),
    fetal_peso_mais_2500 = sum(peso_mais_2500, na.rm = T),
    fetal_sem_info_parto = sum(sem_info_parto, na.rm = T),
    fetal_antes = sum(antes, na.rm = T),
    fetal_durante = sum(durante, na.rm = T),
    fetal_depois = sum(depois, na.rm = T),
    fetal_antes_peso_menos_1500 = sum(antes_peso_menos_1500, na.rm = T),
    fetal_antes_peso_1500_1999 = sum(antes_peso_1500_1999, na.rm = T),
    fetal_antes_peso_2000_2499 = sum(antes_peso_2000_2499, na.rm = T),
    fetal_antes_peso_mais_2500 = sum(antes_peso_mais_2500, na.rm = T),
    fetal_durante_peso_menos_1500 = sum(durante_peso_menos_1500, na.rm = T),
    fetal_durante_peso_1500_1999 = sum(durante_peso_1500_1999, na.rm = T),
    fetal_durante_peso_2000_2499 = sum(durante_peso_2000_2499, na.rm = T),
    fetal_durante_peso_mais_2500 = sum(durante_peso_mais_2500, na.rm = T),
    fetal_depois_peso_menos_1500 = sum(depois_peso_menos_1500, na.rm = T),
    fetal_depois_peso_1500_1999 = sum(depois_peso_1500_1999, na.rm = T),
    fetal_depois_peso_2000_2499 = sum(depois_peso_2000_2499, na.rm = T),
    fetal_depois_peso_mais_2500 = sum(depois_peso_mais_2500, na.rm = T)

    # fetal_peso_menos_1500_sem_menos28 = sum(peso_menos_1500_sem_menos28, na.rm=T),
    # fetal_peso_1500_1999_sem_menos28 = sum(peso_1500_1999_sem_menos28, na.rm=T),
    # fetal_peso_2000_2499_sem_menos28 = sum(peso_2000_2499_sem_menos28, na.rm=T),
    # fetal_peso_mais_2500_sem_menos28  = sum(peso_mais_2500_sem_menos28, na.rm=T),
    # fetal_peso_menos_1500_sem_28_32 = sum(peso_menos_1500_sem_28_32, na.rm=T),
    # fetal_peso_1500_1999_sem_28_32 = sum(peso_1500_1999_sem_28_32, na.rm=T),
    # fetal_peso_2000_2499_sem_28_32= sum(peso_2000_2499_sem_28_32, na.rm=T),
    # fetal_peso_mais_2500_sem_28_32  = sum(peso_mais_2500_sem_28_32, na.rm=T),
    # fetal_peso_menos_1500_sem_33_34 = sum(peso_menos_1500_sem_33_34, na.rm=T),
    # fetal_peso_1500_1999_sem_33_34 = sum(peso_1500_1999_sem_33_34, na.rm=T),
    # fetal_peso_2000_2499_sem_33_34= sum(peso_2000_2499_sem_33_34, na.rm=T),
    # fetal_peso_mais_2500_sem_33_34  = sum(peso_mais_2500_sem_33_34, na.rm=T),
    # fetal_peso_menos_1500_sem_35_36 = sum(peso_menos_1500_sem_35_36, na.rm=T),
    # fetal_peso_1500_1999_sem_35_36 = sum(peso_1500_1999_sem_35_36, na.rm=T),
    # fetal_peso_2000_2499_sem_35_36= sum(peso_2000_2499_sem_35_36, na.rm=T),
    # fetal_peso_mais_2500_sem_35_36  = sum(peso_mais_2500_sem_35_36, na.rm=T),
    # fetal_sem_menos28 = sum(fetal_sem_menos28, na.rm=T),
    # fetal_sem_28_32 = sum(fetal_sem_28_32, na.rm=T),
    # fetal_sem_33_34 = sum(fetal_sem_33_34, na.rm=T),
    # fetal_sem_35_36 = sum(fetal_sem_35_36, na.rm=T)
    ) |>
  ungroup()

### Juntando com a base auxiliar de municípios e preenchendo os NAs gerados com 0
df_bloco7_fetais_originais <- left_join(df_aux_municipios, df_bloco7_fetais_aux, by = c("codmunres", "ano")) |>
  select(
    codmunres,
    ano,
    obitos_fetais_mais_22sem,
    fetal_peso_menos_1500,
    fetal_peso_1500_1999,
    fetal_peso_2000_2499,
    fetal_peso_mais_2500,
    fetal_antes,
    fetal_durante,
    fetal_depois,
    fetal_antes_peso_menos_1500,
    fetal_antes_peso_1500_1999,
    fetal_antes_peso_2000_2499,
    fetal_antes_peso_mais_2500,
    fetal_durante_peso_menos_1500,
    fetal_durante_peso_1500_1999,
    fetal_durante_peso_2000_2499,
    fetal_durante_peso_mais_2500,
    fetal_depois_peso_menos_1500,
    fetal_depois_peso_1500_1999,
    fetal_depois_peso_2000_2499,
    fetal_depois_peso_mais_2500
    # fetal_peso_menos_1500_sem_menos28,
    # fetal_peso_1500_1999_sem_menos28,
    # fetal_peso_2000_2499_sem_menos28,
    # fetal_peso_mais_2500_sem_menos28,
    # fetal_peso_menos_1500_sem_28_32,
    # fetal_peso_1500_1999_sem_28_32,
    # fetal_peso_2000_2499_sem_28_32,
    # fetal_peso_mais_2500_sem_28_32,
    # fetal_peso_menos_1500_sem_33_34,
    # fetal_peso_1500_1999_sem_33_34,
    # fetal_peso_2000_2499_sem_33_34,
    # fetal_peso_mais_2500_sem_33_34,
    # fetal_peso_menos_1500_sem_35_36,
    # fetal_peso_1500_1999_sem_35_36,
    # fetal_peso_2000_2499_sem_35_36,
    # fetal_peso_mais_2500_sem_35_36,
    # fetal_sem_menos28,
    # fetal_sem_28_32,
    # fetal_sem_33_34,
    # fetal_sem_35_36
  ) |>
  mutate(across(starts_with(c("obitos", "fetal")), ~ replace_na(.x, 0)))

### Exportando os dados
write.csv(df_bloco7_fetais_originais, 'data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2024.csv', row.names = FALSE)


## Para o indicador de causas evitáveis ---------------------------------------
### Criando vetores com as cids de cada grupo de causas evitáveis
df_cids_fetal_evitaveis2 <- read_excel("data-raw/extracao-dos-dados/blocos/databases_auxiliares/evitabilidade_fetal.xlsx", sheet = "Fetal") |>
  rename(nome = LBE_FETAL, cid = CID)

imunoprevencao2 <- filter(df_cids_fetal_evitaveis2, nome == "Imunoprevenção")$cid

mulher_gestacao2 <- filter(df_cids_fetal_evitaveis2, nome == "Reduzíveis por adequada atenção à mulher na gestação")$cid

evitaveis_parto2 <- filter(df_cids_fetal_evitaveis2, nome == "Reduzíveis por adequada atenção à mulher no parto")$cid

mal_definidas2 <- filter(df_cids_fetal_evitaveis2, nome == "Causas de morte mal-definidas")$cid

nao_aplica2 <- filter(df_cids_fetal_evitaveis2, nome == "Não se aplicam ao óbito fetal")$cid

### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando todos os momentos de óbito
df_evitaveis_fetal_todos_v2 <- df_fetais_totais |>
  clean_names() |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3),
    faixa_de_peso = case_when(
      is.na(peso) ~ "sem_informacao",
      peso < 1500 ~ "menor_1500",
      peso >= 1500 & peso < 2500 ~ "1500_a_2500",
      peso >= 2500 ~ "2500_mais"
    )
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao2 | causabas2 %in% imunoprevencao2 ~ "evitaveis_fetal_imunoprevencao2",
      causabas %in% mulher_gestacao2 | causabas2 %in% mulher_gestacao2~ "evitaveis_fetal_mulher_gestacao2",
      causabas %in% evitaveis_parto2 | causabas2 %in% evitaveis_parto2 ~ "evitaveis_fetal_parto2",
      causabas %in% nao_aplica2 | causabas2 %in% nao_aplica2 ~ "evitaveis_fetal_nao_aplica2",
      causabas %in% mal_definidas2 | causabas2 %in% mal_definidas2~ "evitaveis_fetal_mal_definidas2"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_outros2", grupo_cid)
  ) |>
  select(codmunres, ano, grupo_cid, faixa_de_peso) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = c(grupo_cid, faixa_de_peso),
    values_from = obitos,
    values_fill = 0,
    names_sort = TRUE
  ) |>
  mutate(obitos_fetais_totais = rowSums(across(starts_with("evitaveis"), ~ .x), na.rm = TRUE)) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

### Substituindo todos os NAs por 0 (gerados após o right join)
df_evitaveis_fetal_todos_v2[is.na(df_evitaveis_fetal_todos_v2)] <- 0

### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando apenas óbitos antes do parto
df_evitaveis_fetal_antes_v2 <- df_fetais_totais |>
  clean_names() |>
  filter(obitoparto == "1") |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3),
    faixa_de_peso = case_when(
      is.na(peso) ~ "sem_informacao",
      peso < 1500 ~ "menor_1500",
      peso >= 1500 & peso < 2500 ~ "1500_a_2500",
      peso >= 2500 ~ "2500_mais"
    )
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao2 | causabas2 %in% imunoprevencao2 ~ "evitaveis_fetal_antes_imunoprevencao2",
      causabas %in% mulher_gestacao2 | causabas2 %in% mulher_gestacao2~ "evitaveis_fetal_antes_mulher_gestacao2",
      causabas %in% evitaveis_parto2 | causabas2 %in% evitaveis_parto2 ~ "evitaveis_fetal_antes_parto2",
      causabas %in% nao_aplica2 | causabas2 %in% nao_aplica2 ~ "evitaveis_fetal_antes_nao_aplica2",
      causabas %in% mal_definidas2 | causabas2 %in% mal_definidas2~ "evitaveis_fetal_antes_mal_definidas2"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_antes_outros2", grupo_cid)
  ) |>
  select(codmunres, ano, grupo_cid, faixa_de_peso) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = c(grupo_cid, faixa_de_peso),
    values_from = obitos,
    values_fill = 0,
    names_sort = TRUE
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

### Substituindo todos os NAs por 0 (gerados após o right join)
df_evitaveis_fetal_antes_v2[is.na(df_evitaveis_fetal_antes_v2)] <- 0

### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando apenas óbitos durante o parto
df_evitaveis_fetal_durante_v2 <- df_fetais_totais |>
  clean_names() |>
  filter(obitoparto == "2") |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3),
    faixa_de_peso = case_when(
      is.na(peso) ~ "sem_informacao",
      peso < 1500 ~ "menor_1500",
      peso >= 1500 & peso < 2500 ~ "1500_a_2500",
      peso >= 2500 ~ "2500_mais"
    )
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao2 | causabas2 %in% imunoprevencao2 ~ "evitaveis_fetal_durante_imunoprevencao2",
      causabas %in% mulher_gestacao2 | causabas2 %in% mulher_gestacao2~ "evitaveis_fetal_durante_mulher_gestacao2",
      causabas %in% evitaveis_parto2 | causabas2 %in% evitaveis_parto2 ~ "evitaveis_fetal_durante_parto2",
      causabas %in% nao_aplica2 | causabas2 %in% nao_aplica2 ~ "evitaveis_fetal_durante_nao_aplica2",
      causabas %in% mal_definidas2 | causabas2 %in% mal_definidas2~ "evitaveis_fetal_durante_mal_definidas2"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_durante_outros2", grupo_cid)
  ) |>
  select(codmunres, ano, grupo_cid, faixa_de_peso) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = c(grupo_cid, faixa_de_peso),
    values_from = obitos,
    values_fill = 0,
    names_sort = TRUE
  ) |>
  # mutate(obitos_fetais_totais_durante = rowSums(across(starts_with("evitaveis"), ~ .x), na.rm = TRUE)) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_evitaveis_fetal_durante_v2[is.na(df_evitaveis_fetal_durante_v2)] <- 0

### Juntando os três data.frames da segunda versão de causas evitáveis
df_bloco7_fetais_evitaveis_v2 <- full_join(
  df_evitaveis_fetal_todos_v2,
  full_join(df_evitaveis_fetal_antes_v2, df_evitaveis_fetal_durante_v2)
)

### Removendo objetos já utilizados
rm(df_evitaveis_fetal_todos_v2, df_evitaveis_fetal_antes_v2, df_evitaveis_fetal_durante_v2)
gc()

### Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_fetal <- full_join(df_bloco7_distribuicao_cids_fetal, df_bloco7_fetais_evitaveis_v2)


## Para o indicador de grupos de causas ------------------------------------
### Criando vetores com as cids de cada grupo
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

### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando todos os momentos de óbito
df_fetais_grupos_todos <- df_fetais_totais |>
  clean_names() |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3),
    faixa_de_peso = case_when(
      is.na(peso) ~ "sem_informacao",
      peso < 1500 ~ "menor_1500",
      peso >= 1500 & peso < 2500 ~ "1500_a_2500",
      peso >= 2500 ~ "2500_mais"
    )
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_grupos_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_grupos_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_grupos_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_grupos_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_grupos_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_grupos_afeccoes_perinatal",
      causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao ~ "fetal_grupos_ma_formacao",
      causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida ~ "fetal_grupos_mal_definida",
      TRUE ~ "fetal_grupos_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid, faixa_de_peso) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = c(grupo_cid, faixa_de_peso),
    values_from = obitos,
    values_fill = 0,
    names_sort = TRUE
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

### Substituindo todos os NAs por 0 (gerados após o right join)
df_fetais_grupos_todos[is.na(df_fetais_grupos_todos)] <- 0

### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando apenas óbitos antes do parto
df_fetais_grupos_antes <- df_fetais_totais |>
  clean_names() |>
  filter(obitoparto == "1") |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3),
    faixa_de_peso = case_when(
      is.na(peso) ~ "sem_informacao",
      peso < 1500 ~ "menor_1500",
      peso >= 1500 & peso < 2500 ~ "1500_a_2500",
      peso >= 2500 ~ "2500_mais"
    )
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_grupos_antes_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_grupos_antes_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_grupos_antes_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_grupos_antes_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_grupos_antes_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_grupos_antes_afeccoes_perinatal",
      causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao~ "fetal_grupos_antes_ma_formacao",
      causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida~ "fetal_grupos_antes_mal_definida",
      TRUE ~ "fetal_grupos_antes_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid, faixa_de_peso) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = c(grupo_cid, faixa_de_peso),
    values_from = obitos,
    values_fill = 0,
    names_sort = TRUE
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

### Substituindo todos os NAs por 0 (gerados após o right join)
df_fetais_grupos_antes[is.na(df_fetais_grupos_antes)] <- 0


### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando apenas óbitos durante o parto
df_fetais_grupos_durante <- df_fetais_totais |>
  clean_names() |>
  filter(obitoparto == "2") |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3),
    faixa_de_peso = case_when(
      is.na(peso) ~ "sem_informacao",
      peso < 1500 ~ "menor_1500",
      peso >= 1500 & peso < 2500 ~ "1500_a_2500",
      peso >= 2500 ~ "2500_mais"
    )
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_grupos_durante_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_grupos_durante_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_grupos_durante_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_grupos_durante_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_grupos_durante_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_grupos_durante_afeccoes_perinatal",
      causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao~ "fetal_grupos_durante_ma_formacao",
      causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida~ "fetal_grupos_durante_mal_definida",
      TRUE ~ "fetal_grupos_durante_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid, faixa_de_peso) |>
  mutate(obitos = 1) |>
  group_by(across(!obitos)) |>
  summarise(obitos = sum(obitos)) |>
  ungroup() |>
  pivot_wider(
    names_from = c(grupo_cid, faixa_de_peso),
    values_from = obitos,
    values_fill = 0,
    names_sort = TRUE
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_fetais_grupos_durante[is.na(df_fetais_grupos_durante)] <- 0

### Juntando os três data.frames de grupos de causas
df_bloco7_fetais_grupos <- full_join(
  df_fetais_grupos_todos,
  full_join(df_fetais_grupos_antes, df_fetais_grupos_durante)
)

### Removendo objetos já utilizados
rm(df_fetais_grupos_todos, df_fetais_grupos_antes, df_fetais_grupos_durante)
gc()

### Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_fetal <- full_join(df_bloco7_distribuicao_cids_fetal, df_bloco7_fetais_grupos)

### Exportando os dados
write.csv(df_bloco7_distribuicao_cids_fetal, "data-raw/csv/indicadores_bloco7_distribuicao_cids_fetal_2012-2024.csv", row.names = FALSE)




################# INDICADORES DA ABA PERINATAL

# Óbitos fetais de idade gestacional maior ou igual a 28 semanas por ano e município

df_fetais_28sem <- df_fetais_totais2 |>
  mutate(SEMAGESTAC = as.numeric(SEMAGESTAC), PESO = as.numeric(PESO)) |>
  filter(
    ((GESTACAO == "28 a 31 semanas" | GESTACAO == "32 a 36 semanas" | GESTACAO == "37 a 41 semanas" | GESTACAO == "42 semanas e mais") | (is.na(GESTACAO) & SEMAGESTAC >= 28 & SEMAGESTAC != 99)) | (PESO >= 1000)
  )|>
  mutate(
    ano = substr(DTOBITO, 1, 4),
    codmunres = as.numeric(CODMUNRES)
  ) |>
  mutate(
    peso_menos_1500 = case_when(
      PESO < 1500 ~ 1,
      !(PESO < 1500) ~ 0
    ),
    peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000) ~ 1,
      !(PESO >= 1500 & PESO < 2000) ~ 0
    ),
    peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500) ~ 1,
      !(PESO >= 2000 & PESO < 2500) ~ 0
    ),
    peso_mais_2500 = case_when(
      (PESO >= 2500) ~ 1,
      !(PESO >=2500) ~ 0
    ),
    antes = case_when(
      OBITOPARTO == "1" ~ 1,
      !(OBITOPARTO == "1") ~ 0
    ),
    durante = case_when(
      (OBITOPARTO == "2") ~ 1,
      !(OBITOPARTO == "2") ~ 0
    ),
    antes_peso_menos_1500 = case_when(
      (PESO < 1500 & OBITOPARTO == "1") ~ 1,
      !(PESO < 1500 & OBITOPARTO == "1") ~ 0
    ),
    antes_peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000 & OBITOPARTO == "1") ~ 1,
      !(PESO >= 1500 & PESO < 2000 & OBITOPARTO == "1") ~ 0
    ),
    antes_peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500 & OBITOPARTO == "1") ~ 1,
      !(PESO >= 2000 & PESO < 2500 & OBITOPARTO == "1") ~ 0
    ),
    antes_peso_mais_2500 = case_when(
      (PESO >= 2500 & OBITOPARTO == "1") ~ 1,
      !(PESO >=2500 & OBITOPARTO == "1") ~ 0
    ),
    durante_peso_menos_1500 = case_when(
      (PESO < 1500 & OBITOPARTO == "2") ~ 1,
      !(PESO < 1500 & OBITOPARTO == "2") ~ 0
    ),
    durante_peso_1500_1999 = case_when(
      (PESO >= 1500 & PESO < 2000 & OBITOPARTO == "2") ~ 1,
      !(PESO >= 1500 & PESO < 2000 & OBITOPARTO == "2") ~ 0
    ),
    durante_peso_2000_2499 = case_when(
      (PESO >= 2000 & PESO < 2500 & OBITOPARTO == "2") ~ 1,
      !(PESO >= 2000 & PESO < 2500 & OBITOPARTO == "2") ~ 0
    ),
    durante_peso_mais_2500 = case_when(
      (PESO >= 2500 & OBITOPARTO == "2") ~ 1,
      !(PESO >=2500 & OBITOPARTO == "2") ~ 0
    ),
    obitos = 1,
    .after = PESO
  )|>
  group_by(ano, codmunres) |>
  summarise(obitos_fetais_mais_28sem = sum(obitos),
            peso_menos_1500_mais_28sem = sum(peso_menos_1500, na.rm=T),
            peso_1500_1999_mais_28sem = sum(peso_1500_1999, na.rm=T),
            peso_2000_2499_mais_28sem = sum(peso_2000_2499, na.rm=T),
            peso_mais_2500_mais_28sem = sum(peso_mais_2500, na.rm=T),
            perinatal_antes = sum(antes, na.rm = T),
            perinatal_durante = sum(durante, na.rm = T),
            perinatal_antes_peso_menos_1500 = sum(antes_peso_menos_1500, na.rm = T),
            perinatal_antes_peso_1500_1999 = sum(antes_peso_1500_1999, na.rm = T),
            perinatal_antes_peso_2000_2499 = sum(antes_peso_2000_2499, na.rm = T),
            perinatal_antes_peso_mais_2500 = sum(antes_peso_mais_2500, na.rm = T),
            perinatal_durante_peso_menos_1500 = sum(durante_peso_menos_1500, na.rm = T),
            perinatal_durante_peso_1500_1999 = sum(durante_peso_1500_1999, na.rm = T),
            perinatal_durante_peso_2000_2499 = sum(durante_peso_2000_2499, na.rm = T),
            perinatal_durante_peso_mais_2500 = sum(durante_peso_mais_2500, na.rm = T)) |>
  ungroup()

############ Juntando as bases de dados
df_fetais_28sem <- df_fetais_28sem |> mutate_if(is.character, as.numeric)

juncao <- left_join(df_aux_municipios, df_fetais_28sem, by=c("codmunres", "ano"))

juncao[is.na(juncao)] <- 0


# juncao$obitos_fetais_mais_28sem[is.na(juncao$obitos_fetais_mais_28sem)] <- 0
# juncao$peso_menos_1500_mais_28sem[is.na(juncao$peso_menos_1500_mais_28sem)] <- 0
# juncao$peso_1500_1999_mais_28sem[is.na(juncao$peso_1500_1999_mais_28sem)] <- 0
# juncao$peso_2000_2499_mais_28sem[is.na(juncao$peso_2000_2499_mais_28sem)] <- 0
# juncao$peso_mais_2500_mais_28sem[is.na(juncao$peso_mais_2500_mais_28sem)] <- 0

df_obitos_perinatais <- juncao %>%
  select(
    codmunres,
    ano,
    obitos_fetais_mais_28sem,
    peso_menos_1500_mais_28sem,
    peso_1500_1999_mais_28sem,
    peso_2000_2499_mais_28sem,
    peso_mais_2500_mais_28sem,
    perinatal_antes,
    perinatal_durante,
    perinatal_antes_peso_menos_1500,
    perinatal_antes_peso_1500_1999,
    perinatal_antes_peso_2000_2499,
    perinatal_antes_peso_mais_2500,
    perinatal_durante_peso_menos_1500,
    perinatal_durante_peso_1500_1999,
    perinatal_durante_peso_2000_2499,
    perinatal_durante_peso_mais_2500
  )

# df_obitos_perinatais_antigo <- read_csv("data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2023.csv") |>
#   filter(ano <= 2021) |>
#   select(-c(`...1`))
#
# df_obitos_perinatais_novo <- rbind(df_obitos_perinatais_antigo, df_obitos_perinatais)

write.table(df_obitos_perinatais, 'data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


######### INDICADORES DA ABA A NEONATAL

df_neonat_total <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  #vars = c("CODMUNRES", "DTOBITO", "IDADE", "PESO"),
  information_system = "SIM-DOINF"
)

df_sim_2023 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv")

sim23 <- df_sim_2023 |> select(-c(contador, OPOR_DO, TP_ALTERA, CB_ALT))

df_sim_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN+(2).csv")

sim24 <- df_sim_2024 |> select(-c(contador, OPOR_DO, TP_ALTERA, CB_ALT))

df_neonat_total <- df_neonat_total |> select(-c(ESTABDESCR, NUDIASOBIN,
                                                              NUDIASINF, FONTESINF,
                                                              CONTADOR))

preliminares <- rbind(sim23, sim24)

# df_sim_total2 <- process_sim(rbind(df_neonat_total, preliminares), municipality_data = T) |>
#   select(
#     CODMUNRES, DTOBITO, IDADEminutos, IDADEhoras, IDADEdias, PESO
#   )

df_sim_total2 <- rbind(df_neonat_total, preliminares) |>
  select(
    CODMUNRES, DTOBITO, IDADE, PESO
  )

df_neonat_total3 <- df_sim_total2  |>
  mutate(
    ano1 = substr(DTOBITO, 5, 8),
    codmunres = as.numeric(CODMUNRES),
    IDADE = as.numeric(IDADE),
    PESO = as.numeric(PESO)
  ) |>
  mutate(
     ano = case_when(
       ano1 == "023" ~ "2023",
       ano1 == "024" ~ "2024",
       TRUE ~ ano1
     ),
    obitos_27dias = case_when(
      (IDADE <= 227) ~ 1,
      !(IDADE <= 227) ~ 0
    ),
    obitos_27dias_menos1500 = case_when(
      ((IDADE <= 227) & PESO <1500)  ~ 1,
      !((IDADE <= 227) & PESO < 1500) ~ 0
    ),

    obitos_27dias_1500_1999 = case_when(
      ((IDADE <= 227) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 227) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_27dias_2000_2499 = case_when(
      ((IDADE <= 227) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 227) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_27dias_mais2500 = case_when(
      ((IDADE <= 227) & PESO >= 2500)  ~ 1,
      !((IDADE <= 227) & PESO >= 2500) ~ 0
    ),

    obitos_6dias = case_when(

      (IDADE <= 206) ~ 1,
      !(IDADE <= 206) ~ 0
    ),

    obitos_6dias_menos1500 = case_when(
      ((IDADE <= 206) & PESO <1500)  ~ 1,
      !((IDADE <= 206) & PESO < 1500) ~ 0
    ),

    obitos_6dias_1500_1999 = case_when(
      ((IDADE <= 206) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 206) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_6dias_2000_2499 = case_when(
      ((IDADE <= 206) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 206) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_6dias_mais2500 = case_when(
      ((IDADE <= 206) & PESO >= 2500)  ~ 1,
      !((IDADE <= 206) & PESO >= 2500) ~ 0
    ),

    obitos_0dias = case_when(

      (IDADE <= 200 ) ~ 1,
      !(IDADE <= 200 ) ~ 0
    ),

    obitos_0dias_menos1500 = case_when(
      ((IDADE <= 200 ) & PESO <1500)  ~ 1,
      !((IDADE <= 200 ) & PESO < 1500) ~ 0
    ),

    obitos_0dias_1500_1999 = case_when(
      ((IDADE <= 200 ) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 200 ) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_0dias_2000_2499 = case_when(
      ((IDADE <= 200 ) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 200 ) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_0dias_mais2500 = case_when(
      ((IDADE <= 200 ) & PESO >= 2500)  ~ 1,
      !((IDADE <= 200 ) & PESO >= 2500) ~ 0
    ),

    obitos_1_6dias = case_when(

      (IDADE <= 206 & IDADE >= 201) ~ 1,
      !(IDADE <= 206 & IDADE >= 201) ~ 0
    ),

    obitos_1_6dias_menos1500 = case_when(
      ((IDADE <= 206 & IDADE >= 201) & PESO <1500)  ~ 1,
      !((IDADE <= 206 & IDADE >= 201) & PESO < 1500) ~ 0
    ),

    obitos_1_6dias_1500_1999 = case_when(
      ((IDADE <= 206 & IDADE >= 201) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 206 & IDADE >= 201) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_1_6dias_2000_2499 = case_when(
      ((IDADE <= 206 & IDADE >= 201) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 206 & IDADE >= 201) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_1_6dias_mais2500 = case_when(
      ((IDADE <= 206 & IDADE >= 201) & PESO >= 2500)  ~ 1,
      !((IDADE <= 206 & IDADE >= 201) & PESO >= 2500) ~ 0
    ),

    obitos_7_27dias = case_when(

      (IDADE <= 227 & IDADE >= 207) ~ 1,
      !(IDADE <= 227 & IDADE >= 207) ~ 0
    ),

    obitos_7_27dias_menos1500 = case_when(
      ((IDADE <= 227 & IDADE >= 207) & PESO <1500)  ~ 1,
      !((IDADE <= 227 & IDADE >= 207) & PESO < 1500) ~ 0
    ),

    obitos_7_27dias_1500_1999 = case_when(
      ((IDADE <= 227 & IDADE >= 207) & PESO >= 1500 & PESO <= 1999)  ~ 1,
      !((IDADE <= 227 & IDADE >= 207) & PESO >= 1500 & PESO <= 1999) ~ 0
    ),

    obitos_7_27dias_2000_2499 = case_when(
      ((IDADE <= 227 & IDADE >= 207) & PESO >= 2000 & PESO <= 2499)  ~ 1,
      !((IDADE <= 227 & IDADE >= 207) & PESO >= 2000 & PESO <= 2499) ~ 0
    ),

    obitos_7_27dias_mais2500 = case_when(
      ((IDADE <= 227 & IDADE >= 207) & PESO >= 2500)  ~ 1,
      !((IDADE <= 227 & IDADE >= 207) & PESO >= 2500) ~ 0
    )

  ) |>
  select(-c(ano1)) |>
  group_by(codmunres, ano) |>
  summarise(

    obitos_27dias = sum( obitos_27dias,na.rm=T),
    obitos_27dias_menos1500 = sum( obitos_27dias_menos1500,na.rm=T),
    obitos_27dias_1500_1999 = sum( obitos_27dias_1500_1999,na.rm=T),
    obitos_27dias_2000_2499 = sum(obitos_27dias_2000_2499 ,na.rm=T),
    obitos_27dias_mais2500 = sum(obitos_27dias_mais2500 ,na.rm=T),
    obitos_6dias = sum(obitos_6dias ,na.rm=T),
    obitos_6dias_menos1500 = sum(obitos_6dias_menos1500 ,na.rm=T),
    obitos_6dias_1500_1999 = sum(obitos_6dias_1500_1999 ,na.rm=T),
    obitos_6dias_2000_2499 = sum(obitos_6dias_2000_2499 ,na.rm=T),
    obitos_6dias_mais2500 = sum(obitos_6dias_mais2500 ,na.rm=T),
    obitos_0dias = sum(obitos_0dias ,na.rm=T),
    obitos_0dias_menos1500 = sum(obitos_0dias_menos1500 ,na.rm=T),
    obitos_0dias_1500_1999 = sum(obitos_0dias_1500_1999 ,na.rm=T),
    obitos_0dias_2000_2499 = sum(obitos_0dias_2000_2499 ,na.rm=T),
    obitos_0dias_mais2500 = sum(obitos_0dias_mais2500 ,na.rm=T),
    obitos_1_6dias = sum(obitos_1_6dias ,na.rm=T),
    obitos_1_6dias_menos1500 = sum(obitos_1_6dias_menos1500 ,na.rm=T),
    obitos_1_6dias_1500_1999 = sum(obitos_1_6dias_1500_1999 ,na.rm=T),
    obitos_1_6dias_2000_2499 = sum(obitos_1_6dias_2000_2499 ,na.rm=T),
    obitos_1_6dias_mais2500 = sum(obitos_1_6dias_mais2500 ,na.rm=T),
    obitos_7_27dias = sum(obitos_7_27dias ,na.rm=T),
    obitos_7_27dias_menos1500 = sum(obitos_7_27dias_menos1500 ,na.rm=T),
    obitos_7_27dias_1500_1999 = sum(obitos_7_27dias_1500_1999 ,na.rm=T),
    obitos_7_27dias_2000_2499 = sum(obitos_7_27dias_2000_2499 ,na.rm=T),
    obitos_7_27dias_mais2500 = sum(obitos_7_27dias_mais2500 ,na.rm=T)
  )|>
  ungroup()


df_nascidos_total1_aux <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  vars = c("CODMUNRES", "DTNASC", "PESO", "GESTACAO", "SEMAGESTAC", "APGAR5"),
  information_system = "SINASC"
)

df_nascidos_total1 <- df_nascidos_total1_aux |>
  select(CODMUNRES, DTNASC, PESO)

options(timeout = 600)
sinasc23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";")
sinasc23_aux <- sinasc23 |>
  select(CODMUNRES, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5)

sinasc23 <- sinasc23 |>
  select(CODMUNRES, DTNASC, PESO)

sinasc24 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN24.csv", sep = ";")
sinasc24_aux <- sinasc24 |>
  select(CODMUNRES, DTNASC, PESO, GESTACAO, SEMAGESTAC, APGAR5)

sinasc24 <- sinasc24 |>
  select(CODMUNRES, DTNASC, PESO)

df_nascidos_total <- rbind(df_nascidos_total1, sinasc23) |>
  rbind(sinasc24)

df_nascidos_total2 <- process_sinasc(df_nascidos_total, municipality_data = T)

#write.csv(df_nascidos_total2, file="Bloco_7/Databases/dados_sinasc_neonat.csv")

df_nascidos_total3 <- df_nascidos_total2 |>
  mutate(
    ano = substr(DTNASC, 1, 4),
    codmunres = as.numeric(CODMUNRES),
    PESO = as.numeric(PESO)
  ) |>
  mutate(
    nascidos = 1,
    nascidos_menos1500 = case_when(
      (PESO < 1500) ~ 1,
      !(PESO < 1500) ~ 0
    ),
    nascidos_1500_1999 = case_when(
      (PESO >=  1500 & PESO<=1999) ~ 1,
      !((PESO >=  1500 & PESO<=1999)) ~ 0
    ),

    nascidos_2000_2499 = case_when(
      (PESO >=  2000 & PESO<=2499) ~ 1,
      !(PESO >=  2000 & PESO<=2499) ~ 0
    ),

    nascidos_mais2500 = case_when(
      (PESO >=  2500) ~ 1,
      !(PESO >=  2500) ~ 0
    )
  ) |>
  group_by(codmunres, ano) |>
  summarise(

    nascidos = sum(nascidos, na.rm=T),
    nascidos_menos1500 = sum(nascidos_menos1500, na.rm = T),
    nascidos_1500_1999 = sum(nascidos_1500_1999, na.rm = T),
    nascidos_2000_2499 = sum(nascidos_2000_2499, na.rm = T),
    nascidos_mais2500 = sum(nascidos_mais2500, na.rm = T)
  ) |>
  ungroup()

df_juncao <- left_join(df_nascidos_total3, df_neonat_total3, by = c("codmunres", "ano"))

df_juncao <- df_juncao |> mutate_if(is.character, as.numeric)

df_juncao[is.na(df_juncao)] <- 0

df_neonat <- left_join(df_aux_municipios, df_juncao, by=c("codmunres", "ano")) |>
  select(
    codmunres,
    ano,
    nascidos,
    nascidos_menos1500,
    nascidos_1500_1999,
    nascidos_2000_2499,
    nascidos_mais2500,
    obitos_27dias,
    obitos_6dias,
    obitos_7_27dias,
    obitos_27dias_menos1500,
    obitos_27dias_1500_1999,
    obitos_27dias_2000_2499,
    obitos_27dias_mais2500,
    obitos_6dias_menos1500,
    obitos_6dias_1500_1999,
    obitos_6dias_2000_2499,
    obitos_6dias_mais2500,
    obitos_7_27dias_menos1500,
    obitos_7_27dias_1500_1999,
    obitos_7_27dias_2000_2499,
    obitos_7_27dias_mais2500,
    obitos_0dias,
    obitos_0dias_menos1500,
    obitos_0dias_1500_1999,
    obitos_0dias_2000_2499,
    obitos_0dias_mais2500,
    obitos_1_6dias,
    obitos_1_6dias_menos1500,
    obitos_1_6dias_1500_1999,
    obitos_1_6dias_2000_2499,
    obitos_1_6dias_mais2500
  )

df_neonat_antigo <- read_csv("data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2023.csv") |>
  filter(ano <= 2021) |>
  select(-c(`...1`))

df_neonat_novo <- rbind(df_neonat_antigo, df_neonat)

write.csv(df_neonat_novo, 'data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


######### INDICADORES DE MORBIDADE NEONATAL

library(janitor)
library(RSQLite)
library(glue)
library(tidyr)
library(data.table)
library(readr)

df_nascidos_total_aux <- rbind(df_nascidos_total1_aux, sinasc23_aux) |>
  rbind(sinasc24_aux)


df_ameacadoras <- df_nascidos_total_aux |>
  mutate(
    ano = as.numeric(substr(DTNASC, nchar(DTNASC) - 3, nchar(DTNASC))),
    PESO = as.numeric(PESO),
    GESTACAO = as.numeric(GESTACAO),
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    APGAR5 = as.numeric(APGAR5),
    .keep = "unused",
  ) |>
  mutate(
    nascidos_condicoes_ameacadoras = if_else(PESO < 1500 | (GESTACAO < 4 | SEMAGESTAC < 32) | APGAR5 < 7, 1, 0, missing = 0),
    .keep = "unused"
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise_at(vars(contains("nascidos")), sum) |>
  ungroup()

# Para os indicadores provenientes do SIH ---------------------------------
## Criando um vetor com os anos considerados
anos <- c(2012:2024)

## Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

procedimentos_parto <- c("0310010012", "0310010039", "0310010047",
                         "0310010055", "0411010026", "0411010034",
                         "0411010042")

## Baixando os dados do SIH-RD
for (estado in estados) {
  # Criando data.frames que guardarão as bases do estado
  df_sih_rd_menores_28_uf <- data.frame()
  df_sih_rd_partos_uf <- data.frame()

  for (ano in anos) {

    erro_rd <- TRUE
    while (erro_rd) {
      erro_rd <- tryCatch({
        # Baixando os dados do SIH-RD para o dado ano e UF
        df_sih_rd_aux <- fetch_datasus(
          year_start = ano,
          year_end = ano,
          uf = estado,
          month_start = 1,
          month_end = 12,
          information_system = "SIH-RD",
          timeout = 500,
          stop_on_error = TRUE,
          vars = c(
            "CNES", "CEP", "MUNIC_RES", "MUNIC_MOV", "ANO_CMPT", "COD_IDADE", "IDADE", "NASC",
            "DT_INTER", "DT_SAIDA", "COBRANCA", "N_AIH", "DIAG_PRINC", "PROC_REA",
            "US_TOT", "UTI_MES_TO"
          )
        )

        # Criando um data.frame que contém apenas as internações de menores de 28 dias
        df_sih_rd_aux_menores_28 <- df_sih_rd_aux |>
          mutate(idade_dias = as.numeric(as.Date(DT_INTER, format = "%Y%m%d") - as.Date(NASC, format = "%Y%m%d"))) |>
          dplyr::filter(
            idade_dias < 28
          )

        # Criando um data.frame que contém apenas os partos
         df_sih_rd_aux_partos <- df_sih_rd_aux |>
           dplyr::filter(
             ((DIAG_PRINC >= "O32" & DIAG_PRINC <= "O36") | (DIAG_PRINC >= "O60" & DIAG_PRINC <= "O69") |
                (DIAG_PRINC >= "O75" & DIAG_PRINC < "O76") | (DIAG_PRINC >= "O80" & DIAG_PRINC <= "O84") |
                DIAG_PRINC == "P95") | (PROC_REA %in% procedimentos_parto)
           )

        erro_rd <- FALSE
      },
      warning = function(cond) return(TRUE)
      )
    }

    # Juntando com os dados dos anos anteriores para a dada UF
    df_sih_rd_menores_28_uf <- bind_rows(df_sih_rd_menores_28_uf, df_sih_rd_aux_menores_28)
    #df_sih_rd_partos_uf <- bind_rows(df_sih_rd_partos_uf, df_sih_rd_aux_partos)


    # Limpando a memória
    rm(df_sih_rd_aux_menores_28#, df_sih_rd_aux_partos
       )
    gc()
  }

  # Salvando as bases da UF
  write.csv2(
    df_sih_rd_menores_28_uf,
    gzfile(glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  write.csv2(
    df_sih_rd_partos_uf,
    gzfile(glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  # Limpando a memória
  rm(df_sih_rd_uf)
  gc()
}

## Criando os data.frames que guardarão as bases finais
df_sih_rd_menores_28 <- data.frame()
df_sih_rd_partos <- data.frame()

for (estado in estados) {
  df_sih_rd_menores_28_aux <- fread(
    glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_menores_28_dias_2012_2024.csv.gz"),
    sep = ";"
  )
  df_sih_rd_menores_28 <- bind_rows(df_sih_rd_menores_28, df_sih_rd_menores_28_aux)

  rm(df_sih_rd_menores_28_aux)
  gc()
}

for (estado in estados) {
  df_sih_rd_partos_aux <- fread(
    glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/{estado}_sih_rd_partos_2022_2024.csv.gz"),
    sep = ";"
  )
  df_sih_rd_partos <- bind_rows(df_sih_rd_partos, df_sih_rd_partos_aux)

  rm(df_sih_rd_partos_aux)
  gc()
}

## Salvando as bases completas
write.csv2(
  df_sih_rd_menores_28,
  glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)

write.csv2(
  df_sih_rd_partos,
  glue("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_{anos[1]}_{anos[length(anos)]}.csv"),
  row.names = FALSE
)


## Para os numeradores dos indicadores (número de internações/internações em UTI em menores de 28 dias) ----
### Lendo uma base com informações auxiliares dos municípios
df_infos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_aux_municipios.csv") |>
  mutate_if(is.numeric, as.character)

### Rodando o algoritmo da Claudia na base completa de internações em menores de 28 dias
#### Criando um vetor que contém o diretório original do projeto
diretorio_original <- getwd()

#### Criando um vetor que contém o diretório das bases brutas do SIH-RD
diretorio_bases_brutas <- glue("{getwd()}/data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH")

#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de internações
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_menores_28_dias_2012_2024.csv"))

#### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

#### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih_internacoes_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_internacoes <- left_join(
  df_aih_internacoes_aux,
  df_sih_rd_menores_28 |>
    select(ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV, idade_dias) |>
    mutate_at(vars(c(DT_INTER, DT_SAIDA, N_AIH, MUNIC_MOV)), as.character)
)

### Passando os casos para o formato wide (cada linha corresponderá a uma pessoa única)
df_aih_internacoes_wide <- df_aih_internacoes |>
  mutate(
    DT_INTER = as.Date(DT_INTER, format = "%Y%m%d"),
    DT_SAIDA = as.Date(DT_SAIDA, format = "%Y%m%d"),
    NASC = as.Date(NASC, format = "%Y%m%d")
  ) |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    CNES = first(CNES),  # CNES do estabelecimento da primeira internação
    MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
    MUNIC_MOV = first(MUNIC_MOV),  # Município do estabelecimento da primeira internação
    idade_dias = first(idade_dias),  # Idade, em dias, na data da primeira internação
    SOMA_UTI = sum(as.integer(UTI_MES_TO)),  # Total de dias na UTI
    PDIAG = first(DIAG_PRINC),  # Diagnóstico principal da primeira internação
    PPROC = first(PROC_REA)
  ) |>
  ungroup() |>
  select(ano = ANO_CMPT, codmunres = MUNIC_RES, causabas = PDIAG, codmunocor = MUNIC_MOV, cnes = CNES, aihref = AIHREF, idade_dias, soma_uti_mes_to = SOMA_UTI) |>
  # Filtrando apenas pelos casos em que os municípios de residência e ocorrência são considerados no painel
  filter(codmunres %in% df_infos_municipios$codmunres & codmunocor %in% df_infos_municipios$codmunres)

### Adicionando as indicadores de internação em UTI e de internação na macrorregião de residência
df_aih_internacoes_wide_macros <- df_aih_internacoes_wide |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_res = macro_r_saude), by = join_by(codmunres == codmunres)) |>
  left_join(df_infos_municipios |> select(codmunres, macro_r_saude_ocor = macro_r_saude), by = join_by(codmunocor == codmunres)) |>
  mutate(
    idade_cat = case_when(
      idade_dias == 0 ~ "0_dias",
      idade_dias >= 1 & idade_dias < 7 ~ "1_a_6_dias",
      idade_dias >= 7 & idade_dias <= 27 ~ "7_a_27_dias",
      TRUE ~ NA_character_
    )        #ifelse(idade_dias < 7, "menores_7_dias", "7_a_27_dias"),
,
    indicadora_mesma_macro = ifelse(macro_r_saude_res == macro_r_saude_ocor, "na_macro", "fora_macro"),
    indicadora_uti = ifelse(soma_uti_mes_to > 0, "internado_uti", "nao_internado_uti")
  ) |>
  select(!c(cnes, codmunocor, aihref, idade_dias, soma_uti_mes_to, macro_r_saude_res, macro_r_saude_ocor))

### Passando a base para o formato wide (um município por linha) e criando as variáveis necessárias
df_bloco5_sih_internacoes <- df_aih_internacoes_wide_macros |>
  group_by_all() |>
  summarise(num_internacoes = n()) |>
  ungroup() |>
  pivot_wider(
    names_from = c(indicadora_mesma_macro, idade_cat, indicadora_uti),
    values_from = num_internacoes,
    values_fill = 0,
    names_prefix = "internacoes_"
  ) |>
  right_join(data.frame(codmunres = rep(df_infos_municipios$codmunres, each = length(2022:2024)), ano = 2022:2024)) |>
  arrange(codmunres, ano) |>
  mutate(across(.cols = -c(codmunres, ano), .fns = ~ replace_na(., 0))) |>
  rowwise() |>
  mutate(
    # Para o indicador de internações geral, os nomes das variáveis seguem o padrão "internacoes_local-do-parto_idade-do-bebe"
    internacoes_na_macro_7_a_27_dias = sum(c_across(contains("na_macro_7_a_27_dias"))),
    internacoes_fora_macro_7_a_27_dias = sum(c_across(contains("fora_macro_7_a_27_dias"))),
    internacoes_na_macro_1_a_6_dias = sum(c_across(contains("na_macro_1_a_6_dias"))),
    internacoes_fora_macro_1_a_6_dias = sum(c_across(contains("fora_macro_1_a_6_dias"))),
    internacoes_na_macro_0_dias = sum(c_across(contains("na_macro_0_dias"))),
    internacoes_fora_macro_0_dias = sum(c_across(contains("fora_macro_0_dias"))),
    internacoes_geral_7_a_27_dias = internacoes_na_macro_7_a_27_dias + internacoes_fora_macro_7_a_27_dias,
    internacoes_geral_1_a_6_dias = internacoes_na_macro_1_a_6_dias + internacoes_fora_macro_1_a_6_dias,
    internacoes_geral_0_dias = internacoes_na_macro_0_dias + internacoes_fora_macro_0_dias,
    internacoes_na_macro_geral = internacoes_na_macro_7_a_27_dias + internacoes_na_macro_1_a_6_dias + internacoes_na_macro_0_dias,
    internacoes_fora_macro_geral = internacoes_fora_macro_7_a_27_dias + internacoes_fora_macro_1_a_6_dias + internacoes_fora_macro_0_dias,
    internacoes_geral_geral = internacoes_geral_7_a_27_dias + internacoes_geral_1_a_6_dias + internacoes_geral_0_dias,
    # Para o indicador de internações em UTI, os nomes das variáveis seguem o padrão "internacoes_local-do-parto_idade-do-bebe_internado_uti"
    internacoes_na_macro_geral_internado_uti = sum(c_across(contains("na_macro") & contains("dias_internado"))),
    internacoes_fora_macro_geral_internado_uti = sum(c_across(contains("fora_macro") & contains("dias_internado"))),
    internacoes_geral_7_a_27_dias_internado_uti = sum(c_across(contains("7_a_27_dias_internado"))),
    internacoes_geral_1_a_6_dias_internado_uti = sum(c_across(contains("1_a_6_dias_internado"))),
    internacoes_geral_0_dias_internado_uti = sum(c_across(contains("0_dias_internado"))),
    internacoes_geral_geral_internado_uti = internacoes_geral_7_a_27_dias_internado_uti + internacoes_geral_1_a_6_dias_internado_uti + internacoes_geral_0_dias_internado_uti
  ) |>
  select(ano, codmunres, ends_with("_6_dias"), ends_with("_27_dias"), ends_with("_0_dias"), ends_with("_geral"), ends_with("internado_uti") & !ends_with("nao_internado_uti"))

### Verificando se o total de internações equivale ao número de linhas das bases df_aih_wide/df_aih_wide_macros
sum(df_bloco5_sih_internacoes$internacoes_geral_geral) == nrow(df_aih_internacoes_wide_macros)

sum(df_bloco5_sih_internacoes$internacoes_geral_geral_internado_uti) == nrow(df_aih_internacoes_wide[df_aih_internacoes_wide$soma_uti_mes_to > 0, ])

sum(df_bloco5_sih_internacoes$internacoes_na_macro_geral_internado_uti, df_bloco5_sih_internacoes$internacoes_fora_macro_geral_internado_uti) ==
  sum(df_bloco5_sih_internacoes$internacoes_geral_7_a_27_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_1_a_6_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_0_dias_internado_uti)

sum(df_bloco5_sih_internacoes$internacoes_geral_7_a_27_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_1_a_6_dias_internado_uti, df_bloco5_sih_internacoes$internacoes_geral_0_dias_internado_uti) ==
  sum(df_bloco5_sih_internacoes$internacoes_geral_geral_internado_uti)


## Para o denominador dos indicadores (total de partos públicos) -----------
### Rodando o algoritmo da Claudia na base completa de partos
#### Mudando o diretório para a pasta que contém o algoritmo em C++
setwd("data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/")

#### Rodando o algoritmo em C++ na base de partos
system(glue("./processaih {diretorio_bases_brutas}/BR_sih_rd_partos_2022_2024.csv"))

 #### Voltando para o diretório original do projeto
setwd(diretorio_original)

#### Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
con <- dbConnect(SQLite(), "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite")

#### Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
df_aih_partos_aux <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")
dbDisconnect(con)

### Adicionando variáveis que estão no SIH-RD, mas que não são devolvidas na base gerada pelo algoritmo
df_aih_partos <- left_join(
  df_aih_partos_aux,
  df_sih_rd_partos |>
    select(ANO_CMPT, DT_INTER, DT_SAIDA, N_AIH) |>
    mutate_at(vars(c(DT_INTER, DT_SAIDA, N_AIH)), as.character)
)

### Passando as casos para o formato wide (cada linha corresponderá a uma pessoa única)
df_bloco5_sih_partos <- df_aih_partos |>
  group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  summarise(
    ANO_CMPT = last(ANO_CMPT),  # Ano de processamento do SIH da última internação
    MUNIC_RES = first(MUNIC_RES),  # Município de residência da primeira internação
  ) |>
  ungroup() |>
  select(ano = ANO_CMPT, codmunres = MUNIC_RES, aihref = AIHREF) |>
  filter(codmunres %in% df_infos_municipios$codmunres) |>
  mutate(nascidos_estabelecimentos_publicos_sih = 1) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  # Criando a variável que contém o número nascimentos em hospitais públicos em cada município
  summarise(nascidos_estabelecimentos_publicos_sih = sum(nascidos_estabelecimentos_publicos_sih)) |>
  ungroup()

### Removendo arquivos já utilizados e que são maiores que o limite de 100 mb
file.remove(c(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_menores_28_dias_2012_2024.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/SIH/BR_sih_rd_partos_2012_2024.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/aihperm.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/aihpermtransf.csv",
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/internacoes_menores_28_dias/algorithm_episode_of_care/work.sqlite"

))

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

df_bloco7_morbidade_neonatal <- left_join(df_aux_municipios, df_ameacadoras, by = c("codmunres", "ano")) |>
  left_join(df_bloco5_sih_partos) |>
  left_join(df_bloco5_sih_internacoes)

## Preenchendo os valores NAs, gerados após o left_join, com 0 (MENOS PARA 2023 PARA AS COLUNAS QUE VEM DO SIH, QUE AINDA NÃO FORAM ATUALIZADAS)
internacoes_cols <- grep("^internacoes|sih$", names(df_bloco7_morbidade_neonatal), value = TRUE)

for (col in names(df_bloco7_morbidade_neonatal)) {
  if (col %in% internacoes_cols) {
    # Nas colunas que começam com "internacoes" ou que terminam com "sih", substituir os NAs por 0 apenas se o ano não for 2023
    df_bloco7_morbidade_neonatal[[col]][is.na(df_bloco7_morbidade_neonatal[[col]])] <- 0 #& df_bloco7_morbidade_neonatal$ano != 2023
  } else {
    # Nas outras colunas, substituir todos os NAs por 0
    df_bloco7_morbidade_neonatal[[col]][is.na(df_bloco7_morbidade_neonatal[[col]])] <- 0
  }
}

df_bloco7_morbidade_neonatal_antigo <- read_csv("data-raw/csv/indicadores_bloco7_morbidade_neonatal_2012-2023.csv") |>
  filter(ano <= 2021)

df_bloco7_morbidade_neonatal_novo <- rbind(df_bloco7_morbidade_neonatal_antigo, df_bloco7_morbidade_neonatal)

write.csv(df_bloco7_morbidade_neonatal_novo, 'data-raw/csv/indicadores_bloco7_morbidade_neonatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)


############ Dados para a distribuição de internações neonatais

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

# Novo agrupamento para a distribuição de morbidade

cids_internacoes_neonatais <- read_csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/cids_internacoes_neonatais.csv") |>
  select(causabas = CID,
         grupos = `CONSIDERAR ESTA  COLUNA -REVISÃO COM CINTIA E TATIANE EM 1/10/2024 Grupo da rede interagencial moficado para causa de internação neonatal (fluxograma na aba 'orientacoes')`)

for(i in unique(cids_internacoes_neonatais$grupos)){
  nome_variavel <- tolower(i)
  nome_variavel <- gsub(" ", "_", nome_variavel)
  assign(nome_variavel, filter(cids_internacoes_neonatais, grupos == i)$causabas)
}


df_internacoes_neonatais_totais <- df_aih_internacoes_wide |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  filter(!(causabas %in% excluir | causabas2 %in% excluir)) |>
  select(codmunres, ano) |>
  mutate(internacoes_neonatais_totais = 1) |>
  group_by(across(!internacoes_neonatais_totais)) |>
  summarise(internacoes_neonatais_totais = sum(internacoes_neonatais_totais)) |>
  ungroup()

# internacoes_neonatais_grupos <- df_aih_internacoes_wide |>
#   mutate(
#     causabas = causabas,
#     causabas2 = substr(causabas, 1 , 3)
#   ) |>
#   mutate(
#     grupo_cid = case_when(
#       causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "morbidade_neonatal_grupos_prematuridade",
#       causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "morbidade_neonatal_grupos_infeccoes",
#       causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "morbidade_neonatal_grupos_asfixia",
#       causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "morbidade_neonatal_grupos_respiratorias",
#       causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "morbidade_neonatal_grupos_gravidez",
#       #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
#       causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "morbidade_neonatal_grupos_afeccoes_perinatal",
#       causabas >= "Q00" & causabas <= "Q99" ~ "morbidade_neonatal_grupos_ma_formacao",
#       causabas >= "R00" & causabas <= "R99" ~ "morbidade_neonatal_grupos_mal_definida",
#       TRUE ~ "morbidade_neonatal_grupos_outros"
#     )
#   ) |>
#   select(codmunres, ano, grupo_cid) |>
#   mutate(internacoes = 1) |>
#   group_by(across(!internacoes)) |>
#   summarise(internacoes = sum(internacoes)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = grupo_cid,
#     values_from = internacoes,
#     values_fill = 0
#   ) |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres)

internacoes_neonatais_grupos <- df_aih_internacoes_wide |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(internacoes = 1) |>
  group_by(across(!internacoes)) |>
  summarise(internacoes = sum(internacoes)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

internacoes_neonatais_grupos[is.na(internacoes_neonatais_grupos)] <- 0


internacoes_neonatais_grupos_0_dias <- df_aih_internacoes_wide|>
  filter(idade_dias == 0) |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_0_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_0_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_0_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_0_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_0_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_0_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_0_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_0_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_0_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_0_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_0_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_0_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_0_dias_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(internacoes = 1) |>
  group_by(across(!internacoes)) |>
  summarise(internacoes = sum(internacoes)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

internacoes_neonatais_grupos_0_dias[is.na(internacoes_neonatais_grupos_0_dias)] <- 0


internacoes_neonatais_grupos_7_27_dias <- df_aih_internacoes_wide |>
  filter(idade_dias >= 7 & idade_dias <= 27)|>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_7_27_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_7_27_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_7_27_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_7_27_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_7_27_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_7_27_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_7_27_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_7_27_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_7_27_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_7_27_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_7_27_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_7_27_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_7_27_dias_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(internacoes = 1) |>
  group_by(across(!internacoes)) |>
  summarise(internacoes = sum(internacoes)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

internacoes_neonatais_grupos_7_27_dias[is.na(internacoes_neonatais_grupos_7_27_dias)] <- 0

internacoes_neonatais_grupos_1_6_dias <- df_aih_internacoes_wide |>
  filter(idade_dias >= 1 & idade_dias <= 6)|>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% infecções | causabas2 %in% infecções | causabas %in% infecção | causabas2 %in% infecção ~ "morbidade_neonatal_grupos_1_6_dias_infeccoes",
      causabas %in% `afecções_respiratórias_do_recém-nascido` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` | causabas %in% `afecções_respiratórias_dos_recém-nascidos` | causabas2 %in% `afecções_respiratórias_do_recém-nascido` ~ "morbidade_neonatal_grupos_1_6_dias_afeccoes_respiratorias",
      causabas %in% fatores_maternos_relacionados_à_gravidez | causabas2 %in% fatores_maternos_relacionados_à_gravidez~ "morbidade_neonatal_grupos_1_6_dias_fatores_maternos",
      causabas %in% `asfixia_/_hipóxia` | causabas2 %in% `asfixia_/_hipóxia` ~ "morbidade_neonatal_grupos_1_6_dias_asfixia",
      causabas %in% prematuridade | causabas2 %in% prematuridade ~ "morbidade_neonatal_grupos_1_6_dias_prematuridade",
      causabas %in% afecções_não_especificadas_do_período_perinatal | causabas %in% afecções_originais_no_período_perinatal| causabas %in% afecções_não_especificadas_originadas_no_período_perinatal |  causabas2 %in% afecções_não_especificadas_do_período_perinatal | causabas2 %in% afecções_originais_no_período_perinatal | causabas2 %in% causabas %in% afecções_não_especificadas_originadas_no_período_perinatal ~ "morbidade_neonatal_grupos_1_6_dias_afeccoes_perinatal",
      causabas %in% transtornos_cardíacos_originados_no_período_perinatal | causabas2 %in% transtornos_cardíacos_originados_no_período_perinatal ~ "morbidade_neonatal_grupos_1_6_dias_cardiacos_perinatal",
      causabas %in% icterícia_neonatal | causabas2 %in% icterícia_neonatal ~ "morbidade_neonatal_grupos_1_6_dias_ictericia",
      causabas %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido`| causabas2 %in% `transtornos_endócrinos_e_metabólicos_transitórios_específicos_do_feto_e_do_recém-nascido` ~ "morbidade_neonatal_grupos_1_6_dias_endocrinos",
      causabas %in% problemas_de_alimentação_do_rn | causabas2 %in% problemas_de_alimentação_do_rn ~ "morbidade_neonatal_grupos_1_6_dias_alimentacao",
      causabas %in% má_formação_congênita | causabas2 %in% má_formação_congênita ~ "morbidade_neonatal_grupos_1_6_dias_ma_formacao",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "morbidade_neonatal_grupos_1_6_dias_mal_definidas",
      TRUE ~ "morbidade_neonatal_grupos_1_6_dias_outros"
    )
  ) |>
  select(codmunres, ano, grupo_cid) |>
  mutate(internacoes = 1) |>
  group_by(across(!internacoes)) |>
  summarise(internacoes = sum(internacoes)) |>
  ungroup() |>
  pivot_wider(
    names_from = grupo_cid,
    values_from = internacoes,
    values_fill = 0
  ) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

internacoes_neonatais_grupos_1_6_dias[is.na(internacoes_neonatais_grupos_1_6_dias)] <- 0

############ Juntandos os dados para a aba de morbidade


df_distribuicao_morbidade <- left_join(internacoes_neonatais_grupos, internacoes_neonatais_grupos_0_dias, by=c("codmunres", "ano"))|>
  left_join(internacoes_neonatais_grupos_1_6_dias, by=c("codmunres", "ano")) |>
  left_join(internacoes_neonatais_grupos_7_27_dias, by=c("codmunres", "ano")) |>
  left_join(df_internacoes_neonatais_totais, by = c("codmunres", "ano"))


write.csv(df_distribuicao_morbidade, 'data-raw/csv/indicadores_bloco7_distribuicao_morbidade_neonatal_2012-2024.csv', sep = ",", dec = ".", row.names = FALSE)







############ ABA NEONATAL - DISTRIBUIÇÕES DE ÓBITOS


df_sim_doinf_aux1 <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  information_system = "SIM-DOINF",
) |>
  clean_names()

df_sim_doinf_aux1 <- df_sim_doinf_aux1|>
  select(-c(estabdescr, nudiasobin, nudiasinf, fontesinf, codmuncart, codcart, numregcart,
            dtregcart, expdifdata, crm, dtrecorig))

df_sim_2023 <- df_sim_2023 |>
  clean_names() |>
  select(-c(opor_do, tp_altera, cb_alt))

df_sim_2024 <- df_sim_2024 |>
  clean_names() |>
  select(-c(opor_do, tp_altera, cb_alt))


df_sim_doinf_aux <- rbind(df_sim_doinf_aux1, df_sim_2023) |>
  rbind(df_sim_2024)


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


## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_totais_precoce)


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

df_neonatais_precoce_totais_peso <- df_sim_doinf |>
  mutate(
    faixa_de_peso = case_when(
      as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
      as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
      as.numeric(peso) > 1500 ~ "> 1500 g"
    )
  ) |>
  filter(idade < 207 | as.numeric(peso) < 1000) |>
  select(codmunres, ano, faixa_de_peso) |>
  mutate(obitos_neonatais_precoce_totais = 1) |>
  group_by(across(!obitos_neonatais_precoce_totais)) |>
  summarise(obitos_neonatais_precoce_totais = sum(obitos_neonatais_precoce_totais)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)


### Criando vetores com as cids de cada grupo de causas evitáveis
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


df_neonatais_evitaveis <- df_sim_doinf |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_neonatal_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_neonatal_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_neonatal_tratamento",
      causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_neonatal_mal_definidas"
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


df_neonatais_precoce_evitaveis <- df_sim_doinf |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao  | causabas2 %in% imunoprevencao~ "evitaveis_neonatal_precoce_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_precoce_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto~ "evitaveis_neonatal_precoce_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_precoce_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento~ "evitaveis_neonatal_precoce_tratamento",
      causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_precoce_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas ~ "evitaveis_neonatal_precoce_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_precoce_outros", grupo_cid),
  ) |>
  filter(idade < 207 | as.numeric(peso) < 1000) |>
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
  right_join(df_neonatais_totais_precoce) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_precoce_evitaveis[is.na(df_neonatais_precoce_evitaveis)] <- 0

df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_precoce_evitaveis)


df_neonatais_evitaveis_0_dias <- df_sim_doinf_0_dias |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_neonatal_0_dias_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_0_dias_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_neonatal_0_dias_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_0_dias_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_neonatal_0_dias_tratamento",
      causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_0_dias_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_neonatal_0_dias_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_0_dias_outros", grupo_cid)#,
    # faixa_de_peso = case_when(
    #   as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
    #   as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
    #   as.numeric(peso) > 1500 ~ "> 1500 g"
    # )
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
  right_join(df_neonatais_totais_0_dias) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres) #|>
#mutate(faixa_de_peso = ifelse(is.na(faixa_de_peso), "não se aplica", faixa_de_peso))

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_evitaveis_0_dias[is.na(df_neonatais_evitaveis_0_dias)] <- 0

df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_evitaveis_0_dias)

df_neonatais_evitaveis_1_6_dias <- df_sim_doinf_1_6_dias |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_neonatal_1_6_dias_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_1_6_dias_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_neonatal_1_6_dias_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_1_6_dias_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_neonatal_1_6_dias_tratamento",
      causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_1_6_dias_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_neonatal_1_6_dias_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_1_6_dias_outros", grupo_cid)#,
    # faixa_de_peso = case_when(
    #   as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
    #   as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
    #   as.numeric(peso) > 1500 ~ "> 1500 g"
    # )
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
  right_join(df_neonatais_totais_1_6_dias) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres) #|>
#mutate(faixa_de_peso = ifelse(is.na(faixa_de_peso), "não se aplica", faixa_de_peso))

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_evitaveis_1_6_dias[is.na(df_neonatais_evitaveis_1_6_dias)] <- 0

df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_evitaveis_1_6_dias)

df_neonatais_evitaveis_7_27_dias <- df_sim_doinf_7_27_dias |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_neonatal_7_27_dias_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao ~ "evitaveis_neonatal_7_27_dias_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_neonatal_7_27_dias_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_neonatal_7_27_dias_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_neonatal_7_27_dias_tratamento",
      causabas %in% saude | causabas2 %in% saude~ "evitaveis_neonatal_7_27_dias_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_neonatal_7_27_dias_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_neonatal_7_27_dias_outros", grupo_cid)#,
    # faixa_de_peso = case_when(
    #   as.numeric(peso) <= 1000 | is.na(peso) ~ "outros",
    #   as.numeric(peso) > 1000 & as.numeric(peso) <= 1500 ~ "> 1000 g",
    #   as.numeric(peso) > 1500 ~ "> 1500 g"
    # )
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
  right_join(df_neonatais_totais_7_27_dias) |>
  right_join(df_aux_municipios) |>
  arrange(codmunres) #|>
#mutate(faixa_de_peso = ifelse(is.na(faixa_de_peso), "não se aplica", faixa_de_peso))

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_evitaveis_7_27_dias[is.na(df_neonatais_evitaveis_7_27_dias)] <- 0

df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_evitaveis_7_27_dias)

#gráfico grupos de causa para óbitos neonatais

df_neonatais_grupos <- df_sim_doinf |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_grupos_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_grupos_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "neonat_grupos_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "neonat_grupos_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_grupos_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_grupos_afeccoes_perinatal",
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

df_neonatais_precoce_grupos <- df_sim_doinf |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_precoce_grupos_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_precoce_grupos_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "neonat_precoce_grupos_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias~ "neonat_precoce_grupos_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_precoce_grupos_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "neonat_precoce_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_precoce_grupos_afeccoes_perinatal",
      causabas >= "Q00" & causabas <= "Q99" ~ "neonat_precoce_grupos_ma_formacao",
      causabas >= "R00" & causabas <= "R99" ~ "neonat_precoce_grupos_mal_definida",
      TRUE ~ "neonat_precoce_grupos_outros"
    )
  ) |>
  filter(idade < 207 | as.numeric(peso) < 1000) |>
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
  right_join(df_neonatais_totais_precoce) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_precoce_grupos[is.na(df_neonatais_precoce_grupos)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_precoce_grupos)

df_neonatais_grupos_0_dias <- df_sim_doinf_0_dias |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_grupos_0_dias_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_grupos_0_dias_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "neonat_grupos_0_dias_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "neonat_grupos_0_dias_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_grupos_0_dias_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_grupos_0_dias_afeccoes_perinatal",
      causabas >= "Q00" & causabas <= "Q99" ~ "neonat_grupos_0_dias_ma_formacao",
      causabas >= "R00" & causabas <= "R99" ~ "neonat_grupos_0_dias_mal_definida",
      TRUE ~ "neonat_grupos_0_dias_outros"
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
  right_join(df_neonatais_totais_0_dias) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_grupos_0_dias[is.na(df_neonatais_grupos_0_dias)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_grupos_0_dias)


df_neonatais_grupos_1_6_dias <- df_sim_doinf_1_6_dias |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_grupos_1_6_dias_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_grupos_1_6_dias_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "neonat_grupos_1_6_dias_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "neonat_grupos_1_6_dias_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_grupos_1_6_dias_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_grupos_1_6_dias_afeccoes_perinatal",
      causabas >= "Q00" & causabas <= "Q99" ~ "neonat_grupos_1_6_dias_ma_formacao",
      causabas >= "R00" & causabas <= "R99" ~ "neonat_grupos_1_6_dias_mal_definida",
      TRUE ~ "neonat_grupos_1_6_dias_outros"
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
  right_join(df_neonatais_totais_1_6_dias) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_grupos_1_6_dias[is.na(df_neonatais_grupos_1_6_dias)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_grupos_1_6_dias)


df_neonatais_grupos_7_27_dias <- df_sim_doinf_7_27_dias |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "neonat_grupos_7_27_dias_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes~ "neonat_grupos_7_27_dias_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia~ "neonat_grupos_7_27_dias_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "neonat_grupos_7_27_dias_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "neonat_grupos_7_27_dias_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "neonat_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal ~ "neonat_grupos_7_27_dias_afeccoes_perinatal",
      causabas >= "Q00" & causabas <= "Q99" ~ "neonat_grupos_7_27_dias_ma_formacao",
      causabas >= "R00" & causabas <= "R99" ~ "neonat_grupos_7_27_dias_mal_definida",
      TRUE ~ "neonat_grupos_7_27_dias_outros"
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
  right_join(df_neonatais_totais_7_27_dias) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_neonatais_grupos_7_27_dias[is.na(df_neonatais_grupos_7_27_dias)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_grupos_7_27_dias)


## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_neonatal <- left_join(df_bloco7_distribuicao_cids_neonatal, df_neonatais_grupos)

df_bloco7_distribuicao_cids_neonatal[is.na(df_bloco7_distribuicao_cids_neonatal)] <- 0


############ ABA PERINATAL - DISTRIBUIÇÃO DE ÓBITOS #############

## Criando a variável de ano, limitando a variável 'causabas' a três caracteres e filtrando apenas pelos óbitos fetais que consideramos
df_perinat <- df_sim_dofet_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito)))
  ) |>
  filter(
    ((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 28 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 1000)
  )

df_sim_perinat_antes <- df_sim_dofet_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    obitoparto = as.numeric(obitoparto)
  ) |>
  filter(
    (((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 28 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 1000)) & obitoparto == 1
  )

df_sim_perinat_durante <- df_sim_dofet_aux |>
  mutate(
    ano = as.numeric(substr(dtobito, nchar(dtobito) - 3, nchar(dtobito))),
    obitoparto = as.numeric(obitoparto)
  ) |>
  filter(
    (((gestacao != "1" & !is.na(gestacao) & gestacao != "9") | (as.numeric(semagestac) >= 28 & as.numeric(semagestac) != 99)) | (as.numeric(peso) >= 1000)) & obitoparto == 2
  )

## Criando um data.frame com os óbitos fetais totais
df_perinat_totais <- df_perinat |>
  select(codmunres, ano) |>
  mutate(obitos_perinat_totais = 1) |>
  group_by(across(!obitos_perinat_totais)) |>
  summarise(obitos_perinat_totais = sum(obitos_perinat_totais)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_perinat_totais[is.na(df_perinat_totais)] <- 0

## Criando um data.frame com os óbitos fetais totais antes do parto
df_perinat_totais_antes <- df_sim_perinat_antes |>
  select(codmunres, ano) |>
  mutate(obitos_perinat_totais_antes = 1) |>
  group_by(across(!obitos_perinat_totais_antes)) |>
  summarise(obitos_perinat_totais_antes = sum(obitos_perinat_totais_antes)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_perinat_totais_antes[is.na(df_perinat_totais_antes)] <- 0

## Criando um data.frame com os óbitos fetais totais durante o parto
df_perinat_totais_durante <- df_perinat |>
  select(codmunres, ano) |>
  mutate(obitos_perinat_totais_durante = 1) |>
  group_by(across(!obitos_perinat_totais_durante)) |>
  summarise(obitos_perinat_totais_durante = sum(obitos_perinat_totais_durante)) |>
  ungroup() |>
  right_join(df_aux_municipios) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_perinat_totais_durante[is.na(df_perinat_totais_durante)] <- 0

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
df_evitaveis_perinat <- df_perinat |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_fetal_28sem_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao~ "evitaveis_fetal_28sem_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_fetal_28sem_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_fetal_28sem_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_fetal_28sem_tratamento",
      causabas %in% saude | causabas2 %in% saude ~ "evitaveis_fetal_28sem_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_fetal_28sem_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_28sem_outros", grupo_cid)
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
  right_join(df_perinat_totais) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_evitaveis_perinat[is.na(df_evitaveis_perinat)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_perinatal <- left_join(df_bloco7_distribuicao_cids_perinatal, df_evitaveis_perinat)

df_evitaveis_perinat_antes <- df_sim_perinat_antes |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_fetal_28sem_antes_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao~ "evitaveis_fetal_28sem_antes_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_fetal_28sem_antes_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_fetal_28sem_antes_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_fetal_28sem_antes_tratamento",
      causabas %in% saude | causabas2 %in% saude~ "evitaveis_fetal_28sem_antes_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_fetal_28sem_antes_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_28sem_antes_outros", grupo_cid)
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
  right_join(df_perinat_totais_antes) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_evitaveis_perinat_antes[is.na(df_evitaveis_perinat_antes)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_perinatal <- left_join(df_bloco7_distribuicao_cids_perinatal, df_evitaveis_perinat_antes)

df_evitaveis_perinat_durante <- df_sim_perinat_durante |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% imunoprevencao | causabas2 %in% imunoprevencao ~ "evitaveis_fetal_28sem_durante_imunoprevencao",
      causabas %in% mulher_gestacao | causabas2 %in% mulher_gestacao~ "evitaveis_fetal_28sem_durante_mulher_gestacao",
      causabas %in% evitaveis_parto | causabas2 %in% evitaveis_parto ~ "evitaveis_fetal_28sem_durante_parto",
      causabas %in% recem_nascido | causabas2 %in% recem_nascido ~ "evitaveis_fetal_28sem_durante_recem_nascido",
      causabas %in% tratamento | causabas2 %in% tratamento ~ "evitaveis_fetal_28sem_durante_tratamento",
      causabas %in% saude | causabas2 %in% saude~ "evitaveis_fetal_28sem_durante_saude",
      causabas %in% mal_definidas | causabas2 %in% mal_definidas~ "evitaveis_fetal_28sem_durante_mal_definidas"
    ),
    grupo_cid = ifelse(is.na(grupo_cid), "evitaveis_fetal_28sem_durante_outros", grupo_cid)
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
  right_join(df_perinat_totais_durante) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_evitaveis_perinat_durante[is.na(df_evitaveis_perinat_durante)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_perinatal <- left_join(df_bloco7_distribuicao_cids_perinatal, df_evitaveis_perinat_durante)

df_perinat_grupos <- df_perinat |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_28sem_grupos_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_28sem_grupos_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_28sem_grupos_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_28sem_grupos_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_28sem_grupos_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_28sem_grupos_afeccoes_perinatal",
      causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao ~ "fetal_28sem_grupos_ma_formacao",
      causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida ~ "fetal_28sem_grupos_mal_definida",
      TRUE ~ "fetal_28sem_grupos_outros"
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
  right_join(df_perinat_totais) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_perinat_grupos[is.na(df_perinat_grupos)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_perinatal <- left_join(df_bloco7_distribuicao_cids_perinatal, df_perinat_grupos)

df_perinat_grupos_antes <- df_sim_perinat_antes |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_28sem_grupos_antes_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_28sem_grupos_antes_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_28sem_grupos_antes_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_28sem_grupos_antes_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_28sem_grupos_antes_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_28sem_grupos_antes_afeccoes_perinatal",
      causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao~ "fetal_28sem_grupos_antes_ma_formacao",
      causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida~ "fetal_28sem_grupos_antes_mal_definida",
      TRUE ~ "fetal_28sem_grupos_antes_outros"
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
  right_join(df_perinat_totais_antes) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_perinat_grupos_antes[is.na(df_perinat_grupos_antes)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_perinatal <- left_join(df_bloco7_distribuicao_cids_perinatal, df_perinat_grupos_antes)


df_perinat_grupos_durante <- df_sim_perinat_durante |>
  mutate(
    causabas = causabas,
    causabas2 = substr(causabas, 1 , 3)
  ) |>
  mutate(
    grupo_cid = case_when(
      causabas %in% grupos_prematuridade | causabas2 %in% grupos_prematuridade ~ "fetal_28sem_grupos_durante_prematuridade",
      causabas %in% grupos_infeccoes | causabas2 %in% grupos_infeccoes  ~ "fetal_28sem_grupos_durante_infeccoes",
      causabas %in% grupos_asfixia | causabas2 %in% grupos_asfixia ~ "fetal_28sem_grupos_durante_asfixia",
      causabas %in% grupos_respiratorias | causabas2 %in% grupos_respiratorias ~ "fetal_28sem_grupos_durante_respiratorias",
      causabas %in% grupos_gravidez | causabas2 %in% grupos_gravidez ~ "fetal_28sem_grupos_durante_gravidez",
      #causabas %in% grupos_cardiorrespiratoria ~ "fetal_grupos_cardiorrespiratoria",
      causabas %in% grupos_afeccoes_perinatal | causabas2 %in% grupos_afeccoes_perinatal~ "fetal_28sem_grupos_durante_afeccoes_perinatal",
      causabas %in% grupos_ma_formacao | causabas2 %in% grupos_ma_formacao~ "fetal_28sem_grupos_durante_ma_formacao",
      causabas %in% grupos_mal_definida | causabas2 %in% grupos_mal_definida~ "fetal_28sem_grupos_durante_mal_definida",
      TRUE ~ "fetal_28sem_grupos_durante_outros"
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
  right_join(df_perinat_totais_durante) |>
  arrange(codmunres)

## Substituindo todos os NAs por 0 (gerados após o right join)
df_perinat_grupos_durante[is.na(df_fetais_grupos_durante)] <- 0

## Juntando com o restante da base de causas evitáveis e grupos de causa
df_bloco7_distribuicao_cids_perinatal <- left_join(df_bloco7_distribuicao_cids_perinatal, df_perinat_grupos_durante)



df_bloco7_distribuicao_cids_perinatal <- full_join(
    df_bloco7_distribuicao_cids_perinatal,
    df_bloco7_distribuicao_cids_neonatal
  ) |>
  mutate(
    obitos_perinatais_totais = obitos_neonatais_precoce_totais + obitos_perinat_totais,
    evitaveis_perinatal_mulher_gestacao = evitaveis_neonatal_precoce_mulher_gestacao + evitaveis_fetal_28sem_mulher_gestacao,
    evitaveis_perinatal_parto = evitaveis_neonatal_precoce_parto + evitaveis_fetal_28sem_parto,
    evitaveis_perinatal_recem_nascido = evitaveis_neonatal_precoce_recem_nascido + evitaveis_fetal_28sem_recem_nascido,
    evitaveis_perinatal_tratamento = evitaveis_neonatal_precoce_tratamento,
    evitaveis_perinatal_saude = evitaveis_neonatal_precoce_saude ,
    evitaveis_perinatal_mal_definidas = evitaveis_neonatal_precoce_mal_definidas + evitaveis_fetal_28sem_mal_definidas,
    evitaveis_perinatal_outros = evitaveis_neonatal_precoce_outros + evitaveis_fetal_28sem_outros,
    perinatal_grupos_prematuridade = neonat_precoce_grupos_prematuridade + fetal_28sem_grupos_prematuridade,
    perinatal_grupos_infeccoes = neonat_precoce_grupos_infeccoes,
    perinatal_grupos_asfixia = neonat_precoce_grupos_asfixia + fetal_28sem_grupos_asfixia,
    perinatal_grupos_ma_formacao = neonat_precoce_grupos_ma_formacao + fetal_28sem_grupos_ma_formacao,
    perinatal_grupos_respiratorias = neonat_precoce_grupos_respiratorias,
    perinatal_grupos_gravidez = neonat_precoce_grupos_gravidez + fetal_28sem_grupos_gravidez,
    perinatal_grupos_afeccoes_perinatal = neonat_precoce_grupos_afeccoes_perinatal + fetal_28sem_grupos_afeccoes_perinatal,
    perinatal_grupos_mal_definida = neonat_precoce_grupos_mal_definida,
    perinatal_grupos_outros = neonat_precoce_grupos_outros + fetal_28sem_grupos_outros,
    evitaveis_perinatal_antes_mulher_gestacao = evitaveis_fetal_28sem_antes_mulher_gestacao,
    evitaveis_perinatal_antes_parto = evitaveis_fetal_28sem_antes_parto,
    evitaveis_perinatal_antes_recem_nascido = evitaveis_fetal_28sem_antes_recem_nascido,
    #evitaveis_perinatal_antes_tratamento = evitaveis_neonatal_precoce_tratamento,
    #evitaveis_perinatal_antes_saude = evitaveis_neonatal_precoce_saude ,
    evitaveis_perinatal_antes_mal_definidas = evitaveis_fetal_28sem_antes_mal_definidas,
    evitaveis_perinatal_antes_outros = evitaveis_fetal_28sem_antes_outros,
    perinatal_grupos_antes_prematuridade = fetal_28sem_grupos_antes_prematuridade,
    #perinatal_grupos_antes_infeccoes = neonat_precoce_grupos_infeccoes,
    perinatal_grupos_antes_asfixia = fetal_28sem_grupos_antes_asfixia,
    perinatal_grupos_antes_ma_formacao = fetal_28sem_grupos_antes_ma_formacao,
    #perinatal_grupos_antes_respiratorias = neonat_precoce_grupos_respiratorias,
    perinatal_grupos_antes_gravidez = fetal_28sem_grupos_antes_gravidez,
    #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
    perinatal_grupos_antes_afeccoes_perinatal = fetal_28sem_grupos_antes_afeccoes_perinatal,
    #perinatal_grupos_antes_mal_definida = neonat_precoce_grupos_mal_definida,
    perinatal_grupos_antes_outros = fetal_28sem_grupos_antes_outros,

    #evitaveis_perinatal_durante_imunoprevencao = evitaveis_fetal_durante_imunoprevencao,
    evitaveis_perinatal_durante_mulher_gestacao = evitaveis_fetal_28sem_durante_mulher_gestacao,
    evitaveis_perinatal_durante_parto = evitaveis_fetal_28sem_durante_parto,
    evitaveis_perinatal_durante_recem_nascido = evitaveis_fetal_28sem_durante_recem_nascido,
    #evitaveis_perinatal_antes_tratamento = evitaveis_neonatal_precoce_tratamento,
    #evitaveis_perinatal_antes_saude = evitaveis_neonatal_precoce_saude ,
    evitaveis_perinatal_durante_mal_definidas = evitaveis_fetal_28sem_durante_mal_definidas,
    evitaveis_perinatal_durante_outros = evitaveis_fetal_28sem_durante_outros,
    perinatal_grupos_durante_prematuridade = fetal_28sem_grupos_durante_prematuridade,
    #perinatal_grupos_antes_infeccoes = neonat_precoce_grupos_infeccoes,
    perinatal_grupos_durante_asfixia = fetal_28sem_grupos_durante_asfixia,
    perinatal_grupos_durante_ma_formacao = fetal_28sem_grupos_durante_ma_formacao,
    #perinatal_grupos_antes_respiratorias = neonat_precoce_grupos_respiratorias,
    perinatal_grupos_durante_gravidez = fetal_28sem_grupos_durante_gravidez,
    #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
    perinatal_grupos_durante_afeccoes_perinatal = fetal_28sem_grupos_durante_afeccoes_perinatal,
    #perinatal_grupos_antes_mal_definida = neonat_precoce_grupos_mal_definida,
    perinatal_grupos_durante_outros = fetal_28sem_grupos_durante_outros,


    #evitaveis_perinatal_0_dias_imunoprevencao = evitaveis_neonatal_0_dias_imunoprevencao,
    evitaveis_perinatal_0_dias_mulher_gestacao = evitaveis_neonatal_0_dias_mulher_gestacao ,
    evitaveis_perinatal_0_dias_parto = evitaveis_neonatal_0_dias_parto,
    evitaveis_perinatal_0_dias_recem_nascido = evitaveis_neonatal_0_dias_recem_nascido,
    evitaveis_perinatal_0_dias_tratamento = evitaveis_neonatal_0_dias_tratamento,
    evitaveis_perinatal_0_dias_saude = evitaveis_neonatal_0_dias_saude ,
    evitaveis_perinatal_0_dias_mal_definidas = evitaveis_neonatal_0_dias_mal_definidas,
    evitaveis_perinatal_0_dias_outros = evitaveis_neonatal_0_dias_outros,
    perinatal_grupos_0_dias_prematuridade = neonat_grupos_0_dias_prematuridade,
    perinatal_grupos_0_dias_infeccoes = neonat_grupos_0_dias_infeccoes,
    perinatal_grupos_0_dias_asfixia = neonat_grupos_0_dias_asfixia,
    perinatal_grupos_0_dias_ma_formacao = neonat_grupos_0_dias_ma_formacao,
    perinatal_grupos_0_dias_respiratorias = neonat_grupos_0_dias_respiratorias,
    perinatal_grupos_0_dias_gravidez = neonat_grupos_0_dias_gravidez,
    #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
    perinatal_grupos_0_dias_afeccoes_perinatal = neonat_grupos_0_dias_afeccoes_perinatal,
    perinatal_grupos_0_dias_mal_definida = neonat_grupos_0_dias_mal_definida,
    perinatal_grupos_0_dias_outros = neonat_grupos_0_dias_outros,

    evitaveis_perinatal_1_6_dias_imunoprevencao = evitaveis_neonatal_1_6_dias_imunoprevencao,
    evitaveis_perinatal_1_6_dias_mulher_gestacao = evitaveis_neonatal_1_6_dias_mulher_gestacao ,
    evitaveis_perinatal_1_6_dias_parto = evitaveis_neonatal_1_6_dias_parto,
    evitaveis_perinatal_1_6_dias_recem_nascido = evitaveis_neonatal_1_6_dias_recem_nascido,
    evitaveis_perinatal_1_6_dias_tratamento = evitaveis_neonatal_1_6_dias_tratamento,
    evitaveis_perinatal_1_6_dias_saude = evitaveis_neonatal_1_6_dias_saude ,
    evitaveis_perinatal_1_6_dias_mal_definidas = evitaveis_neonatal_1_6_dias_mal_definidas,
    evitaveis_perinatal_1_6_dias_outros = evitaveis_neonatal_1_6_dias_outros,
    perinatal_grupos_1_6_dias_prematuridade = neonat_grupos_1_6_dias_prematuridade,
    perinatal_grupos_1_6_dias_infeccoes = neonat_grupos_1_6_dias_infeccoes,
    perinatal_grupos_1_6_dias_asfixia = neonat_grupos_1_6_dias_asfixia,
    perinatal_grupos_1_6_dias_ma_formacao = neonat_grupos_1_6_dias_ma_formacao,
    perinatal_grupos_1_6_dias_respiratorias = neonat_grupos_1_6_dias_respiratorias,
    perinatal_grupos_1_6_dias_gravidez = neonat_grupos_1_6_dias_gravidez,
    #perinatal_grupos_cardiorrespiratorias = neonat_precoce_grupos_cardiorrespiratorias + fetal_grupos_cardiorrespiratorias,
    perinatal_grupos_afeccoes_perinatal = neonat_grupos_1_6_dias_afeccoes_perinatal,
    perinatal_grupos_1_6_dias_mal_definida = neonat_grupos_1_6_dias_mal_definida,
    perinatal_grupos_1_6_dias_outros = neonat_grupos_1_6_dias_outros

  ) |>
  select(-c(
    #evitaveis_fetal_28sem_durante_imunoprevencao,
    evitaveis_fetal_28sem_durante_mulher_gestacao,
    evitaveis_fetal_28sem_durante_parto,
    evitaveis_fetal_28sem_durante_recem_nascido,
    evitaveis_fetal_28sem_durante_tratamento,
    #evitaveis_fetal_28sem_durante_saude,
    evitaveis_fetal_28sem_durante_mal_definidas,
    evitaveis_fetal_28sem_durante_outros,
    evitaveis_fetal_28sem_antes_imunoprevencao,
    evitaveis_fetal_28sem_antes_mulher_gestacao,
    evitaveis_fetal_28sem_antes_parto,
    evitaveis_fetal_28sem_antes_recem_nascido,
    evitaveis_fetal_28sem_antes_tratamento,
    #evitaveis_fetal_28sem_antes_saude,
    evitaveis_fetal_28sem_antes_mal_definidas,
    evitaveis_fetal_28sem_antes_outros,
    evitaveis_fetal_28sem_imunoprevencao,
    evitaveis_fetal_28sem_mulher_gestacao,
    evitaveis_fetal_28sem_parto,
    evitaveis_fetal_28sem_recem_nascido,
    evitaveis_fetal_28sem_tratamento,
    #evitaveis_fetal_28sem_saude,
    evitaveis_fetal_28sem_mal_definidas,
    evitaveis_fetal_28sem_outros,

    fetal_grupos_durante_prematuridade,
    fetal_grupos_durante_infeccoes,
    fetal_grupos_durante_asfixia,
    #fetal_grupos_durante_respiratorias,
    fetal_grupos_durante_gravidez,
    fetal_grupos_durante_afeccoes_perinatal,
    fetal_grupos_durante_ma_formacao,
    #fetal_grupos_durante_mal_definida,
    fetal_grupos_durante_outros,
    fetal_grupos_antes_prematuridade,
    fetal_grupos_antes_infeccoes,
    fetal_grupos_antes_asfixia,
    fetal_grupos_antes_respiratorias,
    fetal_grupos_antes_gravidez,
    fetal_grupos_antes_afeccoes_perinatal,
    fetal_grupos_antes_ma_formacao,
    #fetal_grupos_antes_mal_definida,
    fetal_grupos_antes_outros,
    fetal_grupos_prematuridade,
    fetal_grupos_infeccoes,
    fetal_grupos_asfixia,
    fetal_grupos_respiratorias,
    fetal_grupos_gravidez,
    fetal_grupos_afeccoes_perinatal,
    fetal_grupos_ma_formacao,
    #fetal_grupos_mal_definida,
    fetal_grupos_outros
  ))

write.csv(df_bloco7_distribuicao_cids_perinatal, "data-raw/csv/indicadores_bloco7_distribuicao_cids_perinatal_2012-2024.csv", row.names = FALSE)










# Indicadores já retirados (caso precise recuperar) -----------------------
# ## Para o indicador de causas evitáveis (primeira versão)
# ### Criando vetores com as cids de cada grupo de causas evitáveis
# imunoprevencao <- c(
#   "A17", "A19", "A33", "A35", "A36", "A37", "A80", "B05", "B06",
#   "B16", "B260", "G000", "P350", "P353"
# )
#
# mulher_gestacao <- c(
#   "A50", sprintf("B2%d", 0:4), "P022", "P023", "P027", "P028",
#   "P029", "P00", "P04", "P01", "P05", "P07", "P220", "P26",
#   "P52", "P550", "P551", "P558", "P559", "P56", "P57", "P77"
# )
#
# evitaveis_parto <- c(
#   "P020", "P021", "P024", "P025", "P026", "P03", "P08", sprintf("P1%d", 0:5),
#   "P20", "P21", "P24"
# )
#
# recem_nascido <- c(
#   "P221", "P228", "P229", "P23", "P25", "P27", "P28",
#   sprintf("P3%d", 51:53), sprintf("P3%d", 58:59), sprintf("P3%d", 6:9), sprintf("P5%d", 0:1), sprintf("P5%d", 3:4), "P58", "P59",
#   sprintf("P7%d", 0:4), "P60", "P61",  sprintf("P7%d", 5:6), "P78",
#   sprintf("P8%d", 0:3),  sprintf("P9%d", 0:4),
#   sprintf("P9%d", 60:68)
# )
#
# tratamento <- c(
#   "A15", "A16", "A18", sprintf("G0%d", 0:4), sprintf("J0%d", 0:6),
#   sprintf("J1%d", 2:8), sprintf("J1%d", 2:8), sprintf("J2%d", 0:2),
#   "J384", sprintf("J4%d", 0:2), sprintf("J4%d", 5:7), sprintf("J6%d", 8:9),
#   sprintf("A7%d", 0:4), "A30", "A31", "A32", "A38", "A39", "A40", "A41",
#   "A46", "A49", "E030", "E031", sprintf("E1%d", 0:4), "E700", "E730",
#   "G40", "G41", "Q90", "N390", sprintf("I0%d", 0:9)
# )
#
# saude <- c(
#   sprintf("A0%d", 0:9), sprintf("A2%d", 0:8), sprintf("A9%d", 0:9),
#   sprintf("A7%d", 5:9), "A82", sprintf("B5%d", 0:9), sprintf("B6%d", 0:4),
#   sprintf("B6%d", 5:9), sprintf("B7%d", 0:9), sprintf("B8%d", 0:3),
#   "B99", sprintf("D5%d", 0:3), sprintf("E4%d", 0:9), sprintf("E5%d", 0:9),
#   sprintf("E6%d", 0:4), "E86", c(sprintf("V%02d", 1:99)), sprintf("X4%d", 0:4),
#   sprintf("X4%d", 5:9), "R95", c(sprintf("W%02d", 0:19)), sprintf("X0%d", 0:9),
#   sprintf("X3%d", 0:9), c(sprintf("W%02d", 65:74)), c(sprintf("W%02d", 75:84)),
#   c(sprintf("W%02d", 85:99)), c(sprintf("X%02d", 85:99)),
#   c(sprintf("Y%02d", 00:09)), c(sprintf("Y%02d", 10:34)), c(sprintf("W%02d", 20:49)),
#   c(sprintf("Y%02d", 60:69)), c(sprintf("Y%02d", 83:84)), c(sprintf("Y%02d", 40:59))
# )
#
# mal_definidas <- c(
#   c(sprintf("R%02d", 00:94)), c(sprintf("R%02d", 96:99)),
#   "P95", "P969"
# )
#
# ### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando todos os momentos de óbito
# df_evitaveis_fetal_todos_v1 <- df_fetais_totais |>
#   clean_names() |>
#   mutate(
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
#   mutate(obitos_fetais_totais = rowSums(across(starts_with("evitaveis"), ~ .x), na.rm = TRUE)) |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres)
#
# ### Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal_todos_v1[is.na(df_evitaveis_fetal_todos_v1)] <- 0
#
# ### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando apenas óbitos antes do parto
# df_evitaveis_fetal_antes_v1 <- df_fetais_totais |>
#   clean_names() |>
#   filter(obitoparto == "1") |>
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
#   mutate(obitos_fetais_totais_antes = rowSums(across(starts_with("evitaveis"), ~ .x), na.rm = TRUE)) |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres)
#
# ### Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal_antes_v1[is.na(df_evitaveis_fetal_antes_v1)] <- 0
#
# ### Criando um dataframe com o total de óbitos em cada grupo de causas, considerando apenas óbitos durante o parto
# df_evitaveis_fetal_durante_v1 <- df_fetais_totais |>
#   clean_names() |>
#   filter(obitoparto == "2") |>
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
#   mutate(obitos_fetais_totais_durante = rowSums(across(starts_with("evitaveis"), ~ .x), na.rm = TRUE)) |>
#   right_join(df_aux_municipios) |>
#   arrange(codmunres)
#
# ### Substituindo todos os NAs por 0 (gerados após o right join)
# df_evitaveis_fetal_durante_v1[is.na(df_evitaveis_fetal_durante_v1)] <- 0
#
# ### Juntando os três data.frames da primeira versão de causas evitáveis
# df_bloco7_fetais_evitaveis_v1 <- full_join(
#   df_evitaveis_fetal_todos_v1,
#   full_join(df_evitaveis_fetal_antes_v1, df_evitaveis_fetal_durante_v1)
# )
#
# ### Removendo objetos já utilizados
# rm(df_evitaveis_fetal_todos_v1, df_evitaveis_fetal_antes_v1, df_evitaveis_fetal_durante_v1)
# gc()




