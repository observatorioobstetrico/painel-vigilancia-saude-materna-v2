# Bibliotecas ------------------------------------------------------------------
library(microdatasus)
library(abjData)
library(dplyr)
library(tidyr)
library(janitor)
# library(pheatmap)
# library(tibble)

# Baixar dados -----------------------------------------------------------------

## tpnascassi foi criada em 2013

df <- microdatasus::fetch_datasus(year_start = 2013, year_end = 2023,
                                      information_system = "SINASC",
                                      vars = c("CODMUNRES",
                                               "DTNASC",
                                               "TPNASCASSI",
                                               "LOCNASC",
                                               "PARTO"
                                               )
                                  )

# df <- data.table::fread("sinasc_profissional_local.csv")

# Baixando os dados preliminares do SINASC e selecionando as variáveis ---------
options(timeout=99999)

df_2024 <- data.table::fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2024.csv", sep = ";") |>
  select(CODMUNRES, DTNASC, TPNASCASSI, LOCNASC, PARTO) |>
  mutate(CODMUNRES = as.character(CODMUNRES),
         DTNASC = as.character(DTNASC),
         TPNASCASSI = as.character(TPNASCASSI),
         LOCNASC = as.character(LOCNASC),
         PARTO = as.character(PARTO)
         )

df <- rbind(df, df_2024) |> clean_names()

#rm(df_2024)

# Criando alguns objetos auxiliares --------------------------------------------
## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/csv//tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2013:2024)), ano = 2013:2024)


# tpnascassi Nascimento foi assistido por? Valores: 1– Médico; 2– Enfermagem ou Obstetriz; 3–Parteira; 4– Outros; 9– Ignorado
# locnasc Local de nascimento: 1 – Hospital; 2 – Outros estabelecimentos de saúde; 3 – Domicílio; 4 – Outros; 5- Aldeia Indígena.

# Tratando os dados ------------------------------------------------------------

nascidos_vivos_total <- df |>
  mutate(ano = as.numeric(substr(dtnasc, nchar(dtnasc) - 3, nchar(dtnasc)))) |>
  group_by(codmunres, ano) |>
  summarise(total_de_nascidos_vivos = n())

nascidos_vivos_total_hospital <- df |>
  mutate(ano = as.numeric(substr(dtnasc, nchar(dtnasc) - 3, nchar(dtnasc)))) |>
  filter(locnasc == 1) |>
  group_by(codmunres, ano) |>
  summarise(total_de_nascidos_vivos_hospital = n())

nascidos_vivos_total_hospital_vaginal <- df |>
  mutate(ano = as.numeric(substr(dtnasc, nchar(dtnasc) - 3, nchar(dtnasc)))) |>
  filter(locnasc == 1, parto == "1") |>
  group_by(codmunres, ano) |>
  summarise(total_de_nascidos_vivos_partos_vaginais_hospital = n())

df_bloco4_profissional <- df |>
  # Selecionando somente parto vaginal, PARTO == 1
  filter(parto == "1") |>
  select(-parto) |>
  mutate(
    ano = as.numeric(substr(dtnasc, nchar(dtnasc) - 3, nchar(dtnasc))),
    tpnascassi = as.numeric(tpnascassi),
    locnasc = as.numeric(locnasc),
    .keep = "unused",
  ) |>
  mutate(
    total_de_nascidos_vivos_partos_vaginais = 1,
    # Nascimento assistido
    nasc_assistido_medico = if_else(tpnascassi == 1, 1, 0, missing = 0),
    nasc_assistido_enf_obs = if_else(tpnascassi == 2, 1, 0, missing = 0),
    nasc_assistido_parteira = if_else(tpnascassi == 3, 1, 0, missing = 0),
    nasc_assistido_outros = if_else(tpnascassi == 4, 1, 0, missing = 0),
    nasc_assistido_ignorado = if_else(tpnascassi == 9, 1, 0, missing = 0),
    nasc_assistido_sem_inf = if_else(is.na(tpnascassi), 1, 0, missing = 0),
    # Local de nascimento
    nasc_local_hospital = if_else(locnasc == 1, 1, 0, missing = 0),
    nasc_local_outros_est_saude = if_else(locnasc == 2, 1, 0, missing = 0),
    nasc_local_domicilio = if_else(locnasc == 3, 1, 0, missing = 0),
    nasc_local_outros = if_else(locnasc == 4, 1, 0, missing = 0),
    nasc_local_aldeia = if_else(locnasc == 5, 1, 0, missing = 0),
    nasc_local_sem_inf = if_else(is.na(locnasc), 1, 0, missing = 0),
    # Nascimento assistido com Local de nascimento == Hospital
    nasc_assistido_medico_hospital = if_else(locnasc == 1 & tpnascassi == 1, 1, 0, missing = 0),
    nasc_assistido_enf_obs_hospital = if_else(locnasc == 1 & tpnascassi == 2, 1, 0, missing = 0),
    nasc_assistido_parteira_hospital = if_else(locnasc == 1 & tpnascassi == 3, 1, 0, missing = 0),
    nasc_assistido_outros_hospital = if_else(locnasc == 1 & tpnascassi == 4, 1, 0, missing = 0),
    nasc_assistido_ignorado_hospital = if_else(locnasc == 1 & tpnascassi == 9, 1, 0, missing = 0),
    nasc_assistido_sem_inf_hospital = if_else(locnasc == 1 & is.na(tpnascassi), 1, 0, missing = 0),
    # Nascimento assistido com Local de nascimento == Outros estabelecimentos de saúde
    nasc_assistido_medico_outros_est_saude = if_else(locnasc == 2 & tpnascassi == 1, 1, 0, missing = 0),
    nasc_assistido_enf_obs_outros_est_saude = if_else(locnasc == 2 & tpnascassi == 2, 1, 0, missing = 0),
    nasc_assistido_parteira_outros_est_saude = if_else(locnasc == 2 & tpnascassi == 3, 1, 0, missing = 0),
    nasc_assistido_outros_outros_est_saude = if_else(locnasc == 2 & tpnascassi == 4, 1, 0, missing = 0),
    nasc_assistido_ignorado_outros_est_saude = if_else(locnasc == 2 & tpnascassi == 9, 1, 0, missing = 0),
    nasc_assistido_sem_inf_outros_est_saude = if_else(locnasc == 2 & is.na(tpnascassi), 1, 0, missing = 0),
    # Nascimento assistido com Local de nascimento == Domicílio
    nasc_assistido_medico_domicilio = if_else(locnasc == 3 & tpnascassi == 1, 1, 0, missing = 0),
    nasc_assistido_enf_obs_domicilio = if_else(locnasc == 3 & tpnascassi == 2, 1, 0, missing = 0),
    nasc_assistido_parteira_domicilio = if_else(locnasc == 3 & tpnascassi == 3, 1, 0, missing = 0),
    nasc_assistido_outros_domicilio = if_else(locnasc == 3 & tpnascassi == 4, 1, 0, missing = 0),
    nasc_assistido_ignorado_domicilio = if_else(locnasc == 3 & tpnascassi == 9, 1, 0, missing = 0),
    nasc_assistido_sem_inf_domicilio = if_else(locnasc == 3 & is.na(tpnascassi), 1, 0, missing = 0),
    # Nascimento assistido com Local de nascimento == Outros
    nasc_assistido_medico_outros = if_else(locnasc == 4 & tpnascassi == 1, 1, 0, missing = 0),
    nasc_assistido_enf_obs_outros = if_else(locnasc == 4 & tpnascassi == 2, 1, 0, missing = 0),
    nasc_assistido_parteira_outros = if_else(locnasc == 4 & tpnascassi == 3, 1, 0, missing = 0),
    nasc_assistido_outros_outros = if_else(locnasc == 4 & tpnascassi == 4, 1, 0, missing = 0),
    nasc_assistido_ignorado_outros = if_else(locnasc == 4 & tpnascassi == 9, 1, 0, missing = 0),
    nasc_assistido_sem_inf_outros = if_else(locnasc == 4 & is.na(tpnascassi), 1, 0, missing = 0),
    # Nascimento assistido com Local de nascimento == Aldeia Indígena
    nasc_assistido_medico_aldeia = if_else(locnasc == 5 & tpnascassi == 1, 1, 0, missing = 0),
    nasc_assistido_enf_obs_aldeia = if_else(locnasc == 5 & tpnascassi == 2, 1, 0, missing = 0),
    nasc_assistido_parteira_aldeia = if_else(locnasc == 5 & tpnascassi == 3, 1, 0, missing = 0),
    nasc_assistido_outros_aldeia = if_else(locnasc == 5 & tpnascassi == 4, 1, 0, missing = 0),
    nasc_assistido_ignorado_aldeia = if_else(locnasc == 5 & tpnascassi == 9, 1, 0, missing = 0),
    nasc_assistido_sem_inf_aldeia = if_else(locnasc == 5 & is.na(tpnascassi), 1, 0, missing = 0),
    # Nascimento assistido com Local de nascimento == NA (Sem Informação)
    nasc_assistido_medico_sem_inf = if_else(is.na(locnasc) & tpnascassi == 1, 1, 0, missing = 0),
    nasc_assistido_enf_obs_sem_inf = if_else(is.na(locnasc) & tpnascassi == 2, 1, 0, missing = 0),
    nasc_assistido_parteira_sem_inf = if_else(is.na(locnasc) & tpnascassi == 3, 1, 0, missing = 0),
    nasc_assistido_outros_sem_inf = if_else(is.na(locnasc) & tpnascassi == 4, 1, 0, missing = 0),
    nasc_assistido_ignorado_sem_inf = if_else(is.na(locnasc) & tpnascassi == 9, 1, 0, missing = 0),
    nasc_assistido_sem_inf_sem_inf = if_else(is.na(locnasc) & is.na(tpnascassi), 1, 0, missing = 0),
    .keep = "unused"
  ) |>
  clean_names() |>
  group_by(codmunres, ano) |>
  summarise_at(vars(-group_cols()), sum) |>
  ungroup() |>
  # Juntando com a base aulixiar de municípios
  right_join(df_aux_municipios) |>
  # Juntando com a variável de total de nascidos vivos
  left_join(nascidos_vivos_total, by = c("codmunres", "ano")) |>
  left_join(nascidos_vivos_total_hospital, by = c("codmunres", "ano")) |>
  left_join(nascidos_vivos_total_hospital_vaginal, by = c("codmunres", "ano"))


## Substituindo todos os NAs, gerados após o right_join, por 0
df_bloco4_profissional[is.na(df_bloco4_profissional)] <- 0

data.table::fwrite(df_bloco4_profissional, "data-raw/csv/indicadores_bloco4_profissional_2012-2024.csv")


# sinasc_profissional_local <- df
#
# ## analisando incompletude -----------------------------------------------------
#
# table(sinasc_profissional_local$tpnascassi, useNA = "ifany")
#
# table(sinasc_profissional_local$locnasc, useNA = "ifany")
#
# ##
#
# sinasc <- sinasc_profissional_local %>%
#   mutate(CODMUNRES = as.character(CODMUNRES)) %>%
#   left_join(abjData::muni %>% select(muni_id_6, uf_sigla),
#             by = c("CODMUNRES" = "muni_id_6"))
#
#
# tabela_faltantes_por_uf <- sinasc %>%
#   group_by(uf_sigla) %>%
#   summarise(
#     tpnascassi_faltantes = sum(tpnascassi == 9 | is.na(tpnascassi)),
#     locnasc_faltantes    = sum(locnasc == 9 | is.na(locnasc)),
#     total_obs            = n(),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(cols = c(tpnascassi_faltantes, locnasc_faltantes),
#                names_to = "variavel",
#                values_to = "n_faltantes") %>%
#   mutate(
#     porcentagem = round(100 * n_faltantes / total_obs, 2)
#   ) %>%
#   select(uf_sigla, total_obs, variavel, n_faltantes, porcentagem)
#
#
# # Transforma para matriz (linha: estado; coluna: variável)
# matriz_tpnascassi <- tabela_faltantes_por_uf %>%
#   filter(variavel == "tpnascassi_faltantes") %>%
#   select(uf_sigla, porcentagem)
#
# matriz_tpnascassi <- as.matrix(matriz_tpnascassi[, "porcentagem"])
# rownames(matriz_tpnascassi) <- matriz_tpnascassi[, "uf_sigla"]
