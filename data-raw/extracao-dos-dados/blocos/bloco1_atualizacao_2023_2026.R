# Carregando os pacotes necessários
library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(tidyr)
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
  codmunres = rep(codigos_municipios, each = length(2023:2026)),
  ano = 2023:2026
  ) |>
  mutate_if(is.character, as.numeric)


# Para os indicadores provenientes do SINASC ------------------------------
## Baixando os dados consolidados do SINASC de 2023 a 2024 e selecionando as variáveis de interesse
df_sinasc_consolidados <- fetch_datasus(
  year_start = 2023,
  year_end = 2024,
  vars = c("CODMUNRES", "DTNASC", "IDADEMAE", "RACACORMAE", "ESCMAE"),
  information_system = "SINASC"
  ) |>
  mutate_if(is.character, as.numeric)

## Baixando os dados preliminares do SINASC de 2025 e 2026
## OBS: a 2ª prévia de 2026 foi publicada em XML; confirmar se já existe CSV.
df_sinasc_preliminares <- data.frame()

for (ano_sinasc in 2025:2026) {
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile()
  dir.create(temp_dir)

  url <- paste0(
    "https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_",
    ano_sinasc, "_csv.zip"
  )

  status_download <- tryCatch(
    download.file(url, temp_zip, mode = "wb", quiet = TRUE),
    error = function(e) 1
  )

  if (!identical(status_download, 0L) || !file.exists(temp_zip) ||
      file.size(temp_zip) == 0) {
    stop(paste0(
      "Não foi possível baixar o CSV do SINASC ", ano_sinasc,
      ". Verifique o formato disponível no Portal de Dados Abertos do SUS: ", url,
      ". Não prossiga usando dados incompletos."
    ))
  }

  files <- unzip(temp_zip, exdir = temp_dir)
  files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  if (length(files) == 0) stop("ZIP do SINASC sem arquivo CSV: ", ano_sinasc)

  df_aux_sinasc <- rbindlist(lapply(files, function(arq) {
    fread(arq, sep = ";", select = c("CODMUNRES", "DTNASC", "IDADEMAE",
                                      "RACACORMAE", "ESCMAE"))
  }), use.names = TRUE, fill = TRUE)

  df_sinasc_preliminares <- bind_rows(df_sinasc_preliminares, df_aux_sinasc)
}


## Juntando os dados consolidados com os dados preliminares
df_sinasc <- bind_rows(df_sinasc_consolidados, df_sinasc_preliminares) |>
  clean_names()

## Transformando algumas variáveis e criando as variáveis necessárias p/ o cálculo dos indicadores
df_bloco1_sinasc <- df_sinasc |>
  filter(codmunres %in% codigos_municipios) |>
  mutate(
    ano = as.numeric(substr(dtnasc, nchar(dtnasc) - 3, nchar(dtnasc))),
    total_de_nascidos_vivos = 1,

    # Idade da mãe
    nvm_menor_que_20_anos = if_else(idademae >= 10 & idademae < 20, 1, 0, missing = 0),
    nvm_10_a_14_anos = if_else(idademae >= 10 & idademae <= 14, 1, 0, missing = 0),
    nvm_15_a_19_anos = if_else(idademae >= 15 & idademae <= 19, 1, 0, missing = 0),
    nvm_entre_20_e_34_anos = if_else(idademae >= 20 & idademae < 35, 1, 0, missing = 0),
    nvm_maior_que_34_anos = if_else(idademae >= 35 & idademae <= 55, 1, 0, missing = 0),
    nvm_idade_sem_informacao = if_else(
      is.na(idademae) | idademae %in% c(99, 999),
      1, 0
    ),

    # Raça/cor da mãe
    nvm_com_cor_da_pele_branca = if_else(racacormae == 1, 1, 0, missing = 0),
    nvm_com_cor_da_pele_preta = if_else(racacormae == 2, 1, 0, missing = 0),
    nvm_com_cor_da_pele_parda = if_else(racacormae == 4, 1, 0, missing = 0),
    nvm_com_cor_da_pele_amarela = if_else(racacormae == 3, 1, 0, missing = 0),
    nvm_indigenas = if_else(racacormae == 5, 1, 0, missing = 0),
    nvm_cor_sem_informacao = if_else(
      is.na(racacormae),
      1, 0
    ),

    # Escolaridade da mãe
    nvm_com_escolaridade_ate_3 = if_else(escmae == 1 | escmae == 2, 1, 0, missing = 0),
    nvm_com_escolaridade_de_4_a_7 = if_else(escmae == 3, 1, 0, missing = 0),
    nvm_com_escolaridade_de_8_a_11 = if_else(escmae == 4, 1, 0, missing = 0),
    nvm_com_escolaridade_acima_de_11 = if_else(escmae == 5, 1, 0, missing = 0)
  ) |>
  group_by(codmunres, ano) |>
  summarise_at(vars(starts_with("total_") | starts_with("nvm")), sum) |>
  ungroup() |>
  mutate_if(is.character, as.numeric)

# Juntando com a base auxiliar de municípios
df_bloco1_sinasc <- left_join(df_aux_municipios, df_bloco1_sinasc)

# Transformando todos os NAs, gerados após o left_join, em 0
df_bloco1_sinasc[is.na(df_bloco1_sinasc)] <- 0


# Para os indicadores provenientes do Tabnet ------------------------------
## Importando as funções utilizadas para baixar os dados do Tabnet
source("data-raw/extracao-dos-dados/blocos/funcoes_auxiliares.R")

## População feminina de 10 a 49 anos com plano de saúde -------------------
### Baixando os dados de estimativas da população feminina de 10 a 49 anos
df_est_pop <- est_pop_tabnet(periodo = 12:25, idade_min = 10, idade_max = 49)

### Verificando se existem NAs
if (any(is.na(df_est_pop))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}

### Baixando os dados de mulheres de 10 a 49 anos beneficíarias de planos de saúde
#### Tive que separar em dois porque estava dando erro baixando o período inteiro
df_beneficiarias_aux1 <- pop_com_plano_saude_tabnet(
  faixa_etaria = c(
    "10 a 14 anos",
    "15 a 19 anos",
    "20 a 24 anos",
    "25 a 29 anos",
    "30 a 34 anos",
    "35 a 39 anos",
    "40 a 44 anos",
    "45 a 49 anos"
  ),
  periodo = 2012:2018
  ) |>
  select(!municipio)

df_beneficiarias_aux2 <- pop_com_plano_saude_tabnet(
  faixa_etaria = c(
    "10 a 14 anos",
    "15 a 19 anos",
    "20 a 24 anos",
    "25 a 29 anos",
    "30 a 34 anos",
    "35 a 39 anos",
    "40 a 44 anos",
    "45 a 49 anos"
  ),
  periodo = 2019:2026
  ) |>
  select(!municipio)

df_beneficiarias_aux <- full_join(
  df_beneficiarias_aux1,
  df_beneficiarias_aux2
) |>
  arrange()

#### Verificando se existem NAs
if (any(is.na(df_beneficiarias_aux))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}

#### Aconteceram NAs porque tive que baixar os períodos separadamente
df_beneficiarias_aux[is.na(df_beneficiarias_aux)] <- 0

rm(df_beneficiarias_aux1, df_beneficiarias_aux2)

#### Passando o data.frame para o formato long
df_beneficiarias <- df_beneficiarias_aux |>
  mutate(codmunres = as.character(codmunres)) |>
  pivot_longer(
    !codmunres,
    names_to = "mes_ano",
    values_to = paste0("beneficiarias_10_a_49")
  ) |>
  mutate(
    mes = substr(mes_ano, start = 1, stop = 3),
    ano = as.numeric(paste0("20", substr(mes_ano, start = 5, stop = 6))),
    .after = mes_ano,
    .keep = "unused"
  ) |>
  arrange(codmunres, ano) |>
  filter(codmunres %in% df_aux_municipios$codmunres) |>
  left_join(df_est_pop |> mutate(ano = as.numeric(ano))) |>
  group_by(codmunres, ano) |>
  filter(beneficiarias_10_a_49 < populacao_feminina_10_a_49) |>
  summarise(
    beneficiarias_10_a_49 = round(median(beneficiarias_10_a_49))
  ) |>
  ungroup()

#### Juntando com os dados de estimativas populacionais
df_beneficiarias_pop <- left_join(
  df_est_pop |> mutate(ano = as.numeric(ano)),
  df_beneficiarias
)

### Calculando a cobertura suplementar, os limites inferiores e superiores para a consideração de outliers e inputando caso necessário
df_cob_suplementar <- df_beneficiarias_pop |>
  mutate(
    cob_suplementar = round(
      beneficiarias_10_a_49 / populacao_feminina_10_a_49,
      3
    )
  ) |>
  group_by(codmunres) |>
  mutate(
    q1 = round(
      quantile(
        cob_suplementar[which(cob_suplementar < 1 & ano %in% 2012:2026)],
        0.25
      ),
      3
    ),
    q3 = round(
      quantile(
        cob_suplementar[which(cob_suplementar < 1 & ano %in% 2012:2026)],
        0.75
      ),
      3
    ),
    iiq = q3 - q1,
    lim_inf = round(q1 - 1.5 * iiq, 3),
    lim_sup = round(q3 + 1.5 * iiq, 3),
    outlier = ifelse(
      (cob_suplementar > 1) |
        (cob_suplementar < lim_inf | cob_suplementar > lim_sup) |
        (is.na(q1) & is.na(q3)) |
        (is.na(cob_suplementar)),
      1,
      0
    ),
    novo_cob_suplementar = ifelse(
      outlier == 0,
      cob_suplementar,
      round(
        median(cob_suplementar[which(outlier == 0 & ano %in% 2012:2026)]),
        3
      )
    ),
    novo_beneficiarias_10_a_49 = round(
      novo_cob_suplementar * populacao_feminina_10_a_49
    )
  ) |>
  ungroup() |>
  select(
    codmunres,
    ano,
    pop_fem_10_49_com_plano_saude = novo_beneficiarias_10_a_49,
    populacao_feminina_10_a_49
  )


## Juntando todos os dados provenientes do Tabnet --------------------------
df_bloco1_tabnet <- df_aux_municipios |>
  left_join(df_cob_suplementar |> mutate(codmunres = as.numeric(codmunres)))

### Substituindo os NA's da coluna 'pop_fem_10_49_com_plano_saude' por 0 (gerados após o left_join)
df_bloco1_tabnet$pop_fem_10_49_com_plano_saude[is.na(
  df_bloco1_tabnet$pop_fem_10_49_com_plano_saude) &
  df_bloco1_tabnet$ano <= 2025] <- 0


# Para o indicador de cobertura da AB -------------------------------------
## Lendo as bases contendo as variáveis que serão utilizadas e fazendo os tratamentos necessários
### Para os anos de 2012 até 2020
historico_ab_municipios <- read_delim(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/Historico_AB_MUNICIPIOS_2007_202012.csv"
  ) |>
  janitor::clean_names() |>
  mutate(
    ano = as.numeric(substr(nu_competencia, 1, 4)),
    qt_cobertura_ab = as.numeric(
      str_replace_all(qt_cobertura_ab, "\\.", "") |>
        str_replace_all("\\,", "\\.")
    ),
    qt_populacao = as.numeric(
      str_replace_all(qt_populacao, "\\.", "") |> str_replace_all("\\,", "\\.")
    )
  ) |>
  select(ano, codmunres = co_municipio_ibge, qt_cobertura_ab, qt_populacao)

### Para os anos de 2021 até set/2023
cobertura_potencial_aps_municipio1 <- read.xlsx(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/cobertura_potencial_aps_municipio3.xlsx"
  ) |>
  janitor::clean_names() |>
  select(
    comp_cnes,
    codmunres = codigo_ibge,
    qt_cobertura_ab = qt_capacidade_da_equipe,
    qt_populacao = populacao
  ) |>
  mutate(
    ano = as.numeric(substr(comp_cnes, 1, 4)),
    qt_cobertura_ab = as.numeric(
      str_replace_all(qt_cobertura_ab, "\\.", "") |>
        str_replace_all("\\,", "\\.")
    ),
    qt_populacao = as.numeric(
      str_replace_all(qt_populacao, "\\.", "") |> str_replace_all("\\,", "\\.")
    )
  ) |>
  select(ano, codmunres, qt_cobertura_ab, qt_populacao)

### Para os meses de out/2023 até abr/2024
cobertura_potencial_aps_municipio2 <- readxl::read_xls(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/cobertura_potencial_aps_municipio4.xls"
  ) |>
  janitor::clean_names() |>
  select(
    competencia_cnes,
    codmunres = ibge,
    qt_cobertura_ab = qt_capacidade_da_equipe,
    qt_populacao = populacao
  ) |>
  mutate(
    ano = as.numeric(substr(competencia_cnes, 4, 7)),
    qt_cobertura_ab = as.numeric(
      str_replace_all(qt_cobertura_ab, "\\.", "") |>
        str_replace_all("\\,", "\\.")
    ),
    qt_populacao = as.numeric(
      str_replace_all(qt_populacao, "\\.", "") |> str_replace_all("\\,", "\\.")
    )
  ) |>
  select(ano, codmunres, qt_cobertura_ab, qt_populacao)

### Para os meses de mai/2024 até dez/2025
cobertura_potencial_aps_municipio3 <- readxl::read_excel(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/cobertura_potencial_aps_municipio5.xls"
  ) |>
  select(codmunres = Município,
         ano = "Comp. CNES",
         qt_populacao = População,
         qt_cobertura_ab = "Qt. capacidade da equipe"
  ) |>
  mutate(
    ano = as.numeric(substr(ano, 4, 7))
  ) |>
  select(ano, codmunres, qt_cobertura_ab, qt_populacao)

## Dados de cobertura em:
## https://relatorioaps.saude.gov.br/cobertura/aps

################################################################################

### Para os meses de jan/2026 até jul/2026
cobertura_potencial_aps_municipio4 <- readxl::read_xlsx(
  "data-raw/extracao-dos-dados/blocos/databases_auxiliares/cobertura_potencial_aps_municipio6.xlsx"
) |>
  select(codmunres = Município,
         ano = "Comp. CNES",
         qt_populacao = População,
         qt_cobertura_ab = "Qt. capacidade da equipe"
  ) |>
  mutate(
    ano = as.numeric(substr(ano, 4, 7))
  ) |>
  select(ano, codmunres, qt_cobertura_ab, qt_populacao)


## Juntando os dados de 2021 até 2026
cobertura_potencial_aps_municipio <- rbind(
  cobertura_potencial_aps_municipio1,
  cobertura_potencial_aps_municipio2,
  cobertura_potencial_aps_municipio3,
  cobertura_potencial_aps_municipio4
  )

## Juntando todas as bases
dados_ab_municipios <- rbind(
  historico_ab_municipios,
  cobertura_potencial_aps_municipio
  )

## Calculando a média anual dos valores de qt_cobertura_ab e qt_populacao
dados_ab_municipios <- dados_ab_municipios |>
  group_by(ano, codmunres) |>
  summarize(
    qt_cobertura_ab = mean(qt_cobertura_ab),
    qt_populacao = mean(qt_populacao)
  )

### Garantindo que qt_cobertura_ab não seja maior que qt_populacao
dados_ab_municipios$qt_cobertura_ab <- pmin(
  dados_ab_municipios$qt_cobertura_ab,
  dados_ab_municipios$qt_populacao
  )

### Fazendo um left_join com a base auxiliar de municípios
df_cobertura_ab <- left_join(
  df_aux_municipios,
  dados_ab_municipios |> mutate(codmunres = as.numeric(codmunres))
  )

### Substituindo os NA's da coluna 'qt_cobertura_ab' por 0 (gerados após o left_join)
df_cobertura_ab$qt_cobertura_ab[is.na(df_cobertura_ab$qt_cobertura_ab) &
                                    df_cobertura_ab$ano <= 2025] <- 0

### Substituindo os NA's da coluna 'qt_populacao' por 0 (gerados após o left_join)
df_cobertura_ab$qt_populacao[is.na(df_cobertura_ab$qt_populacao) &
                                 df_cobertura_ab$ano <= 2025] <- 0


# Juntando os dados de todas as bases -------------------------------------
df_bloco1 <- df_bloco1_sinasc |>
  left_join(df_bloco1_tabnet, by = c("codmunres", "ano")) |>
  left_join(
    df_cobertura_ab |>
      rename(
        media_cobertura_esf = qt_cobertura_ab,
        populacao_total = qt_populacao
      ),
    by = c("codmunres", "ano")
  )


## Mantendo somente 2023 a 2026 na nova base
df_bloco1 <- df_bloco1 |> filter(ano %in% 2023:2026)

# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(
  df_bloco1,
  "data-raw/csv/indicadores_bloco1_socioeconomicos_2023-2026.csv",
  row.names = FALSE
)


# Comparando com a base já salva (somente anos em comum: 2023 a 2025) ------
## Não sobrescrever o arquivo anterior.
arquivo_anterior <- "data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2025.csv"
if (!file.exists(arquivo_anterior)) stop("Base anterior não encontrada: ", arquivo_anterior)

df_bloco1_anterior <- read.csv(arquivo_anterior) |>
  filter(ano %in% 2023:2025) |>
  mutate(codmunres = as.numeric(codmunres), ano = as.numeric(ano))

df_bloco1_novo <- df_bloco1 |>
  filter(ano %in% 2023:2025) |>
  mutate(codmunres = as.numeric(codmunres), ano = as.numeric(ano))

## Verificando a unicidade das chaves e os municípios-ano ausentes
if (anyDuplicated(df_bloco1_anterior[c("codmunres", "ano")]) > 0)
  stop("A base anterior tem município/ano duplicado")
if (anyDuplicated(df_bloco1_novo[c("codmunres", "ano")]) > 0)
  stop("A base nova tem município/ano duplicado")

chaves_anteriores <- df_bloco1_anterior |> select(codmunres, ano)
chaves_novas <- df_bloco1_novo |> select(codmunres, ano)
municipios_ausentes_novo <- anti_join(chaves_anteriores, chaves_novas,
                                      by = c("codmunres", "ano"))
municipios_novos <- anti_join(chaves_novas, chaves_anteriores,
                              by = c("codmunres", "ano"))

## Comparação indicador a indicador: diferenças = valor novo - valor anterior
indicadores_comuns <- intersect(names(df_bloco1_anterior), names(df_bloco1_novo))
indicadores_comuns <- setdiff(indicadores_comuns, c("codmunres", "ano"))

comparacao_bloco1 <- inner_join(
  df_bloco1_anterior |> select(codmunres, ano, all_of(indicadores_comuns)),
  df_bloco1_novo |> select(codmunres, ano, all_of(indicadores_comuns)),
  by = c("codmunres", "ano"), suffix = c("_anterior", "_novo")
) |>
  pivot_longer(
    cols = -c(codmunres, ano),
    names_to = c("indicador", "versao"),
    names_pattern = "^(.*)_(anterior|novo)$",
    values_to = "valor"
  ) |>
  pivot_wider(names_from = versao, values_from = valor) |>
  mutate(
    diferenca = novo - anterior,
    status = case_when(
      is.na(anterior) & is.na(novo) ~ "igual",
      is.na(anterior) | is.na(novo) ~ "NA em uma base",
      abs(diferenca) <= 1e-8 ~ "igual",
      TRUE ~ "diferente"
    )
  )

divergencias_bloco1 <- comparacao_bloco1 |>
  filter(status != "igual") |>
  arrange(ano, indicador, codmunres)

resumo_comparacao <- comparacao_bloco1 |>
  group_by(ano, indicador, status) |>
  summarise(n_municipios = n(), .groups = "drop")

## Conferência adicional da nova base de 2026
resumo_anos <- df_bloco1 |>
  group_by(ano) |>
  summarise(
    municipios = n_distinct(codmunres),
    total_nascidos_vivos = sum(total_de_nascidos_vivos, na.rm = TRUE),
    municipios_sem_populacao_feminina = sum(is.na(populacao_feminina_10_a_49)),
    municipios_sem_cobertura_aps = sum(is.na(media_cobertura_esf)),
    .groups = "drop"
  )

print(resumo_anos)
print(resumo_comparacao)
cat("Divergências (município/ano/indicador): ", nrow(divergencias_bloco1), "\n")
cat("Municípios/anos só na base antiga: ", nrow(municipios_ausentes_novo), "\n")
cat("Municípios/anos só na base nova: ", nrow(municipios_novos), "\n")

write.csv(divergencias_bloco1,
          "data-raw/csv/comparacao_bloco1_divergencias_2023-2025.csv",
          row.names = FALSE)
write.csv(resumo_comparacao,
          "data-raw/csv/comparacao_bloco1_resumo_2023-2025.csv",
          row.names = FALSE)
write.csv(resumo_anos,
          "data-raw/csv/comparacao_bloco1_resumo_anos_2023-2026.csv",
          row.names = FALSE)
write.csv(municipios_ausentes_novo,
          "data-raw/csv/comparacao_bloco1_chaves_ausentes.csv",
          row.names = FALSE)
write.csv(municipios_novos,
          "data-raw/csv/comparacao_bloco1_chaves_novas.csv",
          row.names = FALSE)
