
library(microdatasus)
library(dplyr)
library(janitor)
library(readr)
library(openxlsx)
library(readxl)
library(stringr)
library(tidyr)
library(data.table)

# Baixar os dados ano a ano
# Criar um vetor para armazenar os dados de cada ano
df_list <- list()

anos <- c(2012, 2014:2022)

for (ano in anos){

  df_ano <- fetch_datasus(
    year_start = ano,
    year_end = ano,
    information_system = "SINASC",
    vars = c("CODMUNRES", "DTNASC", "CONSPRENAT", "MESPRENAT", "SEMAGESTAC"))

  # Adicionar a variável ANO ao dataframe
  df_ano$ano <- ano

  # Adicionar o dataframe à lista
  df_list[[ano - 2011]] <- df_ano

}

# CONSPRENAT não é definada para o ano de 2013 então a forma de baixar vai ser diferente
df_ano <- fetch_datasus(
  year_start = 2013,
  year_end = 2013,
  information_system = "SINASC")

df_ano <- df_ano |> select(CODMUNRES, DTNASC, MESPRENAT, SEMAGESTAC)

df_ano$ano <- 2013
df_ano$CONSPRENAT <- NA

df_list[[2013 - 2011]] <- df_ano

# Unificando a lista de data frames em um unico
df <- bind_rows(df_list)
rm(df_list, df_ano)

# write.csv(df, "data-raw/csv/dados_sinasc_bloco3.csv")

# df_proc <- process_sinasc(df1, municipality_data = T) |>
#   select(
#     CODMUNRES,
#     DTNASC,
#     CONSPRENAT,
#     MESPRENAT,
#     SEMAGESTAC
#   ) |>
#   mutate(
#     ano = as.numeric(substr(DTNASC, 1, 4))
#   )

# Dados ainda não consolidados
options(timeout=99999)

sinasc23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";")
sinasc23 <- sinasc23 |>
  mutate(ano = 2023) |>
  select(ano, CODMUNRES, DTNASC, CONSPRENAT, MESPRENAT, SEMAGESTAC)

sinasc24 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN24.csv", sep = ";")
sinasc24 <- sinasc24 |>
  mutate(ano = 2024) |>
  select(ano, CODMUNRES, DTNASC, CONSPRENAT, MESPRENAT, SEMAGESTAC)

# Juntar os dataframes da lista em um único dataframe
df <- rbind(df, sinasc23, sinasc24)

rm(sinasc23, sinasc24)

# Tratando os dados
df2 <- df |>
  mutate(
    codmunres = as.numeric(CODMUNRES),
    CONSPRENAT = as.numeric(CONSPRENAT),
    MESPRENAT = as.numeric(MESPRENAT),
    SEMAGESTAC = as.numeric(SEMAGESTAC)
  ) |>
  mutate(

    nascidos= 1,
    pelo_menos_uma_consulta_prenatal = case_when(
      CONSPRENAT >= 1 ~ 1,
      !(CONSPRENAT >= 1) ~0
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
  #select(codmunres, ano, nascidos) |>
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

# Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read_csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(municipio)

# Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

# Transformando as colunas que estão em caracter para numéricas
df2 <- df2 |> mutate_if(is.character, as.numeric)

# Fazendo um left_join da base auxiliar de municípios com o data.frame que contém o total de nascidos vivos
df_bloco3 <- left_join(df_aux_municipios, df2)

df_bloco3[is.na(df_bloco3)] <- 0

rm(df2)

# Incidência de sífilis congênita por mil nascidos vivos ------------------
# Lendo a base de dados obtida pelo site http://indicadoressifilis.aids.gov.br/
df_sifilis_excel1 <- read_excel("data-raw/extracao-dos-dados/blocos/databases_auxiliares/dados_painel_sifilis_2022.xlsx",
                               sheet = "DADOS CONTINUAÇÃO 2"
)

df_sifilis_excel2 <- read_excel("data-raw/extracao-dos-dados/blocos/databases_auxiliares/dados_painel_sifilis_2013_2023.xlsx",
                                sheet = "DADOS CONTINUAÇÃO 2"
)

# Corrigindo os nomes das colunas e filtrando pelos municípios que utilizamos no painel
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

# Passando para o formato long
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

# Juntando com o restante da base do bloco 3
df_bloco3 <- left_join(df_bloco3, df_sifilis_long)

# Substituindo os NA's da coluna 'casos_sc' por 0 (gerados após o left_join)
df_bloco3$casos_sc[is.na(df_bloco3$casos_sc)] <- 0

# Não temos dados para a Incidência de sífilis congênita para o ano de 2024 --(provisorio remover quando a base for atualizada para 2024)--
# Substituindo os 0 da coluna 'casos_sc' por NA para o ano de 2024
df_bloco3$casos_sc[df_bloco3$ano == 2024] <- NA

# Salvando a base de dados completa -----------------
write.csv(df_bloco3, "data-raw/csv/indicadores_bloco3_assistencia_pre-natal_2012-2024.csv", row.names = FALSE)
