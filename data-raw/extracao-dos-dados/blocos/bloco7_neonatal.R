library(microdatasus)
library(dplyr)
library(janitor)
library(data.table)
library(stringr)
library(tidyr)
library(data.table)
library(future)
library(future.apply)
library(readxl)

# Baixando todos os dados necessários (com paralelização) ---------------------
## Criando o planejamento dos futures
plan(multisession)

## Criando uma função que baixa todos os dados necessários para um certo ano
processa_ano <- function(ano) {
  # Carrega os pacotes dentro da worker
  library(microdatasus)
  library(dplyr)
  library(data.table)
  library(stringr)

  # Criando uma função para criar a coluna de "ano" em bases do SIM e SINASC
  extrai_ano <- function(data, n = 4) {
    as.numeric(substr(data, nchar(data) - n + 1, nchar(data)))
  }

  # Criando uma função para filtrar apenas os óbitos neonatais
  pre_processa_doinf <- function(dados) {
    dados |>
      filter(as.numeric(IDADE) < 228) |>
      select(CODMUNRES, ano, IDADE, PESO, CAUSABAS)
  }

  # Criando uma função para insistir várias vezes no download
  fread_retry <- function(url, ..., max_tries = 10, wait_seconds = 10) {
    for (i in seq_len(max_tries)) {
      tryCatch({
        message(sprintf("Tentando baixar: %s (tentativa %d de %d)", url, i, max_tries))
        df <- data.table::fread(url, ...)
        return(df)
      },
      error = function(e) {
        message("Erro ao tentar baixar: ", conditionMessage(e))
        if (i < max_tries) {
          message(sprintf("Aguardando %d segundos para nova tentativa...", wait_seconds))
          Sys.sleep(wait_seconds)
        } else {
          stop("Falha ao baixar após ", max_tries, " tentativas.")
        }
      })
    }
  }

  # Criando uma função genérica para baixar dados pelo microdatasus
  baixa_dados <- function(ano, sistema, data_col, vars = NULL) {
    # Tratamento especial para os dados preliminares
    if (ano == 2024) {
      switch(sistema,
             "SIM-DOINF" = {
               fread_retry("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv") |>
                 mutate(ano = extrai_ano(.data[[data_col]])) |>
                 pre_processa_doinf()
             },
             "SINASC" = {
               fread_retry("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_2024.csv", sep = ";") |>
                 mutate(ano = extrai_ano(.data[[data_col]])) |>
                 select(CODMUNRES, ano, PESO)
             }
      )
    } else {
      # Anos consolidados via microdatasus
      dados <- fetch_datasus(
        year_start = ano,
        year_end = ano,
        information_system = sistema,
        vars = vars
      ) |>
        mutate(ano = extrai_ano(.data[[data_col]]))

      if (sistema == "SINASC") {
        dados <- dados |> select(CODMUNRES, ano, PESO)
      } else if (sistema == "SIM-DOINF") {
        dados <- pre_processa_doinf(dados)
      }

      dados
    }
  }

  message("Processando ano ", ano)

  list(
    sinasc = baixa_dados(ano, "SINASC", "DTNASC", vars = c("CODMUNRES", "DTNASC", "PESO")),
    doinf = baixa_dados(ano, "SIM-DOINF", "DTOBITO", vars = c("CODMUNRES", "DTOBITO", "IDADE", "PESO", "CAUSABAS"))
  )
}

## Criando um vetor com os anos a serem baixados
anos <- 2012:2024

## Baixando todos os dados
resultados <- future_lapply(anos, processa_ano)

## Separando-os em objetos diferentes
df_sinasc <- rbindlist(lapply(resultados, `[[`, "sinasc"), fill = TRUE)
df_doinf <- rbindlist(lapply(resultados, `[[`, "doinf"), fill = TRUE)

## Criando uma função para salvar os arquivos
salva_csv_gz <- function(df, nome) {
  path <- paste0("data-raw/extracao-dos-dados/blocos/databases_auxiliares/", nome, "_2012_2024.csv.gz")
  write.csv(df, gzfile(path), row.names = FALSE)
}

## Salvando os arquivos
salva_csv_gz(df_sinasc, "df_sinasc")
salva_csv_gz(df_doinf, "df_sim_neonatais")


# Para os indicadores de número de óbitos, taxa de mortalidade e distribuição dos óbitos por peso ou momento do óbito -----
## Lendo o arquivo com os óbitos neonatais no período de 2012-2024
df_obitos_neonatais <- fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_sim_neonatais_2012_2024.csv.gz") |>
  mutate(CODMUNRES = as.character(CODMUNRES))

## Lendo o arquivo com os nascimentos no período de 2012-2024
df_sinasc <- fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_sinasc_2012_2024.csv.gz") |>
  mutate(CODMUNRES = as.character(CODMUNRES))

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

## Para os dados do SINASC, criando variáveis de nascidos vivos por faixa de peso
df_indicadores_sinasc <- df_sinasc |>
  clean_names() |>
  mutate(
    # Garantindo que as variáveis são do tipo correto
    peso = as.numeric(peso)
  ) |>
  mutate(
    total_de_nascidos_vivos = 1,
    nv_peso_menos_1000 = if_else(
      peso < 1000, 1, 0, missing = 0
    ),
    nv_peso_1000_1499 = if_else(
      (peso >= 1000 & peso < 1500), 1, 0, missing = 0
    ),
    nv_peso_1500_2499 = if_else(
      (peso >= 1500 & peso < 2500), 1, 0, missing = 0
    ),
    nv_peso_2500_mais = if_else(
      peso >= 2500, 1, 0, missing = 0
    ),
    nv_peso_sem_info = if_else(
      is.na(peso), 1, 0, missing = 0
    )
  ) |>
  group_by(codmunres, ano) |>
  summarise_at(vars(starts_with("total_") | starts_with("nv")), sum) |>
  ungroup()

## Para os dados do SIM, criando variáveis de óbitos neonatais por faixa de peso e momento do óbito
df_indicadores_neonatais <- df_obitos_neonatais |>
  clean_names() |>
  mutate(
    # Garantindo que as variáveis são do tipo correto
    peso = as.numeric(peso),
    idade = as.numeric(idade)
  ) |>
  # Criando as variáveis necessárias para o cálculo dos indicadores
  mutate(
    obitos_neonatais_todos_todos = 1,

    # Número de óbitos neonatais por peso e momento do óbito
    ## Todos os momentos do óbito
    obitos_neonatais_menos_1000_todos = if_else(
      peso < 1000, 1, 0, missing = 0
    ),
    obitos_neonatais_1000_1499_todos = if_else(
      (peso >= 1000 & peso < 1500), 1, 0, missing = 0
    ),
    obitos_neonatais_1500_2499_todos = if_else(
      (peso >= 1500 & peso < 2500), 1, 0, missing = 0
    ),
    obitos_neonatais_2500_mais_todos = if_else(
      peso >= 2500, 1, 0, missing = 0
    ),
    obitos_neonatais_sem_info_todos = if_else(
      is.na(peso), 1, 0, missing = 0
    ),

    ## Apenas óbitos com 0 dias completos
    obitos_neonatais_todos_0_dias = if_else(
      idade <= 200, 1, 0, missing = 0
    ),
    obitos_neonatais_menos_1000_0_dias = if_else(
      peso < 1000 & idade <= 200, 1, 0, missing = 0
    ),
    obitos_neonatais_1000_1499_0_dias = if_else(
      (peso >= 1000 & peso < 1500) & idade <= 200, 1, 0, missing = 0
    ),
    obitos_neonatais_1500_2499_0_dias = if_else(
      (peso >= 1500 & peso < 2500) & idade <= 200, 1, 0, missing = 0
    ),
    obitos_neonatais_2500_mais_0_dias = if_else(
      peso >= 2500 & idade <= 200, 1, 0, missing = 0
    ),
    obitos_neonatais_sem_info_0_dias = if_else(
      is.na(peso) & idade <= 200, 1, 0, missing = 0
    ),

    ## Apenas óbitos com 0 a 6 dias completos
    obitos_neonatais_todos_0_a_6_dias = if_else(
      idade <= 206, 1, 0, missing = 0
    ),
    obitos_neonatais_menos_1000_0_a_6_dias = if_else(
      peso < 1000 & idade <= 206, 1, 0, missing = 0
    ),
    obitos_neonatais_1000_1499_0_a_6_dias = if_else(
      (peso >= 1000 & peso < 1500) & idade <= 206, 1, 0, missing = 0
    ),
    obitos_neonatais_1500_2499_0_a_6_dias = if_else(
      (peso >= 1500 & peso < 2500) & idade <= 206, 1, 0, missing = 0
    ),
    obitos_neonatais_2500_mais_0_a_6_dias = if_else(
      peso >= 2500 & idade <= 206, 1, 0, missing = 0
    ),
    obitos_neonatais_sem_info_0_a_6_dias = if_else(
      is.na(peso) & idade <= 206, 1, 0, missing = 0
    ),

    ## Apenas óbitos com 1 a 6 dias completos
    obitos_neonatais_todos_1_a_6_dias = if_else(
      (idade >= 201 & idade <= 206), 1, 0, missing = 0
    ),
    obitos_neonatais_menos_1000_1_a_6_dias = if_else(
      peso < 1000 & (idade >= 201 & idade <= 206), 1, 0, missing = 0
    ),
    obitos_neonatais_1000_1499_1_a_6_dias = if_else(
      (peso >= 1000 & peso < 1500) & (idade >= 201 & idade <= 206), 1, 0, missing = 0
    ),
    obitos_neonatais_1500_2499_1_a_6_dias = if_else(
      (peso >= 1500 & peso < 2500) & (idade >= 201 & idade <= 206), 1, 0, missing = 0
    ),
    obitos_neonatais_2500_mais_1_a_6_dias = if_else(
      peso >= 2500 & (idade >= 201 & idade <= 206), 1, 0, missing = 0
    ),
    obitos_neonatais_sem_info_1_a_6_dias = if_else(
      is.na(peso) & (idade >= 201 & idade <= 206), 1, 0, missing = 0
    ),

    ## Apenas óbitos com 7 a 27 dias completos
    obitos_neonatais_todos_7_a_27_dias = if_else(
      (idade >= 207 & idade <= 227), 1, 0, missing = 0
    ),
    obitos_neonatais_menos_1000_7_a_27_dias = if_else(
      peso < 1000 & (idade >= 207 & idade <= 227), 1, 0, missing = 0
    ),
    obitos_neonatais_1000_1499_7_a_27_dias = if_else(
      (peso >= 1000 & peso < 1500) & (idade >= 207 & idade <= 227), 1, 0, missing = 0
    ),
    obitos_neonatais_1500_2499_7_a_27_dias = if_else(
      (peso >= 1500 & peso < 2500) & (idade >= 207 & idade <= 227), 1, 0, missing = 0
    ),
    obitos_neonatais_2500_mais_7_a_27_dias = if_else(
      peso >= 2500 & (idade >= 207 & idade <= 227), 1, 0, missing = 0
    ),
    obitos_neonatais_sem_info_7_a_27_dias = if_else(
      is.na(peso) & (idade >= 207 & idade <= 227), 1, 0, missing = 0
    )
  ) |>
  group_by(codmunres, ano) |>
  summarise_at(vars(starts_with("obitos")), sum) |>
  ungroup() |>
  # Juntando com a base auxiliar de municípios
  right_join(df_aux_municipios, by = join_by(codmunres, ano)) |>
  # Juntando com a base contendo as variáveis do SINASC
  left_join(df_indicadores_sinasc, by = join_by(codmunres, ano)) |>
  mutate(across(everything(), ~replace_na(., 0))) |>
  dplyr::select(
    codmunres,
    ano,
    total_de_nascidos_vivos,
    starts_with("nv"),
    starts_with("obitos")
  ) |>
  arrange(codmunres, ano)

## Salvando a base final
write.csv(df_indicadores_neonatais, "data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2024.csv", row.names = FALSE)


# Para os indicadores de causas evitáveis ---------------------------------------
## Lendo o arquivo com os óbitos neonatais no período de 2012-2024
df_obitos_neonatais <- fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_sim_neonatais_2012_2024.csv.gz") |>
  mutate(CODMUNRES = as.character(CODMUNRES))

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

## Definindo os vetores de CIDs
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

### Definindo os grupos como lista nomeada
lista_cids_evitaveis <- list(
  imunoprevencao = imunoprevencao,
  mulher_gestacao = mulher_gestacao,
  evitaveis_parto = evitaveis_parto,
  recem_nascido = recem_nascido,
  tratamento = tratamento,
  saude = saude,
  mal_definidas = mal_definidas
)

rm(imunoprevencao, mulher_gestacao, evitaveis_parto, recem_nascido, tratamento, saude, mal_definidas)

## Criando uma função para categorizar CIDs de acordo com os grupos de causas evitáveis
cria_grupo_evitavel <- function(data, cids, prefixo, idade_start = NULL, idade_end = NULL) {
  data <- janitor::clean_names(data)

  if (!is.null(idade_start) & !is.null(idade_end)) {
    if (!is.null(idade_start)) {
      data <- dplyr::filter(data, idade >= idade_start & idade <= idade_end)
    } else {
      data <- dplyr::filter(data, idade <= idade_end)
    }
  }

  data |>
    dplyr::mutate(
      causabas2 = substr(causabas, 1, 3),
      faixa_de_peso = dplyr::case_when(
        is.na(peso) ~ "sem_informacao",
        peso < 1000 ~ "menor_1000",
        peso < 1500 ~ "1000_a_1499",
        peso < 2500 ~ "1500_a_2499",
        peso >= 2500 ~ "2500_mais"
      ),
      grupo_cid = dplyr::case_when(
        causabas %in% cids$imunoprevencao | causabas2 %in% cids$imunoprevencao ~ paste0(prefixo, "_imunoprevencao"),
        causabas %in% cids$mulher_gestacao | causabas2 %in% cids$mulher_gestacao ~ paste0(prefixo, "_mulher_gestacao"),
        causabas %in% cids$evitaveis_parto | causabas2 %in% cids$evitaveis_parto ~ paste0(prefixo, "_parto"),
        causabas %in% cids$recem_nascido | causabas2 %in% cids$recem_nascido ~ paste0(prefixo, "_recem_nascido"),
        causabas %in% cids$tratamento | causabas2 %in% cids$tratamento ~ paste0(prefixo, "_tratamento"),
        causabas %in% cids$saude | causabas2 %in% cids$saude ~ paste0(prefixo, "_saude"),
        causabas %in% cids$mal_definidas | causabas2 %in% cids$mal_definidas ~ paste0(prefixo, "_mal_definidas"),
        TRUE ~ paste0(prefixo, "_outros")
      )
    ) |>
    dplyr::select(codmunres, ano, grupo_cid, faixa_de_peso) |>
    dplyr::mutate(obitos = 1L) |>
    dplyr::group_by(across(!obitos)) |>
    dplyr::summarise(obitos = sum(obitos), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = c(grupo_cid, faixa_de_peso),
      values_from = obitos,
      values_fill = 0,
      names_sort = TRUE
    ) |>
    dplyr::right_join(df_aux_municipios, by = join_by(codmunres, ano)) |>
    dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::arrange(codmunres)
}

## Criando os dataframes consolidados
df_evitaveis_neonatal_0_dias <- cria_grupo_evitavel(df_obitos_neonatais, lista_cids_evitaveis, "evitaveis_neonatal_0_dias", idade_end = 200)
df_evitaveis_neonatal_1_a_6_dias <- cria_grupo_evitavel(df_obitos_neonatais, lista_cids_evitaveis, "evitaveis_neonatal_1_a_6_dias", idade_start = 201, idade_end = 206)
df_evitaveis_neonatal_7_a_27_dias <- cria_grupo_evitavel(df_obitos_neonatais, lista_cids_evitaveis, "evitaveis_neonatal_7_a_27_dias", idade_start = 207, idade_end = 227)

## Unindo todos os dataframes em um só
df_bloco7_neonatais_evitaveis <- list(
  df_evitaveis_neonatal_0_dias,
  df_evitaveis_neonatal_1_a_6_dias,
  df_evitaveis_neonatal_7_a_27_dias
) |>
  purrr::reduce(dplyr::full_join) |>
  select(
    codmunres,
    ano,
    starts_with("evitaveis")
  )

## Salvando a base final
write.csv(df_bloco7_neonatais_evitaveis, "data-raw/csv/indicadores_bloco7_causas_evitaveis_neonatal_2012-2024.csv", row.names = FALSE)


# Para os indicadores de grupos de causas ---------------------------------------
## Lendo o arquivo com os óbitos neonatais no período de 2012-2024
df_obitos_neonatais <- fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_sim_neonatais_2012_2024.csv.gz") |>
  mutate(CODMUNRES = as.character(CODMUNRES))

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

## Definindo os vetores de CIDs
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

grupos_afeccoes_perinatal <- c("P969")

grupos_ma_formacao <- c(paste0("Q", sprintf("%02d", 0:99)))

grupos_mal_definida <- c(paste0("R", sprintf("%02d", 0:99)))

### Definindo os grupos como lista nomeada
lista_cids_causas_principais <- list(
  prematuridade = grupos_prematuridade,
  infeccoes = grupos_infeccoes,
  asfixia = grupos_asfixia,
  respiratorias = grupos_respiratorias,
  gravidez = grupos_gravidez,
  afeccoes_perinatal = grupos_afeccoes_perinatal,
  ma_formacao = grupos_ma_formacao,
  mal_definida = grupos_mal_definida
)

## Criando uma função para categorizar CIDs de acordo com os grupos de causas principais
cria_grupo_causa <- function(data, lista_cids, prefixo, idade_start = NULL, idade_end = NULL) {
  data <- janitor::clean_names(data)

  if (!is.null(idade_start) & !is.null(idade_end)) {
    if (!is.null(idade_start)) {
      data <- dplyr::filter(data, idade >= idade_start & idade <= idade_end)
    } else {
      data <- dplyr::filter(data, idade <= idade_end)
    }
  }

  data |>
    dplyr::mutate(
      causabas2 = substr(causabas, 1, 3),
      faixa_de_peso = dplyr::case_when(
        is.na(peso) ~ "sem_informacao",
        peso < 1000 ~ "menor_1000",
        peso < 1500 ~ "1000_a_1499",
        peso < 2500 ~ "1500_a_2499",
        peso >= 2500 ~ "2500_mais"
      ),
      grupo_cid = dplyr::case_when(
        causabas %in% lista_cids$prematuridade | causabas2 %in% lista_cids$prematuridade ~ paste0(prefixo, "_prematuridade"),
        causabas %in% lista_cids$infeccoes | causabas2 %in% lista_cids$infeccoes ~ paste0(prefixo, "_infeccoes"),
        causabas %in% lista_cids$asfixia | causabas2 %in% lista_cids$asfixia ~ paste0(prefixo, "_asfixia"),
        causabas %in% lista_cids$respiratorias | causabas2 %in% lista_cids$respiratorias ~ paste0(prefixo, "_respiratorias"),
        causabas %in% lista_cids$gravidez | causabas2 %in% lista_cids$gravidez ~ paste0(prefixo, "_gravidez"),
        causabas %in% lista_cids$afeccoes_perinatal | causabas2 %in% lista_cids$afeccoes_perinatal ~ paste0(prefixo, "_afeccoes_perinatal"),
        causabas %in% lista_cids$ma_formacao | causabas2 %in% lista_cids$ma_formacao ~ paste0(prefixo, "_ma_formacao"),
        causabas %in% lista_cids$mal_definida | causabas2 %in% lista_cids$mal_definida ~ paste0(prefixo, "_mal_definida"),
        TRUE ~ paste0(prefixo, "_outros")
      )
    ) |>
    dplyr::select(codmunres, ano, grupo_cid, faixa_de_peso) |>
    dplyr::mutate(obitos = 1L) |>
    dplyr::group_by(across(!obitos)) |>
    dplyr::summarise(obitos = sum(obitos), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = c(grupo_cid, faixa_de_peso),
      values_from = obitos,
      values_fill = 0,
      names_sort = TRUE
    ) |>
    dplyr::right_join(df_aux_municipios, by = join_by(codmunres, ano)) |>
    dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::arrange(codmunres)
}

# Gerando os dataframes
df_principais_neonatal_0_dias <- cria_grupo_causa(df_obitos_neonatais, lista_cids_causas_principais, "principais_neonatal_0_dias", idade_end = 200)
df_principais_neonatal_1_a_6_dias <- cria_grupo_causa(df_obitos_neonatais, lista_cids_causas_principais, "principais_neonatal_1_a_6_dias", idade_start = 201, idade_end = 206)
df_principais_neonatal_sem_info_parto <- cria_grupo_causa(df_obitos_neonatais, lista_cids_causas_principais, "principais_neonatal_7_a_27_dias", idade_start = 207, idade_end = 227)

# Unindo os dataframes
df_bloco7_principais_neonatal <- list(
  df_principais_neonatal_0_dias,
  df_principais_neonatal_1_a_6_dias,
  df_principais_neonatal_sem_info_parto
) |>
  purrr::reduce(dplyr::full_join) |>
  select(
    codmunres,
    ano,
    starts_with("principais_")
  )

## Salvando a base final
write.csv(df_bloco7_principais_neonatal, "data-raw/csv/indicadores_bloco7_causas_principais_neonatal_2012-2024.csv", row.names = FALSE)
