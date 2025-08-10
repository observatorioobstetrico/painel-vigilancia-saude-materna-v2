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

  # Criando uma função para filtrar apenas os óbitos fetais que consideramos
  pre_processa_dofet <- function(dados) {
    dados |>
      mutate(
        SEMAGESTAC = as.numeric(SEMAGESTAC),
        PESO = as.numeric(PESO),
        TIPOBITO = as.numeric(TIPOBITO),
        GESTACAO = as.character(GESTACAO)
      ) |>
      filter(
        TIPOBITO == 1,
        ((GESTACAO != "1" & !is.na(GESTACAO) & GESTACAO != "9") |
         (SEMAGESTAC >= 22 & SEMAGESTAC != 99)) |
         (PESO >= 500)
      ) |>
      select(CODMUNRES, ano, PESO, GESTACAO, SEMAGESTAC, OBITOPARTO, CAUSABAS)
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
        "SIM-DOFET" = {
          fread_retry("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv") |>
            mutate(ano = extrai_ano(.data[[data_col]])) |>
            pre_processa_dofet()
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
      } else if (sistema == "SIM-DOFET") {
        dados <- pre_processa_dofet(dados)
      }

      dados
    }
  }

  message("Processando ano ", ano)

  list(
    sinasc = baixa_dados(ano, "SINASC", "DTNASC", vars = c("CODMUNRES", "DTNASC", "PESO")),
    dofet = baixa_dados(ano, "SIM-DOFET", "DTOBITO", vars = c("CODMUNRES", "DTOBITO", "TIPOBITO", "PESO", "GESTACAO", "SEMAGESTAC", "OBITOPARTO", "CAUSABAS"))
  )
}

## Criando um vetor com os anos a serem baixados
anos <- 2012:2024

## Baixando todos os dados
resultados <- future_lapply(anos, processa_ano)

## Separando-os em objetos diferentes
df_sinasc <- rbindlist(lapply(resultados, `[[`, "sinasc"), fill = TRUE)
df_dofet <- rbindlist(lapply(resultados, `[[`, "dofet"), fill = TRUE)

## Criando uma função para salvar os arquivos
salva_csv_gz <- function(df, nome) {
  path <- paste0("data-raw/extracao-dos-dados/blocos/databases_auxiliares/", nome, "_2012_2024.csv.gz")
  write.csv(df, gzfile(path), row.names = FALSE)
}

## Salvando os arquivos
salva_csv_gz(df_sinasc, "df_sinasc")
salva_csv_gz(df_dofet, "df_sim_fetais")


# Para os indicadores de número de óbitos, taxa de mortalidade e distribuição dos óbitos por peso ou momento do óbito -----
## Lendo o arquivo com os óbitos fetais no período de 2012-2024
df_obitos_fetais <- fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_sim_fetais_2012_2024.csv.gz") |>
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

## Para os dados do SIM, criando variáveis de óbitos fetais por faixa de peso e momento do óbito
df_indicadores_fetais <- df_obitos_fetais |>
  clean_names() |>
  mutate(
    # Garantindo que as variáveis são do tipo correto
    peso = as.numeric(peso),
    obitoparto = as.numeric(obitoparto)
  ) |>
  # Criando as variáveis necessárias para o cálculo dos indicadores
  mutate(
    obitos_fetais_todos_todos = 1,

    # Número de óbitos fetais por peso e momento do óbito
    ## Todos os momentos do óbito
    obitos_fetais_menos_1000_todos = if_else(
      peso < 1000, 1, 0, missing = 0
    ),
    obitos_fetais_1000_1499_todos = if_else(
      peso >= 1000 & peso < 1500, 1, 0, missing = 0
    ),
    obitos_fetais_1500_2499_todos = if_else(
      peso >= 1500 & peso < 2500, 1, 0, missing = 0
    ),
    obitos_fetais_2500_mais_todos = if_else(
      peso >= 2500, 1, 0, missing = 0
    ),
    obitos_fetais_sem_info_todos = if_else(
      is.na(peso), 1, 0, missing = 0
    ),

    ## Apenas antes do parto
    obitos_fetais_todos_antes = if_else(
      obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_menos_1000_antes = if_else(
      peso < 1000 & obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_1000_1499_antes = if_else(
      peso >= 1000 & peso < 1500 & obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_1500_2499_antes = if_else(
      peso >= 1500 & peso < 2500 & obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_2500_mais_antes = if_else(
      peso >= 2500 & obitoparto == 1, 1, 0, missing = 0
    ),
    obitos_fetais_sem_info_antes = if_else(
      is.na(peso) & obitoparto == 1, 1, 0, missing = 0
    ),

    ## Apenas durante o parto
    obitos_fetais_todos_durante = if_else(
      obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_menos_1000_durante = if_else(
      peso < 1000 & obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_1000_1499_durante = if_else(
      peso >= 1000 & peso < 1500 & obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_1500_2499_durante = if_else(
      peso >= 1500 & peso < 2500 & obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_2500_mais_durante = if_else(
      peso >= 2500 & obitoparto == 2, 1, 0, missing = 0
    ),
    obitos_fetais_sem_info_durante = if_else(
      is.na(peso) & obitoparto == 2, 1, 0, missing = 0
    )
  ) |>
  group_by(codmunres, ano) |>
  summarise_at(vars(starts_with("obitos")), sum) |>
  ungroup() |>
  # Juntando com a base auxiliar de municipios
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
write.csv(df_indicadores_fetais, "data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2024.csv", row.names = FALSE)


# Para os indicadores de causas evitáveis ---------------------------------------
## Lendo o arquivo com os óbitos fetais no período de 2012-2024
df_obitos_fetais <- fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_sim_fetais_2012_2024.csv.gz") |>
  mutate(CODMUNRES = as.character(CODMUNRES))

## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

## Definindo os vetores de CIDs
df_cids_evitaveis <- read_excel("data-raw/extracao-dos-dados/blocos/databases_auxiliares/evitabilidade_fetal.xlsx", sheet = "Fetal") |>
  dplyr::rename(nome = LBE_FETAL, cid = CID)

lista_cids_evitaveis <- list(
  imunoprevencao = df_cids_evitaveis |> dplyr::filter(nome == "Imunoprevenção") |> dplyr::pull(cid),
  gestacao = df_cids_evitaveis |> dplyr::filter(nome == "Reduzíveis por adequada atenção à mulher na gestação") |> dplyr::pull(cid),
  parto = df_cids_evitaveis |> dplyr::filter(nome == "Reduzíveis por adequada atenção à mulher no parto") |> dplyr::pull(cid),
  mal_definidas = df_cids_evitaveis |> dplyr::filter(nome == "Causas de morte mal-definidas") |> dplyr::pull(cid),
  nao_aplica = df_cids_evitaveis |> dplyr::filter(nome == "Não se aplicam ao óbito fetal") |> dplyr::pull(cid)
)

## Criando uma função para categorizar CIDs de acordo com os grupos de causas evitáveis
cria_grupo_evitavel <- function(data, lista_cids, prefixo, filtro_obitoparto = NULL) {
  data <- janitor::clean_names(data)

  if (!is.null(filtro_obitoparto)) {
    if (is.na(filtro_obitoparto)) {
      data <- dplyr::filter(data, is.na(obitoparto) | obitoparto == 9)
    } else {
      data <- dplyr::filter(data, obitoparto == filtro_obitoparto)
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
        causabas %in% lista_cids$imunoprevencao | causabas2 %in% lista_cids$imunoprevencao ~ paste0(prefixo, "_imunoprevencao"),
        causabas %in% lista_cids$gestacao | causabas2 %in% lista_cids$gestacao ~ paste0(prefixo, "_mulher_gestacao"),
        causabas %in% lista_cids$parto | causabas2 %in% lista_cids$parto ~ paste0(prefixo, "_parto"),
        causabas %in% lista_cids$nao_aplica | causabas2 %in% lista_cids$nao_aplica ~ paste0(prefixo, "_nao_aplica"),
        causabas %in% lista_cids$mal_definidas | causabas2 %in% lista_cids$mal_definidas ~ paste0(prefixo, "_mal_definidas"),
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

## Criando os dataframes
df_evitaveis_fetal_antes <- cria_grupo_evitavel(df_obitos_fetais, lista_cids_evitaveis, "evitaveis_fetal_antes", filtro_obitoparto = 1)
df_evitaveis_fetal_durante <- cria_grupo_evitavel(df_obitos_fetais, lista_cids_evitaveis, "evitaveis_fetal_durante", filtro_obitoparto = 2)
df_evitaveis_fetal_sem_info_parto <- cria_grupo_evitavel(df_obitos_fetais, lista_cids_evitaveis, "evitaveis_fetal_sem_info_parto", filtro_obitoparto = NA)

## Unindo todos os dataframes em um só
df_bloco7_fetais_evitaveis <- list(
  df_evitaveis_fetal_antes,
  df_evitaveis_fetal_durante,
  df_evitaveis_fetal_sem_info_parto
) |>
  purrr::reduce(dplyr::full_join) |>
  select(
    codmunres,
    ano,
    starts_with("evitaveis_")
  )

## Salvando a base final
write.csv(df_bloco7_fetais_evitaveis, "data-raw/csv/indicadores_bloco7_causas_evitaveis_fetal_2012-2024.csv", row.names = FALSE)


# Para os indicadores de grupos de causas ---------------------------------------
## Lendo o arquivo com os óbitos fetais no período de 2012-2024
df_obitos_fetais <- fread("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_sim_fetais_2012_2024.csv.gz") |>
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
cria_grupo_causa <- function(data, lista_cids, prefixo, filtro_obitoparto = NULL) {
  data <- janitor::clean_names(data)

  if (!is.null(filtro_obitoparto)) {
    if (is.na(filtro_obitoparto)) {
      data <- dplyr::filter(data, is.na(obitoparto) | obitoparto == 9)
    } else {
      data <- dplyr::filter(data, obitoparto == filtro_obitoparto)
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

## Gerando os dataframes
df_principais_fetal_antes <- cria_grupo_causa(df_obitos_fetais, lista_cids_causas_principais, "principais_fetal_antes", filtro_obitoparto = 1)
df_principais_fetal_durante <- cria_grupo_causa(df_obitos_fetais, lista_cids_causas_principais, "principais_fetal_durante", filtro_obitoparto = 2)
df_principais_fetal_sem_info_parto <- cria_grupo_causa(df_obitos_fetais, lista_cids_causas_principais, "principais_fetal_sem_info_parto", filtro_obitoparto = NA)

## Unindo todos os dataframes em um só
df_bloco7_principais_fetal <- list(
  df_principais_fetal_antes,
  df_principais_fetal_durante,
  df_principais_fetal_sem_info_parto
) |>
  purrr::reduce(dplyr::full_join) |>
  select(
    codmunres,
    ano,
    starts_with("principais_")
  )

## Salvando a base final
write.csv(df_bloco7_principais_fetal, "data-raw/csv/indicadores_bloco7_causas_principais_fetal_2012-2024.csv", row.names = FALSE)
