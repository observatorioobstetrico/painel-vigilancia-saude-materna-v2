library(microdatasus)
library(data.table)
library(dplyr)
library(glue)
library(future)
library(future.apply)
library(purrr)

# Criando o planejamento dos futures
plan(multisession)
options(future.rng.onMisuse = "ignore")

# Definindo vetores estáticos -------------------------------------------------
## Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
             "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
             "RS","RO","RR","SC","SP","SE","TO")

## Criando um vetor com os anos considerados (2012 a 2024)
anos <- 2012:2024

# Definindo os diretórios de entrada e saída ----------------------------------
sih_rd_dir <- "data-raw/morbidade/databases/01_sih_rd/01_arquivos_brutos"
sih_sp_dir <- "data-raw/morbidade/databases/02_sih_sp"
if (!dir.exists(sih_sp_dir)) dir.create(sih_sp_dir, recursive = TRUE)

# Criando uma função para baixar os dados do SIH-SP para cada UF --------------
processa_uf_sih_sp <- function(uf_sigla, anos, sih_rd_dir, sih_sp_dir) {

  # Criando um data.table que guardará a base final de cada UF
  df_sih_sp_uf <- data.table()

  # Lendo vetor de N_AIH da base SIH-RD já baixada para a UF e anos
  naih_sih_rd <- fread(glue("{sih_rd_dir}/{uf_sigla}_sih_rd_bruto_{min(anos)}_{max(anos)}.csv.gz"))[, N_AIH]

  # Loop para baixar dados SIH-SP e filtrar apenas pelos SP_NAIH que têm correspondência na base do SIH-RD
  for (ano in anos) {
    df_aux <- fetch_datasus(
      year_start = ano,
      year_end = ano,
      month_start = 1,
      month_end = 12,
      uf = uf_sigla,
      information_system = "SIH-SP",
      vars = c("SP_NAIH", "SP_ATOPROF")
    ) %>%
      filter(SP_NAIH %in% naih_sih_rd) %>%
      as.data.table()

    # Juntando dados do ano com os anteriores
    df_sih_sp_uf <- rbindlist(list(df_sih_sp_uf, df_aux), use.names = TRUE, fill = TRUE)

    # Limpeza de memória intermediária
    rm(df_aux)
    gc()
  }

  # Salvando arquivo final para a UF
  fwrite(
    df_sih_sp_uf,
    file = glue("{sih_sp_dir}/{uf_sigla}_sih_sp_filtrado_{min(anos)}_{max(anos)}.csv.gz"),
    sep = ";",
    compress = "gzip"
  )

  # Limpeza final de memória
  rm(df_sih_sp_uf, naih_sih_rd)
  gc()

  message("✅  Concluído UF: ", uf_sigla)
  invisible(TRUE)
}

# Baixando todos os dados -----------------------------------------------------
invisible(
  future_lapply(
    estados,
    function(uf) processa_uf_sih_sp(uf_sigla = uf, anos = anos, sih_rd_dir = sih_rd_dir, sih_sp_dir = sih_sp_dir)
  )
)
