library(microdatasus)
library(dplyr)
library(glue)

# Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Criando um vetor com os anos considerados (2012 a 2024)
anos <- c(2012:2024)

## Baixando os dados do SIH-SP para cada UF
for (estado in estados) {
  # Criando um data.frame que guardará a base final de cada UF
  df_sih_sp_uf <- data.frame()

  # Criando um vetor que contém todos os N_AIHs de interesse na respectiva base do SIH-RD
  naih_sih_rd <- read.csv2(glue("data-raw/morbidade/databases/01_sih_rd/01_arquivos_brutos/{estado}_sih_rd_bruto_{anos[1]}_{anos[length(anos)]}.csv")) |>
    pull(N_AIH)

  for (ano in anos) {
    # Baixando os dados para o dado ano e para a dada UF
    df_sih_sp_uf_aux <- fetch_datasus(
      year_start = ano,
      year_end = ano,
      month_start = 1,
      month_end = 12,
      uf = estado,
      information_system = "SIH-SP",
      vars = c("SP_NAIH", "SP_ATOPROF")
    ) |>
      # Filtrando apenas pelos SP_NAIH que têm correspondência na base do SIH-RD
      filter(SP_NAIH %in% naih_sih_rd)

    # Juntando com os dados dos anos anteriores para a dada UF
    if (ano == anos[1]) {
      df_sih_sp_uf <- df_sih_sp_uf_aux
    } else {
      df_sih_sp_uf <- full_join(df_sih_sp_uf, df_sih_sp_uf_aux)
    }

    # Limpando a memória
    rm(df_sih_sp_uf_aux)
    gc()
  }

  # Salvando a base do completa para a dada UF
  output_dir <- "data-raw/morbidade/databases/02_sih_sp"
  if (!dir.exists(output_dir)) {dir.create(output_dir)}

  write.csv(
    df_sih_sp_uf,
    gzfile(glue("{output_dir}/{estado}_sih_sp_filtrado_{anos[1]}_{anos[length(anos)]}.csv.gz")),
    row.names = FALSE
  )

  # Limpando a memória
  rm(df_sih_sp_uf, naih_sih_rd)
  gc()
}
