library(dplyr)
library(readr)
library(glue)

# Defina o diretório onde estão os arquivos
dir_path <- "databases/01_sih_rd/01_arquivos_brutos/"

# Obtenha a lista de todos os arquivos CSV no diretório
files <- list.files(path = dir_path, pattern = "*.csv", full.names = TRUE)

# Extraia as siglas dos estados a partir dos nomes dos arquivos
estados <- unique(sub("_.*$", "", basename(files)))

for (estado in estados) {
  dt_aux <- files[grep(paste0("^", estado), basename(files))]

  dt_aux1 <- read_delim(dt_aux[1], delim = ";", escape_double = FALSE, trim_ws = TRUE)
  dt_aux2 <- read_delim(dt_aux[2], delim = ";", escape_double = FALSE, trim_ws = TRUE)

  dt <- rbind(dt_aux1,dt_aux2)

  write.csv2(dt,glue("databases/01_sih_rd/01_arquivos_brutos/{estado}_sih_rd_bruto_2012_2023.csv"),
             row.names = FALSE)
}
