library(dplyr)
library(RSQLite)
library(glue)
library(data.table)

# Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Criando um vetor com os anos considerados (2012 a 2024)
anos <- c(2024)

# # Criando uma função para calcular a diferença, em dias, entre duas datas
# datediff <- function(mindate, maxdate) {
#   diff <- as.numeric(difftime(maxdate, mindate, units = "days"))
#   return(diff)
# }
#
# # Criando uma função para calcular a idade em anos (a partir da data de nascimento e de uma certa data)
# calc_idade <- function(nascimento, data) {
#   idade <- as.numeric(difftime(data, nascimento, units = "days")) / 365.25
#   return(floor(idade))
# }

# Criando um vetor que contém o diretório original do projeto
diretorio_original <- getwd()

# Criando um vetor que contém o diretório das bases brutas do SIH-RD
diretorio_bases_brutas <- glue("{getwd()}/data-raw/morbidade/databases/01_sih_rd/01_arquivos_brutos")

# Tratando os dados de cada UF
for (estado in estados) {
  # Mudando o diretório para a pasta que contém o algoritmo em C++
  setwd("data-raw/morbidade/algorithm_episode_of_care/")

  # Rodando o algoritmo em C++ na base do SIH-RD com os dados brutos
  system(glue("./processaih {diretorio_bases_brutas}/{estado}_sih_rd_bruto_{anos[1]}_{anos[length(anos)]}.csv"))

  # Voltando para o diretório original do projeto
  setwd(diretorio_original)

  # Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
  con <- dbConnect(SQLite(), "data-raw/morbidade/algorithm_episode_of_care/work.sqlite")

  # Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
  df_aih <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")

  # Lendo a base do SIH-RD com os dados brutos e seleciobnando as variáveis adicionais de diagnóstico
  df_aih_bruto <- fread(glue("data-raw/morbidade/databases/01_sih_rd/01_arquivos_brutos/{estado}_sih_rd_bruto_{anos[1]}_{anos[length(anos)]}.csv"), sep = ";") |>
    select(
      N_AIH, ANO_CMPT, DIAG_SECUN, DIAGSEC1, DIAGSEC2, DIAGSEC3, DIAGSEC4,
      DIAGSEC5, DIAGSEC6, DIAGSEC7, DIAGSEC8, DIAGSEC9, CID_MORTE,
      CID_ASSO, CID_NOTIF
    ) |>
    mutate(N_AIH = as.character(N_AIH))

  # Adicionando as variáveis adicionais de diagnóstico à base tratada
  df_aih_completo <- df_aih |>
    left_join(
      df_aih_bruto
    )

  # Exportando a base resultante do algotimo em C++, com as variáveis necessárias adicionadas
  output_dir <- "data-raw/morbidade/databases/01_sih_rd/02_arquivos_tratados_long"
  if (!dir.exists(output_dir)) {dir.create(output_dir)}

  write.csv(df_aih, glue("{output_dir}/{estado}_sih_rd_tratado_long_{anos[1]}_{anos[length(anos)]}.csv"), row.names = FALSE)

  # Limpando a memória
  rm(df_aih, df_aih_completo)
  gc()
  dbDisconnect(con)
}

