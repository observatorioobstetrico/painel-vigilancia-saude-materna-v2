library(dplyr)
library(RSQLite)
library(glue)
library(data.table)

# Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

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
diretorio_bases_brutas <- glue("{getwd()}/databases")

# Tratando os dados de cada UF
for (estado in estados) {
  # Mudando o diretório para a pasta que contém o algoritmo em C++
  #setwd("algorithm_episode_of_care/")

  # Rodando o algoritmo em C++ na base do SIH-RD com os dados brutos
  system(glue("./processaih {diretorio_bases_brutas}/{estado}_sih_rd_bruto_2012_2023.csv"))

  # Voltando para o diretório original do projeto
  setwd(diretorio_original)

  # Criando a conexão com o arquivo .sqlite gerado como resultado do algoritmo em C++
  con <- dbConnect(SQLite(), "data-raw/morbidade/algorithm_episode_of_care/work.sqlite")

  # Selecionando a tabela "aih" com todas as suas variáveis, ordenadas por AIHREF e DT_INTER
  df_aih <- dbGetQuery(con, "select * from aih order by AIHREF, DT_INTER")

  # Lendo a base do SIH-RD com os dados brutos e seleciobnando as variáveis adicionais de diagnóstico
  df_aih_bruto <- fread(glue("databases/01_sih_rd/01_arquivos_brutos/{estado}_sih_rd_bruto_2022_2024.csv"), sep = ";") |>
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
  output_dir <- "databases/01_sih_rd/02_arquivos_tratados_long"
  if (!dir.exists(output_dir)) {dir.create(output_dir)}

  write.csv(df_aih, glue("{output_dir}/{estado}_sih_rd_tratado_long_2022_2024.csv"), row.names = FALSE)

  # # Passando a base para o formato wide (cada linha corresponderá a uma pessoa única)
  # df_aih_wide <- df_aih |>
  #   mutate(
  #     DT_INTER = as.Date(DT_INTER, format = "%Y%m%d"),
  #     DT_SAIDA = as.Date(DT_SAIDA, format = "%Y%m%d"),
  #     NASC = as.Date(NASC, format = "%Y%m%d")
  #   ) |>
  #   group_by(AIHREF) |>  # Agrupando pelo N_AIH comum para todas as linhas de um episódio de cuidado completo
  #   summarise(
  #     NRECS = n(),  # Número de internações
  #     DT_INI = first(DT_INTER),  # Data de admissão da primeira internação
  #     DT_FIM = last(DT_SAIDA),  # Data de saída da última internação
  #     COBRANCA = last(COBRANCA),  # Motivo de saída/permanência da última internação
  #     PDIAG = first(DIAG_PRINC),  # Diagnóstico principal da primeira internação
  #     PPROC = first(PROC_REA),  # Procedimento principal da primeira internação
  #     SOMA_UTI = sum(as.integer(UTI_MES_TO)),  # Total de dias na UTI
  #     SOMA_US = sum(as.numeric(US_TOT)),  # Valor total, em dólares
  #     SOMA_CRIT = sum(as.integer(criterio_primario)),  # Quantas das internações foram eventos obstétricos
  #     NASC = first(NASC)  # Data de nascimento
  #   ) |>
  #   mutate(
  #     DIAS = as.numeric(difftime(DT_FIM, DT_INI, units = "days")),  # Tempo total, em dias, das internações
  #     IDADE = calc_idade(NASC, DT_INI)  # Idade da paciente (em anos)
  #   ) |>
  #   mutate(DT_INI = as.numeric(gsub("-", "", as.character(DT_INI))),
  #          DT_FIM = as.numeric(gsub("-", "", as.character(DT_FIM)))) |>
  #   arrange(AIHREF) |>
  #   select(!"NASC") |>
  #   select("AIHREF", "NRECS", "DT_INI", "DT_FIM", "DIAS", "COBRANCA",
  #          "PDIAG", "PPROC", "SOMA_UTI", "SOMA_US", "SOMA_CRIT", "IDADE")
  #
  # # Exportando a base no formato wide tratada
  # write.csv(df_aih_wide, glue("databases/01_sih_rd/03_arquivos_tratados_wide/{estado}_sih_rd_tratado_wide_2012_2022.csv"), row.names = FALSE)

  # Limpando a memória
  #rm(df_aih, df_aih_wide)
  rm(df_aih, df_aih_completo)
  gc()
  dbDisconnect(con)
}

