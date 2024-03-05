library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(microdatasus)

# Inicialize o dataframe vazio
df_microdatasus_aux <- data.frame()
anos <- 2012:2022
vars <- c("CONSPRENAT","ESCMAE","GESTACAO","IDADEMAE","MESPRENAT","PARTO",
          "PESO","QTDPARTCES","QTDPARTNOR","RACACOR","SEMAGESTAC","TPROBSON")
# Inicialize o dataframe vazio
df_microdatasus_aux <- data.frame()
anos <- 2012:2022
# Loop pelos anos
for(var in vars){
for (ano in anos) {
  tryCatch({
    aux <- microdatasus::fetch_datasus(
      year_start = ano,
      year_end = ano,
      vars = c("CODMUNRES", "DTNASC", var),
      information_system = "SINASC"
    )
    df_microdatasus_aux <- rbind(df_microdatasus_aux, aux)
  }, error = function(e) {
    # Lidar com o erro (pode imprimir uma mensagem ou apenas continuar)
    cat("Erro ao processar o ano", ano, ":", conditionMessage(e), "\n")
  })
}
df <- df_microdatasus_aux %>%
  mutate(ANO = as.numeric(substr(DTNASC, 5, 8)),
         CODMUNRES = as.numeric(CODMUNRES)) %>%
  group_by(ANO, CODMUNRES,df_microdatasus_aux[[var]]  ) %>%
  summarise(NASC = n())

colnames(df) <- c("ANO","CODMUNRES", var,"NASC")
write.csv(df, paste0("bases_novas/",var,"_muni.csv"), row.names = FALSE)
}

# Inicialize o dataframe vazio
df_microdatasus_aux <- data.frame()
anos <- 2012:2022
# Loop pelos anos
for (ano in anos) {
  tryCatch({
    aux <- microdatasus::fetch_datasus(
      year_start = ano,
      year_end = ano,
      vars = c("CODMUNRES", "DTNASC", "TPROBSON", "PARTO"),
      information_system = "SINASC"
    )
    df_microdatasus_aux <- rbind(df_microdatasus_aux, aux)
  }, error = function(e) {
    # Lidar com o erro (pode imprimir uma mensagem ou apenas continuar)
    cat("Erro ao processar o ano", ano, ":", conditionMessage(e), "\n")
  })
}
df <- df_microdatasus_aux %>%
  mutate(ANO = as.numeric(substr(DTNASC, 5, 8)),
         CODMUNRES = as.numeric(CODMUNRES)) %>%
  group_by(ANO, CODMUNRES, TPROBSON, PARTO ) %>%
  summarise(NASC = n())


write.csv(df, "bases_novas/TPROBSON_PARTO_muni.csv", row.names = FALSE)
## AS BASES DE TPROBSON E CONSPRENAT NAO POSSUEM REGISTROS EM 2013
## PRECISAMOS DA BASE DE 2013 PARA TPROBSON_PARTO, USAR BASE ANTIGA PARA TAL
