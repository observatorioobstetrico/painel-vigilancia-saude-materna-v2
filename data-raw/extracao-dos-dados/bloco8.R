library(dplyr)
library(httr)
library(ggplot2)
library(getPass)
library(repr)
library(questionr)

token = getPass()

url_base = "https://bigdata-api.fiocruz.br"

endpoint = paste0(url_base,"/","show_tables")

request <- POST(url = endpoint, body = list("token" = token), encode = "json")

as_tibble(content(request))

convertRequestToDF <- function(request){
  variables = unlist(content(request)$columns)
  variables = variables[names(variables) == "name"]
  column_names <- unname(variables)
  values = content(request)$rows
  df <- as.data.frame(do.call(rbind, lapply(values, function(r) {
    row <- r
    row[sapply(row,is.null)] <- NA
    rbind(unlist(row))
  })))
  names(df) <- column_names
  return(df)
}



# #########################################
# # Número de nascimentos total 2012 AND 2021 RESIDENCIA
# #########################################

estados <- c('RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN',
             'PB', 'PE', 'AL', 'SE', 'BA', 'MG', 'ES', 'RJ', 'SP', 'PR', 'SC',
             'RS', 'MS', 'MT', 'GO', 'DF')

df_municipio <- data.frame()

endpoint <- paste0(url_base, "/", "sql_query")

for (estado in estados) {
  
  print(estado)
  
  params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query": "SELECT res_SIGLA_UF, res_MUNNOMEX, res_codigo_adotado, CODESTAB, ano_nasc, PESO, APGAR5, IDANOMAL, CODANOMAL, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (nasc_SIGLA_UF = \'',estado,'\') AND ano_nasc BETWEEN 2012 AND 2021',
                  ' GROUP BY res_SIGLA_UF, res_MUNNOMEX, res_codigo_adotado,  CODESTAB, ano_nasc, PESO, APGAR5, IDANOMAL, CODANOMAL", "fetch_size": 65000}
          }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "json")
  df_mun <- convertRequestToDF(request)
  names(df_mun) <- c('UF', 'Municipio', 'Codigo', 'CNES', 'Ano', 'Peso', 'APGAR5', 'IDANOMAL', 'CODANOMAL', 'Nascimentos')
  df_municipio <- rbind(df_municipio, df_mun)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
            "token": {
              "token": "',token,'"
            },
            "sql": {
              "sql": {"cursor": "',cursor,'"}
            }
          }')
    
    request <- POST(url = endpoint, body = params, encode = "json")
    
    if (length(content(request)$rows) == 0)
      break
    
    df_mun <- convertRequestToDF(request)
    names(df_mun) <- c('UF', 'Municipio', 'Codigo', 'CNES', 'Ano', 'Peso', 'APGAR5', 'IDANOMAL', 'CODANOMAL','Nascimentos')
    df_municipio <- rbind(df_municipio, df_mun)
    
  }
}




######## salvar base em .csv

write.table(df_municipio, 'res_df_asfixia_2012_2021.csv', sep = ";", dec = ".", row.names = FALSE)


################

estados <- c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF')
endpoint <- paste0(url_base,"/","sql_query")

#Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("tabela_aux_municipios.csv") |>
  pull(codmunres)


#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2021)), ano = 2012:2021)


#Criando o data.frame que irá receber todos os dados do bloco 1
df_bloco8 <- data.frame()


# Total de nascidos vivos -------------------------------------------------
df <- dataframe <- data.frame()

for (estado in estados){
  
  params = paste0('{
      "token": {
        "token": "',token,'"
      },
      "sql": {
        "sql": {"query": "SELECT CODMUNRES, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                  ' GROUP BY CODMUNRES, ano_nasc",
                        "fetch_size": 65000}
      }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "form")
  dataframe <- convertRequestToDF(request)
  names(dataframe) <- c('codmunres', 'ano', 'total_de_nascidos_vivos')
  df <- rbind(df, dataframe)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT CODMUNRES, ano_nasc, COUNT(1)',
                    ' FROM \\"datasus-sinasc\\"',
                    ' WHERE (res_SIGLA_UF = \'',estado,'\' AND ano_nasc >= 2012)',
                    ' GROUP BY CODMUNRES, ano_nasc",
                           "fetch_size": 65000, "cursor": "',cursor,'"}
          }
        }')
    
    
    request <- POST(url = endpoint, body = params, encode = "form")
    
    if (length(content(request)$rows) == 0)
      break
    else print("oi")
    
    dataframe <- convertRequestToDF(request)
    names(dataframe) <- c('codmunres', 'ano', 'total_de_nascidos_vivos')
    df <- rbind(df, dataframe)
  }
}
head(df)

##Transformando as colunas que estão em caracter para numéricas
df <- df |> mutate_if(is.character, as.numeric)

##Fazendo um left_join da base auxiliar de municípios com o data.frame que contém o total de nascidos vivos
df_bloco8 <- left_join(df_aux_municipios, df)

##Substituindo os NA's da coluna 'total_de_nascidos_vivos' por 0 (os NA's surgem quando um município não apresentou nascidos vivos num dado ano)
df_bloco8$total_de_nascidos_vivos[is.na(df_bloco8$total_de_nascidos_vivos)] <- 0

write.table(df_bloco8, 'df_bloco8.csv', sep = ";", dec = ".", row.names = FALSE)

###################
# TRATAMENTO DOS DADOS PARA CRIAÇÃO DAS TABELAS
##################


dados <- read.csv('res_df_asfixia_2012_2021.csv', sep = ";")


########## asfixia 1 ##########
# IDANOMAL = Anomalia congênita: 9:Ignorado 1:Sim 2:Não

asfixia_1 <- dados |> 
    filter(Peso >= 2500 & (IDANOMAL == 2) | (IDANOMAL == '' | is.na(IDANOMAL)) & 
           (CODANOMAL == '' | is.na(CODANOMAL))) 


asfixia_1.1 <- asfixia_1 |> 
  filter(APGAR5 < 7)

sum(asfixia_1.1$Nascimentos) # temos 156960 nascimentos com essas condições

write.table(asfixia_1.1, 'asfixia1_2012_2021.csv', sep = ";", dec = ".", row.names = FALSE)

########## asfixia 2 ##########
asfixia_2 <- dados

codigos_anomalia <- str_extract_all(asfixia_2$CODANOMAL, "\\D\\d{3}")

# Determine o número máximo de códigos em uma observação
max_codigos <- max(sapply(codigos_anomalia, length))

# Crie variáveis para cada código de anomalia (neste caso temos no máximo 5)
for (i in 1:max_codigos) {
  col_name <- paste0("codigo_", i)
  asfixia_2[, col_name] <- sapply(codigos_anomalia, function(x) ifelse(length(x) >= i, x[i], NA))
}



codigos_indesejados  <- c('Q000', 'Q001', 'Q002', 
                          'Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019', 
                          'Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059', 
                          'Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029', 
                          'Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209', 
                          'Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219', 
                          'Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229', 
                          'Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239', 
                          'Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249', 
                          'Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259', 
                          'Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269', 
                          'Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279', 
                          'Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289', 
                          'Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359', 
                          'Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369', 
                          'Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379', 
                          'Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549',
                          'Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569', 
                          'Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669', 
                          'Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699', 
                          'Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719', 
                          'Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729', 
                          'Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739', 
                          'Q743', 'Q792', 'Q793', 
                          'Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')



# Filtrar o banco de dados

asfixia_2 <- asfixia_2 |>
  filter((Peso >= 2500 & (IDANOMAL == 2) | (IDANOMAL == '' | is.na(IDANOMAL)) & 
            (CODANOMAL == '' | is.na(CODANOMAL))) | (
              !str_detect(codigo_1, paste0("^(", paste(codigos_indesejados, collapse = "|"), ")")) &
                !str_detect(codigo_2, paste0("^(", paste(codigos_indesejados, collapse = "|"), ")")) &
                !str_detect(codigo_3, paste0("^(", paste(codigos_indesejados, collapse = "|"), ")")) &
                !str_detect(codigo_4, paste0("^(", paste(codigos_indesejados, collapse = "|"), ")")) &
                !str_detect(codigo_5, paste0("^(", paste(codigos_indesejados, collapse = "|"), ")"))
           )
  )


asfixia_2.1 <- asfixia_2 |> 
  filter(APGAR5 < 7)

sum(asfixia_2.1$Nascimentos) # 157096

write.table(asfixia_2.1, 'asfixia2_2012_2021.csv', sep = ";", dec = ".", row.names = FALSE)


###################### malformação 


malformacao <- dados 

codigos_anomalia <- str_extract_all(malformacao$CODANOMAL, "\\D\\d{3}")

# Determine o número máximo de códigos em uma observação
max_codigos <- max(sapply(codigos_anomalia, length))

# Crie variáveis para cada código de anomalia (neste caso temos no máximo 5)
for (i in 1:max_codigos) {
  col_name <- paste0("codigo_", i)
  malformacao[, col_name] <- sapply(codigos_anomalia, function(x) ifelse(length(x) >= i, x[i], NA))
}



codigos <- c('Q000', 'Q001', 'Q002', 
             'Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019', 
             'Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059', 
             'Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029', 
             'Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209', 
             'Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219', 
             'Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229', 
             'Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239', 
             'Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249', 
             'Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259', 
             'Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269', 
             'Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279', 
             'Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289', 
             'Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359', 
             'Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369', 
             'Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379', 
             'Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549',
             'Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569', 
             'Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669', 
             'Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699', 
             'Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719', 
             'Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729', 
             'Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739', 
             'Q743', 'Q792', 'Q793', 
             'Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')


filtro_inicio <- function(variavel, codigos) {
  grepl(paste(codigos, collapse = "|"), variavel)
}

filtro_malformacao <- malformacao %>%
  filter(str_detect(codigo_1, paste(codigos, collapse = "|")) |
           str_detect(codigo_2, paste(codigos, collapse = "|")) |
           str_detect(codigo_3, paste(codigos, collapse = "|")) |
           str_detect(codigo_4, paste(codigos, collapse = "|")) |
           str_detect(codigo_5, paste(codigos, collapse = "|")))

filtro_malformacao <- filtro_malformacao |> 
  gather(codigo, valor, codigo_1:codigo_5) |> 
  filter(str_detect(valor, paste0("^(", paste(codigos, collapse = "|"), ")")))

codigos <- c("Q01", "Q05", "Q02", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q35", "Q36", "Q37", "Q54", "Q56", "Q66", "Q69", "Q71", "Q72", "Q73", "Q90")

filtro_malformacao$codigo_cid <- ifelse(substr(filtro_malformacao$valor, 1, 3) %in% codigos, substr(filtro_malformacao$valor, 1, 3), filtro_malformacao$valor)


# Vetores com os códigos para cada grupo de anomalias congênitas
defeitos_de_tubo_neural <- c('Q000', 'Q001', 'Q002', 
                             'Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019', 
                             'Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059')


microcefalia <- c('Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029')

cardiopatias_congenitas <- c(  'Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209', 
                               'Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219', 
                               'Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229', 
                               'Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239', 
                               'Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249', 
                               'Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259', 
                               'Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269', 
                               'Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279', 
                               'Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289')


fendas_orais <- c('Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359', 
                  'Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369', 
                  'Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379')

defeitos_de_orgaos_genitais <- c('Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549',
                                 'Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569')

defeitos_de_membros <- c('Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669', 
                         'Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699', 
                         'Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719', 
                         'Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729', 
                         'Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739',
                         'Q743')

defeitos_de_parede_abdominal <- c('Q792', 'Q793')

sindrome_de_down <- c('Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')


map_grupo <- function(valor) {
  if (substr(valor, 1, 4) %in% defeitos_de_tubo_neural) {
    return('Defeitos de tubo neural')
  } else if (substr(valor, 1, 4) %in% microcefalia) {
    return('Microcefalia')
  } else if (substr(valor, 1, 4) %in% cardiopatias_congenitas) {
    return('Cardiopatias congênitas')
  } else if (substr(valor, 1, 4) %in% fendas_orais) {
    return('Fendas orais')
  } else if (substr(valor, 1, 4) %in% defeitos_de_orgaos_genitais) {
    return('Defeitos de órgãos genitais')
  } else if (substr(valor, 1, 4) %in% defeitos_de_membros) {
    return('Defeitos de membros')
  } else if (substr(valor, 1, 4) %in% defeitos_de_parede_abdominal) {
    return('Defeitos da parede abdominal')
  } else if (substr(valor, 1, 4) %in% sindrome_de_down) {
    return('Síndrome de Down')
  } else {
    return(NA)
  }
}


# Aplicar a função ao DataFrame
filtro_malformacao$grupo_de_anomalias_congenitas <- sapply(filtro_malformacao$valor, map_grupo)

### criar descricao

# Defina os vetores de anomalias
Anencefalia <- c('Q000')
Craniorraquisquise <- c('Q001')
Iniencefalia <- c('Q002')
Encefalocele <- c('Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019')
Espinha_bifida <- c('Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059')
Microcefalia <- c('Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029')
Malformacoes_congenitas_das_camaras_e_das_comunicacoes_cardiacas <- c('Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209')
Malformacoes_congenitas_dos_septos_cardiacos <- c('Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219')
Malformacoes_congenitas_das_valvas_pulmonar_e_tricuspide <- c('Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229')
Malformacoes_congenitas_das_valvas_aortica_e_mitral <- c('Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239')
Outras_malformacoes_congenitas_do_coracao <- c('Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249')
Malformacoes_congenitas_das_grandes_arterias <- c('Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259')
Malformacoes_congenitas_das_grandes_veias <- c('Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269')
Outras_malformacoes_congenitas_do_sistema_vascular_periferico <- c('Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279')
Outras_malformacoes_congenitas_do_aparelho_circulatorio <- c('Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289')
Fenda_palatina <- c('Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359')
Fenda_labial <- c('Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369')
Fenda_labial_com_fenda_palatina <- c('Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379')
Hipospadia <- c('Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549')
Sexo_indeterminado_e_pseudo_hermafroditismo <- c('Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569')
Deformidades_congenitas_do_pe <- c('Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669')
Polidactilia <- c('Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699')
Defeitos_por_reducao_do_membro_superior <- c('Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719')
Defeitos_por_reducao_do_membro_inferior <- c('Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729')
Defeitos_por_reducao_de_membro_nao_especificado <- c('Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739')
Artrogripose_congenita_multipla <- c('Q743')
Enxofalia <- c('Q792')
Gastrite <- c('Q793')
Sindrome_de_down <- c('Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')

# Função para obter a descrição com base no valor
obter_descricao <- function(valor) {
  if (valor %in% Anencefalia) {
    return("Anencefalia")
  } else if (valor %in% Craniorraquisquise) {
    return("Craniorraquisquise")
  } else if (valor %in% Iniencefalia) {
    return("Iniencefalia")
  } else if (valor %in% Encefalocele) {
    return("Encefalocele")
  } else if (valor %in% Espinha_bifida) {
    return("Espinha bifida")
  } else if (valor %in% Microcefalia) {
    return("Microcefalia")
  } else if (valor %in% Malformacoes_congenitas_das_camaras_e_das_comunicacoes_cardiacas) {
    return("Malformações congênitas das câmaras e das comunicações cardíacas")
  } else if (valor %in% Malformacoes_congenitas_dos_septos_cardiacos) {
    return("Malformações congênitas dos septos cardíacos")
  } else if (valor %in% Malformacoes_congenitas_das_valvas_pulmonar_e_tricuspide) {
    return("Malformações congênitas das valvas pulmonar e tricúspide")
  } else if (valor %in% Malformacoes_congenitas_das_valvas_aortica_e_mitral) {
    return("Malformações congênitas das valvas aórtica e mitral")
  } else if (valor %in% Outras_malformacoes_congenitas_do_coracao) {
    return("Outras malformações congênitas do coração")
  } else if (valor %in% Malformacoes_congenitas_das_grandes_arterias) {
    return("Malformações congênitas das grandes artérias")
  } else if (valor %in% Malformacoes_congenitas_das_grandes_veias) {
    return("Malformações congênitas das grandes veias")
  } else if (valor %in% Outras_malformacoes_congenitas_do_sistema_vascular_periferico) {
    return("Outras malformações congênitas do sistema vascular periférico")
  } else if (valor %in% Outras_malformacoes_congenitas_do_aparelho_circulatorio) {
    return("Outras malformações congênitas do aparelho circulatório")
  } else if (valor %in% Fenda_palatina) {
    return("Fenda palatina")
  } else if (valor %in% Fenda_labial) {
    return("Fenda labial")
  } else if (valor %in% Fenda_labial_com_fenda_palatina) {
    return("Fenda labial com fenda palatina")
  } else if (valor %in% Hipospadia) {
    return("Hipospadia")
  } else if (valor %in% Sexo_indeterminado_e_pseudo_hermafroditismo) {
    return("Sexo indeterminado e pseudo-hermafroditismo")
  } else if (valor %in% Deformidades_congenitas_do_pe) {
    return("Deformidades congênitas do pé")
  } else if (valor %in% Polidactilia) {
    return("Polidactilia")
  } else if (valor %in% Defeitos_por_reducao_do_membro_superior) {
    return("Defeitos por redução do membro superior")
  } else if (valor %in% Defeitos_por_reducao_do_membro_inferior) {
    return("Defeitos por redução do membro inferior")
  } else if (valor %in% Defeitos_por_reducao_de_membro_nao_especificado) {
    return("Defeitos por redução de membro não especificado")
  } else if (valor %in% Artrogripose_congenita_multipla) {
    return("Artrogripose congênita múltipla")
  } else if (valor %in% Enxofalia) {
    return("Enxofalia")
  } else if (valor %in% Gastrite) {
    return("Gastrite")
  } else if (valor %in% Sindrome_de_down) {
    return("Síndrome de Down")
  } else {
    return("Outra anomalia")
  }
}


# Aplique a função para criar a variável "descricao"
filtro_malformacao$descricao <- sapply(filtro_malformacao$valor, obter_descricao)


write.table(filtro_malformacao, 'malformacao1_2012_2021.csv', sep = ";", dec = ".", row.names = FALSE)



################### criação das bases para o painel


asfixia_1 <- read.csv('asfixia1_2012_2021.csv', sep = ";")

asfixia_1 <- asfixia_1[,c(3:10)]

asfixia_1 <- asfixia_1 |> 
  rename(total_de_nascidos_vivos = Nascimentos)

#write.table(asfixia_1, 'asfixia1_2012_2021.csv', sep = ";", dec = ".", row.names = FALSE)


####


asfixia_2 <- read.csv('asfixia2_2012_2021.csv', sep = ";")

asfixia_2 <- asfixia_2[,c(3:10)]

asfixia_2 <- asfixia_2 |> 
  rename(total_de_nascidos_vivos = Nascimentos)

#write.table(asfixia_2, 'asfixia2_2012_2021.csv', sep = ";", dec = ".", row.names = FALSE)


####

malformacao <- read.csv('malformacao1_2012_2021.csv', sep = ";")

malformacao <- malformacao[,c(3:15)]

malformacao <- malformacao |> 
  rename(nascidos_vivos_anomalia = Nascimentos,
         codmunres = Codigo,
         ano = Ano,
         anomalia = valor)

malformacao <- malformacao |> 
  select(codmunres,
         ano,
         anomalia, 
         grupo_de_anomalias_congenitas,
         descricao,
         codigo_cid,
         nascidos_vivos_anomalia)

write.table(malformacao, 'malformacao_2012_2021.csv', sep = ";", dec = ".", row.names = FALSE)


asfixia_1 <- asfixia_1 |> 
  group_by(Codigo, Ano) |> 
  summarise(nascidos_vivos_asfixia1 = sum(total_de_nascidos_vivos)) |> 
  ungroup() |> 
  rename(codmunres = Codigo,
         ano = Ano)

df_bloco8 <- left_join(df_bloco8, asfixia_1)

df_bloco8$nascidos_vivos_asfixia1[is.na(df_bloco8$nascidos_vivos_asfixia1)] <- 0


asfixia_2 <- asfixia_2 |> 
  group_by(Codigo, Ano) |> 
  summarise(nascidos_vivos_asfixia2 = sum(total_de_nascidos_vivos)) |> 
  ungroup() |> 
  rename(codmunres = Codigo,
         ano = Ano)

df_bloco8 <- left_join(df_bloco8, asfixia_2)

df_bloco8$nascidos_vivos_asfixia2[is.na(df_bloco8$nascidos_vivos_asfixia2)] <- 0


write.table(df_bloco8, 'asfixia_2012_2021.csv', sep = ";", dec = ".", row.names = FALSE)















