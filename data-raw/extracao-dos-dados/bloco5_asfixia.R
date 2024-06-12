################################
#################### microdatasus
################################

library(tidyverse)
library(microdatasus)
library(data.table)

sinasc12 <- microdatasus::fetch_datasus(year_start = 2012, year_end = 2012, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc13 <- microdatasus::fetch_datasus(year_start = 2013, year_end = 2013, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc14 <- microdatasus::fetch_datasus(year_start = 2014, year_end = 2014, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc15 <- microdatasus::fetch_datasus(year_start = 2015, year_end = 2015, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc16 <- microdatasus::fetch_datasus(year_start = 2016, year_end = 2016, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc17 <- microdatasus::fetch_datasus(year_start = 2017, year_end = 2017, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc18 <- microdatasus::fetch_datasus(year_start = 2018, year_end = 2018, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc19 <- microdatasus::fetch_datasus(year_start = 2019, year_end = 2019, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc20 <- microdatasus::fetch_datasus(year_start = 2020, year_end = 2020, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc21 <- microdatasus::fetch_datasus(year_start = 2021, year_end = 2021, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

sinasc22 <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022, information_system = 'SINASC', vars = c("CODMUNRES", "PESO", "APGAR5", "IDANOMAL", "CODANOMAL"))

# dados preliminares  SINASC 2023

sinasc23 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";")
sinasc23 <- sinasc23 |>
  select(CODMUNRES, PESO, APGAR5, IDANOMAL, CODANOMAL) |>
  mutate(CODANOMAL = ifelse(CODANOMAL == "", NA, CODANOMAL))

sinasc23$CODMUNRES <- as.character(sinasc23$CODMUNRES)

## criando coluna de anos
sinasc12$Ano <- 2012
sinasc13$Ano <- 2013
sinasc14$Ano <- 2014
sinasc15$Ano <- 2015
sinasc16$Ano <- 2016
sinasc17$Ano <- 2017
sinasc18$Ano <- 2018
sinasc18$Ano <- 2018
sinasc19$Ano <- 2019
sinasc20$Ano <- 2020
sinasc21$Ano <- 2021
sinasc22$Ano <- 2022
sinasc23$Ano <- 2023


sinasc_microdatasus <- dplyr::bind_rows(sinasc12, sinasc13, sinasc14, sinasc15, sinasc16, sinasc17, sinasc18, sinasc19, sinasc20, sinasc21, sinasc22, sinasc23)

#write.table(sinasc_microdatasus, 'data-raw/csv/bruto_sinasc_microdatasus_2012_2023.csv', sep = ";", dec = ".", row.names = FALSE)

#### agrupando

#sinasc_microdatasus <- read.csv('data-raw/csv/bruto_sinasc_microdatasus_2012_2023.csv', sep = ";")

sinasc_microdatasus <- sinasc_microdatasus |>
  group_by(CODMUNRES, Ano, PESO, APGAR5, IDANOMAL, CODANOMAL) |>
  summarise(Nascimentos = n())

#### salvar base agrupada
#write.table(sinasc_microdatasus, 'data-raw/csv/sinasc_microdatasus_2012_2023.csv', sep = ";", dec = ".", row.names = FALSE)


###################
# TRATAMENTO DOS DADOS PARA CRIAÇÃO DAS TABELAS
##################
#dados <- read.csv('data-raw/csv/sinasc_microdatasus_2012_2023.csv', sep = ";")
dados <- sinasc_microdatasus
tabela_aux_municipios <- read.csv("data-raw/csv/tabela_aux_municipios.csv", row.names = 1)

#### adicionando uf e municipio os dados

tabela_aux_municipios <- tabela_aux_municipios[,c(1, 2, 8)] |>
  rename(CODMUNRES = codmunres)


dados <- dados |>
  filter(CODMUNRES %in% tabela_aux_municipios$CODMUNRES)

dados <- left_join(dados, tabela_aux_municipios)


########## asfixia 1 ##########
## Asfixia 1

asfixia1 <- dados |>
  filter(PESO >= 2500 & ((IDANOMAL == 2) | ((IDANOMAL == '' | is.na(IDANOMAL)) &
                                              (CODANOMAL == '' | is.na(CODANOMAL)))))

#uma dúvida: será que tem dados faltantes para peso?
unique(asfixia1$PESO)
#Tem sim: 9999 é peso ignorado

asfixia12 <- asfixia1 %>%
  filter(PESO < 9999)


asfixia12$APGAR5 <- as.numeric(asfixia12$APGAR5)

##O apgar 99 é dado faltante e criar indicadora de apgar menor 7
asfixia13 <- asfixia12 %>%
  filter(APGAR5 < 99) %>%
  mutate(apgar5_menor7 = ifelse(APGAR5 < 7, 1, 0))

# View(asfixia13)

asfixia14 <- asfixia13 %>%
  group_by(uf, municipio, CODMUNRES, Ano) %>%
  summarise(total_de_nascidos_vivos = n()) # total_de_nascidos_vivos = peso <2500 & sem anomalia

#filtrar apgar menor 7
asfixia14_1 <- asfixia13 %>%
  group_by(uf, municipio, CODMUNRES, Ano) %>%
  summarise(nascidos_vivos_asfixia1 = sum(apgar5_menor7 == 1))
asfixia1_final <- left_join(asfixia14, asfixia14_1,
                            by = c("uf", "municipio",
                                   "CODMUNRES", "Ano"))

asfixia1_final <- asfixia1_final %>%
  mutate(nascidos_vivos_asfixia1  = ifelse(is.na(nascidos_vivos_asfixia1), 0, nascidos_vivos_asfixia1))

#write.csv(asfixia1_final, "data-raw/csv/asfixia1_2012_2023.csv")

###################### malformação


malformacao <- dados

codigos_anomalia <- str_extract_all(malformacao$CODANOMAL, "[A-Z]\\d{3}|[A-Z]\\d{2}X|[A-Z]\\d{2}")

# Determine o número máximo de códigos em uma observação
max_codigos <- max(sapply(codigos_anomalia, length))
# Crie variáveis para cada código de anomalia (neste caso temos no máximo 5)
for (i in 1:max_codigos) {
  col_name <- paste0("codigo_", i)
  malformacao[, col_name] <- sapply(codigos_anomalia, function(x) ifelse(length(x) >= i, x[i], NA))
}
codigos <- c('Q000', 'Q001', 'Q002',
             'Q01X','Q01', 'Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019',
             'Q05X','Q05', 'Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059',
             'Q02X', 'Q02', 'Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029',
             'Q20X', 'Q20', 'Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209',
             'Q21X', 'Q21', 'Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219',
             'Q22X', 'Q22', 'Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229',
             'Q23X', 'Q23', 'Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239',
             'Q24X', 'Q24', 'Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249',
             'Q25X', 'Q25', 'Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259',
             'Q26X', 'Q26', 'Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269',
             'Q27X', 'Q27', 'Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279',
             'Q28X', 'Q28', 'Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289',
             'Q35X', 'Q35', 'Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359',
             'Q36X', 'Q36', 'Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369',
             'Q37X', 'Q37', 'Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379',
             'Q54X', 'Q54', 'Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549',
             'Q56X', 'Q56', 'Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569',
             'Q66X', 'Q66', 'Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669',
             'Q69X', 'Q69', 'Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699',
             'Q71X', 'Q71', 'Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719',
             'Q72X', 'Q72', 'Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729',
             'Q73X', 'Q73', 'Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739',
             'Q743', 'Q792', 'Q793',
             'Q90X', 'Q90', 'Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')
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
                             'Q01X','Q01', 'Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019',
                             'Q05X','Q05','Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059')
microcefalia <- c('Q02X','Q02','Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029')
cardiopatias_congenitas <- c(  'Q20X','Q20','Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209',
                               'Q21X','Q21','Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219',
                               'Q22X','Q22','Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229',
                               'Q23X','Q23','Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239',
                               'Q24X','Q24','Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249',
                               'Q25X','Q25','Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259',
                               'Q26X','Q26','Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269',
                               'Q27X','Q27','Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279',
                               'Q28X','Q28','Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289')
fendas_orais <- c('Q35X','Q35','Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359',
                  'Q36X','Q36','Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369',
                  'Q37X','Q37','Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379')
defeitos_de_orgaos_genitais <- c('Q54X','Q54','Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549',
                                 'Q56X','Q56','Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569')
defeitos_de_membros <- c('Q66X','Q66', 'Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669',
                         'Q69X','Q69', 'Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699',
                         'Q71X','Q71', 'Q710', 'Q711', 'Q712', 'Q713', 'Q714', 'Q715', 'Q716', 'Q717', 'Q718', 'Q719',
                         'Q72X','Q72', 'Q720', 'Q721', 'Q722', 'Q723', 'Q724', 'Q725', 'Q726', 'Q727', 'Q728', 'Q729',
                         'Q73X','Q73', 'Q730', 'Q731', 'Q732', 'Q733', 'Q734', 'Q735', 'Q736', 'Q737', 'Q738', 'Q739',
                         'Q743')
defeitos_de_parede_abdominal <- c('Q792', 'Q793')
sindrome_de_down <- c('Q90X','Q90','Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909')
map_grupo <- function(valor) {
  # Converter para maiúsculas para tornar a correspondência sem distinção entre maiúsculas e minúsculas
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
Encefalocele <- c('Q01X','Q010', 'Q011', 'Q012', 'Q013', 'Q014', 'Q015', 'Q016', 'Q017', 'Q018', 'Q019')
Espinha_bifida <- c('Q05X', 'Q050', 'Q051', 'Q052', 'Q053', 'Q054', 'Q055', 'Q056', 'Q057', 'Q058', 'Q059')
Microcefalia <- c('Q02X','Q02','Q020', 'Q021', 'Q022', 'Q023', 'Q024', 'Q025', 'Q026', 'Q027', 'Q028', 'Q029')
Malformacoes_congenitas_das_camaras_e_das_comunicacoes_cardiacas <- c('Q200', 'Q201', 'Q202', 'Q203', 'Q204', 'Q205', 'Q206', 'Q207', 'Q208', 'Q209')
Malformacoes_congenitas_dos_septos_cardiacos <- c('Q21X','Q210', 'Q211', 'Q212', 'Q213', 'Q214', 'Q215', 'Q216', 'Q217', 'Q218', 'Q219')
Malformacoes_congenitas_das_valvas_pulmonar_e_tricuspide <- c('Q220', 'Q221', 'Q222', 'Q223', 'Q224', 'Q225', 'Q226', 'Q227', 'Q228', 'Q229')
Malformacoes_congenitas_das_valvas_aortica_e_mitral <- c('Q230', 'Q231', 'Q232', 'Q233', 'Q234', 'Q235', 'Q236', 'Q237', 'Q238', 'Q239')
Outras_malformacoes_congenitas_do_coracao <- c('Q24X','Q240', 'Q241', 'Q242', 'Q243', 'Q244', 'Q245', 'Q246', 'Q247', 'Q248', 'Q249')
Malformacoes_congenitas_das_grandes_arterias <- c('Q250', 'Q251', 'Q252', 'Q253', 'Q254', 'Q255', 'Q256', 'Q257', 'Q258', 'Q259')
Malformacoes_congenitas_das_grandes_veias <- c('Q260', 'Q261', 'Q262', 'Q263', 'Q264', 'Q265', 'Q266', 'Q267', 'Q268', 'Q269')
Outras_malformacoes_congenitas_do_sistema_vascular_periferico <- c('Q270', 'Q271', 'Q272', 'Q273', 'Q274', 'Q275', 'Q276', 'Q277', 'Q278', 'Q279')
Outras_malformacoes_congenitas_do_aparelho_circulatorio <- c('Q280', 'Q281', 'Q282', 'Q283', 'Q284', 'Q285', 'Q286', 'Q287', 'Q288', 'Q289')
Fenda_palatina <- c('Q35X', 'Q350', 'Q351', 'Q352', 'Q353', 'Q354', 'Q355', 'Q356', 'Q357', 'Q358', 'Q359')
Fenda_labial <- c('Q360', 'Q361', 'Q362', 'Q363', 'Q364', 'Q365', 'Q366', 'Q367', 'Q368', 'Q369')
Fenda_labial_com_fenda_palatina <- c('Q37X', 'Q370', 'Q371', 'Q372', 'Q373', 'Q374', 'Q375', 'Q376', 'Q377', 'Q378', 'Q379')
Hipospadia <- c('Q540', 'Q541', 'Q542', 'Q543', 'Q544', 'Q545', 'Q546', 'Q547', 'Q548', 'Q549')
Sexo_indeterminado_e_pseudo_hermafroditismo <- c('Q560', 'Q561', 'Q562', 'Q563', 'Q564', 'Q565', 'Q566', 'Q567', 'Q568', 'Q569')
Deformidades_congenitas_do_pe <- c('Q66X','Q660', 'Q661', 'Q662', 'Q663', 'Q664', 'Q665', 'Q666', 'Q667', 'Q668', 'Q669')
Polidactilia <- c('Q69X','Q690', 'Q691', 'Q692', 'Q693', 'Q694', 'Q695', 'Q696', 'Q697', 'Q698', 'Q699')
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
    return("Exonfalia")
  } else if (valor %in% Gastrite) {
    return("Gastrosquise")
  } else if (valor %in% Sindrome_de_down) {
    return("Síndrome de Down")
  } else {
    return("Outra anomalia")
  }
}
# Aplique a função para criar a variável "descricao"
filtro_malformacao$descricao <- sapply(filtro_malformacao$valor, obter_descricao)

#write.table(filtro_malformacao, 'data-raw/csv/malformacao_2012_2023.csv', sep = ";", dec = ".", row.names = FALSE)


###################


#asfixia_1 <- read.csv('asfixia1_2012_2023.csv', sep = ",")

asfixia_1 <- asfixia1_final


df_bloco8 <- asfixia_1 |>
  group_by(CODMUNRES, Ano) |>
  summarise(nascidos_vivos_asfixia1 = sum(nascidos_vivos_asfixia1),
            total_de_nascidos_vivos = sum(total_de_nascidos_vivos)) |>
  ungroup() |>
  rename(codmunres = CODMUNRES,
         ano = Ano)

write.table(df_bloco8, 'data-raw/csv/asfixia_2012_2023.csv', sep = ";", dec = ".", row.names = FALSE)


####

#malformacao <- read.csv('data-raw/csv/malformacao_2012_2023.csv', sep = ";")
#malformacao <- malformacao[,c(3:12)]

malformacao <- filtro_malformacao

malformacao <- malformacao |>
  rename(nascidos_vivos_anomalia = Nascimentos,
         codmunres = CODMUNRES,
         ano = Ano,
         anomalia = valor)


malformacao <- malformacao |>
  dplyr::select(codmunres,
         ano,
         anomalia,
         grupo_de_anomalias_congenitas,
         descricao,
         nascidos_vivos_anomalia)

malformacao <- malformacao[,-c(1,2,3)]

write.table(malformacao, 'data-raw/csv/malformacao_2012_2023.csv', sep = ";", dec = ".", row.names = FALSE)
malformacao2 <- read.csv('data-raw/csv/malformacao_2012_2022.csv', sep = ";")
malformacao <- rbind(malformacao, malformacao2)


# adicionando malformacao geral a base de asfixia

df_bloco8 <- read.csv("data-raw/csv/asfixia_2012_2023.csv", sep = ';')

tabela_aux_municipios <- read.csv("data-raw/csv/tabela_aux_municipios.csv")
#
# sinasc_microdatasus_2012_2023 <- data.table::fread("data-raw/csv/sinasc_microdatasus_2012_2023.csv")

sinasc_microdatasus_2012_2023 <- sinasc_microdatasus


malformacao_geral <- sinasc_microdatasus_2012_2023 |>
  dplyr::filter((IDANOMAL == 1) | !is.na(CODANOMAL)) |>
  janitor::clean_names()


malformacao_geral <- malformacao_geral |>
  filter(codmunres %in% tabela_aux_municipios$codmunres)

dados <- left_join(malformacao_geral, tabela_aux_municipios)

malformacao_geral1 <- malformacao_geral |>
  group_by(codmunres, ano) %>%
  summarise(total_de_nascidos_malformacao = n())


df_bloco8 <- left_join(df_bloco8, malformacao_geral1)

df_bloco8[is.na(df_bloco8)] <- 0

write.table(df_bloco8, 'data-raw/csv/asfixia_2012_2023.csv', sep = ";", dec = ".", row.names = FALSE)
















