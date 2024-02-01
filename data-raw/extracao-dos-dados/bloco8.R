library(tidyverse)

dados_obitos_maternos <- read_delim("data-raw/csv/dados_oobr_obitos_grav_puerp_maternos_oficiais_2023.csv",
                                    delim = ",", escape_double = FALSE, trim_ws = TRUE) |>
  rename(codmunres = codigo) |>
  filter(ano >= 2012)


dados_obitos_fetais <- read.csv("data-raw/csv/dados_obitos_fetais.csv") |>
  rename(codmunres = codigo)

dados_obitos_neonatais <- read.csv("data-raw/csv/dados_obitos_neonatais.csv") |>
  rename(codmunres = codigo)


dados_obitos_maternos$cid <- sapply(strsplit(dados_obitos_maternos$causabas_categoria, " "), function(x) x[1])

dados_obitos_fetais$cid <- sapply(strsplit(dados_obitos_fetais$causabas_categoria, " "), function(x) x[1])

dados_obitos_neonatais$cid <- sapply(strsplit(dados_obitos_neonatais$causabas_categoria, " "), function(x) x[1])


###############################
###############################
###############################

dados_obitos_neonatais <- dados_obitos_neonatais |>
  mutate(
    idade_dias = case_when(
      idade < 207 ~ "0 a 6 dias",
      idade >= 207 & idade < 228 ~ "7 a 27 dias",
      idade >= 228 ~ "28 a 364 dias"
    )
  )

dados_obitos_neonatais$idade_dias <- factor(dados_obitos_neonatais$idade_dias,
                                            levels = c("0 a 6 dias", "7 a 27 dias",
                                                       "28 a 364 dias"))


###############################
#materno
###############################

materno <- c("D39", "F53", "O08", "O94", "O95")

materno_garbage <- dados_obitos_maternos |>
  filter(cid %in% materno)


###############################
#fetal
###############################

fetal <- c(sprintf("P%02d", 90:96),
           sprintf("Q%02d", 10:18),
           sprintf("Q%02d", 35:37),
           sprintf("Q%02d", 80:89),
           sprintf("Q%02d", 90:99),
           sprintf("R%02d", 0:94),
           sprintf("R9%d", 6:9),
           "I469", "I959", "I99", "J960", "J969", "P285"
)

fetais_garbage <- dados_obitos_fetais |>
  filter(cid %in% fetal | CAUSABAS %in% fetal)



###############################
#neonatal
###############################

mal_definidas <- c(sprintf("R%02d", 0:94),
                   sprintf("R9%d", 6:9),
                   "I469", "I959", "I99", "J960", "J969", "P285")

infantil <- c(
  sprintf("A%02d", 00:09),
  sprintf("A%02d", 30:49),
  sprintf("A%02d", 50:64),
  sprintf("A%02d", 70:64),
  sprintf("A%02d", 90:99),
  sprintf("B%02d", 00:09),
  sprintf("B%02d", 25:34),
  sprintf("B%02d", 35:49),
  sprintf("B%02d", 50:64),
  sprintf("B%02d", 65:83),
  sprintf("B%02d", 85:89),
  sprintf("B%02d", 90:97),
  sprintf("B%02d", 99:99),
  sprintf("C%02d", 00:97),
  sprintf("D%02d", 00:09),
  sprintf("D%02d", 10:48),
  sprintf("D%02d", 50:53),
  sprintf("D%02d", 55:59),
  sprintf("D%02d", 60:64),
  sprintf("D%02d", 65:69),
  sprintf("D%02d", 70:77),
  sprintf("D%02d", 80:89),
  sprintf("E%02d", 00:07),
  sprintf("E%02d", 15:16),
  sprintf("E%02d", 20:35),
  sprintf("E%02d", 50:64),
  sprintf("E%02d", 70:90),
  sprintf("F%02d", 00:39),
  sprintf("F%02d", 40:48),
  sprintf("F%02d", 50:99),
  sprintf("G%02d", 00:09),
  sprintf("G%02d", 30:32),
  sprintf("G%02d", 40:47),
  sprintf("G%02d", 50:59),
  sprintf("G%02d", 60:64),
  sprintf("G%02d", 80:83),
  sprintf("G%02d", 90:99),
  sprintf("H%02d", 00:06),
  sprintf("H%02d", 10:13),
  sprintf("H%02d", 15:22),
  sprintf("H%02d", 25:28),
  sprintf("H%02d", 30:36),
  sprintf("H%02d", 40:59),
  sprintf("H%02d", 60:62),
  sprintf("H%02d", 65:75),
  sprintf("H%02d", 80:83),
  sprintf("H%02d", 90:95),
  sprintf("I%02d", 10:15),
  sprintf("I%02d", 26:28),
  sprintf("I%02d", 30:52),
  sprintf("I%02d", 60:69),
  sprintf("I%02d", 70:79),
  sprintf("I%02d", 95:99),
  sprintf("J%02d", 00:06),
  sprintf("J%02d", 09:18),
  sprintf("J%02d", 20:22),
  sprintf("J%02d", 60:70),
  sprintf("J%02d", 80:86),
  sprintf("J%02d", 90:99),
  sprintf("J%02d", 95:99),
  sprintf("K%02d", 00:14),
  sprintf("K%02d", 20:31),
  sprintf("K%02d", 55:63),
  sprintf("K%02d", 65:67),
  sprintf("K%02d", 70:77),
  sprintf("K%02d", 80:87),
  sprintf("K%02d", 90:93),
  sprintf("L%02d", 20:30),
  sprintf("L%02d", 40:45),
  sprintf("L%02d", 50:75),
  sprintf("L%02d", 80:99),
  sprintf("M%02d", 00:25),
  sprintf("M%02d", 40:54),
  sprintf("M%02d", 60:99),
  sprintf("N%02d", 10:19),
  sprintf("N%02d", 30:51),
  sprintf("N%02d", 60:64),
  sprintf("N%02d", 80:98),
  sprintf("O%02d", 00:08),
  sprintf("O%02d", 94:99),
  sprintf("P%02d", 20:29),
  sprintf("P%02d", 35:39),
  sprintf("P%02d", 90:96),
  sprintf("Q%02d", 10:18),
  sprintf("Q%02d", 35:37),
  sprintf("Q%02d", 80:89),
  sprintf("Q%02d", 90:99),
  sprintf("S%02d", 00:99),
  sprintf("T%02d", 00:98),
  sprintf("V%02d", 87:89),
  sprintf("V%02d", 98:99),
  sprintf("W%02d", 76:76),
  sprintf("X%02d", 40:49),
  sprintf("X%02d", 59:59),
  sprintf("Y%02d", 09:34),
  sprintf("Y%02d", 85:89),
  sprintf("Y%02d", 90:98),
  mal_definidas
  # sprintf("%s%02d", rep(c("A", "B"), each = 100), 0:99),
  # sprintf("%s%02d", rep(c("C", "D"), each = 49), 0:48),
  # sprintf("%s%02d", rep(c("D", "E"), each = 40), 50:89),
  # sprintf("%s%02d", rep("E", each = 91), 0:90),
  # sprintf("%s%02d", rep("F", each = 100), 0:99),
  # sprintf("%s%02d", rep("G", each = 100), 0:99),
  # sprintf("%s%02d", rep("H", each = 60), 0:59),
  # sprintf("%s%02d", rep("H", each = 36), 60:95),
  # sprintf("%s%02d", rep("I", each = 100), 0:99),
  # sprintf("%s%02d", rep("J", each = 100), 0:99),
  # sprintf("%s%02d", rep("K", each = 94), 0:93),
  # sprintf("%s%02d", rep("L", each = 100), 0:99),
  # sprintf("%s%02d", rep("M", each = 100), 0:99),
  # sprintf("%s%02d", rep("N", each = 100), 0:99),
  # sprintf("%s%02d", rep("O", each = 100), 0:99),
  # #sprintf("%s%02d", rep("P", each = 97), 0:96),
  # #sprintf("%s%02d", rep("Q", each = 100), 0:99),
  # fetal,
  # sprintf("%s%02d", rep(c("S", "T"), each = 99), 0:98),
  # sprintf("%s%02d", rep(c("V", "W", "X", "Y"), each = 98), 1:98),
)

neonat_garbage <- dados_obitos_neonatais |>
  filter(cid %in% infantil | CAUSABAS %in% infantil)



###############################
#principais causas
###############################


afeccoes_perinatais <- c("P21", "A502", "P07")

mf_confenitas <- c(sprintf("Q%02d", 00:99))

doencas_infec <- c(sprintf("A%02d", 00:09),
                   sprintf("J%02d", 10:21),
                   sprintf("E%02d", 40:46),
                   sprintf("A%02d", 40:41),
                   "A39", "A87", "G00", "G03",
                   "J45")

principais_causas <- c(afeccoes_perinatais, mf_confenitas, doencas_infec)

fetais_causas <- dados_obitos_fetais |>
  filter(cid %in% principais_causas | CAUSABAS %in% principais_causas)

neonat_causas <- dados_obitos_neonatais |>
  filter(cid %in% principais_causas | CAUSABAS %in% principais_causas)


###############################
# causas evitaveis
###############################


# atencao a mulher na gestacao

atencao_gestac <- c("P00", "P01", "P03", "P04",
                    "P05", "P07",
                    "P22")

atencao_parto <- c("P02",
                   "P20", "P21"
)



acoes_diag <- c(sprintf("J%02d", 10:21),
                "J22",
                "A48")

acoes_saude <- c(sprintf("A%02d", 00:09),
                 sprintf("A%02d", 40:41),
                 #"A39", "A87", "G00", "G03",
                 sprintf("E%02d", 40:46),
                 #"J45",
                 "E63",
                 sprintf("W%02d", 75:84))

mal_definidas <- c(sprintf("R%02d", 0:94),
                   sprintf("R9%d", 6:9),
                   "I469", "I959", "I99", "J960", "J969", "P285")

atencao_rn <- c(#sprintf("P21%d", 0:9),
  #sprintf("P22%d", 0:9),
  sprintf("P23%d", 0:9),
  sprintf("P24%d", 0:9),
  sprintf("P25%d", 0:9),
  sprintf("P26%d", 0:9),
  sprintf("P27%d", 0:9),
  sprintf("P28%d", 0:9),
  sprintf("P29%d", 0:9),
  sprintf("P35%d", 1:2),
  sprintf("P35%d", 4:9),
  sprintf("P36%d", 0:9),
  sprintf("P37%d", 0:9),
  sprintf("P38%d", 0:9),
  sprintf("P39%d", 0:9)
)


filtro_evitaveis <- c(atencao_gestac, atencao_parto, acoes_diag,
                acoes_saude, atencao_rn, mal_definidas)


fetais_evitaveis <- dados_obitos_fetais |>
  filter(cid %in% filtro_evitaveis | CAUSABAS %in% filtro_evitaveis) |>
  mutate(grupo = case_when(
    cid %in% atencao_gestac ~ "Aten??o ? mulher na gesta??o",
    cid %in% atencao_parto ~ "Aten??o ? mulher no parto",
    cid %in% atencao_rn | CAUSABAS %in% atencao_rn  ~ "Aten??o ao rec?m-nascido",
    cid %in% acoes_diag ~ "A??es diagn?stico e tratamento adequado",
    cid %in% acoes_saude ~ "A??es adequadas de promo??o ? sa?de",
    cid %in% mal_definidas | CAUSABAS %in% mal_definidas ~ "Causas mal definidas",
    TRUE ~ "Demais causas"
  ))

neonat_evitaveis <- dados_obitos_neonatais |>
  filter(cid %in% filtro_evitaveis | CAUSABAS %in% filtro_evitaveis) |>
  mutate(grupo = case_when(
    cid %in% atencao_gestac ~ "Aten??o ? mulher na gesta??o",
    cid %in% atencao_parto ~ "Aten??o ? mulher no parto",
    cid %in% atencao_rn | CAUSABAS %in% atencao_rn  ~ "Aten??o ao rec?m-nascido",
    cid %in% acoes_diag ~ "A??es diagn?stico e tratamento adequado",
    cid %in% acoes_saude ~ "A??es adequadas de promo??o ? sa?de",
    cid %in% mal_definidas | CAUSABAS %in% mal_definidas ~ "Causas mal definidas",
    TRUE ~ "Demais causas"
  ))


#############################
#baixa dados
#############################

write.csv(materno_garbage, "data-raw/csv/materno_garbage_2012-2022.csv", row.names = FALSE)
write.csv(fetais_garbage, "data-raw/csv/fetais_garbage_2012-2022.csv", row.names = FALSE)
write.csv(neonat_garbage, "data-raw/csv/neonat_garbage_2012-2022.csv", row.names = FALSE)
write.csv(fetais_causas, "data-raw/csv/fetais_causas_2012-2022.csv", row.names = FALSE)
write.csv(neonat_causas, "data-raw/csv/neonat_causas_2012-2022.csv", row.names = FALSE)
write.csv(fetais_evitaveis, "data-raw/csv/fetais_evitaveis_2012-2022.csv", row.names = FALSE)
write.csv(neonat_evitaveis, "data-raw/csv/neonat_evitaveis_2012-2022.csv", row.names = FALSE)






