library( tidyverse )
library( magrittr )
library( lubridate )
library( openxlsx )
library( foreign )
library(data.table)
library(glue)


# definir diretorio onde estao os arquivos obs_cluster

arq_obs_cluster <- "data-raw/morbidade/databases/01_sih_rd/02_arquivos_tratados_long"


# definir diretorio onde estao os arquivos SP

arq_SP <- "data-raw/morbidade/databases/02_sih_sp"


# Criando um vetor com os anos considerados (2012 a 2024)
anos <- c(2012:2024)


### Descolamento prematuro de placenta (cids O450; O458; O459)

desc_plac <- c( "O450" ,"O458", "O459" )


### Placenta acreta, increta ou percreta - não disponível no SIH

### Gravidez ectópica (cids O00.0, O00.1, O00.2, O00.8, O00.9 e proc 0411020048)

ectopica <- c( "O000", "O001", "O002", "O008", "O009" )


### Hemorragia pós-parto (cids O720,  O721, O722)

hemorragia <- c( "O720", "O721", "O722" )


### Rotura uterina (cids O710, O711 e atoprof 0409060160)

rotura <- c( "O710", "O711" )


### Pré-eclâmpsia grave (cid O141)

pre_eclamp <- "O141"


### Eclâmpsia (cids O150; O151; O152; O159 e proc 303100028)

eclampsia <- c( "O150", "O151", "O152", "O159" )


### Hipertensão grave (proc 0303060107 OU 0303100036 OU
# CID "O11", "O13", "O149", "O16", "O100", "O101", "O102", "O103", "O104", "O109", "I10", "I11", "I12", "I13", "I15")

hiper_grave_cid <- c( "O11", "O13", "O149", "O16", "O100", "O101", "O102", "O103", "O104", "O109", "I10", "I11", "I12", "I13", "I15" )

hiper_grave_proc <- c ( "303060107", "303100036" )


### Encefalopatia hipertensiva (cid I674 e proc 303040211)

encef_hiper <- "I674"


### Síndrome HELLP (CID  O14.2)

hellp <- "O142"


### Endometrite (cid O85 )

endometrite <- "O85"


### Edema pulmonar (cid J81 e proc 0303060131)

edema_pulm <- "J81"


### Insuficiência respiratória (cids J80,  J96.0, J96.9, R09.2,  I26, I260, I269, O03.2, O03.7,
# O04.2, O04.7, O05.2, O05.7,O06.2, O06.7, O07.2, O07.7, O08.2,
# O88.0, O88.1,  O88.2 , O88.3, O88.8 e proc 303060140)

insu_resp <- c( "J80", "J960", "J969", "R092", "I26", "I260", "I269", "O032", "O037", "O042", "O047", "O052", "O057", "O062",
                "O067", "O072", "O077", "O082", "O880", "O881", "O882", "O883", "O888" )


### Convulsões (cid R568)

convulsao <- "R568"


### Sepse (cids A41; A41.0; A41.1; A41.2; A41.3; A41.4; A41.5; A41.8; A41.9, A40;
# A40.0; A40.1; A40.2; A40.3; A40.8; A40.9, A02.1, A22.7, A26.7, A32.7,
# A42.7, B37.7, O08.0, O75.3, R57.2, O85)

sepse <- c( "A41", "A410", "A411", "A412", "A413", "A414", "A415", "A418", "A419", "A40", "A400", "A401", "A402", "A403", "A408", "A409",
            "A021", "A227", "A267", "A327", "A427", "B377", "O080", "O753", "R572", "O85" )


### Choque (cids E86, R570, R571,  R578, R579, T811,  I46.0, O08.3, O75.1 e procs 0303060077,
# 0303060069, 0303060050 e 0303060255)

choque_cid <- c( "E86", "R570", "R571", "R578", "R579", "T811", "I460", "O083", "O751" )

choque_proc <- c( "303060077", "303060069", "303060050", "303060255" )


### Crise tireotóxica (cid E055)

crise_tireo <- "E055"


### Transfusão sanguínea (cids Z51.3 e ATOPROF 0306020068, 0306020076, 0306020084, 0306020092,
# 0306020106, 0306020114, 0306020122, 0306020130, 0306020149)

transfusao_cid <- "Z513"

transfusao_proc <- c( "306020068", "306020076", "306020084", "306020092", "306020106", "306020114", "306020122",
                      "306020130", "306020149" )


### Acesso venoso central (ATOPROF 07.02.05.009-1, 07.02.05.081-4, 03.09.06.001-0, 03.09.06.003-6)

acesso_venoso <- c( "702050091", "702050814", "309060010", "309060036" )


### Histerectomia (CID O82.2 e proc/atoprof 0411020030, 0409060100, 0409060119, 0409060127 e 0409060135)

histerectomia_cid <- "O822"

histerectomia_proc <- c( "411020030", "409060100", "409060119", "409060127", "409060135" )


### Admissão à unidade de tratamento intensivo (UTI_MES_TO > 0 e atoprof 802010105, 802010083,
# 802010091, 802010296 e 802010318)

UTI <- c( "802010105", "802010083", "802010091", "802010296", "802010318" )


# CPAV_INTUB

cpav_intubacao <- c( "O290", "O291", "O292", "O293", "O295", "O296", "O298", "O299", "O740", "O741", "O742", "O743", "O744",
                     "O746", "O747", "O748", "O749", "O890", "O891", "O892", "O893", "O895", "O896", "O898", "O899" )


### procedimentos de parto

parto_proc <- c( "310010012", "310010039", "310010047", "310010055", "411010026", "411010034", "411010042" )


### filtrando partos ou complicação no puerperio

# motivo da internação

intercorrencias_puerp <- c( "O700",	"O70", "O701", "O702", "O703", "O709", "O710", "O711", "O712", "O713", "O714",
                            "O715",	"O716",	"O717",	"O718",	"O719",	"O720",	"O721",	"O722",	"O723",	"O730",	"O731",
                            "O85", "O850", "O860", "O861", "O862", "O863", "O864", "O868", "O870", "O871", "O872",
                            "O873", "O878", "O879", "O880",	"O881",	"O882",	"O883",	"O888",	"O890",	"O891",	"O892",
                            "O893",	"O894",	"O895",	"O896",	"O898",	"O899", "O900",	"O901",	"O902",	"O903",	"O904",
                            "O905",	"O908",	"O909",	"O910",	"O911",	"O912",	"O920",	"O921",	"O922",	"O923",	"O924",
                            "O925",	"O926",	"O927", "O94" )


### proc curetagem

### filtrando quem internou por complicações ou parto

curetagem <- c("411020013")


#### Intervenção cirúrgica (13/03/2023) - (ATOPROF 0415010012, 0415020034, 0407040161, 0407040250,
# 0407040030, 0407040021, 0407040013, 0411010085, 0411010050, 0407040200, 0407040242 e 0415040027)

# Add: 0411010077, 0401010031, 0401010104

### criterio inter cirurgica ampliada ( curetagem + procedimentos com novos )

proc_cirurgicos <- c( "415010012", "415020034", "407040161", "407040250", "407040030", "407040021", "407040013",
                      "411010085", "411010050", "407040200", "407040242", "415040027", "401010031",
                      "401010104" )


# criar variavel com os estados

estados <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
             "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
             "RS","RO","RR","SC","SP","SE","TO")

# criar tabelas totais

obs_cluster_total_mun_ano <- data.frame()

obs_cluster_sumario_total_uf <- data.frame()


# ler as bases para cada estado

for( estado in estados ) {
  # ler a base BR

  sp_obs_cluster_atual <- fread(glue("{arq_obs_cluster}/{estado}_sih_rd_tratado_long_{anos[1]}_{anos[length(anos)]}.csv"), sep = ",") |>
    mutate(N_AIH = as.character(N_AIH), PROC_REA = as.character(PROC_REA))

  # selecionar apenas as variaveis a serem utilizadas

  sp_obs_cluster_atual %<>% select( N_AIH, CODMUNRES = MUNIC_RES, DT_INTER, DT_SAIDA, starts_with( "DIAG" ), starts_with( "CID_" ),
                                             UTI_MES_TO, COBRANCA, PROC_REA, AIHREF )

  sp_obs_cluster_atual %<>% mutate( UTI_MES_TO = as.integer( UTI_MES_TO ) )

  # garbage collector

  gc()


  # calculo das variaveis de interesse

  sp_obs_cluster_atual %<>%
    mutate( CPAV_DESC_PLAC_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% desc_plac ),
                                             1L, NA_integer_ ),
            CPAV_ECTOPICA_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% ectopica ) |
                                              PROC_REA == "411020048", 1L, NA_integer_ ),
            CPAV_HEMORRAGIA_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% hemorragia ),
                                              1L, NA_integer_ ),
            CPAV_ROTURA_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% rotura ) |
                                            PROC_REA == "409060160", 1L, NA_integer_ ),
            CPAV_PRE_ECLAMP_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == pre_eclamp ),
                                              1L, NA_integer_ ),
            CPAV_ECLAMPSIA_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% eclampsia ) |
                                               PROC_REA == "303100028", 1L, NA_integer_ ),
            CPAV_HIPER_GRAVE_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% hiper_grave_cid ) |
                                                 PROC_REA %in% hiper_grave_proc, 1L, NA_integer_ ),
            CPAV_ENCEF_HIPER_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == encef_hiper ) |
                                                 PROC_REA == "303040211", 1L, NA_integer_ ),
            CPAV_HELLP_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == hellp ),
                                         1L, NA_integer_ ),
            CPAV_ENDOMETRITE_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == endometrite ),
                                               1L, NA_integer_ ),
            CPAV_EDEMA_PULM_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == edema_pulm ) |
                                                PROC_REA == "303060131", 1L, NA_integer_ ),
            CPAV_INSU_RESP_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% insu_resp ) |
                                               PROC_REA == "303060140", 1L, NA_integer_ ),
            CPAV_CONVULSOES_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == convulsao ),
                                              1L, NA_integer_ ),
            CPAV_SEPSE_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% sepse ),
                                         1L, NA_integer_ ),
            CPAV_CHOQUE_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% choque_cid ) |
                                            PROC_REA %in% choque_proc, 1L, NA_integer_ ),
            CPAV_CRISE_TIREO_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == crise_tireo ),
                                               1L, NA_integer_ ),
            casos_MMG_Transfusao_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == transfusao_cid ) |
                                                 PROC_REA %in% transfusao_proc, 1L, NA_integer_ ),
            CPAV_ACESSO_VENOSO_SIH_BR = if_else( PROC_REA %in% acesso_venoso, 1L, NA_integer_ ),
            CPAV_HISTERECTOMIA_SIH_BR = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) == histerectomia_cid ) |
                                                   PROC_REA %in% histerectomia_proc, 1L, NA_integer_ ),
            casos_MMG_UTI_BR = if_else( PROC_REA %in% UTI | UTI_MES_TO > 0, 1L, NA_integer_ ),
            CPAV_INTUB = if_else( if_any( starts_with( "DIAG" ) | starts_with( "CID_" ), ~ str_replace_na( .x, "" ) %in% cpav_intubacao ),
                                  1L, NA_integer_ ),
            PARTO_PROC_BR = if_else( PROC_REA %in% parto_proc, 1L, NA_integer_ ) )


  # selecionar os clusters

  grupos_aih <- sp_obs_cluster_atual %>% count( AIHREF, name = "freq" ) %>% filter( freq > 1 )


  # separar em clusters e nao clusters

  sp_uf_obs_clusters <- sp_obs_cluster_atual %>% filter( AIHREF %in% grupos_aih$AIHREF )

  sp_uf_obs_nao_clusters <- sp_obs_cluster_atual %>% filter( ! AIHREF %in% grupos_aih$AIHREF )


  # calcular tempo de permanencia, casos_MMG_TMP_BR: INCLUIR AS INTERNAÇÕES COM TEMPO DE INTERNAÇÃO > 7 DIAS
  ### filtrando partos ou complicação no puerperio, proc curetagem

  sp_uf_obs_nao_clusters %<>%
    mutate( DT_INTER = ymd( DT_INTER ), DT_SAIDA = ymd( DT_SAIDA ), ORDEM = 1,
            tempo_de_permanencia = time_length( interval( DT_INTER, DT_SAIDA ), "days" ),
            INTERCORRENCIA_PUERP = if_else( PROC_REA == "303100010" | DIAG_PRINC %in% intercorrencias_puerp, 1L, NA_integer_ ),
            casos_MMG_TMP_BR = if_else( PARTO_PROC_BR == 1 & tempo_de_permanencia > 7 & PROC_REA != "802010024", 1L, NA_integer_ ),
            CURETAGEM = if_else( INTERCORRENCIA_PUERP == 1 & PROC_REA %in% curetagem, 1L, NA_integer_ ) )


  sp_uf_obs_clusters %<>% mutate( DT_INTER = ymd( DT_INTER ), DT_SAIDA = ymd( DT_SAIDA ) ) %>%
    group_by( AIHREF ) %>% arrange( DT_INTER, DT_SAIDA, N_AIH, .by_group = TRUE ) %>%
    mutate( ORDEM = row_number(), prim_entrada = min( DT_INTER ), ult_saida = max( DT_SAIDA ),
            tempo_de_permanencia = if_else( ORDEM == 1, time_length( interval( prim_entrada, ult_saida ), "days" ), NA_integer_ ) ) %>%
    ungroup() %>% select( - prim_entrada, - ult_saida )

  sp_uf_obs_clusters %<>%
    mutate( INTERCORRENCIA_PUERP = if_else( ORDEM == 1 & ( PROC_REA == "303100010" | DIAG_PRINC %in% intercorrencias_puerp ), 1L, NA_integer_ ) )

  sp_uf_obs_clusters %<>% group_by( AIHREF ) %>%
    mutate( casos_MMG_TMP_BR = if_else( ORDEM == 1 & PARTO_PROC_BR == 1 & tempo_de_permanencia > 7 &
                                                  all( PROC_REA != "802010024", na.rm = TRUE ), 1L, NA_integer_ ),
            CURETAGEM = if_else( any( PARTO_PROC_BR == 1 | INTERCORRENCIA_PUERP == 1, na.rm = TRUE ) &
                                   any( PROC_REA %in% curetagem, na.rm = TRUE ), 1L, NA_integer_ ) ) %>% ungroup()


  # juntar os clusters e nao clusters

  rm( sp_obs_cluster_atual )

  sp_obs_cluster_atual <- sp_uf_obs_nao_clusters %>% bind_rows( sp_uf_obs_clusters )

  # garbage collector

  gc()


  # PARTO, Internação hospitalar prolongada (> 7 dias pós-parto)
  # criterio inter cirurgica ampliada ( curetagem + procedimentos com novos ), CPAV_INTUB

  sp_obs_cluster_atual %<>%
    mutate( CPAV_INTER_CIRURGICA_SIH_BR = if_else( CURETAGEM == 1 | PROC_REA %in% proc_cirurgicos, 1L, NA_integer_ ) )


  # ler base SP

  base_sp_redu <- fread(glue("{arq_SP}/{estado}_sih_sp_filtrado_{anos[1]}_{anos[length(anos)]}.csv.gz"), sep = ";") |>
    mutate(SP_ATOPROF = as.character(SP_ATOPROF)) |>
    filter(SP_NAIH %in% sp_obs_cluster_atual$N_AIH)

  # base_sp_redu %<>% select( SP_NAIH, SP_ATOPROF )

  # garbage collector

  gc()


  # procedimentos

  base_sp_redu %<>% mutate( ACOMPANHA_BB_AP = if_else( SP_ATOPROF == "802010024", 1L, NA_integer_ ),
                            ECTOPICA_ATOPROF = if_else( SP_ATOPROF == "411020048", 1L, NA_integer_ ),
                            ROTURA_UTERINA_ATOPROF = if_else( SP_ATOPROF == "409060160", 1L, NA_integer_ ),
                            DIAG_ECLAMPSIA_ATOPROF = if_else( SP_ATOPROF == "303100028", 1L, NA_integer_ ),
                            HIPER_GRAVE_ATOPROF = if_else( SP_ATOPROF %in% hiper_grave_proc, 1L, NA_integer_ ),
                            ENCEFALOPATIA_HIPER_ATOPROF = if_else( SP_ATOPROF == "303040211", 1L, NA_integer_ ),
                            EDEMA_PULMONAR_ATOPROF = if_else( SP_ATOPROF == "303060131", 1L, NA_integer_ ),
                            INSUFICIENCIA_RESPIRATORIA_ATOPROF = if_else( SP_ATOPROF == "303060140", 1L, NA_integer_ ),
                            CHOQUE_ATOPROF = if_else( SP_ATOPROF %in% choque_proc, 1L, NA_integer_ ),
                            TRANSFUSAO_ATOPROF = if_else( SP_ATOPROF %in% transfusao_proc, 1L, NA_integer_ ),
                            CATETER_VENOSO_ATOPROF = if_else( SP_ATOPROF %in% acesso_venoso, 1L, NA_integer_ ),
                            HISTERECTOMIA_ATOPROF = if_else( SP_ATOPROF %in% histerectomia_proc, 1L, NA_integer_ ),
                            INTER_UTI_ATOPROF = if_else( SP_ATOPROF %in% UTI, 1L, NA_integer_ ),
                            INTERVENCAO_CIRURGICA_ATOPROF = if_else( SP_ATOPROF %in% proc_cirurgicos, 1L, NA_integer_ ),
                            CURETAGEM_ATOPROF = if_else( SP_ATOPROF %in% curetagem, 1L, NA_integer_ ) )


  # reduzir base SP

  base_sp_grp <- base_sp_redu %>% group_by( SP_NAIH ) %>%
    summarise( ACOMPANHA_BB_AP = if_else( any( ACOMPANHA_BB_AP == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               ECTOPICA_ATOPROF = if_else( any( ECTOPICA_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               ROTURA_UTERINA_ATOPROF = if_else( any( ROTURA_UTERINA_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               DIAG_ECLAMPSIA_ATOPROF = if_else( any( DIAG_ECLAMPSIA_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               HIPER_GRAVE_ATOPROF = if_else( any( HIPER_GRAVE_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               ENCEFALOPATIA_HIPER_ATOPROF = if_else( any( ENCEFALOPATIA_HIPER_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               EDEMA_PULMONAR_ATOPROF = if_else( any( EDEMA_PULMONAR_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               INSUFICIENCIA_RESPIRATORIA_ATOPROF = if_else( any( INSUFICIENCIA_RESPIRATORIA_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               CHOQUE_ATOPROF = if_else( any( CHOQUE_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               TRANSFUSAO_ATOPROF = if_else( any( TRANSFUSAO_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               CATETER_VENOSO_ATOPROF = if_else( any( CATETER_VENOSO_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               HISTERECTOMIA_ATOPROF = if_else( any( HISTERECTOMIA_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               INTER_UTI_ATOPROF = if_else( any( INTER_UTI_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               INTERVENCAO_CIRURGICA_ATOPROF = if_else( any( INTERVENCAO_CIRURGICA_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ),
               CURETAGEM_ATOPROF = if_else( any( CURETAGEM_ATOPROF == 1, na.rm = TRUE ), 1L, NA_integer_ ) )


  # garbage collector

  gc()


  # juntar BR e ATOPROF

  sp_obs_cluster_atual %<>% left_join( base_sp_grp |> mutate(SP_NAIH = as.character(SP_NAIH)), by = c( "N_AIH" = "SP_NAIH" ) )


  # criar variaveis BR_SP

  sp_obs_cluster_atual %<>%
    mutate( CPAV_ECTOPICA_SIH_BR_SP = if_else( CPAV_ECTOPICA_SIH_BR == 1 | ECTOPICA_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_ROTURA_SIH_BR_SP = if_else( CPAV_ROTURA_SIH_BR == 1 | ROTURA_UTERINA_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_ECLAMPSIA_SIH_BR_SP = if_else( CPAV_ECLAMPSIA_SIH_BR == 1 | DIAG_ECLAMPSIA_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_HIPER_GRAVE_SIH_BR_SP = if_else( CPAV_HIPER_GRAVE_SIH_BR == 1 | HIPER_GRAVE_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_ENCEF_HIPER_SIH_BR_SP = if_else( CPAV_ENCEF_HIPER_SIH_BR == 1 | ENCEFALOPATIA_HIPER_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_EDEMA_PULM_SIH_BR_SP = if_else( CPAV_EDEMA_PULM_SIH_BR == 1 | EDEMA_PULMONAR_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_INSU_RESP_SIH_BR_SP = if_else( CPAV_INSU_RESP_SIH_BR == 1 | INSUFICIENCIA_RESPIRATORIA_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_CHOQUE_SIH_BR_SP = if_else( CPAV_CHOQUE_SIH_BR == 1 | CHOQUE_ATOPROF == 1, 1L, NA_integer_ ),
            casos_MMG_Transfusao = if_else( casos_MMG_Transfusao_BR == 1 | TRANSFUSAO_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_ACESSO_VENOSO_SIH_BR_SP = if_else( CPAV_ACESSO_VENOSO_SIH_BR == 1 | CATETER_VENOSO_ATOPROF == 1, 1L, NA_integer_ ),
            CPAV_HISTERECTOMIA_SIH_BR_SP = if_else( CPAV_HISTERECTOMIA_SIH_BR  == 1 | HISTERECTOMIA_ATOPROF == 1, 1L, NA_integer_ ),
            casos_MMG_UTI = if_else( casos_MMG_UTI_BR == 1 | INTER_UTI_ATOPROF == 1, 1L, NA_integer_ ) )


  # separar em clusters e nao clusters

  rm( list = c( "base_sp_grp", "base_sp_redu", "sp_uf_obs_clusters", "sp_uf_obs_nao_clusters" ) )

  gc()

  sp_uf_obs_clusters <- sp_obs_cluster_atual %>% filter( AIHREF %in% grupos_aih$AIHREF )

  sp_uf_obs_nao_clusters <- sp_obs_cluster_atual %>% filter( ! AIHREF %in% grupos_aih$AIHREF )


  # calcular variaveis casos_MMG_TMP_BR_SP e CURETAGEM_BR_SP

  sp_uf_obs_nao_clusters %<>%
    mutate( casos_MMG_TMP = if_else( casos_MMG_TMP_BR == 1 & is.na( ACOMPANHA_BB_AP ), 1L, NA_integer_ ),
            CURETAGEM_BR_SP = if_else( CURETAGEM_ATOPROF == 1 & INTERCORRENCIA_PUERP == 1, 1L, NA_integer_ ) )


  sp_uf_obs_clusters %<>% group_by( AIHREF ) %>%
    mutate( casos_MMG_TMP = if_else( casos_MMG_TMP_BR == 1 & all( is.na( ACOMPANHA_BB_AP ), na.rm = TRUE ),
                                                   1L, NA_integer_ ),
            CURETAGEM_BR_SP = if_else( any( CURETAGEM_ATOPROF == 1, na.rm = TRUE ) & any( INTERCORRENCIA_PUERP == 1 | PARTO_PROC_BR == 1, na.rm = TRUE ),
                                       1L, NA_integer_ ) ) %>% ungroup()


  # juntar as bases

  rm( sp_obs_cluster_atual )

  sp_obs_cluster_atual <- sp_uf_obs_nao_clusters %>% bind_rows( sp_uf_obs_clusters )

  gc()


  # calcular CPAV_INTER_CIRURGICA_SIH_BR_SP

  sp_obs_cluster_atual %<>%
    mutate( CPAV_INTER_CIRURGICA_SIH_BR_SP = if_else( CPAV_INTER_CIRURGICA_SIH_BR == 1 | CURETAGEM_BR_SP == 1 |
                                                         INTERVENCAO_CIRURGICA_ATOPROF == 1, 1L, NA_integer_ ) )


  # criando grupo hipertensão no SIH
  ### hipertensão total (Pré-eclâmpsia grave, Eclâmpsia, Hipertensão grave, Encefalopatia hipertensiva e síndrome HELLP)

  sp_obs_cluster_atual %<>%
    mutate( casos_MMG_hipertensao = if_else( CPAV_PRE_ECLAMP_SIH_BR == 1 | CPAV_ECLAMPSIA_SIH_BR_SP == 1 | CPAV_HIPER_GRAVE_SIH_BR_SP == 1 |
                                           CPAV_ENCEF_HIPER_SIH_BR_SP == 1 | CPAV_HELLP_SIH_BR == 1, 1L, NA_integer_ ) )


  # CPAV COM MUDANÇA NO TMP, INTERVENÇÃO CIRÚRGICA E INCLUSÃO DA HA GESTACIONAL E CRONICA E CRITÉRIO INTUBAÇÃO
  # ( CPAV_DESC_PLAC_SIH = 1 | CPAV_ECTOPICA_SIH = 1 | CPAV_HEMORRAGIA_SIH = 1 | CPAV_ROTURA_SIH = 1 | CPAV_HA_SIHd = 1 |
  #     CPAV_ENDOMETRITE_SIH = 1 | CPAV_EDEMA_PULM_SIH = 1 | CPAV_INSU_RESP_SIH = 1 | CPAV_CONVULSOES_SIH = 1 |
  #     CPAV_SEPSE_SIH = 1 | CPAV_CHOQUE_SIH = 1 | CPAV_CRISE_TIREO_SIH = 1 | CPAV_TRANSFUSAO_SIH = 1 |
  #     CPAV_ACESSO_VENOSO_SIH = 1 | CPAV_HISTERECTOMIA_SIH = 1 | CPAV_UTI_SIH = 1 | CPAV_INTER_7_DIAS_PARTO_SIH = 1 |
  #     CPAV_INTER_CIRURGICA_SIH2 = 1 | CPAV_INTUB = 1 )  CPAV_SIH_TMP_IC_HAd_IENTUB=1.


  sp_obs_cluster_atual %<>%
    mutate( casos_MMG = if_else( CPAV_DESC_PLAC_SIH_BR == 1 | CPAV_ECTOPICA_SIH_BR_SP == 1 | CPAV_HEMORRAGIA_SIH_BR == 1 |
                                   CPAV_ROTURA_SIH_BR_SP == 1 | casos_MMG_hipertensao == 1 | CPAV_ENDOMETRITE_SIH_BR == 1 |
                                   CPAV_EDEMA_PULM_SIH_BR_SP == 1 | CPAV_INSU_RESP_SIH_BR_SP == 1 |
                                   CPAV_CONVULSOES_SIH_BR == 1 | CPAV_SEPSE_SIH_BR == 1 | CPAV_CHOQUE_SIH_BR_SP == 1 |
                                   CPAV_CRISE_TIREO_SIH_BR == 1 | casos_MMG_Transfusao == 1 |
                                   CPAV_ACESSO_VENOSO_SIH_BR_SP == 1 | CPAV_HISTERECTOMIA_SIH_BR_SP == 1 |
                                   casos_MMG_UTI == 1 | casos_MMG_TMP == 1 |
                                   CPAV_INTER_CIRURGICA_SIH_BR_SP == 1 | CPAV_INTUB == 1, 1L, NA_integer_ ) )


  # criando grupo hemorrágica no SIH
  # casos_MMG_Transfusao=1|CPAV_DESC_PLAC_SIH=1|CPAV_ECTOPICA_SIH=1|CPAV_HEMORRAGIA_SIH=1|CPAV_ROTURA_SIH=1
  # casos_MMG_infeccoes (casos que tenham o critério CPAV_ENDOMETRITE_SIH_BR OU CPAV_SEPSE_SIH_BR
  # casos_MMG_cirurgia (casos que tenham o CPAV_HISTERECTOMIA_SIH_BR_SP OU CPAV_INTER_CIRURGICA_SIH_BR_SP)


  sp_obs_cluster_atual %<>%
    mutate( casos_MMG_hemorragia = if_else(casos_MMG_Transfusao == 1 | CPAV_DESC_PLAC_SIH_BR == 1 | CPAV_ECTOPICA_SIH_BR_SP == 1 |
                                              CPAV_HEMORRAGIA_SIH_BR == 1 | CPAV_ROTURA_SIH_BR_SP == 1, 1L, NA_integer_ ),
            casos_MMG_infeccoes = if_else( CPAV_ENDOMETRITE_SIH_BR == 1 | CPAV_SEPSE_SIH_BR == 1, 1L, NA_integer_ ),
            casos_MMG_cirurgia = if_else( CPAV_HISTERECTOMIA_SIH_BR_SP == 1, 1L, NA_integer_ ) )


  # selecionar apenas as variaveis de interesse

  sp_obs_cluster_atual %<>% select( N_AIH, CODMUNRES, DT_INTER, AIHREF, ORDEM, casos_MMG, casos_MMG_hipertensao,
                                             casos_MMG_hemorragia, casos_MMG_infeccoes, casos_MMG_UTI, casos_MMG_TMP, casos_MMG_Transfusao,
                                             casos_MMG_cirurgia )


  # separar em clusters e nao clusters

  rm( list = c( "sp_uf_obs_nao_clusters", "sp_uf_obs_clusters" ) )
  gc()

  sp_uf_obs_clusters <- sp_obs_cluster_atual %>% filter( AIHREF %in% grupos_aih$AIHREF )

  sp_uf_obs_nao_clusters <- sp_obs_cluster_atual %>% filter( ! AIHREF %in% grupos_aih$AIHREF )


  # sumarizar pelo AIHREF

  sp_obs_cluster_grp <- sp_uf_obs_clusters %>% group_by( AIHREF ) %>% arrange( ORDEM, .by_group = TRUE ) %>%
    summarise( DT_INTER = first( DT_INTER, order_by = ORDEM ), CODMUNRES = first( CODMUNRES, order_by = ORDEM ) )

  sp_obs_cluster_grp %<>%
    left_join( sp_uf_obs_clusters %>% group_by( AIHREF ) %>%
                 summarise( across( starts_with( "casos_" ), ~ if_else( any( .x == 1, na.rm = TRUE ), 1L, NA_integer_ ) ) ), by = "AIHREF" )


  # juntar cluster e nao cluster

  sp_obs_cluster_grp %<>%
    bind_rows( sp_uf_obs_nao_clusters %>%
                 select( AIHREF, DT_INTER, CODMUNRES, casos_MMG, casos_MMG_hipertensao, casos_MMG_hemorragia,
                         casos_MMG_infeccoes, casos_MMG_UTI, casos_MMG_TMP, casos_MMG_Transfusao, casos_MMG_cirurgia ) )


  # liberar memoria

  #rm( list = c( glue("sp_obs_cluster_{anos[1]}_{anos[length(anos)]}atual"), "grupos_aih", "sp_uf_obs_nao_clusters", "sp_uf_obs_clusters" ) )
  gc()


  # filtrar apenas os anos de interesse

  sp_obs_cluster_grp %<>% mutate( ANO = year( DT_INTER ) ) %>% filter( between( ANO, anos[1], anos[length(anos)]) )


  # criar sumario dos estados

  obs_cluster_sumario_total_uf %<>%
    bind_rows( sp_obs_cluster_grp %>%
                 summarise( total_internacoes = n(),
                            across( starts_with( "casos_" ), ~ sum( .x, na.rm = TRUE ) ) ) %>%
                 mutate( UF = estado ) )


  # gerar df com todos os municipios e os anos

  obs_cluster_mun_ano <- data.frame( CODMUNRES = rep( unique( sp_obs_cluster_grp$CODMUNRES ), each = length(anos[1]:anos[length(anos)])),
                                 ANO = rep( anos[1]:anos[length(anos)], length( unique( sp_obs_cluster_grp$CODMUNRES ) ) ) )

  obs_cluster_mun_ano %<>% arrange( CODMUNRES, ANO )


  # juntar df com todos os municipios e os anos com df por municipio e ano

  obs_cluster_mun_ano %<>%
    left_join( sp_obs_cluster_grp %>% group_by( CODMUNRES, ANO ) %>%
                 summarise( total_internacoes = n(),
                            casos_MMG = sum( casos_MMG, na.rm = TRUE ),
                            casos_MMG_hipertensao = sum( casos_MMG_hipertensao, na.rm = TRUE ),
                            casos_MMG_hemorragia = sum( casos_MMG_hemorragia, na.rm = TRUE ),
                            casos_MMG_infeccoes = sum( casos_MMG_infeccoes, na.rm = TRUE ),
                            casos_MMG_UTI = sum( casos_MMG_UTI, na.rm = TRUE ),
                            casos_MMG_TMP = sum( casos_MMG_TMP, na.rm = TRUE ),
                            casos_MMG_Transfusao = sum( casos_MMG_Transfusao, na.rm = TRUE ),
                            casos_MMG_cirurgia = sum( casos_MMG_cirurgia, na.rm = TRUE ) ),
               by = c( "CODMUNRES", "ANO" ), na_matches = "never" )


  # substituir NA por 0

  obs_cluster_mun_ano %<>% mutate( across( total_internacoes | starts_with( "casos_" ), ~ if_else( is.na( .x ), 0L, .x ) ) )


  # apendar a base total

  obs_cluster_total_mun_ano %<>% bind_rows( obs_cluster_mun_ano )


  # liberar memoria

  rm( sp_obs_cluster_grp )
  gc()

}


# selecionar CODMUNRES nao nulos

obs_cluster_total_mun_ano %<>% filter( ! is.na( CODMUNRES ) )


# agrupar por CODMUNRES e ANO

obs_cluster_total_mun_ano %<>% group_by( CODMUNRES, ANO ) %>%
  summarise( total_internacoes = sum( total_internacoes, na.rm = TRUE ),
             casos_MMG = sum( casos_MMG, na.rm = TRUE ),
             casos_MMG_hipertensao = sum( casos_MMG_hipertensao, na.rm = TRUE ),
             casos_MMG_hemorragia = sum( casos_MMG_hemorragia, na.rm = TRUE ),
             casos_MMG_infeccoes = sum( casos_MMG_infeccoes, na.rm = TRUE ),
             casos_MMG_UTI = sum( casos_MMG_UTI, na.rm = TRUE ),
             casos_MMG_TMP = sum( casos_MMG_TMP, na.rm = TRUE ),
             casos_MMG_Transfusao = sum( casos_MMG_Transfusao, na.rm = TRUE ),
             casos_MMG_cirurgia = sum( casos_MMG_cirurgia, na.rm = TRUE ) )


# salvar as bases
output_dir <- "data-raw/morbidade/databases/03_bases_finais"
if (!dir.exists(output_dir)) {dir.create(output_dir)}

# Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres)

# Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

df_bloco6_morbidade <- left_join(df_aux_municipios, obs_cluster_total_mun_ano |> janitor::clean_names())

df_bloco6_morbidade[is.na(df_bloco6_morbidade)] <- 0

write.csv(df_bloco6_morbidade, glue("data-raw/csv/indicadores_bloco6_morbidade_materna_2012-2024.csv"), row.names = FALSE)
