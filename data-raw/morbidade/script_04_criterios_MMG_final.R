library( tidyverse )
library( magrittr )
library( lubridate )
library( openxlsx )
library( foreign )
library(data.table)
library(glue)


# Diretórios dos arquivos

arq_obs_cluster <- "databases"
arq_SP <- "databases"

# Listas de códigos CID e procedimentos

desc_plac <- c("O450", "O458", "O459")  # Descolamento prematuro de placenta
ectopica <- c("O000", "O001", "O002", "O008", "O009")  # Gravidez ectópica
hemorragia <- c("O720", "O721", "O722")  # Hemorragia pós-parto
rotura <- c("O710", "O711")  # Rotura uterina
pre_eclamp <- "O141"  # Pré-eclâmpsia grave
eclampsia <- c("O150", "O151", "O152", "O159")  # Eclâmpsia
hiper_grave_cid <- c("O11", "O13", "O149", "O16", "O100", "O101", "O102", "O103", "O104", "O109", "I10", "I11", "I12", "I13", "I15")  # Hipertensão grave (CID)
hiper_grave_proc <- c("303060107", "303100036")  # Hipertensão grave (Procedimento)
encef_hiper <- "I674"  # Encefalopatia hipertensiva
hellp <- "O142"  # Síndrome HELLP
endometrite <- "O85"  # Endometrite
edema_pulm <- "J81"  # Edema pulmonar
insu_resp <- c("J80", "J960", "J969", "R092", "I26", "I260", "I269", "O032", "O037", "O042", "O047", "O052", "O057", "O062", "O067", "O072", "O077", "O082", "O880", "O881", "O882", "O883", "O888")  # Insuficiência respiratória
convulsao <- "R568"  # Convulsões
sepse <- c("A41", "A410", "A411", "A412", "A413", "A414", "A415", "A418", "A419", "A40", "A400", "A401", "A402", "A403", "A408", "A409", "A021", "A227", "A267", "A327", "A427", "B377", "O080", "O753", "R572", "O85")  # Sepse
choque_cid <- c("E86", "R570", "R571", "R578", "R579", "T811", "I460", "O083", "O751")  # Choque (CID)
choque_proc <- c("303060077", "303060069", "303060050", "303060255")  # Choque (Procedimento)
crise_tireo <- "E055"  # Crise tireotóxica
transfusao_cid <- "Z513"  # Transfusão sanguínea (CID)
transfusao_proc <- c("306020068", "306020076", "306020084", "306020092", "306020106", "306020114", "306020122", "306020130", "306020149")  # Transfusão sanguínea (Procedimento)
acesso_venoso <- c("702050091", "702050814", "309060010", "309060036")  # Acesso venoso central (Procedimento)
histerectomia_cid <- "O822"  # Histerectomia (CID)
histerectomia_proc <- c("411020030", "409060100", "409060119", "409060127", "409060135")  # Histerectomia (Procedimento)
UTI <- c("802010105", "802010083", "802010091", "802010296", "802010318")  # Admissão à unidade de tratamento intensivo (Procedimento)
cpav_intubacao <- c("O290", "O291", "O292", "O293", "O295", "O296", "O298", "O299", "O740", "O741", "O742", "O743", "O744", "O746", "O747", "O748", "O749", "O890", "O891", "O892", "O893", "O895", "O896", "O898", "O899")  # CPAP e intubação (CID)
parto_proc <- c("310010012", "310010039", "310010047", "310010055", "411010026", "411010034", "411010042")  # Procedimentos de parto (Procedimento
intercorrencias_puerp <- c( "O700",	"O70", "O701", "O702", "O703", "O709", "O710", "O711", "O712", "O713", "O714",
                            "O715",	"O716",	"O717",	"O718",	"O719",	"O720",	"O721",	"O722",	"O723",	"O730",	"O731",
                            "O85", "O850", "O860", "O861", "O862", "O863", "O864", "O868", "O870", "O871", "O872",
                            "O873", "O878", "O879", "O880",	"O881",	"O882",	"O883",	"O888",	"O890",	"O891",	"O892",
                            "O893",	"O894",	"O895",	"O896",	"O898",	"O899", "O900",	"O901",	"O902",	"O903",	"O904",
                            "O905",	"O908",	"O909",	"O910",	"O911",	"O912",	"O920",	"O921",	"O922",	"O923",	"O924",
                            "O925",	"O926",	"O927", "O94" ) # motivo da internação
curetagem <- c("409060070", "411020013")  # Curetagem (Procedimento)
proc_cirurgicos <- c("415010012", "415020034", "407040161", "407040250", "407040030", "407040021", "407040013",
                     "411010085", "411010050", "407040200", "407040242", "415040027", "411010077", "401010031",
                     "401010104") # Intervenção cirúrgica


# Estados
estados <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI",
             "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

# Tabelas totais
obs_cluster_total_mun_ano <- tibble()
obs_cluster_sumario_total_uf <- tibble()


# ler as bases para cada estado

for( estado in estados ) {
  # Ler o arquivo CSV relacionado ao SIH para um estado específico e período específico
  arquivo <- glue("{arq_obs_cluster}/{estado}_sih_rd_tratado_long_2022_2024.csv")
  sp_obs_cluster_2012_atual <- fread(arquivo, sep = ",") |>
    mutate(PROC_REA = as.character(PROC_REA))

  # Selecionar apenas as variáveis de interesse do conjunto de dados recém-carregado
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    select(
      N_AIH,
      CODMUNRES = MUNIC_RES,
      DT_INTER,
      DT_SAIDA,
      starts_with("DIAG"),
      starts_with("CID_"),
      UTI_MES_TO,
      COBRANCA,
      PROC_REA,
      AIHREF
    )

  # Converter a variável UTI_MES_TO para tipo inteiro (integer)
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    mutate(UTI_MES_TO = as.integer(UTI_MES_TO))

  # Limpar a memória utilizando o coletor de lixo
  gc()


  # calculo das variaveis de interesse

  # Calcula as variáveis de interesse com base nos diagnósticos (DIAG) e procedimentos (PROC_REA)
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>

    # Cria uma nova variável indicando a presença de descolamento prematuro de placenta (CPAV_DESC_PLAC_SIH_BR)
    mutate(
      CPAV_DESC_PLAC_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% desc_plac),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de gravidez ectópica (CPAV_ECTOPICA_SIH_BR)
      CPAV_ECTOPICA_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% ectopica) |
          PROC_REA == "411020048",
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de hemorragia pós-parto (CPAV_HEMORRAGIA_SIH_BR)
      CPAV_HEMORRAGIA_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% hemorragia),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de rotura uterina (CPAV_ROTURA_SIH_BR)
      CPAV_ROTURA_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% rotura) |
          PROC_REA == "409060160",
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de pré-eclâmpsia grave (CPAV_PRE_ECLAMP_SIH_BR)
      CPAV_PRE_ECLAMP_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == pre_eclamp),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de eclâmpsia (CPAV_ECLAMPSIA_SIH_BR)
      CPAV_ECLAMPSIA_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% eclampsia) |
          PROC_REA == "303100028",
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de hipertensão grave (CPAV_HIPER_GRAVE_SIH_BR)
      CPAV_HIPER_GRAVE_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% hiper_grave_cid) |
          PROC_REA %in% hiper_grave_proc,
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de encefalopatia hipertensiva (CPAV_ENCEF_HIPER_SIH_BR)
      CPAV_ENCEF_HIPER_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == encef_hiper) |
          PROC_REA == "303040211",
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença da síndrome HELLP (CPAV_HELLP_SIH_BR)
      CPAV_HELLP_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == hellp),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de endometrite (CPAV_ENDOMETRITE_SIH_BR)
      CPAV_ENDOMETRITE_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == endometrite),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de edema pulmonar (CPAV_EDEMA_PULM_SIH_BR)
      CPAV_EDEMA_PULM_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == edema_pulm) |
          PROC_REA == "303060131",
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de insuficiência respiratória (CPAV_INSU_RESP_SIH_BR)
      CPAV_INSU_RESP_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% insu_resp) |
          PROC_REA == "303060140",
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de convulsões (CPAV_CONVULSOES_SIH_BR)
      CPAV_CONVULSOES_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == convulsao),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de sepse (CPAV_SEPSE_SIH_BR)
      CPAV_SEPSE_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% sepse),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de choque (CPAV_CHOQUE_SIH_BR)
      CPAV_CHOQUE_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% choque_cid) |
          PROC_REA %in% choque_proc,
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de crise tireotóxica (CPAV_CRISE_TIREO_SIH_BR)
      CPAV_CRISE_TIREO_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == crise_tireo),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de casos de transfusão sanguínea (casos_MMG_Transfusao_BR)
      casos_MMG_Transfusao_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == transfusao_cid) |
          PROC_REA %in% transfusao_proc,
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de acesso venoso central (CPAV_ACESSO_VENOSO_SIH_BR)
      CPAV_ACESSO_VENOSO_SIH_BR = if_else(
        PROC_REA %in% acesso_venoso,
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de histerectomia (CPAV_HISTERECTOMIA_SIH_BR)
      CPAV_HISTERECTOMIA_SIH_BR = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") == histerectomia_cid) |
          PROC_REA %in% histerectomia_proc,
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de admissão à unidade de tratamento intensivo (casos_MMG_UTI_BR)
      casos_MMG_UTI_BR = if_else(
        PROC_REA %in% UTI | UTI_MES_TO > 0,
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de intubação (CPAV_INTUB)
      CPAV_INTUB = if_else(
        if_any(starts_with("DIAG") | starts_with("CID_"), ~ str_replace_na(.x, "") %in% cpav_intubacao),
        1L,
        NA_integer_
      ),

      # Cria uma nova variável indicando a presença de procedimentos de parto (PARTO_PROC_BR)
      PARTO_PROC_BR = if_else(
        PROC_REA %in% parto_proc,
        1L,
        NA_integer_
      )
    )


  # Selecionar os clusters
  grupos_aih <- sp_obs_cluster_2012_atual |>
    group_by(AIHREF) |>
    summarise(freq = n()) |>
    filter(freq > 1)

  # Separar em clusters e não clusters
  sp_uf_obs_clusters <- semi_join(sp_obs_cluster_2012_atual, grupos_aih, by = "AIHREF")
  sp_uf_obs_nao_clusters <- anti_join(sp_obs_cluster_2012_atual, grupos_aih, by = "AIHREF")



  # Calcular tempo de permanência e casos_MMG_TMP_BR (incluindo internações com tempo de internação > 7 dias)
  # Filtrando partos ou complicações no puerpério e procedimento de curetagem para não clusters
  sp_uf_obs_nao_clusters <- sp_uf_obs_nao_clusters |>
    mutate(
      DT_INTER = ymd(DT_INTER),
      DT_SAIDA = ymd(DT_SAIDA),
      ORDEM = 1,
      tempo_de_permanencia = as.numeric(DT_SAIDA - DT_INTER, "days"),
      INTERCORRENCIA_PUERP = if_else(PROC_REA == "303100010" | DIAG_PRINC %in% intercorrencias_puerp, 1L, NA_integer_),
      casos_MMG_TMP_BR = if_else(PARTO_PROC_BR == 1 & tempo_de_permanencia > 7 & PROC_REA != "802010024", 1L, NA_integer_),
      CURETAGEM = if_else(INTERCORRENCIA_PUERP == 1 & PROC_REA %in% curetagem, 1L, NA_integer_)
    )

  # Calcular tempo de permanência para clusters e filtrar partos ou complicações no puerpério e procedimento de curetagem
  sp_uf_obs_clusters <- sp_uf_obs_clusters |>
    mutate(
      DT_INTER = ymd(DT_INTER),
      DT_SAIDA = ymd(DT_SAIDA)
    ) |>
    group_by(AIHREF) |>
    arrange(DT_INTER, DT_SAIDA, N_AIH, .by_group = TRUE) |>
    mutate(
      ORDEM = row_number(),
      prim_entrada = min(DT_INTER),
      ult_saida = max(DT_SAIDA),
      tempo_de_permanencia = if_else(ORDEM == 1, time_length(interval(prim_entrada, ult_saida), "days"), NA_integer_)
    ) |>
    ungroup() |>
    select(-prim_entrada, -ult_saida)

  # Identificar intercorrências no puerpério para clusters
  sp_uf_obs_clusters <- sp_uf_obs_clusters |>
    mutate(
      INTERCORRENCIA_PUERP = if_else(ORDEM == 1 & (PROC_REA == "303100010" | DIAG_PRINC %in% intercorrencias_puerp), 1L, NA_integer_)
    )

  # Identificar casos de MMG_TMP_BR e curetagem para clusters
  sp_uf_obs_clusters <- sp_uf_obs_clusters |>
    group_by(AIHREF) |>
    mutate(
      casos_MMG_TMP_BR = if_else(
        ORDEM == 1 & PARTO_PROC_BR == 1 & tempo_de_permanencia > 7 & !PROC_REA %in% "802010024", 1L, NA_integer_
      ),
      CURETAGEM = if_else(
        any(PARTO_PROC_BR == 1 | INTERCORRENCIA_PUERP == 1) & any(PROC_REA %in% curetagem), 1L, NA_integer_
      )
    ) |>
    ungroup()


  # Juntar clusters e não clusters
  rm(sp_obs_cluster_2012_atual)
  sp_obs_cluster_2012_atual <- bind_rows(sp_uf_obs_nao_clusters, sp_uf_obs_clusters)

  # Limpar memória
  gc()

  # Calcular CPAV_INTER_CIRURGICA_SIH_BR
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    mutate(CPAV_INTER_CIRURGICA_SIH_BR = if_else(CURETAGEM == 1 | PROC_REA %in% proc_cirurgicos, 1L, NA_integer_))

  # Ler base SP
  base_sp_redu <- fread(glue("{arq_SP}/{estado}_sih_sp_filtrado_2022_2024.csv.gz"), sep = ",") |>
    mutate(SP_ATOPROF = as.character(SP_ATOPROF))

  # Limpar memória
  gc()


  # Calcular variáveis relacionadas aos procedimentos na base SP
  base_sp_redu <- base_sp_redu |>
    mutate(
      ACOMPANHA_BB_AP = case_when(SP_ATOPROF == "802010024" ~ 1L,
                                  TRUE ~ NA_integer_),
      ECTOPICA_ATOPROF = case_when(SP_ATOPROF == "411020048" ~ 1L,
                                   TRUE ~ NA_integer_),
      ROTURA_UTERINA_ATOPROF = case_when(SP_ATOPROF == "409060160" ~ 1L,
                                         TRUE ~ NA_integer_),
      DIAG_ECLAMPSIA_ATOPROF = case_when(SP_ATOPROF == "303100028" ~ 1L,
                                         TRUE ~ NA_integer_),
      HIPER_GRAVE_ATOPROF = case_when(SP_ATOPROF %in% sort(hiper_grave_proc) ~ 1L,
                                      TRUE ~ NA_integer_),
      ENCEFALOPATIA_HIPER_ATOPROF = case_when(SP_ATOPROF == "303040211" ~ 1L,
                                              TRUE ~ NA_integer_),
      EDEMA_PULMONAR_ATOPROF = case_when(SP_ATOPROF == "303060131" ~ 1L,
                                         TRUE ~ NA_integer_),
      INSUFICIENCIA_RESPIRATORIA_ATOPROF = case_when(SP_ATOPROF == "303060140" ~ 1L,
                                                     TRUE ~ NA_integer_),
      CHOQUE_ATOPROF = case_when(SP_ATOPROF %in% sort(choque_proc) ~ 1L,
                                 TRUE ~ NA_integer_),
      TRANSFUSAO_ATOPROF = case_when(SP_ATOPROF %in% sort(transfusao_proc) ~ 1L,
                                     TRUE ~ NA_integer_),
      CATETER_VENOSO_ATOPROF = case_when(SP_ATOPROF %in% sort(acesso_venoso) ~ 1L,
                                         TRUE ~ NA_integer_),
      HISTERECTOMIA_ATOPROF = case_when(SP_ATOPROF %in% sort(histerectomia_proc) ~ 1L,
                                        TRUE ~ NA_integer_),
      INTER_UTI_ATOPROF = case_when(SP_ATOPROF %in% sort(UTI) ~ 1L,
                                    TRUE ~ NA_integer_),
      INTERVENCAO_CIRURGICA_ATOPROF = case_when(SP_ATOPROF %in% sort(proc_cirurgicos) ~ 1L,
                                                TRUE ~ NA_integer_),
      CURETAGEM_ATOPROF = case_when(SP_ATOPROF %in% sort(curetagem) ~ 1L,
                                    TRUE ~ NA_integer_)
    )



  # Reduzir a base SP
  base_sp_grp <- base_sp_redu |>
    group_by(SP_NAIH) |>
    summarise(
      across(starts_with(c("ACOMPANHA_BB_AP", "ECTOPICA_ATOPROF", "ROTURA_UTERINA_ATOPROF", "DIAG_ECLAMPSIA_ATOPROF",
                           "HIPER_GRAVE_ATOPROF", "ENCEFALOPATIA_HIPER_ATOPROF", "EDEMA_PULMONAR_ATOPROF",
                           "INSUFICIENCIA_RESPIRATORIA_ATOPROF", "CHOQUE_ATOPROF", "TRANSFUSAO_ATOPROF",
                           "CATETER_VENOSO_ATOPROF", "HISTERECTOMIA_ATOPROF", "INTER_UTI_ATOPROF",
                           "INTERVENCAO_CIRURGICA_ATOPROF", "CURETAGEM_ATOPROF")),
             ~if_else(any(.x == 1, na.rm = TRUE), 1L, NA_integer_))
    )

  # Convert SP_NAIH to character type in base_sp_grp
  base_sp_grp <- base_sp_grp |>
    mutate(SP_NAIH = as.character(SP_NAIH))

  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    mutate(N_AIH = as.character(N_AIH))

  # Join sp_obs_cluster_2012_atual with base_sp_grp
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    left_join(base_sp_grp, by = c("N_AIH" = "SP_NAIH"))

  # Criar variáveis BR_SP
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    mutate(
      CPAV_ECTOPICA_SIH_BR_SP = if_else(CPAV_ECTOPICA_SIH_BR == 1 | ECTOPICA_ATOPROF == 1, 1L, NA_integer_),
      CPAV_ROTURA_SIH_BR_SP = if_else(CPAV_ROTURA_SIH_BR == 1 | ROTURA_UTERINA_ATOPROF == 1, 1L, NA_integer_),
      CPAV_ECLAMPSIA_SIH_BR_SP = if_else(CPAV_ECLAMPSIA_SIH_BR == 1 | DIAG_ECLAMPSIA_ATOPROF == 1, 1L, NA_integer_),
      CPAV_HIPER_GRAVE_SIH_BR_SP = if_else(CPAV_HIPER_GRAVE_SIH_BR == 1 | HIPER_GRAVE_ATOPROF == 1, 1L, NA_integer_),
      CPAV_ENCEF_HIPER_SIH_BR_SP = if_else(CPAV_ENCEF_HIPER_SIH_BR == 1 | ENCEFALOPATIA_HIPER_ATOPROF == 1, 1L, NA_integer_),
      CPAV_EDEMA_PULM_SIH_BR_SP = if_else(CPAV_EDEMA_PULM_SIH_BR == 1 | EDEMA_PULMONAR_ATOPROF == 1, 1L, NA_integer_),
      CPAV_INSU_RESP_SIH_BR_SP = if_else(CPAV_INSU_RESP_SIH_BR == 1 | INSUFICIENCIA_RESPIRATORIA_ATOPROF == 1, 1L, NA_integer_),
      CPAV_CHOQUE_SIH_BR_SP = if_else(CPAV_CHOQUE_SIH_BR == 1 | CHOQUE_ATOPROF == 1, 1L, NA_integer_),
      casos_MMG_Transfusao = if_else(casos_MMG_Transfusao_BR == 1 | TRANSFUSAO_ATOPROF == 1, 1L, NA_integer_),
      CPAV_ACESSO_VENOSO_SIH_BR_SP = if_else(CPAV_ACESSO_VENOSO_SIH_BR == 1 | CATETER_VENOSO_ATOPROF == 1, 1L, NA_integer_),
      CPAV_HISTERECTOMIA_SIH_BR_SP = if_else(CPAV_HISTERECTOMIA_SIH_BR == 1 | HISTERECTOMIA_ATOPROF == 1, 1L, NA_integer_),
      casos_MMG_UTI = if_else(casos_MMG_UTI_BR == 1 | INTER_UTI_ATOPROF == 1, 1L, NA_integer_)
    )


  # Remover objetos desnecessários da memória
  rm(list = c("base_sp_grp", "base_sp_redu", "sp_uf_obs_clusters", "sp_uf_obs_nao_clusters"))

  # Limpar a memória
  gc()

  # Separar em clusters e não clusters
  sp_uf_obs_clusters <- filter(sp_obs_cluster_2012_atual, AIHREF %in% grupos_aih$AIHREF)
  sp_uf_obs_nao_clusters <- filter(sp_obs_cluster_2012_atual, !AIHREF %in% grupos_aih$AIHREF)

  # Calcular variáveis casos_MMG_TMP_BR_SP e CURETAGEM_BR_SP para não clusters
  sp_uf_obs_nao_clusters <- sp_uf_obs_nao_clusters |>
    mutate(
      casos_MMG_TMP = if_else(casos_MMG_TMP_BR == 1 & is.na(ACOMPANHA_BB_AP), 1L, NA_integer_),
      CURETAGEM_BR_SP = if_else(CURETAGEM_ATOPROF == 1 & INTERCORRENCIA_PUERP == 1, 1L, NA_integer_)
    )

  # Calcular variáveis casos_MMG_TMP_BR_SP e CURETAGEM_BR_SP para clusters
  sp_uf_obs_clusters <- sp_uf_obs_clusters |>
    group_by(AIHREF) |>
    mutate(
      casos_MMG_TMP = if_else(casos_MMG_TMP_BR == 1 & all(is.na(ACOMPANHA_BB_AP), na.rm = TRUE), 1L, NA_integer_),
      CURETAGEM_BR_SP = if_else(any(CURETAGEM_ATOPROF == 1, na.rm = TRUE) & any(INTERCORRENCIA_PUERP == 1 | PARTO_PROC_BR == 1, na.rm = TRUE), 1L, NA_integer_)
    ) |>
    ungroup()

  # Juntar as bases
  rm(sp_obs_cluster_2012_atual)
  sp_obs_cluster_2012_atual <- bind_rows(sp_uf_obs_nao_clusters, sp_uf_obs_clusters)

  # Limpar a memória
  gc()


  # Calcular CPAV_INTER_CIRURGICA_SIH_BR_SP
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    mutate(
      CPAV_INTER_CIRURGICA_SIH_BR_SP = if_else(
        CPAV_INTER_CIRURGICA_SIH_BR == 1 | CURETAGEM_BR_SP == 1 | INTERVENCAO_CIRURGICA_ATOPROF == 1,
        1L, NA_integer_
      )
    )

  # Criar variáveis intermediárias para os casos MMG
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    mutate(
      casos_MMG_hipertensao = if_else(
        CPAV_PRE_ECLAMP_SIH_BR == 1 | CPAV_ECLAMPSIA_SIH_BR_SP == 1 | CPAV_HIPER_GRAVE_SIH_BR_SP == 1 |
          CPAV_ENCEF_HIPER_SIH_BR_SP == 1 | CPAV_HELLP_SIH_BR == 1, 1L, NA_integer_
      ),
      casos_MMG_hemorragia = if_else(
        CPAV_DESC_PLAC_SIH_BR == 1 | CPAV_ECTOPICA_SIH_BR_SP == 1 | CPAV_HEMORRAGIA_SIH_BR == 1 |
          CPAV_ROTURA_SIH_BR_SP == 1, 1L, NA_integer_
      ),
      casos_MMG_infeccoes = if_else(
        CPAV_ENDOMETRITE_SIH_BR == 1 | CPAV_SEPSE_SIH_BR == 1, 1L, NA_integer_
      ),
      casos_MMG_cirurgia = if_else(
        CPAV_HISTERECTOMIA_SIH_BR_SP == 1 | CPAV_INTER_CIRURGICA_SIH_BR_SP == 1, 1L, NA_integer_
      )
    )

  # Calcular casos MMG
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    mutate(
      casos_MMG = if_else(
        CPAV_DESC_PLAC_SIH_BR == 1 | CPAV_ECTOPICA_SIH_BR_SP == 1 | CPAV_HEMORRAGIA_SIH_BR == 1 |
          CPAV_ROTURA_SIH_BR_SP == 1 | casos_MMG_hipertensao == 1 | CPAV_ENDOMETRITE_SIH_BR == 1 |
          CPAV_EDEMA_PULM_SIH_BR_SP == 1 | CPAV_INSU_RESP_SIH_BR_SP == 1 | CPAV_CONVULSOES_SIH_BR == 1 |
          CPAV_SEPSE_SIH_BR == 1 | CPAV_CHOQUE_SIH_BR_SP == 1 | CPAV_CRISE_TIREO_SIH_BR == 1 |
          casos_MMG_Transfusao == 1 | CPAV_ACESSO_VENOSO_SIH_BR_SP == 1 | CPAV_HISTERECTOMIA_SIH_BR_SP == 1 |
          casos_MMG_UTI == 1 | casos_MMG_TMP == 1 | CPAV_INTER_CIRURGICA_SIH_BR_SP == 1 | CPAV_INTUB == 1,
        1L, NA_integer_
      )
    )

  # Selecionar apenas as variáveis de interesse
  sp_obs_cluster_2012_atual <- sp_obs_cluster_2012_atual |>
    select(
      N_AIH, CODMUNRES, DT_INTER, AIHREF, ORDEM, casos_MMG, casos_MMG_hipertensao, casos_MMG_hemorragia,
      casos_MMG_infeccoes, casos_MMG_UTI, casos_MMG_TMP, casos_MMG_Transfusao, casos_MMG_cirurgia
    )

  # Separar em clusters e não clusters
  rm(list = c("sp_uf_obs_nao_clusters", "sp_uf_obs_clusters"))
  gc()

  sp_uf_obs_clusters <- sp_obs_cluster_2012_atual |> filter(AIHREF %in% grupos_aih$AIHREF)

  sp_uf_obs_nao_clusters <- sp_obs_cluster_2012_atual |> anti_join(sp_uf_obs_clusters, by = "AIHREF")


  # Sumarizar pelo AIHREF
  sp_obs_cluster_2014_2019_grp <- sp_uf_obs_clusters |>
    group_by(AIHREF) |>
    arrange(ORDEM, .by_group = TRUE) |>
    summarise(
      DT_INTER = first(DT_INTER, order_by = ORDEM),
      CODMUNRES = first(CODMUNRES, order_by = ORDEM),
      across(starts_with("casos_"), ~ if_else(any(.x == 1, na.rm = TRUE), 1L, NA_integer_))
    )

  # Juntar cluster e nao cluster
  sp_obs_cluster_2014_2019_grp <- sp_obs_cluster_2014_2019_grp |>
    bind_rows(
      sp_uf_obs_nao_clusters |>
        select(
          AIHREF, DT_INTER, CODMUNRES, casos_MMG, casos_MMG_hipertensao, casos_MMG_hemorragia,
          casos_MMG_infeccoes, casos_MMG_UTI, casos_MMG_TMP, casos_MMG_Transfusao, casos_MMG_cirurgia
        )
    )

  # Liberar memória
  rm(list = c("sp_obs_cluster_2012_atual", "grupos_aih", "sp_uf_obs_nao_clusters", "sp_uf_obs_clusters"))
  gc()



  # Filtrar apenas os anos de interesse
  sp_obs_cluster_2014_2019_grp <- sp_obs_cluster_2014_2019_grp |>
    mutate(ANO = year(DT_INTER)) |>
    filter(between(ANO, 2012, 2023))

  # Criar sumário dos estados
  obs_cluster_sumario_total_uf <- obs_cluster_sumario_total_uf |>
    bind_rows(
      sp_obs_cluster_2014_2019_grp |>
        summarise(
          total_internacoes = n(),
          across(starts_with("casos_"), ~sum(., na.rm = TRUE))
        ) |>
        mutate(UF = estado)
    )

  # Gerar dataframe com todos os municípios e os anos
  obs_cluster_mun_ano <- tibble(
    CODMUNRES = rep(unique(sp_obs_cluster_2014_2019_grp$CODMUNRES), each = length(2012:2023)),
    ANO = rep(2012:2023, length(unique(sp_obs_cluster_2014_2019_grp$CODMUNRES)))
  )

  obs_cluster_mun_ano <- obs_cluster_mun_ano |> arrange(CODMUNRES, ANO)


  # Juntar df com todos os municípios e os anos com df por município e ano
  obs_cluster_mun_ano <- obs_cluster_mun_ano |>
    left_join(
      sp_obs_cluster_2014_2019_grp |>
        group_by(CODMUNRES, ANO) |>
        summarise(
          total_internacoes = n(),
          casos_MMG = sum(casos_MMG, na.rm = TRUE),
          casos_MMG_hipertensao = sum(casos_MMG_hipertensao, na.rm = TRUE),
          casos_MMG_hemorragia = sum(casos_MMG_hemorragia, na.rm = TRUE),
          casos_MMG_infeccoes = sum(casos_MMG_infeccoes, na.rm = TRUE),
          casos_MMG_UTI = sum(casos_MMG_UTI, na.rm = TRUE),
          casos_MMG_TMP = sum(casos_MMG_TMP, na.rm = TRUE),
          casos_MMG_Transfusao = sum(casos_MMG_Transfusao, na.rm = TRUE),
          casos_MMG_cirurgia = sum(casos_MMG_cirurgia, na.rm = TRUE)
        ),
      by = c("CODMUNRES", "ANO"),
      na_matches = "never"
    )

  # Substituir NA por 0
  obs_cluster_mun_ano <- obs_cluster_mun_ano |>
    mutate(across(total_internacoes | starts_with("casos_"), ~if_else(is.na(.x), 0L, .x)))

  # Adicionar à base total
  obs_cluster_total_mun_ano <- bind_rows(obs_cluster_total_mun_ano, obs_cluster_mun_ano)

  # Liberar memória
  rm(sp_obs_cluster_2014_2019_grp)
  gc()

}


# Selecionar CODMUNRES não nulos
obs_cluster_total_mun_ano <- obs_cluster_total_mun_ano |>
  filter(!is.na(CODMUNRES))

# Agrupar por CODMUNRES e ANO
obs_cluster_total_mun_ano <- obs_cluster_total_mun_ano |>
  group_by(CODMUNRES, ANO) |>
  summarise(
    total_internacoes = sum(total_internacoes, na.rm = TRUE),
    casos_MMG = sum(casos_MMG, na.rm = TRUE),
    casos_MMG_hipertensao = sum(casos_MMG_hipertensao, na.rm = TRUE),
    casos_MMG_hemorragia = sum(casos_MMG_hemorragia, na.rm = TRUE),
    casos_MMG_infeccoes = sum(casos_MMG_infeccoes, na.rm = TRUE),
    casos_MMG_UTI = sum(casos_MMG_UTI, na.rm = TRUE),
    casos_MMG_TMP = sum(casos_MMG_TMP, na.rm = TRUE),
    casos_MMG_Transfusao = sum(casos_MMG_Transfusao, na.rm = TRUE),
    casos_MMG_cirurgia = sum(casos_MMG_cirurgia, na.rm = TRUE)
  )

# salvar as bases
write.csv(obs_cluster_total_mun_ano, "databases/03_bases_finais/obs_cluster_total_mun_ano_2022_2024.csv", row.names = FALSE)

obs_cluster_total_mun_ano_antigo <- read_csv("data-raw/csv/indicadores_bloco6_morbidade_materna_2012-2023.csv") |>
  filter(ano == 2021)

obs_cluster_total_mun_ano_novo <- rbind(obs_cluster_total_mun_ano_antigo, obs_cluster_total_mun_ano)

write.csv(obs_cluster_total_mun_ano_novo, "databases/03_bases_finais/indicadores_bloco6_morbidade_materna_2012-2024.csv", row.names = FALSE)


write.csv( obs_cluster_sumario_total_uf, "databases/03_bases_finais/obs_cluster_sumario_total_2022_2024.csv", row.names = FALSE)


