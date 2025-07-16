library(microdatasus)
library(data.table)
library(dplyr)
library(janitor)
library(glue)
library(future)
library(future.apply)
library(purrr)

# Criando o planejamento dos futures
plan(multisession)
options(future.rng.onMisuse = "ignore")

# Definindo vetores estáticos -------------------------------------------------
## Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
             "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
             "RS","RO","RR","SC","SP","SE","TO")

## Criando um vetor com os anos considerados (2012 a 2024)
anos <- 2012:2024

## Criando um vetor com os procedimentos de interesse
procedimentos <- c(
  "0201010011","0211040010","0211040061","0310010012","0310010020",
  "0310010039","0310010047","0310010055","0303100010","0303100028",
  "0303100036","0303100044","0303100052","0409060011","0409060054",
  "0409060070","0411010018","0411010026","0411010034","0411010042",
  "0411010050","0411010069","0411010077","0411010085","0411020013",
  "0411020021","0411020030","0411020048","0411020056","0417010028",
  "0417010010","0417010036"
)

## Criando um vetor com as variáveis de diagnóstico de interesse
variaveis_diagnostico <- c(
  "DIAG_PRINC","DIAG_SECUN","DIAGSEC1","DIAGSEC2","DIAGSEC3","DIAGSEC4",
  "DIAGSEC5","DIAGSEC6","DIAGSEC7","DIAGSEC8","DIAGSEC9","CID_MORTE",
  "CID_ASSO","CID_NOTIF"
)

# Definindo o diretório de saída ----------------------------------------------
out_dir <- "data-raw/morbidade/databases/01_sih_rd/01_arquivos_brutos"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Criando uma função para baixar os dados do SIH-RD para cada UF --------------
processa_uf <- function(uf_sigla, anos, out_dir) {

  # Criando uma função interna para baixar os dados para o dado ano e para a dada UF
  baixa_filtra_ano <- function(ano) {
    df <- fetch_datasus(
      year_start = ano,
      year_end = ano,
      month_start = 1,
      month_end = 12,
      uf = uf_sigla,
      information_system = "SIH-RD"
    )

    # Para certos anos, algumas das variáveis de diagnóstico de interesse não existem
    ## Criando as variáveis que não existem e as preenchendo com NA, se for o caso
    df <- as.data.table(df)
    miss_cols <- setdiff(variaveis_diagnostico, names(df))
    if (length(miss_cols) > 0) df[, (miss_cols) := NA_character_]

    # Fazendo as filtragens necessárias, criando marcadores e filtrando apenas as internações obstétricas
    df |>
      # Criando marcadores para identificar uma internação obstétrica
      mutate(
        across(all_of(variaveis_diagnostico), as.character),
        FLAG_PROC_REA  = fifelse(PROC_REA %in% procedimentos, 1L, 0L, na = 0L),
        FLAG_DIAGNOSTICO = fifelse(
          # qualquer código começando em "O"
          pmap_lgl(across(all_of(variaveis_diagnostico)), ~ any(startsWith(c(...), "O"), na.rm = TRUE)),
          1L, 0L
        ),
        criterio_primario = fifelse(FLAG_PROC_REA == 1L | FLAG_DIAGNOSTICO == 1L, 1L, 0L)
      ) |>
      # Filtrando apenas por mulheres (SEXO == 3), com idade de 10 a 49 anos e com IDENT != 5
      filter(
        SEXO == 3,
        COD_IDADE == 4, between(IDADE, 10, 49),
        IDENT != 5,
        criterio_primario == 1L
      ) |>
      as.data.table()   # devolve como data.table para rbindlist
  }

  # Baixando todos os dados de cada ano para a dada uf
  dados_uf <- lapply(anos, baixa_filtra_ano) |> rbindlist(fill = TRUE)

  # Salvando a base completa contendo apenas internações obstétricas para a dada UF
  arq_saida <- glue("{out_dir}/{uf_sigla}_sih_rd_bruto_{min(anos)}_{max(anos)}.csv")
  fwrite(dados_uf, arq_saida, sep = ";", compress = "none")

  # Limpando a memória
  rm(dados_uf); gc()
  message("✅  Concluído UF: ", uf_sigla)
  invisible(TRUE)
}

# Baixando todos os dados -----------------------------------------------------
invisible(
  future_lapply(
    estados,
    function(uf) processa_uf(uf_sigla = uf, anos = anos, out_dir = out_dir)
  )
)



# Referências:

## DIAGNÓSTICO:

# Todos os diagnósticos do grupo XV - causas obstétricas (Grupo "O" = O00 a O99)

## Procedimentos:

# 0201010011	Amniocentese
# 0211040010	Amnioscopia
# 0211040061	Tococardiografia anteparto
# 0310010012	Assistência ao parto sem distâcia
# 0310010020	Atendimento ao recém-nascido no momento do nascimento
# 0310010039	Parto normal
# 0310010047	Parto normal em gestação de alto risco
# 0310010055	Parto normal em centro de parto normal (CPN)
# 0303100010	Tratamento de complicações relacionadas predominantemente ao puerpério
# 0303100028	Tratamento de eclâmpsia
# 0303100036	Tratamento de edema, proteinúria e transtornos hipertensivos na gravidez, parto e puerpério.
# 0303100044	Tratamento de intercorrências clínicas na gravidez
# 0303100052	Tratamento de mola hidatiforme
# 0409060011 	Cerclagem de colo do útero
# 0409060054	Curetagem uterina em mola hidatiforme
# 0409060070	Esvaziamento de útero pós-aborto por aspiração manual intra-uterina (AMIU)
# 0411010018	Descolamento manual de placenta
# 0411010026	Parto cesariano em gestação de alto risco
# 0411010034	Parto cesariano
# 0411010042	Parto cesariano com laqueadura tubária
# 0411010050	Redução manual de inversão uterina aguda pós-parto
# 0411010069	Ressutura de episiorrafia pós-parto
# 0411010077	Sutura de lacerações de trajeto pélvico (no parto antes da admissão)
# 0411010085	Tratamento cirúrgico de inversão uterina aguda pós-parto
# 0411020013	Curetagem pós-abortamento/puerperal
# 0411020021	Embriotomia
# 0411020030	Histerectomia puerperal
# 0411020048	Tratamento cirúrgico de gravidez ectópica
# 0411020056	Tratamento de outros transtornos maternos relacionados predominantemente à gravidez
# 0417010028	Analgesia Obstétrica p/ Parto Normal
# 0417010010	Anestesia Obstétrica p/ Cesariana
# 0417010036	Anestesia Obstétrica p/ Cesariana em Gestação de Alto Risco
