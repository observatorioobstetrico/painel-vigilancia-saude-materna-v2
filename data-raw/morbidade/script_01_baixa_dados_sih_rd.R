library(microdatasus)
library(dplyr)
library(janitor)
library(glue)

# Criando um vetor com as siglas de todos os estados do Brasil
estados <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
             "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Criando um vetor com os anos considerados (2012 a 2024)
anos <- c(2024)

options(timeout = 999999999)

# Criando um vetor com os procedimentos de interesse
procedimentos <- c(
  "0201010011", "0211040010", "0211040061", "0310010012", "0310010020",
  "0310010039", "0310010047", "0310010055", "0303100010", "0303100028",
  "0303100036", "0303100044", "0303100052", "0409060011", "0409060054",
  "0409060070", "0411010018", "0411010026", "0411010034", "0411010042",
  "0411010050", "0411010069", "0411010077", "0411010085", "0411020013",
  "0411020021", "0411020030", "0411020048", "0411020056", "0417010028",
  "0417010010", "0417010036"
)

# Criando um vetor com as variáveis de diagnóstico de interesse
variaveis_diagnostico <- c(
  "DIAG_PRINC", "DIAG_SECUN", "DIAGSEC1", "DIAGSEC2",
  "DIAGSEC3", "DIAGSEC4", "DIAGSEC5", "DIAGSEC6",
  "DIAGSEC7", "DIAGSEC8", "DIAGSEC9", "CID_MORTE",
  "CID_ASSO", "CID_NOTIF"
)

# Baixando os dados do SIH-RD para cada UF
for (estado in estados) {
  # Criando um data.frame que guardará a base final de cada UF
  df_sih_rd_uf <- data.frame()

  for (ano in anos) {
    # Baixando os dados para o dado ano e para a dada UF
    df_sih_rd <- fetch_datasus(
      year_start = ano,
      year_end = ano,
      month_start = 1,
      month_end = 12,
      uf = estado,
      information_system = "SIH-RD"
    )

    # Para certos anos, algumas das variáveis de diagnóstico de interesse não existem
    ## Criando as variáveis que não existem e as preenchendo com NA, se for o caso
    df_sih_rd[setdiff(variaveis_diagnostico, names(df_sih_rd))] <- NA

    # Fazendo as filtragens necessárias, criando marcadores e filtrando apenas as internações obstétricas
    df_sih_rd_uf_aux <- df_sih_rd |>
      # Filtrando apenas por mulheres (SEXO == 3), com idade de 10 a 49 anos e com IDENT != 5
      filter(
        SEXO == 3,
        COD_IDADE == 4 & IDADE >= 10 & IDADE <= 49,
        IDENT != 5
      ) |>
      # Criando marcadores para identificar uma internação obstétrica
      mutate(
        across(all_of(variaveis_diagnostico), ~ as.character(.)),
        FLAG_PROC_REA = if_else(
          PROC_REA %in% procedimentos, 1, 0, missing = 0
        ),
        FLAG_DIAGNOSTICO = if_else(
          startsWith(DIAG_PRINC, "O") |
            startsWith(DIAG_SECUN, "O") |
            startsWith(DIAGSEC1, "O") |
            startsWith(DIAGSEC2, "O") |
            startsWith(DIAGSEC3, "O") |
            startsWith(DIAGSEC4, "O") |
            startsWith(DIAGSEC5, "O") |
            startsWith(DIAGSEC6, "O") |
            startsWith(DIAGSEC7, "O") |
            startsWith(DIAGSEC8, "O") |
            startsWith(DIAGSEC9, "O") |
            startsWith(CID_MORTE, "O") |
            startsWith(CID_ASSO, "O") |
            startsWith(CID_NOTIF, "O"),
          1, 0, missing = 0
        ),
        criterio_primario = ifelse(FLAG_PROC_REA == 1 | FLAG_DIAGNOSTICO == 1, 1, 0)
      ) |>
      # Filtrando apenas pelas internações obstétricas
      filter(
        criterio_primario == 1
      )

    # Juntando com os dados dos anos anteriores para a dada UF
    if (ano == anos[1]) {
      df_sih_rd_uf <- df_sih_rd_uf_aux
    } else {
      df_sih_rd_uf <- full_join(df_sih_rd_uf, df_sih_rd_uf_aux)
    }

    # Limpando a memória
    rm(df_sih_rd_uf_aux)
    gc()
  }

  # Salvando a base completa contendo apenas internações obstétricas para a dada UF
  output_dir <- glue("data-raw/morbidade/databases/01_sih_rd/01_arquivos_brutos")
  if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE)}

  write.csv2(
    df_sih_rd_uf,
    glue("{output_dir}/{estado}_sih_rd_bruto_{anos[1]}_{anos[length(anos)]}.csv"),
    row.names = FALSE
  )

  # Limpando a memória
  rm(df_sih_rd_uf)
  gc()
}


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
