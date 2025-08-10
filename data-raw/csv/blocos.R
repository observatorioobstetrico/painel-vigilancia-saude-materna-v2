# Criando a base auxiliar de municípios -----------------------------------
## Lendo uma base auxiliar que contém variáveis referentes ao nome do município, UF, região e IDHM
df_municipios_kmeans <- read.csv2("data-raw/csv/IDH_municipios-com-agrupamento_Kmeans.csv", sep = ";") |>
  janitor::clean_names() |>
  dplyr::select(
    codmunres = codmun6,
    municipio = nome,
    uf = nome_uf,
    regiao = regiao_pais,
    idhm,
    grupo_kmeans
  ) |>
  dplyr::mutate(
    regiao = ifelse(regiao == "Centro-oeste", "Centro-Oeste", regiao),
    uf = ifelse(uf == "SAO PAULO", "São Paulo", uf)
  )

## Lendo as informações de municípios sem IDHM
df_municipios_adicionais <- read.csv2("data-raw/csv/tabela_auxiliar_de_municipios_e_IDH.csv", sep = ";") |>
  janitor::clean_names() |>
  dplyr::select(
    codmunres = codmun6,
    municipio = nome,
    uf = nome_uf,
    regiao = regiao_pais,
    idhm
  ) |>
  dplyr::filter(idhm == "#N/A") |>
  dplyr::mutate(
    idhm = NA,
    regiao = ifelse(regiao == "Centro-oeste", "Centro-Oeste", regiao),
    uf = ifelse(uf == "SAO PAULO", "São Paulo", uf)
  )

## Lendo uma base auxiliar que contém variáveis referentes ao IDH das UFs e do Brasil
df_aux_idh_estados <- read.csv("data-raw/csv/tabela_IDH-censo2010_UFs-e-Brasil.csv", dec = ",")[-1, c(1, 3, 2)] |>
  janitor::clean_names() |>
  dplyr::rename(
    "uf" = territorialidade,
    "idh_uf" = idhm,
    "posicao_idh_uf" = posicao_idhm
  )

## Juntando as três bases
df_aux_municipios <- list(
  df_municipios_kmeans,
  df_municipios_adicionais,
  df_aux_idh_estados
) |>
  purrr::reduce(dplyr::full_join)

## Lendo uma base auxiliar que contém variáveis referentes às micro e macrorregiões de saúde estaduais
df_aux_r_saude <- readxl::read_xlsx("data-raw/csv/regiao_macro_municipio.xlsx") |>
  janitor::clean_names() |>
  dplyr::select(
    codmunres = cod_mun,
    municipio2 = nome_municipio,
    cod_r_saude = codigo_regiao_de_saude,
    r_saude = regiao_saude,
    cod_macro_r_saude = codigo_regiao_de_saude,
    macro_r_saude = macro
  ) |>
  dplyr::mutate(codmunres = as.numeric(codmunres))

## Criando a tabela final, que será exportada para o painel
tabela_aux_municipios <- dplyr::left_join(
  df_aux_municipios,
  df_aux_r_saude,
  by = "codmunres"
) |>
  dplyr::select(!municipio2) |>
  dplyr::mutate(posicao_idhm = dplyr::min_rank(dplyr::desc(idhm)))


# Criando as bases de cada bloco ------------------------------------------
## Para o bloco 1 ---------------------------------------------------------
### Lendo o arquivo contendo todos os indicadores
bloco1_aux <- read.csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2024.csv") |>
  janitor::clean_names()

### Adicionando as informações dos municípios
bloco1 <- dplyr::left_join(bloco1_aux, tabela_aux_municipios, by = "codmunres")
bloco1 <- bloco1 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco1) == "ano") + 1):(which(names(bloco1) == "municipio") - 1)
  )


## Para o bloco 2 ---------------------------------------------------------
### Lendo o arquivo contendo todos os indicadores
bloco2_aux <- read.csv("data-raw/csv/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2024.csv") |>
  janitor::clean_names()

### Adicionando as informações dos municípios
bloco2 <- dplyr::left_join(bloco2_aux, tabela_aux_municipios, by = "codmunres")
bloco2 <- bloco2 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco2) == "ano") + 1):(which(names(bloco2) == "municipio") - 1)
  )


## Para o bloco 3 ---------------------------------------------------------
### Lendo o arquivo contendo todos os indicadores
bloco3_aux <- read.csv("data-raw/csv/indicadores_bloco3_assistencia_pre-natal_2012-2024.csv") |>
  janitor::clean_names()

### Adicionando as informações dos municípios
bloco3 <- dplyr::left_join(bloco3_aux, tabela_aux_municipios, by = "codmunres")
bloco3 <- bloco3 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco3) == "ano") + 1):(which(names(bloco3) == "municipio") - 1)
  )

## Para o bloco 4 (aba de grupos de Robson) -------------------------------
### Lendo o arquivo contendo todos os indicadores
bloco4_aux <- read.csv("data-raw/csv/indicadores_bloco4_assistencia_ao_parto_2012-2024.csv") |>
  janitor::clean_names()

### Adicionando as informações dos municípios
bloco4 <- dplyr::left_join(bloco4_aux, tabela_aux_municipios, by = "codmunres")
bloco4 <- bloco4 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4) == "ano") + 1):(which(names(bloco4) == "municipio") - 1)
  )


## Para o bloco 4 (aba de deslocamento para o parto) ----------------------
### Lendo os arquivos contendo todos os indicadores para municípios
bloco4_deslocamento_muni_aux <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2012-2024.csv") |>
  janitor::clean_names()

bloco4_deslocamento_macrorregiao_aux <- read.csv("data-raw/csv/indicador_deslocamento_1500_2012_2024.csv")

### Adicionando as informações dos municípios
bloco4_deslocamento_muni <- dplyr::left_join(bloco4_deslocamento_muni_aux, tabela_aux_municipios, by = "codmunres")
bloco4_deslocamento_muni <- bloco4_deslocamento_muni |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4_deslocamento_muni) == "ano") + 1):(which(names(bloco4_deslocamento_muni) == "municipio") - 1)
  )

bloco4_deslocamento_macrorregiao <- dplyr::left_join(bloco4_deslocamento_macrorregiao_aux, tabela_aux_municipios, by = "codmunres")
bloco4_deslocamento_macrorregiao <- bloco4_deslocamento_macrorregiao |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4_deslocamento_macrorregiao) == "ano") + 1):(which(names(bloco4_deslocamento_macrorregiao) == "municipio") - 1)
  )

### Lendo os arquivo contendo todos os indicadores para UFs
bloco4_deslocamento_uf_aux1 <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_UF_2012-2020.csv") |>
  janitor::clean_names() |>
  dplyr::rename(cod_uf = uf) |>
  dplyr::rename(uf = nome) |>
  dplyr::mutate(
    uf = sub('.', '', uf),
    uf = ifelse(uf == "rasil", "Brasil", uf),
    km_partos_fora_macrorregiao = as.numeric(km_partos_fora_macrorregiao),
    km_partos_fora_macrorregiao_alta_complexidade = as.numeric(km_partos_fora_macrorregiao_alta_complexidade),
    km_partos_fora_macrorregiao_baixa_complexidade = as.numeric(km_partos_fora_macrorregiao_baixa_complexidade)
  )

bloco4_deslocamento_uf_aux2 <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_UF_2012-2024.csv") |>
  janitor::clean_names() |> dplyr::filter(ano > 2020)

bloco4_deslocamento_uf_aux <- rbind(bloco4_deslocamento_uf_aux2, bloco4_deslocamento_uf_aux1)

### Adicionando as informações dos municípios
bloco4_deslocamento_uf <- dplyr::left_join(
  bloco4_deslocamento_uf_aux,
  tabela_aux_municipios |> dplyr::select(uf, regiao) |> unique(),
  by = "uf"
) |>
  dplyr::arrange(ano, cod_uf)


## Para o bloco 4 (aba de local e profissional de assistência) ------------
### Lendo o arquivo contendo todos os indicadores para municípios
bloco4_profissional_aux <- read.csv("data-raw/csv/indicadores_bloco4_profissional_2012-2024.csv")

### Adicionando as informações dos municípios
bloco4_profissional <- dplyr::right_join(bloco4_profissional_aux, tabela_aux_municipios, by = "codmunres")
bloco4_profissional <- bloco4_profissional |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4_profissional) == "ano") + 1):(which(names(bloco4_profissional) == "municipio") - 1)
  )


## Para o bloco 5 ---------------------------------------------------------
### Lendo o arquivo contendo todos os indicadores
bloco5_aux <- read.csv("data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2024.csv") |>
  janitor::clean_names()

### Adicionando as informações dos municípios
bloco5 <- dplyr::left_join(bloco5_aux, tabela_aux_municipios, by = "codmunres")
bloco5 <- bloco5 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco5) == "ano") + 1):(which(names(bloco5) == "municipio") - 1)
  )

### Lendo o arquivo com as informações para a construção da tabela de malformações
malformacao_aux <- read.csv("data-raw/csv/malformacao_2012_2024.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres) |>
  dplyr::select(-c(3:10))

### Adicionando as informações dos municípios
malformacao <- dplyr::left_join(malformacao_aux, tabela_aux_municipios, by = "codmunres")
malformacao <- malformacao |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(malformacao) == "ano") + 1):(which(names(malformacao) == "municipio") - 1)
  )


## Para o bloco 6  --------------------------------------------------------
### Lendo o arquivo contendo todos os indicadores de mortalidade materna
bloco6_mortalidade_aux <- read.csv("data-raw/csv/indicadores_bloco6_mortalidade_materna_2012-2024.csv")

### Lendo o arquivo contendo todos os indicadores de morbidade materna
bloco6_morbidade_aux <- read.csv("data-raw/csv/indicadores_bloco6_morbidade_materna_2012-2024.csv", sep = ",") |>
  janitor::clean_names()

### Juntando as duas bases
bloco6_aux <- dplyr::left_join(bloco6_mortalidade_aux, bloco6_morbidade_aux, by = c("ano", "codmunres"))

### Adicionando as informações dos municípios
bloco6 <- dplyr::left_join(bloco6_aux, tabela_aux_municipios, by = "codmunres")
bloco6 <- bloco6 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco6) == "ano") + 1):(which(names(bloco6) == "municipio") - 1)
  )


## Para o bloco 7 (aba fetal)  --------------------------------------------
### Para os indicadores originais -----------------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_fetal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2024.csv")

#### Adicionando as informações dos municípios
bloco7_fetal <- dplyr::left_join(bloco7_fetal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_fetal <- bloco7_fetal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_fetal) == "ano") + 1):(which(names(bloco7_fetal) == "municipio") - 1)
  )

### Para os indicadores de causas evitáveis -------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_evitaveis_fetal_aux <- read.csv("data-raw/csv/indicadores_bloco7_causas_evitaveis_fetal_2012-2024.csv")

#### Adicionando as informações dos municípios
bloco7_evitaveis_fetal <- dplyr::left_join(bloco7_evitaveis_fetal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_evitaveis_fetal <- bloco7_evitaveis_fetal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_evitaveis_fetal) == "ano") + 1):(which(names(bloco7_evitaveis_fetal) == "municipio") - 1)
  )

### Para os indicadores de causas principais ------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_principais_fetal_aux <- read.csv("data-raw/csv/indicadores_bloco7_causas_principais_fetal_2012-2024.csv")

#### Adicionando as informações dos municípios
bloco7_principais_fetal <- dplyr::left_join(bloco7_principais_fetal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_principais_fetal <- bloco7_principais_fetal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_principais_fetal) == "ano") + 1):(which(names(bloco7_principais_fetal) == "municipio") - 1)
  )


## Para o bloco 7 (aba neonatal)  -----------------------------------------
### Para os indicadores originais -----------------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2024.csv")

#### Adicionando as informações dos municípios
bloco7_neonatal <- dplyr::left_join(bloco7_neonatal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_neonatal <- bloco7_neonatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_neonatal) == "ano") + 1):(which(names(bloco7_neonatal) == "municipio") - 1)
  )

### Para os indicadores de causas evitáveis -------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_evitaveis_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_causas_evitaveis_neonatal_2012-2024.csv")
bloco8_grafico_evitaveis_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco8_grafico_evitaveis_neonatal_2012-2024.csv") |>
  janitor::clean_names()

#### Adicionando as informações dos municípios
bloco7_evitaveis_neonatal <- dplyr::left_join(bloco7_evitaveis_neonatal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_evitaveis_neonatal <- bloco7_evitaveis_neonatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_evitaveis_neonatal) == "ano") + 1):(which(names(bloco7_evitaveis_neonatal) == "municipio") - 1)
  )

bloco8_grafico_evitaveis_neonatal <- dplyr::left_join(bloco8_grafico_evitaveis_neonatal_aux, tabela_aux_municipios, by = "codmunres")
bloco8_grafico_evitaveis_neonatal <- bloco8_grafico_evitaveis_neonatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_grafico_evitaveis_neonatal) == "ano") + 1):(which(names(bloco8_grafico_evitaveis_neonatal) == "municipio") - 1)
  )

### Para os indicadores de causas principais ------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_principais_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_causas_principais_neonatal_2012-2024.csv")

#### Adicionando as informações dos municípios
bloco7_principais_neonatal <- dplyr::left_join(bloco7_principais_neonatal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_principais_neonatal <- bloco7_principais_neonatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_principais_neonatal) == "ano") + 1):(which(names(bloco7_principais_neonatal) == "municipio") - 1)
  )


## Para o bloco 7 (aba perinatal)  -----------------------------------------
### Para os indicadores originais -----------------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_perinatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2024.csv")

#### Adicionando as informações dos municípios
bloco7_perinatal <- dplyr::left_join(bloco7_perinatal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_perinatal <- bloco7_perinatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_perinatal) == "ano") + 1):(which(names(bloco7_perinatal) == "municipio") - 1)
  )

### Para os indicadores de causas evitáveis -------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_evitaveis_perinatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_causas_evitaveis_perinatal_2012-2024.csv")

#### Adicionando as informações dos municípios
bloco7_evitaveis_perinatal <- dplyr::left_join(bloco7_evitaveis_perinatal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_evitaveis_perinatal <- bloco7_evitaveis_perinatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_evitaveis_perinatal) == "ano") + 1):(which(names(bloco7_evitaveis_perinatal) == "municipio") - 1)
  )

### Para os indicadores de causas principais ------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_principais_perinatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_causas_principais_perinatal_2012-2024.csv")

#### Adicionando as informações dos municípios
bloco7_principais_perinatal <- dplyr::left_join(bloco7_principais_perinatal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_principais_perinatal <- bloco7_principais_perinatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_principais_perinatal) == "ano") + 1):(which(names(bloco7_principais_perinatal) == "municipio") - 1)
  )


## Para o bloco 7 (aba morbidade neonatal)  -------------------------------
### Para os indicadores originais -----------------------------------------
#### Lendo o arquivo contendo todos os indicadores
bloco7_morbidade_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_morbidade_neonatal_2012-2024.csv") |>
  dplyr::left_join(bloco7_neonatal_aux |> dplyr::select(codmunres, ano, total_de_nascidos_vivos))

#### Adicionando as informações dos municípios
bloco7_morbidade_neonatal <- dplyr::left_join(bloco7_morbidade_neonatal_aux, tabela_aux_municipios, by = "codmunres")
bloco7_morbidade_neonatal <- bloco7_morbidade_neonatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_morbidade_neonatal) == "ano") + 1):(which(names(bloco7_morbidade_neonatal) == "municipio") - 1)
  )

### Para os indicadores de causas evitáveis e causas principais -----------
#### Lendo o arquivo contendo todos os indicadores
bloco7_dist_morbidade_aux <- read.csv("data-raw/csv/indicadores_bloco7_distribuicao_morbidade_neonatal_2012-2024.csv") |>
  dplyr::left_join(bloco7_neonatal_aux |> dplyr::select(codmunres, ano, total_de_nascidos_vivos))

#### Adicionando as informações dos municípios
bloco7_dist_morbidade <- dplyr::left_join(bloco7_dist_morbidade_aux, tabela_aux_municipios, by = "codmunres")
bloco7_dist_morbidade <- bloco7_dist_morbidade |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco7_dist_morbidade) == "ano") + 1):(which(names(bloco7_dist_morbidade) == "municipio") - 1)
  )

bloco7_dist_morbidade[is.na(bloco7_dist_morbidade)] <- 0



# Criando as bases de indicadores de qualidade da informação --------------
## Para os indicadores de garbage codes -----------------------------------
### Lendo o arquivo contendo todos os indicadores de garbage code para mortalidade
base_garbage_code_mortalidade_aux <- read.csv("data-raw/csv/indicadores_bloco8_graficos_garbage_code_2012-2024.csv") |>
  janitor::clean_names()

### Lendo o arquivo contendo todos os indicadores de garbage code para morbidade
base_garbage_code_morbidade_aux <- read.csv(gzfile("data-raw/csv/indicadores_bloco8_graficos_garbage_code_morbidade_2012-2024.csv")) |>
  janitor::clean_names()

### Juntando as duas bases
base_garbage_code_aux <- dplyr::full_join(base_garbage_code_mortalidade_aux, base_garbage_code_morbidade_aux)
base_garbage_code_aux[is.na(base_garbage_code_aux)] <- 0

### Adicionando as informações dos municípios
base_garbage_code <- dplyr::left_join(base_garbage_code_aux, tabela_aux_municipios, by = "codmunres")
base_garbage_code <- base_garbage_code |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_garbage_code) == "ano") + 1):(which(names(base_garbage_code) == "municipio") - 1)
  )


## Para os indicadores de incompletude ------------------------------------
### Para os indicadores de incompletude do SINASC -------------------------
#### Lendo o arquivo contendo todos os indicadores
base_incompletude_antiga <- read.csv("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_antigas/base_incompletude_2012_2023.csv")

base_incompletude_sinasc_aux <- read.csv2("data-raw/csv/incompletude_SINASC_2012-2023.csv", sep = ",")[, -1] |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres) |>
  dplyr::select(!c(dplyr::contains("consprenat"), dplyr::contains("idanomal"), dplyr::contains("qtdpartces"), dplyr::contains("tprobson"))) |>
  dplyr::full_join(
    base_incompletude_antiga |>
      dplyr::select(
        codmunres, ano, dplyr::contains("consprenat"),
        dplyr::contains("idanomal"), dplyr::contains("qtdpartces"),
        dplyr::contains("tprobson")
      )
  )

#### Adicionando as informações dos municípios
base_incompletude_sinasc <- dplyr::left_join(base_incompletude_sinasc_aux, tabela_aux_municipios, by = "codmunres")
base_incompletude_sinasc <- base_incompletude_sinasc |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_sinasc) == "ano") + 1):(which(names(base_incompletude_sinasc) == "municipio") - 1)
  )


### Para os indicadores de incompletude do deslocamento ------------------
#### Lendo o arquivo contendo todos os indicadores
base_incompletude_deslocamento_aux <- read.csv("data-raw/csv/incompletitude_indicadores_deslocamento_parto.csv") |>
  janitor::clean_names() |>
  dplyr::select(!uf)

### Adicionando as informações dos municípios
base_incompletude_deslocamento <- dplyr::left_join(base_incompletude_deslocamento_aux, tabela_aux_municipios, by = "codmunres")
base_incompletude_deslocamento <- base_incompletude_deslocamento |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_deslocamento) == "ano") + 1):(which(names(base_incompletude_deslocamento) == "municipio") - 1)
  )

### Para os indicadores de incompletude do profissional de assist --------
#### Lendo o arquivo contendo todos os indicadores
base_incompletude_bloco4_profissional <- read.csv("data-raw/csv/indicadores_incompletude_bloco4_profissional_2013-2023.csv") |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres)


### Para os indicadores de incompletude do SIM (bloco 6) -----------------
#### Lendo o arquivo contendo todos os indicadores de incompletude de óbitos maternos
base_incompletude_sim_maternos_aux <- read.csv("data-raw/csv/incompletude_sim_obitos_maternos.csv") |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres)

#### Lendo o arquivo contendo todos os indicadores de incompletude de óbitos de MIF
base_incompletude_sim_mif_aux <- read.csv("data-raw/csv/incompletude_sim_obitos_mif.csv") |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres)

#### Juntando as duas bases
base_incompletude_sim_aux <- dplyr::full_join(base_incompletude_sim_maternos_aux, base_incompletude_sim_mif_aux, by = c("codmunres", "ano"))

#### Adicionando as informações dos municípios
base_incompletude_sim <- dplyr::left_join(base_incompletude_sim_aux, tabela_aux_municipios, by = "codmunres")
base_incompletude_sim <- base_incompletude_sim |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_sim) == "ano") + 1):(which(names(base_incompletude_sim) == "municipio") - 1)
  )

### Para os indicadores de incompletude do SIM (bloco 7) -----------------
#### Lendo o arquivo contendo todos os indicadores de incompletude de morbidade neonatal
base_incompletude_bloco7_morbidade_aux <- read.csv('data-raw/csv/indicadores_incompletude_bloco7_morbidade_2012-2023.csv')

#### Lendo o arquivo contendo todos os indicadores de incompletude das outras abas
base_incompletude_bloco7_outros_aux <- read.csv('data-raw/csv/indicadores_incompletude_bloco7_2012-2024.csv') |>
  dplyr::filter(ano <= 2023) |>
  dplyr::mutate(across(everything(), ~tidyr::replace_na(.x, 0)))

#### Juntando as duas bases
base_incompletude_bloco7_aux <- dplyr::full_join(base_incompletude_bloco7_outros_aux, base_incompletude_bloco7_morbidade_aux)

#### Adicionando as informações dos municípios
base_incompletude_bloco7 <- dplyr::left_join(base_incompletude_bloco7_aux, tabela_aux_municipios, by = "codmunres")
base_incompletude_bloco7 <- base_incompletude_bloco7 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_bloco7) == "ano") + 1):(which(names(base_incompletude_bloco7) == "municipio") - 1)
  )


### Juntando todas as bases de incompletude em uma base única ------------
base_incompletude <- dplyr::full_join(
  dplyr::full_join(
    dplyr::full_join(
      dplyr::full_join(
        dplyr::full_join(
          base_incompletude_sinasc,
          base_incompletude_deslocamento,
          by = c("ano", "codmunres", "municipio", "grupo_kmeans", "uf", "regiao", "cod_r_saude", "r_saude", "cod_macro_r_saude", "macro_r_saude")
        ),
        base_incompletude_sim,
        by = c("ano", "codmunres", "municipio", "grupo_kmeans", "uf", "regiao", "cod_r_saude", "r_saude", "cod_macro_r_saude", "macro_r_saude")
      ),
      base_incompletude_deslocamento
    ),
    base_incompletude_bloco7
  ),
  base_incompletude_bloco4_profissional
)



# Lendo outros arquivos ---------------------------------------------------
## Carregando a base que contém as informações necessárias para o cálcula da referência de baixo peso
base_referencia_baixo_peso <- read.csv("data-raw/csv/Nasc_baixo_peso_muni2006_2010.csv")

## Carregando a base que contém os fatores de correção para a RMM
rmm_fator_de_correcao <- read.csv("data-raw/csv/rmm_fator_de_correcao.csv", sep = ";", dec = ",", fileEncoding = "utf-8")[, -c(2, 3, 4)] |>
  janitor::clean_names() |>
  tidyr::pivot_longer(
    cols = !localidade,
    names_to = "ano",
    values_to = "fator_de_correcao"
  ) |>
  dplyr::mutate(
    ano = dplyr::case_when(
      ano == "x2012" ~ 2012,
      ano == "x2013" ~ 2013,
      ano == "x2014" ~ 2014,
      ano == "x2015" ~ 2015,
      ano == "x2016" ~ 2016,
      ano == "x2017" ~ 2017,
      ano == "x2018" ~ 2018,
      ano == "x2019" ~ 2019,
      ano == "x2020" ~ 2020,
    )
  )

## Carregando a base que contém as RMM corrigidas para estado, região e Brail de 2012 a 2021
rmm_corrigida <- read.csv("data-raw/csv/rmm_corrigida_2012-2021.csv") |>
  dplyr::select(ano, localidade, RMM) |>
  dplyr::mutate(RMM = round(RMM, 1))

## Criando os dataframes/vetores contendo as escolhas de municípios, estados e micro e macrorregões de saúde
municipios_choices <- tabela_aux_municipios |>
  dplyr::select(uf, municipio)

estados_choices <- tabela_aux_municipios |>
  dplyr::pull(uf) |>
  unique()

micro_r_saude_choices <- tabela_aux_municipios |>
  dplyr::select(uf, r_saude) |>
  unique()

macro_r_saude_choices <- tabela_aux_municipios |>
  dplyr::select(uf, macro_r_saude) |>
  unique()

## Lendo a tabela contendo as informações sobre os indicadores
tabela_indicadores <- read.csv("data-raw/csv/tabela_indicadores.csv")

## Lendo a tabela contendo as informações para o grafico de radar
tabela_radar <- read.csv("data-raw/csv/tabela_radar.csv") |>
  dplyr::mutate(
    nome_abreviado = dplyr::case_when(
      nome_abreviado == "porc_1con" ~ "cobertura_pre_natal",
      nome_abreviado == "porc_baixo_peso" ~ "porc_nasc_baixo_peso",
      nome_abreviado == "porc_baixo_peso_menos_1500" ~ "porc_nasc_baixo_peso_menos_1500",
      nome_abreviado == "porc_baixo_peso_menos_2500" ~ "porc_nasc_baixo_peso_menos_2500",
      nome_abreviado == "porc_premat" ~ "porc_nasc_premat",
      nome_abreviado == "porc_premat_menos_28" ~ "porc_nasc_premat_menos_28",
      nome_abreviado == "porc_premat_menos_32" ~ "porc_nasc_premat_menos_32",
      nome_abreviado == "porc_premat_menos_37" ~ "porc_nasc_premat_menos_37",
      .default = nome_abreviado
    )
  )

## Lendo a tabela contendo informações sobre as CIDs
df_cid10 <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_cid10_completo.csv")


# Exportando os arquivos para dentro do painel ----------------------------
usethis::use_data(bloco1, overwrite = TRUE)
usethis::use_data(bloco2, overwrite = TRUE)
usethis::use_data(bloco3, overwrite = TRUE)
usethis::use_data(bloco4, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_muni, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_uf, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_macrorregiao, overwrite = TRUE)
usethis::use_data(bloco4_profissional, overwrite = TRUE)
usethis::use_data(bloco5, overwrite = TRUE)
usethis::use_data(malformacao, overwrite = TRUE)
usethis::use_data(bloco6, overwrite = TRUE)
usethis::use_data(bloco7_fetal, overwrite = TRUE)
usethis::use_data(bloco7_neonatal, overwrite = TRUE)
usethis::use_data(bloco7_perinatal, overwrite = TRUE)
usethis::use_data(bloco7_morbidade_neonatal, overwrite = TRUE)
usethis::use_data(bloco7_evitaveis_fetal, overwrite = TRUE)
usethis::use_data(bloco7_evitaveis_neonatal, overwrite = TRUE)
usethis::use_data(bloco8_grafico_evitaveis_neonatal, overwrite = TRUE)
usethis::use_data(bloco7_evitaveis_perinatal, overwrite = TRUE)
usethis::use_data(bloco7_principais_fetal, overwrite = TRUE)
usethis::use_data(bloco7_principais_neonatal, overwrite = TRUE)
usethis::use_data(bloco7_principais_perinatal, overwrite = TRUE)
usethis::use_data(bloco7_dist_morbidade, overwrite = TRUE)
usethis::use_data(base_garbage_code, overwrite = TRUE)
usethis::use_data(df_cid10, overwrite = TRUE)
usethis::use_data(base_incompletude, overwrite = TRUE)
usethis::use_data(tabela_aux_municipios, overwrite = TRUE)
usethis::use_data(municipios_choices, overwrite = TRUE)
usethis::use_data(estados_choices, overwrite = TRUE)
usethis::use_data(micro_r_saude_choices, overwrite = TRUE)
usethis::use_data(macro_r_saude_choices, overwrite = TRUE)
usethis::use_data(base_referencia_baixo_peso, overwrite = TRUE)
usethis::use_data(tabela_indicadores, overwrite = TRUE)  #Utilizada no nível 3
usethis::use_data(tabela_radar, overwrite = TRUE)  #Utilizada no gráfico de radar
usethis::use_data(rmm_fator_de_correcao, overwrite = TRUE)
usethis::use_data(rmm_corrigida, overwrite = TRUE)

