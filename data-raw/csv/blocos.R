#Carregando a base auxiliar que contém variáveis referentes ao nome do município, UF, região e IDHM
municipios_kmeans <- read.csv2("data-raw/csv/IDH_municipios-com-agrupamento_Kmeans.csv", sep = ";") |>
  janitor::clean_names() |>
  dplyr::select(
    codmunres = codmun6,
    municipio = nome,
    uf = nome_uf,
    regiao = regiao_pais,
    idhm,
    grupo_kmeans
  )

municipios_adicionais <- read.csv2("data-raw/csv/tabela_auxiliar_de_municipios_e_IDH.csv", sep = ";") |>
  janitor::clean_names() |>
  dplyr::select(
    codmunres = codmun6,
    municipio = nome,
    uf = nome_uf,
    regiao = regiao_pais,
    idhm
  ) |>
  dplyr::filter(idhm == "#N/A")

municipios_adicionais$idhm <- NA

aux_municipios_1 <- dplyr::full_join(municipios_kmeans, municipios_adicionais)

aux_municipios_1$regiao[which(aux_municipios_1$regiao == "Centro-oeste")] <- "Centro-Oeste"

aux_municipios_1$uf[which(aux_municipios_1$uf == "SAO PAULO")] <- "São Paulo"

#Carregando a base auxiliar que contém variáveis referentes às micro e macrorregiões de saúde
aux_r_saude <- readODS::read_ods("data-raw/extracao-dos-dados/cobertura/regioes_macrorregioes.ods") |>
  janitor::clean_names() |>
  dplyr::rename(
    codmunres = codmun,
    municipio2 = nomemun,
    cod_r_saude = codr_saude,
    r_saude = rsaude
  )

#Juntando as duas bases auxiliares
aux_municipios <- dplyr::left_join(aux_municipios_1, aux_r_saude, by = "codmunres") |>
  dplyr::select(!municipio2)

#Criando uma coluna contendo o ranking dos IDHMs
aux_idhm <- data.frame(
  "idhm" = unique(sort(aux_municipios$idhm)),
  "posicao_idhm" = c(0, (length(unique(sort(aux_municipios$idhm))) - 1):1)
)

#Carregando a base auxiliar que contém variáveis referentes ao IDH das UFs e do Brasil
aux_idh_estados <- read.csv("data-raw/csv/tabela_IDH-censo2010_UFs-e-Brasil.csv", dec = ",")[-1, c(1, 3, 2)] |>
  janitor::clean_names() |>
  dplyr::rename(
    "uf" = territorialidade,
    "idh_uf" = idhm,
    "posicao_idh_uf" = posicao_idhm
  )

#Juntando todas as bases auxiliares (a tabela_aux_municipios será salva para ser utilizada ao carregar o pacote)
tabela_aux_municipios <- dplyr::left_join(dplyr::left_join(aux_municipios, aux_idhm, by = "idhm"), aux_idh_estados, by = "uf")

#Lendo os arquivos originais
bloco1_aux <- read.csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2024.csv") |>
  janitor::clean_names()

bloco2_aux <- read.csv("data-raw/csv/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2024.csv") |>
  janitor::clean_names()

bloco3_aux <- read.csv("data-raw/csv/indicadores_bloco3_assistencia_pre-natal_2012-2024.csv") |>
  janitor::clean_names()

bloco4_aux <- read.csv("data-raw/csv/indicadores_bloco4_assistencia_ao_parto_2012-2024.csv") |>
  janitor::clean_names()

# bloco4_deslocamento_muni_aux1 <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2012-2020.csv") |>
#   janitor::clean_names()
#
# bloco4_deslocamento_muni_aux2 <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2021-2022.csv") |>
#   janitor::clean_names() |>
#   dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres)
#
# bloco4_deslocamento_muni_aux3 <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2023.csv") |>
#   janitor::clean_names() |>
#   dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres)
#
# aux_municipios_deslocamento_mun_2023 <- aux_municipios |>
#   dplyr::select(codmunres) |>
#   dplyr::mutate(ano = 2023)
#
# bloco4_deslocamento_muni_aux3 <- dplyr::left_join(aux_municipios_deslocamento_mun_2023, bloco4_deslocamento_muni_aux3)
#
# bloco4_deslocamento_muni_aux4 <- rbind(bloco4_deslocamento_muni_aux2, bloco4_deslocamento_muni_aux3)
#
# bloco4_deslocamento_muni_aux1$km_partos_fora_uf <- as.numeric(bloco4_deslocamento_muni_aux1$km_partos_fora_uf)
#
# bloco4_deslocamento_muni_aux <- dplyr::full_join(bloco4_deslocamento_muni_aux1, bloco4_deslocamento_muni_aux4) |>
#   dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres) |>
#   dplyr::arrange(codmunres, ano)
#
# bloco4_deslocamento_muni_aux$km_partos_fora_uf_alta_complexidade <- as.numeric(bloco4_deslocamento_muni_aux$km_partos_fora_uf_alta_complexidade)
# bloco4_deslocamento_muni_aux$km_partos_fora_uf_baixa_complexidade <- as.numeric(bloco4_deslocamento_muni_aux$km_partos_fora_uf_baixa_complexidade)

bloco4_deslocamento_muni_aux <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2012-2024.csv") |>
  dplyr::select(1:37)

################################################################################

bloco4_deslocamento_uf_aux1 <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_UF_2012-2020.csv") |>
  janitor::clean_names() |>
  dplyr::rename(cod_uf = uf) |>
  dplyr::rename(uf = nome) |>
  dplyr::mutate(
    uf = sub('.', '', uf)
  )

bloco4_deslocamento_uf_aux2 <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_UF_2021-2023.csv") |>
  janitor::clean_names()

bloco4_deslocamento_uf_aux1$km_partos_fora_macrorregiao <- as.numeric(bloco4_deslocamento_uf_aux1$km_partos_fora_macrorregiao)

bloco4_deslocamento_uf_aux1$km_partos_fora_macrorregiao_alta_complexidade <- as.numeric(bloco4_deslocamento_uf_aux1$km_partos_fora_macrorregiao_alta_complexidade)
bloco4_deslocamento_uf_aux1$km_partos_fora_macrorregiao_baixa_complexidade <- as.numeric(bloco4_deslocamento_uf_aux1$km_partos_fora_macrorregiao_baixa_complexidade)

bloco4_deslocamento_uf_aux <- rbind(bloco4_deslocamento_uf_aux2, bloco4_deslocamento_uf_aux1)

bloco4_deslocamento_uf_aux$uf[which(bloco4_deslocamento_uf_aux$uf == "rasil")] <- "Brasil"

# bloco4_deslocamento_macrorregiao <- read.csv("data-raw/csv/indicador_deslocamento_1500_2012_2023.csv")

bloco4_deslocamento_macrorregiao <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2012-2023.csv") |>
  dplyr::select(c(1:2, 38:44))

################################################################################

bloco5_aux <- read.csv("data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012_2024.csv") |>
  janitor::clean_names()

# asfixia_aux <- read.csv("data-raw/csv/asfixia_2012_2022.csv", sep = ';') |>
#   janitor::clean_names() |>
#   dplyr::rename(total_nascidos = total_de_nascidos_vivos) |>
#   select(!total_de_nascidos_malformacao)

malformacao_aux <- read.csv("data-raw/csv/malformacao_2012_2024.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  dplyr::filter(codmunres %in% aux_municipios$codmunres) |>
  dplyr::select(-c(3:10))

bloco6_mortalidade_aux <- read.csv("data-raw/csv/indicadores_bloco6_mortalidade_materna_2012-2024.csv") |>
  dplyr::select(!c(uf, municipio, regiao))

bloco6_morbidade_aux <- read.csv("data-raw/csv/indicadores_bloco6_morbidade_materna_2012-2024.csv", sep = ",") |>
  janitor::clean_names()

bloco6_aux <- dplyr::left_join(bloco6_mortalidade_aux, bloco6_morbidade_aux, by = c("ano", "codmunres"))

bloco7_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2024.csv") #|>
  #dplyr::select(!c(uf, municipio, regiao))

bloco7_neonatal_aux[is.na(bloco7_neonatal_aux)] <- 0

bloco7_morbidade_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_morbidade_neonatal_2012-2024.csv") #|>


bloco7_fetal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2024.csv") #|>
  #dplyr::select(!(nascidos)
  #) |>
 # dplyr::select(!c(uf, municipio, regiao))

bloco7_perinatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2024.csv") #|>
  #dplyr::select(!c(uf, municipio, regiao))
bloco7_perinatal_aux$codmunres <- as.numeric(bloco7_perinatal_aux$codmunres)

bloco7_perinatal_aux[is.na(bloco7_perinatal_aux)] <- 0


juncao_bloco7_aux1 <- dplyr::left_join(bloco7_neonatal_aux, bloco7_fetal_aux, by = c("ano", "codmunres"))
juncao_bloco7_aux2 <- dplyr::left_join(juncao_bloco7_aux1, bloco7_morbidade_neonatal_aux, by = c("ano", "codmunres"))
bloco7_aux <- dplyr::left_join(juncao_bloco7_aux2, bloco7_perinatal_aux, by = c("ano", "codmunres"))

bloco7_dist_morbidade_aux <- read.csv("data-raw/csv/indicadores_bloco7_distribuicao_morbidade_neonatal_2012-2024.csv") #|>


bloco8_graficos_aux <- read.csv("data-raw/csv/indicadores_bloco8_graficos_2012-2024.csv") |>
  janitor::clean_names()

bloco8_grafico_evitaveis_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco8_grafico_evitaveis_neonatal_2012-2024.csv") |>
  janitor::clean_names()

bloco8_garbage_materno_aux <- read.csv(gzfile("data-raw/csv/garbage_materno_2012_2022.csv.gz")) |>
  janitor::clean_names()

bloco8_garbage_fetal_aux <- read.csv(gzfile("data-raw/csv/garbage_fetal_2012_2022.csv.gz")) |>
  janitor::clean_names()

bloco8_garbage_neonatal_aux <- read.csv(gzfile("data-raw/csv/garbage_neonatal_2012_2022.csv.gz")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    faixa_de_peso = factor(faixa_de_peso, levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")),
    faixa_de_idade = factor(faixa_de_idade, levels = c("0 a 6 dias", "7 a 27 dias")),
  )

bloco8_principais_fetal_aux <- read.csv(gzfile("data-raw/csv/principais_fetal_2012_2022.csv.gz")) |>
  janitor::clean_names()

bloco8_principais_neonatal_aux <- read.csv(gzfile("data-raw/csv/principais_neonatal_2012_2022.csv.gz")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    faixa_de_peso = factor(faixa_de_peso, levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")),
    faixa_de_idade = factor(faixa_de_idade, levels = c("0 a 6 dias", "7 a 27 dias")),
  )

bloco8_evitaveis_fetal_aux <- read.csv(gzfile("data-raw/csv/evitaveis_fetal_2012_2022.csv.gz")) |>
  janitor::clean_names()

bloco8_evitaveis_neonatal_aux <- read.csv(gzfile("data-raw/csv/evitaveis_neonatal_2012_2022.csv.gz")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    faixa_de_peso = factor(faixa_de_peso, levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")),
    faixa_de_idade = factor(faixa_de_idade, levels = c("0 a 6 dias", "7 a 27 dias")),
  )

bloco8_fetal_grupos_aux <- read.csv(gzfile("data-raw/csv/grupos_fetal_2012_2022.csv.gz")) |>
  janitor::clean_names()

bloco8_grupos_neonatal_aux <- read.csv(gzfile("data-raw/csv/grupos_neonatal_2012_2022.csv.gz")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    faixa_de_peso = factor(faixa_de_peso, levels = c("< 1500 g", "1500 a 1999 g", "2000 a 2499 g", "\U2265 2500 g", "Sem informação")),
    faixa_de_idade = factor(faixa_de_idade, levels = c("0 a 6 dias", "7 a 27 dias")),
  )

base_incompletude_sinasc_aux <- read.csv2("data-raw/csv/incompletude_SINASC_2012-2022.csv", sep = ",")[, -1] |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% aux_municipios$codmunres)

base_incompletude_sim_maternos_aux <- read.csv("data-raw/csv/incompletude_sim_obitos_maternos.csv") |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% aux_municipios$codmunres)

base_incompletude_sim_mif_aux <- read.csv("data-raw/csv/incompletude_sim_obitos_mif.csv") |>
  janitor::clean_names() |>
  dplyr::filter(codmunres %in% aux_municipios$codmunres)

base_incompletude_sim_aux <- dplyr::full_join(base_incompletude_sim_maternos_aux, base_incompletude_sim_mif_aux, by = c("codmunres", "ano"))

base_incompletude_deslocamento_aux <- read.csv("data-raw/csv/incompletitude_indicadores_deslocamento_parto.csv") |>
  janitor::clean_names() |>
  dplyr::select(!uf)


#Adicionando as variáveis referentes ao nome do município, UF, região e micro e macrorregiões de saúde
bloco1 <- dplyr::left_join(bloco1_aux, aux_municipios, by = "codmunres")
bloco1 <- bloco1 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco1) == "ano") + 1):(which(names(bloco1) == "municipio") - 1)
  )

bloco2 <- dplyr::left_join(bloco2_aux, aux_municipios, by = "codmunres")
bloco2 <- bloco2 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco2) == "ano") + 1):(which(names(bloco2) == "municipio") - 1)
  )

bloco3 <- dplyr::left_join(bloco3_aux, aux_municipios, by = "codmunres")
bloco3 <- bloco3 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco3) == "ano") + 1):(which(names(bloco3) == "municipio") - 1)
  )


bloco4 <- dplyr::left_join(bloco4_aux, aux_municipios, by = "codmunres")
bloco4 <- bloco4 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4) == "ano") + 1):(which(names(bloco4) == "municipio") - 1)
  )

bloco4_deslocamento_muni <- dplyr::left_join(bloco4_deslocamento_muni_aux, aux_municipios, by = "codmunres")

bloco4_deslocamento_muni <- bloco4_deslocamento_muni |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4_deslocamento_muni) == "ano") + 1):(which(names(bloco4_deslocamento_muni) == "municipio") - 1)
  )

bloco4_deslocamento_uf <- dplyr::left_join(bloco4_deslocamento_uf_aux, aux_municipios |> dplyr::select(uf, regiao) |> unique(), by = "uf")

bloco4_deslocamento_uf <- bloco4_deslocamento_uf |> dplyr::arrange(ano)


bloco4_deslocamento_macrorregiao <- dplyr::left_join(bloco4_deslocamento_macrorregiao, aux_municipios, by = "codmunres")
bloco4_deslocamento_macrorregiao <- bloco4_deslocamento_macrorregiao |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco4_deslocamento_macrorregiao) == "ano") + 1):(which(names(bloco4_deslocamento_macrorregiao) == "municipio") - 1)
  )


bloco5 <- dplyr::left_join(bloco5_aux, aux_municipios, by = "codmunres")
bloco5 <- bloco5 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco5) == "ano") + 1):(which(names(bloco5) == "municipio") - 1)
  )

# asfixia <- dplyr::left_join(asfixia_aux, aux_municipios, by = "codmunres")
# asfixia <- asfixia |>
#   dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres) |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(asfixia) == "ano") + 1):(which(names(asfixia) == "municipio") - 1)
#   )

malformacao <- dplyr::left_join(malformacao_aux, aux_municipios, by = "codmunres")
malformacao <- malformacao |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(malformacao) == "ano") + 1):(which(names(malformacao) == "municipio") - 1)
  )

bloco6_aux$codmunres <- as.numeric(bloco6_aux$codmunres)

bloco6 <- dplyr::left_join(bloco6_aux, aux_municipios, by = "codmunres")
bloco6 <- bloco6 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco6) == "ano") + 1):(which(names(bloco6) == "municipio") - 1)
  )

bloco7 <- dplyr::left_join(bloco7_aux, aux_municipios, by = "codmunres")
# bloco7 <- bloco7 |>
#   dplyr::select(
#     ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#     (which(names(bloco7) == "ano") + 1):(which(names(bloco7) == "municipio") - 1)
#   )

bloco7_dist_morbidade <- dplyr::left_join(bloco7_dist_morbidade_aux, aux_municipios, by = "codmunres")

bloco8_graficos <- dplyr::left_join(bloco8_graficos_aux, aux_municipios, by = "codmunres")
bloco8_graficos <- bloco8_graficos |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_graficos) == "ano") + 1):(which(names(bloco8_graficos) == "municipio") - 1)
  )

bloco8_grafico_evitaveis_neonatal <- dplyr::left_join(bloco8_grafico_evitaveis_neonatal_aux, aux_municipios, by = "codmunres")
bloco8_grafico_evitaveis_neonatal <- bloco8_grafico_evitaveis_neonatal |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_grafico_evitaveis_neonatal) == "ano") + 1):(which(names(bloco8_grafico_evitaveis_neonatal) == "municipio") - 1)
  )

bloco8_garbage_materno_aux <- dplyr::left_join(bloco8_garbage_materno_aux, aux_municipios, by = "codmunres")
bloco8_garbage_materno <- bloco8_garbage_materno_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_garbage_materno_aux) == "ano") + 1):(which(names(bloco8_garbage_materno_aux) == "municipio") - 1)
  )

bloco8_garbage_fetal_aux <- dplyr::left_join(bloco8_garbage_fetal_aux, aux_municipios, by = "codmunres")
bloco8_garbage_fetal <- bloco8_garbage_fetal_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_garbage_fetal_aux) == "ano") + 1):(which(names(bloco8_garbage_fetal_aux) == "municipio") - 1)
  )


bloco8_garbage_neonatal_aux <- dplyr::left_join(bloco8_garbage_neonatal_aux, aux_municipios, by = "codmunres")
bloco8_garbage_neonatal <- bloco8_garbage_neonatal_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_garbage_neonatal_aux) == "ano") + 1):(which(names(bloco8_garbage_neonatal_aux) == "municipio") - 1)
  )

bloco8_principais_fetal_aux <- dplyr::left_join(bloco8_principais_fetal_aux, aux_municipios, by = "codmunres")
bloco8_principais_fetal <- bloco8_principais_fetal_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_principais_fetal_aux) == "ano") + 1):(which(names(bloco8_principais_fetal_aux) == "municipio") - 1)
  )

bloco8_principais_neonatal_aux <- dplyr::left_join(bloco8_principais_neonatal_aux, aux_municipios, by = "codmunres")
bloco8_principais_neonatal <- bloco8_principais_neonatal_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_principais_neonatal_aux) == "ano") + 1):(which(names(bloco8_principais_neonatal_aux) == "municipio") - 1)
  )

bloco8_evitaveis_fetal_aux <- dplyr::left_join(bloco8_evitaveis_fetal_aux, aux_municipios, by = "codmunres")
bloco8_evitaveis_fetal <- bloco8_evitaveis_fetal_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_evitaveis_fetal_aux) == "ano") + 1):(which(names(bloco8_evitaveis_fetal_aux) == "municipio") - 1)
  )


bloco8_evitaveis_neonatal_aux <- dplyr::left_join(bloco8_evitaveis_neonatal_aux, aux_municipios, by = "codmunres")
bloco8_evitaveis_neonatal <- bloco8_evitaveis_neonatal_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_evitaveis_neonatal_aux) == "ano") + 1):(which(names(bloco8_evitaveis_neonatal_aux) == "municipio") - 1)
  )

bloco8_fetal_grupos_aux <- dplyr::left_join(bloco8_fetal_grupos_aux, aux_municipios, by = "codmunres")
bloco8_fetal_grupos <- bloco8_fetal_grupos_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_fetal_grupos_aux) == "ano") + 1):(which(names(bloco8_fetal_grupos_aux) == "municipio") - 1)
  )

bloco8_grupos_neonatal_aux <- dplyr::left_join(bloco8_grupos_neonatal_aux, aux_municipios, by = "codmunres")
bloco8_grupos_neonatal <- bloco8_grupos_neonatal_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_grupos_neonatal_aux) == "ano") + 1):(which(names(bloco8_grupos_neonatal_aux) == "municipio") - 1)
  )



base_incompletude_sinasc <- dplyr::left_join(base_incompletude_sinasc_aux, aux_municipios, by = "codmunres")
base_incompletude_sinasc <- base_incompletude_sinasc |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_sinasc) == "ano") + 1):(which(names(base_incompletude_sinasc) == "municipio") - 1)
  )

base_incompletude_sim <- dplyr::left_join(base_incompletude_sim_aux, aux_municipios, by = "codmunres")
base_incompletude_sim <- base_incompletude_sim |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_sim) == "ano") + 1):(which(names(base_incompletude_sim) == "municipio") - 1)
  )

base_incompletude_deslocamento <- dplyr::left_join(base_incompletude_deslocamento_aux, aux_municipios, by = "codmunres")
base_incompletude_deslocamento <- base_incompletude_deslocamento |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(base_incompletude_deslocamento) == "ano") + 1):(which(names(base_incompletude_deslocamento) == "municipio") - 1)
  )

base_incompletude <- dplyr::full_join(
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
)


#Carregando a base que contém as informações necessárias para o cálcula da referência de baixo peso
base_referencia_baixo_peso <- read.csv("data-raw/csv/Nasc_baixo_peso_muni2006_2010.csv")

#Carregando a base que contém os fatores de correção para a RMM
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

#Carregando a base que contém as RMM corrigidas para estado, região e Brail de 2012 a 2021
rmm_corrigida <- read.csv("data-raw/csv/rmm_corrigida_2012-2021.csv") |>
  dplyr::select(ano, localidade, RMM) |>
  dplyr::mutate(RMM = round(RMM, 1))

#Criando os dataframes/vetores contendo as escolhas de municípios, estados e micro e macrorregões de saúde
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

#Lendo a tabela contendo as informações sobre os indicadores
tabela_indicadores <- read.csv("data-raw/csv/tabela_indicadores.csv")

#Lendo a tabela contendo as informações para o grafico de radar
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


#Lendo a tabela contendo informações sobre as CIDs
df_cid10 <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/df_cid10_completo.csv")

usethis::use_data(bloco1, overwrite = TRUE)
usethis::use_data(bloco2, overwrite = TRUE)
usethis::use_data(bloco3, overwrite = TRUE)
usethis::use_data(bloco4, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_muni, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_uf, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_macrorregiao, overwrite = TRUE)
usethis::use_data(bloco5, overwrite = TRUE)
# usethis::use_data(asfixia, overwrite = TRUE)
usethis::use_data(malformacao, overwrite = TRUE)
usethis::use_data(bloco6, overwrite = TRUE)
usethis::use_data(bloco7, overwrite = TRUE)
usethis::use_data(bloco7_dist_morbidade, overwrite = TRUE)
usethis::use_data(bloco8_garbage_materno, overwrite = TRUE)
usethis::use_data(bloco8_garbage_fetal, overwrite = TRUE)
usethis::use_data(bloco8_garbage_neonatal, overwrite = TRUE)
usethis::use_data(bloco8_principais_fetal, overwrite = TRUE)
usethis::use_data(bloco8_principais_neonatal, overwrite = TRUE)
usethis::use_data(bloco8_evitaveis_fetal, overwrite = TRUE)
usethis::use_data(bloco8_evitaveis_neonatal, overwrite = TRUE)
usethis::use_data(bloco8_fetal_grupos, overwrite = TRUE)
usethis::use_data(bloco8_grupos_neonatal, overwrite = TRUE)
usethis::use_data(bloco8_graficos, overwrite = TRUE)
usethis::use_data(bloco8_grafico_evitaveis_neonatal, overwrite = TRUE)
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





