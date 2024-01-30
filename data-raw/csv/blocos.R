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
aux_r_saude <- readODS::read_ods("data-raw/ods/regioes_macrorregioes.ods") |>
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
bloco1_aux <- read.csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2021.csv") |>
  janitor::clean_names()

bloco2_aux <- read.csv("data-raw/csv/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2021.csv") |>
  janitor::clean_names()

bloco3_aux <- read.csv("data-raw/csv/indicadores_bloco3_assistencia_pre-natal_2012-2021.csv") |>
  janitor::clean_names()

bloco4_aux <- read.csv("data-raw/csv/indicadores_bloco4_assistencia_ao_parto_2012-2022.csv") |>
  janitor::clean_names()

bloco4_deslocamento_muni_aux <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_municipio_2012-2020.csv") |>
  janitor::clean_names()

bloco4_deslocamento_muni_aux$km_partos_fora_uf <- as.numeric(bloco4_deslocamento_muni_aux$km_partos_fora_uf)
bloco4_deslocamento_muni_aux$km_partos_fora_uf_alta_complexidade <- as.numeric(bloco4_deslocamento_muni_aux$km_partos_fora_uf_alta_complexidade)
bloco4_deslocamento_muni_aux$km_partos_fora_uf_baixa_complexidade <- as.numeric(bloco4_deslocamento_muni_aux$km_partos_fora_uf_baixa_complexidade)

bloco4_deslocamento_uf_aux <- read.csv("data-raw/csv/indicadores_bloco4_deslocamento_parto_UF_2012-2020.csv") |>
  janitor::clean_names() |>
  dplyr::select(!uf) |>
  dplyr::rename(uf = nome) |>
  dplyr::mutate(
    uf = sub('.', '', uf)
  )

bloco4_deslocamento_uf_aux$uf[which(bloco4_deslocamento_uf_aux$uf == "rasil")] <- "Brasil"
bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao <- as.numeric(bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao)
bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao_alta_complexidade <- as.numeric(bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao_alta_complexidade)
bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao_baixa_complexidade <- as.numeric(bloco4_deslocamento_uf_aux$km_partos_fora_macrorregiao_baixa_complexidade)

bloco5_aux <- read.csv("data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012-2021.csv") |>
  janitor::clean_names()

asfixia_aux <- read.csv("data-raw/csv/asfixia_2012_2021.csv", sep = ';') |>
  janitor::clean_names()

malformacao_aux <- read.csv("data-raw/csv/malformacao_2012_2021.csv", sep = ';') |>
  janitor::clean_names() |>
  dplyr::arrange(codmunres, ano) |>
  dplyr::filter(codmunres %in% aux_municipios$codmunres)

bloco6_mortalidade_aux <- read.csv("data-raw/csv/indicadores_bloco6_mortalidade_materna_2012-2021.csv") |>
  dplyr::select(!c(uf, municipio, regiao))

bloco6_morbidade_aux <- read.csv("data-raw/csv/indicadores_bloco6_morbidade_materna_2012-2020.csv", sep = ";") |>
  janitor::clean_names()

bloco6_aux <- dplyr::left_join(bloco6_mortalidade_aux, bloco6_morbidade_aux, by = c("ano", "codmunres"))

bloco7_neonatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_neonatal_2012-2021.csv", sep=";") |>
  dplyr::select(!c(uf, municipio, regiao))

bloco7_fetal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_fetal_2012-2021.csv") |>
  dplyr::select(!(nascidos)
  ) |>
  dplyr::select(!c(uf, municipio, regiao))

bloco7_perinatal_aux <- read.csv("data-raw/csv/indicadores_bloco7_mortalidade_perinatal_2012-2021.csv") |>
  dplyr::select(!c(uf, municipio, regiao))
bloco7_perinatal_aux$codmunres <- as.numeric(bloco7_perinatal_aux$codmunres)

juncao_bloco7_aux <- dplyr::left_join(bloco7_neonatal_aux, bloco7_fetal_aux, by = c("ano", "codmunres"))
bloco7_aux <- dplyr::left_join(juncao_bloco7_aux, bloco7_perinatal_aux, by = c("ano", "codmunres"))

bloco8_materno_garbage_aux <- read.csv("data-raw/csv/materno_garbage_2019-2022.csv") |>
  janitor::clean_names() |>
  dplyr::select(!c(uf, municipio, regiao))

bloco8_fetal_garbage_aux <- read.csv("data-raw/csv/fetais_garbage_2019-2022.csv") |>
  janitor::clean_names() |>
  dplyr::select(!c(uf, municipio, regiao))

bloco8_neonat_garbage_aux <- read.csv("data-raw/csv/neonat_garbage_2019-2022.csv") |>
  janitor::clean_names() |>
  dplyr::select(!c(uf, municipio, regiao))

bloco8_fetal_causas_aux <- read.csv("data-raw/csv/fetais_causas_2019-2022.csv") |>
  janitor::clean_names() |>
  dplyr::select(!c(uf, municipio, regiao))

bloco8_neonat_causas_aux <- read.csv("data-raw/csv/neonat_causas_2019-2022.csv") |>
  janitor::clean_names() |>
  dplyr::select(!c(uf, municipio, regiao))

bloco8_fetal_evitaveis_aux <- read.csv("data-raw/csv/fetais_evitaveis_2019-2022.csv") |>
  janitor::clean_names() |>
  dplyr::select(!c(uf, municipio, regiao))

bloco8_neonat_evitaveis_aux <- read.csv("data-raw/csv/neonat_evitaveis_2019-2022.csv") |>
  janitor::clean_names() |>
  dplyr::select(!c(uf, municipio, regiao))


###VER QUAIS COLUNAS VOCÊS PRECISAM TIRAR AQUI; NO ARQUIVO DE 2012 A 2020, ESTOU TIRANDO A COLUNA 'X' E A COLUNA 'UF'
base_incompletude_sinasc_aux <- read.csv2("data-raw/csv/incompletude_SINASC_2012-2021.csv", sep = ",")[, -1] |>
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

bloco5 <- dplyr::left_join(bloco5_aux, aux_municipios, by = "codmunres")
bloco5 <- bloco5 |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco5) == "ano") + 1):(which(names(bloco5) == "municipio") - 1)
  )

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

asfixia <- dplyr::left_join(asfixia_aux, aux_municipios, by = "codmunres")
asfixia <- asfixia |>
  dplyr::filter(codmunres %in% tabela_aux_municipios$codmunres) |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(asfixia) == "ano") + 1):(which(names(asfixia) == "municipio") - 1)
  )

malformacao <- dplyr::left_join(malformacao_aux, aux_municipios, by = "codmunres")

malformacao <- malformacao |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(malformacao) == "ano") + 1):(which(names(malformacao) == "municipio") - 1)
  )

bloco8_materno_garbage_aux <- dplyr::left_join(bloco8_materno_garbage_aux, aux_municipios, by = "codmunres")
bloco8_materno_garbage <- bloco8_materno_garbage_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_materno_garbage_aux) == "ano") + 1):(which(names(bloco8_materno_garbage_aux) == "municipio") - 1)
  )

bloco8_fetal_garbage_aux <- dplyr::left_join(bloco8_fetal_garbage_aux, aux_municipios, by = "codmunres")
bloco8_fetal_garbage <- bloco8_fetal_garbage_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_fetal_garbage_aux) == "ano") + 1):(which(names(bloco8_fetal_garbage_aux) == "municipio") - 1)
  )

bloco8_neonat_garbage_aux <- dplyr::left_join(bloco8_neonat_garbage_aux, aux_municipios, by = "codmunres")
bloco8_neonat_garbage <- bloco8_neonat_garbage_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_neonat_garbage_aux) == "ano") + 1):(which(names(bloco8_neonat_garbage_aux) == "municipio") - 1)
  )

bloco8_fetal_causas_aux <- dplyr::left_join(bloco8_fetal_causas_aux, aux_municipios, by = "codmunres")
bloco8_fetal_causas <- bloco8_fetal_causas_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_fetal_causas_aux) == "ano") + 1):(which(names(bloco8_fetal_causas_aux) == "municipio") - 1)
  )

bloco8_neonat_causas_aux <- dplyr::left_join(bloco8_neonat_causas_aux, aux_municipios, by = "codmunres")
bloco8_neonat_causas <- bloco8_neonat_causas_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_neonat_causas_aux) == "ano") + 1):(which(names(bloco8_neonat_causas_aux) == "municipio") - 1)
  )

bloco8_fetal_evitaveis_aux <- dplyr::left_join(bloco8_fetal_evitaveis_aux, aux_municipios, by = "codmunres")
bloco8_fetal_evitaveis <- bloco8_fetal_evitaveis_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_fetal_evitaveis_aux) == "ano") + 1):(which(names(bloco8_fetal_evitaveis_aux) == "municipio") - 1)
  )

bloco8_neonat_evitaveis_aux <- dplyr::left_join(bloco8_neonat_evitaveis_aux, aux_municipios, by = "codmunres")
bloco8_neonat_evitaveis <- bloco8_neonat_evitaveis_aux |>
  dplyr::select(
    ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    (which(names(bloco8_neonat_evitaveis_aux) == "ano") + 1):(which(names(bloco8_neonat_evitaveis_aux) == "municipio") - 1)
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

#Criando uma tabela contendo informações sobre os indicadores
tabela_indicadores <- data.frame(
  nome_abreviado = c(
    "porc_nvm_menor_que_20_anos",
    "porc_nvm_entre_20_e_34_anos",
    "porc_nvm_maior_que_34_anos",
    "porc_nvm_com_cor_da_pele_branca",
    "porc_nvm_com_cor_da_pele_preta",
    "porc_nvm_com_cor_da_pele_parda",
    "porc_nvm_com_cor_da_pele_amarela",
    "porc_nvm_indigenas",
    "porc_nvm_com_escolaridade_ate_3",
    "porc_nvm_com_escolaridade_de_4_a_7",
    "porc_nvm_com_escolaridade_de_8_a_11",
    "porc_nvm_com_escolaridade_acima_de_11",
    "porc_dependentes_sus",
    "porc_cobertura_esf",  #Fim do bloco 1
    "porc_menor20",
    "porc_mais_3pt",
    "tx_abortos_mil_mulheres_valor_medio",
    "tx_abortos_cem_nascidos_vivos_valor_medio",  #Fim do bloco 2
    "cobertura_pre_natal",
    "porc_inicio_prec",
    "porc_7",
    "porc_sc",  #Fim do bloco 3
    "prop_tx_cesariana_geral",
    "prop_robson1_tx_cesariana",
    "prop_robson2_tx_cesariana",
    "prop_robson3_tx_cesariana",
    "prop_robson4_tx_cesariana",
    "prop_robson5_tx_cesariana",
    "prop_robson6_a_9_tx_cesariana",
    "prop_robson10_tx_cesariana",
    "contrib_robson1_tx_cesariana",
    "contrib_robson2_tx_cesariana",
    "contrib_robson3_tx_cesariana",
    "contrib_robson4_tx_cesariana",
    "contrib_robson5_tx_cesariana",
    "contrib_robson6_a_9_tx_cesariana",
    "contrib_robson10_tx_cesariana",
    "prop_nasc_robson1",
    "prop_nasc_robson2",
    "prop_nasc_robson3",
    "prop_nasc_robson4",
    "prop_nasc_robson5",
    "prop_nasc_robson6_a_9",
    "prop_nasc_robson10", #Fim do bloco 4 (grupos de Robson)
    "prop_partos_municipio_res",
    "prop_partos_fora_municipio_res",
    "prop_partos_rsaude_res",
    "prop_partos_macro_rsaude_res",
    "prop_partos_fora_macro_rsaude_res",
    "prop_partos_fora_uf_res",
    "km_partos_fora_municipio",
    "km_partos_na_regiao",
    "km_partos_na_macrorregiao",
    "km_partos_fora_macrorregiao",
    "km_partos_fora_uf",  #Fim do bloco 4 (deslocamento)
    "porc_baixo_peso",
    "porc_premat",
    "porc_termo_precoce",  #Fim do bloco 5
    "obitos_mat_totais",
    "rmm",
    "prop_obitos_diretos",
    "prop_obitos_aborto",
    "prop_obitos_hipertens",
    "prop_obitos_hemo",
    "prop_obitos_infec",  #Fim do bloco 6 (mortalidade)
    "prop_mmg_int_publicas",
    "prop_mmg_hipertensao",
    "prop_mmg_hemorragia",
    "prop_mmg_infeccao",
    "prop_mmg_uti",
    "prop_mmg_tmp",
    "prop_mmg_transfusao",
    "prop_mmg_cirurgia", #Fim do bloco 6 (morbidade)
    "mort_neonat",
    "mort_neonat_precoc",
    "mort_neonat_tardia",
    "mort_neonat_menos1500",
    "mort_neonat_precoc_menos1500",
    "mort_neonat_tardia_menos1500",
    "mort_neonat_1500_1999",
    "mort_neonat_precoc_1500_1999",
    "mort_neonat_tardia_1500_1999",
    "mort_neonat_2000_2499",
    "mort_neonat_precoc_2000_2499",
    "mort_neonat_tardia_2000_2499",
    "mort_neonat_mais2500",
    "mort_neonat_precoc_mais2500",
    "mort_neonat_tardia_mais2500", #Fim do bloco 7 (neonatal)
    "obitos_fetais_mais22sem",
    "mort_fetal" # Fim do bloco 7 (fetal)
  ),
  indicador = c(
    "Porcentagem de nascidos vivos de mães com idade inferior a 20 anos",
    "Porcentagem de nascidos vivos de mães com idade entre 20 a 34 anos",
    "Porcentagem de nascidos vivos de mães com idade de 35 ou mais anos",
    "Porcentagem de nascidos vivos de mães de raça/cor branca",
    "Porcentagem de nascidos vivos de mães de raça/cor preta",
    "Porcentagem de nascidos vivos de mães de raça/cor parda",
    "Porcentagem de nascidos vivos de mães de raça/cor amarela",
    "Porcentagem de nascidos vivos de mães de raça/cor indígena",
    "Porcentagem de nascidos vivos de mães com menos de 4 anos de estudo",
    "Porcentagem de nascidos vivos de mães com 4 a 7 anos de estudo",
    "Porcentagem de nascidos vivos de mães com 8 a 11 anos de estudo",
    "Porcentagem de nascidos vivos de mães com mais de 11 anos de estudo",
    "Porcentagem de mulheres com 10 a 49 anos usuárias exclusivas do SUS",
    "Cobertura populacional com equipes de Saúde da Família",  #Fim do bloco 1
    "Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil)",
    "Porcentagem de mulheres com mais de 3 partos anteriores",
    "Taxa de abortos inseguros por mil mulheres em idade fértil",
    "Razão de abortos inseguros por 100 nascidos vivos", #Fim do bloco 2
    "Cobertura de assistência pré-natal",
    "Porcentagem de mulheres com início precoce do pré-natal",
    "Porcentagem de mulheres com mais de 7 consultas de pré-natal",
    "Incidência de sífilis congênita",  #Fim do bloco 3
    "Porcentagem de nascidos vivos por cesariana",
    "Porcentagem de cesarianas no grupo 1 de Robson",
    "Porcentagem de cesarianas no grupo 2 de Robson",
    "Porcentagem de cesarianas no grupo 3 de Robson",
    "Porcentagem de cesarianas no grupo 4 de Robson",
    "Porcentagem de cesarianas no grupo 5 de Robson",
    "Porcentagem de cesarianas nos grupos 6 a 9 de Robson",
    "Porcentagem de cesarianas no grupo 10 de Robson",
    "Contribuição relativa do grupo 1 de Robson para a taxa global de cesariana",
    "Contribuição relativa do grupo 2 de Robson para taxa global de cesariana",
    "Contribuição relativa do grupo 3 de Robson para taxa global de cesariana",
    "Contribuição relativa do grupo 4 de Robson para taxa global de cesariana",
    "Contribuição relativa do grupo 5 de Robson para taxa global de cesariana",
    "Contribuição relativa dos grupos 6 a 9 de Robson para taxa global de cesariana",
    "Contribuição relativa do grupo 10 de Robson para taxa global de cesariana",
    "Porcentagem de nascidos vivos do grupo 1 de Robson",
    "Porcentagem de nascidos vivos do grupo 2 de Robson",
    "Porcentagem de nascidos vivos do grupo 3 de Robson",
    "Porcentagem de nascidos vivos do grupo 4 de Robson",
    "Porcentagem de nascidos vivos do grupo 5 de Robson",
    "Porcentagem de nascidos vivos dos grupos 6 a 9 de Robson",
    "Porcentagem de nascidos vivos do grupo 10 de Robson", #Fim do bloco 4 (grupos de Robson)
    "Porcentagem de partos ocorridos no município de residência da mulher",
    "Porcentagem do total de partos ocorridos fora do município de residência da mulher",
    "Porcentagem de partos ocorridos fora do município, mas na microrregião de saúde de residência da mulher",
    "Porcentagem de partos ocorridos fora da microrregião de saúde, mas na macrorregião de saúde de residência da mulher",
    "Porcentagem de partos ocorridos fora da macrorregião de saúde, mas dentro da UF de residência da mulher",
    "Porcentagem de partos ocorridos fora da UF de residência da mulher",
    "Medianas de deslocamento para partos ocorridos fora do municipio de residência da mulher",
    "Medianas de deslocamento para partos ocorridos fora do município, mas na microrregião de saúde de residência da mulher",
    "Medianas de deslocamento para partos ocorridos fora da microrregião de saúde, mas na macrorregião de saúde de residência da mulher",
    "Medianas de deslocamento para partos ocorridos fora da macrorregião de saúde, mas na UF de residência da mulher",
    "Medianas de deslocamento do total de partos ocorridos fora da UF de residência da mulher",  #Fim do bloco 4 (deslocamento)
    "Porcentagem de nascidos vivos com baixo peso ao nascer",
    "Porcentagem de nascidos vivos prematuros",
    "Porcentagem de nascidos vivos a termo precoce",  #Fim do bloco 5
    "Número absoluto de óbitos maternos",
    "Razão de mortalidade materna",
    "Porcentagem de óbitos maternos por causas obstétricas diretas",
    "Porcentagem de óbitos maternos diretos por aborto",
    "Porcentagem de óbitos maternos diretos por hipertensão",
    "Porcentagem de óbitos maternos diretos por hemorragia",
    "Porcentagem de óbitos maternos diretos por infecção puerperal",  #Fim do bloco 6
    "Porcentagem de casos de morbidade materna grave em internações obstétricas públicas",
    "Porcentagem de casos de morbidade materna grave por hipertensão",
    "Porcentagem de casos de morbidade materna grave por hemorragia",
    "Porcentagem de casos de morbidade materna grave por infecção",
    "Porcentagem de casos de morbidade materna grave com internação em UTI",
    "Porcentagem de casos de morbidade materna grave com Tempo de Permanência Prolongada",
    "Porcentagem de casos de morbidade materna grave com transfusão sanguínea",
    "Porcentagem de casos de morbidade materna grave com intervenções cirúrgicas",
    "Mortalidade neonatal",
    "Mortalidade neonatal precoce",
    "Mortalidade neonatal tardia",
    "Mortalidade neonatal para peso ao nascer menor que 1500g",
    "Mortalidade neonatal precoce para peso ao nascer menor que 1500g",
    "Mortalidade neonatal tardia para peso ao nascer menor que 1500g",
    "Mortalidade neonatal para peso ao nascer de 1500g a 1999g",
    "Mortalidade neonatal precoce para peso ao nascer de 1500g a 1999g",
    "Mortalidade neonatal tardia para peso ao nascer de 1500g a 1999g",
    "Mortalidade neonatal para peso ao nascer de 2000g a 2499g",
    "Mortalidade neonatal precoce para peso ao nascer de 2000g a 2499g",
    "Mortalidade neonatal tardia para peso ao nascer de 2000g a 2499g",
    "Mortalidade neonatal para peso ao nascer maior ou igual a 2500g",
    "Mortalidade neonatal precoce para peso ao nascer maior ou igual a 2500g",
    "Mortalidade neonatal tardia para peso ao nascer maior ou igual a 2500g",
    "Número de óbitos fetais",
    "Taxa de mortalidade fetal"

  ),
  bloco = c(
    rep("bloco1", times = 14),
    rep("bloco2", times = 4),
    rep("bloco3", times = 4),
    rep("bloco4", times = 22),
    rep("bloco4_deslocamento", times = 11),
    rep("bloco5", times = 3),
    rep("bloco6", times = 7),
    rep("bloco6_morbidade", times = 8),
    rep("bloco7_neonatal", times = 15),
    rep("bloco7_fetal", times = 2)
  ),
  numerador = c(
    "nvm_menor_que_20_anos",
    "nvm_entre_20_e_34_anos",
    "nvm_maior_que_34_anos",
    "nvm_com_cor_da_pele_branca",
    "nvm_com_cor_da_pele_preta",
    "nvm_com_cor_da_pele_parda",
    "nvm_com_cor_da_pele_amarela",
    "nvm_indigenas",
    "nvm_com_escolaridade_ate_3",
    "nvm_com_escolaridade_de_4_a_7",
    "nvm_com_escolaridade_de_8_a_11",
    "nvm_com_escolaridade_acima_de_11",
    "exceção",
    "exceção",  #Fim do bloco 1
    "nvm_menor_que_20",
    "mulheres_com_mais_de_tres_partos_anteriores",
    "exceção",
    "exceção",  #Fim do bloco 2
    "mulheres_com_pelo_menos_uma_consulta_prenatal",
    "mulheres_com_inicio_precoce_do_prenatal",
    "mulheres_com_mais_de_sete_consultas_prenatal",
    "casos_sc",  #Fim do bloco 3
    "mulheres_com_parto_cesariana",
    "total_cesariana_grupo_robson_1",
    "total_cesariana_grupo_robson_2",
    "total_cesariana_grupo_robson_3",
    "total_cesariana_grupo_robson_4",
    "total_cesariana_grupo_robson_5",
    "total_cesariana_grupo_robson_6_ao_9",
    "total_cesariana_grupo_robson_10",
    "total_cesariana_grupo_robson_1",
    "total_cesariana_grupo_robson_2",
    "total_cesariana_grupo_robson_3",
    "total_cesariana_grupo_robson_4",
    "total_cesariana_grupo_robson_5",
    "total_cesariana_grupo_robson_6_ao_9",
    "total_cesariana_grupo_robson_10",
    "mulheres_dentro_do_grupo_de_robson_1",
    "mulheres_dentro_do_grupo_de_robson_2",
    "mulheres_dentro_do_grupo_de_robson_3",
    "mulheres_dentro_do_grupo_de_robson_4",
    "mulheres_dentro_do_grupo_de_robson_5",
    "mulheres_dentro_do_grupo_de_robson_6_ao_9",
    "mulheres_dentro_do_grupo_de_robson_10", #Fim do bloco 4 (grupos de Robson)
    "local",
    "nao_local",
    "dentro_regiao_saude",
    "dentro_macrorregiao_saude",
    "fora_macrorregiao_saude",
    "outra_uf",
    rep("exceção", times = 5),  #Fim do bloco 4 (deslocamento)
    "nascidos_vivos_com_baixo_peso",
    "nascidos_vivos_prematuros",
    "nascidos_vivos_termo_precoce",  #Fim do bloco 5
    "exceção",
    "obitos_mat_totais",
    "obitos_mat_diretos",
    "obitos_mat_aborto",
    "obitos_mat_hipertensao",
    "obitos_mat_hemorragia",
    "obitos_mat_infec_puerperal",  #Fim do bloco 6 (mortalidade)
    "casos_mmg",
    "casos_mmg_hipertensao",
    "casos_mmg_hemorragia",
    "casos_mmg_infeccoes",
    "casos_mmg_uti",
    "casos_mmg_tmp",
    "casos_mmg_transfusao",
    "casos_mmg_cirurgia", #fim do bloco 6 (morbidade)
    "obitos_27dias",
    "obitos_6dias",
    "obitos_7_27dias",
    "obitos_27dias_menos1500",
    "obitos_6dias_menos1500",
    "obitos_7_27dias_menos1500",
    "obitos_27dias_1500-1999",
    "obitos_6dias_1500_1999",
    "obitos_7_27dias_1500_1999",
    "obitos_27dias_2000_2499",
    "obitos_6dias_2000_2499",
    "obitos_7_27dias_2000_2499",
    "obitos_27dias_mais2500",
    "obitos_6dias_mais2500",
    "obitos_7_27dias_mais2500",
    "obitos_fetais_mais22sem",
    "obitos_fetais_mais22sem" #fim do bloco 7
  ),
  denominador = c(
    rep("total_de_nascidos_vivos", times = 12),
    "populacao_feminina_10_a_49",
    "populacao_total",  #Fim do bloco 1
    "pop_feminina_10_a_19",
    "total_de_nascidos_vivos",
    rep("exceção", times = 2),  #Fim do bloco 2
    rep("total_de_nascidos_vivos", times = 4),  #Fim do bloco 3
    rep("total_de_nascidos_vivos", times = 1),
    "mulheres_dentro_do_grupo_de_robson_1",
    "mulheres_dentro_do_grupo_de_robson_2",
    "mulheres_dentro_do_grupo_de_robson_3",
    "mulheres_dentro_do_grupo_de_robson_4",
    "mulheres_dentro_do_grupo_de_robson_5",
    "mulheres_dentro_do_grupo_de_robson_6_ao_9",
    "mulheres_dentro_do_grupo_de_robson_10",
    rep("mulheres_com_parto_cesariana", times = 7),
    rep("total_de_nascidos_vivos", times = 7), #Fim do bloco 4 (grupos de Robson)
    rep("destino_total", times = 6),
    rep("excecao", times = 5),  #Fim do bloco 4 (deslocamento)
    rep("total_de_nascidos_vivos", times = 3),  #Fim do bloco 5,
    "exceção",
    "nascidos",
    "obitos_mat_totais",
    rep("obitos_mat_diretos", times = 4),  #Fim do bloco 6 (mortalidade)
    "total_internacoes",
    rep("casos_mmg", times = 7), #Fim do bloco 6 (morbidade)
    rep("nascidos", times=15), #Fim do bloco 7 (neonatal)
    "exceção",
    "nascidos + obitos_fetais_mais22sem"
  ),
  fator = c(
    rep(100, times = 12),
    rep(100, times = 2), #Fim do bloco 1
    1000,
    100,
    1000,
    100,  #Fim do bloco 2
    rep(100, times = 3),
    1000,  #Fim do bloco 3
    rep(100, times = 22),  #Fim do bloco 4 (grupos de Robson)
    rep(100, times = 6),
    rep(1, times = 5),  #Fim do bloco 4 (deslocamento)
    rep(100, times = 3),  #Fim do bloco 5
    1,
    100000,
    rep(100, times = 5),  #Fim do bloco 6 (mortalidade)
    rep(100, times = 8),  #Fim do bloco 6 (morbidade),
    rep(1000, times=15), #Fim do bloco 7 (neonatal)
    1,
    1000 #Fim do bloco 7 (fetal)
  ),
  tipo_do_indicador = c(
    rep("porcentagem", times = 12),
    rep("porcentagem", times = 2), #Fim do bloco 1
    "taxa",
    "porcentagem",
    "taxa",
    "taxa",  #Fim do bloco 2
    rep("porcentagem", times = 3),
    "taxa",  #Fim do bloco 3
    rep("porcentagem", times = 22),  #Fim do bloco 4 (grupos de Robson)
    rep("porcentagem", times = 6),
    rep("absoluto", times = 5),  #Fim do bloco 4 (deslocamento)
    rep("porcentagem", times = 3),  #Fim do bloco 5
    "absoluto",
    "taxa",
    rep("porcentagem", times = 5),  #Fim do bloco 6 (mortalidade)
    rep("porcentagem", times = 8), #Fim do bloco 6 (morbidade)
    rep("taxa", times=15), #Fim do bloco 7 (neonatal)
    "absoluto",
    "taxa"  #Fim do bloco 7 (fetal)
  ),
  referencia = c(
    rep("Nacional", times = 13),
    95,  #Fim do bloco 1
    30,
    "Nacional",
    "Nacional",
    "Nacional", #Fim do bloco 2
    rep(95, times = 3),
    0.5,  #Fim do bloco 3
    15,
    10,
    20,
    3,
    15,
    50,
    "Nacional",
    30,
    rep("Nacional", times = 14),  #Fim do bloco 4 (grupos de Robson)
    rep("Nacional", times = 6),
    rep("exceção", times = 5),  #Fim do bloco 4 (deslocamento)
    "exceção",
    10,
    "20",  #Fim do bloco 5
    "sem referência",
    30,
    rep("Nacional", times = 5),
    rep("Nacional", times = 8), #Fim do bloco 6
    rep("Nacional", times=17) #Fim do bloco 7
  ),
  descricao_referencia = c(
    rep("média nacional", times = 13),
    "meta ODS",  #Fim do bloco 1
    "países desenvolvidos",
    rep("média nacional", times = 3), #Fim do bloco 2
    rep("recomendações OMS", times = 3),
    "meta OMS",  #Fim do bloco 3
    rep("meta OMS", times = 6),
    "média nacional",
    "meta OMS",
    rep("média nacional", times = 14),  #Fim do bloco 4 (grupos de Robson)
    rep("média nacional", times = 6),
    rep("sem referência", times = 5),  #Fim do bloco 4 (deslocamento)
    "meta de redução global",
    rep("países desenvolvidos", times = 2),  #Fim do bloco 5
    "sem referência",
    "Meta ODS",
    rep("média nacional", times = 5), #Fim do bloco 6
    rep("média nacional", times = 8),
    rep("média nacional", times = 17) #Fim do bloco 7
  ),
  nome_numerador = c(
    "Nascidos vivos de mães com menos de 20 anos",
    "Nascidos vivos de mães com entre 20 e 34 anos",
    "Nascidos vivos de mães com mais de 34 anos",
    "Nascidos vivos de mães de raça/cor branca",
    "Nascidos vivos de mães de raça/cor preta",
    "Nascidos vivos de mães de raça/cor parda",
    "Nascidos vivos de mães de raça/cor amarela",
    "Nascidos vivos de mães de raça/cor indígena",
    "Nascidos vivos de mães com até 3 anos de estudo",
    "Nascidos vivos de mães com 4 a 7 anos de estudo",
    "Nascidos vivos de mães com 8 a 11 anos de estudo",
    "Nascidos vivos de mães com 12 a 29 anos de estudo",
    "População feminina sem plano de saúde",
    "Média da população atendida por equipes de Saúde da Família",  #Fim do bloco 1
    "Nascidos vivos de mães com menos de 20 anos",
    "Mulheres com mais de 3 partos anteriores",
    rep("Número de internações por aborto corrigido", times = 2),  #Fim do bloco 2
    "Mulheres com pelo menos uma consulta pré-natal",
    "Mulheres com início precoce do pré-natal",
    "Mulheres com mais de sete consultas pré-natal",
    "Casos de sífilis congênita",  #Fim do bloco 3
    "Nascidos vivos por cesariana",
    "Nascidos vivos por cesariana no grupo 1 de Robson",
    "Nascidos vivos por cesariana no grupo 2 de Robson",
    "Nascidos vivos por cesariana no grupo 3 de Robson",
    "Nascidos vivos por cesariana no grupo 4 de Robson",
    "Nascidos vivos por cesariana no grupo 5 de Robson",
    "Nascidos vivos por cesariana nos grupos 6 a 9 de Robson",
    "Nascidos vivos por cesariana no grupo 10 de Robson",
    "Nascidos vivos por cesariana no grupo 1 de Robson",
    "Nascidos vivos por cesariana no grupo 2 de Robson",
    "Nascidos vivos por cesariana no grupo 3 de Robson",
    "Nascidos vivos por cesariana no grupo 4 de Robson",
    "Nascidos vivos por cesariana no grupo 5 de Robson",
    "Nascidos vivos por cesariana nos grupos 6 a 9 de Robson",
    "Nascidos vivos por cesariana no grupo 10 de Robson",
    "Nascidos vivos do grupo 1 de Robson",
    "Nascidos vivos do grupo 2 de Robson",
    "Nascidos vivos do grupo 3 de Robson",
    "Nascidos vivos do grupo 4 de Robson",
    "Nascidos vivos do grupo 5 de Robson",
    "Nascidos vivos dos grupos 6 a 9 de Robson",
    "Nascidos vivos do grupo 10 de Robson", #Fim do bloco 4 (grupos de Robson)
    "Nascidos vivos ocorridos no município de residência da mãe",
    "Nascidos vivos ocorridos fora do município de residência da mãe",
    "Nascidos vivos ocorridos na microrregião de saúde de residência da mãe",
    "Nascidos vivos ocorridos na macrorregião de residência da mãe",
    "Nascidos vivos ocorridos fora da macrorregião de residência da mãe",
    "Nascidos vivos ocorridos fora da UF de residência da mãe",
    "Mediana de deslocamento do total de partos ocorridos fora do município de residência da mulher",
    "Mediana de deslocamento do total de partos ocorridos na microrregião de saúde de residência da mulher",
    "Mediana de deslocamento do total de partos na macrorregião de saúde de residência da mulher",
    "Mediana de deslocamento do total de partos ocorridos fora da macrorregião de saúde de residência da mulher",
    "Mediana de deslocamento do total de partos ocorridos fora da UF de residência da mulher",  #Fim do bloco 4 (deslocamento)
    "Nascidos vivos com baixo peso",
    "Nascidos vivos prematuros",
    "Nascidos vivos termo precoce",  #Fim do bloco 5
    "Óbitos maternos",
    "Óbitos maternos",
    "Óbitos maternos diretos",
    "Óbitos maternos por aborto",
    "Óbitos maternos por hipertensão",
    "Óbitos maternos por hemorragia",
    "Óbitos maternos por infecção puerperal",  #Fim do bloco 6 (mortalidade)
    "Casos de morbidade materna grave",
    "Casos de morbidade materna grave por hipertensão",
    "Casos de morbidade materna grave por hemorragia",
    "Casos de morbidade materna grave por infecção",
    "Casos de morbidade materna grave com internação em UTI",
    "Casos de morbidade materna grave com Tempo de Permanência Prolongada",
    "Casos de morbidade materna grave com transfusão sanguínea",
    "Casos de morbidade materna grave com intervenções cirúrgicas", #Fim do bloco 6 (morbidade)
    "Número de óbitos até 27 dias de vida",
    "Número de óbitos até 6 dias de vida",
    "Número de óbitos de 7 a 27 dias de vida",
    "Número de óbitos até 27 dias de vida com peso menor que 1500g",
    "Número de óbitos até 6 dias de vida com peso menor que 1500g",
    "Número de óbitos de 7 a 27 dias de vida com peso menor que 1500g",
    "Número de óbitos até 27 dias de vida com peso de 1500g a 1999g",
    "Número de óbitos até 6 dias de vida com peso de 1500g a 1999g",
    "Número de óbitos de 7 a 27 dias de vida com peso de 1500g a 1999g",
    "Número de óbitos até 27 dias de vida com peso de 2000g a 2499g",
    "Número de óbitos até 6 dias de vida com peso de 2000g a 2499g",
    "Número de óbitos de 7 a 27 dias de vida com peso de 2000g a 2499g",
    "Número de óbitos até 27 dias de vida com peso maior ou igual a 2500g",
    "Número de óbitos até 6 dias de vida com peso maior ou igual a 2500g",
    "Número de óbitos de 7 a 27 dias de vida com peso maior ou igual a 2500g",
    "Número total de óbitos fetais",
    "Número total de óbitos fetais"
  ),
  nome_denominador = c(
    rep("Total de nascidos vivos", times = 12),
    "População feminina entre 10 e 49 anos",
    "População total",  #Fim do bloco 1
    "População feminina entre 10 e 19 anos",
    rep("Total de nascidos vivos", times = 3),  #Fim do bloco 2
    rep("Total de nascidos vivos", times = 4),  #Fim do bloco 3
    rep("Total de nascidos vivos", times = 1),
    "Mulheres dentro do grupo 1 de Robson",
    "Mulheres dentro do grupo 2 de Robson",
    "Mulheres dentro do grupo 3 de Robson",
    "Mulheres dentro do grupo 4 de Robson",
    "Mulheres dentro do grupo 5 de Robson",
    "Mulheres dentro dos grupos 6 a 9 de Robson",
    "Mulheres dentro do grupo 10 de Robson",
    rep("Nascidos vivos por cesariana", times = 7),
    rep("Total de nascidos vivos", times = 7),  #Fim do bloco 4 (grupos de Robson)
    rep("Total de nascidos vivos", times = 6),
    rep("sem denominador", times = 5), #Fim do bloco 4 (deslocamento)
    rep("Total de nascidos vivos", times = 3),  #Fim do bloco 5,
    "sem denominador",
    "Total de nascidos vivos",
    "Óbitos maternos",
    rep("Óbitos maternos diretos", times = 4),  #Fim do bloco 6 (mortalidade)
    "Total de internações obstétricas",
    rep("Total de casos de morbidade materna grave", times = 7), #fim do bloco 6 (morbidade)
    rep("Total de nascidos vivos", times=15),
    "sem denominador",
    "Total de nascidos vivos + total de óbits fetais" # Fim do bloco 7
  ),
  num_indicadores_incompletude = c(
    rep(1, times = 12),
    rep(0, times = 2),   #Fim do bloco 1
    1,
    2,
    rep(0, times = 2),   #Fim do bloco 2
    rep(1, times = 3),
    0,  #Fim do bloco 3
    rep(1, times = 22),  #Fim do bloco 4 (grupos de Robson)
    rep(2, times = 11),  #Fim do bloco 4 (deslocamento)
    rep(1, times = 3),  #Fim do bloco 5
    rep(2, times = 7),  #Fim do bloco 6
    rep(0, times = 8),
    rep(0, times=17) #Bloco 7
  ),
  nome_incompletude1 = c(
    rep("Porcentagem da variável IDADEMAE, do SINASC, não preenchida, ignorada ou >55", times = 3),
    rep("Porcentagem da variável RACACOR, do SINASC, não preenchida ou ignorada", times = 5),
    rep("Porcentagem da variável ESCMAE, do SINASC, não preenchida ou ignorada", times = 4),
    rep("Sem incompletude", times = 2),  #Fim do bloco 1
    "Porcentagem da variável IDADEMAE, do SINASC, não preenchida, ignorada ou maior que 55 anos",
    "Porcentagem da variável QTDPARTNOR, do SINASC, não preenchida ou preenchida com 99",
    rep("Sem incompletude", times = 2),  #Fim do bloco 2
    "Porcentagem da variável CONSPRENAT, do SINASC, em branco",
    "Porcentagem da variável MESPRENAT, do SINASC, em branco",
    "Porcentagem da variável CONSPRENAT, do SINASC, em branco",
    "Sem incompletude", #Fim do bloco 3
    "Porcentagem da variável PARTO, do SINASC, em branco, ignorada ou igual a 9",
    rep("Porcentagem das variáveis PARTO, do SINASC, em branco ou ignorado (PARTO = 9) ou TPROBSON, do SINASC, em branco ou ignorado (TPROBSON = 11 ou 12)", times = 14),
    rep("Porcentagem da variável TPROBSON, do SINASC, em branco, ignorada ou igual a 11 ou 12", times = 7),  #Fim do bloco 4 (grupos de Robson)
    rep("Porcentagem de Declaração de Nascido Vivo sem CNES preenchido", times = 11),  #Fim do bloco 4 (deslocamento)
    "Porcentagem da variável PESO, do SINASC, em branco ou preenchida com 9999",
    "Porcentagem da variável GESTACAO, do SINASC, em branco, ignorada ou igual a 9",
    "Porcentagem da variável SEMAGESTAC, do SINASC, em branco ou sem informação", #Fim do bloco 5
    rep("Porcentagem de óbitos de mulheres em idade fértil investigados", times = 7), #Fim do bloco 6
    rep("Sem incompletude", times = 8),
    rep("-", times=17) #bloco 7
  ),
  nome_incompletude2 = c(
    rep("-", times = 12),
    rep("-", times = 2), #Fim do bloco 1
    "-",
    "Porcentagem da variável QTDPARTCES, do SINASC, não preenchida ou preenchida com 99",
    rep("-", times = 2),  #Fim do bloco 2
    rep("-", times = 4),  #Fim do bloco 3
    rep("-", times = 22),  #Fim do bloco 4 (grupos de Robson)
    rep("Porcentagem de Declaração de Nascido Vivo com CNES inválido", times = 11),  #Fim do bloco 4 (deslocamento)
    rep("-", times = 3),  #Fim do bloco 5
    rep("Porcentagem de óbitos maternos investigados", times = 7),  #Fim do bloco 6
    rep("-", times = 8),
    rep("-", times=17) #bloco 7
  ),
  numerador_incompletude1 = c(
    rep("idademae_incompletos", times = 3),
    rep("racacor_incompletos", times = 5),
    rep("escmae_incompletos", times = 4),
    rep("Sem incompletude", 2),  #Fim do bloco 1,
    "idademae_incompletos",
    "qtdpartces_incompletos",
    rep("Sem incompletude", times=2),  #Fim do bloco 2
    "consprenat_incompletos",
    "mesprenat_incompletos",
    "consprenat_incompletos",
    "Sem incompletude",  #Fim do bloco 3
    "parto_incompletos",
    rep("parto_tprobson_incompletos", times = 14),
    rep("tprobson_incompletos", times = 7),  #Fim do bloco 4 (grupos de Robson)
    rep("exceção", times = 11),  #Fim do bloco 4 (deslocamento)
    "peso_incompletos",
    "gestacao_incompletos",
    "semagestac_incompletos", #Fim do bloco 5
    rep("exceção", times = 7),  #Fim do bloco 6
    rep("Sem incompletude", times = 8),
    rep("-", times=17) #bloco 7
  ),
  numerador_incompletude2 = c(
    rep("-", times = 12),
    rep("-", times = 2),  #Fim do bloco 1
    "-",
    "qtdpartnor_incompletos",
    rep("-", times = 2),  #Fim do bloco 2
    rep("-", times = 4),  #Fim do bloco 3
    rep("-", times = 22),  #Fim do bloco 4 (grupos de Robson)
    rep("exceção", times = 11),  #Fim do bloco 4 (deslocamento)
    rep("-", times = 3),  #Fim do bloco 5
    rep("exceção", times = 7),  #Fim do bloco 6
    rep("-", times = 8),
    rep("-", times=17) #bloco 7
  ),
  denominador_incompletude1 = c(
    rep("idademae_totais", times = 3),
    rep("racacor_totais", times = 5),
    rep("escmae_totais", times = 4),
    rep("Sem incompletude", times = 2),  #Fim do bloco 1,
    "idademae_totais",
    "qtdpartces_totais",
    rep("Sem incompletude", times = 2),  #Fim do bloco 2
    "consprenat_totais",
    "mesprenat_totais",
    "consprenat_totais",
    "Sem incompletude",  #Fim do bloco 3
    "parto_totais",
    rep("parto_tprobson_totais", times = 14),
    rep("tprobson_totais", times = 7), #Fim do bloco 4 (grupos de Robson)
    rep("dn_hospital_id_fertil", times = 11),  #Fim do bloco 4 (deslocamento)
    "peso_totais",
    "gestacao_totais",
    "semagestac_totais", #Fim do bloco 5
    rep("total_obitos_mulher_idade_fertil", times = 7),  #Fim do bloco 6
    rep("Sem incompletude", times = 8) ,
    rep("-", times=17) #bloco 7
  ),
  denominador_incompletude2 = c(
    rep("-", times = 12),
    rep("-", times = 2),  #Fim do bloco 1
    "-",
    "qtdpartnor_totais",
    rep("-", times = 2),  #Fim do bloco 2
    rep("-", times = 4),  #Fim do bloco 3
    rep("-", times = 22),  #Fim do bloco 4 (grupos de Robson)
    rep("dn_hospital_id_fertil", times = 11),  #Fim do bloco 4 (deslocamento)
    rep("-", times = 3),  #Fim do bloco 5
    rep("total_obitos_maternos", times = 7),
    rep("-", times = 8) ,
    rep("-", times=17) #bloco 7
  ),
  fator_incompletude = c(
    rep(100, times = 73),
    rep(100, times=17) # bloco 7
  )
)


usethis::use_data(bloco1, overwrite = TRUE)
usethis::use_data(bloco2, overwrite = TRUE)
usethis::use_data(bloco3, overwrite = TRUE)
usethis::use_data(bloco4, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_muni, overwrite = TRUE)
usethis::use_data(bloco4_deslocamento_uf, overwrite = TRUE)
usethis::use_data(bloco5, overwrite = TRUE)
usethis::use_data(bloco6, overwrite = TRUE)
usethis::use_data(bloco7, overwrite = TRUE)
usethis::use_data(asfixia, overwrite = TRUE)
usethis::use_data(malformacao, overwrite = TRUE)
usethis::use_data(bloco8_materno_garbage, overwrite = TRUE)
usethis::use_data(bloco8_fetal_garbage, overwrite = TRUE)
usethis::use_data(bloco8_neonat_garbage, overwrite = TRUE)
usethis::use_data(bloco8_fetal_causas, overwrite = TRUE)
usethis::use_data(bloco8_neonat_causas, overwrite = TRUE)
usethis::use_data(bloco8_fetal_evitaveis, overwrite = TRUE)
usethis::use_data(bloco8_neonat_evitaveis, overwrite = TRUE)
usethis::use_data(base_incompletude, overwrite = TRUE)
usethis::use_data(tabela_aux_municipios, overwrite = TRUE)
usethis::use_data(municipios_choices, overwrite = TRUE)
usethis::use_data(estados_choices, overwrite = TRUE)
usethis::use_data(micro_r_saude_choices, overwrite = TRUE)
usethis::use_data(macro_r_saude_choices, overwrite = TRUE)
usethis::use_data(base_referencia_baixo_peso, overwrite = TRUE)
usethis::use_data(tabela_indicadores, overwrite = TRUE)  #Utilizada no nível 3
usethis::use_data(rmm_fator_de_correcao, overwrite = TRUE)
usethis::use_data(rmm_corrigida, overwrite = TRUE)





