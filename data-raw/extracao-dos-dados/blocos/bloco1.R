library(tidyverse)
library(httr)
library(janitor)
library(getPass)
library(repr)
library(data.table)
library(readr)
library(openxlsx)
library(tidyr)

# Criando alguns objetos auxiliares ---------------------------------------
## Criando um objeto que recebe os códigos dos municípios que utilizamos no painel
codigos_municipios <- read.csv("data-raw/extracao-dos-dados/blocos/databases_auxiliares/tabela_aux_municipios.csv") |>
  pull(codmunres) |>
  as.character()

## Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios, each = length(2012:2024)), ano = 2012:2024)

df_aux_municipios <- df_aux_municipios %>%
  mutate_if(is.character, as.numeric)

# Lendo o arquivo com os dados de 2012 a 2023, que utilizamos no painel original
df_bloco1_antigo <- read.csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2023.csv") |>
  clean_names() |>
  filter(ano <= 2022)

# Para os indicadores provenientes do SINASC ------------------------------
## Baixando os dados consolidados do SINASC de 2012 a 2022 e selecionando as variáveis de interesse

df_sinasc_consolidados <- fetch_datasus(
  year_start = 2012,
  year_end = 2022,
  vars = c("CODMUNRES", "DTNASC", "IDADEMAE", "RACACORMAE", "ESCMAE"),
           information_system = "SINASC"
  )

df_sinasc_consolidados <- df_sinasc_consolidados %>%
  mutate_if(is.character, as.numeric) %>%
  select(-'...1')

## Baixando os dados preliminares do SINASC de 2023 e selecionando as variáveis de interesse
df_sinasc_preliminares_2023 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv", sep = ";") |>
  select("CODMUNRES", "DTNASC", "IDADEMAE", "RACACORMAE", "ESCMAE")

## Baixando os dados preliminares do SINASC de 2024 e selecionando as variáveis de interesse
df_sinasc_preliminares_2024 <- fread("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN24.csv", sep = ";") |>
  select("CODMUNRES", "DTNASC", "IDADEMAE", "RACACORMAE", "ESCMAE")

## Juntando os dados consolidados com os dados preliminares
df_sinasc <- full_join(df_sinasc_consolidados, df_sinasc_preliminares_2023)

df_sinasc <- full_join(df_sinasc, df_sinasc_preliminares_2024)

df_sinasc <- df_sinasc |> clean_names()

## Transformando algumas variáveis e criando as variáveis necessárias p/ o cálculo dos indicadores

df_bloco1_sinasc <- df_sinasc |>
  filter(codmunres %in% codigos_municipios) %>%
  mutate(
    ano = as.numeric(substr(dtnasc, nchar(dtnasc) - 3, nchar(dtnasc))),
    nvm_menor_que_20_anos = if_else(idademae < 20, 1, 0, missing = 0),
    nvm_entre_20_e_34_anos = if_else(idademae >= 20 & idademae < 35, 1, 0, missing = 0),
    nvm_maior_que_34_anos = if_else(idademae >= 35 & idademae <= 55, 1, 0, missing = 0),
    nvm_com_cor_da_pele_branca = if_else(racacormae == 1, 1, 0, missing = 0),
    nvm_com_cor_da_pele_preta = if_else(racacormae == 2, 1, 0, missing = 0),
    nvm_com_cor_da_pele_parda = if_else(racacormae == 4, 1, 0, missing = 0),
    nvm_com_cor_da_pele_amarela = if_else(racacormae == 3, 1, 0, missing = 0),
    nvm_indigenas = if_else(racacormae == 5, 1, 0, missing = 0),
    nvm_com_escolaridade_ate_3 = if_else(escmae == 1 | escmae == 2, 1, 0, missing = 0),
    nvm_com_escolaridade_de_4_a_7 = if_else(escmae == 3, 1, 0, missing = 0),
    nvm_com_escolaridade_de_8_a_11 = if_else(escmae == 4, 1, 0, missing = 0),
    nvm_com_escolaridade_acima_de_11 = if_else(escmae == 5, 1, 0, missing = 0)
  ) %>%
  group_by(codmunres, ano) %>%
  summarise(
    total_de_nascidos_vivos = n(),
    nvm_menor_que_20_anos = sum(nvm_menor_que_20_anos),
    nvm_entre_20_e_34_anos = sum(nvm_entre_20_e_34_anos),
    nvm_maior_que_34_anos = sum(nvm_maior_que_34_anos),
    nvm_com_cor_da_pele_branca = sum(nvm_com_cor_da_pele_branca),
    nvm_com_cor_da_pele_preta = sum(nvm_com_cor_da_pele_preta),
    nvm_com_cor_da_pele_parda = sum(nvm_com_cor_da_pele_parda),
    nvm_com_cor_da_pele_amarela = sum(nvm_com_cor_da_pele_amarela),
    nvm_indigenas = sum(nvm_indigenas),
    nvm_com_escolaridade_ate_3 = sum(nvm_com_escolaridade_ate_3),
    nvm_com_escolaridade_de_4_a_7 = sum(nvm_com_escolaridade_de_4_a_7),
    nvm_com_escolaridade_de_8_a_11 = sum(nvm_com_escolaridade_de_8_a_11),
    nvm_com_escolaridade_acima_de_11 = sum(nvm_com_escolaridade_acima_de_11)
  ) %>%
  mutate_if(is.character, as.numeric)

# Incluindo dados de municipios
df_bloco1_sinasc <- left_join(df_aux_municipios, df_bloco1_sinasc)

# Todos os NA's transformam em 0

df_bloco1_sinasc[is.na(df_bloco1_sinasc)] <- 0

# Para os indicadores provenientes do Tabnet ------------------------------
## Observação: Tabnet foi atualizado até 2021.

# Criando a função que utiliza web scrapping para baixar dados de estimativas populacionais do Tabnet DATASUS
est_pop_tabnet <- function (linha = "Município", coluna = "Ano", conteudo = 1, periodo = 2012:2021, regiao = "Todas as categorias",
                            unidade_da_federacao = "Todas as categorias", sexo = "Feminino", faixa_etaria = c("10 a 14 anos", "15 a 19 anos", "20 a 29 anos", "30 a 39 anos", "40 a 49 anos"),
                            faixa_etaria_reajuste = "Todas as categorias")
{
  page <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/popsvsbr.def")

  linha.df <- data.frame(id = page |> rvest::html_elements("#L option") |>
                           rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#L option") |>
                           rvest::html_attr("value"))
  linha.df[] <- lapply(linha.df, as.character)
  coluna.df <- data.frame(id = page |> rvest::html_elements("#C option") |>
                            rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#C option") |>
                            rvest::html_attr("value"))
  coluna.df[] <- lapply(coluna.df, as.character)

  conteudo.df <- data.frame(id1 = 1, id2 = "População_residente", value = "População_residente")

  periodo.df <- data.frame(id = page |> rvest::html_elements("#A option") |>
                             rvest::html_text() |> as.numeric(), value = page |>
                             rvest::html_elements("#A option") |> rvest::html_attr("value"))
  regiao.df <- suppressWarnings(data.frame(id = page |> rvest::html_elements("#S1 option") |>
                                             rvest::html_text() |> readr::parse_number(), value = page |>
                                             rvest::html_elements("#S1 option") |> rvest::html_attr("value")))
  unidade_da_federacao.df <- suppressWarnings(data.frame(id = page |>
                                                           rvest::html_elements("#S2 option") |> rvest::html_text() |>
                                                           trimws(), value = page |> rvest::html_elements("#S2 option") |>
                                                           rvest::html_attr("value")))
  unidade_da_federacao.df[] <- lapply(unidade_da_federacao.df,
                                      as.character)
  sexo.df <- data.frame(id = page |> rvest::html_elements("#S16 option") |>
                          rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S16 option") |>
                          rvest::html_attr("value"))
  sexo.df[] <- lapply(sexo.df, as.character)
  faixa_etaria.df <- data.frame(id = page |> rvest::html_elements("#S17 option") |>
                                  rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S17 option") |>
                                  rvest::html_attr("value"))
  faixa_etaria.df[] <- lapply(faixa_etaria.df, as.character)
  faixa_etaria_reajuste.df <- data.frame(id = page |> rvest::html_elements("#S18 option") |>
                                           rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S18 option") |>
                                           rvest::html_attr("value"))
  faixa_etaria_reajuste.df[] <- lapply(faixa_etaria_reajuste.df, as.character)
  regiao.df$id[1] <- unidade_da_federacao.df$id[1] <- "Todas as categorias"
  sexo.df$id[1] <- faixa_etaria.df$id[1] <- faixa_etaria_reajuste.df$id[1] <- "Todas as categorias"
  if (linha != "Unidade da Federação") {
    if (!is.character(linha))
      stop("The 'linha' argument must be a character element")
    if (length(linha) != 1)
      stop("The 'linha' argument must have only one element")
    if (!(all(linha %in% linha.df$id))) {
      if (!(all(linha %in% linha.df$value))) {
        stop("The 'linha' argument is misspecified")
      }
    }
  }
  if (coluna != "Não ativa") {
    if (!is.character(coluna))
      stop("The 'coluna' argument must be a character element")
    if (length(coluna) != 1)
      stop("The 'coluna' argument must have only one element")
    if (!(all(coluna %in% coluna.df$id))) {
      if (!(all(coluna %in% coluna.df$value))) {
        stop("The 'coluna' argument is misspecified")
      }
    }
  }
  if (conteudo != 1 & conteudo != 2 & conteudo != 3) {
    if (is.numeric(conteudo))
      stop("The only numeric elements allowed are 1 or 2")
    if (length(conteudo) != 1)
      stop("The 'coluna' argument must have only one element")
    if (!(all(conteudo %in% conteudo.df$id2))) {
      if (!(all(conteudo %in% conteudo.df$value))) {
        stop("The 'conteudo' argument is misspecified")
      }
    }
  }
  if (periodo[1] != "last") {
    if (is.character(periodo)) {
      periodo <- as.numeric(periodo)
    }
    if (!(all(periodo %in% periodo.df$id)))
      stop("The 'periodo' argument is misspecified")
  }
  if (any(regiao != "Todas as categorias")) {
    regiao <- as.character(regiao)
    if (!(all(regiao %in% regiao.df$id)))
      stop("Some element in 'regiao' argument is wrong")
  }
  if (any(unidade_da_federacao != "Todas as categorias")) {
    unidade_da_federacao <- as.character(unidade_da_federacao)
    if (!(all(unidade_da_federacao %in% unidade_da_federacao.df$id)))
      stop("Some element in 'unidade_da_federacao' argument is wrong")
  }
  if (any(faixa_etaria != "Todas as categorias")) {
    if (!(all(faixa_etaria %in% faixa_etaria.df$id))) {
      faixa_etaria <- as.character(faixa_etaria)
      if (!(all(faixa_etaria %in% faixa_etaria.df$value))) {
        stop("Some element in 'faixa_etaria' argument is wrong")
      }
    }
  }
  if (any(faixa_etaria_reajuste != "Todas as categorias")) {
    if (!(all(faixa_etaria_reajuste %in% faixa_etaria_reajuste.df$id))) {
      faixa_etaria_reajuste <- as.character(faixa_etaria_reajuste)
      if (!(all(faixa_etaria_reajuste %in% faixa_etaria_reajuste.df$value))) {
        stop("Some element in 'faixa_etaria_reajuste' argument is wrong")
      }
    }
  }
  if (any(sexo != "Todas as categorias")) {
    if (!(all(sexo %in% sexo.df$id))) {
      sexo <- as.character(sexo)
      if (!(all(sexo %in% sexo.df$value))) {
        stop("Some element in 'sexo' argument is wrong")
      }
    }
  }
  if (linha %in% linha.df$id) {
    linha <- dplyr::filter(linha.df, linha.df$id %in% linha)
    linha <- linha$value
  }
  if (!stringi::stri_enc_isascii(linha)) {
    form_linha <- paste0("Linha=", stringi::stri_escape_unicode(linha))
  } else {
    form_linha <- paste0("Linha=", linha)
  }
  if (coluna %in% coluna.df$id) {
    coluna <- dplyr::filter(coluna.df, coluna.df$id %in%
                              coluna)
    coluna <- coluna$value
  }
  if (!stringi::stri_enc_isascii(coluna)) {
    form_coluna <- paste0("Coluna=", stringi::stri_escape_unicode(coluna))
  } else {
    form_coluna <- paste0("Coluna=", coluna)
  }
  form_conteudo <- conteudo.df$value[conteudo]
  if (!stringi::stri_enc_isascii(form_conteudo)) {
    form_conteudo <- paste0("Incremento=", stringi::stri_escape_unicode(form_conteudo))
  } else {
    form_conteudo <- paste0("Incremento=", form_conteudo)
  }

  form_periodo <- dplyr::filter(periodo.df, periodo.df$id %in%
                                  periodo)
  form_periodo <- paste0("Arquivos=", form_periodo$value, collapse = "&")
  form_pesqmes1 <- "pesqmes1=Digite+o+texto+e+ache+f%E1cil"
  form_regiao <- dplyr::filter(regiao.df, regiao.df$id %in%
                                 regiao)
  form_regiao <- paste0("SRegi%E3o=", form_regiao$value, collapse = "&")
  form_pesqmes2 <- "pesqmes2=Digite+o+texto+e+ache+f%E1cil"
  form_unidade_da_federacao <- dplyr::filter(unidade_da_federacao.df,
                                             unidade_da_federacao.df$id %in% unidade_da_federacao)
  form_unidade_da_federacao <- paste0("SUnidade_da_Federa%E7%E3o=",
                                      form_unidade_da_federacao$value, collapse = "&")
  form_pesqmes3 <- "pesqmes3=Digite+o+texto+e+ache+f%E1cil"
  form_faixa_etaria <- dplyr::filter(faixa_etaria.df, faixa_etaria.df$id %in%
                                       faixa_etaria)
  form_faixa_etaria <- paste0("SFaixa_Et%E1ria_1=", form_faixa_etaria$value,
                              collapse = "&")
  form_faixa_etaria_reajuste <- dplyr::filter(faixa_etaria_reajuste.df, faixa_etaria_reajuste.df$id %in%
                                                faixa_etaria_reajuste)
  form_faixa_etaria_reajuste <- paste0("SFaixa_Et%E1ria_2=", form_faixa_etaria_reajuste$value,
                                       collapse = "&")
  form_sexo <- dplyr::filter(sexo.df, sexo.df$id %in% sexo)
  form_sexo <- paste0("SSexo=", form_sexo$value, collapse = "&")
  form_data <- paste(form_linha, form_coluna, form_conteudo,
                     form_periodo, form_pesqmes1, form_regiao, form_pesqmes2,
                     form_unidade_da_federacao, form_pesqmes3, form_sexo,
                     form_faixa_etaria, form_faixa_etaria_reajuste, "formato=table&mostre=Mostra",
                     sep = "&")
  form_data <- gsub("\\\\u00", "%", form_data)
  site <- httr::POST(url = "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/popsvsbr.def",
                     body = form_data)
  tabdados <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements(".tabdados tbody td") |> rvest::html_text() |>
    trimws()
  col_tabdados <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements("th") |> rvest::html_text() |> trimws()
  f1 <- function(x) x <- gsub("\\.", "", x)
  f2 <- function(x) x <- as.numeric(as.character(x))
  tabela_final <- as.data.frame(matrix(data = tabdados, nrow = length(tabdados)/length(col_tabdados),
                                       ncol = length(col_tabdados), byrow = TRUE))
  names(tabela_final) <- col_tabdados
  tabela_final[-1] <- lapply(tabela_final[-1], f1)
  tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1],
                                              f2))

  if (linha == "Município") {
    tabela_final <- tabela_final[-1, ] |>
      dplyr::mutate(
        codmunres = as.numeric(stringr::str_extract(Município, "\\d+")),
        municipio = stringr::str_replace(Município, "\\d+ ", ""),
        .before = "Município",
        .keep = "unused"
      )
  } else {
    tabela_final <- tabela_final[-1, ]
  }

}

# Criando a função que utiliza web scrapping para baixar dados de usuárias de planos de saúde do Tabnet ANS
pop_com_plano_saude_tabnet <- function (linha = "Município",
                                        coluna = "Competência",
                                        conteudo = "Assistência Médica",
                                        periodo = 2012:2021,
                                        sexo = "Feminino",
                                        faixa_etaria = c(
                                          "10 a 14 anos",
                                          "15 a 19 anos",
                                          "20 a 24 anos",
                                          "25 a 29 anos",
                                          "30 a 34 anos",
                                          "35 a 39 anos",
                                          "40 a 44 anos",
                                          "45 a 49 anos"
                                        ),
                                        faixa_etaria_reajuste = "Todas as categorias",
                                        tipo_de_contratacao = "Todas as categorias",
                                        epoca_de_contratacao = "Todas as categorias",
                                        segmentacao = "Todas as categorias",
                                        segmentacao_grupo = c(
                                          "Ambulatorial", "Hospitalar", "Hospitalar e Ambulatorial",
                                          "Referência", "Informado incorretamente", "Não Informado"
                                        ),
                                        uf = "Todas as categorias",
                                        regiao = "Todas as categorias",
                                        capital = "Todas as categorias",
                                        reg_metropolitana = "Todas as categorias",
                                        microrregiao = "Todas as categorias",
                                        municipio = "Todas as categorias"
)

{

  page <- xml2::read_html("http://www.ans.gov.br/anstabnet/cgi-bin/dh?dados/tabnet_02.def")

  linha.df <- data.frame(
    id = page |>
      rvest::html_elements("#L option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#L option") |>
      rvest::html_attr("value")
  )

  coluna.df <- data.frame(
    id = page |>
      rvest::html_elements("#C option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#C option") |>
      rvest::html_attr("value")
  )

  conteudo.df <- data.frame(
    id = page |>
      rvest::html_elements("#I option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#I option") |>
      rvest::html_attr("value")
  )

  periodo.df <- data.frame(
    id = page |>
      rvest::html_elements("#A option") |>
      rvest::html_text() |>
      substr(start = 1, stop = 8),
    value = page |>
      rvest::html_elements("#A option") |>
      rvest::html_attr("value")
  )

  sexo.df <- data.frame(
    id = page |>
      rvest::html_elements("#S1 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S1 option") |>
      rvest::html_attr("value")
  )

  faixa_etaria.df <- data.frame(
    id = page |>
      rvest::html_elements("#S2 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S2 option") |>
      rvest::html_attr("value")
  )

  faixa_etaria_reajuste.df <- data.frame(
    id = page |>
      rvest::html_elements("#S3 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S3 option") |>
      rvest::html_attr("value")
  )

  tipo_de_contratacao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S4 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S4 option") |>
      rvest::html_attr("value")
  )

  epoca_de_contratacao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S5 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S5 option") |>
      rvest::html_attr("value")
  )

  segmentacao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S6 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S6 option") |>
      rvest::html_attr("value")
  )

  segmentacao_grupo.df <- data.frame(
    id = page |>
      rvest::html_elements("#S7 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S7 option") |>
      rvest::html_attr("value")
  )

  uf.df <- data.frame(
    id = page |>
      rvest::html_elements("#S8 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S8 option") |>
      rvest::html_attr("value")
  )

  regiao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S9 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S9 option") |>
      rvest::html_attr("value")
  )

  capital.df <- data.frame(
    id = page |>
      rvest::html_elements("#S10 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S10 option") |>
      rvest::html_attr("value")
  )

  reg_metropolitana.df <- data.frame(
    id = page |>
      rvest::html_elements("#S11 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S11 option") |>
      rvest::html_attr("value")
  )

  microrregiao.df <- data.frame(
    id = page |>
      rvest::html_elements("#S12 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S12 option") |>
      rvest::html_attr("value")
  )

  municipio.df <- data.frame(
    id = page |>
      rvest::html_elements("#S13 option") |>
      rvest::html_text() |>
      trimws(),
    value = page |>
      rvest::html_elements("#S13 option") |>
      rvest::html_attr("value")
  )

  if (is.numeric(periodo)) {
    periodo <- periodo.df |>
      dplyr::filter(
        substr(id, start = 5, stop = 8) %in% as.character(periodo)
      ) |>
      dplyr::pull(id)
  }

  argumentos <- c(
    "linha",
    "coluna",
    "conteudo",
    "periodo",
    "sexo",
    "faixa_etaria",
    "faixa_etaria_reajuste",
    "tipo_de_contratacao",
    "epoca_de_contratacao",
    "segmentacao",
    "segmentacao_grupo",
    "uf",
    "regiao",
    "capital",
    "reg_metropolitana",
    "microrregiao",
    "municipio"
  )

  invisible(lapply(argumentos, function(argumento) {
    if (!(all(get(argumento) %in% get(glue::glue("{argumento}.df"))$id))) {
      stop(glue::glue("Some element in the '{argumento}' argument is wrong"))
    }
    if (argumento == "periodo") {
      if (length(periodo) > 1 & !(linha == "Competência" | coluna == "Competência")) {
        stop("When more than one period is specified, either the 'linha' or the 'coluna' argument must be equal to 'Competência'")
      }
    }
  }))

  argumentos.df <- data.frame(
    argumento = argumentos,
    name =   page |>
      rvest::html_elements("select") |>
      rvest::html_attr("name") |>
      stringi::stri_escape_unicode()
  )

  for(argumento in argumentos.df$argumento) {
    assign(
      glue::glue("{argumento}.value"),
      get(glue::glue("{argumento}.df")) |>
        dplyr::filter(id %in% get(argumento)) |>
        dplyr::pull(value)
    )

    name <- argumentos.df$name[argumentos.df$argumento == argumento]

    assign(
      glue::glue("form_{argumento}"),
      paste0(name, "=",  stringi::stri_escape_unicode(get(glue::glue("{argumento}.value"))), collapse = "&")
    )
  }

  form_data <- paste(
    form_linha,
    form_coluna,
    form_conteudo,
    form_periodo,
    form_sexo,
    form_faixa_etaria,
    form_faixa_etaria_reajuste,
    form_tipo_de_contratacao,
    form_epoca_de_contratacao,
    form_segmentacao,
    form_segmentacao_grupo,
    form_uf,
    form_regiao,
    form_capital,
    form_reg_metropolitana,
    form_microrregiao,
    form_municipio,
    "formato=table&mostre=Mostra",
    sep = "&"
  )

  form_data <- gsub("\\\\u00", "%", form_data)

  #form_data <- "Linha=Munic%edpio&Coluna=--N%E3o-Ativa--&Incremento=Assist%EAncia_M%E9dica&Arquivos=tb_bb_2306.dbf&SSexo=TODAS_AS_CATEGORIAS__&SFaixa_et%E1ria=TODAS_AS_CATEGORIAS__&SFaixa_et%E1ria-Reajuste=TODAS_AS_CATEGORIAS__&STipo_de_contrata%E7%E3o=TODAS_AS_CATEGORIAS__&S%C9poca_de_Contrata%E7%E3o=TODAS_AS_CATEGORIAS__&SSegmenta%E7%E3o=TODAS_AS_CATEGORIAS__&SSegmenta%E7%E3o_grupo=TODAS_AS_CATEGORIAS__&SUF=TODAS_AS_CATEGORIAS__&SGrande_Regi%E3o=TODAS_AS_CATEGORIAS__&SCapital=TODAS_AS_CATEGORIAS__&SReg._Metropolitana=TODAS_AS_CATEGORIAS__&SMicrorregi%E3o=TODAS_AS_CATEGORIAS__&SMunic%EDpio=TODAS_AS_CATEGORIAS__&formato=table&mostre=Mostra"

  site <- httr::POST(url = "http://www.ans.gov.br/anstabnet/cgi-bin/tabnet?dados/tabnet_02.def",
                     body = form_data)

  tabdados_col1 <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements("tr th") |> rvest::html_text() |>
    trimws()
  tabdados_col1 <- tabdados_col1[-c(1:(which(tabdados_col1 == "TOTAL") - 1))]

  tabdados_outras_cols <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements("center td") |> rvest::html_text() |>
    trimws()

  col_tabdados <- httr::content(site, encoding = "Latin1") |>
    rvest::html_elements("tr:nth-child(1) th") |> rvest::html_text() |> trimws()

  f1 <- function(x) x <- gsub("\\.", "", x)
  f2 <- function(x) x <- as.numeric(as.character(x))

  tabela_final <- as.data.frame(
    cbind(
      tabdados_col1,
      matrix(tabdados_outras_cols, ncol = length(tabdados_outras_cols)/length(tabdados_col1), byrow = TRUE)
    )
  )

  names(tabela_final) <- col_tabdados
  tabela_final[-1] <- lapply(tabela_final[-1], f1)
  tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1], f2))

  if (linha == "Município") {
    tabela_final <- tabela_final[-1, ] |>
      dplyr::mutate(
        codmunres = as.numeric(stringr::str_extract(Município, "\\d+")),
        municipio = stringr::str_replace(Município, "\\d+ ", ""),
        .before = "Município",
        .keep = "unused"
      )
  } else {
    tabela_final <- tabela_final[-1, ]
  }

}

## Selecinando dados
df_cobertura_esf <- df_bloco1_antigo |>
  select(codmunres, ano, media_cobertura_esf, populacao_total,populacao_feminina_10_a_49,pop_fem_10_49_com_plano_saude)

## Juntando com o restante da base do bloco 1
df_bloco1 <- left_join(df_bloco1_sinasc, df_cobertura_esf)


# População total ---------------------------------------------------------
df_est_pop_total_aux <- est_pop_tabnet(
  coluna = "Ano",
  periodo = as.character(2012:2021),
  sexo = "Todas as categorias",
  faixa_etaria = "Todas as categorias"
) |>
  select(!municipio)

##Passando os dados para o formato long
df_est_pop_total <- df_est_pop_total_aux |>
  pivot_longer(
    cols = !c(codmunres),
    names_to = "ano",
    values_to = "populacao_total"
  ) |>
  mutate_if(is.character, as.numeric)

## Juntando com o restante da base do bloco 1
df_bloco1 <- left_join(df_bloco1, df_est_pop_total)


# População feminina de 10 a 49 anos com plano de saúde -------------------
##Baixando os dados de estimativas da população feminina de 10 a 49 anos
df_est_pop_aux <- est_pop_tabnet(
  coluna = "Ano", periodo = as.character(2012:2021)
) |>
  select(!municipio)

##Verificando se existem NAs
if (any(is.na(df_est_pop_aux))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}

##Passando o data.frame para o formato long
df_est_pop <- df_est_pop_aux |>
  pivot_longer(
    !codmunres,
    names_to = "ano",
    values_to = "populacao_feminina_10_a_49"
  ) |>
  mutate(
    ano = as.numeric(ano),
  ) |>
  arrange(codmunres, ano) |>
  filter(codmunres %in% df_aux_municipios$codmunres)

##Baixando os dados de mulheres de 10 a 49 anos beneficíarias de planos de saúde
df_beneficiarias_aux <- pop_com_plano_saude_tabnet(
  faixa_etaria = c("10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")
) |>
  select(!municipio)

##Verificando se existem NAs
if (any(is.na(df_beneficiarias_aux))) {
  print("existem NAs")
} else {
  print("não existem NAs")
}

##Passando o data.frame para o formato long
df_beneficiarias <- df_beneficiarias_aux |>
  pivot_longer(
    !codmunres,
    names_to = "mes_ano",
    values_to = paste0("beneficiarias_10_a_49")
  ) |>
  mutate(
    mes = substr(mes_ano, start = 1, stop = 3),
    ano = as.numeric(paste0("20", substr(mes_ano, start = 5, stop = 6))),
    .after = mes_ano,
    .keep = "unused"
  ) |>
  arrange(codmunres, ano) |>
  filter(codmunres %in% df_aux_municipios$codmunres) |>
  left_join(df_est_pop) |>
  group_by(codmunres, ano) |>
  filter(beneficiarias_10_a_49 < populacao_feminina_10_a_49) |>
  summarise(
    beneficiarias_10_a_49 = round(median(beneficiarias_10_a_49))
  ) |>
  ungroup()

##Juntando com os dados de estimativas populacionais
df_beneficiarias_pop <- left_join(df_est_pop, df_beneficiarias)

##Calculando a cobertura suplementar, os limites inferiores e superiores para a consideração de outliers e inputando caso necessário
df_cob_suplementar <- df_beneficiarias_pop |>
  mutate(
    cob_suplementar = round(beneficiarias_10_a_49 / populacao_feminina_10_a_49, 3)
  ) |>
  group_by(codmunres) |>
  mutate(
    q1 = round(quantile(cob_suplementar[which(cob_suplementar < 1 & ano %in% 2014:2021)], 0.25), 3),
    q3 = round(quantile(cob_suplementar[which(cob_suplementar < 1 & ano %in% 2014:2021)], 0.75), 3),
    iiq = q3 - q1,
    lim_inf = round(q1 - 1.5*iiq, 3),
    lim_sup = round(q3 + 1.5*iiq, 3),
    outlier = ifelse((cob_suplementar > 1) | (cob_suplementar < lim_inf | cob_suplementar > lim_sup) | (is.na(q1) & is.na(q3)) | (is.na(cob_suplementar)), 1, 0),
    novo_cob_suplementar = ifelse(
      outlier == 0,
      cob_suplementar,
      round(median(cob_suplementar[which(outlier == 0 & ano %in% 2014:2021)]), 3)
    ),
    novo_beneficiarias_10_a_49 = round(novo_cob_suplementar * populacao_feminina_10_a_49)
  ) |>
  ungroup() |>
  select(codmunres, ano, pop_fem_10_49_com_plano_saude = novo_beneficiarias_10_a_49, populacao_feminina_10_a_49)

### Juntando com o restante da base do bloco 1
df_bloco1 <- left_join(df_bloco1, df_cob_suplementar)

### Substituindo os NA's da coluna 'pop_fem_10_49_com_plano_saude' por 0 (gerados após o left_join)
df_bloco1$pop_fem_10_49_com_plano_saude[is.na(df_bloco1$pop_fem_10_49_com_plano_saude) & df_bloco1$ano <= 2021] <- 0


# Verificando se os dados novos e antigos estão batendo -------------------
## Considerando que 2023 e 2024 são dados preliminares, só sera considerado 2012 até 2022

sum(df_bloco1 |> filter(ano < 2023) |> pull(total_de_nascidos_vivos)) - sum(df_bloco1_antigo$total_de_nascidos_vivos)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_menor_que_20_anos)) - sum(df_bloco1_antigo$nvm_menor_que_20_anos) #Não está batendo
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_entre_20_e_34_anos)) - sum(df_bloco1_antigo$nvm_entre_20_e_34_anos)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_maior_que_34_anos)) - sum(df_bloco1_antigo$nvm_maior_que_34_anos)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_cor_da_pele_branca)) - sum(df_bloco1_antigo$nvm_com_cor_da_pele_branca)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_cor_da_pele_preta)) - sum(df_bloco1_antigo$nvm_com_cor_da_pele_preta)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_cor_da_pele_parda)) - sum(df_bloco1_antigo$nvm_com_cor_da_pele_parda)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_cor_da_pele_amarela)) - sum(df_bloco1_antigo$nvm_com_cor_da_pele_amarela)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_indigenas)) - sum(df_bloco1_antigo$nvm_indigenas)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_escolaridade_ate_3)) - sum(df_bloco1_antigo$nvm_com_escolaridade_ate_3)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_escolaridade_de_4_a_7)) - sum(df_bloco1_antigo$nvm_com_escolaridade_de_4_a_7)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_escolaridade_de_8_a_11)) - sum(df_bloco1_antigo$nvm_com_escolaridade_de_8_a_11)
sum(df_bloco1 |> filter(ano < 2023) |> pull(nvm_com_escolaridade_acima_de_11)) - sum(df_bloco1_antigo$nvm_com_escolaridade_acima_de_11)
# as variáveis que utilizam tabnet devem ser comparadas antes de 2021
sum(df_bloco1 |> filter(ano < 2021) |> pull(media_cobertura_esf)) - sum(df_bloco1_antigo$media_cobertura_esf)
sum(df_bloco1 |> filter(ano < 2021) |> pull(populacao_total)) - sum(df_bloco1_antigo$populacao_total) #Não está batendo, mas são dados de lugares diferentes
#sum(df_bloco1 |> filter(ano < 2021) |> pull(pop_fem_10_49_com_plano_saude), na.rm = TRUE) - sum(df_bloco1_antigo$pop_fem_10_49_com_plano_saude, na.rm = TRUE) #Metodologias diferentes
sum(df_bloco1 |> filter(ano < 2021) |> pull(populacao_feminina_10_a_49)) - sum(df_bloco1_antigo$populacao_feminina_10_a_49)

# novo indicador cobertura -----------------------------------------------------

# importando as bases com as variaveis que serao utilizadas

historico_ab_municipios <- read_delim("data-raw/csv/Historico_AB_MUNICIPIOS_2007_202012.csv") |>
  janitor::clean_names() |>
  dplyr::select(nu_competencia, co_municipio_ibge, qt_cobertura_ab, qt_populacao)


cobertura_potencial_aps_municipio <- read_delim("data-raw/csv/cobertura_potencial_aps_municipio2.csv",
                                                delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
                                                trim_ws = TRUE) |>
  janitor::clean_names() |>
  dplyr::select(co_municipio_ibge, mes_ano, qt_capacidade_equipe_mun, qt_pop_municipio)


# fazendo tratamento na variavel ano e renomeando as variaveis

historico_ab_municipios <- historico_ab_municipios |>
  mutate(ano = substr(historico_ab_municipios$nu_competencia, 1, 4)) |>
  rename(codmunres = co_municipio_ibge) |>
  select(ano, codmunres, qt_cobertura_ab, qt_populacao)

cobertura_potencial_aps_municipio <- cobertura_potencial_aps_municipio |>
  mutate(ano = substr(cobertura_potencial_aps_municipio$mes_ano, nchar(cobertura_potencial_aps_municipio$mes_ano) - 3, nchar(cobertura_potencial_aps_municipio$mes_ano))) |>
  rename(codmunres = co_municipio_ibge,
         qt_cobertura_ab = qt_capacidade_equipe_mun,
         qt_populacao = qt_pop_municipio) |>
  select(ano, codmunres, qt_cobertura_ab, qt_populacao)

# trasformando as variaveis qt_cobertura_ab e qt_populacao para numerico

historico_ab_municipios[, 3:4] <- lapply(historico_ab_municipios[, 3:4], function(x) as.numeric(str_replace_all(x, "\\.", "") %>% str_replace_all("\\,", "\\.")))

# juntando as bases e transformando ano em numerica
dados_ab_municipios <- rbind(historico_ab_municipios, cobertura_potencial_aps_municipio)
dados_ab_municipios$ano <- as.numeric(dados_ab_municipios$ano)


# fazendo a media dos registros para qt_cobertura_ab e qt_populacao

dados_ab_municipios <- dados_ab_municipios |>
  group_by(ano, codmunres) |>
  summarize(qt_cobertura_ab = mean(qt_cobertura_ab),
            qt_populacao = mean(qt_populacao))

# Remover o ponto da variavel qt_populacao
dados_ab_municipios$qt_populacao <- as.numeric(str_replace(dados_ab_municipios$qt_populacao, "\\..*$", ""))


# Vamos garantir que qt_cobertura_ab não seja maior que qt_populacao

dados_ab_municipios$qt_cobertura_ab <- pmin(dados_ab_municipios$qt_cobertura_ab, dados_ab_municipios$qt_populacao)


## importando base antiga

df_bloco1 <- read.csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2024.csv")
## Fazendo um left_join da base auxiliar de municípios com a base de cobertura
df_bloco1 <- left_join(df_bloco1, dados_ab_municipios)


### Substituindo os NA's da coluna 'qt_cobertura_ab' por 0 (gerados após o left_join)
df_bloco1$qt_cobertura_ab[is.na(df_bloco1$qt_cobertura_ab) & df_bloco1$ano < 2023] <- 0

### Substituindo os NA's da coluna 'qt_populacao' por 0 (gerados após o left_join)
df_bloco1$qt_populacao[is.na(df_bloco1$qt_populacao) & df_bloco1$ano < 2023] <- 0



## Juntando com o restante da base do bloco 1
df_bloco1 <- df_bloco1 |>
  select(!c(media_cobertura_esf, populacao_total)) |>
  rename(media_cobertura_esf = qt_cobertura_ab, populacao_total = qt_populacao)


# Salvando a base de dados completa na pasta data-raw/csv -----------------
write.csv(df_bloco1, "data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2024.csv", row.names = FALSE)
