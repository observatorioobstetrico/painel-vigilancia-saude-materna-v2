# Criando a função que baixa os dados de estimativas populacionais por município, idade e sexo a partir de um endereço FTP
est_pop_tabnet <- function(periodo = 12:24, sexo = 2, idade_min = 10, idade_max = 49, temp_dir = "data-raw/extracao-dos-dados/blocos/databases_auxiliares/dados_populacao") {
  library(dplyr)
  library(foreign)
  library(RCurl)
  library(utils)
  library(stringr)

  # Criar um vetor com os anos de interesse
  anos <- periodo

  # URL base
  diretorio <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/"

  # Criar diretório temporário
  temp_dir <- temp_dir
  dir.create(temp_dir, showWarnings = FALSE)

  # Lista para armazenar os dataframes
  lista_dfs <- list()

  for (ano in anos) {
    # Nome do arquivo
    arquivo_zip <- paste0("POPSBR", ano, ".zip")
    arquivo_dbf <- paste0("pop", ano, ".dbf")

    # Caminho completo do arquivo ZIP
    url_zip <- paste0(diretorio, arquivo_zip)

    # Nome temporário para salvar o ZIP
    temp_zip <- tempfile(fileext = ".zip")

    # Baixar o arquivo ZIP
    download.file(url_zip, temp_zip, mode = "wb")

    # Descompactar o ZIP
    unzip(temp_zip, exdir = temp_dir)

    # Ler o arquivo DBF
    caminho_dbf <- file.path(temp_dir, arquivo_dbf)
    df <- read.dbf(caminho_dbf, as.is = TRUE)

    # Transformar COD_MUN em caracter e renomeá-la
    df <- df |>
      rename(codmunres = COD_MUN, ano = ANO) |>
      mutate(
        codmunres = as.numeric(codmunres),
        codmunres = substr(codmunres, 1, 6)
      )

    # Filtrar SEXO e IDADE
    df <- df |>
      mutate(IDADE = as.numeric(as.character(IDADE))) |>
      filter(SEXO == sexo, IDADE >= idade_min, IDADE <= idade_max)

    # Agrupar e somar POP
    df_resumo <- df |>
      group_by(codmunres, ano) |>
      summarise(populacao_feminina_10_a_49 = sum(POP, na.rm = TRUE), .groups = "drop")

    # Adicionar o dataframe à lista
    lista_dfs[[ano]] <- df_resumo
  }

  # Juntar todos os dataframes em um só
  df_final <- bind_rows(lista_dfs) |>
    arrange(codmunres, ano)

  # Apagar a pasta temporária
  unlink("data-raw/extracao-dos-dados/blocos/databases_auxiliares/dados_populacao", recursive = TRUE)

  return(df_final)
}


# Criando a função que utiliza web scrapping para baixar dados de estimativas populacionais do Tabnet DATASUS
#### NÃO MAIS EM USO ###
# est_pop_tabnet <- function (linha = "Município", coluna = "Ano", conteudo = 1, periodo = 2012:2021, regiao = "Todas as categorias",
#                             unidade_da_federacao = "Todas as categorias", sexo = "Feminino", faixa_etaria = c("10 a 14 anos", "15 a 19 anos", "20 a 29 anos", "30 a 39 anos", "40 a 49 anos"),
#                             faixa_etaria_reajuste = "Todas as categorias")
# {
#   page <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/popsvsbr.def")
#
#   linha.df <- data.frame(id = page |> rvest::html_elements("#L option") |>
#                            rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#L option") |>
#                            rvest::html_attr("value"))
#   linha.df[] <- lapply(linha.df, as.character)
#   coluna.df <- data.frame(id = page |> rvest::html_elements("#C option") |>
#                             rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#C option") |>
#                             rvest::html_attr("value"))
#   coluna.df[] <- lapply(coluna.df, as.character)
#
#   conteudo.df <- data.frame(id1 = 1, id2 = "População_residente", value = "População_residente")
#
#   periodo.df <- data.frame(id = page |> rvest::html_elements("#A option") |>
#                              rvest::html_text() |> as.numeric(), value = page |>
#                              rvest::html_elements("#A option") |> rvest::html_attr("value"))
#   regiao.df <- suppressWarnings(data.frame(id = page |> rvest::html_elements("#S1 option") |>
#                                              rvest::html_text() |> readr::parse_number(), value = page |>
#                                              rvest::html_elements("#S1 option") |> rvest::html_attr("value")))
#   unidade_da_federacao.df <- suppressWarnings(data.frame(id = page |>
#                                                            rvest::html_elements("#S2 option") |> rvest::html_text() |>
#                                                            trimws(), value = page |> rvest::html_elements("#S2 option") |>
#                                                            rvest::html_attr("value")))
#   unidade_da_federacao.df[] <- lapply(unidade_da_federacao.df,
#                                       as.character)
#   sexo.df <- data.frame(id = page |> rvest::html_elements("#S16 option") |>
#                           rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S16 option") |>
#                           rvest::html_attr("value"))
#   sexo.df[] <- lapply(sexo.df, as.character)
#   faixa_etaria.df <- data.frame(id = page |> rvest::html_elements("#S17 option") |>
#                                   rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S17 option") |>
#                                   rvest::html_attr("value"))
#   faixa_etaria.df[] <- lapply(faixa_etaria.df, as.character)
#   faixa_etaria_reajuste.df <- data.frame(id = page |> rvest::html_elements("#S18 option") |>
#                                            rvest::html_text() |> trimws(), value = page |> rvest::html_elements("#S18 option") |>
#                                            rvest::html_attr("value"))
#   faixa_etaria_reajuste.df[] <- lapply(faixa_etaria_reajuste.df, as.character)
#   regiao.df$id[1] <- unidade_da_federacao.df$id[1] <- "Todas as categorias"
#   sexo.df$id[1] <- faixa_etaria.df$id[1] <- faixa_etaria_reajuste.df$id[1] <- "Todas as categorias"
#   if (linha != "Unidade da Federação") {
#     if (!is.character(linha))
#       stop("The 'linha' argument must be a character element")
#     if (length(linha) != 1)
#       stop("The 'linha' argument must have only one element")
#     if (!(all(linha %in% linha.df$id))) {
#       if (!(all(linha %in% linha.df$value))) {
#         stop("The 'linha' argument is misspecified")
#       }
#     }
#   }
#   if (coluna != "Não ativa") {
#     if (!is.character(coluna))
#       stop("The 'coluna' argument must be a character element")
#     if (length(coluna) != 1)
#       stop("The 'coluna' argument must have only one element")
#     if (!(all(coluna %in% coluna.df$id))) {
#       if (!(all(coluna %in% coluna.df$value))) {
#         stop("The 'coluna' argument is misspecified")
#       }
#     }
#   }
#   if (conteudo != 1 & conteudo != 2 & conteudo != 3) {
#     if (is.numeric(conteudo))
#       stop("The only numeric elements allowed are 1 or 2")
#     if (length(conteudo) != 1)
#       stop("The 'coluna' argument must have only one element")
#     if (!(all(conteudo %in% conteudo.df$id2))) {
#       if (!(all(conteudo %in% conteudo.df$value))) {
#         stop("The 'conteudo' argument is misspecified")
#       }
#     }
#   }
#   if (periodo[1] != "last") {
#     if (is.character(periodo)) {
#       periodo <- as.numeric(periodo)
#     }
#     if (!(all(periodo %in% periodo.df$id)))
#       stop("The 'periodo' argument is misspecified")
#   }
#   if (any(regiao != "Todas as categorias")) {
#     regiao <- as.character(regiao)
#     if (!(all(regiao %in% regiao.df$id)))
#       stop("Some element in 'regiao' argument is wrong")
#   }
#   if (any(unidade_da_federacao != "Todas as categorias")) {
#     unidade_da_federacao <- as.character(unidade_da_federacao)
#     if (!(all(unidade_da_federacao %in% unidade_da_federacao.df$id)))
#       stop("Some element in 'unidade_da_federacao' argument is wrong")
#   }
#   if (any(faixa_etaria != "Todas as categorias")) {
#     if (!(all(faixa_etaria %in% faixa_etaria.df$id))) {
#       faixa_etaria <- as.character(faixa_etaria)
#       if (!(all(faixa_etaria %in% faixa_etaria.df$value))) {
#         stop("Some element in 'faixa_etaria' argument is wrong")
#       }
#     }
#   }
#   if (any(faixa_etaria_reajuste != "Todas as categorias")) {
#     if (!(all(faixa_etaria_reajuste %in% faixa_etaria_reajuste.df$id))) {
#       faixa_etaria_reajuste <- as.character(faixa_etaria_reajuste)
#       if (!(all(faixa_etaria_reajuste %in% faixa_etaria_reajuste.df$value))) {
#         stop("Some element in 'faixa_etaria_reajuste' argument is wrong")
#       }
#     }
#   }
#   if (any(sexo != "Todas as categorias")) {
#     if (!(all(sexo %in% sexo.df$id))) {
#       sexo <- as.character(sexo)
#       if (!(all(sexo %in% sexo.df$value))) {
#         stop("Some element in 'sexo' argument is wrong")
#       }
#     }
#   }
#   if (linha %in% linha.df$id) {
#     linha <- dplyr::filter(linha.df, linha.df$id %in% linha)
#     linha <- linha$value
#   }
#   if (!stringi::stri_enc_isascii(linha)) {
#     form_linha <- paste0("Linha=", stringi::stri_escape_unicode(linha))
#   } else {
#     form_linha <- paste0("Linha=", linha)
#   }
#   if (coluna %in% coluna.df$id) {
#     coluna <- dplyr::filter(coluna.df, coluna.df$id %in%
#                               coluna)
#     coluna <- coluna$value
#   }
#   if (!stringi::stri_enc_isascii(coluna)) {
#     form_coluna <- paste0("Coluna=", stringi::stri_escape_unicode(coluna))
#   } else {
#     form_coluna <- paste0("Coluna=", coluna)
#   }
#   form_conteudo <- conteudo.df$value[conteudo]
#   if (!stringi::stri_enc_isascii(form_conteudo)) {
#     form_conteudo <- paste0("Incremento=", stringi::stri_escape_unicode(form_conteudo))
#   } else {
#     form_conteudo <- paste0("Incremento=", form_conteudo)
#   }
#
#   form_periodo <- dplyr::filter(periodo.df, periodo.df$id %in%
#                                   periodo)
#   form_periodo <- paste0("Arquivos=", form_periodo$value, collapse = "&")
#   form_pesqmes1 <- "pesqmes1=Digite+o+texto+e+ache+f%E1cil"
#   form_regiao <- dplyr::filter(regiao.df, regiao.df$id %in%
#                                  regiao)
#   form_regiao <- paste0("SRegi%E3o=", form_regiao$value, collapse = "&")
#   form_pesqmes2 <- "pesqmes2=Digite+o+texto+e+ache+f%E1cil"
#   form_unidade_da_federacao <- dplyr::filter(unidade_da_federacao.df,
#                                              unidade_da_federacao.df$id %in% unidade_da_federacao)
#   form_unidade_da_federacao <- paste0("SUnidade_da_Federa%E7%E3o=",
#                                       form_unidade_da_federacao$value, collapse = "&")
#   form_pesqmes3 <- "pesqmes3=Digite+o+texto+e+ache+f%E1cil"
#   form_faixa_etaria <- dplyr::filter(faixa_etaria.df, faixa_etaria.df$id %in%
#                                        faixa_etaria)
#   form_faixa_etaria <- paste0("SFaixa_Et%E1ria_1=", form_faixa_etaria$value,
#                               collapse = "&")
#   form_faixa_etaria_reajuste <- dplyr::filter(faixa_etaria_reajuste.df, faixa_etaria_reajuste.df$id %in%
#                                                 faixa_etaria_reajuste)
#   form_faixa_etaria_reajuste <- paste0("SFaixa_Et%E1ria_2=", form_faixa_etaria_reajuste$value,
#                                        collapse = "&")
#   form_sexo <- dplyr::filter(sexo.df, sexo.df$id %in% sexo)
#   form_sexo <- paste0("SSexo=", form_sexo$value, collapse = "&")
#   form_data <- paste(form_linha, form_coluna, form_conteudo,
#                      form_periodo, form_pesqmes1, form_regiao, form_pesqmes2,
#                      form_unidade_da_federacao, form_pesqmes3, form_sexo,
#                      form_faixa_etaria, form_faixa_etaria_reajuste, "formato=table&mostre=Mostra",
#                      sep = "&")
#   form_data <- gsub("\\\\u00", "%", form_data)
#   site <- httr::POST(url = "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/popsvsbr.def",
#                      body = form_data)
#   tabdados <- httr::content(site, encoding = "Latin1") |>
#     rvest::html_elements(".tabdados tbody td") |> rvest::html_text() |>
#     trimws()
#   col_tabdados <- httr::content(site, encoding = "Latin1") |>
#     rvest::html_elements("th") |> rvest::html_text() |> trimws()
#   f1 <- function(x) x <- gsub("\\.", "", x)
#   f2 <- function(x) x <- as.numeric(as.character(x))
#   tabela_final <- as.data.frame(matrix(data = tabdados, nrow = length(tabdados)/length(col_tabdados),
#                                        ncol = length(col_tabdados), byrow = TRUE))
#   names(tabela_final) <- col_tabdados
#   tabela_final[-1] <- lapply(tabela_final[-1], f1)
#   tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1],
#                                               f2))
#
#   if (linha == "Município") {
#     tabela_final <- tabela_final[-1, ] |>
#       dplyr::mutate(
#         codmunres = as.numeric(stringr::str_extract(Município, "\\d+")),
#         municipio = stringr::str_replace(Município, "\\d+ ", ""),
#         .before = "Município",
#         .keep = "unused"
#       )
#   } else {
#     tabela_final <- tabela_final[-1, ]
#   }
#
# }

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
