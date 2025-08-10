#' @exportS3Method pkg::generic
#' @import data.table
.datatable.aware <- TRUE # avisa o cedta() que o pacote é DT‑aware
cria_indicadores <- function(df_localidade, df_calcs, df_calcs_dist_bloco7 = NULL, filtros, referencia = FALSE, comp = FALSE, adicionar_localidade = TRUE, input = NULL, bloco = "outros", localidade_resumo = "escolha1") {

  if (referencia == FALSE) {
    df_calcs <- df_calcs |>
      dplyr::filter(tipo == "local") |>
      dplyr::select(!tipo)

    if (grepl("bloco7", bloco) & !grepl("morbidade", bloco)) {
      df_calcs_dist_bloco7 <- df_calcs_dist_bloco7 |>
        dplyr::filter(tipo == "local") |>
        dplyr::select(!tipo)
    }
  } else {
    df_calcs <- df_calcs |>
      dplyr::filter(tipo == "referencia") |>
      dplyr::select(!tipo)

    if (grepl("bloco7", bloco) & !grepl("morbidade", bloco)) {
      df_calcs_dist_bloco7 <- df_calcs_dist_bloco7 |>
        dplyr::filter(tipo == "referencia") |>
        dplyr::select(!tipo)
    }
  }

  colunas_summarise <- names(df_calcs)

  df_localidade_aux <- df_localidade |>
    dplyr::summarise() |>
    dplyr::ungroup()

  if (ncol(df_localidade_aux) == 0) {
    for (coluna in colunas_summarise) {
      df_localidade_aux <- cbind(
        df_localidade_aux,
        dplyr::summarise(df_localidade, !!coluna := !!rlang::parse_expr(df_calcs[[coluna]]))
      )
    }
  } else {
    for (coluna in colunas_summarise) {
      df_localidade_aux <- dplyr::full_join(
        df_localidade_aux,
        dplyr::summarise(df_localidade, !!coluna := !!rlang::parse_expr(df_calcs[[coluna]])),
        by = dplyr::join_by(ano)
      )
    }
  }

  if (grepl("bloco7", bloco) & !grepl("morbidade", bloco)) {
    colunas_summarise_dist_bloco7 <- names(df_calcs_dist_bloco7)
    if (nrow(df_localidade_aux) == 1) {
      for (coluna in colunas_summarise_dist_bloco7) {
        df_localidade_aux <- cbind(
          df_localidade_aux,
          dplyr::summarise(df_localidade_aux, !!coluna := !!rlang::parse_expr(df_calcs_dist_bloco7[[coluna]]))
        )
      }
    } else {
      for (coluna in colunas_summarise_dist_bloco7) {
        df_localidade_aux <- dplyr::full_join(
          df_localidade_aux,
          dplyr::summarise(df_localidade_aux |> dplyr::group_by(ano), !!coluna := !!rlang::parse_expr(df_calcs_dist_bloco7[[coluna]])),
          by = dplyr::join_by(ano)
        )
      }
    }
  }

  if (adicionar_localidade == TRUE) {
    sufixo <- ifelse(comp == TRUE | localidade_resumo == "escolha2", "2", "")

    df_localidade_aux |>
      dplyr::mutate(
        class = dplyr::case_when(
          filtros[[paste0("nivel", sufixo)]] == "nacional" | referencia == TRUE ~ dplyr::if_else(
            filtros$comparar == "Não",
            "Brasil (valor de referência)",
            dplyr::if_else(
              filtros$mostrar_referencia == "nao_mostrar_referencia",
              "Brasil",
              "Brasil (valor de referência)"
            )
          ),
          filtros[[paste0("nivel", sufixo)]] == "regional" ~ filtros[[paste0("regiao", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "estadual" ~ filtros[[paste0("estado", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "macro" ~ filtros[[paste0("macro", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "micro" ~ filtros[[paste0("micro", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "municipal" ~ filtros[[paste0("municipio", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
        )
      ) |>
      dplyr::ungroup()
  } else {
    df_localidade_aux |> dplyr::ungroup()
  }
}


filtra_localidade <- function(data, filtros, comp = FALSE, referencia = FALSE, localidade_resumo = "escolha1", add_class = TRUE) {
  # Definindo o sufixo
  sufixo <- ifelse(comp == TRUE | localidade_resumo == "escolha2", "2", "")

  # Criando uma função auxiliar que devolve filtros$input ou filtros$input2 a depender do sufixo
  get_filtro <- function(var) filtros[[paste0(var, sufixo)]]

  # Obtendo o nível selecionado
  nivel_selecionado <- get_filtro("nivel")

  # Filtrando o período de análise escolhido
  data <- dplyr::filter(
    data,
    ano >= filtros$ano2[1] & ano <= filtros$ano2[2]
  )

  # Se for para a referência, não precisamos fazer nenhum filtro adicional
  if (referencia) return(data |> dplyr::mutate(class = "Brasil (valor de referência)"))

  # Se não for para a referência, filtrando a localidade selecionada
  data <- switch(
    nivel_selecionado,
    "nacional" = data,
    "regional" = dplyr::filter(data, regiao == get_filtro("regiao")),
    "estadual" = dplyr::filter(data, uf == get_filtro("estado")),
    "macro" = dplyr::filter(data, macro_r_saude == get_filtro("macro") & uf == get_filtro("estado_macro")),
    "micro" = dplyr::filter(data, r_saude == get_filtro("micro") & uf == get_filtro("estado_micro")),
    "municipal" = dplyr::filter(data, municipio == get_filtro("municipio") & uf == get_filtro("estado_municipio")),
    "municipios_semelhantes" = dplyr::filter(
      data,
      grupo_kmeans == tabela_aux_municipios$grupo_kmeans[
        tabela_aux_municipios$municipio == get_filtro("municipio") &
          tabela_aux_municipios$uf == get_filtro("estado_municipio")
        ]
    )
  )

  # Adicionando a coluna "class" (caso solicitado)
  if (add_class) {
    data <- dplyr::mutate(
      data,
      class = dplyr::case_when(
        nivel_selecionado == "nacional" | referencia ~ dplyr::if_else(
          filtros$comparar == "Não",
          "Brasil (valor de referência)",
          dplyr::if_else(
            filtros$mostrar_referencia == "nao_mostrar_referencia",
            "Brasil",
            "Brasil (valor de referência)"
          )
        ),
        nivel_selecionado == "regional" ~ get_filtro("regiao"),
        nivel_selecionado == "estadual" ~ get_filtro("estado"),
        nivel_selecionado == "macro" ~ get_filtro("macro"),
        nivel_selecionado == "micro" ~ get_filtro("micro"),
        nivel_selecionado == "municipal" ~ get_filtro("municipio"),
        nivel_selecionado == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
      )
    )
  }

  data
}

processa_bloco7 <- function(data, input, filtros, cols_fn, indicadores_fn, localidade_resumo = "escolha1", comp = FALSE, referencia = FALSE) {
  ## 1. Filtra e garante data.table ----------
  dt <- data |>
    data.table::as.data.table()

  ## 2. Define colunas dinamicamente ----------
  cols <- cols_fn(input)

  ## 3. Função genérica para cálculo de indicadores -------
  calcula_indicadores <- function(grp) {
    dt[,
       {
         s <- lapply(cols, \(v) sum(as.numeric(unlist(.SD[, v, with = FALSE])), na.rm = TRUE))
         names(s) <- names(cols)
         indicadores_fn(s)
       },
       by = grp,
       .SDcols = unique(unlist(cols))
    ]
  }

  ## 4. Aplica para os dois níveis ----------
  df_graficos <- calcula_indicadores(c("ano", "class"))
  df_resumo <- calcula_indicadores("class")

  list(graficos = df_graficos, resumo = df_resumo)
}

filtra_colunas_evitaveis_principais <- function(
    data,
    filtros,
    pesos,
    momentos,
    comp = FALSE,
    referencia = FALSE,
    resumo = FALSE,
    localidade_resumo = "escolha1",
    prefixo_coluna = "evitaveis_fetal"
) {

  # Preparação inicial
  dt <- data.table::as.data.table(data)

  todas_fases <- length(momentos) == 3
  pesos_pat   <- paste(pesos, collapse = "|")
  momentos_pat <- paste(momentos, collapse = "|")

  # Seleção das colunas alvo
  alvo_cols <- names(dt)[
    grepl(prefixo_coluna, names(dt)) &
      grepl(pesos_pat,    names(dt)) &
      (todas_fases | grepl(momentos_pat, names(dt)))
  ]

  # Filtragem por localidade e escopo
  dt <- filtra_localidade(as.data.frame(dt), filtros, comp, referencia, localidade_resumo) |>
    data.table::as.data.table()

  dt <- dt[class == "Brasil (valor de referência)", class := "Brasil"]

  # Define variáveis de agregação conforme o argumento `resumo`
  agrupadores <- if (resumo) "class" else c("ano", "class")

  # Agregação e cálculo das proporções
  out <- dt[
    ,
    lapply(.SD, sum, na.rm = TRUE),
    by = agrupadores,
    .SDcols = alvo_cols
  ]

  out[, total := rowSums(.SD), .SDcols = alvo_cols]

  out[, (alvo_cols) := lapply(.SD, \(x) x / total * 100), .SDcols = alvo_cols]

  # Reestruturação em formato longo
  out_long <- data.table::melt(
    out,
    id.vars = agrupadores,
    measure.vars  = alvo_cols,
    variable.name = "grupo_cid10",
    value.name = "porc_obitos"
  )

  return(as.data.frame(out_long))
}

processa_causas <- function(
    data,
    pesos,
    momentos,
    filtros,
    comp = FALSE,
    referencia = FALSE,
    grupos = NULL,
    localidade_resumo = "escolha1",
    prefixo_coluna = "evitaveis_fetal"
) {

  # 1. Preparar os dados
  dt_graficos <- data.table::as.data.table(
    filtra_colunas_evitaveis_principais(
      data = data,
      comp = comp,
      referencia = referencia,
      filtros = filtros,
      pesos = pesos,
      momentos = momentos,
      prefixo_coluna = prefixo_coluna
    )
  )

  dt_resumo <- data.table::as.data.table(
    filtra_colunas_evitaveis_principais(
      data = data,
      comp = comp,
      referencia = referencia,
      resumo = TRUE,
      filtros = filtros,
      pesos = pesos,
      momentos = momentos,
      localidade_resumo = localidade_resumo,
      prefixo_coluna = prefixo_coluna
    )
  )

  # 2. Filtragem por grupos (se houver)
  if (!is.null(grupos)) {
    padrao <- paste(grupos, collapse = "|")
    dt_graficos[, grupo_cid10 := as.character(grupo_cid10)]
    dt_graficos[, grupo_cid10 := data.table::fifelse(
      grepl(padrao, grupo_cid10),
      grupo_cid10,
      "Grupos não selecionados"
    )]
    dt_resumo[, grupo_cid10 := as.character(grupo_cid10)]
    dt_resumo[, grupo_cid10 := data.table::fifelse(
      grepl(padrao, grupo_cid10),
      grupo_cid10,
      "Grupos não selecionados"
    )]
  }

  # 3. Renomear grupos
  ## Função para renomear grupos dentro do data.table
  renomeia_grupos <- function(dt, prefixo_coluna) {
    # Garante que 'grupo_cid10' não tem NA e é character
    dt[, grupo_cid10 := as.character(grupo_cid10)]
    dt[is.na(grupo_cid10), grupo_cid10 := ""]

    if (startsWith(prefixo_coluna, "evitaveis")) {
      dt[, grupo_cid10 := data.table::fcase(
        grepl("imunoprevencao", grupo_cid10), "Imunoprevenção",
        grepl("mulher_gestacao", grupo_cid10), "Adequada atenção à mulher na gestação",
        grepl("parto", grupo_cid10), "Adequada atenção à mulher no parto",
        grepl("recem_nascido", grupo_cid10), "Adequada atenção ao recém nascido",
        grepl("tratamento", grupo_cid10), "Ações de diagnóstico e tratamento adequado",
        grepl("saude", grupo_cid10), "Ações de promoção à saúde vinculadas a ações de atenção",
        grepl("mal_definidas", grupo_cid10), "Causas mal definidas",
        grepl("nao_aplica", grupo_cid10), "Causa básica não se aplica a um óbito fetal",
        grepl("outros", grupo_cid10), "Causas não claramente evitáveis",
        default = "Grupos não selecionados"
      )]
    } else if (startsWith(prefixo_coluna, "principais")) {
      dt[, grupo_cid10 := data.table::fcase(
        grepl("prematuridade", grupo_cid10), "Prematuridade",
        grepl("infeccoes", grupo_cid10), "Infecções",
        grepl("asfixia", grupo_cid10), "Asfixia/Hipóxia",
        grepl("ma_formacao", grupo_cid10), "Anomalia congênita",
        grepl("respiratorias", grupo_cid10), "Afecções respiratórias do recém-nascido",
        grepl("gravidez", grupo_cid10), "Fatores maternos relacionados à gravidez",
        grepl("afeccoes", grupo_cid10), "Afecções originais no período perinatal",
        grepl("mal_definida", grupo_cid10), "Mal definidas",
        grepl("outros", grupo_cid10), "Demais causas",
        default = "Grupos não selecionados"
      )]
    } else {
      stop("Prefixo de coluna não reconhecido para renomeação dos grupos.")
    }

    return(dt)
  }

  dt_graficos <- renomeia_grupos(dt_graficos, prefixo_coluna)
  dt_resumo <- renomeia_grupos(dt_resumo, prefixo_coluna)

  # 4. Fator ordenado
  ## Definições de níveis para 'evitaveis_fetal'
  niveis_evitaveis_fetal <- c(
    "Adequada atenção à mulher na gestação",
    "Adequada atenção à mulher no parto",
    "Imunoprevenção",
    "Grupos não selecionados",
    "Causas mal definidas",
    "Causa básica não se aplica a um óbito fetal",
    "Causas não claramente evitáveis"
  )

  ## Definições de níveis gerais para outros casos (ex: evitaveis não fetais)
  niveis_evitaveis_geral <- c(
    "Imunoprevenção",
    "Adequada atenção à mulher na gestação",
    "Adequada atenção à mulher no parto",
    "Adequada atenção ao recém nascido",
    "Ações de diagnóstico e tratamento adequado",
    "Ações de promoção à saúde vinculadas a ações de atenção",
    "Causas mal definidas",
    "Grupos não selecionados",
    "Causas não claramente evitáveis"
  )

  ## Definições para o grupo 'principais' (exemplo: principais_fetal)
  niveis_principais <- c(
    "Fatores maternos relacionados à gravidez",
    "Asfixia/Hipóxia",
    "Anomalia congênita",
    "Infecções",
    "Afecções respiratórias do recém-nascido",
    "Prematuridade",
    "Afecções originais no período perinatal",
    "Mal definidas",
    "Grupos não selecionados",
    "Demais causas"
  )

  # Escolha o padrão de renomeação e níveis com base no prefixo_coluna
  if (startsWith(prefixo_coluna, "evitaveis")) {
    # Níveis a usar: fetal ou geral, conforme o prefixo completo
    niveis <- if (prefixo_coluna == "evitaveis_fetal") niveis_evitaveis_fetal else niveis_evitaveis_geral

  } else if (startsWith(prefixo_coluna, "principais")) {
    niveis <- niveis_principais

  } else {
    stop("Prefixo de coluna não reconhecido para definição de grupos e níveis.")
  }

  dt_graficos[, grupo_cid10 := factor(grupo_cid10, levels = niveis)]
  dt_resumo[, grupo_cid10 := factor(grupo_cid10, levels = niveis)]

  # 5. Agrupamento final
  dt_graficos <- dt_graficos[
    ,
    .(porc_obitos = round(sum(porc_obitos, na.rm = TRUE), 1)),
    by = .(ano, grupo_cid10, class)
  ]

  dt_resumo <- dt_resumo[
    ,
    .(porc_obitos = round(sum(porc_obitos, na.rm = TRUE), 1)),
    by = .(grupo_cid10)
  ]

  # 6. Ordenar resumo
  dt_resumo <- dt_resumo[order(-porc_obitos)]

  # 7. Retornar lista
  return(
    if (startsWith(prefixo_coluna, "evitaveis")) {
      list(
        graficos = as.data.frame(dt_graficos),
        resumo = as.data.frame(dt_resumo) |>
          dplyr::filter(!(grupo_cid10 %in% c("Causas mal definidas", "Causas não claramente evitáveis"))) |>
          dplyr::summarise(porc_obitos_evitaveis = sum(porc_obitos))
      )
    } else {
      list(
        graficos = as.data.frame(dt_graficos),
        resumo = as.data.frame(dt_resumo) |> dplyr::arrange(desc(porc_obitos))
      )
    }

  )
}



cria_caixa_server <- function(dados, indicador, titulo, tem_meta = FALSE, nivel_de_analise, tipo_referencia, valor_de_referencia, valor_indicador = NULL, tipo = "porcentagem", invertido = FALSE, texto_caixa = NULL, cor = NULL, texto_footer = NULL, tamanho_caixa = 300, fonte_titulo = "fonte-grande", fonte_comparacao = "fonte-media", pagina, width_caixa = 12, retornar_caixa_completa = TRUE) {

  if (is.null(valor_indicador)) {
    if (isTruthy(dados[[indicador]])) {
      if (as.integer(dados[[indicador]]) == dados[[indicador]]) {
        valor_indicador_aux <- as.integer(dados[[indicador]])
      } else {
        valor_indicador_aux <- dados[[indicador]]
      }
    } else {
      valor_indicador_aux <- NaN
    }
  } else {
    if (isTruthy(valor_indicador)) {
      if (as.integer(valor_indicador) == valor_indicador) {
        valor_indicador_aux <- as.integer(valor_indicador)
      } else {
        valor_indicador_aux <- valor_indicador
      }
    } else {
      valor_indicador_aux <- NaN
    }
  }

  if (isTruthy(valor_de_referencia) & isTruthy(valor_indicador_aux)) {
    if (length(valor_de_referencia) == 1) {
      if (valor_indicador_aux == 0 & valor_de_referencia == 0) {
        razao <- 0
      } else {
        razao <- round(valor_indicador_aux/valor_de_referencia, 1)
      }
    } else {
      if (valor_indicador_aux < min(valor_de_referencia)) {
        razao <- round(valor_indicador_aux/min(valor_de_referencia), 1)
      } else if (valor_indicador_aux > max(valor_de_referencia)) {
        razao <- round(valor_indicador_aux/max(valor_de_referencia), 1)
      } else {
        razao <- 0
      }
    }
  } else {
    razao <- 0
  }

  if (length(valor_de_referencia) == 1) {
    if (razao >= 2) {
      valor_comp <- razao
    } else {
      valor_comp <- round(100 - 100*valor_indicador_aux/valor_de_referencia, 1)
    }
  } else {
    if (is.nan(valor_indicador_aux)) {
      valor_comp <- NaN
    } else if (valor_indicador_aux < min(valor_de_referencia)) {
      if (razao >= 2) {
        valor_comp <- razao
      } else {
        valor_comp <- round(100 - 100*valor_indicador_aux/min(valor_de_referencia), 1)
      }
    } else if (valor_indicador_aux > max(valor_de_referencia)) {
      if (razao >= 2) {
        valor_comp <- razao
      } else {
        valor_comp <- round(100 - 100*valor_indicador_aux/max(valor_de_referencia), 1)
      }
    } else {
      valor_comp <- 0
    }
  }

  valor_comp_formatado <- formatC(abs(valor_comp), big.mark = '.', decimal.mark = ',')
  valor_de_referencia_formatado <- formatC(valor_de_referencia, big.mark = '.', decimal.mark = ',')

  if (tem_meta == FALSE) {
    tipo_referencia <- "média nacional"
  } else {
    tipo_referencia <- tipo_referencia
  }

  if (tipo_referencia == "média nacional" & nivel_de_analise == "nacional" & is.null(texto_footer)) {
    if (is.nan(valor_comp)) {
      texto_footer <- "Comparação não aplicável"
      cor <- "lightgrey"
    } else {
      texto_footer <- "Comparação não aplicável (este é o valor de referência)"
      cor <- "lightgrey"
    }
  }

  if (is.null(cor)) {
    if (length(valor_de_referencia) == 1) {
      if (invertido == TRUE) {
        if (razao >= 2) {
          cor_comp <- "#a2e4b8"
        } else {
          cor_comp <- dplyr::case_when(
            valor_comp > 0 ~ "#d998a0",  #vermelho
            valor_comp <= 0 ~ "#a2e4b8",  #verde
            #valor_comp == 0 ~ "#f1eb99", #amarelo
            is.nan(valor_comp) ~ "lightgrey"
          )
        }
      } else {
        if (razao >= 2) {
          cor_comp <- "#d998a0"
        } else {
          cor_comp <- dplyr::case_when(
            valor_comp < 0 ~ "#d998a0",  #vermelho
            valor_comp >= 0 ~ "#a2e4b8",  #verde
            #valor_comp == 0 ~ "#f1eb99", #amarelo
            is.nan(valor_comp) ~ "lightgrey"
          )
        }
      }
    } else {
      cor_comp <- dplyr::case_when(
        valor_comp == 0 ~ "#a2e4b8",  #verde
        valor_comp != 0 ~ "#d998a0",  #vermelho
        is.nan(valor_comp) ~ "lightgrey"
      )
    }
  } else {
    cor_comp <- cor
  }

  if (is.null(texto_caixa)) {
    if (is.nan(valor_indicador_aux)) {
      texto <- "---"
    } else {
      if (tipo == "porcentagem") {
        texto <- "{formatC(valor_indicador_aux, big.mark = '.', decimal.mark = ',')}%"
      } else if (tipo == "km") {
        texto <- "{formatC(valor_indicador_aux, big.mark = '.', decimal.mark = ',')} km"
      } else {
        texto <-  "{formatC(valor_indicador_aux, big.mark = '.', decimal.mark = ',')}"
      }
    }
  } else {
    texto <- texto_caixa
  }

  if (tipo_referencia == "média nacional") {
    final_texto_comp <- glue::glue(" ({valor_de_referencia_formatado}{ifelse(tipo == 'porcentagem', '%', '')}, média nacional)")
  } else {
    if (invertido == FALSE) {
      final_texto_comp <- glue::glue(", de no máximo {valor_de_referencia_formatado}{ifelse(tipo == 'porcentagem', '%', '')} ({tipo_referencia})")
    } else {
      final_texto_comp <- glue::glue(", de no mínimo {valor_de_referencia_formatado}{ifelse(tipo == 'porcentagem', '%', '')} ({tipo_referencia})")
    }
  }

  if (is.null(texto_footer)) {
    if (length(valor_de_referencia) != 1) {
      if (razao >= 2) {
        if (valor_indicador_aux < min(valor_de_referencia)) {
          texto_comp <- dplyr::case_when(
            tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-down'> </i> {valor_comp_formatado} vezes menor que o valor de referência mínimo, de {valor_de_referencia_formatado[1]}% ({tipo_referencia})",
            tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-down'> </i> {valor_comp_formatado} vezes menor que o valor de referência mínimo, de {valor_de_referencia_formatado[1]} ({tipo_referencia})"
          )
        } else {
          texto_comp <- dplyr::case_when(
            tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado} vezes maior que o valor de referência máximo, de {valor_de_referencia_formatado[2]}% ({tipo_referencia})",
            tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado} vezes maior que o valor de referência máximo, de {valor_de_referencia_formatado[2]} ({tipo_referencia})"
          )
        }
      } else {
        texto_comp <- dplyr::case_when(
          valor_comp < 0 & tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado}% maior que o valor de referência máximo, de {valor_de_referencia_formatado[2]}% ({tipo_referencia})",
          valor_comp < 0 & tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado}% maior que o valor de referência máximo, de {valor_de_referencia_formatado[2]} ({tipo_referencia})",
          valor_comp > 0 & tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-down'> </i> {valor_comp_formatado}% menor que o valor de referência mínimo, de {valor_de_referencia_formatado[1]}% ({tipo_referencia})",
          valor_comp > 0 & tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-down'> </i> {valor_comp_formatado}% menor que o valor de referência mínimo, de {valor_de_referencia_formatado[1]} ({tipo_referencia})",
          valor_comp == 0 ~ "Dentro da faixa de referência, de {valor_de_referencia_formatado[1]} a {valor_de_referencia_formatado[2]}% ({tipo_referencia})",
          is.nan(valor_comp) ~ "Comparação não aplicável"
        )
      }
    } else {
      if (razao >= 2) {
        texto_comp <- dplyr::case_when(
          tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado} vezes maior que o valor de referência{final_texto_comp}",
          tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado} vezes maior que o valor de referência{final_texto_comp}"
        )
      } else {
        texto_comp <- dplyr::case_when(
          valor_comp < 0 & tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado}% maior que o valor de referência{final_texto_comp}",
          valor_comp < 0 & tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado}% maior que o valor de referência{final_texto_comp}",
          valor_comp > 0 & tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-down'> </i> {valor_comp_formatado}% menor que o valor de referência{final_texto_comp}",
          valor_comp > 0 & tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-down'> </i> {valor_comp_formatado}% menor que o valor de referência{final_texto_comp}",
          valor_comp == 0 ~ "Igual ao valor de referência ({tipo_referencia})",
          is.nan(valor_comp) ~ "Comparação não aplicável"
        )
      }

    }
  } else {
    texto_comp <- texto_footer
  }

  fonte_texto <- dplyr::if_else(floor(log10(valor_indicador_aux)) + 1 < 7 | is.nan(valor_indicador_aux), "fonte-destaque-caixas1", "fonte-destaque-caixas2")

  style_texto <- glue::glue("height: 28%; overflow: auto; padding: 0 10px; display: flex; justify-content: center; text-align: center;")

  if (retornar_caixa_completa == TRUE) {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}px; overflow: auto; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(class = fonte_titulo, style = glue::glue("height: 31%; overflow: auto; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>"))),
      div(style = "height: 3%"),
      div(class = fonte_texto, style = style_texto, HTML(glue::glue("<b> {glue::glue(texto)} </b>"))),
      div(class = fonte_comparacao, style = glue::glue("overflow: auto; height: 38%; padding: 10px 5px; display: flex; align-items:center; justify-content:center; text-align: center; background-color: {cor_comp};"), HTML(glue::glue("<b> {glue::glue(texto_comp)} </b>")))
    )
  } else {
    div(
      style = glue::glue("height: {24 + tamanho_caixa}px; box-shadow: 0 0 1px rgba(0,0,0,.125),0 1px 3px rgba(0,0,0,.2); background: white; padding-top: 24px; position: relative; top: -24px;"),
      div(class = fonte_titulo, style = glue::glue("height: 31%; overflow: auto; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>"))),
      div(style = "height: 3%"),
      div(class = fonte_texto, style = style_texto, HTML(glue::glue("<b> {glue::glue(texto)} </b>"))),
      div(class = fonte_comparacao, style = glue::glue("overflow: auto; height: 38%; padding: 10px 5px; display: flex; align-items:center; justify-content:center; text-align: center; background-color: {cor_comp};"), HTML(glue::glue("<b> {glue::glue(texto_comp)} </b>")))
    )
  }
}


cria_caixa_conjunta_bloco5 <- function(dados, titulo, indicador, tamanho_caixa = 300, fonte_titulo = "fonte-grande", width_caixa = 12) {

  if (indicador == "baixo peso") {
    valor_indicador1 <- dados[["porc_baixo_peso_menor_1000"]]
    valor_indicador2 <- dados[["porc_baixo_peso_1000_a_1499"]]
    valor_indicador3 <- dados[["porc_baixo_peso_1500_a_2499"]]
  }

  if (indicador == "prematuridade") {
    valor_indicador1 <- dados[["porc_premat_menos_de_28_semanas"]]
    valor_indicador2 <- dados[["porc_premat_28_a_32_semanas"]]
    valor_indicador3 <- dados[["porc_premat_33_a_34_semanas"]]
    valor_indicador4 <- dados[["porc_premat_35_a_36_semanas"]]
    valor_indicador5 <- dados[["porc_premat_faltantes"]]
  }

  if (is.nan(valor_indicador1)) {
    texto1 <- "---"
  } else {
    texto1 <- "{formatC(valor_indicador1, big.mark = '.', decimal.mark = ',')}%"
  }

  if (is.nan(valor_indicador2)) {
    texto2 <- "---"
  } else {
    texto2 <- "{formatC(valor_indicador2, big.mark = '.', decimal.mark = ',')}%"
  }

  if (is.nan(valor_indicador3)) {
    texto3 <- "---"
  } else {
    texto3 <- "{formatC(valor_indicador3, big.mark = '.', decimal.mark = ',')}%"
  }

  if (indicador == "prematuridade") {
    if (is.nan(valor_indicador4)) {
      texto4 <- "---"
    } else {
      texto4 <- "{formatC(valor_indicador4, big.mark = '.', decimal.mark = ',')}%"
    }

    if (is.nan(valor_indicador5)) {
      texto5 <- "---"
    } else {
      texto5 <- "{formatC(valor_indicador5, big.mark = '.', decimal.mark = ',')}%"
    }
  }

  style_texto <- "display: flex; justify-content: center; text-align: center; margin-bottom: 0"
  style_descricao <- "display: flex; padding: 0 5px; justify-content: center; text-align: center; margin-bottom: 0"

  if (indicador == "baixo peso") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}px; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(class = fonte_titulo, style = glue::glue("height: 20%; padding: 0px 10px 10px 10px;"), HTML(glue::glue("<b> {titulo} </b>"))),
      hr(),
      div(
        style = "height: 65%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "possuem peso < 1000 g")
        ),
        div(
          p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "possuem peso de 1000 a 1499 g")
        ),
        div(
          p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "possuem peso de 1500 a 2499 g")
        )
      )
    )
  } else if (indicador == "prematuridade") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}px; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(class = fonte_titulo, style = glue::glue("height: 22%; padding: 0px 10px 10px 10px;"), HTML(glue::glue("<b> {titulo} </b>"))),
      hr(),
      div(
        style = "height: 63%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "nasceram com menos de 28 semanas")
        ),
        div(
          p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "nasceram de 28 a 32 semanas")
        ),
        div(
          p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "nasceram de 33 a 34 semanas")
        ),
        div(
          p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
          p(style = style_descricao, "nasceram de 35 a 36 semanas")
        ),        div(
          p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto5)} </b>"))),
          p(style = style_descricao, "não possuem informação")
        )
      )
    )

  }

}

cria_caixa_conjunta_bloco7 <- function(dados, titulo, indicador, tamanho_caixa = 303, fonte_titulo = "fonte-grande", retornar_caixa_completa = TRUE, width_caixa = 12) {

  pega_valor_formatado <- function(valor) {
    req(valor)
    if (is.nan(valor)) {
      "---"
    } else {
      glue::glue("{formatC(valor, big.mark = '.', decimal.mark = ',')}%")
    }
  }

  indicadores_campos <- list(
    "fetal peso por idade gestacional" = c(
      "porc_obito_fetal_menos_1000",
      "porc_obito_fetal_1000_1499",
      "porc_obito_fetal_1500_2499",
      "porc_obito_fetal_2500_mais",
      "porc_obito_fetal_peso_faltante"
    ),
    "fetal momento do obito por peso" = c(
      "porc_obito_fetal_antes",
      "porc_obito_fetal_durante",
      "porc_obito_fetal_momento_faltante"
    ),
    "perinatal momento do obito por peso" = c(
      "porc_obito_perinatal_antes",
      "porc_obito_perinatal_durante",
      "porc_obito_perinatal_0_dias",
      "porc_obito_perinatal_1_a_6_dias",
      "porc_obito_perinatal_momento_faltante"
    ),
    "perinatal peso por momento do obito" = c(
      "porc_obito_perinatal_menos_1000",
      "porc_obito_perinatal_1000_1499",
      "porc_obito_perinatal_1500_2499",
      "porc_obito_perinatal_2500_mais",
      "porc_obito_perinatal_peso_faltante"
    ),
    "neonatal momento do obito por peso" = c(
      "porc_obito_neonatal_0_dias",
      "porc_obito_neonatal_1_a_6_dias",
      "porc_obito_neonatal_7_a_27_dias"
    ),
    "neonatal peso por momento do obito" = c(
      "porc_obito_neonatal_menos_1000",
      "porc_obito_neonatal_1000_1499",
      "porc_obito_neonatal_1500_2499",
      "porc_obito_neonatal_2500_mais",
      "porc_obito_neonatal_peso_faltante"
    )
  )

  indicadores_descricoes <- list(
    "fetal peso por idade gestacional" = c(
      "possuem peso menor que 1000 g",
      "possuem peso de 1000 a 1499 g",
      "possuem peso de 1500 a 2499 g",
      "possuem peso maior ou igual a 2500g",
      "não tem informação"
    ),
    "fetal momento do obito por peso" = c(
      "ocorreram antes do parto",
      "ocorreram durante o parto",
      "não têm informação"
    ),
    "perinatal momento do obito por peso" = c(
      "ocorreram antes do parto",
      "ocorreram durante o parto",
      "ocorreram no dia 0 de vida",
      "ocorreram com 1 a 6 dias de vida",
      "não tem informação"
    ),
    "perinatal peso por momento do obito" = c(
      "possuem peso menor que 1000 g",
      "possuem peso de 1000 a 1499 g",
      "possuem peso de 1500 a 2499 g",
      "possuem peso maior ou igual a 2500g",
      "não tem informação"
    ),
    "neonatal momento do obito por peso" = c(
      "ocorreram no dia 0 de vida",
      "ocorreram com 1 a 6 dias de vida",
      "ocorreram com 7 a 27 dias de vida"
    ),
    "neonatal peso por momento do obito" = c(
      "possuem peso menor que 1000 g",
      "possuem peso de 1000 a 1499 g",
      "possuem peso de 1500 a 2499 g",
      "possuem peso maior ou igual a 2500g",
      "não tem informação"
    )
  )

  style_texto <- "display: flex; justify-content: center; text-align: center; margin-bottom: 0"
  style_descricao <- "display: flex; padding: 0 5px; justify-content: center; text-align: center; margin-bottom: 0"

  valores <- indicadores_campos[[indicador]]
  descricoes <- indicadores_descricoes[[indicador]]

  blocos <- purrr::map2(valores, descricoes, ~{
    valor <- dados[[.x]]
    list(texto = pega_valor_formatado(valor), descricao = .y)
  })

  padding_titulo <- if (indicador %in% c("perinatal momento do obito por peso", "perinatal peso por momento do obito")) {
    "0px 10px 0px 10px"
  } else {
    "0px 10px 10px 10px"
  }

  if (retornar_caixa_completa == TRUE) {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}px; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(
        class = fonte_titulo,
        style = glue::glue("height: 15%; padding: {padding_titulo}; overflow: auto"),
        HTML(glue::glue("<b> {titulo} </b>"))
      ),
      hr(),
      div(
        style = "height: 75%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        purrr::map(blocos, ~{
          div(
            p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> { .x$texto } </b>"))),
            p(style = style_descricao, .x$descricao)
          )
        })
      )
    )
  } else {
    div(
      style = glue::glue("height: {24 + tamanho_caixa}px; box-shadow: 0 0 1px rgba(0,0,0,.125),0 1px 3px rgba(0,0,0,.2); background: white; padding-top: 24px; position: relative; top: -24px;"),
      div(
        class = fonte_titulo,
        style = glue::glue("height: 15%; padding: {padding_titulo}; overflow: auto"),
        HTML(glue::glue("<b> {titulo} </b>"))
      ),
      hr(),
      div(
        style = "height: 75%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        purrr::map(blocos, ~{
          div(
            p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> { .x$texto } </b>"))),
            p(style = style_descricao, .x$descricao)
          )
        })
      )
    )
  }
}


cria_caixa_principais_evitaveis_bloco7 <- function(dados, titulo, tamanho_caixa = 330, fonte_titulo = "fonte-grande", width_caixa = 12, retornar_caixa_completa = TRUE) {

  style_porc <- "display: flex; justify-content: center; text-align: center; margin-bottom: 0"
  style_grupo <- "display: flex; padding: 0 5px; justify-content: center; text-align: center; margin-bottom: 0"

  # Processa os dados (formata porcentagens e textos)
  blocos <- purrr::map2(dados$grupo_cid10, dados$porc_obitos, function(grupo, porc) {
    porc_texto <- if (is.nan(porc)) {
      "---"
    } else {
      glue::glue("{formatC(porc, big.mark = '.', decimal.mark = ',')}%")
    }

    list(
      porc = porc_texto,
      grupo = grupo
    )
  })

  # Monta a caixa com map dos blocos
  if (retornar_caixa_completa == TRUE) {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}px; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(
        class = fonte_titulo,
        style = glue::glue("height: 15%; padding: 0px 10px 0px 10px; overflow: auto"),
        HTML(glue::glue("<b> {titulo} </b>"))
      ),
      hr(),
      div(
        style = glue::glue("height: 75%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;"),
        purrr::map(blocos, ~{
          div(
            p(class = "fonte-destaque-caixas2", style = style_porc, HTML(glue::glue("<b> { .x$porc } </b>"))),
            p(style = style_grupo, HTML(glue::glue("pertencem ao grupo de causa { .x$grupo }")))
          )
        })
      )
    )
  } else {
    div(
      style = glue::glue("height: {24 + tamanho_caixa}px; box-shadow: 0 0 1px rgba(0,0,0,.125),0 1px 3px rgba(0,0,0,.2); background: white; padding-top: 24px; position: relative; top: -24px;"),
      div(
        class = fonte_titulo,
        style = glue::glue("height: 15%; padding: 0px 10px 0px 10px; overflow: auto"),
        HTML(glue::glue("<b> {titulo} </b>"))
      ),
      hr(),
      div(
        style = glue::glue("height: 75%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;"),
        purrr::map(blocos, ~{
          div(
            p(class = "fonte-destaque-caixas2", style = style_porc, HTML(glue::glue("<b> { .x$porc } </b>"))),
            p(style = style_grupo, HTML(glue::glue("pertencem ao grupo de causa { .x$grupo }")))
          )
        })
      )
    )
  }

}

# cria_caixa_conjunta_bloco7 <- function(dados, titulo, indicador, tamanho_caixa = "303px", fonte_titulo = "fonte-grande", width_caixa = 12) {
#
#   if (indicador == "fetal peso por idade gestacional") {
#     valor_indicador1 <- dados[["menos_1000_distribuicao_peso_fetal"]]
#     valor_indicador2 <- dados[["de_1000_1499_distribuicao_peso_fetal"]]
#     valor_indicador3 <- dados[["de_1500_2499_distribuicao_peso_fetal"]]
#     valor_indicador4 <- dados[["mais_2500_distribuicao_peso_fetal"]]
#     valor_indicador5 <- dados[["faltante_distribuicao_peso_fetal"]]
#   }
#
#   if (indicador == "fetal momento do obito por peso") {
#     valor_indicador1 <- dados[["antes_distribuicao_moment_obito_fetal"]]
#     valor_indicador2 <- dados[["durante_distribuicao_moment_obito_fetal"]]
#     valor_indicador3 <- dados[["faltante_distribuicao_moment_obito_fetal"]]
#   }
#
#   if (indicador == "perinatal momento do obito por peso") {
#     valor_indicador1 <- dados[["antes_distribuicao_moment_obito_perinat"]]
#     valor_indicador2 <- dados[["durante_distribuicao_moment_obito_perinat"]]
#     valor_indicador3 <- dados[["dia_0_distribuicao_moment_obito_perinat"]]
#     valor_indicador4 <- dados[["dia_1_6_distribuicao_moment_obito_perinat"]]
#     valor_indicador5 <- dados[["faltante_distribuicao_moment_obito_perinat"]]
#   }
#
#   if (indicador == "perinatal peso por momento do obito") {
#     valor_indicador1 <- dados[["menos_1000_distribuicao_peso_perinat"]]
#     valor_indicador2 <- dados[["de_1000_1499_distribuicao_peso_perinat"]]
#     valor_indicador3 <- dados[["de_1500_2499_distribuicao_peso_perinat"]]
#     valor_indicador4 <- dados[["mais_2500_distribuicao_peso_perinat"]]
#     valor_indicador5 <- dados[["faltante_distribuicao_peso_perinat"]]
#   }
#
#   if (indicador == "neonatal momento do obito por peso") {
#     valor_indicador1 <- dados[["dia_0_distribuicao_moment_obito_neonat"]]
#     valor_indicador2 <- dados[["dia_1_6distribuicao_moment_obito_neonat"]]
#     valor_indicador3 <- dados[["dia_7_27distribuicao_moment_obito_neonat"]]
#   }
#
#   if (indicador == "neonatal peso por momento do obito") {
#     valor_indicador1 <- dados[["menos_1000_distribuicao_peso_neonat"]]
#     valor_indicador2 <- dados[["de_1000_1499_distribuicao_peso_neonat"]]
#     valor_indicador3 <- dados[["de_1500_2499_distribuicao_peso_neonat"]]
#     valor_indicador4 <- dados[["mais_2500_distribuicao_peso_neonat"]]
#     valor_indicador5 <- dados[["faltante_distribuicao_peso_neonat"]]
#   }
#
#   if (is.nan(valor_indicador1)) {
#     texto1 <- "---"
#   } else {
#     texto1 <- "{formatC(valor_indicador1, big.mark = '.', decimal.mark = ',')}%"
#   }
#
#   if (is.nan(valor_indicador2)) {
#     texto2 <- "---"
#   } else {
#     texto2 <- "{formatC(valor_indicador2, big.mark = '.', decimal.mark = ',')}%"
#   }
#
#   if (is.nan(valor_indicador3)) {
#     texto3 <- "---"
#   } else {
#     texto3 <- "{formatC(valor_indicador3, big.mark = '.', decimal.mark = ',')}%"
#   }
#
#   if (indicador == "fetal peso por idade gestacional" | indicador == "perinatal momento do obito por peso"|
#       indicador == "perinatal peso por momento do obito" | indicador == "neonatal peso por momento do obito"){
#
#     if (is.nan(valor_indicador4)) {
#       texto4 <- "---"
#     } else {
#       texto4 <- "{formatC(valor_indicador4, big.mark = '.', decimal.mark = ',')}%"
#     }
#
#   }
#
#   if (indicador == "fetal peso por idade gestacional" | indicador == "perinatal momento do obito por peso"|
#       indicador == "perinatal peso por momento do obito" | indicador == "neonatal peso por momento do obito"){
#
#     if (is.nan(valor_indicador5)) {
#       texto5 <- "---"
#     } else {
#       texto5 <- "{formatC(valor_indicador5, big.mark = '.', decimal.mark = ',')}%"
#     }
#   }
#
#   style_texto <- "display: flex; justify-content: center; text-align: center; margin-bottom: 0"
#   style_descricao <- "display: flex; padding: 0 5px; justify-content: center; text-align: center; margin-bottom: 0"
#
#   if (indicador == "fetal peso por idade gestacional") {
#     bs4Dash::box(
#       style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
#       width = width_caixa,
#       collapsible = FALSE,
#       headerBorder = FALSE,
#       div(class = fonte_titulo, style = glue::glue("height: 15%; padding: 0px 10px 10px 10px; overflow: auto"), HTML(glue::glue("<b> {titulo} </b>"))),
#       hr(),
#       div(
#         style = "height: 70%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
#           p(style = style_descricao, "possuem peso menor que 1000 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
#           p(style = style_descricao, "possuem peso de 1000 a 1499 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
#           p(style = style_descricao, "possuem peso de 1500 a 2499 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
#           p(style = style_descricao, "possuem peso maior ou igual a 2500g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto5)} </b>"))),
#           p(style = style_descricao, "não tem informação")
#         )
#       )
#     )
#   } else if (indicador == "fetal momento do obito por peso") {
#     bs4Dash::box(
#       style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
#       width = width_caixa,
#       collapsible = FALSE,
#       headerBorder = FALSE,
#       div(class = fonte_titulo, style = glue::glue("height: 15%; padding: 0px 10px 10px 10px; overflow: auto"), HTML(glue::glue("<b> {titulo} </b>"))),
#       hr(),
#       div(
#         style = "height: 70%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
#           p(style = style_descricao, "ocorreram antes do parto")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
#           p(style = style_descricao, "ocorreram durante o parto")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
#           p(style = style_descricao, "não têm informação")
#         )
#       )
#     )
#
#   } else if (indicador == "perinatal momento do obito por peso") {
#     bs4Dash::box(
#       style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
#       width = width_caixa,
#       collapsible = FALSE,
#       headerBorder = FALSE,
#       div(class = fonte_titulo, style = glue::glue("height: 15%; padding: 0px 10px 0px 10px; overflow: auto"), HTML(glue::glue("<b> {titulo} </b>"))),
#       hr(),
#       div(
#         style = "height: 70%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
#           p(style = style_descricao, "ocorreram antes do parto")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
#           p(style = style_descricao, "ocorreram durante o parto")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
#           p(style = style_descricao, "ocorreram no dia 0 de vida")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
#           p(style = style_descricao, "ocorreram de 1 a 6 dias de vida")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto5)} </b>"))),
#           p(style = style_descricao, "não tem informação")
#         )
#       )
#     )
#
#   } else if (indicador == "perinatal peso por momento do obito") {
#     bs4Dash::box(
#       style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
#       width = width_caixa,
#       collapsible = FALSE,
#       headerBorder = FALSE,
#       div(class = fonte_titulo, style = glue::glue("height: 15%; padding: 0px 10px 0px 10px; overflow: auto"), HTML(glue::glue("<b> {titulo} </b>"))),
#       hr(),
#       div(
#         style = "height: 70%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
#           p(style = style_descricao, "possuem peso menor que 1000 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
#           p(style = style_descricao, "possuem peso de 1000 a 1499 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
#           p(style = style_descricao, "possuem peso de 1500 a 2499 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
#           p(style = style_descricao, "possuem peso maior ou igual a 2500g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto5)} </b>"))),
#           p(style = style_descricao, "não tem informação")
#         ),
#       )
#     )
#   } else if (indicador == "neonatal momento do obito por peso") {
#     bs4Dash::box(
#       style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
#       width = width_caixa,
#       collapsible = FALSE,
#       headerBorder = FALSE,
#       div(class = fonte_titulo, style = glue::glue("height: 15%; padding: 0px 10px 10px 10px;"), HTML(glue::glue("<b> {titulo} </b>"))),
#       hr(),
#       div(
#         style = "height: 70%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
#           p(style = style_descricao, "ocorreram no dia 0 de vida")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
#           p(style = style_descricao, "ocorreram de 1 a 6 dias de vida")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
#           p(style = style_descricao, "ocorreram de 7 a 27 dias de vida")
#         )
#       )
#     )
#
#   } else if (indicador == "neonatal peso por momento do obito") {
#     bs4Dash::box(
#       style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
#       width = width_caixa,
#       collapsible = FALSE,
#       headerBorder = FALSE,
#       div(class = fonte_titulo, style = glue::glue("height: 15%; padding: 0px 10px 10px 10px;"), HTML(glue::glue("<b> {titulo} </b>"))),
#       hr(),
#       div(
#         style = "height: 70%; overflow: auto; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
#           p(style = style_descricao, "possuem peso menor que 1000 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
#           p(style = style_descricao, "possuem peso de 1000 a 1499 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
#           p(style = style_descricao, "possuem peso de 1500 a 2499 g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
#           p(style = style_descricao, "possuem peso maior ou igual a 2500g")
#         ),
#         div(
#           p(class = "fonte-destaque-caixas2", style = style_texto, HTML(glue::glue("<b> {glue::glue(texto5)} </b>"))),
#           p(style = style_descricao, "não tem informação")
#         )
#       )
#     )
#   }
#
# }

cria_modal_incompletude <- function(df, incompletude1, variavel_incompletude1 = NULL, descricao_incompletude1 = NULL, incompletude2 = NULL, variavel_incompletude2 = NULL, descricao_incompletude2 = NULL, incompletude3 = NULL, variavel_incompletude3 = NULL, descricao_incompletude3 = NULL, incompletude4 = NULL, variavel_incompletude4 = NULL, descricao_incompletude4 = NULL, cobertura, base = "SINASC", bloco = "geral", nivel = 2) {

  if (bloco == "bloco6" | substr(bloco, 1, 6) == "bloco7") {
    base <- "SIM"
  }

  if (bloco != "bloco6") {
    req(any(incompletude1 > 5, na.rm = TRUE) | any(incompletude2 > 5, na.rm = TRUE) | any(incompletude3 > 5, na.rm = TRUE) | any(incompletude4 > 5, na.rm = TRUE) | any(cobertura < 90, na.rm = TRUE))
  } else {
    req(any(incompletude1 < 90, na.rm = TRUE) | any(incompletude2 < 100, na.rm = TRUE) | any(cobertura < 90, na.rm = TRUE))
  }

  if (bloco != "bloco6") {
    anos1_aux <- df$ano[which(incompletude1 > 5)]
    val_incomp1_aux <- formatC(incompletude1[which(incompletude1 > 5)], big.mark = '.', decimal.mark = ',')
  } else {
    anos1_aux <- df$ano[which(incompletude1 < 90)]
    val_incomp1_aux <- formatC(incompletude1[which(incompletude1 < 90)], big.mark = '.', decimal.mark = ',')
  }

  anos1 <- paste(
    paste0(anos1_aux[1:length(anos1_aux) - 1], collapse = ", "), "e", anos1_aux[length(anos1_aux)]
  )
  valores_incompletude1 <- paste0(
    paste0(val_incomp1_aux[1:length(val_incomp1_aux) - 1], "%", collapse = ", "), " e ", val_incomp1_aux[length(val_incomp1_aux)], "%"
  )

  if (nivel != 3) {
    mais_detalhes <- "Para mais detalhes, vá para o <span style = 'font-weight: 700'> Nível 3: Visão detalhada dos indicadores. </span>"
  } else {
    mais_detalhes <- ""
  }

  if (length(anos1_aux) > 1) {
    if (bloco == "bloco6") {
      texto_incompletude <- glue::glue("Os <span style = 'font-weight: 700'>óbitos de mulheres em idade fértil (MIF)</span> apresentam problemas de investigação nos anos de <span style = 'font-weight: 700'> {anos1}.
           </span> Nesses anos, a porcentagem de óbitos investigados dessa população foi, respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude1}. </span>
           São considerados como valores ideais aqueles acima de 90%.")
    } else if (bloco == "deslocamento") {
      texto_incompletude <- glue::glue("As Declarações de Nascidos Vivos apresentam problemas de <span style = 'font-weight: 700'>preenchimento do CNES</span> nos anos de <span style = 'font-weight: 700'> {anos1}.
           </span> Nesses anos, a porcentagem de DNs sem CNES preenchido foi, respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude1}. </span>
           Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são considerados bons.")
    } else {
      if (variavel_incompletude1 == "PARTO e TPROBSON") {
        texto_incompletude <- glue::glue("As variáveis <span style = 'font-weight: 700'>PARTO</span> e <span style = 'font-weight: 700'>TPROBSON</span>, </span> do {base}, apresentam problemas de incompletude nos anos de
           <span style = 'font-weight: 700'> {anos1}. </span> Nesses anos, a porcentagem de valores em branco ou ignorados dessas variáveis foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude1}. </span> Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são
           considerados bons.")
      } else if (variavel_incompletude1 == "GESTACAO, SEMAGESTAC e PESO") {
        texto_incompletude <- glue::glue("As variáveis <span style = 'font-weight: 700'>GESTACAO</span>, <span style = 'font-weight: 700'>SEMAGESTAC</span> e <span style = 'font-weight: 700'>PESO</span>, </span> do {base}, apresentam problemas de incompletude nos anos de
           <span style = 'font-weight: 700'> {anos1}. </span> Nesses anos, a porcentagem de valores em branco ou ignorados dessas variáveis foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude1}. </span> Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são
           considerados bons.")
      } else if (variavel_incompletude1 == "PESO, GESTACAO, SEMAGESTAC e APGAR5") {
        texto_incompletude <- glue::glue("As variáveis <span style = 'font-weight: 700'>PESO</span>, <span style = 'font-weight: 700'>GESTACAO</span>, <span style = 'font-weight: 700'>SEMAGESTAC</span> e <span style = 'font-weight: 700'>APGAR5</span>, </span> do {base}, apresentam problemas de incompletude nos anos de
           <span style = 'font-weight: 700'> {anos1}. </span> Nesses anos, a porcentagem de valores em branco ou ignorados dessas variáveis simultaneamente foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude1}. </span> Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são
           considerados bons.")
      } else {
        texto_incompletude <- glue::glue("A variável <span style = 'font-weight: 700'> {variavel_incompletude1}, </span> do {base}, apresenta problemas de incompletude nos anos de
           <span style = 'font-weight: 700'> {anos1}. </span> Nesses anos, a porcentagem de valores {descricao_incompletude1} dessa variável foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude1}. </span> Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são
           considerados bons.")
      }

    }
  } else if (length(anos1_aux) == 1) {
    if (bloco == "bloco6") {
      texto_incompletude <- glue::glue("Os <span style = 'font-weight: 700'>óbitos de mulheres em idade fértil (MIF)</span> apresentam problemas de investigação no ano de <span style = 'font-weight: 700'> {anos1_aux}.
           </span> Nesse ano, a porcentagem de óbitos investigados dessa população foi de <span style = 'font-weight: 700'> {val_incomp1_aux}%. </span>
           São considerados como valores ideais aqueles acima de 90%.")
    } else if (bloco == "deslocamento") {
      texto_incompletude <- glue::glue("As Declarações de Nascidos Vivos apresentam problemas de <span style = 'font-weight: 700'>preenchimento do CNES</span> no ano de <span style = 'font-weight: 700'> {anos1_aux}.
           </span> Nesse ano, a porcentagem de DNs sem CNES preenchido foi de <span style = 'font-weight: 700'> {val_incomp1_aux}%. </span>
           Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são considerados bons.")
    } else {
      if (variavel_incompletude1 == "PARTO e TPROBSON") {
        texto_incompletude <- glue::glue("As variáveis <span style = 'font-weight: 700'>PARTO</span> e <span style = 'font-weight: 700'>TPROBSON</span>, do {base}, apresentam problemas de incompletude no ano de
             <span style = 'font-weight: 700'> {anos1_aux}. </span> Nesse ano, a porcentagem de valores em branco ou ignorados dessas variáveis simultaneamente foi
             de <span style = 'font-weight: 700'> {val_incomp1_aux}%. </span> Valores abaixo de 5% são considerados excelentes, enquanto
             valores abaixo de 10% são considerados bons.")
      } else if (variavel_incompletude1 == "GESTACAO, SEMAGESTAC e PESO") {
        texto_incompletude <- glue::glue("As variáveis <span style = 'font-weight: 700'>GESTACAO</span>, <span style = 'font-weight: 700'>SEMAGESTAC</span> e <span style = 'font-weight: 700'>PESO</span>, </span> do {base}, apresentam problemas de incompletude no ano de
           <span style = 'font-weight: 700'> {anos1_aux}. </span> Nesse ano, a porcentagem de valores em branco ou ignorados dessas variáveis simultaneamente foi de <span style = 'font-weight: 700'> {val_incomp1_aux}%. </span> Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são
           considerados bons.")
      } else if (variavel_incompletude1 == "PESO, GESTACAO, SEMAGESTAC e APGAR5") {
        texto_incompletude <- glue::glue("As variáveis <span style = 'font-weight: 700'>PESO</span>, <span style = 'font-weight: 700'>GESTACAO</span>, <span style = 'font-weight: 700'>SEMAGESTAC</span> e <span style = 'font-weight: 700'>APGAR5</span>, </span> do {base}, apresentam problemas de incompletude no ano de
           <span style = 'font-weight: 700'> {anos1_aux}. </span> Nesse ano, a porcentagem de valores em branco ou ignorados dessas variáveis simultaneamente foi de<span style = 'font-weight: 700'> {val_incomp1_aux}%. </span> Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são
           considerados bons.")
      } else {
        texto_incompletude <- glue::glue("A variável <span style = 'font-weight: 700'> {variavel_incompletude1}</span>, do {base}, apresenta problemas de incompletude no ano de
             <span style = 'font-weight: 700'> {anos1_aux}. </span> Nesse ano, a porcentagem de valores {descricao_incompletude1} dessa variável foi
             de <span style = 'font-weight: 700'> {val_incomp1_aux}%. </span> Valores abaixo de 5% são considerados excelentes, enquanto
             valores abaixo de 10% são considerados bons.")
      }

    }
  } else {
    texto_incompletude <- ""
  }

  if ((bloco != "bloco6" & !any(incompletude2 > 5, na.rm = TRUE)) | (bloco == "bloco6" & !any(incompletude2 < 100, na.rm = TRUE))) {
    if (!any(cobertura < 90, na.rm = TRUE)) {
      texto <- glue::glue(
        "<div style = 'text-align: justify; text-justify: inter-word;'>
             {texto_incompletude} {mais_detalhes}
           </div>"
      )
    } else {
      anos_cobertura_aux <- df$ano[which(cobertura < 90)]
      val_cobertura_aux <- formatC(cobertura[which(cobertura < 90)], big.mark = '.', decimal.mark = ',')

      anos_cobertura <- paste(
        paste0(anos_cobertura_aux[1:length(anos_cobertura_aux) - 1], collapse = ", "), "e", anos_cobertura_aux[length(anos_cobertura_aux)]
      )
      valores_cobertura <- paste0(
        paste0(val_cobertura_aux[1:length(val_cobertura_aux) - 1], "%", collapse = ", "), " e ", val_cobertura_aux[length(val_cobertura_aux)], "%"
      )

      if (length(anos_cobertura_aux) > 1) {
        texto_cobertura <- glue::glue("{ifelse(length(anos1_aux) > 0, '</br></br>Além disso, a', 'A')} localidade apresenta <span style = 'font-weight: 700'> problemas na cobertura do {base} </span> nos anos de <span style = 'font-weight: 700'> {anos_cobertura}. </span> Nesses anos, a cobertura dessa base de dados foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_cobertura}. </span> São considerados como valores ideais aqueles acima de 90%.")
      } else {
        texto_cobertura <- glue::glue("{ifelse(length(anos1_aux) > 0, '</br></br>Além disso, a', 'A')} localidade apresenta <span style = 'font-weight: 700'> problemas na cobertura do {base} </span> no ano de <span style = 'font-weight: 700'> {anos_cobertura_aux}. </span> Nesse ano, a cobertura dessa base de dados foi
           de <span style = 'font-weight: 700'> {val_cobertura_aux}%. </span> São considerados como valores ideais aqueles acima de 90%.")
      }

      texto <- glue::glue(
        "<div style = 'text-align: justify; text-justify: inter-word;'>
             {texto_incompletude} {texto_cobertura} {mais_detalhes}
           </div>"
      )
    }
  } else {
    if (bloco != "bloco6") {
      anos2_aux <- df$ano[which(incompletude2 > 5)]
      val_incomp2_aux <- formatC(incompletude2[which(incompletude2 > 5)], big.mark = '.', decimal.mark = ',')
    } else {
      anos2_aux <- df$ano[which(incompletude2 < 100)]
      val_incomp2_aux <- formatC(incompletude2[which(incompletude2 < 100)], big.mark = '.', decimal.mark = ',')
    }

    anos2 <- paste(
      paste0(anos2_aux[1:length(anos2_aux) - 1], collapse = ", "), "e", anos2_aux[length(anos2_aux)]
    )
    valores_incompletude2 <- paste0(
      paste0(val_incomp2_aux[1:length(val_incomp2_aux) - 1], "%", collapse = ", "), " e ", val_incomp2_aux[length(val_incomp2_aux)], "%"
    )

    if (length(anos2_aux) > 1) {
      if (bloco == "bloco6") {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, '</br></br>Além disso, os', 'Os')} <span style = 'font-weight: 700'>óbitos maternos</span> apresentam problemas de investigação nos anos de <span style = 'font-weight: 700'> {anos2}.
           </span> Nesses anos, a porcentagem de óbitos investigados dessa população foi, respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude2}. </span>
           São considerados como valores ideais aqueles iguais a 100%.")
      } else if (bloco == "deslocamento") {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, '</br></br>Além disso, as', 'As')} Declarações de Nascidos Vivos apresentam problemas de <span style = 'font-weight: 700'>validade do CNES</span> nos anos de <span style = 'font-weight: 700'> {anos2}.
           </span> Nesses anos, a porcentagem de DNs com CNES inválido foi, respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude2}. </span>
           Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são considerados bons.")
      } else {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, '</br></br>Além disso, a', 'A')} variável <span style = 'font-weight: 700'> {variavel_incompletude2}</span>, do {base}, apresenta problemas de incompletude nos anos de
           <span style = 'font-weight: 700'> {anos2}. </span> Nesses anos, a porcentagem de valores {descricao_incompletude2} dessa variável foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude2}. </span> Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são
           considerados bons.")
      }
    } else if (length(anos2_aux) == 1) {
      if (bloco == "bloco6") {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, '</br></br>Além disso, os', 'Os')} <span style = 'font-weight: 700'>óbitos maternos</span> apresentam problemas de investigação no ano de <span style = 'font-weight: 700'> {anos2_aux}.
           </span> Nesse ano, a porcentagem de óbitos investigados dessa população foi de <span style = 'font-weight: 700'> {val_incomp2_aux}%. </span>
           São considerados como valores ideais aqueles iguais a 100%.")
      } else if (bloco == "deslocamento") {
        texto_incompletude2 <-  glue::glue("{dplyr::if_else(length(anos1_aux) > 0, '</br></br>Além disso, as', 'As')} Declarações de Nascidos Vivos apresentam problemas de <span style = 'font-weight: 700'>validade do CNES</span> no ano de <span style = 'font-weight: 700'> {anos2_aux}.
           </span> Nesse ano, a porcentagem de DNs com CNES inválido foi de <span style = 'font-weight: 700'> {val_incomp2_aux}%. </span>
           Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são considerados bons.")
      } else {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, '</br></br>Além disso, a', 'A')} variável <span style = 'font-weight: 700'> {variavel_incompletude2}</span>, do {base}, apresenta problemas de incompletude no ano de
             <span style = 'font-weight: 700'> {anos2_aux}. </span> Nesse ano, a porcentagem de valores {descricao_incompletude2} dessa variável foi
             de <span style = 'font-weight: 700'> {val_incomp2_aux}%. </span> Valores abaixo de 5% são considerados excelentes, enquanto
             valores abaixo de 10% são considerados bons.")
      }
    } else {
      texto_incompletude2 <- ""
    }

    if (!any(cobertura < 90, na.rm = TRUE)) {
      texto <- glue::glue(
        "<div style = 'text-align: justify; text-justify: inter-word;'>
             {texto_incompletude} {texto_incompletude2} {mais_detalhes}
           </div>"
      )
    } else {
      anos_cobertura_aux <- df$ano[which(cobertura < 90)]
      val_cobertura_aux <- formatC(cobertura[which(cobertura < 90)], big.mark = '.', decimal.mark = ',')

      anos_cobertura <- paste(
        paste0(anos_cobertura_aux[1:length(anos_cobertura_aux) - 1], collapse = ", "), "e", anos_cobertura_aux[length(anos_cobertura_aux)]
      )
      valores_cobertura <- paste0(
        paste0(val_cobertura_aux[1:length(val_cobertura_aux) - 1], "%", collapse = ", "), " e ", val_cobertura_aux[length(val_cobertura_aux)], "%"
      )

      if (length(anos1_aux) > 0 & length(anos2_aux) > 0) {
        texto_cobertura2 <- glue::glue("<br>
               A localidade apresenta, também, <span style = 'font-weight: 700'> problemas na cobertura do {base} </span> no ano de <span style = 'font-weight: 700'> {anos_cobertura_aux}. </span> Nesse ano, a cobertura
               dessa base de dados foi de <span style = 'font-weight: 700'> {val_cobertura_aux}%. </span> São considerados como valores ideais aqueles acima de 90%.")
      } else {
        texto_cobertura2 <- glue::glue("</br></br>Além disso, a localidade apresenta <span style = 'font-weight: 700'> problemas na cobertura do {base} </span> nos anos de <span style = 'font-weight: 700'> {anos_cobertura}. </span> Nesses anos, a cobertura dessa base de dados foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_cobertura}. </span> São considerados como valores ideais aqueles acima de 90%.")
      }

      texto <- glue::glue(
        "<div style = 'text-align: justify; text-justify: inter-word;'>
             {texto_incompletude} {texto_incompletude2} {texto_cobertura2} {mais_detalhes}
           </div>"
      )
    }
  }

  # Show a simple modal
  shinyalert::shinyalert(
    html = TRUE,
    title = "<div class = 'fonte-titulos-modal'> Qualidade da informação </div>",
    text = texto,
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    confirmButtonText = "OK",
    confirmButtonCol = "#007bff",
    animation = TRUE,
    immediate = TRUE
  )
}

minha_paged_windmill <- function(logo = "0",
                                 front_img = "0",
                                 back_img = "0",
                                 img_to_dark = FALSE,
                                 logo_to_white = FALSE,
                                 other_css = "inst/app/www/report/css_report.css") {
  # arguments
  main_css <-
    "inst/app/www/report/minha_style_windmill.css"
  pandoc_html <-
    "inst/app/www/report/template_paged.html"

  # default img
  if (front_img == "0") {
    front_img <-
      "inst/app/www/report/capa.jpg"
  }

  if (back_img == "0") {
    back_img <-
      "inst/app/www/report/fundo.jpg"
  }

  if (logo == "0") {
    logo <-
      "inst/app/www/report/blank.svg"
  }

  # darken img
  if (img_to_dark == TRUE) {
    # opacity
    front_img_init <-
      magick::image_read(front_img)
    front_img_ok <-
      magick::image_colorize(front_img_init, opacity = 50, color = "black")

    back_img_init <-
      magick::image_read(back_img)
    back_img_ok <-
      magick::image_colorize(back_img_init, opacity = 50, color = "black")

    # path to image
    front_img <- paste0(tempfile("front_img"), ".jpg")
    magick::image_write(front_img_ok, front_img, format = "jpg")

    back_img <- paste0(tempfile("back_img"), ".jpg")
    magick::image_write(back_img_ok, back_img, format = "jpg")
  }

  # logo to white - logo should be svg
  if (logo_to_white == TRUE) {
    logo_init <- magick::image_read_svg(logo)
    logo_ok <-
      magick::image_colorize(logo_init, opacity = 100, color = "white")

    # path to logo
    logo <- paste0(tempfile("logo"), ".svg")
    magick::image_write(logo_ok, logo, format = "svg")
  }

  # template
  pagedown::html_paged(
    css = c(other_css, main_css),
    template = pandoc_html,
    front_cover = c(logo, front_img),
    back_cover = c(logo, back_img),
    toc = FALSE,
    toc_depth = 2
  )
}

seleciona <- function(aba, indicador, input){

  if(aba == "fetal"){

    if(indicador == "momento de obito por peso"){

      if(length(input) < 4){

        return(
          opcoes <- c( "distribuicao_moment_obito_fetal_menos1000", "distribuicao_moment_obito_fetal_1000_1499", "distribuicao_moment_obito_fetal_1500_2499", "distribuicao_moment_obito_fetal_mais2500", "falso")
        )

      } else{
        return(
          opcoes <- c(rep("falso", 4), "distribuicao_moment_obito_fetal_menos1000")
        )
      }




    } else if(indicador == "peso por momento do obito"){

      if(length(input)<2){

        return(

          opcoes <- c("distribuicao_peso_fetal_antes", "distribuicao_peso_fetal_durante", "falso")
        )
      } else{
        return(
          opcoes <- c("falso", "falso", "distribuicao_peso_fetal_antes")
        )
      }
    }


  } else if(aba == "perinatal"){


    if(indicador == "momento de obito por peso"){

      if(length(input)<4){

        return(

          opcoes <- c("distribuicao_moment_obito_perinat_menos1000", "distribuicao_moment_obito_perinat_1000_1499", "distribuicao_moment_obito_perinat_1500_2499", "distribuicao_moment_obito_perinat_mais2500", "falso")
        )

      } else{
        return( opcoes <- c(rep("falso", 4), "distribuicao_moment_obito_perinat_menos1000"))
      }



    } else if(indicador == "peso por momento do obito"){

      if(length(input) < 4){

        return(

          opcoes <- c("distribuicao_peso_perinat_antes_parto", "distribuicao_peso_perinat_durante_parto", "distribuicao_peso_perinat_dia_0", "distribuicao_peso_perinat_dia_1_6", "falso")
        )

      } else{
        return(opcoes <- c(rep("falso", 4), "distribuicao_peso_perinat_antes_parto"))
      }

    }


  } else if(aba == "neonatal"){


    if(indicador == "momento de obito por peso"){

      if(length(input) < 4){

        return(

          opcoes <- c("distribuicao_moment_obito_neonat_menos1000", "distribuicao_moment_obito_neonat_1000_1499", "distribuicao_moment_obito_neonat_1500_2499", "distribuicao_moment_obito_neonat_mais2500", "falso")
        )

      } else{
        return(opcoes <- c(rep("falso", 4), "distribuicao_moment_obito_neonat_menos1000"))
      }



    } else if(indicador == "peso por momento do obito"){

      if(length(input) < 3){

        return(

          opcoes <- c("distribuicao_peso_neonat_dia_0", "distribuicao_peso_neonat_dia_1_6", "distribuicao_peso_neonat_dia_7_27", "falso")
        )

      } else{
        return(opcoes <- c(rep("falso", 3), "distribuicao_peso_neonat_dia_0"))
      }




    }


  }

}

momento_obitos <- function(aba, grafico, input){

  if(aba == "perinatal"){
    if(grafico == "grupos"){

      if(length(input) < 5){
        return(paste(input, collapse = "|"))
      } else {
        return("perinatal_grupos_prematuridade|perinatal_grupos_infeccoes|perinatal_grupos_asfixia|perinatal_grupos_ma_formacao|perinatal_grupos_respiratorias|perinatal_grupos_gravidez|perinatal_grupos_afeccoes_perinatal|perinatal_grupos_mal_definida|perinatal_grupos_outros")
      }

    } else if(grafico == "evitaveis"){

      if(length(input) < 5){
        return(paste(input, collapse = "|"))
      } else {
        return("evitaveis_perinatal_imunoprevencao|evitaveis_perinatal_mulher_gestacao|evitaveis_perinatal_parto|evitaveis_perinatal_recem_nascido|evitaveis_perinatal_tratamento|evitaveis_perinatal_saude|evitaveis_perinatal_mal_definidas|evitaveis_perinatal_outros")
      }

    }
  }
  else if(aba == "fetal"){

    if(grafico == "grupos"){

      if(length(input) < 2){
        return(paste(input, collapse = "|"))
      } else {
        #return("fetal_grupos_antes"|"fetal_grupos_durante")
        return("fetal_grupos_prematuridade|fetal_grupos_infeccoes|fetal_grupos_asfixia|fetal_grupos_ma_formacao|fetal_grupos_respiratorias|fetal_grupos_gravidez|fetal_grupos_afeccoes_perinatal|fetal_grupos_mal_definida|fetal_grupos_outros")
      }

    } else if(grafico == "evitaveis"){

      if(length(input) < 2){
        return(paste(input, collapse = "|"))
      } else {
        #return("evitaveis_fetal_antes|evitaveis_fetal_durante")
        return("evitaveis_fetal_imunoprevencao|evitaveis_fetal_mulher_gestacao|evitaveis_fetal_parto|evitaveis_fetal_recem_nascido|evitaveis_fetal_tratamento|evitaveis_fetal_saude|evitaveis_fetal_mal_definidas|evitaveis_fetal_outros")
      }

    } else if(grafico == "evitaveis2"){

      if(length(input) < 2){
        return(paste(input, collapse = "|"))
      } else {
        return("evitaveis_fetal_imunoprevencao2|evitaveis_fetal_mulher_gestacao2|evitaveis_fetal_parto2|evitaveis_fetal_nao_aplica2|evitaveis_fetal_mal_definidas2|evitaveis_fetal_outros2")
      }


    }

  }

  else if(aba == "neonatal"){
    if(grafico == "grupos"){

      if(length(input) < 3){
        return(paste(input, collapse = "|"))
      } else {
        #return("neonat_grupos_0_dias|neonat_grupos_1_6_dias|neonat_grupos_7_27_dias")
        return("neonat_grupos_prematuridade|neonat_grupos_infeccoes|neonat_grupos_asfixia|neonat_grupos_ma_formacao|neonat_grupos_respiratorias|neonat_grupos_gravidez|neonat_grupos_afeccoes_perinatal|neonat_grupos_mal_definida|neonat_grupos_outros")

      }


    } else if(grafico == "evitaveis"){

      if(length(input) < 3){
        return(paste(input, collapse = "|"))
      } else {
        return("evitaveis_neonatal_0_dias|evitaveis_neonatal_1_6_dias|evitaveis_neonatal_7_27_dias")
        #return("evitaveis_neonatal_imunoprevencao|evitaveis_neonatal_mulher_gestacao|evitaveis_neonatal_parto|evitaveis_neonatal_recem_nascido|evitaveis_neonatal_tratamento|evitaveis_neonatal_saude|evitaveis_neonatal_mal_definidas|evitaveis_neonatal_outros")


      }

    }
  }
}

momento_internacoes <- function(input){
  if(length(input) < 3){
    return(paste(input, collapse = "|"))
  } else {
    return("morbidade_neonatal_grupos_prematuridade|morbidade_neonatal_grupos_infeccoes|morbidade_neonatal_grupos_asfixia|morbidade_neonatal_grupos_ma_formacao|morbidade_neonatal_grupos_afeccoes_respiratorias|morbidade_neonatal_grupos_fatores_maternos|morbidade_neonatal_grupos_afeccoes_perinatal|morbidade_neonatal_grupos_mal_definidas|morbidade_neonatal_grupos_outros|morbidade_neonatal_grupos_ictericia|morbidade_neonatal_grupos_cardiacos_perinatal|morbidade_neonatal_grupos_endocrinos|morbidade_neonatal_grupos_alimentacao")

  }
}

