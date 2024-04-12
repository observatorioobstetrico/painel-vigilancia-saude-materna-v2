#' @exportS3Method pkg::generic
cria_caixa_server <- function(dados, indicador, titulo, tem_meta = FALSE, nivel_de_analise, tipo_referencia, valor_de_referencia, valor_indicador = NULL, tipo = "porcentagem", invertido = FALSE, texto_caixa = NULL, cor = NULL, texto_footer = NULL, tamanho_caixa = "300px", fonte_titulo = "16px", pagina, width_caixa = 12) {

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
      razao <- round(valor_indicador_aux/valor_de_referencia, 1)
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

  if (tipo_referencia == "média nacional" & nivel_de_analise == "Nacional" & is.null(texto_footer)) {
    if (is.nan(valor_comp)) {
      texto_footer <- "Comparação não aplicável"
      cor <- "lightgrey"
    } else {
      texto_footer <- "Comparação não aplicável (a média nacional é o valor de referência)"
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

  if (invertido == FALSE) {
    final_texto_comp <- glue::glue("de no máximo {valor_de_referencia_formatado}")
  } else {
    final_texto_comp <- glue::glue("de no mínimo {valor_de_referencia_formatado}")
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
          tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado} vezes maior que o valor de referência, {final_texto_comp}% ({tipo_referencia})",
          tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado} vezes maior que o valor de referência, {final_texto_comp} ({tipo_referencia})"
        )
      } else {
        texto_comp <- dplyr::case_when(
          valor_comp < 0 & tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado}% maior que o valor de referência, {final_texto_comp}% ({tipo_referencia})",
          valor_comp < 0 & tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-up'> </i> {valor_comp_formatado}% maior que o valor de referência, {final_texto_comp} ({tipo_referencia})",
          valor_comp > 0 & tipo == "porcentagem" ~ "<i class='fa-solid fa-caret-down'> </i> {valor_comp_formatado}% menor que o valor de referência, {final_texto_comp}% ({tipo_referencia})",
          valor_comp > 0 & tipo != "porcentagem" ~ "<i class='fa-solid fa-caret-down'> </i> {valor_comp_formatado}% menor que o valor de referência, {final_texto_comp} ({tipo_referencia})",
          valor_comp == 0 ~ "Igual ao valor de referência ({tipo_referencia})",
          is.nan(valor_comp) ~ "Comparação não aplicável"
        )
      }

    }
  } else {
    texto_comp <- texto_footer
  }

  fonte_texto <- dplyr::case_when(
    pagina == "nivel_1" ~ dplyr::if_else(floor(log10(valor_indicador_aux)) + 1 < 7 | is.nan(valor_indicador_aux), "40px", "36px"),
    pagina != "nivel_1" ~ dplyr::if_else(floor(log10(valor_indicador_aux)) + 1 < 7 | is.nan(valor_indicador_aux), "34px", "30px")
  )

  # style_texto <- dplyr::case_when(
  #   pagina != "nivel_1" & pagina != "bloco_5" & titulo != "IDH" & titulo != "IDHM" ~ glue::glue("font-size: {fonte_texto}; height: 33%; overflow: auto; padding: 0 5px; display: flex; align-items:center; justify-content: center; text-align: center;"),
  #   pagina == "nivel_1" | pagina == "bloco_5" | titulo == "IDH" | titulo == "IDHM" ~ glue::glue("font-size: {fonte_texto}; height: 33%; overflow: auto; padding: 0 5px; display: flex; justify-content: center; text-align: center;")
  # )

  style_texto <- glue::glue("font-size: {fonte_texto}; height: 28%; overflow: auto; padding: 0 10px; display: flex; justify-content: center; text-align: center;")

  bs4Dash::box(
    style = glue::glue("height: {tamanho_caixa}; overflow: auto; padding: 0;"),
    width = width_caixa,
    collapsible = FALSE,
    headerBorder = FALSE,
    div(style = glue::glue("font-size: {fonte_titulo}; height: 31%; overflow: auto; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>"))),
    div(style = "height: 3%"),
    div(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto)} </b>"))),
    div(style = glue::glue("font-size: {fonte_titulo}; overflow: auto; height: 38%; padding: 10px 5px; display: flex; align-items:center; justify-content:center; text-align: center; background-color: {cor_comp};"), HTML(glue::glue("<b> {glue::glue(texto_comp)} </b>")))
  )


}


cria_caixa_conjunta_bloco5 <- function(dados, titulo, indicador, tamanho_caixa = "300px", fonte_titulo = "16px", width_caixa = 12) {

  if (indicador == "baixo peso") {
    valor_indicador1 <- dados[["porc_peso_menor_1500"]]
    valor_indicador2 <- dados[["porc_peso_1500_a_1999"]]
    valor_indicador3 <- dados[["porc_peso_2000_a_2499"]]
  }

  if (indicador == "prematuridade") {
    valor_indicador1 <- dados[["porc_menos_de_28_semanas"]]
    valor_indicador2 <- dados[["porc_28_a_32_semanas"]]
    valor_indicador3 <- dados[["porc_33_a_34_semanas"]]
    valor_indicador4 <- dados[["porc_35_a_36_semanas"]]
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

  style_texto <- "font-size: 30px; display: flex; justify-content: center; text-align: center; margin-bottom: 0"
  style_descricao <- "display: flex; padding: 0 5px; justify-content: center; text-align: center; margin-bottom: 0"

  if (indicador == "baixo peso") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(style = glue::glue("font-size: {fonte_titulo}; height: 20%; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>")), hr()),
      div(
        style = "height: 80%; overflow: auto; padding-bottom: 10px; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "possuem peso < 1500 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "possuem peso de 1500 a 1999 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "possuem peso de 2000 a 2499 g")
        )
      )
    )
  } else if (indicador == "prematuridade") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(style = glue::glue("font-size: {fonte_titulo}; height: 20%; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>")), hr()),
      div(
        style = "height: 80%; overflow: auto; padding-bottom: 10px; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "nasceram com menos de 28 semanas")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "nasceram de 28 a 32 semanas")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "nasceram de 33 a 34 semanas")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
          p(style = style_descricao, "nasceram de 35 a 36 semanas")
        ),        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto5)} </b>"))),
          p(style = style_descricao, "não possuem informação")
        )
      )
    )

  }

}

cria_caixa_conjunta_bloco7 <- function(dados, titulo, indicador, tamanho_caixa = "303px", fonte_titulo = "16px", width_caixa = 12) {

  if (indicador == "fetal peso por idade gestacional") {
    valor_indicador1 <- dados[["menos_1500_dist_peso_fetal"]]
    valor_indicador2 <- dados[["de_1500_1999_dist_peso_fetal"]]
    valor_indicador3 <- dados[["de_2000_2499_dist_peso_fetal"]]
    valor_indicador4 <- dados[["mais_2500_dist_peso_fetal"]]
  }

  if (indicador == "fetal momento do obito por peso") {
    valor_indicador1 <- dados[["antes_dist_moment_obito_fetal"]]
    valor_indicador2 <- dados[["durante_dist_moment_obito_fetal"]]
    valor_indicador3 <- dados[["faltante_dist_moment_obito_fetal"]]
  }

  if (indicador == "perinatal momento do obito por peso") {
    valor_indicador1 <- dados[["antes_dist_moment_obito_perinat"]]
    valor_indicador2 <- dados[["durante_dist_moment_obito_perinat"]]
    valor_indicador3 <- dados[["dia_0_dist_moment_obito_perinat"]]
    valor_indicador4 <- dados[["dia_1_6_dist_moment_obito_perinat"]]
  }

  if (indicador == "perinatal peso por momento do obito") {
    valor_indicador1 <- dados[["menos_1500_dist_peso_perinat"]]
    valor_indicador2 <- dados[["de_1500_1999_dist_peso_perinat"]]
    valor_indicador3 <- dados[["de_2000_2499_dist_peso_perinat"]]
    valor_indicador4 <- dados[["mais_2500_dist_peso_perinat"]]
  }

  if (indicador == "neonatal momento do obito por peso") {
    valor_indicador1 <- dados[["dia_0_dist_moment_obito_neonat"]]
    valor_indicador2 <- dados[["dia_1_6dist_moment_obito_neonat"]]
    valor_indicador3 <- dados[["dia_7_27dist_moment_obito_neonat"]]
  }

  if (indicador == "neonatal peso por momento do obito") {
    valor_indicador1 <- dados[["menos_1500_dist_peso_neonat"]]
    valor_indicador2 <- dados[["de_1500_1999_dist_peso_neonat"]]
    valor_indicador3 <- dados[["de_2000_2499_dist_peso_neonat"]]
    valor_indicador4 <- dados[["mais_2500_dist_peso_neonat"]]
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

  if (indicador == "fetal peso por idade gestacional" | indicador == "perinatal momento do obito por peso"|
      indicador == "perinatal peso por momento do obito" | indicador == "neonatal peso por momento do obito"){

    if (is.nan(valor_indicador4)) {
      texto4 <- "---"
    } else {
      texto4 <- "{formatC(valor_indicador4, big.mark = '.', decimal.mark = ',')}%"
    }

  }

  style_texto <- "font-size: 30px; display: flex; justify-content: center; text-align: center; margin-bottom: 0"
  style_descricao <- "display: flex; padding: 0 5px; justify-content: center; text-align: center; margin-bottom: 0"

  if (indicador == "fetal peso por idade gestacional") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(style = glue::glue("font-size: {fonte_titulo}; height: 15%; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>")), hr()),
      div(
        style = "height: 85%; overflow: auto; padding-bottom: 10px; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "possuem peso menor que 1500 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "possuem peso de 1500 a 1999 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "possuem peso de 2000 a 2499 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
          p(style = style_descricao, "possuem peso maior ou igual a 2500g")
        )
      )
    )
  } else if (indicador == "fetal momento do obito por peso") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(style = glue::glue("font-size: {fonte_titulo}; height: 15%; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>")), hr()),
      div(
        style = "height: 85%; overflow: auto; padding-bottom: 10px; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "ocorreram antes do parto")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "ocorreram durante o parto")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "não têm informação")
        )
      )
    )

  } else if (indicador == "perinatal momento do obito por peso") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(style = glue::glue("font-size: {fonte_titulo}; height: 15%; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>")), hr()),
      div(
        style = "height: 85%; overflow: auto; padding-bottom: 10px; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "ocorreram antes do parto")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "ocorreram durante o parto")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "ocorreram no dia 0 de vida")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
          p(style = style_descricao, "ocorreram de 1 a 6 dias de vida")
        )
      )
    )

  } else if (indicador == "perinatal peso por momento do obito") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(style = glue::glue("font-size: {fonte_titulo}; height: 15%; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>")), hr()),
      div(
        style = "height: 85%; overflow: auto; padding-bottom: 10px; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "possuem peso menor que 1500 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "possuem peso de 1500 a 1999 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "possuem peso de 2000 a 2499 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
          p(style = style_descricao, "possuem peso maior ou igual a 2500g")
        )
      )
    )
  } else if (indicador == "neonatal momento do obito por peso") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(style = glue::glue("font-size: {fonte_titulo}; height: 15%; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>")), hr()),
      div(
        style = "height: 85%; overflow: auto; padding-bottom: 10px; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "ocorreram no dia 0 de vida")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "ocorreram de 1 a 6 dias de vida")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "ocorreram de 7 a 27 dias de vida")
        )
      )
    )

  } else if (indicador == "neonatal peso por momento do obito") {
    bs4Dash::box(
      style = glue::glue("height: {tamanho_caixa}; padding: 0;"),
      width = width_caixa,
      collapsible = FALSE,
      headerBorder = FALSE,
      div(style = glue::glue("font-size: {fonte_titulo}; height: 15%; padding: 0 10px;"), HTML(glue::glue("<b> {titulo} </b>")), hr()),
      div(
        style = "height: 85%; overflow: auto; padding-bottom: 10px; display: flex; align-items: center; justify-content: center; flex-wrap: wrap;",
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto1)} </b>"))),
          p(style = style_descricao, "possuem peso menor que 1500 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto2)} </b>"))),
          p(style = style_descricao, "possuem peso de 1500 a 1999 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto3)} </b>"))),
          p(style = style_descricao, "possuem peso de 2000 a 2499 g")
        ),
        div(
          p(style = style_texto, HTML(glue::glue("<b> {glue::glue(texto4)} </b>"))),
          p(style = style_descricao, "possuem peso maior ou igual a 2500g")
        )
      )
    )
  }

}



cria_modal_incompletude <- function(df, incompletude1, variavel_incompletude1 = NULL, descricao_incompletude1 = NULL, incompletude2 = NULL, variavel_incompletude2 = NULL, descricao_incompletude2 = NULL, cobertura, base = "SINASC", bloco = "geral", nivel = 2) {

  if (bloco == "bloco6") {
    base <- "SIM"
  }

  if (bloco != "bloco6") {
    req(any(incompletude1 > 5, na.rm = TRUE) | any(incompletude2 > 5, na.rm = TRUE) | any(cobertura < 90, na.rm = TRUE))
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
           </span> Nesses anos, a porcentagem de óbitos não investigados dessa população foi, respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude1}. </span>
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
           </span> Nesse ano, a porcentagem de óbitos não investigados dessa população foi de <span style = 'font-weight: 700'> {val_incomp1_aux}%. </span>
           São considerados como valores ideais aqueles acima de 90%.")
    } else if (bloco == "deslocamento") {
      texto_incompletude <- glue::glue("As Declarações de Nascidos Vivos apresentam problemas de <span style = 'font-weight: 700'>preenchimento do CNES</span> no ano de <span style = 'font-weight: 700'> {anos1_aux}.
           </span> Nesse ano, a porcentagem de DNs sem CNES preenchido foi de <span style = 'font-weight: 700'> {val_incomp1_aux}%. </span>
           Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são considerados bons.")
    } else {

      if (variavel_incompletude1 == "PARTO e TPROBSON") {
        texto_incompletude <- glue::glue("As variáveis <span style = 'font-weight: 700'>PARTO</span> e <span style = 'font-weight: 700'>TPROBSON</span>, do {base}, apresentam problemas de incompletude no ano de
             <span style = 'font-weight: 700'> {anos1_aux}. </span> Nesse ano, a porcentagem de valores em branco ou ignorados dessas variáveis foi
             de <span style = 'font-weight: 700'> {val_incomp1_aux}%. </span> Valores abaixo de 5% são considerados excelentes, enquanto
             valores abaixo de 10% são considerados bons.")
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
        texto_cobertura <- glue::glue("{ifelse(length(anos1_aux) > 0, 'Além disso, a', 'A')} localidade apresenta <span style = 'font-weight: 700'> problemas na cobertura do {base} </span> nos anos de <span style = 'font-weight: 700'> {anos_cobertura}. </span> Nesses anos, a cobertura dessa base de dados foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_cobertura}. </span> São considerados como valores ideais aqueles acima de 90%.")
      } else {
        texto_cobertura <- glue::glue("{ifelse(length(anos1_aux) > 0, 'Além disso, a', 'A')} localidade apresenta <span style = 'font-weight: 700'> problemas na cobertura do {base} </span> no ano de <span style = 'font-weight: 700'> {anos_cobertura_aux}. </span> Nesse ano, a cobertura dessa base de dados foi
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
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, 'Além disso, os', 'Os')} <span style = 'font-weight: 700'>óbitos maternos</span> apresentam problemas de investigação nos anos de <span style = 'font-weight: 700'> {anos2}.
           </span> Nesses anos, a porcentagem de óbitos não investigados dessa população foi, respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude2}. </span>
           São considerados como valores ideais aqueles iguais a 100%.")
      } else if (bloco == "deslocamento") {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, 'Além disso, as', 'As')} Declarações de Nascidos Vivos apresentam problemas de <span style = 'font-weight: 700'>validade do CNES</span> nos anos de <span style = 'font-weight: 700'> {anos2}.
           </span> Nesses anos, a porcentagem de DNs com CNES inválido foi, respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude2}. </span>
           Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são considerados bons.")
      } else {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, 'Além disso, a', 'A')} a variável <span style = 'font-weight: 700'> {variavel_incompletude2}</span>, do {base}, apresenta problemas de incompletude nos anos de
           <span style = 'font-weight: 700'> {anos2}. </span> Nesses anos, a porcentagem de valores {descricao_incompletude2} dessa variável foi,
           respectivamente, de <span style = 'font-weight: 700'> {valores_incompletude2}. </span> Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são
           considerados bons.")
      }
    } else if (length(anos2_aux) == 1) {
      if (bloco == "bloco6") {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, 'Além disso, os', 'Os')} <span style = 'font-weight: 700'>óbitos maternos</span> apresentam problemas de investigação no ano de <span style = 'font-weight: 700'> {anos2_aux}.
           </span> Nesse ano, a porcentagem de óbitos não investigados dessa população foi de <span style = 'font-weight: 700'> {val_incomp2_aux}%. </span>
           São considerados como valores ideais aqueles iguais a 100%.")
      } else if (bloco == "deslocamento") {
        texto_incompletude2 <-  glue::glue("{dplyr::if_else(length(anos1_aux) > 0, 'Além disso, as', 'As')} Declarações de Nascidos Vivos apresentam problemas de <span style = 'font-weight: 700'>validade do CNES</span> no ano de <span style = 'font-weight: 700'> {anos2_aux}.
           </span> Nesse ano, a porcentagem de DNs com CNES inválido foi de <span style = 'font-weight: 700'> {val_incomp2_aux}%. </span>
           Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são considerados bons.")
      } else {
        texto_incompletude2 <- glue::glue("{dplyr::if_else(length(anos1_aux) > 0, 'Além disso, a', 'A')} variável <span style = 'font-weight: 700'> {variavel_incompletude2}</span>, do {base}, apresenta problemas de incompletude no ano de
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
        texto_cobertura2 <- glue::glue("Além disso, a localidade apresenta <span style = 'font-weight: 700'> problemas na cobertura do {base} </span> nos anos de <span style = 'font-weight: 700'> {anos_cobertura}. </span> Nesses anos, a cobertura dessa base de dados foi,
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
    title = "<div style = 'font-size: 25px;'> Qualidade da informação </div>",
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
                          img_to_dark = TRUE,
                          logo_to_white = FALSE,
                          other_css = NULL) {
  # arguments
  main_css <-
    "pagedreport/resources/css/minha_style_windmill.css"
  pandoc_html <-
    "pagedreport/resources/html/template_paged.html"

  # default img
  if (front_img == "0") {
    front_img <-
      "pagedreport/resources/img/nuts-img.jpg"
  }

  if (back_img == "0") {
    back_img <-
      "pagedreport/resources/img/hazelnuts-img.jpg"
  }

  if (logo == "0") {
    logo <-
      "pagedreport/resources/logo/square-logo.svg"
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

seleciona <- function(aba, indicador){

  if(aba == "fetal"){

    if(indicador == "momento de obito por peso"){

      return(
        opcoes <- c( "dist_moment_obito_fetal_menos1500", "dist_moment_obito_fetal_1500_1999", "dist_moment_obito_fetal_2000_2499", "dist_moment_obito_fetal_mais2500")
      )


    } else if(indicador == "peso por momento do obito"){

      return(

        opcoes <- c("dist_peso_fetal_antes", "dist_peso_fetal_durante")
      )
    }


  } else if(aba == "perinatal"){


    if(indicador == "momento de obito por peso"){

      return(

        opcoes <- c("dist_moment_obito_perinat_menos1500", "dist_moment_obito_perinat_1500_1999", "dist_moment_obito_perinat_2000_2499", "dist_moment_obito_perinat_mais2500")
      )


    } else if(indicador == "peso por momento do obito"){

      return(

        opcoes <- c("dist_peso_perinat_antes_parto", "dist_peso_perinat_durante_parto", "dist_peso_perinat_dia_0", "dist_peso_perinat_dia_1_6")
      )

    }


  } else if(aba == "neonatal"){


    if(indicador == "momento de obito por peso"){

      return(

        opcoes <- c("dist_moment_obito_neonat_menos1500", "dist_moment_obito_neonat_1500_1999", "dist_moment_obito_neonat_2000_2499", "dist_moment_obito_neonat_mais2500")
      )


    } else if(indicador == "peso por momento do obito"){

      return(

        opcoes <- c("dist_peso_neonat_dia_0", "dist_peso_neonat_dia_1_6", "dist_peso_neonat_dia_7_27")
      )


    }


  }

}
