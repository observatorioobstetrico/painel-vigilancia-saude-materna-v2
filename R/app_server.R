#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  mod_documentacao_server("documentacao_1")
  mod_nivel_1_server("nivel_1_1", filtros = filtros)
  mod_bloco_1_server("bloco_1_1", filtros = filtros)
  mod_bloco_2_server("bloco_2_1", filtros = filtros)
  mod_bloco_3_server("bloco_3_1", filtros = filtros)
  mod_bloco_4_server("bloco_4_1", filtros = filtros)
  mod_bloco_5_server("bloco_5_1", filtros = filtros)
  mod_bloco_6_server("bloco_6_1", filtros = filtros)
  mod_bloco_7_server("bloco_7_1", filtros = filtros)
  #mod_bloco_8_server("bloco_8_1", filtros = filtros)
  #mod_bloco_9_server("bloco_9_1", filtros = filtros)
  mod_nivel_3_server("nivel_3_1", filtros = filtros)


  output$label_nivel_comp <- renderUI({
    div(
      tags$b(HTML("Nível de análise &nbsp;")),
      if (input$nivel2 == "Municípios semelhantes") {
        shinyWidgets::actionBttn(
          inputId = "botao_agrupamento",
          icon = icon("question"),
          color = "primary",
          style = "material-circle",
          size = "xs"
        )
      }
    )
  })

  observeEvent(input$botao_agrupamento, {
    shinyalert::shinyalert(
      html = TRUE,
      title = "<div style = 'font-size: 25px; color: #656565'> Sobre o agrupamento de municípios semelhantes </div>",
      text = "<div style = 'text-align: justify; text-justify: inter-word;'> Os municípios foram agrupados a partir de seu IDHM e de sua latitude por meio do algoritmo de agrupamento K-médias. Por meio da análise do gráfico do cotovelo e dos índices de Davies-Bouldin, Dunn,
Silhueta e Calinski-Harabasz, o número de grupos adotado foi 3. </div>",
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#007bff",
      animation = TRUE,
      immediate = TRUE
    )
  })

  rv <- reactiveValues(input_nivel_anterior = NULL)
  observeEvent(input$nivel, {
    if (length(rv$input_nivel_anterior) == 1) {
      rv$input_nivel_anterior <- c(rv$input_nivel_anterior, input$nivel)
    } else {
      rv$input_nivel_anterior <- c(rv$input_nivel_anterior[length(rv$input_nivel_anterior)], input$nivel)
    }
  })

  observe({
    if (input$nivel == "Municipal") {
      if (rv$input_nivel_anterior[length(rv$input_nivel_anterior) - 1] != "Municipal") {
        updateSelectizeInput(
          inputId = "nivel2",
          choices = c("Nacional", "Regional", "Macrorregião de saúde", "Microrregião de saúde", "Estadual", "Municipal", "Municípios semelhantes")
        )
      }
    }
    if (input$nivel != "Municipal") {
      if (length(rv$input_nivel_anterior) > 1) {
        if (rv$input_nivel_anterior[length(rv$input_nivel_anterior) - 1] == "Municipal") {
          updateSelectizeInput(
            inputId = "nivel2",
            choices = c("Nacional", "Regional", "Macrorregião de saúde", "Microrregião de saúde", "Estadual", "Municipal")
          )
        }
      }
    }
  })

  observeEvent(input$estado_municipio, {
    updateSelectizeInput(
      session,
      inputId = "municipio",
      choices = sort(municipios_choices$municipio[which(municipios_choices$uf == input$estado_municipio)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_micro,{
    updateSelectizeInput(
      session,
      inputId = "micro",
      choices = sort(micro_r_saude_choices$r_saude[which(micro_r_saude_choices$uf == input$estado_micro)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_macro, {
    updateSelectizeInput(
      session,
      inputId = "macro",
      choices = sort(macro_r_saude_choices$macro_r_saude[which(macro_r_saude_choices$uf == input$estado_macro)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_municipio2, {
    updateSelectizeInput(
      session,
      inputId = "municipio2",
      choices = sort(municipios_choices$municipio[which(municipios_choices$uf == input$estado_municipio2)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_micro2, {
    updateSelectizeInput(
      session,
      inputId = "micro2",
      choices = sort(micro_r_saude_choices$r_saude[which(micro_r_saude_choices$uf == input$estado_micro2)]),
      server = FALSE
    )
  })

  observeEvent(input$estado_macro2, {
    updateSelectizeInput(
      session,
      inputId = "macro2",
      choices = sort(macro_r_saude_choices$macro_r_saude[which(macro_r_saude_choices$uf == input$estado_macro2)]),
      server = FALSE
    )
  })

  observeEvent(input$bloco, {
    updateSelectizeInput(
      session,
      inputId = "indicador",
      choices = tabela_indicadores$indicador[which(tabela_indicadores$bloco == input$bloco)],
      server = FALSE
    )

    if (input$bloco == "bloco4") {
      updateSelectizeInput(
        session,
        inputId = "tipo_do_indicador_blocos4_6_7",
        choices = c(
          "Relacionados aos grupos de Robson e cesariana" = "robson",
          "Relacionados ao deslocamento para o parto" = "deslocamento"
        )
      )
    } else if (input$bloco == "bloco6") {
      updateSelectizeInput(
        session,
        inputId = "tipo_do_indicador_blocos4_6_7",
        choices = c(
          "Relacionados à mortalidade materna" = "mortalidade",
          "Relacionados à morbidade materna" = "morbidade"
        )
      )
    } else if (input$bloco == "bloco7") {
      updateSelectizeInput(
        session,
        inputId = "tipo_do_indicador_blocos4_6_7",
        choices = c(
          "Relacionados à mortalidade fetal" = "fetal",
          "Relacionados à mortalidade perinatal" = "perinatal",
          "Relacionados à mortalidade neonatal" = "neonatal",
          "Relacionados à morbidade neonatal" = "morbidade neonatal"

        )
      )
    }
  })

  observeEvent(c(input$tipo_do_indicador_blocos4_6_7, input$nivel), {
    if (input$tipo_do_indicador_blocos4_6_7 == "robson") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6_7",
        choices = c(
          tabela_indicadores$indicador[which(startsWith(tabela_indicadores$indicador, "Porcentagem de nascidos vivos") & tabela_indicadores$bloco == "bloco4")],
          tabela_indicadores$indicador[which(startsWith(tabela_indicadores$indicador, "Porcentagem de cesarianas") & tabela_indicadores$bloco == "bloco4")],
          tabela_indicadores$indicador[which(startsWith(tabela_indicadores$indicador, "Contribuição") & tabela_indicadores$bloco == "bloco4")]
        ),
        server = FALSE
      )
    } else if (input$tipo_do_indicador_blocos4_6_7 == "deslocamento") {
      if (input$nivel %in% c("Municipal", "Estadual")) {
        updateSelectizeInput(
          session,
          inputId = "indicador_blocos4_6_7",
          choices = trimws(tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco4_deslocamento")]),
          server = FALSE
        )
      } else {
        updateSelectizeInput(
          session,
          inputId = "indicador_blocos4_6_7",
          choices = trimws(tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco4_deslocamento" & !base::startsWith(tabela_indicadores$indicador, "Medianas"))]),
          server = FALSE
        )
      }
    } else if (input$tipo_do_indicador_blocos4_6_7 == "mortalidade") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6_7",
        choices = trimws(tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco6")]),
        server = FALSE
      )
    } else if (input$tipo_do_indicador_blocos4_6_7 == "morbidade") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6_7",
        choices = trimws(tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco6_morbidade")]),
        server = FALSE
      )
    } else if (input$tipo_do_indicador_blocos4_6_7 == "fetal") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6_7",
        choices = trimws(tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco7_fetal")]),
        server = FALSE
      )
    } else if (input$tipo_do_indicador_blocos4_6_7 == "perinatal") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6_7",
        choices = trimws(tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco7_perinatal")]),
        server = FALSE
      )
    } else if (input$tipo_do_indicador_blocos4_6_7 == "neonatal") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6_7",
        choices = trimws(tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco7_neonatal")]),
        server = FALSE
      )
    } else if (input$tipo_do_indicador_blocos4_6_7 == "morbidade neonatal") {
      updateSelectizeInput(
        session,
        inputId = "indicador_blocos4_6_7",
        choices = trimws(tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco7_morbidade_neonatal")]),
        server = FALSE
      )
    }
  })


  observeEvent(input$indicador, {
    if (input$indicador == "Porcentagem de nascidos vivos com baixo peso ao nascer") {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco5",
        choices = c("Menor que 1000 g" = "menos_1000", "De 1000 a 1499 g" = "1000_a_1499",
                    "De 1500 a 2499 g" = "1500_a_2499", "Menor que 2500 g" = "menos_2500"),
        label = "Faixa de peso"
      )
    } else if (input$indicador == "Porcentagem de nascidos vivos prematuros") {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco5",
        choices = c("Menos que 28 semanas" = "menos_28", "De 28 a 32 semanas" = "28_a_32",
                    "De 33 a 34 semans" = "33_a_34", "De 35 a 36 semanas" = "35_a_36",
                    "Menos que 37 semanas" = "menos_37"),
        label = "Idade gestacional"
      )
    }
  })

  observeEvent(input$indicador_blocos4_6_7, {
    if (input$indicador_blocos4_6_7 %in% c("Número de óbitos neonatais",
                                           "Número de óbitos perinatais (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
                                           "Taxa de óbitos perinatais (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
                                           "Número de óbitos perinatais (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
                                           "Taxa de óbitos perinatais (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
                                           "Taxa de mortalidade neonatal por 1000 nascidos vivos ",
                                           "Taxa de mortalidade neonatal precoce por 1000 nascidos vivos  ",
                                           "Taxa de mortalidade neonatal tardia por 1000 nascidos vivos  "#,
                                           # "Porcentagem de óbitos fetais por causas evitáveis",
                                           # "Porcentagem de óbitos perinatais por causas evitáveis",
                                           # "Porcentagem de óbitos neonatais por causas evitáveis",
                                           # "Porcentagem de óbitos fetais por grupos de causas",
                                           # "Porcentagem de óbitos perinatais por grupos de causas",
                                           # "Porcentagem de óbitos neonatais por grupos de causas"
                                           )) {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c(
          "Geral" = "geral", "< 1000 g" = "menos1000", "1000 a 1499 g" = "1000_1499", "1500 a 2499 g" = "1500_2499", "Maior ou igual a 2500 g" = "mais2500"
        ),
        label = "Faixa de peso"
      )
    }

    if (input$indicador_blocos4_6_7 %in% c("Porcentagem de óbitos perinatais por grupos de causas evitáveis",
                                           "Porcentagem de óbitos neonatais por grupos de causas evitáveis"
                                           )) {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c("Reduzíveis por ações de imunização" = "imunoprevencao",
          "Reduzíveis por adequada atenção à mulher na gestação" = "mulher_gestacao",
          "Reduzíveis por adequada atenção à mulher no parto" = "parto",
          "Reduzíveis por adequada atenção ao recém-nascido" = "recem_nascido",
          "Reduzíveis por ações de promoção à saúde vinculadas a ações de atenção" = "saude",
          "Reduzíveis por ações de diagnóstico e tratamento adequado" = "tratamento",
          "Reduzíveis por causas mal definidas" = "mal_definidas",
          "Reduzíveis por demais causas" = "outros"),
        label = "Grupos de interesse"
      )
    }

    if (input$indicador_blocos4_6_7 %in% c("Porcentagem de óbitos perinatais por grupos de causas",
                                           "Porcentagem de óbitos neonatais por grupos de causas"    )) {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c("Prematuridade" = "prematuridade",
                           "Infecções" = "infeccoes",
                            "Asfixia/hipóxia" = "asfixia",
                             "Malformação" = "ma_formacao",
                             "Afecções respiratórias do recém-nascido" = "respiratorias",
                              "Fatores maternos relacionados à gravidez" = "gravidez",
                              "Afecções originais no período perinatal" = "afeccoes",
                              "Causas mal definidas" = "mal_definidas",
                              "Demais causas" = "outros"),
        label = "Grupos de interesse"
      )
    }

    if (input$indicador_blocos4_6_7 == "Porcentagem de internações neonatais por grupos de causas"
    ) {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c("Prematuridade" = "prematuridade",
                    "Infecções" = "infeccoes",
                    "Asfixia/Hipóxia" = "asfixia",
                    "Má formação congênita" = "ma_formacao",
                    "Afecções respiratórias do recém-nascido" = "afeccoes_respiratorias",
                    "Fatores maternos relacionados à gravidez" = "fatores_maternos",
                    "Afecções originais no período perinatal" = "afeccoes_perinatal",
                    "Mal definidas" = "mal_definidas",
                    "Icterícia neonatal" = "ictericia",
                    "Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido" = "endocrinos",
                    "Problemas de alimentação do recém-nascido"= "alimentacao",
                    "Transtornos cardíacos originados no período perinatal" = "cardiacos_perinatal",
                    "Demais causas" = "outros"),
        label = "Grupos de interesse"
      )
    }


    if (input$indicador_blocos4_6_7 == "Porcentagem de óbitos fetais por grupos de causas evitáveis") {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c("Reduzíveis por ações de imunização" = "imunoprevencao",
                    "Reduzíveis por adequada atenção à mulher na gestação" = "mulher_gestacao",
                    "Reduzíveis por adequada atenção à mulher no parto" = "parto",
                    "Reduzíveis por adequada atenção ao recém-nascido" = "recem_nascido",
                    #"Reduzíveis por ações de promoção à saúde vinculadas a ações de atenção" = "saude",
                    "Reduzíveis por ações de diagnóstico e tratamento adequado" = "tratamento",
                    "Reduzíveis por causas mal definidas" = "mal_definidas",
                    "Reduzíveis por demais causas" = "outros"),
        label = "Grupos de interesse"
      )
    }

    if (input$indicador_blocos4_6_7 == "Porcentagem de óbitos fetais por grupos de causas") {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c("Prematuridade" = "prematuridade",
                    "Infecções" = "infeccoes",
                    "Asfixia/hipóxia" = "asfixia",
                    "Malformação" = "ma_formacao",
                    #"Afecções respiratórias do recém-nascido" = "respiratorias",
                    "Fatores maternos relacionados à gravidez" = "gravidez",
                    "Afecções originais no período perinatal" = "afeccoes",
                    #"Causas mal definidas" = "mal_definidas",
                    "Demais causas" = "outros"),
        label = "Grupos de interesse"
      )
    }

    if (input$indicador_blocos4_6_7 %in% c("Número de óbitos fetais (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g)",
                                           "Número de óbitos fetais (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g)",
                                           "Taxa de mortalidade fetal (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g)",
                                           "Taxa de mortalidade fetal (eto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g)")) {
      updateSelectizeInput(
        session,
        inputId = "indicador_duas_caixinhas_adicionais1",
        choices = c(
          "Geral" = "geral", "Antes do parto" = "antes", "Durante o parto" = "durante"
        ),
        label = "Momento do óbito"
      )
      updateSelectizeInput(
        session,
        inputId = "indicador_duas_caixinhas_adicionais2",
        choices = c(
          "Geral" = "geral", "< 1000 g" = "menos1000", "1000 a 1499 g" = "1000_1499", "1500 a 2499 g" = "1500_2499", "Maior ou igual a 2500 g" = "mais2500"
        ),
        label = "Faixa de peso"
      )
    }
    if (input$indicador_blocos4_6_7 == "Porcentagem de nascidos vivos segundo local de ocorrência do parto") {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c("No município de residência" = "municipio_res",
                    "Fora do município de residência" = "fora_municipio_res",
                    "Fora do município, mas na microrregião de saúde de residência" = "rsaude_res",
                    "Fora da microrregião de saúde, mas na macrorregião de saúde de residência" = "macro_rsaude_res",
                    "Fora da macrorregião de saúde, mas dentro da UF de residência" = "macro_rsaude_res",
                    "Fora da UF de residência" = "fora_uf_res"

        ),
        label = "Local do parto"
      )
    }

    if (input$indicador_blocos4_6_7 == "Medianas de deslocamento segundo o local de ocorrência do parto") {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c("Medianas de deslocamento para partos ocorridos fora do municipio de residência" = "fora_municipio",
                    "Medianas de deslocamento para partos ocorridos fora do município, mas na microrregião de saúde de residência" = "na_regiao",
                    "Medianas de deslocamento para partos ocorridos fora da microrregião de saúde, mas na macrorregião de saúde de residência" = "na_macrorregiao",
                    "Medianas de deslocamento para partos ocorridos fora da macrorregião de saúde, mas na UF de residência" = "fora_macrorregiao",
                    "Medianas de deslocamento do total de partos ocorridos fora da UF de residência" = "fora_uf"
        ),
        label = "Local do parto"
      )
    }


    if (input$indicador_blocos4_6_7 == "Porcentagem de partos com peso < 1500g segundo local de ocorrência do parto") {
      updateSelectizeInput(
        session,
        inputId = "indicador_uma_caixinha_adicional_bloco7",
        choices = c("Na macrorregião de saúde e em estabelecimento que tem pelo menos um leito de UTI" = "na_macro_com_uti",
                    "Na macrorregião de saúde, mas em estabelecimento que não tem leito de UTI" = "na_macro_sem_uti",
                    "Fora da macrorregião de saúde, mas em estabelecimento que tem pelo menos um leito de UTI" = "fora_macro_com_uti",
                    "Fora da macrorregião de saúde e em estabelecimento que não tem leito de UTI" = "fora_macro_sem_uti",
                    "Na macrorregião de saúde, mas sem informação sobre leito de UTI" = "na_macro_sem_inf",
                    "Fora da macrorregião de saúde, mas sem informação sobre leito de UTI" = "fora_macro_sem_inf"
                    ),
        label = "Localidade e tipo de estabelecimento"
      )
    }


  })

  filtros <- eventReactive(input$pesquisar, {
    list(
      ano = input$ano,
      ano2 = input$ano2,
      nivel = input$nivel,
      regiao = input$regiao,
      estado = input$estado,
      estado_macro = input$estado_macro,
      estado_micro = input$estado_micro,
      estado_municipio = input$estado_municipio,
      macro = input$macro,
      micro = input$micro,
      municipio = input$municipio,
      comparar = input$comparar,
      nivel2 = input$nivel2,
      regiao2 = input$regiao2,
      estado2 = input$estado2,
      estado_macro2 = input$estado_macro2,
      estado_micro2 = input$estado_micro2,
      estado_municipio2 = input$estado_municipio2,
      macro2 = input$macro2,
      micro2 = input$micro2,
      municipio2 = input$municipio2,
      bloco = input$bloco,
      indicador = input$indicador,
      pesquisar = input$pesquisar,
      mostrar_referencia = input$mostrar_referencia,
      tipo_do_indicador_blocos4_6_7 = input$tipo_do_indicador_blocos4_6_7,
      indicador_blocos4_6_7 = input$indicador_blocos4_6_7,
      indicador_uma_caixinha_adicional_bloco5 = input$indicador_uma_caixinha_adicional_bloco5,
      indicador_uma_caixinha_adicional_bloco7 = input$indicador_uma_caixinha_adicional_bloco7,
      indicador_duas_caixinhas_adicionais1 = input$indicador_duas_caixinhas_adicionais1,
      indicador_duas_caixinhas_adicionais2 = input$indicador_duas_caixinhas_adicionais2
    )
  },
  ignoreNULL = FALSE
  )

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$decimalPoint <- ","
  hcoptslang$thosandsSep <- "."
  options(highcharter.lang = hcoptslang)

  options(reactable.theme = reactable::reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#e5efff",
    highlightColor = "#CDDEFC",
    cellPadding = "8px 12px",
    searchInputStyle = list(width = "100%")
  )
  )

  options(pagedown.remote.maxattempts=60) # number of attempt in total

}



