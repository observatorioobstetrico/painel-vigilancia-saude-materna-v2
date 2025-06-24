#' bloco_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(class = "fonte-titulos-pagina", tags$b(HTML("Assistência pré-natal: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    fluidRow(
      column(
        width = 4,
        HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
        div(
          HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
          shinyWidgets::actionBttn(
            inputId = ns('botao_resumo'),
            icon = icon('question'),
            style = 'material-circle',
            color = "primary",
            size = 'xs'
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 12,
            HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
            uiOutput(ns("input_localidade_resumo")),
            align = "center"
          )
        ),
        # fluidRow(
        #   bs4Dash::box(
        #     width = 12,
        #     collapsible = FALSE,
        #     headerBorder = FALSE,
        #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
        #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart"), height = 530))
        #   )
        # ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i1")), proxy.height = "300px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i2")), proxy.height = "300px")
          ),
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i5")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i3")), proxy.height = "325px")
          )

        ),
        fluidRow(
          column(
            offset = 3,
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i4")), proxy.height = "325px")
          )
        ),
      ),
      column(
        width = 8,
        fluidRow(
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 11%; display: flex; align-items: center;",
                HTML("<b class = 'fonte-muito-grande'> Cobertura de assistência pré-natal &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao1"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao1"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1"), height = 455)))
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 11%; display: flex; align-items: center;",
                HTML(
                  "<b class = 'fonte-muito-grande'> Porcentagem de mulheres com início do pré-natal até 12 semanas de gestação &nbsp;</b>"
                ),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao2"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao2"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2"), height = 455))
            )
          ),

          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 11%; display: flex; align-items: center;",
                HTML(
                  "<b class = 'fonte-muito-grande'> Porcentagem de mulheres com oito ou mais consultas de pré-natal &nbsp;</b>"
                ),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao5"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao3"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5"), height = 455))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 11%; display: flex; align-items: center;",
                HTML(
                  "<b class = 'fonte-muito-grande'> Porcentagem de mulheres com número adequado de consultas de pré-natal para a idade gestacional no parto &nbsp;</b>"
                ),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao3"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao3"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3"), height = 455))
            )
          ),
          column(
            offset = 3,
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 11%; display: flex; align-items: center;",
                HTML(
                  "<b class = 'fonte-muito-grande'> Incidência de sífilis congênita por mil nascidos vivos &nbsp;</b>"
                ),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao4"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao4"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4"), height = 455))
            )
          )
        )
      )
    )
  )
}

#' bloco_3 Server Functions
#'
#' @noRd

mod_bloco_3_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco3_calcs <- data.frame(
      tipo = c("local", "referencia"),
      porc_inicio_prec = c("round(sum(mulheres_com_inicio_precoce_do_prenatal) / sum(total_de_nascidos_vivos) * 100, 1)", "dplyr::first(95)"),
      porc_sc = c("round(sum(casos_sc[ano < 2024]) / sum(total_de_nascidos_vivos[ano < 2024]) * 1000, 1)", "dplyr::first(0.5[ano < 2024])"),
      cobertura_pre_natal = c("round(sum(mulheres_com_pelo_menos_uma_consulta_prenatal[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 1)", "dplyr::first(95)"),
      porc_7 = c("round(sum(mulheres_com_mais_de_sete_consultas_prenatal[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 1)", "dplyr::first(95)"),
      porc_consultas_adequadas = c("round(sum(mulheres_com_consultas_prenatal_adequadas[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 1)", "dplyr::first(95)")
    )


    # Criando alguns outputs para a UI ----------------------------------------
    ## Criando o output que recebe a localidade e o ano escolhidos ------------
    output$titulo_localidade <- renderUI({

      if (length(filtros()$ano2[1]:filtros()$ano2[2]) > 1) {
        ano <- glue::glue("{filtros()$ano2[1]} a {filtros()$ano2[2]}")
      } else {
        ano <- filtros()$ano2[1]
      }

      if (filtros()$comparar == "Não") {
        local1 <- dplyr::case_when(
          filtros()$nivel == "nacional" ~ "Brasil",
          filtros()$nivel == "regional" ~ filtros()$regiao,
          filtros()$nivel == "estadual" ~ filtros()$estado,
          filtros()$nivel == "macro" ~ filtros()$macro,
          filtros()$nivel == "micro" ~ filtros()$micro,
          filtros()$nivel == "municipal" ~ filtros()$municipio
        )
        texto <- glue::glue("({local1}, {ano})")
      } else {
        local1 <- dplyr::case_when(
          filtros()$nivel == "nacional" ~ "Brasil",
          filtros()$nivel == "regional" ~ filtros()$regiao,
          filtros()$nivel == "estadual" ~ filtros()$estado,
          filtros()$nivel == "macro" ~ filtros()$macro,
          filtros()$nivel == "micro" ~ filtros()$micro,
          filtros()$nivel == "municipal" ~ filtros()$municipio
        )
        local2 <- dplyr::case_when(
          filtros()$nivel2 == "nacional" ~ "Brasil",
          filtros()$nivel2 == "regional" ~ filtros()$regiao2,
          filtros()$nivel2 == "estadual" ~ filtros()$estado2,
          filtros()$nivel2 == "macro" ~ filtros()$macro2,
          filtros()$nivel2 == "micro" ~ filtros()$micro2,
          filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
          filtros()$nivel2 == "municipios_semelhantes" ~ "municípios semelhantes"
        )
        texto <- glue::glue("({local1} e {local2}, {ano})")
      }

      tags$b(texto, class = "fonte-titulos-pagina")
    })

    ## Criando o output que receberá os nomes dos locais selecionados quando há comparação --------
    output$input_localidade_resumo <- renderUI({
      localidade_original <- dplyr::case_when(
        filtros()$nivel == "nacional" ~ "Brasil",
        filtros()$nivel == "regional" ~ filtros()$regiao,
        filtros()$nivel == "estadual" ~ filtros()$estado,
        filtros()$nivel == "macro" ~ filtros()$macro,
        filtros()$nivel == "micro" ~ filtros()$micro,
        filtros()$nivel == "municipal" ~ filtros()$municipio
      )

      localidade_comparacao <- dplyr::case_when(
        filtros()$nivel2 == "nacional" ~ "Brasil",
        filtros()$nivel2 == "regional" ~ filtros()$regiao2,
        filtros()$nivel2 == "estadual" ~ filtros()$estado2,
        filtros()$nivel2 == "macro" ~ filtros()$macro2,
        filtros()$nivel2 == "micro" ~ filtros()$micro2,
        filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
        filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
      )

      if (filtros()$comparar == "Sim") {
        radioButtons(
          inputId = ns("localidade_resumo"),
          label = NULL,
          choiceNames = list(
            localidade_original,
            localidade_comparacao
          ),
          choiceValues = list("escolha1", "escolha2"),
          selected = "escolha1",
          inline = TRUE
        )
      }
    })

    ## Criando o pop-up com a informação sobre o resumo do período -------------
    observeEvent(input$botao_resumo, {
      shinyalert::shinyalert(
        html = TRUE,
        title = '<div class = "fonte-titulos-modal" style = "color: #657065"> Sobre o "Resumo do período" </div>',
        text = '
          <div style = "text-align: justify; text-justify: inter-word;">
            Todas as caixinhas que estão sob o "Resumo do período", na esquerda da página, referem-se aos valores dos indicadores calculados considerando todo o período selecionado.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando alguma comparação é feita, o usuário pode selecionar para qual localidade o resumo do período será calculado clicando em um dos botões que irão aparecer em cima das caixinhas.
          </div>',
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

    ## Para os botões de alerta quanto à incompletude e cobertura --------------
    ### Calculando os indicadores de incompletude ------------------------------
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          consprenat = round(sum(consprenat_incompletos, na.rm = TRUE)/sum(consprenat_totais, na.rm = TRUE) * 100, 1),
          mesprenat = round(sum(mesprenat_incompletos, na.rm = TRUE)/sum(mesprenat_totais, na.rm = TRUE) * 100, 1),
          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })

    ### Calculando os indicadores de cobertura --------------------------------
    data_cobertura <- reactive({
      if (filtros()$nivel == "municipal") {
        sub_registro_sinasc_muni_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "estadual") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$estado
          )
      } else if (filtros()$nivel == "regional") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$regiao
          )
      } else if (filtros()$nivel == "nacional") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == "Brasil"
          )
      } else {
        data.frame(
          ano = filtros()$ano2[1]:filtros()$ano2[2],
          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          ),
          cobertura = 100
        )
      }
    })

    ### Juntando os dados de incompletude e cobertura -------------------------
    data_incompletude <- reactive({dplyr::full_join(data_incompletude_aux(), data_cobertura(), by = c("ano", "localidade"))})

    ### Ativando os botões de alerta quando necessário ------------------------
    #### Cobertura de assistência pré-natal -----------------------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)] > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)],
        variavel_incompletude1 = "CONSPRENAT",
        descricao_incompletude1 = "em branco",
        df = data_incompletude() |> dplyr::filter(ano >= 2014),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de mulheres com inicio precoce do pré-natal --------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$mesprenat > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao2, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$mesprenat,
        variavel_incompletude1 = "MESPRENAT",
        descricao_incompletude1 = "em branco",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de mulheres com número adequado de consultas de pré-natal ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)] > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)],
        variavel_incompletude1 = "CONSPRENAT",
        descricao_incompletude1 = "em branco",
        df = data_incompletude() |> dplyr::filter(ano >= 2014),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de mulheres com oito ou mais consultas de pré-natal  -----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao5", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)] > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao5", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)],
        variavel_incompletude1 = "CONSPRENAT",
        descricao_incompletude1 = "em branco",
        df = data_incompletude() |> dplyr::filter(ano >= 2014),
        cobertura = data_incompletude()$cobertura
      )
    })


    # Para o resumo do período ------------------------------------------------
    ## Calculando uma média dos indicadores para o período selecionado --------
    ### Para a localidade selecionada -----------------------------------------
    data3_resumo <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$comparar == "Não") {
            if (filtros()$nivel == "nacional")
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            else if (filtros()$nivel == "regional")
              regiao == filtros()$regiao
            else if (filtros()$nivel == "estadual")
              uf == filtros()$estado
            else if (filtros()$nivel == "macro")
              macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
            else if(filtros()$nivel == "micro")
              r_saude == filtros()$micro & uf == filtros()$estado_micro
            else if(filtros()$nivel == "municipal")
              municipio == filtros()$municipio & uf == filtros()$estado_municipio
          } else {
            req(input$localidade_resumo)
            if (input$localidade_resumo == "escolha1") {
              if (filtros()$nivel == "nacional")
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              else if (filtros()$nivel == "regional")
                regiao == filtros()$regiao
              else if (filtros()$nivel == "estadual")
                uf == filtros()$estado
              else if (filtros()$nivel == "macro")
                macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
              else if(filtros()$nivel == "micro")
                r_saude == filtros()$micro & uf == filtros()$estado_micro
              else if(filtros()$nivel == "municipal")
                municipio == filtros()$municipio & uf == filtros()$estado_municipio
            } else {
              if (filtros()$nivel2 == "nacional")
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              else if (filtros()$nivel2 == "regional")
                regiao == filtros()$regiao2
              else if (filtros()$nivel2 == "estadual")
                uf == filtros()$estado2
              else if (filtros()$nivel2 == "macro")
                macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
              else if(filtros()$nivel2 == "micro")
                r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
              else if(filtros()$nivel2 == "municipal")
                municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
              else if (filtros()$nivel2 == "municipios_semelhantes")
                grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
            }
          }
        ) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros(), localidade_resumo = input$localidade_resumo)
    })

    ### Para a referência -----------------------------------------------------
    data3_resumo_referencia <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros(), referencia = TRUE)
    })


    ## Criando o output do gráfico de radar -----------------------------------
    ### Definindo os indicadores que aparecerão no gráfico
    selected_indicators <- c(
      "cobertura_pre_natal",
      "porc_inicio_prec",
      "porc_7",
      "porc_consultas_adequadas",
      "porc_sc"
    )

    ### Selecionando colunas relevantes nos dataframes de resumo e arrumando seus formatos
    df <- reactive({
      data3_resumo()[, c('class', selected_indicators)] |>
        dplyr::mutate(
          class = ifelse(grepl("Brasil \\(valor de referência\\)", class), "Brasil", class)
        ) |>
        tidyr::pivot_longer(
          !class,
          names_to = "indicador",
          values_to = "values1"
        ) |>
        dplyr::mutate(
          sufixo = c(rep("%", 4), "")
        )
    })

    df2 <- reactive({
      data3_resumo_referencia()[, selected_indicators] |>
        dplyr::mutate(class = "Referência") |>
        tidyr::pivot_longer(
          !class,
          names_to = "indicador",
          values_to = "values2"
        ) |>
        dplyr::mutate(
          tipo_de_referencia = lapply(selected_indicators, function(indicador_abrev) tabela_indicadores$descricao_referencia[tabela_indicadores$nome_abreviado == indicador_abrev]) |> unlist(),
          sufixo = c(rep("%", 4), "")
        )
    })

    ### Criando o output
    output$spider_chart <- highcharter::renderHighchart({
      # Categorias para o eixo x
      categories <- lapply(selected_indicators, function(indicador_abrev) gsub("Porcentagem", "%", tabela_radar$indicador[tabela_radar$nome_abreviado == indicador_abrev])) |> unlist()

      # Obter valores para o gráfico
      values1 <- round(as.numeric(unlist(df()[, "values1"])), 3)
      values2 <- round(as.numeric(unlist(df2()[, "values2"])), 3)

      # Encontrar o valor máximo dos dados
      max_value1 <- max(values1, na.rm = TRUE)
      max_value2 <- max(values2, na.rm = TRUE)

      # Definir o valor máximo do eixo y como o próximo múltiplo de 100 maior que o valor máximo
      yAxis_max <- ceiling(max(max_value1, max_value2) / 100) * 100

      # Criar gráfico
      highcharter::highchart() |>
        highcharter::hc_chart(polar = TRUE, type = "line", backgroundColor = "transparent") |>
        highcharter::hc_pane(size = '65%') |>
        highcharter::hc_xAxis(categories = categories, tickmarkPlacement = 'on', lineWidth = 0, labels = list(style = list(fontWeight = 'bold', fontSize = '12px'))) |>
        highcharter::hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = yAxis_max) |>
        highcharter::hc_add_series(
          name = df()$class[1],
          data = df() |> dplyr::select(y = values1, sufixo),
          color = "#2c115f",
          lineWidth = 2,
          marker = list(enabled = FALSE, symbol = "circle", radius = 4),
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'>&#9679 </span> {series.name}: <b> {point.y}{point.sufixo} </b><br>"
          )
        ) |>
        highcharter::hc_add_series(
          name = df2()$class[1],
          data = df2() |> dplyr::select(y = values2, tipo_de_referencia, sufixo),
          color = "#b73779",
          lineWidth = 2,
          opacity = 0.6,
          dashStyle = "ShortDash",
          marker = list(enabled = FALSE, symbol = "diamond", radius = 4),
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'>&#9670 </span> {series.name} ({point.tipo_de_referencia}): <b> {point.y}{point.sufixo} </b>"
          )
        ) |>
        highcharter::hc_legend(align = 'center', layout = 'horizontal', itemStyle = list(fontWeight = 'bold', fontSize = '14px')) |>
        highcharter::hc_tooltip(shared = TRUE) |>
        highcharter::hc_legend(itemMarginTop = 25)  # Ajustar a margem entre itens da legenda
    })



    ## Criando os outputs das caixinhas ---------------------------------------
    ### Cobertura de assistência pré-natal ------------------------------------
    output$caixa_b3_i1 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "cobertura_pre_natal",
        titulo = "Cobertura de assistência pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    ### Porcentagem de mulheres com inicio precoce do pré-natal -----------------
    output$caixa_b3_i2 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_inicio_prec",
        titulo = "Porcentagem de mulheres com início precoce do pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    ### Porcentagem de mulheres com número adequado de consultas de pré-natal -----------------
    output$caixa_b3_i3 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_consultas_adequadas",
        titulo = "Porcentagem de mulheres com número adequado de consultas de pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    ### Incidência de sífilis congênita por mil nascidos vivos -----------------
    output$caixa_b3_i4 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_sc",
        titulo = "Incidência de sífilis congênita por mil nascidos vivos",
        tem_meta = TRUE,
        valor_de_referencia = 0.5,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "meta OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b3_i5 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_7",
        titulo = "Porcentagem de mulheres com oito ou mais consultas de pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })


    # Para os gráficos --------------------------------------------------------
    cols <- c("#2c115f", "#b73779", "#fc8961", "#000004FF", "#f1605d")

    ## Calculando os indicadores para cada ano do período selecionado ---------
    ### Para a localidade selecionada -----------------------------------------
    data3 <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros()) |>
        dplyr::mutate(class = ifelse(class == "Brasil (valor de referência)", "Brasil", class))
    })

    ### Para a comparação selecionada -----------------------------------------
    data3_comp <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel2 == "regional")
            regiao == filtros()$regiao2
          else if (filtros()$nivel2 == "estadual")
            uf == filtros()$estado2
          else if (filtros()$nivel2 == "macro")
            macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
          else if (filtros()$nivel2 == "micro")
            r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
          else if (filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros(), comp = TRUE) |>
        dplyr::mutate(class = ifelse(class == "Brasil (valor de referência)", "Brasil", class))
    })

    ### Para a referência -----------------------------------------------------
    data3_referencia <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros(), referencia = TRUE)
    })


    ## Criando os outputs dos gráficos ----------------------------------------
    ### Cobertura de assistência pré-natal ------------------------------------
    output$plot1 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2014,
          "Este indicador só está disponível a partir de 2014."
        )
      )
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = cobertura_pre_natal, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia() |> dplyr::filter(ano >= 2014),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = cobertura_pre_natal, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = max(2014, filtros()$ano2[1]):filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = cobertura_pre_natal, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = cobertura_pre_natal, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = max(2014, filtros()$ano2[1]):filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data3_referencia() |> dplyr::filter(ano >= 2014),
              type = "line",
              name = "Referência (recomendações OMS)",
              highcharter::hcaes(x = ano, y = cobertura_pre_natal, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    ### Porcentagem de mulheres com inicio precoce do pré-natal -----------------
    output$plot2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia(),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data3_referencia(),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.6
          )
        }
      }

    })

    ### Porcentagem de mulheres com número adequado de consultas de pré-natal -----------------
    # output$plot3 <- highcharter::renderHighchart({
    #   validate(
    #     need(
    #       filtros()$ano2[2] >= 2014,
    #       "Este indicador só está disponível a partir de 2014."
    #     )
    #   )
    #   if (filtros()$comparar == "Não") {
    #     grafico_base <- highcharter::highchart() |>
    #       highcharter::hc_add_series(
    #         data = data3_referencia() |> dplyr::filter(ano >= 2014),
    #         name = "Referência (recomendações OMS)",
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
    #       ) |>
    #       highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
    #       highcharter::hc_xAxis(title = list(text = ""), categories = max(2014, filtros()$ano2[1]):filtros()$ano2[2], allowDecimals = FALSE) |>
    #       highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100, max = 100) |>
    #       highcharter::hc_colors(cols)
    #     if (filtros()$nivel == "nacional") {
    #       grafico_base
    #     } else {
    #       grafico_base |>
    #         highcharter::hc_add_series(
    #           data = data3_referencia() |> dplyr::filter(ano >= 2014),
    #           type = "line",
    #           name = "Referência (recomendações OMS)",
    #           highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class),
    #           dashStyle = "ShortDot",
    #           opacity = 0.8
    #         )
    #     }
    #   } else {
    #     grafico_base <- highcharter::highchart() |>
    #       highcharter::hc_add_series(
    #         data = data3_referencia() |> dplyr::filter(ano >= 2014),
    #         name = "Referência (recomendações OMS)",
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
    #       ) |>
    #       highcharter::hc_add_series(
    #         data = data3_comp() |> dplyr::filter(ano >= 2014),
    #         name = dplyr::if_else(filtros()$nivel2 == "nacional", "Brasil", unique(data3_comp()$class)),
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
    #       ) |>
    #       highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
    #       highcharter::hc_xAxis(title = list(text = ""), categories = max(2014, filtros()$ano2[1]):filtros()$ano2[2], allowDecimals = FALSE) |>
    #       highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
    #       highcharter::hc_colors(cols)
    #     if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
    #       grafico_base
    #     } else {
    #       grafico_base |>
    #         highcharter::hc_add_series(
    #           data = data3_referencia() |> dplyr::filter(ano >= 2014),
    #           type = "line",
    #           name = "Referência (recomendações OMS)",
    #           highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class),
    #           dashStyle = "ShortDot",
    #           opacity = 0.6
    #         )
    #     }
    #   }
    # })

    output$plot3 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2014,
          "Este indicador só está disponível a partir de 2014."
        )
      )
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia() |> dplyr::filter(ano >= 2014),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data3_referencia() |> dplyr::filter(ano >= 2014),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.6
          )
        }
      }
    })

    ### Incidência de sífilis congênita por mil nascidos vivos -----------------
    output$plot4 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia(),
            type = "line",
            name = "Referência (meta OMS)",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data3_referencia(),
            type = "line",
            name = "Referência (meta OMS)",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.6
          )
        }
      }
    })



    #gráficos porcentagem mais de 7 consultas
    output$plot5 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2014,
          "Este indicador só está disponível a partir de 2014."
        )
      )
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia() |> dplyr::filter(ano >= 2014),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data3_referencia() |> dplyr::filter(ano >= 2014),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.6
          )
        }
      }
    })



  })
}


## To be copied in the UI
# mod_bloco_3_ui("bloco_3_1")

## To be copied in the server
# mod_bloco_3_server("bloco_3_1")
