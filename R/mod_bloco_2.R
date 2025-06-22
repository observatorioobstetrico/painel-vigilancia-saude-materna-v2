#' bloco_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(
        tags$b(
          HTML("Planejamento reprodutivo: série histórica"),
          htmlOutput(ns("titulo_localidade"), inline = TRUE)
        ),
        class = "fonte-titulos-pagina",
        style = "padding-left: 0.4em"
      ),
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
            shinycssloaders::withSpinner(
              uiOutput(ns("caixa_b2_i1")),
              proxy.height = "300px"
            )
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(
              uiOutput(ns("caixa_b2_i2")),
              proxy.height = "300px"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(
              uiOutput(ns("caixa_b2_i3")),
              proxy.height = "325px"
            )
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(
              uiOutput(ns("caixa_b2_i4")),
              proxy.height = "325px"
            )
          )
        )
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
                style = "height: 10%; display: flex; align-items: center;",
                HTML(
                  "<b class = 'fonte-muito-grande'> Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil) &nbsp;</b>"
                ),
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

              fluidRow(
                column(
                  width = 12,
                  selectizeInput(
                    inputId = ns("faixa_etaria"),
                    label = "Faixa etária",
                    options = list(placeholder = "Selecione a faixa etária"),
                    choices = c(
                      "Menos de 20 anos" = "porc_menor20",
                      "De 10 a 14 anos" = "porc_10_a_14",
                      "De 15 a 19 anos" = "porc_15_a_19"
                    ),
                    width = "100%",
                    selected = "porc_menor20"
                  )
                )
              ),

              shinycssloaders::withSpinner(highcharter::highchartOutput(
                ns("plot1"),
                height = 385
              ))
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
                style = "height: 10%; display: flex; align-items: center;",
                HTML(
                  "<b class = 'fonte-muito-grande'> Porcentagem de mulheres com mais de 3 partos anteriores &nbsp;</b>"
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(
                ns("plot2"),
                height = 460
              ))
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
                style = "height: 10%; display: flex; align-items: center;",
                HTML(
                  "<b class = 'fonte-muito-grande'> Taxa de abortos inseguros por mil mulheres em idade fértil &nbsp;</b>"
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
              fluidRow(
                column(
                  width = 12,
                  selectizeInput(
                    inputId = ns("tipo_taxa_aborto"),
                    label = "Grupo de mulheres",
                    options = list(
                      placeholder = "Selecione o grupo de mulheres"
                    ),
                    choices = c(
                      "Todas" = "geral",
                      "Usuárias exclusivas do SUS" = "sus",
                      "Beneficiárias de planos de saúde privados" = "ans"
                    ),
                    width = "100%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(
                ns("plot3"),
                height = 385
              ))
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
                style = "height: 10%; display: flex; align-items: center;",
                HTML(
                  "<b class = 'fonte-muito-grande'> Razão de abortos inseguros por 100 nascidos vivos &nbsp;</b>"
                ),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao5"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao5"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  selectizeInput(
                    inputId = ns("tipo_razao_aborto"),
                    label = "Grupo de mulheres",
                    options = list(
                      placeholder = "Selecione o grupo de mulheres"
                    ),
                    choices = c(
                      "Todas" = "geral",
                      "Usuárias exclusivas do SUS" = "sus",
                      "Beneficiárias de planos de saúde privados" = "ans"
                    ),
                    width = "100%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(
                ns("plot4"),
                height = 385
              ))
            )
          )
        )
      )
    )
  )
}

#' bloco_2 Server Functions
#'
#' @noRd

mod_bloco_2_server <- function(id, filtros) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco2_calcs <- data.frame(
      tipo = c("local", "referencia"),
      porc_10_a_14 = rep("round(sum(total_de_nascidos_vivos_10_a_14) / sum(populacao_feminina_10_a_14) * 1000, 1)", 2),
      porc_15_a_19 = rep("round(sum(total_de_nascidos_vivos_15_a_19) / sum(populacao_feminina_15_a_19) * 1000, 1)", 2),
      porc_menor20 = c("round(sum(nvm_menor_que_20) / sum(populacao_feminina_10_a_14 + populacao_feminina_15_a_19) * 1000, 1)", "dplyr::first(20)"),
      porc_mais_3pt = rep("round(sum(mulheres_com_mais_de_tres_partos_anteriores) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
      geral_tx_abortos_mil_mulheres_lim_inf = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 3) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      geral_tx_abortos_mil_mulheres_valor_medio = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      geral_tx_abortos_mil_mulheres_lim_sup = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 6)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      sus_tx_abortos_mil_mulheres_lim_inf = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 3) / sum(pop_fem_sus_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      sus_tx_abortos_mil_mulheres_valor_medio = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) / sum(pop_fem_sus_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      sus_tx_abortos_mil_mulheres_lim_sup = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5) / sum(pop_fem_sus_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      ans_tx_abortos_mil_mulheres_lim_inf = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) / sum(pop_fem_ans_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      ans_tx_abortos_mil_mulheres_valor_medio = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5) / sum(pop_fem_ans_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      ans_tx_abortos_mil_mulheres_lim_sup = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 6) / sum(pop_fem_ans_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1)", 2),
      geral_tx_abortos_cem_nascidos_vivos_lim_inf = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 3) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4)) / sum(total_de_nascidos_vivos_10_a_49[ano >= 2015 & ano <= 2023]) * 100, 1)", 2),
      geral_tx_abortos_cem_nascidos_vivos_valor_medio = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49[ano >= 2015 & ano <= 2023]) * 100, 1)", 2),
      geral_tx_abortos_cem_nascidos_vivos_lim_sup = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 6)) / sum(total_de_nascidos_vivos_10_a_49[ano >= 2015 & ano <= 2023]) * 100, 1)", 2),
      sus_tx_abortos_cem_nascidos_vivos_lim_inf = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 3) / sum(total_de_nascidos_vivos_10_a_49_sus[ano >= 2015 & ano <= 2023]) * 100, 1)", 2),
      sus_tx_abortos_cem_nascidos_vivos_valor_medio = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus[ano >= 2015 & ano <= 2023]) * 100, 1)", 2),
      sus_tx_abortos_cem_nascidos_vivos_lim_sup = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_sus[ano >= 2015 & ano <= 2023]) * 100, 1)", 2),
      ans_tx_abortos_cem_nascidos_vivos_lim_inf = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_ans[ano >= 2015 & ano <= 2023]) * 100, 1)", 2),
      ans_tx_abortos_cem_nascidos_vivos_valor_medio = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans[ano >= 2015 & ano <= 2023]) * 100, 1)", 2),
      ans_tx_abortos_cem_nascidos_vivos_lim_sup = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 6) / sum(total_de_nascidos_vivos_10_a_49_ans[ano >= 2015 & ano <= 2023]) * 100, 1)", 2)
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
          filtros()$nivel2 == "municipios_semelhantes" ~
            "municípios semelhantes"
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
        filtros()$nivel2 == "municipios_semelhantes" ~
          "Média dos municípios semelhantes"
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
        title = '<div class = "fonte-titulos-modal" style = "color: #656565"> Sobre o "Resumo do período" </div>',
        text = '
          <div style = "text-align: justify; text-justify: inter-word;">
            Todas as caixinhas que estão sob o "Resumo do período", na esquerda da página, referem-se aos valores dos indicadores calculados considerando todo o período selecionado.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando alguma comparação é feita, o usuário pode selecionar para qual localidade o resumo do período será calculado clicando em um dos botões que irão aparecer em cima das caixinhas.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Para este bloco, as caixinhas relacionadas aos indicadores de abortos inseguros mudam de acordo com os grupos de mulheres selecionados nos gráficos.
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
          else if (filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if (filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          idademae = round(sum(idademae_incompletos, na.rm = TRUE) / sum(idademae_totais, na.rm = TRUE) * 100, 1),
          qtdpartces = round(sum(qtdpartces_incompletos, na.rm = TRUE) / sum(qtdpartces_totais, na.rm = TRUE) * 100, 1),
          qtdpartnor = round(sum(qtdpartnor_incompletos, na.rm = TRUE) / sum(qtdpartnor_totais, na.rm = TRUE) * 100, 1),
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
    data_incompletude <- reactive({
      dplyr::full_join(
        data_incompletude_aux(),
        data_cobertura(),
        by = c("ano", "localidade")
      )
    })

    ### Ativando os botões de alerta quando necessário ------------------------
    #### Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil) ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(
        id = "mostrar_botao1",
        anim = TRUE,
        animType = "fade",
        time = 0.8
      )
      req(any(data_incompletude()$idademae > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(
        id = "mostrar_botao1",
        anim = TRUE,
        animType = "fade",
        time = 0.8
      )
    })

    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$idademae,
        variavel_incompletude1 = "IDADEMAE",
        descricao_incompletude1 = "ignorados, em branco ou maiores que 55",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de mulheres com mais de 3 partos anteriores --------------
    observeEvent(
      filtros()$pesquisar, {
        shinyjs::hide(
          id = "mostrar_botao2",
          anim = TRUE,
          animType = "fade",
          time = 0.8
        )
        req(
          any(data_incompletude()$qtdpartces > 5, na.rm = TRUE) |
            any(data_incompletude()$qtdpartnor > 5, na.rm = TRUE) |
            any(data_incompletude()$cobertura < 90, na.rm = TRUE)
        )
        shinyjs::show(
          id = "mostrar_botao2",
          anim = TRUE,
          animType = "fade",
          time = 0.8
        )
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$botao2, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$qtdpartces,
        variavel_incompletude1 = "QTDPARTCES",
        descricao_incompletude1 = "em branco ou preenchidos com 99",
        incompletude2 = data_incompletude()$qtdpartnor,
        variavel_incompletude2 = "QTDPARTNOR",
        descricao_incompletude2 = "em branco ou preenchidos com 99",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    # Para o resumo do período ------------------------------------------------
    ## Calculando uma média dos indicadores para o período selecionado --------
    ### Para a localidade selecionada -----------------------------------------
    data2_resumo <- reactive({
      dplyr::left_join(bloco2, bloco6) |>
        dplyr::mutate(dplyr::across(
          dplyr::everything(),
          ~ replace(., is.na(.), 0)
        )) |>
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
            else if (filtros()$nivel == "micro")
              r_saude == filtros()$micro & uf == filtros()$estado_micro
            else if (filtros()$nivel == "municipal")
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
              else if (filtros()$nivel == "micro")
                r_saude == filtros()$micro & uf == filtros()$estado_micro
              else if (filtros()$nivel == "municipal")
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
              else if (filtros()$nivel2 == "micro")
                r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
              else if (filtros()$nivel2 == "municipal")
                municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
              else if (filtros()$nivel2 == "municipios_semelhantes")
                grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
            }
          }
        ) |>
        cria_indicadores(
          df_calcs = bloco2_calcs,
          filtros = filtros(),
          localidade_resumo = input$localidade_resumo
        )
    })

    ### Para a referência -----------------------------------------------------
    data2_resumo_referencia <- reactive({
      dplyr::left_join(bloco2, bloco6) |>
        dplyr::mutate(dplyr::across(
          dplyr::everything(),
          ~ replace(., is.na(.), 0)
        )) |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(
          df_calcs = bloco2_calcs,
          filtros = filtros(),
          referencia = TRUE
        )
    })

    ## Criando o output do gráfico de radar -----------------------------------
    ### Definindo os indicadores que aparecerão no gráfico
    selected_indicators <- c(
      "porc_menor20",
      "porc_mais_3pt",
      "geral_tx_abortos_mil_mulheres_valor_medio",
      "geral_tx_abortos_cem_nascidos_vivos_valor_medio",
      "prop_obitos_aborto"
    )

    ### Selecionando colunas relevantes nos dataframes de resumo e arrumando seus formatos
    df <- reactive({
      data2_resumo()[, c('class', selected_indicators)] |>
        dplyr::mutate(
          class = ifelse(
            grepl("Brasil \\(valor de referência\\)", class),
            "Brasil",
            class
          )
        ) |>
        tidyr::pivot_longer(
          !class,
          names_to = "indicador",
          values_to = "values1"
        ) |>
        dplyr::mutate(
          sufixo = c("", "%", "", "%", "%")
        )
    })

    df2 <- reactive({
      data2_resumo_referencia()[, selected_indicators] |>
        dplyr::mutate(class = "Referência") |>
        tidyr::pivot_longer(
          !class,
          names_to = "indicador",
          values_to = "values2"
        ) |>
        dplyr::mutate(
          tipo_de_referencia = lapply(
            selected_indicators,
            function(indicador_abrev)
              tabela_indicadores$descricao_referencia[
                tabela_indicadores$nome_abreviado == indicador_abrev
              ]
          ) |>
            unlist(),
          sufixo = c("", "%", "", "%", "%")
        )
    })

    ### Criando o output
    output$spider_chart <- highcharter::renderHighchart({
      # Categorias para o eixo x
      categories <- lapply(
        selected_indicators,
        function(indicador_abrev)
          gsub(
            "Porcentagem",
            "%",
            tabela_radar$indicador[
              tabela_radar$nome_abreviado == indicador_abrev
            ]
          )
      ) |>
        unlist()

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
        highcharter::hc_chart(
          polar = TRUE,
          type = "line",
          backgroundColor = "transparent"
        ) |>
        highcharter::hc_pane(size = '65%') |>
        highcharter::hc_xAxis(
          categories = categories,
          tickmarkPlacement = 'on',
          lineWidth = 0,
          labels = list(style = list(fontWeight = 'bold', fontSize = '12px'))
        ) |>
        highcharter::hc_yAxis(
          gridLineInterpolation = 'polygon',
          lineWidth = 0,
          min = 0,
          max = yAxis_max
        ) |>
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
          data = df2() |>
            dplyr::select(y = values2, tipo_de_referencia, sufixo),
          color = "#b73779",
          lineWidth = 2,
          opacity = 0.6,
          dashStyle = "ShortDash",
          marker = list(enabled = FALSE, symbol = "diamond", radius = 4),
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'>&#9670 </span> {series.name} ({point.tipo_de_referencia}): <b> {point.y}{point.sufixo} </b>"
          )
        ) |>
        highcharter::hc_legend(
          align = 'center',
          layout = 'horizontal',
          itemStyle = list(fontWeight = 'bold', fontSize = '14px')
        ) |>
        highcharter::hc_tooltip(shared = TRUE) |>
        highcharter::hc_legend(itemMarginTop = 25) # Ajustar a margem entre itens da legenda
    })

    ## Criando os outputs das caixinhas ---------------------------------------
    ### Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil) ----
    output$caixa_b2_i1 <- renderUI({
      cria_caixa_server(
        dados = data2_resumo(),
        indicador = "porc_menor20",
        titulo = "Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil)",
        tem_meta = TRUE,
        valor_de_referencia = 30,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "300px",
        #fonte_titulo = "15px",
        pagina = "bloco_2",
        tipo_referencia = "países desenvolvidos",
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

    ### Porcentagem de nascidos vivos de mulheres com mais de 3 partos anteriores ----
    output$caixa_b2_i2 <- renderUI({
      cria_caixa_server(
        dados = data2_resumo(),
        indicador = "porc_mais_3pt",
        titulo = "Porcentagem de nascidos vivos de mulheres com mais de 3 partos anteriores",
        tem_meta = FALSE,
        valor_de_referencia = data2_resumo_referencia()$porc_mais_3pt,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
        #fonte_titulo = "15px",
        pagina = "bloco_2",
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

    ### Valor médio da taxa de abortos inseguros por mil MIF ------------------
    output$caixa_b2_i3 <- renderUI({
      cria_caixa_server(
        dados = data2_resumo(),
        indicador = "geral_tx_abortos_mil_mulheres_valor_medio",
        titulo = "Valor médio da taxa de abortos inseguros por mil MIF",
        tem_meta = FALSE,
        valor_de_referencia = data2_resumo_referencia()$geral_tx_abortos_mil_mulheres_valor_medio,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "300px",
        #fonte_titulo = "15px",
        pagina = "bloco_2",
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

    ### Valor médio da razão de abortos inseguros por 100 nascidos vivos ------
    output$caixa_b2_i4 <- renderUI({
      cria_caixa_server(
        dados = data2_resumo(),
        indicador = "geral_tx_abortos_cem_nascidos_vivos_valor_medio",
        titulo = "Valor médio da razão de abortos inseguros por 100 nascidos vivos",
        tem_meta = FALSE,
        valor_de_referencia = data2_resumo_referencia()$geral_tx_abortos_cem_nascidos_vivos_valor_medio,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "300px",
        #fonte_titulo = "15px",
        pagina = "bloco_2",
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
    data2 <- reactive({
      dplyr::left_join(bloco2, bloco6) |>
        dplyr::mutate(dplyr::across(
          dplyr::everything(),
          ~ replace(., is.na(.), 0)
        )) |>
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
          else if (filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if (filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco2_calcs, filtros = filtros())
    })

    ### Para a comparação selecionada -----------------------------------------
    data2_comp <- reactive({
      dplyr::left_join(bloco2, bloco6) |>
        dplyr::mutate(dplyr::across(
          dplyr::everything(),
          ~ replace(., is.na(.), 0)
        )) |>
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
        cria_indicadores(
          df_calcs = bloco2_calcs,
          filtros = filtros(),
          comp = TRUE
        )
    })

    ### Para a referência -----------------------------------------------------
    data2_referencia <- reactive({
      dplyr::left_join(bloco2, bloco6) |>
        dplyr::mutate(dplyr::across(
          dplyr::everything(),
          ~ replace(., is.na(.), 0)
        )) |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(
          df_calcs = bloco2_calcs,
          filtros = filtros(),
          referencia = TRUE
        )
    })

    ## Criando os outputs dos gráficos ----------------------------------------
    ### Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil) -----------------
    output$plot1 <- highcharter::renderHighchart({
      faixa_selecionada <- input$faixa_etaria

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data2(),
            name = dplyr::if_else(
              filtros()$nivel == "nacional",
              ifelse(faixa_selecionada == "porc_menor20", "Brasil", "Brasil (valor de referência)"),
              unique(data2()$class)
            ),
            type = "line",
            highcharter::hcaes(x = ano, y = !!faixa_selecionada, group = class, colour = class)
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
        if (input$faixa_etaria == "porc_menor20") {
          grafico_base |>
            highcharter::hc_add_series(
              data = data2_referencia(),
              type = "line",
              name = "Referência (países desenvolvidos)",
              highcharter::hcaes(x = ano, y = !!faixa_selecionada, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        } else {
          if (filtros()$nivel == "nacional") {
            grafico_base
          } else {
            grafico_base |>
              highcharter::hc_add_series(
                data = data2_referencia(),
                type = "line",
                name = "Referência (média nacional)",
                highcharter::hcaes(x = ano, y = !!faixa_selecionada, group = class, colour = class),
                dashStyle = "ShortDot",
                opacity = 0.8
              )
          }
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data2(),
            name = dplyr::if_else(
              filtros()$nivel == "nacional",
              ifelse(
                faixa_selecionada == "porc_menor20",
                "Brasil",
                "Brasil (valor de referência)"
              ),
              unique(data2()$class)
            ),
            type = "line",
            highcharter::hcaes(x = ano, y = !!faixa_selecionada, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data2_comp(),
            name = dplyr::if_else(
              filtros()$nivel2 == "nacional",
              ifelse(
                faixa_selecionada == "porc_menor20",
                "Brasil",
                "Brasil (valor de referência)"
              ),
              unique(data2_comp()$class)
            ),
            type = "line",
            highcharter::hcaes(x = ano, y = !!faixa_selecionada, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE ) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          if (input$faixa_etaria == "porc_menor20") {
            grafico_base |>
              highcharter::hc_add_series(
                data = data2_referencia(),
                type = "line",
                name = "Referência (países desenvolvidos)",
                highcharter::hcaes(x = ano, y = !!faixa_selecionada, group = class, colour = class),
                dashStyle = "ShortDot",
                opacity = 0.6
              )
          } else {
            if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional")) {
              grafico_base
            } else {
              grafico_base |>
                highcharter::hc_add_series(
                  data = data2_referencia(),
                  type = "line",
                  name = "Referência (média nacional)",
                  highcharter::hcaes(x = ano, y = !!faixa_selecionada, group = class, colour = class),
                  dashStyle = "ShortDot",
                  opacity = 0.8
                )
            }
          }
        }
      }
    })

    ### Porcentagem de mulheres com mais de 3 partos anteriores -----------------
    output$plot2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data2(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mais_3pt, group = class, colour = class)
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
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data2_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mais_3pt, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data2(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mais_3pt, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data2_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mais_3pt, group = class, colour = class)
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
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data2_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mais_3pt, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    ### Taxa de abortos inseguros por mil mulheres em idade fértil -----------------
    data2_taxa_aborto <- reactive({
      data2() |>
        dplyr::rename(
          tx_abortos_mil_mulheres_valor_medio = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_valor_medio"
          ),
          tx_abortos_mil_mulheres_lim_inf = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_lim_inf"
          ),
          tx_abortos_mil_mulheres_lim_sup = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_lim_sup"
          )
        )
    })

    data2_taxa_aborto_comp <- reactive({
      data2_comp() |>
        dplyr::rename(
          tx_abortos_mil_mulheres_valor_medio = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_valor_medio"
          ),
          tx_abortos_mil_mulheres_lim_inf = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_lim_inf"
          ),
          tx_abortos_mil_mulheres_lim_sup = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_lim_sup"
          )
        )
    })

    data2_taxa_aborto_referencia <- reactive({
      data2_referencia() |>
        dplyr::rename(
          tx_abortos_mil_mulheres_valor_medio = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_valor_medio"
          ),
          tx_abortos_mil_mulheres_lim_inf = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_lim_inf"
          ),
          tx_abortos_mil_mulheres_lim_sup = glue::glue(
            "{input$tipo_taxa_aborto}_tx_abortos_mil_mulheres_lim_sup"
          )
        )
    })

    output$plot3 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2015,
          "Este indicador só está disponível a partir de 2014."
        )
      )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data2_taxa_aborto() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_mil_mulheres_lim_inf:,f} e limite superior de {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "Taxa"),
            min = 0,
            max = max(c(
              data2()$geral_tx_abortos_mil_mulheres_valor_medio,
              data2()$sus_tx_abortos_mil_mulheres_valor_medio,
              data2()$ans_tx_abortos_mil_mulheres_valor_medio,
              data2_referencia()$geral_tx_abortos_mil_mulheres_valor_medio,
              data2_referencia()$sus_tx_abortos_mil_mulheres_valor_medio,
              data2_referencia()$ans_tx_abortos_mil_mulheres_valor_medio
            ), na.rm = TRUE) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              name = "Referência (média nacional)",
              data = data2_taxa_aborto_referencia() |>
                dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_mil_mulheres_lim_inf:,f} e limite superior de {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
              ),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data2_taxa_aborto() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_mil_mulheres_lim_inf:,f} e limite superior de {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_add_series(
            data = data2_taxa_aborto_comp() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_mil_mulheres_lim_inf:,f} e limite superior de {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "Taxa"),
            min = 0,
            max = max(c(
                data2()$geral_tx_abortos_mil_mulheres_valor_medio,
                data2()$sus_tx_abortos_mil_mulheres_valor_medio,
                data2()$ans_tx_abortos_mil_mulheres_valor_medio,
                data2_comp()$geral_tx_abortos_mil_mulheres_valor_medio,
                data2_comp()$sus_tx_abortos_mil_mulheres_valor_medio,
                data2_comp()$ans_tx_abortos_mil_mulheres_valor_medio,
                data2_referencia()$geral_tx_abortos_mil_mulheres_valor_medio,
                data2_referencia()$sus_tx_abortos_mil_mulheres_valor_medio,
                data2_referencia()$ans_tx_abortos_mil_mulheres_valor_medio
              ), na.rm = TRUE) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              name = "Referência (média nacional)",
              data = data2_taxa_aborto_referencia() |>
                dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_mil_mulheres_lim_inf:,f} e limite superior de {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
              ),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      }
    })

    ### Razão de abortos inseguros por 100 nascidos vivos -----------------
    data2_razao_aborto <- reactive({
      data2() |>
        dplyr::rename(
          tx_abortos_cem_nascidos_vivos_valor_medio = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_valor_medio"
          ),
          tx_abortos_cem_nascidos_vivos_lim_inf = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_lim_inf"
          ),
          tx_abortos_cem_nascidos_vivos_lim_sup = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_lim_sup"
          )
        )
    })

    data2_razao_aborto_comp <- reactive({
      data2_comp() |>
        dplyr::rename(
          tx_abortos_cem_nascidos_vivos_valor_medio = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_valor_medio"
          ),
          tx_abortos_cem_nascidos_vivos_lim_inf = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_lim_inf"
          ),
          tx_abortos_cem_nascidos_vivos_lim_sup = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_lim_sup"
          )
        )
    })

    data2_razao_aborto_referencia <- reactive({
      data2_referencia() |>
        dplyr::rename(
          tx_abortos_cem_nascidos_vivos_valor_medio = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_valor_medio"
          ),
          tx_abortos_cem_nascidos_vivos_lim_inf = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_lim_inf"
          ),
          tx_abortos_cem_nascidos_vivos_lim_sup = glue::glue(
            "{input$tipo_razao_aborto}_tx_abortos_cem_nascidos_vivos_lim_sup"
          )
        )
    })

    output$plot4 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2015,
          "Este indicador só está disponível a partir de 2014."
        )
      )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data2_razao_aborto() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} e limite superior de {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "Taxa"),
            min = 0,
            max = max(c(
              c(data2()$geral_tx_abortos_cem_nascidos_vivos_valor_medio, data2()$sus_tx_abortos_cem_nascidos_vivos_valor_medio, data2()$ans_tx_abortos_cem_nascidos_vivos_valor_medio),
              c(data2_referencia()$geral_tx_abortos_cem_nascidos_vivos_valor_medio, data2_referencia()$sus_tx_abortos_cem_nascidos_vivos_valor_medio, data2_referencia()$ans_tx_abortos_cem_nascidos_vivos_valor_medio)
            ), na.rm = TRUE) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              name = "Referência (média nacional)",
              data = data2_razao_aborto_referencia() |> dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} e limite superior de {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
              ),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data2_razao_aborto() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} e limite superior de {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_add_series(
            data = data2_razao_aborto_comp() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} e limite superior de {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "Taxa"),
            min = 0,
            max = max(c(
              c(data2()$geral_tx_abortos_cem_nascidos_vivos_valor_medio, data2()$sus_tx_abortos_cem_nascidos_vivos_valor_medio, data2()$ans_tx_abortos_cem_nascidos_vivos_valor_medio),
              c(data2_comp()$geral_tx_abortos_cem_nascidos_vivos_valor_medio, data2_comp()$sus_tx_abortos_cem_nascidos_vivos_valor_medio, data2_comp()$ans_tx_abortos_cem_nascidos_vivos_valor_medio),
              c(data2_referencia()$geral_tx_abortos_cem_nascidos_vivos_valor_medio, data2_referencia()$sus_tx_abortos_cem_nascidos_vivos_valor_medio, data2_referencia()$ans_tx_abortos_cem_nascidos_vivos_valor_medio)
            ), na.rm = TRUE) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              name = "Referência (média nacional)",
              data = data2_razao_aborto_referencia() |> dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> média de {point.y} (limite inferior de {point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} e limite superior de {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
              ),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }

      }
    })

  })
}

## To be copied in the UI
# mod_bloco_2_ui("bloco_2_1")

## To be copied in the server
# mod_bloco_2_server("bloco_2_1")
