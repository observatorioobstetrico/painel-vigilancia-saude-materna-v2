#' bloco_5 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_5_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Condições de nascimento: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    conditionalPanel(
      ns = ns,
      condition = "output.comparar == 'Sim'",
      column(
        width = 12,
        HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
        HTML(
          "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para visualizar os valores referentes à localidade de comparação selecionada nos gráficos de distribuição percentual,
                passe o cursor do mouse sobre a barra que contém a categoria de interesse.
                </b> </div>"
        ),
        hr(),
        HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
      )
    ),
    fluidRow(
      column(
        width = 4,
        HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
        HTML("<b style='font-size:19px'> Resumo do período </b>"),
        hr(),
        fluidRow(
          column(
            width = 12,
            HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
            uiOutput(ns("input_localidade_resumo")),
            align = "center"
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i1")), proxy.height = "300px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i2")), proxy.height = "300px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i3")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i4")), proxy.height = "325px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i5")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i7")), proxy.height = "325px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i10")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i11")), proxy.height = "325px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i12")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i6")), proxy.height = "325px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i9")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i8")), proxy.height = "325px")
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
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 10%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Porcentagem de baixo peso ao nascer &nbsp;</b>"
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
                    inputId = ns("baixo_peso"),
                    label = "Peso ao nascer",
                    options = list(placeholder = "Selecione a faixa de peso ao nascer"),
                    choices = c(
                      "Menor que 1500 g" = "porc_nasc_peso_menor_1500",
                      "De 1500 a 1999 g" = "porc_nasc_peso_1500_a_1999",
                      "De 2000 a 2499 g" = "porc_nasc_peso_2000_a_2499",
                      "Menor que 2500 g" = "porc_nasc_baixo_peso"
                    ),
                    width = "100%", selected = "porc_nasc_baixo_peso"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1"), height = 400))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 10%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Distribuição percentual do baixo peso ao nascer (< 2500g) &nbsp;</b>"
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_1"), height = 490))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 10%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Porcentagem de nascimentos prematuros &nbsp;</b>"
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
                    inputId = ns("faixa_prematuridade"),
                    label = "Idade gestacional",
                    options = list(placeholder = "Selecione a idade gestacional"),
                    choices = c(
                      "Menor que 28 semanas" = "porc_nasc_menos_de_28_semanas",
                      "De 28 a 32 semanas" = "porc_nasc_28_a_32_semanas",
                      "De 33 a 34 semanas" = "porc_nasc_33_a_34_semanas",
                      "De 35 a 36 semanas" = "porc_nasc_35_a_36_semanas",
                      "Menos que 37 semanas" = "porc_nasc_premat"


                    ),
                    width = "100%", selected = "porc_nasc_premat"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2"), height = 400))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 10%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Distribuição percentual da prematuridade &nbsp;</b>"
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_1"), height = 490))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Porcentagem de nascimentos termo precoce &nbsp;</b>"
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3"), height = 450))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao7"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao7"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5"), height = 450))
            )
          ),
          # column(
          #   width = 6,
          #   bs4Dash::bs4Card(
          #     width = 12,
          #     status = "primary",
          #     collapsible = FALSE,
          #     headerBorder = FALSE,
          #     style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          #     div(
          #       style = "height: 15%; display: flex; align-items: center;",
          #       HTML("<b style='font-size:18px'> Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos com vínculo com o SUS &nbsp;</b>"),
          #       shinyjs::hidden(
          #         span(
          #           id = ns("mostrar_botao11"),
          #           shinyWidgets::actionBttn(
          #             inputId = ns("botao11"),
          #             icon = icon("triangle-exclamation", style = "color: red"),
          #             color = "warning",
          #             style = "material-circle",
          #             size = "xs"
          #           )
          #         )
          #       )
          #     ),
          #     hr(),
          #     fluidRow(
          #       column(
          #         width = 6,
          #         selectizeInput(
          #           inputId = ns("local_internacao_sus"),
          #           label = "Local da internação",
          #           options = list(placeholder = "Selecione o local de internação"),
          #           choices = c(
          #             "Todos" = "geral",
          #             "Dentro da macrorregião de saúde" = "na_macro",
          #             "Fora da macrorregião de saúde" = "fora_macro"
          #           ),
          #           width = "100%"
          #         )
          #       ),
          #       column(
          #         width = 6,
          #         strong(p("Idade, em dias, do bebê", style = "margin-bottom: 0.5rem")),
          #         tags$div(
          #           align = 'left',
          #           class = 'multicol',
          #           checkboxGroupInput(
          #             inputId = ns("idade_dias_sus"),
          #             label = NULL,
          #             choices = c(
          #               "0 a 6 dias" = "menores_7_dias",
          #               "7 a 27 dias" = "7_a_27_dias"
          #             ),
          #             selected = c("menores_7_dias", "7_a_27_dias")
          #           )
          #         )
          #       )
          #     ),
          #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot11"), height = 350))
          #   )
          # ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                # HTML("<b style='font-size:18px'> Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos públicos &nbsp;</b>"),
                HTML("<b style='font-size:18px'> Porcentagem de internações até o 27º dia de vida de bebês nascidos em hospitais com vínculo com o SUS &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao10"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao10"),
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
                  width = 6,
                  selectizeInput(
                    inputId = ns("local_internacao_sih"),
                    label = "Local da internação",
                    options = list(placeholder = "Selecione o local de internação"),
                    choices = c(
                      "Todos" = "geral",
                      "Dentro da macrorregião de saúde" = "na_macro",
                      "Fora da macrorregião de saúde" = "fora_macro"
                    ),
                    width = "100%"
                  )
                ),
                column(
                  width = 6,
                  strong(p("Idade, em dias, do bebê", style = "margin-bottom: 0.5rem")),
                  tags$div(
                    align = 'left',
                    class = 'multicol',
                    checkboxGroupInput(
                      inputId = ns("idade_dias_sih"),
                      label = NULL,
                      choices = c(
                        "0 a 6 dias" = "menores_7_dias",
                        "7 a 27 dias" = "7_a_27_dias"
                      ),
                      selected = c("menores_7_dias", "7_a_27_dias")
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot10"), height = 350))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao12"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao12"),
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
                  width = 6,
                  selectizeInput(
                    inputId = ns("local_internacao_uti_sih"),
                    label = "Local da internação",
                    options = list(placeholder = "Selecione o local de internação"),
                    choices = c(
                      "Todos" = "geral",
                      "Dentro da macrorregião de saúde" = "na_macro",
                      "Fora da macrorregião de saúde" = "fora_macro"
                    ),
                    width = "100%"
                  )
                ),
                column(
                  width = 6,
                  strong(p("Idade, em dias, do bebê", style = "margin-bottom: 0.5rem")),
                  tags$div(
                    align = 'left',
                    class = 'multicol',
                    checkboxGroupInput(
                      inputId = ns("idade_dias_uti_sih"),
                      label = NULL,
                      choices = c(
                        "0 a 6 dias" = "menores_7_dias",
                        "7 a 27 dias" = "7_a_27_dias"
                      ),
                      selected = c("menores_7_dias", "7_a_27_dias")
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot12"), height = 360))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Porcentagem de nascidos vivos com asfixia dentre os nascidos vivos sem anomalias e com peso > 2500 g &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao6"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao6"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4"), height = 450))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center",
                HTML("<b style='font-size:18px'> Porcentagem de nascidos vivos com malformações &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao8"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao8"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6"), height = 450))
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            collapsible = FALSE,
            headerBorder = FALSE,
            style = "height: 630px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
            div(
              style = "height: 20%; display: flex; align-items: center",
              HTML("<b style='font-size:18px'> Porcentagem de nascidos vivos com malformações prioritárias para vigilância definidas pelo Ministério da Saúde (<a href = http://dx.doi.org/10.1590/s1679-49742021000100030 , target = _blank>http://dx.doi.org/10.1590/s1679-49742021000100030</a>). &nbsp;</b>"),
              shinyjs::hidden(
                span(
                  id = ns("mostrar_botao9"),
                  shinyWidgets::actionBttn(
                    inputId = ns("botao9"),
                    icon = icon("triangle-exclamation", style = "color: red"),
                    color = "warning",
                    style = "material-circle",
                    size = "xs"
                  )
                )
              )
            ),
            hr(),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot7"), height = 450))
          )
        ),
        column(
          width = 8,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            collapsible = FALSE,
            headerBorder = FALSE,
            style = "height: 630px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
            div(
              style = "height: 15%; display: flex; align-items: center;",
              HTML("<b style='font-size:18px'> Tabela dos grupos de malformações prioritárias para vigilância definidos pelo Ministério da Saúde (<a href = http://dx.doi.org/10.1590/s1679-49742021000100030 , target = _blank>http://dx.doi.org/10.1590/s1679-49742021000100030</a>). &nbsp;</b>")
            ),
            hr(),
            shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_malformacoes")))
          )
        )
      )
    )
  )
}

#' bloco_5 Server Functions
#'
#' @noRd
mod_bloco_5_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco5_calcs <- reactive({
      df_calcs_aux1 <- data.frame(
        tipo = c("local", "referencia"),
        porc_nasc_baixo_peso = rep("round(sum(nascidos_vivos_com_baixo_peso) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_peso_menor_1500 = rep("round(sum(nascidos_vivos_peso_menor_1500) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_peso_1500_a_1999 = rep("round(sum(nascidos_vivos_peso_1500_a_1999) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_peso_2000_a_2499= rep("round(sum(nascidos_vivos_peso_2000_a_2499) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_baixo_peso_menor_1500 = rep("round(sum(nascidos_vivos_peso_menor_1500) / sum(nascidos_vivos_com_baixo_peso) * 100, 1)", 2),
        porc_baixo_peso_1500_a_1999 = rep("round(sum(nascidos_vivos_peso_1500_a_1999) / sum(nascidos_vivos_com_baixo_peso) * 100, 1)", 2),
        porc_baixo_peso_2000_a_2499 = rep("round(sum(nascidos_vivos_peso_2000_a_2499) / sum(nascidos_vivos_com_baixo_peso) * 100, 1)", 2),
        porc_nasc_premat = c("round(sum(nascidos_vivos_prematuros) / sum(total_de_nascidos_vivos) * 100, 1)", "10"),
        porc_nasc_menos_de_28_semanas = rep("round(sum(nascidos_vivos_menos_de_28_semanas) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_28_a_32_semanas = rep("round(sum(nascidos_vivos_28_a_32_semanas) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_33_a_34_semanas = rep("round(sum(nascidos_vivos_33_a_34_semanas) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_35_a_36_semanas = rep("round(sum(nascidos_vivos_35_a_36_semanas) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_premat_menos_de_28_semanas = rep("round(sum(nascidos_vivos_menos_de_28_semanas) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_premat_28_a_32_semanas = rep("round(sum(nascidos_vivos_28_a_32_semanas) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_premat_33_a_34_semanas = rep("round(sum(nascidos_vivos_33_a_34_semanas) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_premat_35_a_36_semanas = rep("round(sum(nascidos_vivos_35_a_36_semanas) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_premat_faltantes = rep("round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                           nascidos_vivos_28_a_32_semanas,
                                                                                           nascidos_vivos_33_a_34_semanas,
                                                                                           nascidos_vivos_35_a_36_semanas)))) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_termo_precoce = c("round(sum(nascidos_vivos_termo_precoce) / sum(total_de_nascidos_vivos) * 100, 1)", "20"),
        porc_condicoes_ameacadoras = rep("round(sum(nascidos_condicoes_ameacadoras) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nascidos_vivos_asfixia1 = rep("round(sum(nascidos_vivos_asfixia1) / sum(total_nascidos) * 100, 1)", 2),
        porc_malformacao_geral = rep("round(sum(total_de_nascidos_malformacao) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_malformacao_vigilancia = rep("round(sum(nascidos_vivos_anomalia) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_internacoes_menores_28_dias_vinc_sus_geral = rep("round(sum(internacoes_geral_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_sus[ano <= 2022]) * 100, 1)", 2),
        porc_internacoes_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)", 2),
        porc_internacoes_uti_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral_internado_uti[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)", 2)
      )

      if (is.null(input$idade_dias_sus[1])) {
        df_calcs_aux2 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_menores_28_dias_vinc_sus = "NA"
        )
      } else {
        df_calcs_aux2 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_menores_28_dias_vinc_sus = rep(glue::glue(
            "ifelse(
              length(input$idade_dias_sus) == 2,
              round(sum(internacoes_{input$local_internacao_sus}_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_sus[ano <= 2022]) * 100, 1),
              round(sum(internacoes_{input$local_internacao_sus}_{input$idade_dias_sus[1]}[ano <= 2022]) / sum(nascidos_estabelecimentos_sus[ano <= 2022]) * 100, 1)
            )"
          ), 2)
        )
      }

      if (is.null(input$idade_dias_sih[1])) {
        df_calcs_aux3 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_menores_28_dias_sih = "NA"
        )
      } else {
        df_calcs_aux3 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_menores_28_dias_sih = rep(glue::glue(
            "ifelse(
              length(input$idade_dias_sih) == 2,
              round(sum(internacoes_{input$local_internacao_sih}_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1),
              round(sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_sih[1]}[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)
            )"
          ), 2)
        )
      }

      if (is.null(input$idade_dias_uti_sih[1])) {
        df_calcs_aux4 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_uti_menores_28_dias_sih = "NA"
        )
      } else {
        df_calcs_aux4 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_uti_menores_28_dias_sih = rep(glue::glue(
            "ifelse(
              length(input$idade_dias_sih) == 2,
              round(sum(internacoes_{input$local_internacao_sih}_geral_internado_uti[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1),
              round(sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_uti_sih[1]}_internado_uti[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)
            )"
          ), 2)
        )
      }

      dplyr::full_join(
        df_calcs_aux1,
        dplyr::full_join(df_calcs_aux2, dplyr::full_join(df_calcs_aux3, df_calcs_aux4))
      )
    })

    # Criando alguns outputs para a UI ----------------------------------------
    ## Criando o output que diz se há ou não comparação -----------------------
    output$comparar <- renderText({filtros()$comparar})
    outputOptions(output, "comparar", suspendWhenHidden = FALSE)

    ## Criando o output que recebe a localidade e o ano escolhidos ------------
    output$titulo_localidade <- renderUI({
      if (length(filtros()$ano2[1]:filtros()$ano2[2]) > 1) {
        ano <- glue::glue("{filtros()$ano2[1]} a {filtros()$ano2[2]}")
      } else {
        ano <- filtros()$ano2[1]
      }

      if (filtros()$comparar == "Não") {
        local1 <- dplyr::case_when(
          filtros()$nivel == "Nacional" ~ "Brasil",
          filtros()$nivel == "Regional" ~ filtros()$regiao,
          filtros()$nivel == "Estadual" ~ filtros()$estado,
          filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
          filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
          filtros()$nivel == "Municipal" ~ filtros()$municipio
        )
        texto <- glue::glue("({local1}, {ano})")
      } else {
        local1 <- dplyr::case_when(
          filtros()$nivel == "Nacional" ~ "Brasil",
          filtros()$nivel == "Regional" ~ filtros()$regiao,
          filtros()$nivel == "Estadual" ~ filtros()$estado,
          filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
          filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
          filtros()$nivel == "Municipal" ~ filtros()$municipio
        )
        local2 <- dplyr::case_when(
          filtros()$nivel2 == "Nacional" ~ "Brasil",
          filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
          filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
          filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
          filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
          filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
          filtros()$nivel2 == "Municípios semelhantes" ~ "municípios semelhantes"
        )
        texto <- glue::glue("({local1} e {local2}, {ano})")
      }

      tags$b(texto, style = "font-size: 33px")
    })

    ## Criando o output que receberá os nomes dos locais selecionados quando há comparação --------
    output$input_localidade_resumo <- renderUI({
      localidade_original <- dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )

      class <- dplyr::case_when(
        filtros()$nivel2 == "Nacional" ~ "Brasil",
        filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
        filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
        filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
        filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
        filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
        filtros()$nivel2 == "Municípios semelhantes" ~ "Média dos municípios semelhantes"
      )

      if (filtros()$comparar == "Sim") {
        radioButtons(
          inputId = ns("localidade_resumo"),
          label = NULL,
          choiceNames = list(
            localidade_original,
            class
          ),
          choiceValues = list("escolha1", "escolha2"),
          selected = "escolha1",
          inline = TRUE
        )
      }
    })

    ## Para os botões de alerta quanto à incompletude e cobertura --------------
    ### Calculando os indicadores de incompletude ------------------------------
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel == "Regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "Estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "Macrorregião de saúde")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "Microrregião de saúde")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "Municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          peso = round(sum(peso_incompletos, na.rm = TRUE) / sum(peso_totais, na.rm = TRUE) * 100, 2),
          gestacao = round(sum(gestacao_incompletos, na.rm = TRUE) / sum(gestacao_totais, na.rm = TRUE) * 100, 2),
          semagestac = round(sum(semagestac_incompletos, na.rm = TRUE) / sum(semagestac_totais, na.rm = TRUE) * 100, 2),
          idanomal = round(sum(idanomal_incompletos, na.rm = TRUE) / sum(idanomal_totais,na.rm = TRUE) *100, 2),
          localidade = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })

    ### Calculando os indicadores de cobertura --------------------------------
    data_cobertura <- reactive({
      if (filtros()$nivel == "Municipal") {
        sub_registro_sinasc_muni_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "Estadual") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$estado
          )
      } else if (filtros()$nivel == "Regional") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$regiao
          )
      } else if (filtros()$nivel == "Nacional") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == "Brasil"
          )
      } else {
        data.frame(
          ano = filtros()$ano2[1]:filtros()$ano2[2],
          localidade = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          ),
          cobertura = 100
        )
      }
    })

    ### Juntando os dados de incompletude e cobertura -------------------------
    data_incompletude <- reactive({dplyr::full_join(data_incompletude_aux(), data_cobertura(), by = c("ano", "localidade"))})

    ### Ativando os botões de alerta quando necessário ------------------------
    #### Porcentagem de baixo peso ao nascer e sua distribuição percentual ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$peso > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(c(input$botao1, input$botao2), {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$peso,
        variavel_incompletude1 = "PESO",
        descricao_incompletude1 = "em branco ou preenchida com 9999",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de nascimentos prematuros --------------------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$gestacao > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$gestacao,
        variavel_incompletude1 = "GESTACAO",
        descricao_incompletude1 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })


    #### Distribuição percentual da prematuridade -----------------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao4", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$gestacao > 5, na.rm = TRUE) | any(data_incompletude()$semagestac > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao4", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao4, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$gestacao,
        variavel_incompletude1 = "GESTACAO",
        descricao_incompletude1 = "em branco ou ignorados",
        incompletude2 = data_incompletude()$semagestac,
        variavel_incompletude2 = "SEMAGESTAC",
        descricao_incompletude2 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de nascimentos termo precoce -----------------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao5", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$semagestac > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao5", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao5, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$semagestac,
        variavel_incompletude1 = "SEMAGESTAC",
        descricao_incompletude1 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de nascidos vivos com asfixia dentre os nascidos vivos sem anomalias e com peso > 2500 g ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao6", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$peso > 5, na.rm = TRUE) |
            any(data_incompletude()$idanomal > 5, na.rm = TRUE) |
            any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao6", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao6, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$peso,
        variavel_incompletude1 = "PESO",
        descricao_incompletude1 = "em branco ou preenchida com 9999",
        incompletude2 = data_incompletude()$idanomal,
        variavel_incompletude2 = "IDANOMAL",
        descricao_incompletude2 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao7", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$gestacao > 5, na.rm = TRUE) |
            any(data_incompletude()$peso > 5, na.rm = TRUE) |
            any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao7", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao7, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$peso,
        variavel_incompletude1 = "PESO",
        descricao_incompletude1 = "em branco ou ignorados",
        incompletude2 = data_incompletude()$gestacao,
        variavel_incompletude2 = "GESTACAO",
        descricao_incompletude2 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de nascidos vivos com malformações -----------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao8", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao9", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$idanomal > 5, na.rm = TRUE) |
            any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao8", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao9", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(c(input$botao8,input$botao9), {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$idanomal,
        variavel_incompletude1 = "IDANOMAL",
        descricao_incompletude1 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })


    # Para o resumo do período ------------------------------------------------
    ## Calculando uma média dos indicadores para o período selecionado --------
    ### Para a localidade selecionada -----------------------------------------
    data5_resumo_aux <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$comparar == "Não") {
            if (filtros()$nivel == "Nacional")
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            else if (filtros()$nivel == "Regional")
              regiao == filtros()$regiao
            else if (filtros()$nivel == "Estadual")
              uf == filtros()$estado
            else if (filtros()$nivel == "Macrorregião de saúde")
              macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
            else if(filtros()$nivel == "Microrregião de saúde")
              r_saude == filtros()$micro & uf == filtros()$estado_micro
            else if(filtros()$nivel == "Municipal")
              municipio == filtros()$municipio & uf == filtros()$estado_municipio
          } else {
            req(input$localidade_resumo)
            if (input$localidade_resumo == "escolha1") {
              if (filtros()$nivel == "Nacional")
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              else if (filtros()$nivel == "Regional")
                regiao == filtros()$regiao
              else if (filtros()$nivel == "Estadual")
                uf == filtros()$estado
              else if (filtros()$nivel == "Macrorregião de saúde")
                macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
              else if(filtros()$nivel == "Microrregião de saúde")
                r_saude == filtros()$micro & uf == filtros()$estado_micro
              else if(filtros()$nivel == "Municipal")
                municipio == filtros()$municipio & uf == filtros()$estado_municipio
            } else {
              if (filtros()$nivel2 == "Nacional")
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              else if (filtros()$nivel2 == "Regional")
                regiao == filtros()$regiao2
              else if (filtros()$nivel2 == "Estadual")
                uf == filtros()$estado2
              else if (filtros()$nivel2 == "Macrorregião de saúde")
                macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
              else if(filtros()$nivel2 == "Microrregião de saúde")
                r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
              else if(filtros()$nivel2 == "Municipal")
                municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
              else if (filtros()$nivel2 == "Municípios semelhantes")
                grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
            }
          }
        ) |>
        cria_indicadores(df_calcs = bloco5_calcs(), input = input, filtros = filtros())
    })

    # Não queremos que as caixinhas se atualizem quando os inputs dos gráficos mudarem
    data5_resumo <- eventReactive(c(filtros()$pesquisar, input$localidade_resumo), data5_resumo_aux(), ignoreNULL = FALSE)

    ### Para a referência -----------------------------------------------------
    data5_resumo_referencia_aux <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco5_calcs(), input = input, filtros = filtros(), referencia = TRUE)
    })

    # Não queremos que as caixinhas se atualizem quando os inputs dos gráficos mudarem
    data5_resumo_referencia <- eventReactive(c(filtros()$pesquisar, input$localidade_resumo), data5_resumo_referencia_aux(), ignoreNULL = FALSE)

    ### Para a referência do baixo peso  --------------------------------------
    data5_resumo_referencia_baixo_peso <- reactive({
      base_referencia_baixo_peso |>
        dplyr::filter(
          if (filtros()$comparar == "Não") {
            if (filtros()$nivel == "Nacional")
              regiao %in% unique(tabela_aux_municipios$regiao)
            else if (filtros()$nivel == "Regional")
              regiao == filtros()$regiao
            else if (filtros()$nivel == "Estadual")
              uf == filtros()$estado
            else if (filtros()$nivel == "Macrorregião de saúde")
              macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
            else if(filtros()$nivel == "Microrregião de saúde")
              r_saude == filtros()$micro & uf == filtros()$estado_micro
            else if(filtros()$nivel == "Municipal")
              municipio == filtros()$municipio & uf == filtros()$estado_municipio
          } else {
            req(input$localidade_resumo)
            if (input$localidade_resumo == "escolha1") {
              if (filtros()$nivel == "Nacional")
                regiao %in% unique(tabela_aux_municipios$regiao)
              else if (filtros()$nivel == "Regional")
                regiao == filtros()$regiao
              else if (filtros()$nivel == "Estadual")
                uf == filtros()$estado
              else if (filtros()$nivel == "Macrorregião de saúde")
                macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
              else if(filtros()$nivel == "Microrregião de saúde")
                r_saude == filtros()$micro & uf == filtros()$estado_micro
              else if(filtros()$nivel == "Municipal")
                municipio == filtros()$municipio & uf == filtros()$estado_municipio
            } else {
              if (filtros()$nivel2 == "Nacional")
                regiao %in% unique(tabela_aux_municipios$regiao)
              else if (filtros()$nivel2 == "Regional")
                regiao == filtros()$regiao2
              else if (filtros()$nivel2 == "Estadual")
                uf == filtros()$estado2
              else if (filtros()$nivel2 == "Macrorregião de saúde")
                macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
              else if(filtros()$nivel2 == "Microrregião de saúde")
                r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
              else if(filtros()$nivel2 == "Municipal")
                municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
              else if (filtros()$nivel2 == "Municípios semelhantes")
                grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
            }
          }
        ) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(nascidos, na.rm = TRUE),
          porc_nasc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1)
        )

    })


    ## Criando os outputs das caixinhas ---------------------------------------
    ### Porcentagem de baixo peso ao nascer -----------------------------------
    output$b5_i1 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_nasc_baixo_peso",
        titulo = "Porcentagem de baixo peso ao nascer (< 2500 g)",
        tem_meta = TRUE,
        valor_de_referencia = data5_resumo_referencia_baixo_peso()$porc_nasc_baixo_peso,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        pagina = "bloco_5",
        tipo_referencia = "meta de redução global",
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

    ### Distribuição percentual do baixo peso ao nascer -----------------------
    output$b5_i2 <- renderUI({
      cria_caixa_conjunta_bloco5(
        dados = data5_resumo(),
        indicador = "baixo peso",
        titulo = "Dentre os nascidos vivos com baixo peso (< 2500 g),",
        fonte_titulo = "15px",
        tamanho_caixa = "320px"
      )
    })

    ### Porcentagem de nascimentos prematuros ---------------------------------
    output$b5_i3 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_nasc_premat",
        titulo = "Porcentagem de nascimentos prematuros",
        tem_meta = TRUE,
        valor_de_referencia = 10,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        pagina = "bloco_5",
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

    ### Distribuição percentual da prematuridade ------------------------------
    output$b5_i4 <- renderUI({
      cria_caixa_conjunta_bloco5(
        dados = data5_resumo(),
        indicador = "prematuridade",
        titulo = "Dentre os nascimentos prematuros,",
        tamanho_caixa = "320px"
      )
    })

    ### Porcentagem de nascimentos termo precoce ------------------------------
    output$b5_i5 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_termo_precoce",
        titulo = "Porcentagem de nascimentos termo precoce",
        tem_meta = TRUE,
        valor_de_referencia = 20,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        pagina = "bloco_5",
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

    ### Porcentagem de nascidos vivos com asfixia dentre os nascidos vivos sem anomalias e com peso > 2500 g ----
    output$b5_i6 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_nascidos_vivos_asfixia1",
        titulo = "Porcentagem de nascidos vivos com asfixia dentre os nascidos vivos sem anomalias e com peso > 2500 g",
        tem_meta = FALSE,
        valor_de_referencia = data5_resumo_referencia()$porc_nascidos_vivos_asfixia1,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
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

    ### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    output$b5_i7 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_condicoes_ameacadoras",
        titulo = "Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida",
        tem_meta = TRUE,
        valor_de_referencia = data5_resumo_referencia()$porc_condicoes_ameacadoras,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
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

    ### Porcentagem de nascidos vivos com malformações prioritárias para vigilância definidas pelo MS ---
    output$b5_i8 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_malformacao_vigilancia",
        titulo = "Porcentagem de nascidos vivos com malformações prioritárias para vigilância definidas pelo Ministério da Saúde",
        tem_meta = TRUE,
        valor_de_referencia = data5_resumo_referencia()$porc_malformacao_vigilancia,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
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

    ### Porcentagem de nascidos vivos com malformações ------------------------
    output$b5_i9 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_malformacao_geral",
        titulo = "Porcentagem de nascidos vivos com malformações",
        tem_meta = TRUE,
        valor_de_referencia = data5_resumo_referencia()$porc_malformacao_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
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

    ### Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos públicos -----------
    output$b5_i11 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_internacoes_menores_28_dias_sih_geral",
        titulo = "Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos públicos (geral)",
        tem_meta = TRUE,
        valor_de_referencia = data5_resumo_referencia()$porc_internacoes_menores_28_dias_sih_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
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

    ### Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos com vínculo com o SUS ----
    output$b5_i10 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_internacoes_menores_28_dias_vinc_sus_geral",
        titulo = "Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos com vínculo com o SUS (geral)",
        tem_meta = TRUE,
        valor_de_referencia = data5_resumo_referencia()$porc_internacoes_menores_28_dias_vinc_sus_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
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

    ### Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS -----------
    output$b5_i12 <- renderUI({
      cria_caixa_server(
        dados = data5_resumo(),
        indicador = "porc_internacoes_uti_menores_28_dias_sih_geral",
        titulo = "Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS (geral)",
        tem_meta = TRUE,
        valor_de_referencia = data5_resumo_referencia()$porc_internacoes_uti_menores_28_dias_sih_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
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
    cols <- c("#2c115f", "#b73779", "#fc8961")

    ## Organizando a base de malformações -------------------------------------
    ## Calculando os indicadores para cada ano do período selecionado ---------
    ### Para a tabela de malformações -----------------------------------------
    data5_nascidos_vivos <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= min(filtros()$ano2[2], 2023)) |>
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel == "Regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "Estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "Macrorregião de saúde")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "Microrregião de saúde")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "Municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarize(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos)
        ) |>
        dplyr::ungroup()
    })

    data5_malformacao <- reactive({
      malformacao |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= min(filtros()$ano2[2], 2023)) |>
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano >= filtros()$ano2[1] & ano <= min(filtros()$ano2[2], 2023)
          else if (filtros()$nivel == "Regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "Estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "Macrorregião de saúde")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "Microrregião de saúde")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "Municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(grupo_de_anomalias_congenitas, anomalia, descricao, ano) |>
        dplyr::summarize(
          frequencia = sum(nascidos_vivos_anomalia)
        ) |>
        dplyr::ungroup() |>
        dplyr::right_join(data5_nascidos_vivos()) |>
        dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
        dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    ### Para a localidade selecionada -----------------------------------------
    data5_aux <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel == "Regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "Estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "Macrorregião de saúde")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "Microrregião de saúde")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "Municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco5_calcs(), input = input, filtros = filtros())
    })

    # Não queremos que todos os gráficos se atualizem quando os inputs dos gráficos de internações mudarem
    data5 <- eventReactive(c(filtros()$pesquisar), data5_aux(), ignoreNULL = FALSE)
    data5_internacoes_vinc_sus <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sus, input$idade_dias_sus), data5_aux(), ignoreNULL = FALSE)
    data5_internacoes_publicos_sih <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sih, input$idade_dias_sih), data5_aux(), ignoreNULL = FALSE)
    data5_internacoes_uti_sih <- eventReactive(c(filtros()$pesquisar, input$local_internacao_uti_sih, input$idade_dias_uti_sih), data5_aux(), ignoreNULL = FALSE)

    # Para o gráfico de porcentagem de nascidos vivos com baixo peso, selecionando apenas a coluna de escolha do usuário
    data5_baixo_peso <- reactive(
      data5() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$baixo_peso),
          class
        )
    )

    # Para o gráfico de porcentagem de nascidos vivos prematuros, selecionando apenas a coluna de escolha do usuário
    data5_prematuridade <- reactive(
      data5() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_prematuridade),
          class
        )
    )

    ### Para a comparação selecionada -----------------------------------------
    data5_comp_aux <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "Nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel2 == "Regional")
            regiao == filtros()$regiao2
          else if (filtros()$nivel2 == "Estadual")
            uf == filtros()$estado2
          else if (filtros()$nivel2 == "Macrorregião de saúde")
            macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
          else if(filtros()$nivel2 == "Microrregião de saúde")
            r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
          else if(filtros()$nivel2 == "Municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "Municípios semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco5_calcs(), input = input, filtros = filtros(), comp = TRUE)
    })

    # Não queremos que todos os gráficos se atualizem quando os inputs dos gráficos de internações mudarem
    data5_comp <- eventReactive(c(filtros()$pesquisar), data5_comp_aux(), ignoreNULL = FALSE)
    data5_internacoes_vinc_sus_comp <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sus, input$idade_dias_sus), data5_comp_aux(), ignoreNULL = FALSE)
    data5_internacoes_publicos_sih_comp <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sih, input$idade_dias_sih), data5_comp_aux(), ignoreNULL = FALSE)
    data5_internacoes_uti_sih_comp <- eventReactive(c(filtros()$pesquisar, input$local_internacao_uti_sih, input$idade_dias_uti_sih), data5_comp_aux(), ignoreNULL = FALSE)

    # Para o gráfico de porcentagem de nascidos vivos com baixo peso, selecionando apenas a coluna de escolha do usuário
    data5_comp_baixo_peso <- reactive(
      data5_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$baixo_peso),
          class
        )
    )

    # Para o gráfico de porcentagem de nascidos vivos prematuros, selecionando apenas a coluna de escolha do usuário
    data5_comp_prematuridade <- reactive(
      data5_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_prematuridade),
          class
        )
    )

    ### Para a referência -----------------------------------------------------
    data5_referencia_aux <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco5_calcs(), filtros = filtros(), input = input, referencia = TRUE, adicionar_localidade = FALSE) |>
        dplyr::mutate(
          localidade_comparacao = "Média nacional"
        ) |>
        dplyr::rename_with(~paste0("br_", .x), dplyr::starts_with("porc_baixo_peso_") | dplyr::starts_with("porc_premat_"))
    })

    # Não queremos que todos os gráficos se atualizem quando os inputs dos gráficos de internações mudarem
    data5_referencia <- eventReactive(c(filtros()$pesquisar), data5_referencia_aux(), ignoreNULL = FALSE)
    data5_internacoes_vinc_sus_referencia <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sus, input$idade_dias_sus), data5_referencia_aux(), ignoreNULL = FALSE)
    data5_internacoes_publicos_sih_referencia <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sih, input$idade_dias_sih), data5_referencia_aux(), ignoreNULL = FALSE)
    data5_internacoes_uti_sih_referencia <- eventReactive(c(filtros()$pesquisar, input$local_internacao_uti_sih, input$idade_dias_uti_sih), data5_referencia_aux(), ignoreNULL = FALSE)

    data5_referencia_baixo_peso <- reactive({
      data5_referencia_baixo_peso_aux <- reactive({
        base_referencia_baixo_peso |>
          dplyr::filter(
            if (filtros()$nivel == "Nacional")
              regiao %in% unique(tabela_aux_municipios$regiao)
            else if (filtros()$nivel == "Regional")
              regiao == filtros()$regiao
            else if (filtros()$nivel == "Estadual")
              uf == filtros()$estado
            else if (filtros()$nivel == "Macrorregião de saúde")
              macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
            else if(filtros()$nivel == "Microrregião de saúde")
              r_saude == filtros()$micro & uf == filtros()$estado_micro
            else if(filtros()$nivel == "Municipal")
              municipio == filtros()$municipio & uf == filtros()$estado_municipio
          ) |>
          dplyr::summarise(
            total_de_nascidos_vivos = sum(nascidos, na.rm = TRUE),
            porc_nasc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1),
            class = "Referência"
          ) |>
          dplyr::ungroup()
      })

      dplyr::full_join(
        data5_referencia_baixo_peso_aux(),
        data.frame(
          ano = filtros()$ano2[1]:filtros()$ano2[2],
          porc_nasc_baixo_peso = data5_referencia_baixo_peso_aux()$porc_nasc_baixo_peso,
          class = "Referência"
        )
      )
    })

    data5_referencia_baixo_peso_comp <- reactive({
      data5_referencia_baixo_peso_comp_aux <- reactive({
        base_referencia_baixo_peso |>
          dplyr::filter(
            if (filtros()$nivel2 == "Nacional")
              regiao %in% unique(tabela_aux_municipios$regiao)
            else if (filtros()$nivel2 == "Regional")
              regiao == filtros()$regiao2
            else if (filtros()$nivel2 == "Estadual")
              uf == filtros()$estado2
            else if (filtros()$nivel2 == "Macrorregião de saúde")
              macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
            else if(filtros()$nivel2 == "Microrregião de saúde")
              r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
            else if(filtros()$nivel2 == "Municipal")
              municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
            else if (filtros()$nivel2 == "Municípios semelhantes")
              grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
          ) |>
          dplyr::summarise(
            total_de_nascidos_vivos = sum(nascidos, na.rm = TRUE),
            porc_nasc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1),
            class = "Referência"
          ) |>
          dplyr::ungroup()
      })

      dplyr::full_join(
        data5_referencia_baixo_peso_comp_aux(),
        data.frame(
          ano = filtros()$ano2[1]:filtros()$ano2[2],
          porc_nasc_baixo_peso = data5_referencia_baixo_peso_comp_aux()$porc_nasc_baixo_peso,
          class = "Referência"
        )
      )
    })

    ### Para os gráficos de barras, juntando a localidade selecionada com a referência ----
    data5_juncao_barras <- reactive({
      if (filtros()$comparar == FALSE) {
        dplyr::full_join(data5(), data5_referencia(), by = "ano") |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
          )
      } else {
        dplyr::full_join(
          data5(),
          data5_comp() |>
            dplyr::rename(localidade_comparacao = class) |>
            dplyr::rename_with(~paste0("br_", .x), dplyr::starts_with("porc_baixo_peso_") | dplyr::starts_with("porc_premat_")),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
          )
      }
    })


    ## Criando os outputs dos gráficos ----------------------------------------
    ### Porcentagem de baixo peso ao nascer -----------------------------------
    output$plot1 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_baixo_peso(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)

        if (input$baixo_peso == "porc_nasc_baixo_peso") {
          grafico_base <- grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia_baixo_peso(),
              type = "line",
              name = "Referência para a localidade (meta de redução global)",
              highcharter::hcaes(x = ano, y = porc_nasc_baixo_peso, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        } else {
          grafico_base
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_baixo_peso(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp_baixo_peso(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia_baixo_peso(),
              type = "line",
              name = glue::glue("Referência para {unique(data5()$class)} (meta de redução global)"),
              highcharter::hcaes(x = ano, y = porc_nasc_baixo_peso, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Distribuição percentual do baixo peso ao nascer -----------------------
    output$plot1_1 <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "De 2000 a 2499 g",
          data =  data5_juncao_barras(),
          highcharter::hcaes(x = ano, y = porc_baixo_peso_2000_a_2499),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_baixo_peso_2000_a_2499:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500 a 1999 g",
          data =  data5_juncao_barras(),
          highcharter::hcaes(x = ano, y = porc_baixo_peso_1500_a_1999),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_baixo_peso_1500_a_1999:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1500 g",
          data =  data5_juncao_barras(),
          highcharter::hcaes(x = ano, y = porc_baixo_peso_menor_1500),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_baixo_peso_menor_1500:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(5, direction = -1)[-c(1, 5)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data5_juncao_barras()$ano), allowDecimals = FALSE, reversed = TRUE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)

    })

    ### Porcentagem de nascimentos prematuros ---------------------------------
    output$plot2 <- highcharter::renderHighchart({

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_prematuridade(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          )  |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if(input$faixa_prematuridade == "porc_nasc_premat") {
          grafico_base <- grafico_base  |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (países desenvolvidos)",
              highcharter::hcaes(x = ano, y = porc_nasc_premat, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        } else {
          grafico_base
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_prematuridade(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp_prematuridade(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data5_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_nasc_premat, group = localidade_comparacao, colour = localidade_comparacao),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
    })

    ### Distribuição percentual da prematuridade ------------------------------
    output$plot2_1 <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          data = data5_juncao_barras(),
          name = "Sem informação",
          highcharter::hcaes(x = ano, y = porc_premat_faltantes),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_premat_faltantes:,f}% </b>"
          )
        )  |>
        highcharter::hc_add_series(
          data = data5_juncao_barras(),
          name = "De 35 a 36 semanas",
          highcharter::hcaes(x = ano, y = porc_premat_35_a_36_semanas ),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_premat_35_a_36_semanas:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data5_juncao_barras(),
          name = "De 33 a 34 semanas",
          highcharter::hcaes(x = ano, y = porc_premat_33_a_34_semanas),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_premat_33_a_34_semanas:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data5_juncao_barras(),
          name = "De 28 a 32 semanas",
          highcharter::hcaes(x = ano , y = porc_premat_28_a_32_semanas),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_premat_28_a_32_semanas:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data5_juncao_barras(),
          name = "Menos de 28 semanas",
          highcharter::hcaes(x = ano, y = porc_premat_menos_de_28_semanas),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_premat_menos_de_28_semanas:,f}% </b>"
          )
        ) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""),categories = unique(data5_juncao_barras()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100) |>
        highcharter::hc_legend(reversed = TRUE)
    })

    ### Porcentagem de nascimentos termo precoce ------------------------------
    output$plot3 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = localidade_comparacao, colour = localidade_comparacao),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)

        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data5_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = localidade_comparacao, colour = localidade_comparacao),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
    })

    ### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    output$plot5 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data5_comp()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos públicos ----
    output$plot10 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_internacoes_publicos_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5_internacoes_publicos_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = max(c(
            c(data5_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih, data5_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_vinc_sus),
            c(data5_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih, data5_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_vinc_sus)
          ), na.rm = TRUE) + 1) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_internacoes_publicos_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_internacoes_publicos_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5_internacoes_publicos_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_internacoes_publicos_sih_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data5_internacoes_publicos_sih_comp()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = max(c(
            c(data5_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih, data5_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_vinc_sus),
            c(data5_internacoes_publicos_sih_comp()$porc_internacoes_menores_28_dias_sih, data5_internacoes_publicos_sih_comp()$porc_internacoes_menores_28_dias_vinc_sus),
            c(data5_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih, data5_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_vinc_sus)
          ), na.rm = TRUE) + 1) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_internacoes_publicos_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos com vínculo com o SUS ----
    output$plot11 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_internacoes_vinc_sus(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5_internacoes_vinc_sus()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_vinc_sus, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = max(c(
            c(data5_internacoes_vinc_sus()$porc_internacoes_menores_28_dias_sih, data5_internacoes_vinc_sus()$porc_internacoes_menores_28_dias_vinc_sus),
            c(data5_internacoes_vinc_sus_referencia()$porc_internacoes_menores_28_dias_sih, data5_internacoes_vinc_sus_referencia()$porc_internacoes_menores_28_dias_vinc_sus)
          ), na.rm = TRUE) + 1) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_internacoes_vinc_sus_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_vinc_sus, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_internacoes_vinc_sus(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5_internacoes_vinc_sus()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_vinc_sus, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_internacoes_vinc_sus_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data5_internacoes_vinc_sus_comp()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_vinc_sus, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = max(c(
            c(data5_internacoes_vinc_sus()$porc_internacoes_menores_28_dias_sih, data5_internacoes_vinc_sus()$porc_internacoes_menores_28_dias_vinc_sus),
            c(data5_internacoes_vinc_sus_comp()$porc_internacoes_menores_28_dias_sih, data5_internacoes_vinc_sus_comp()$porc_internacoes_menores_28_dias_vinc_sus),
            c(data5_internacoes_vinc_sus_referencia()$porc_internacoes_menores_28_dias_sih, data5_internacoes_vinc_sus_referencia()$porc_internacoes_menores_28_dias_vinc_sus)
          ), na.rm = TRUE) + 1) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_internacoes_vinc_sus_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_vinc_sus, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS ----
    output$plot12 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_internacoes_uti_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5_internacoes_uti_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_internacoes_uti_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_internacoes_uti_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5_internacoes_uti_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_internacoes_uti_sih_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data5_internacoes_uti_sih_comp()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_internacoes_uti_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Porcentagem de nascidos vivos com asfixia dentre os nascidos vivos sem anomalias e com peso > 2500 g ----
    output$plot4 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5()$class),
            highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5()$class),
            highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data5_comp()$class),
            highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Porcentagem de nascidos vivos com malformações ------------------------
    output$plot6 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5()$class),
            highcharter::hcaes(x = ano, y = porc_malformacao_geral, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:min(filtros()$ano2[2], 2023), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            max = max(c(
              c(data5()$porc_malformacao_geral, data5()$porc_malformacao_vigilancia),
              c(data5_referencia()$porc_malformacao_geral, data5_referencia()$porc_malformacao_vigilancia)
            ), na.rm = TRUE) + 0.1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_malformacao_geral, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5()$class),
            highcharter::hcaes(x = ano, y = porc_malformacao_geral, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data5_comp()$class),
            highcharter::hcaes(x = ano, y = porc_malformacao_geral, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:min(filtros()$ano2[2], 2023), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            max = max(c(
              c(data5()$porc_malformacao_geral, data5()$porc_malformacao_vigilancia),
              c(data5_comp()$porc_malformacao_geral, data5_comp()$porc_malformacao_vigilancia),
              c(data5_referencia()$porc_malformacao_geral, data5_referencia()$porc_malformacao_vigilancia)
            ), na.rm = TRUE) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_malformacao_geral, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Porcentagem de nascidos vivos com malformações prioritárias para vigilância definidas pelo MS ----
    output$plot7 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5()$class),
            highcharter::hcaes(x = ano, y = porc_malformacao_vigilancia, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:min(filtros()$ano2[2], 2023), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            max = max(c(
              c(data5()$porc_malformacao_geral, data5()$porc_malformacao_vigilancia),
              c(data5_referencia()$porc_malformacao_geral, data5_referencia()$porc_malformacao_vigilancia)
            ), na.rm = TRUE) + 0.1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_malformacao_vigilancia, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data5()$class),
            highcharter::hcaes(x = ano, y = porc_malformacao_vigilancia, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data5_comp()$class),
            highcharter::hcaes(x = ano, y = porc_malformacao_vigilancia, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:min(filtros()$ano2[2], 2023), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            max = max(c(
              c(data5()$porc_malformacao_geral, data5()$porc_malformacao_vigilancia),
              c(data5_comp()$porc_malformacao_geral, data5_comp()$porc_malformacao_vigilancia),
              c(data5_referencia()$porc_malformacao_geral, data5_referencia()$porc_malformacao_vigilancia)
            ), na.rm = TRUE) + 0.1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_malformacao_vigilancia, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Tabela dos grupos de malformações prioritárias para vigilância definidos pelo MS ----
    output$tabela_malformacoes <- reactable::renderReactable({
      proporcao_geral <- function(numerador, denominador, fator) {
        reactable::JS(
          paste0(
            "function(values, rows) {
              var numerator = 0
              var denominator = 0
              var uniqueDenominatorValues = new Set();

              rows.forEach(function (row, index) {
                numerator += row['", numerador, "'];

                // Adicione o valor ao conjunto de valores únicos no denominador
                uniqueDenominatorValues.add(row['", denominador, "']);
              });

              // Converta o conjunto de valores únicos de volta a um array
              var uniqueDenominatorArray = Array.from(uniqueDenominatorValues);

              // Soma os valores únicos no denominador
              uniqueDenominatorArray.forEach(function (value) {
                denominator += value;
              });

              if ('", fator, "' == 10000) {
                return numerator / denominator * 10000
              }
            }"
          )
        )
      }

      data5_malformacao() |>
        reactable::reactable(
          groupBy = c("grupo_de_anomalias_congenitas", "anomalia_descricao"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            grupo_de_anomalias_congenitas = reactable::colDef(
              name = "Grupo de anomalias congênitas",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            anomalia_descricao = reactable::colDef(
              name = "Código CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            ano = reactable::colDef(
              name = "Período",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = glue::glue("{filtros()$ano2[1]} a {min(filtros()$ano2[2], 2023)}")))
            ),
            frequencia = reactable::colDef(
              name = "Frequência",
              minWidth = 60,
              aggregate = "sum"
            ),
            total_de_nascidos_vivos = reactable::colDef(show = FALSE),
            prevalencia = reactable::colDef(
              name = "Prevalência (por 10000 nascidos vivos)",
              minWidth = 60,
              aggregate = proporcao_geral("frequencia", "total_de_nascidos_vivos", 10000),
              format = reactable::colFormat(
                digits = 2
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 500,
          rowStyle = htmlwidgets::JS(
            "function(rowInfo) {
                if (rowInfo.aggregated === true) {
                 return { fontWeight: 700 }
                }
              }"
          )
        )

    })

  })
}



## To be copied in the UI
# mod_bloco_5_ui("bloco_5_1")

## To be copied in the server
# mod_bloco_5_server("bloco_5_1")
