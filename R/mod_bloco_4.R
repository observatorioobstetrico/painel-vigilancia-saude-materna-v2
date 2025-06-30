#' bloco_4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_4_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(class = "fonte-titulos-pagina", tags$b(HTML("Assistência ao parto: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset1"),
      width = 12,
      collapsible = FALSE,
      # tabPanel(
      #   HTML("<b>Relacionados aos grupos de Robson</b>"),
      #   fluidRow(
      #     column(
      #       width = 5,
      #       selectizeInput(
      #         inputId = ns("indicador_robson"),
      #         label = HTML("<p class = 'fonte-muito-grande' style='margin-bottom: 0px'>Indicador</p>"),
      #         options = list(placeholder = "Selecione o indicador relacionado aos grupos de Robson"),
      #         choices = c(
      #           "Porcentagem de nascidos vivos por grupo de Robson" = "indicador2",
      #           "Porcentagem de cesarianas por grupo de Robson" = "indicador1",
      #           "Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas" = "indicador3"
      #         ),
      #         width = "81%"
      #       )
      #     )
      #   ),
      #   hr(),
      #   conditionalPanel(
      #     ns = ns,
      #     condition = "input.indicador_robson == 'indicador1'",
      #     fluidRow(
      #       column(
      #         width = 4,
      #         HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
      #         div(
      #           HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
      #           shinyWidgets::actionBttn(
      #             inputId = ns('botao_resumo1'),
      #             icon = icon('question'),
      #             style = 'material-circle',
      #             color = "primary",
      #             size = 'xs'
      #           )
      #         ),
      #         hr(),
      #         fluidRow(
      #           column(
      #             width = 12,
      #             HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      #             uiOutput(ns("input_localidade_resumo1")),
      #             align = "center"
      #           )
      #         ),
      #         # fluidRow(
      #         #   bs4Dash::box(
      #         #     width = 12,
      #         #     collapsible = FALSE,
      #         #     headerBorder = FALSE,
      #         #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
      #         #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart1"), height = 530))
      #         #   )
      #         # ),
      #         fluidRow(
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_indicador1")), proxy.height = "300px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_indicador1")), proxy.height = "300px")
      #           )
      #         ),
      #         fluidRow(
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_indicador1")), proxy.height = "332px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_indicador1")), proxy.height = "332px")
      #           )
      #         ),
      #         fluidRow(
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_indicador1")), proxy.height = "332px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_indicador1")), proxy.height = "332px")
      #           )
      #         ),
      #         fluidRow(
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_indicador1")), proxy.height = "332px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i8_indicador1")), proxy.height = "332px")
      #           )
      #         )
      #       ),
      #       column(
      #         width = 8,
      #         bs4Dash::bs4Card(
      #           width = 12,
      #           status = "primary",
      #           collapsible = FALSE,
      #           headerBorder = FALSE,
      #           style = "padding-top: 0; padding-bottom: 0; overflow-y: auto",
      #           HTML("<b class = 'fonte-muito-grande'> Porcentagem de cesarianas por grupo de Robson &nbsp;</b>"),
      #           shinyjs::hidden(
      #             span(
      #               id = ns("mostrar_botao1"),
      #               shinyWidgets::actionBttn(
      #                 inputId = ns("botao1"),
      #                 icon = icon("triangle-exclamation", style = "color: red"),
      #                 color = "warning",
      #                 style = "material-circle",
      #                 size = "xs"
      #               )
      #             )
      #           ),
      #           hr(),
      #           fluidRow(
      #             column(
      #               width = 6,
      #               div(
      #                 style = "text-align: center;",
      #                 HTML("<b class = 'fonte-grande'> Geral </b>"),
      #                 shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador1"), height = 340)),
      #               ),
      #               div(
      #                 style = "text-align: center;",
      #                 HTML("<b class = 'fonte-grande'> Grupo 2 de Robson &nbsp;</b>"),
      #                 shinyWidgets::actionBttn(
      #                   inputId = ns('texto_robson2'),
      #                   icon = icon('question'),
      #                   style = 'material-circle',
      #                   color = "primary",
      #                   size = 'xs'
      #                 ),
      #                 shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_indicador1"), height = 340))
      #               ),
      #               div(
      #                 style = "text-align: center;",
      #                 HTML("<b class = 'fonte-grande'> Grupo 4 de Robson &nbsp;</b>"),
      #                 shinyWidgets::actionBttn(
      #                   inputId = ns('texto_robson4'),
      #                   icon = icon('question'),
      #                   style = 'material-circle',
      #                   color = "primary",
      #                   size = 'xs'
      #                 ),
      #                 shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_indicador1"), height = 340)),
      #               ),
      #               div(
      #                 style = "text-align: center;",
      #                 HTML("<b class = 'fonte-grande'> Grupos 6 a 9 de Robson &nbsp;</b>"),
      #                 shinyWidgets::actionBttn(
      #                   inputId = ns('texto_robson6_a_9'),
      #                   icon = icon('question'),
      #                   style = 'material-circle',
      #                   color = "primary",
      #                   size = 'xs'
      #                 ),
      #                 shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot7_indicador1"), height = 339))
      #               )
      #             ),
      #             column(
      #               width = 6,
      #               div(
      #                 style = "text-align: center;",
      #                 HTML("<b class = 'fonte-grande'> Grupo 1 de Robson &nbsp;</b>"),
      #                 shinyWidgets::actionBttn(
      #                   inputId = ns('texto_robson1'),
      #                   icon = icon('question'),
      #                   style = 'material-circle',
      #                   color = "primary",
      #                   size = 'xs'
      #                 ),
      #                 shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_indicador1"), height = 340)),
      #               ),
      #               div(
      #                 style = "text-align: center;",
      #                 HTML("<b class = 'fonte-grande'> Grupo 3 de Robson &nbsp;</b>"),
      #                 shinyWidgets::actionBttn(
      #                   inputId = ns('texto_robson3'),
      #                   icon = icon('question'),
      #                   style = 'material-circle',
      #                   color = "primary",
      #                   size = 'xs'
      #                 ),
      #                 shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_indicador1"), height = 340)),
      #               ),
      #               div(
      #                 style = "text-align: center;",
      #                 HTML("<b class = 'fonte-grande'> Grupo 5 de Robson &nbsp;</b>"),
      #                 shinyWidgets::actionBttn(
      #                   inputId = ns('texto_robson5'),
      #                   icon = icon('question'),
      #                   style = 'material-circle',
      #                   color = "primary",
      #                   size = 'xs'
      #                 ),
      #                 shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_indicador1"), height = 340)),
      #               ),
      #               div(
      #                 style = "text-align: center;",
      #                 HTML("<b class = 'fonte-grande'> Grupo 10 de Robson &nbsp;</b>"),
      #                 shinyWidgets::actionBttn(
      #                   inputId = ns('texto_robson10'),
      #                   icon = icon('question'),
      #                   style = 'material-circle',
      #                   color = "primary",
      #                   size = 'xs'
      #                 ),
      #                 shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot8_indicador1"), height = 339))
      #               )
      #             )
      #           )
      #         )
      #       )
      #     )
      #   ),
      #   conditionalPanel(
      #     ns = ns,
      #     condition = "input.indicador_robson == 'indicador2'",
      #     fluidRow(
      #       column(
      #         width = 4,
      #         HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
      #         div(
      #           HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
      #           shinyWidgets::actionBttn(
      #             inputId = ns('botao_resumo2'),
      #             icon = icon('question'),
      #             style = 'material-circle',
      #             color = "primary",
      #             size = 'xs'
      #           )
      #         ),
      #         hr(),
      #         fluidRow(
      #           column(
      #             width = 12,
      #             HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      #             uiOutput(ns("input_localidade_resumo2")),
      #             align = "center"
      #           )
      #         ),
      #         # fluidRow(
      #         #   bs4Dash::box(
      #         #     width = 12,
      #         #     collapsible = FALSE,
      #         #     headerBorder = FALSE,
      #         #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
      #         #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart2"), height = 578))
      #         #   )
      #         # ),
      #         fluidRow(
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_indicador2")), proxy.height = "293px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_indicador2")), proxy.height = "293px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_indicador2")), proxy.height = "293px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_indicador2")), proxy.height = "325px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_indicador2")), proxy.height = "325px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_indicador2")), proxy.height = "325px")
      #           ),
      #           column(
      #             offset = 3,
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_indicador2")), proxy.height = "325px")
      #           )
      #         )
      #       ),
      #       column(
      #         width = 8,
      #         bs4Dash::bs4Card(
      #           width = 12,
      #           status = "primary",
      #           collapsible = FALSE,
      #           headerBorder = FALSE,
      #           style = "height: 741px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
      #           HTML("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos por grupo de Robson &nbsp;</b>"),
      #           shinyjs::hidden(
      #             span(
      #               id = ns("mostrar_botao2"),
      #               shinyWidgets::actionBttn(
      #                 inputId = ns("botao2"),
      #                 icon = icon("triangle-exclamation", style = "color: red"),
      #                 color = "warning",
      #                 style = "material-circle",
      #                 size = "xs"
      #               )
      #             )
      #           ),
      #           hr(),
      #           shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador2"), height = 650))
      #         )
      #       )
      #     )
      #   ),
      #   conditionalPanel(
      #     ns = ns,
      #     condition = "input.indicador_robson == 'indicador3'",
      #     fluidRow(
      #       column(
      #         width = 4,
      #         HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
      #         div(
      #           HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
      #           shinyWidgets::actionBttn(
      #             inputId = ns('botao_resumo3'),
      #             icon = icon('question'),
      #             style = 'material-circle',
      #             color = "primary",
      #             size = 'xs'
      #           )
      #         ),
      #         hr(),
      #         fluidRow(
      #           column(
      #             width = 12,
      #             HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      #             uiOutput(ns("input_localidade_resumo3")),
      #             align = "center"
      #           )
      #         ),
      #         # fluidRow(
      #         #   bs4Dash::box(
      #         #     width = 12,
      #         #     collapsible = FALSE,
      #         #     headerBorder = FALSE,
      #         #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
      #         #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart3"), height = 578))
      #         #   )
      #         # ),
      #         fluidRow(
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_indicador3")), proxy.height = "293px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_indicador3")), proxy.height = "293px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_indicador3")), proxy.height = "293px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_indicador3")), proxy.height = "325px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_indicador3")), proxy.height = "325px")
      #           ),
      #           column(
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_indicador3")), proxy.height = "325px")
      #           ),
      #           column(
      #             offset = 3,
      #             width = 6,
      #             shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_indicador3")), proxy.height = "325px")
      #           )
      #         )
      #       ),
      #       column(
      #         width = 8,
      #         bs4Dash::bs4Card(
      #           width = 12,
      #           status = "primary",
      #           collapsible = FALSE,
      #           headerBorder = FALSE,
      #           style = "height: 741px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
      #           HTML("<b class = 'fonte-muito-grande'> Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas &nbsp;</b>"),
      #           shinyjs::hidden(
      #             span(
      #               id = ns("mostrar_botao3"),
      #               shinyWidgets::actionBttn(
      #                 inputId = ns("botao3"),
      #                 icon = icon("triangle-exclamation", style = "color: red"),
      #                 color = "warning",
      #                 style = "material-circle",
      #                 size = "xs"
      #               )
      #             )
      #           ),
      #           hr(),
      #           shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador3"), height = 650))
      #         )
      #       )
      #     )
      #   )
      # ),
      tabPanel(
        HTML("<b>Relacionados ao deslocamento para o parto</b>"),
        conditionalPanel(
          ns = ns,
          condition = "output.comparar == 'Sim'",
          column(
            width = 12,
            HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
            conditionalPanel(
              ns = ns,
              condition = "output.nivel == 'estadual' | output.nivel == 'municipal'",
              HTML(
                "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; A função de comparação não está disponível para os indicadores de medianas de deslocamento.
                </b> </div>"
              )
            ),
            hr(),
            HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
          )
        ),
        fluidRow(
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            div(
              HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
              shinyWidgets::actionBttn(
                inputId = ns('botao_resumo4'),
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
                uiOutput(ns("input_localidade_resumo4")),
                align = "center"
              )
            ),
            # fluidRow(
            #   bs4Dash::box(
            #     width = 12,
            #     collapsible = FALSE,
            #     headerBorder = FALSE,
            #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
            #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart4"), height = 530))
            #   )
            # ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_deslocamento")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_deslocamento")), proxy.height = "300px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_deslocamento")), proxy.height = "332px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_deslocamento")), proxy.height = "332px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_deslocamento")), proxy.height = "332px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i9_deslocamento_macro")), proxy.height = "332px")
              )
            ),
            # fluidRow(
            #   column(
            #     width = 6,
            #     shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i11_deslocamento_macro")), proxy.height = "332px")
            #   ),
            #   column(
            #     width = 6,
            #     shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i10_deslocamento_macro")), proxy.height = "332px")
            #   )
            # )
          ),
          column(
            width = 8,
            fluidRow(
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 7%; display: flex; align-items: center;",
                  HTML(glue::glue("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos segundo local de ocorrência do parto &nbsp;</b>")),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_deslocamento_prop1"),
                      shinyWidgets::actionBttn(
                        inputId = ns("botao_prop1"),
                        icon = icon("triangle-exclamation", style = "color: red"),
                        color = "warning",
                        style = "material-circle",
                        size = "xs"
                      )
                    )
                  )
                ),
                hr(),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_prop"), height = "550px"))
              )
            ),
            fluidRow(
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 7%; display: flex; align-items: center;",
                  HTML(glue::glue("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos com peso < 1500g segundo região de ocorrência e disponibilidade de pelo menos quatro leitos de UTI neonatal &nbsp;</b>")),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_deslocamento_prop2"),
                      shinyWidgets::actionBttn(
                        inputId = ns("botao_prop2"),
                        icon = icon("triangle-exclamation", style = "color: red"),
                        color = "warning",
                        style = "material-circle",
                        size = "xs"
                      )
                    )
                  )
                ),
                hr(),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_macrorregiao"), height = "550px"))
              )
            ),
            shinyjs::hidden(
              fluidRow(
                id = ns("mostrar_card_indicadores_deslocamento_muni_uf"),
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 7%; display: flex; align-items: center;",
                    HTML(glue::glue("<b class = 'fonte-muito-grande'> Mediana de deslocamento para o destino, segundo destinos &nbsp;</b>")),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_deslocamento_mediana1"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_mediana1"),
                          icon = icon("triangle-exclamation", style = "color: red"),
                          color = "warning",
                          style = "material-circle",
                          size = "xs"
                        )
                      )
                    )
                  ),
                  hr(),
                  selectizeInput(
                    inputId = ns("local_med"),
                    label = span(class = "fonte-grande", "Local de ocorrência do parto"),
                    options = list(placeholder = "Selecione o local de ocorrência do parto"),
                    choices = c(
                      "Fora do município de residência da mulher" = "fora_municipio",
                      "Na região de saúde, mas fora do município de residência da mulher" = "na_regiao",
                      "Na macrorregião de saúde, mas fora da região de saúde de residência mulher" = "na_macrorregiao",
                      "Na UF, mas fora da macrorregião de saúde de residência mulher" = "fora_macrorregiao",
                      "Fora da UF de residência da mulher" = "fora_uf"
                    ),
                    width = "100%"
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_med"), height = "480px"))
                )
              )
            ),
            shinyjs::hidden(
              fluidRow(
                id = ns("mostrar_card_indicadores_deslocamento_muni"),
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 7%; display: flex; align-items: center;",
                    tags$b(HTML(glue::glue("Informações adicionais sobre os partos")), htmlOutput(ns("municipio_informacoes_adicionais"), inline = TRUE), HTML("&nbsp"), class = 'fonte-muito-grande'),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_deslocamento_infos"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_infos"),
                          icon = icon("triangle-exclamation", style = "color: red"),
                          color = "warning",
                          style = "material-circle",
                          size = "xs"
                        )
                      )
                    )
                  ),
                  hr(),
                  shinycssloaders::withSpinner(reactable::reactableOutput(ns("infos_deslocamento_muni"), height = "520px"))
                )
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b>Relacionados ao profissional e local do parto</b>"),

        fluidRow(
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            div(
              HTML("<b style='font-size:19px'> Resumo do período &nbsp;</b>"),
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
            ## caixinhas

            ##AQUI

            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_profissional")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_profissional")), proxy.height = "300px")
              )
            )
          ),
          column(
            width = 8,
            fluidRow(
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'>Distribuição percentual de nascimentos segundo local</b>"),
                  ),
                  hr(),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_dist_local"), height = "550px"))
                ),
            )
          )
        ),

        #AQUI
        fluidRow(
          column(
            width = 4,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "display: flex; align-items: center;",
                HTML("<b style='font-size:19px'>Distribuição percentual de partos vaginais</b>")
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_dist_partos_vaginais"), height = "550px"))
            )
          ),
          column(
            width = 8,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "display: flex; align-items: center;",
                HTML(glue::glue("<b style = 'font-size: 19px'> Distribuição percentual do tipo de profissional na assistência em partos vaginais hospitalares &nbsp;</b>")),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao_deslocamento_prop1"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao_prop1"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              # hr(),
              # fluidRow(
              #   column(
              #     width = 12,
              #     shinyWidgets::pickerInput(
              #       inputId = ns("local_nasc"),
              #       label = "Selecione, aqui, os locais de interesse:",
              #       options = list(placeholder = "Selecione, aqui, os locais de interesse:",
              #                      `actions-box` = TRUE,
              #                      `deselect-all-text` = "Desselecionar todas",
              #                      `select-all-text` = "Selecionar todas",
              #                      `none-selected-text` = "Nenhuma opção selecionada"),
              #       choices = c(
              #         "Hospital" = "hospital",
              #         "Outros estabelecimentos de saúde" = "outros_est_saude",
              #         "Domicílio" = "domicilio",
              #         "Aldeia Indígena" = "aldeia",
              #         "Outros" = "outros",
              #         "Sem informação" = "sem_inf"
              #
              #       ),
              #       selected = c(
              #         "hospital",
              #         "outros_est_saude",
              #         "domicilio",
              #         "aldeia",
              #         "outros",
              #         "sem_inf"
              #       ),
              #       multiple = TRUE,
              #       width = "99%"
              #     )
              #   )
              # ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_dist_profissional"), height = "550px"))
              )
            )
        ) ## [XXX]
        ## AQUI
      ), # incluir robson aqui
      tabPanel(
      HTML("<b>Relacionados aos grupos de Robson</b>"),
      fluidRow(
        column(
          width = 5,
          selectizeInput(
            inputId = ns("indicador_robson"),
            label = HTML("<p class = 'fonte-muito-grande' style='margin-bottom: 0px'>Indicador</p>"),
            options = list(placeholder = "Selecione o indicador relacionado aos grupos de Robson"),
            choices = c(
              "Porcentagem de nascidos vivos por grupo de Robson" = "indicador2",
              "Porcentagem de cesarianas por grupo de Robson" = "indicador1",
              "Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas" = "indicador3"
            ),
            width = "81%"
          )
        )
      ),
      hr(),
      conditionalPanel(
        ns = ns,
        condition = "input.indicador_robson == 'indicador1'",
        fluidRow(
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            div(
              HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
              shinyWidgets::actionBttn(
                inputId = ns('botao_resumo1'),
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
                uiOutput(ns("input_localidade_resumo1")),
                align = "center"
              )
            ),
            # fluidRow(
            #   bs4Dash::box(
            #     width = 12,
            #     collapsible = FALSE,
            #     headerBorder = FALSE,
            #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
            #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart1"), height = 530))
            #   )
            # ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_indicador1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_indicador1")), proxy.height = "300px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_indicador1")), proxy.height = "332px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_indicador1")), proxy.height = "332px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_indicador1")), proxy.height = "332px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_indicador1")), proxy.height = "332px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_indicador1")), proxy.height = "332px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i8_indicador1")), proxy.height = "332px")
              )
            )
          ),
          column(
            width = 8,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "padding-top: 0; padding-bottom: 0; overflow-y: auto",
              HTML("<b class = 'fonte-muito-grande'> Porcentagem de cesarianas por grupo de Robson &nbsp;</b>"),
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
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  div(
                    style = "text-align: center;",
                    HTML("<b class = 'fonte-grande'> Geral </b>"),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador1"), height = 340)),
                  ),
                  div(
                    style = "text-align: center;",
                    HTML("<b class = 'fonte-grande'> Grupo 2 de Robson &nbsp;</b>"),
                    shinyWidgets::actionBttn(
                      inputId = ns('texto_robson2'),
                      icon = icon('question'),
                      style = 'material-circle',
                      color = "primary",
                      size = 'xs'
                    ),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_indicador1"), height = 340))
                  ),
                  div(
                    style = "text-align: center;",
                    HTML("<b class = 'fonte-grande'> Grupo 4 de Robson &nbsp;</b>"),
                    shinyWidgets::actionBttn(
                      inputId = ns('texto_robson4'),
                      icon = icon('question'),
                      style = 'material-circle',
                      color = "primary",
                      size = 'xs'
                    ),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_indicador1"), height = 340)),
                  ),
                  div(
                    style = "text-align: center;",
                    HTML("<b class = 'fonte-grande'> Grupos 6 a 9 de Robson &nbsp;</b>"),
                    shinyWidgets::actionBttn(
                      inputId = ns('texto_robson6_a_9'),
                      icon = icon('question'),
                      style = 'material-circle',
                      color = "primary",
                      size = 'xs'
                    ),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot7_indicador1"), height = 339))
                  )
                ),
                column(
                  width = 6,
                  div(
                    style = "text-align: center;",
                    HTML("<b class = 'fonte-grande'> Grupo 1 de Robson &nbsp;</b>"),
                    shinyWidgets::actionBttn(
                      inputId = ns('texto_robson1'),
                      icon = icon('question'),
                      style = 'material-circle',
                      color = "primary",
                      size = 'xs'
                    ),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_indicador1"), height = 340)),
                  ),
                  div(
                    style = "text-align: center;",
                    HTML("<b class = 'fonte-grande'> Grupo 3 de Robson &nbsp;</b>"),
                    shinyWidgets::actionBttn(
                      inputId = ns('texto_robson3'),
                      icon = icon('question'),
                      style = 'material-circle',
                      color = "primary",
                      size = 'xs'
                    ),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_indicador1"), height = 340)),
                  ),
                  div(
                    style = "text-align: center;",
                    HTML("<b class = 'fonte-grande'> Grupo 5 de Robson &nbsp;</b>"),
                    shinyWidgets::actionBttn(
                      inputId = ns('texto_robson5'),
                      icon = icon('question'),
                      style = 'material-circle',
                      color = "primary",
                      size = 'xs'
                    ),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_indicador1"), height = 340)),
                  ),
                  div(
                    style = "text-align: center;",
                    HTML("<b class = 'fonte-grande'> Grupo 10 de Robson &nbsp;</b>"),
                    shinyWidgets::actionBttn(
                      inputId = ns('texto_robson10'),
                      icon = icon('question'),
                      style = 'material-circle',
                      color = "primary",
                      size = 'xs'
                    ),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot8_indicador1"), height = 339))
                  )
                )
              )
            )
          )
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.indicador_robson == 'indicador2'",
        fluidRow(
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            div(
              HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
              shinyWidgets::actionBttn(
                inputId = ns('botao_resumo2'),
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
                uiOutput(ns("input_localidade_resumo2")),
                align = "center"
              )
            ),
            # fluidRow(
            #   bs4Dash::box(
            #     width = 12,
            #     collapsible = FALSE,
            #     headerBorder = FALSE,
            #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
            #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart2"), height = 578))
            #   )
            # ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_indicador2")), proxy.height = "293px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_indicador2")), proxy.height = "293px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_indicador2")), proxy.height = "293px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_indicador2")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_indicador2")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_indicador2")), proxy.height = "325px")
              ),
              column(
                offset = 3,
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_indicador2")), proxy.height = "325px")
              )
            )
          ),
          column(
            width = 8,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              HTML("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos por grupo de Robson &nbsp;</b>"),
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
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador2"), height = 550))
            )
          )
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.indicador_robson == 'indicador3'",
        fluidRow(
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            div(
              HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
              shinyWidgets::actionBttn(
                inputId = ns('botao_resumo3'),
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
                uiOutput(ns("input_localidade_resumo3")),
                align = "center"
              )
            ),
            # fluidRow(
            #   bs4Dash::box(
            #     width = 12,
            #     collapsible = FALSE,
            #     headerBorder = FALSE,
            #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
            #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart3"), height = 578))
            #   )
            # ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_indicador3")), proxy.height = "293px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_indicador3")), proxy.height = "293px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_indicador3")), proxy.height = "293px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_indicador3")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_indicador3")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_indicador3")), proxy.height = "325px")
              ),
              column(
                offset = 3,
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_indicador3")), proxy.height = "325px")
              )
            )
          ),
          column(
            width = 8,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              HTML("<b class = 'fonte-muito-grande'> Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas &nbsp;</b>"),
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
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador3"), height = 550))
            )
          )
        )
      )
      )


    )
  )
}

#' bloco_4 Server Functions
#'
#' @noRd
mod_bloco_4_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Criando data.frames com os cálculos dos indicadores ---------------------
    bloco4_calcs <- data.frame(
      tipo = c("local", "referencia"),
      prop_nasc_robson1 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_1) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
      prop_nasc_robson2 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_2) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
      prop_nasc_robson3 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_3) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
      prop_nasc_robson4 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_4) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
      prop_nasc_robson5 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_5) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
      prop_nasc_robson6_a_9 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
      prop_nasc_robson10 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_10) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
      prop_nasc_robson_faltante = rep('round((sum(total_de_nascidos_vivos) - sum(dplyr::across(dplyr::starts_with("mulheres_dentro")))) / sum(total_de_nascidos_vivos) * 100, 1)', 2),
      prop_tx_cesariana_geral = c("round(sum(mulheres_com_parto_cesariana)/sum(total_de_nascidos_vivos) * 100, 1)", "25"),
      prop_robson1_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_1) / sum(mulheres_dentro_do_grupo_de_robson_1)) * 100, 1)", "10"),
      prop_robson2_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_2) / sum(mulheres_dentro_do_grupo_de_robson_2)) * 100, 1)", "35"),
      prop_robson3_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_3) / sum(mulheres_dentro_do_grupo_de_robson_3)) * 100, 1)", "3"),
      prop_robson4_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_4) / sum(mulheres_dentro_do_grupo_de_robson_4)) * 100, 1)", "15"),
      prop_robson5_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_5) / sum(mulheres_dentro_do_grupo_de_robson_5)) * 100, 1)", "60"),
      prop_robson6_a_9_tx_cesariana = rep("round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1)", 2),
      prop_robson10_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_10) / sum(mulheres_dentro_do_grupo_de_robson_10)) * 100, 1)", "30"),
      contrib_robson1_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_1) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
      contrib_robson2_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_2) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
      contrib_robson3_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_3) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
      contrib_robson4_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_4) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
      contrib_robson5_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_5) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
      contrib_robson6_a_9_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
      contrib_robson10_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_10) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
      contrib_robson_faltante_tx_cesariana = rep("round((sum(mulheres_com_parto_cesariana) - sum(dplyr::across(dplyr::starts_with('total_cesariana')))) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
      #[REFF]
      percentil_95_robson6_a_9_tx_cesariana = rep("round(quantile((total_cesariana_grupo_robson_6_ao_9 / mulheres_dentro_do_grupo_de_robson_6_ao_9)* 100, probs = 0.95, na.rm = T), 2)"),
      percentil_5_robson6_a_9_tx_cesariana = rep("round(quantile((total_cesariana_grupo_robson_6_ao_9 / mulheres_dentro_do_grupo_de_robson_6_ao_9)* 100, probs = 0.05, na.rm = T), 2)")
    )

    bloco4_deslocamento_calcs <- data.frame(
      tipo = c("local", "referencia"),
      prop_partos_municipio_res = rep("round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_rsaude_res = rep("round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_macro_rsaude_res = rep("round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_fora_macro_rsaude_res = rep("round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_fora_uf_res = rep("round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2)
    )

    #[PROFF]
    bloco4_profissional_calcs <- data.frame( # corrigir total_de_nascidos_vivos para total_de_nascidos_vivos_partos_vaginais
      tipo = c("local", "referencia"),

      prop_nasc_local_hospital = rep("round(sum(nasc_local_hospital, na.rm = TRUE)/sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE) * 100, 1)", 2),
      prop_nasc_local_outros_est_saude = rep("round(sum(nasc_local_outros_est_saude, na.rm = TRUE)/sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE) * 100, 1)", 2),
      prop_nasc_local_domicilio = rep("round(sum(nasc_local_domicilio, na.rm = TRUE)/sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE) * 100, 1)", 2),
      prop_nasc_local_outros = rep("round(sum(nasc_local_outros, na.rm = TRUE)/sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE) * 100, 1)", 2),
      prop_nasc_local_aldeia = rep("round(sum(nasc_local_aldeia, na.rm = TRUE)/sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE) * 100, 1)", 2),
      prop_nasc_local_sem_inf = rep("round(sum(nasc_local_sem_inf, na.rm = TRUE)/sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE) * 100, 1)", 2),

      prop_nasc_local_fora_hospital = rep("round(sum(nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros,  nasc_local_aldeia, na.rm = TRUE)/sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE) * 100, 1)", 2),

      prop_nasc_assistido_enf_obs = rep("round(sum(nasc_assistido_enf_obs, na.rm = TRUE)/sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE) * 100,1)",2)

      #prop_nasc_partos_vaginais = rep("round(sum(total_de_nascidos_vivos_partos_vaginais, na.tm = TRUE)/total_de_nascidos_vivos, na.rm = TRUE)", 2)

      # dist_medico = rep("round(
      #   sum(c(nasc_assistido_medico_hospital, nasc_assistido_medico_outros_est_saude, nasc_assistido_medico_domicilio, nasc_assistido_medico_outros, nasc_assistido_medico_aldeia, nasc_assistido_medico_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)/
      #     sum(c(nasc_local_hospital, nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros, nasc_local_aldeia, nasc_local_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)
      #   *100, 1)", 2),
      #
      # dist_enf_obs = rep("round(
      #   sum(c(nasc_assistido_enf_obs_hospital, nasc_assistido_enf_obs_outros_est_saude, nasc_assistido_enf_obs_domicilio, nasc_assistido_enf_obs_outros, nasc_assistido_enf_obs_aldeia, nasc_assistido_enf_obs_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)/
      #     sum(c(nasc_local_hospital, nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros, nasc_local_aldeia, nasc_local_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)
      #   *100, 1)", 2),
      #
      # dist_parteira = rep("round(
      #   sum(c(nasc_assistido_parteira_hospital, nasc_assistido_parteira_outros_est_saude, nasc_assistido_parteira_domicilio, nasc_assistido_parteira_outros, nasc_assistido_parteira_aldeia, nasc_assistido_parteira_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)/
      #     sum(c(nasc_local_hospital, nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros, nasc_local_aldeia, nasc_local_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)
      #   *100, 1)", 2),
      #
      # dist_outros = rep("round(
      #   sum(c(nasc_assistido_outros_hospital, nasc_assistido_outros_outros_est_saude, nasc_assistido_outros_domicilio, nasc_assistido_outros_outros, nasc_assistido_outros_aldeia, nasc_assistido_outros_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)/
      #     sum(c(nasc_local_hospital, nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros, nasc_local_aldeia, nasc_local_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)
      #   *100, 1)", 2),
      #
      # dist_ignorado = rep("round(
      #   sum(c(nasc_assistido_ignorado_hospital, nasc_assistido_ignorado_outros_est_saude, nasc_assistido_ignorado_domicilio, nasc_assistido_ignorado_outros, nasc_assistido_ignorado_aldeia, nasc_assistido_ignorado_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)/
      #     sum(c(nasc_local_hospital, nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros, nasc_local_aldeia, nasc_local_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)
      #   *100, 1)", 2),
      #
      # dist_sem_inf = rep("round(
      #   sum(c(nasc_assistido_sem_inf_hospital, nasc_assistido_sem_inf_outros_est_saude, nasc_assistido_sem_inf_domicilio, nasc_assistido_sem_inf_outros, nasc_assistido_sem_inf_aldeia, nasc_assistido_sem_inf_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)/
      #     sum(c(nasc_local_hospital, nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros, nasc_local_aldeia, nasc_local_sem_inf)[seleciona(aba = 'profissional e local') %in% input$local_nasc], na.rm=T)
      #   *100, 1)", 2)
    )

    #AQUI EU FIZ UMA GAMBIARRA

    # selecao_local <- reactive({
    #   selected_locations <- c('hospital', 'outros_est_saude', 'domicilio',
    #                           'outros', 'aldeia', 'sem_inf') %in% input$local_nasc
    #   paste0(selected_locations, collapse = ", ")
    #   print(paste0(selected_locations, collapse = ", "))
    # })

    bloco4_profissional_calcs2 <- data.frame(
      tipo = c("local", "referencia"),

      dist_medico = rep("round(sum(nasc_assistido_medico_hospital, na.rm = TRUE)/
                               sum(nasc_local_hospital, na.rm = TRUE) * 100, 1)", 2),

      dist_enf_obs = rep("round(sum(nasc_assistido_enf_obs_hospital, na.rm = TRUE)/
                               sum(nasc_local_hospital, na.rm = TRUE) * 100, 1)", 2),

      dist_parteira = rep("round(sum(nasc_assistido_parteira_hospital, na.rm = TRUE)/
                               sum(nasc_local_hospital, na.rm = TRUE) * 100, 1)", 2),

      dist_outros = rep("round(sum(nasc_assistido_outros_hospital, na.rm = TRUE)/
                               sum(nasc_local_hospital, na.rm = TRUE) * 100, 1)", 2),

      dist_ignorado = rep("round(sum(nasc_assistido_ignorado_hospital, na.rm = TRUE)/
                               sum(nasc_local_hospital, na.rm = TRUE) * 100, 1)", 2),

      dist_sem_inf = rep("round(sum(nasc_assistido_sem_inf_hospital, na.rm = TRUE)/
                               sum(nasc_local_hospital, na.rm = TRUE) * 100, 1)", 2)

    )

    # bloco4_profissional_calcs3 <- data.frame(
    #   tipo = c("local", "referencia"),
    #
    #   dist_medico = rep("sum(nasc_assistido_medico_hospital, na.rm = TRUE)", 2),
    #   dist_enf_obs = rep("sum(nasc_assistido_enf_obs_hospital, na.rm = TRUE)", 2),
    #   dist_parteira = rep("sum(nasc_assistido_parteira_hospital, na.rm = TRUE)", 2),
    #   dist_outros = rep("sum(nasc_assistido_outros_hospital, na.rm = TRUE)", 2),
    #   dist_ignorado = rep("sum(nasc_assistido_ignorado_hospital, na.rm = TRUE)", 2),
    #   dist_sem_inf = rep("sum(nasc_assistido_sem_inf_hospital, na.rm = TRUE)", 2)
    #
    # )

    bloco4_profissional_calcs4 <- data.frame(
      tipo = c("local", "referencia"),

    dist_partos_vaginais = rep("round(sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE)*100, 1)", 2),
    dist_outros = rep("round((sum(total_de_nascidos_vivos, na.rm = TRUE) - sum(total_de_nascidos_vivos_partos_vaginais, na.rm = TRUE))/sum(total_de_nascidos_vivos, na.rm = TRUE)*100, 1)", 2)
    )

    # selecao_local1 <- reactive({
    #   selected_locations <- c('hospital', 'outros_est_saude', 'domicilio',
    #                           'outros', 'aldeia', 'sem_inf') %in% input$local_nasc
    # })
    #
    # bloco4_profissional_calcs2 <- reactive({
    #   bloco4_profissional_calcs2_aux <- data.frame(
    #     tipo = c("local", "referencia"),
    #
    #     dist_medico = rep(paste0("round(
    #       sum(c(
    #       nasc_assistido_medico_hospital[", selecao_local1()[1],"],
    #       nasc_assistido_medico_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_assistido_medico_domicilio[", selecao_local1()[3],"],
    #       nasc_assistido_medico_outros[", selecao_local1()[4],"],
    #       nasc_assistido_medico_aldeia[", selecao_local1()[5],"],
    #       nasc_assistido_medico_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)/
    #         sum(c(
    #       nasc_local_hospital[", selecao_local1()[1],"],
    #       nasc_local_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_local_domicilio[", selecao_local1()[3],"],
    #       nasc_local_outros[", selecao_local1()[4],"],
    #       nasc_local_aldeia[", selecao_local1()[5],"],
    #       nasc_local_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)
    #       *100, 1)"), 2),
    #
    #     dist_enf_obs = rep(paste0("round(
    #       sum(c(
    #       nasc_assistido_enf_obs_hospital[", selecao_local1()[1],"],
    #       nasc_assistido_enf_obs_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_assistido_enf_obs_domicilio[", selecao_local1()[3],"],
    #       nasc_assistido_enf_obs_outros[", selecao_local1()[4],"],
    #       nasc_assistido_enf_obs_aldeia[", selecao_local1()[5],"],
    #       nasc_assistido_enf_obs_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)/
    #         sum(c(
    #       nasc_local_hospital[", selecao_local1()[1],"],
    #       nasc_local_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_local_domicilio[", selecao_local1()[3],"],
    #       nasc_local_outros[", selecao_local1()[4],"],
    #       nasc_local_aldeia[", selecao_local1()[5],"],
    #       nasc_local_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)
    #       *100, 1)"), 2),
    #
    #     dist_parteira = rep(paste0("round(
    #       sum(c(
    #       nasc_assistido_parteira_hospital[", selecao_local1()[1],"],
    #       nasc_assistido_parteira_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_assistido_parteira_domicilio[", selecao_local1()[3],"],
    #       nasc_assistido_parteira_outros[", selecao_local1()[4],"],
    #       nasc_assistido_parteira_aldeia[", selecao_local1()[5],"],
    #       nasc_assistido_parteira_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)/
    #         sum(c(
    #       nasc_local_hospital[", selecao_local1()[1],"],
    #       nasc_local_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_local_domicilio[", selecao_local1()[3],"],
    #       nasc_local_outros[", selecao_local1()[4],"],
    #       nasc_local_aldeia[", selecao_local1()[5],"],
    #       nasc_local_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)
    #       *100, 1)"), 2),
    #
    #     dist_outros = rep(paste0("round(
    #       sum(c(
    #       nasc_assistido_outros_hospital[", selecao_local1()[1],"],
    #       nasc_assistido_outros_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_assistido_outros_domicilio[", selecao_local1()[3],"],
    #       nasc_assistido_outros_outros[", selecao_local1()[4],"],
    #       nasc_assistido_outros_aldeia[", selecao_local1()[5],"],
    #       nasc_assistido_outros_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)/
    #         sum(c(
    #       nasc_local_hospital[", selecao_local1()[1],"],
    #       nasc_local_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_local_domicilio[", selecao_local1()[3],"],
    #       nasc_local_outros[", selecao_local1()[4],"],
    #       nasc_local_aldeia[", selecao_local1()[5],"],
    #       nasc_local_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)
    #       *100, 1)"), 2),
    #
    #     dist_ignorado = rep(paste0("round(
    #       sum(c(
    #       nasc_assistido_ignorado_hospital[", selecao_local1()[1],"],
    #       nasc_assistido_ignorado_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_assistido_ignorado_domicilio[", selecao_local1()[3],"],
    #       nasc_assistido_ignorado_outros[", selecao_local1()[4],"],
    #       nasc_assistido_ignorado_aldeia[", selecao_local1()[5],"],
    #       nasc_assistido_ignorado_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)/
    #         sum(c(
    #       nasc_local_hospital[", selecao_local1()[1],"],
    #       nasc_local_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_local_domicilio[", selecao_local1()[3],"],
    #       nasc_local_outros[", selecao_local1()[4],"],
    #       nasc_local_aldeia[", selecao_local1()[5],"],
    #       nasc_local_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)
    #       *100, 1)"), 2),
    #
    #     dist_sem_inf = rep(paste0("round(
    #       sum(c(
    #       nasc_assistido_sem_inf_hospital[", selecao_local1()[1],"],
    #       nasc_assistido_sem_inf_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_assistido_sem_inf_domicilio[", selecao_local1()[3],"],
    #       nasc_assistido_sem_inf_outros[", selecao_local1()[4],"],
    #       nasc_assistido_sem_inf_aldeia[", selecao_local1()[5],"],
    #       nasc_assistido_sem_inf_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)/
    #         sum(c(
    #       nasc_local_hospital[", selecao_local1()[1],"],
    #       nasc_local_outros_est_saude[", selecao_local1()[2],"],
    #       nasc_local_domicilio[", selecao_local1()[3],"],
    #       nasc_local_outros[", selecao_local1()[4],"],
    #       nasc_local_aldeia[", selecao_local1()[5],"],
    #       nasc_local_sem_inf[", selecao_local1()[6],"]),
    #       na.rm=T)
    #       *100, 1)"), 2)
    #   )
    #
    # })


    #[LOOK]
    bloco4_calcs_resumo <- dplyr::full_join(bloco4_calcs, bloco4_deslocamento_calcs) |> #[xxx]
      dplyr::mutate(
        prop_partos_sem_4mais_uti = rep("round((sum(partos_na_macro_sem_4mais_uti) + sum(partos_fora_macro_sem_4mais_uti)) / (sum(partos_na_macro_com_4mais_uti) + sum(partos_na_macro_sem_4mais_uti) + sum(partos_fora_macro_com_4mais_uti) + sum(partos_fora_macro_sem_4mais_uti)) * 100, 1)", 2),
        percentil_5_partos_sem_uti = rep("round(quantile(((partos_na_macro_sem_4mais_uti + partos_fora_macro_sem_4mais_uti) / (partos_na_macro_com_4mais_uti + partos_na_macro_sem_4mais_uti + partos_fora_macro_com_4mais_uti + partos_fora_macro_sem_4mais_uti)) * 100, probs = 0.1, na.rm = T), 1)", 2),
        prop_partos_com_4mais_uti = rep("round((sum(partos_na_macro_com_4mais_uti) + sum(partos_fora_macro_com_4mais_uti)) / (sum(partos_na_macro_com_4mais_uti) + sum(partos_na_macro_sem_4mais_uti) + sum(partos_fora_macro_com_4mais_uti) + sum(partos_fora_macro_sem_4mais_uti)) * 100, 1)", 2),
        percentil_95_partos_com_uti = rep("round(quantile(((partos_na_macro_com_4mais_uti + partos_fora_macro_com_4mais_uti) / (partos_na_macro_com_4mais_uti + partos_na_macro_sem_4mais_uti + partos_fora_macro_com_4mais_uti + partos_fora_macro_sem_4mais_uti)) * 100, probs = 0.95, na.rm = T), 1)", 2),
        percentil_90_partos_com_uti = rep("round(quantile(((partos_na_macro_com_4mais_uti + partos_fora_macro_com_4mais_uti) / (partos_na_macro_com_4mais_uti + partos_na_macro_sem_4mais_uti + partos_fora_macro_com_4mais_uti + partos_fora_macro_sem_4mais_uti)) * 100, probs = 0.9, na.rm = T), 1)", 2)
      )



    # bloco4_calcs_resumo <- dplyr::full_join(bloco4_calcs, bloco4_deslocamento_calcs) |>
    #   dplyr::mutate(
    #     prop_partos_na_macro_com_4mais_uti = rep("round(sum(partos_na_macro_com_4mais_uti)/sum(nascimentos) * 100, 1)", 2),
    #     prop_partos_na_macro_sem_4mais_uti = rep("round(sum(partos_na_macro_sem_4mais_uti)/sum(nascimentos) * 100, 1)", 2),
    #     prop_partos_fora_macro_com_4mais_uti = rep("round(sum(partos_fora_macro_com_4mais_uti)/sum(nascimentos) * 100, 1)", 2),
    #     prop_partos_fora_macro_sem_4mais_uti = rep("round(sum(partos_fora_macro_sem_4mais_uti)/sum(nascimentos) * 100, 1)", 2),
    #     prop_partos_na_macro_sem_inf = rep("round(sum(partos_na_macro_sem_inf)/sum(nascimentos) * 100, 1)", 2),
    #     prop_partos_fora_macro_sem_inf = rep("round(sum(partos_fora_macro_sem_inf)/sum(nascimentos) * 100, 1)", 2),
    #     prop_partos_sem_4mais_uti = rep("round((sum(partos_na_macro_sem_4mais_uti) + sum(partos_fora_macro_sem_4mais_uti)) / (sum(partos_na_macro_com_4mais_uti) + sum(partos_na_macro_sem_4mais_uti) + sum(partos_fora_macro_com_4mais_uti) + sum(partos_fora_macro_sem_4mais_uti)) * 100, 1)", 2),
    #     prop_obitos_fetais_durante = rep("round(sum(fetal_durante) / sum(obitos_fetais_mais_22sem) * 100, 1)", 2),
    #     porc_obitos_fetais_evitaveis_parto = rep("round(sum(evitaveis_fetal_parto) / sum(obitos_fetais_totais) * 100, 1)", 2)
    #   )

    # Juntando as bases de deslocamento ---------------------------------------
    ## Isso deveria ter sido feito na parte de extração
    bloco4_deslocamento_muni <- dplyr::left_join(bloco4_deslocamento_muni, bloco4_deslocamento_macrorregiao) |>
      dplyr::arrange(desc(ano))

    bloco4_deslocamento_uf <- dplyr::left_join(
      # Algumas gambiarras por conta do gráfico de radar
      bloco4_deslocamento_uf,
      bloco4_deslocamento_macrorregiao |>
        dplyr::group_by(ano, uf) |>
        dplyr::summarise_at(dplyr::vars(dplyr::starts_with("partos") | dplyr::starts_with("nascimentos")), sum) |>
        dplyr::ungroup()
    )


    # Criando alguns outputs para a UI ----------------------------------------
    ## Criando o output que diz se há ou não comparação -----------------------
    output$comparar <- renderText({filtros()$comparar})
    outputOptions(output, "comparar", suspendWhenHidden = FALSE)

    ## Criando o output que diz o nível de análise selecionado ----------------
    output$nivel <- renderText({filtros()$nivel})
    outputOptions(output, "nivel", suspendWhenHidden = FALSE)

    ## Mostrando/ocultando partes de deslocamento da UI de acordo com o nível selecionado ----
    observeEvent(filtros()$nivel, {
      if (filtros()$nivel == "municipal" | (filtros()$comparar == "Sim" & filtros()$nivel2 == "municipal")) {
        shinyjs::show(id = "mostrar_card_indicadores_deslocamento_muni_uf", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "mostrar_card_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.8)
      } else if (filtros()$nivel == "estadual" | (filtros()$comparar == "Sim" & filtros()$nivel2 == "estadual")) {
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "mostrar_card_indicadores_deslocamento_muni_uf", anim = TRUE, animType = "slide", time = 0.8)
      } else {
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_muni_uf", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.001)
      }
    })

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

    ## Criando os outputs que receberão os nomes dos locais selecionados quando há comparação --------
    localidade_original <- reactive({
      dplyr::case_when(
        filtros()$nivel == "nacional" ~ "Brasil",
        filtros()$nivel == "regional" ~ filtros()$regiao,
        filtros()$nivel == "estadual" ~ filtros()$estado,
        filtros()$nivel == "macro" ~ filtros()$macro,
        filtros()$nivel == "micro" ~ filtros()$micro,
        filtros()$nivel == "municipal" ~ filtros()$municipio
      )
    })

    localidade_comparacao <- reactive({
      dplyr::case_when(
        filtros()$nivel2 == "nacional" ~ "Brasil",
        filtros()$nivel2 == "regional" ~ filtros()$regiao2,
        filtros()$nivel2 == "estadual" ~ filtros()$estado2,
        filtros()$nivel2 == "macro" ~ filtros()$macro2,
        filtros()$nivel2 == "micro" ~ filtros()$micro2,
        filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
        filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
      )
    })

    output$input_localidade_resumo1 <- renderUI({
      if (filtros()$comparar == "Sim") {
        radioButtons(
          inputId = ns("localidade_resumo1"),
          label = NULL,
          choiceNames = list(
            localidade_original(),
            localidade_comparacao()
          ),
          choiceValues = list("escolha1", "escolha2"),
          selected = "escolha1",
          inline = TRUE
        )
      }
    })

    output$input_localidade_resumo2 <- renderUI({
      if (filtros()$comparar == "Sim") {
        radioButtons(
          inputId = ns("localidade_resumo2"),
          label = NULL,
          choiceNames = list(
            localidade_original(),
            localidade_comparacao()
          ),
          choiceValues = list("escolha1", "escolha2"),
          selected = "escolha1",
          inline = TRUE
        )
      }
    })

    output$input_localidade_resumo3 <- renderUI({
      if (filtros()$comparar == "Sim") {
        radioButtons(
          inputId = ns("localidade_resumo3"),
          label = NULL,
          choiceNames = list(
            localidade_original(),
            localidade_comparacao()
          ),
          choiceValues = list("escolha1", "escolha2"),
          selected = "escolha1",
          inline = TRUE
        )
      }
    })

    output$input_localidade_resumo4 <- renderUI({
      if (filtros()$comparar == "Sim") {
        radioButtons(
          inputId = ns("localidade_resumo4"),
          label = NULL,
          choiceNames = list(
            localidade_original(),
            localidade_comparacao()
          ),
          choiceValues = list("escolha1", "escolha2"),
          selected = "escolha1",
          inline = TRUE
        )
      }
    })

    # Criando os pop-ups com a informação sobre o resumo do período -------------
    observeEvent(c(input$botao_resumo1, input$botao_resumo2, input$botao_resumo3), {
      shinyalert::shinyalert(
        html = TRUE,
        title = '<div class = "fonte-titulos-modal" style = "color: #656565"> Sobre o "Resumo do período" </div>',
        text = '
          <div style = "text-align: justify; text-justify: inter-word;">
            Todas as caixinhas que estão sob o "Resumo do período", na esquerda da página, referem-se aos valores dos indicadores calculados considerando todo o período selecionado.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando alguma comparação é feita, o usuário pode selecionar para qual localidade o resumo do período será calculado clicando em um dos botões que irão aparecer em cima das caixinhas.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Dentro desta aba, referente aos indicadores relacionados aos grupos de Robson, as caixinhas mudam de acordo com o indicador selecionado na caixa de seleção acima.
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
    },
    ignoreInit = TRUE
    )

    observeEvent(c(input$botao_resumo4), {
      shinyalert::shinyalert(
        html = TRUE,
        title = '<div class = "fonte-titulos-modal" style = "color: #656565"> Sobre o "Resumo do período" </div>',
        text = '
          <div style = "text-align: justify; text-justify: inter-word;">
            Todas as caixinhas que estão sob o "Resumo do período", na esquerda da página, referem-se aos valores dos indicadores calculados considerando todo o período selecionado.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando alguma comparação é feita, o usuário pode selecionar para qual localidade o resumo do período será calculado clicando em um dos botões que irão aparecer em cima das caixinhas.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando a comparação é feita com estados ou municípios, o gráfico contendo as medianas de deslocamento para o parto é atualizado de acordo com a localidade escolhida no resumo do período.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando a comparação é feita com municípios, a tabela contendo informações adicionais sobre os partos é atualizada de acordo com a localidade escolhida no resumo do período.
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
    },
    ignoreInit = TRUE
    )

    ## Para os botões de alerta quanto à incompletude e cobertura --------------
    ### Calculando os indicadores de incompletude ------------------------------
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
          parto_tprobson = round(sum(parto_tprobson_incompletos, na.rm = TRUE)/sum(parto_tprobson_totais, na.rm = TRUE) * 100, 1),
          tprobson = round(sum(tprobson_incompletos, na.rm = TRUE)/sum(tprobson_totais, na.rm = TRUE) * 100, 1),
          prop_cnes_nao_preenchido = round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_preenchido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 1),
          prop_cnes_invalido = round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_valido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 1),
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
    #### Porcentagem de cesarianas por grupo de Robson ------------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$parto_tprobson > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$parto_tprobson,
        variavel_incompletude1 = "PARTO e TPROBSON",
        descricao_incompletude1 = "em branco ou sem informação",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de nascidos vivos por grupo de Robson --------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$tprobson > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao2, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$tprobson,
        variavel_incompletude1 = "TPROBSON",
        descricao_incompletude1 = "ignorados ou em branco",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$parto_tprobson > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$parto_tprobson,
        variavel_incompletude1 = "PARTO e TPROBSON",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Indicadores de deslocamento ------------------------------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop3", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop4", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop5", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop6", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_mediana1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_mediana2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_infos", anim = TRUE, animType = "fade", time = 0.8)
      req(
        any(data_incompletude()$prop_cnes_nao_preenchido > 5, na.rm = TRUE) |
          any(data_incompletude()$prop_cnes_invalido > 5, na.rm = TRUE) |
          any(data_incompletude()$cobertura < 90, na.rm = TRUE)
      )
      shinyjs::show(id = "mostrar_botao_deslocamento_prop1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_prop2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_prop3", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_prop4", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_prop5", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_prop6", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_mediana1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_mediana2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_infos", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(c(input$botao_prop1, input$botao_prop2, input$botao_prop3,
                   input$botao_prop4, input$botao_prop5, input$botao_prop6,
                   input$botao_mediana1, input$botao_mediana2, input$botao_infos), {
                     cria_modal_incompletude(
                       incompletude1 = data_incompletude()$prop_cnes_nao_preenchido,
                       incompletude2 = data_incompletude()$prop_cnes_invalido,
                       df = data_incompletude(),
                       cobertura = data_incompletude()$cobertura,
                       bloco = "deslocamento"
                     )
                   })


    # Para o resumo do período ------------------------------------------------
    ## Calculando uma média dos indicadores para o período selecionado --------
    ### Para a localidade selecionada -----------------------------------------
    data4_resumo <- reactive({
      dplyr::left_join(bloco4, bloco4_deslocamento_muni) |>
        # dplyr::left_join(
        #   dplyr::left_join(bloco4, bloco4_deslocamento_muni),
        #   dplyr::left_join(
        #     bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
        #     bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        #   )
        # ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
            req(
              input$indicador_robson,
              get('input')[[glue::glue("localidade_resumo{substr(input$indicador_robson, start = nchar(input$indicador_robson), stop = nchar(input$indicador_robson))}")]]
            )
            if (get('input')[[glue::glue("localidade_resumo{substr(input$indicador_robson, start = nchar(input$indicador_robson), stop = nchar(input$indicador_robson))}")]] == "escolha1") {
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
        cria_indicadores(
          df_calcs = bloco4_calcs_resumo,
          filtros = filtros(),
          localidade_resumo = get('input')[[glue::glue("localidade_resumo{substr(input$indicador_robson, start = nchar(input$indicador_robson), stop = nchar(input$indicador_robson))}")]]
        )
    })

    data4_deslocamento_resumo <- reactive({
      dplyr::left_join(bloco4, bloco4_deslocamento_muni) |>
        # dplyr::left_join(
        #   dplyr::left_join(bloco4, bloco4_deslocamento_muni),
        #   dplyr::left_join(
        #     bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
        #     bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        #   )
        # ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
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
            req(input$localidade_resumo4)
            if (input$localidade_resumo4 == "escolha1") {
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
        cria_indicadores(
          df_calcs = bloco4_calcs_resumo,
          filtros = filtros(),
          localidade_resumo = input$localidade_resumo4
        )
    })

    data4_profissional_resumo <- reactive({
      bloco4_profissional |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
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
            req(input$localidade_resumo4)
            if (input$localidade_resumo4 == "escolha1") {
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
        ) |> # aqui
        cria_indicadores(
          df_calcs = bloco4_profissional_calcs,
          filtros = filtros(),
          localidade_resumo = input$localidade_resumo4
        )
    })

    ### Para a referência -----------------------------------------------------
    data4_resumo_referencia <- reactive({
      dplyr::left_join(bloco4, bloco4_deslocamento_muni) |>
        # dplyr::left_join(
        #   dplyr::left_join(bloco4, bloco4_deslocamento_muni),
        #   dplyr::left_join(
        #     bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
        #     bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        #   )
        # ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco4_calcs_resumo, filtros = filtros(), referencia = TRUE)
    })

    data4_deslocamento_resumo_referencia <- reactive({
      dplyr::left_join(bloco4, bloco4_deslocamento_muni) |>
        # dplyr::left_join(
        #   dplyr::left_join(bloco4, bloco4_deslocamento_muni),
        #   dplyr::left_join(
        #     bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
        #     bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        #   )
        # ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco4_calcs_resumo, filtros = filtros(), referencia = TRUE)
    })

    data4_profissional_resumo_referencia <- reactive({
      bloco4_profissional |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs, filtros = filtros(), referencia = TRUE)
    })


    ## Criando o output do gráfico de radar -----------------------------------
    ### Definindo os indicadores que aparecerão no gráfico
    selected_indicators <- c(
      "prop_tx_cesariana_geral",
      "prop_partos_fora_macro_rsaude_res",
      "prop_partos_sem_4mais_uti",
      "prop_partos_com_4mais_uti",
      "prop_obitos_fetais_durante",
      "porc_obitos_fetais_evitaveis_parto"
    )

    ## Selecionando colunas relevantes nos dataframes de resumo e arrumando seus formatos
    df <- reactive({
      if (grepl("Robson", input$tabset1)) {
        df_aux <- data4_resumo()
      } else {
        df_aux <- data4_deslocamento_resumo()
      }
      df_aux[, c('class', selected_indicators)] |>
        dplyr::mutate(
          class = ifelse(grepl("Brasil \\(valor de referência\\)", class), "Brasil", class)
        ) |>
        tidyr::pivot_longer(
          !class,
          names_to = "indicador",
          values_to = "values1"
        ) |>
        dplyr::mutate(
          sufixo = rep("%", 5)
        )
    })

    df2 <- reactive({
      if (grepl("Robson", input$tabset1)) {
        df2_aux <- data4_resumo_referencia()
      } else {
        df2_aux <- data4_deslocamento_resumo_referencia()
      }
      df2_aux[, selected_indicators] |>
        dplyr::mutate(class = "Referência") |>
        tidyr::pivot_longer(
          !class,
          names_to = "indicador",
          values_to = "values2"
        ) |>
        dplyr::mutate(
          tipo_de_referencia = lapply(
            selected_indicators,
            function(indicador_abrev) {
              if (indicador_abrev %in% c("prop_partos_sem_4mais_uti","prop_partos_com_4mais_uti", "prop_obitos_fetais_durante")) {
                "média nacional"
              } else {
                tabela_indicadores$descricao_referencia[tabela_indicadores$nome_abreviado == indicador_abrev]
              }
            }
          ) |> unlist(),
          sufixo = rep("%", 5)
        )
    })

    ### Criando o output
    output$spider_chart1 <- output$spider_chart2 <- output$spider_chart3 <- output$spider_chart4 <- output$spider_chart5 <- highcharter::renderHighchart({
      # Categorias para o eixo x
      categories <- lapply(
        selected_indicators,
        function(indicador_abrev) {
          if (indicador_abrev == "prop_obitos_fetais_durante") {
            "% de óbitos fetais ocorridos durante o parto"
          } else if (indicador_abrev == "prop_partos_sem_4mais_uti") {
            "% de nascidos vivos com peso ao nascer < 1500 g nascidos em serviço sem UTI neonatal"
          } else if (indicador_abrev == "prop_partos_com_4mais_uti") {
            "% de nascidos vivos com peso ao nascer < 1500 g nascidos em serviço com UTI neonatal"
          } else {
            gsub("Porcentagem", "%", tabela_radar$indicador[tabela_radar$nome_abreviado == indicador_abrev])
          }
        }
      ) |> unlist()

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
    ### Relacionadas à porcentagem de cesarianas por grupo de Robson ----------
    output$caixa_b4_i1_indicador1 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_tx_cesariana_geral",
        titulo = "Porcentagem global de cesarianas",
        tem_meta = TRUE,
        valor_de_referencia = 15,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "315px", "327px"),
        pagina = "bloco_4",
        tipo_referencia = "meta OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo1 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i2_indicador1 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_robson1_tx_cesariana",
        titulo = "Porcentagem de cesarianas no grupo 1 de Robson",
        tem_meta = TRUE,
        valor_de_referencia = 10,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "315px", "327px"),
        pagina = "bloco_4",
        tipo_referencia = "meta OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo1 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i3_indicador1 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_robson2_tx_cesariana",
        titulo = "Porcentagem de cesarianas no grupo 2 de Robson",
        tem_meta = TRUE,
        valor_de_referencia = c(20, 35),
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "315px", "327px"),
        pagina = "bloco_4",
        tipo_referencia = "meta OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo1 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i4_indicador1 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_robson3_tx_cesariana",
        titulo = "Porcentagem de cesarianas no grupo 3 de Robson",
        tem_meta = TRUE,
        valor_de_referencia = 3,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "315px", "327px"),
        pagina = "bloco_4",
        tipo_referencia = "meta OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo1 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i5_indicador1 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_robson4_tx_cesariana",
        titulo = "Porcentagem de cesarianas no grupo 4 de Robson",
        tem_meta = TRUE,
        valor_de_referencia = 15,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "315px", "327px"),
        pagina = "bloco_4",
        tipo_referencia = "meta OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo1 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i6_indicador1 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_robson5_tx_cesariana",
        titulo = "Porcentagem de cesarianas no grupo 5 de Robson",
        tem_meta = TRUE,
        valor_de_referencia = c(50, 60),
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "315px", "327px"),
        pagina = "bloco_4",
        tipo_referencia = "meta OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo1 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i7_indicador1 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_robson6_a_9_tx_cesariana",
        titulo = "Porcentagem de cesarianas nos grupos 6 a 9 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$prop_robson6_a_9_tx_cesariana,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "315px", "327px"),
        pagina = "bloco_4",
        tipo_referencia = "média nacional",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo1 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i8_indicador1 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_robson10_tx_cesariana",
        titulo = "Porcentagem de cesarianas no grupo 10 de Robson",
        tem_meta = TRUE,
        valor_de_referencia = 30,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "314px", "328px"),
        pagina = "bloco_4",
        tipo_referencia = "meta OMS",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo1 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    ### Relacionadas à porcentagem de nascidos vivos por grupo de Robson ------
    output$caixa_b4_i1_indicador2 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_nasc_robson1",
        titulo = "Porcentagem de nascidos vivos no grupo 1 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$prop_nasc_robson1,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo2 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i2_indicador2 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_nasc_robson2",
        titulo = "Porcentagem de nascidos vivos no grupo 2 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$prop_nasc_robson2,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo2 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i3_indicador2 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_nasc_robson3",
        titulo = "Porcentagem de nascidos vivos no grupo 3 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$prop_nasc_robson3,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo2 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i4_indicador2 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_nasc_robson4",
        titulo = "Porcentagem de nascidos vivos no grupo 4 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$prop_nasc_robson4,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo2 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i5_indicador2 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_nasc_robson5",
        titulo = "Porcentagem de nascidos vivos no grupo 5 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$prop_nasc_robson5,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo2 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i6_indicador2 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_nasc_robson6_a_9",
        titulo = "Porcentagem de nascidos vivos nos grupos 6 a 9 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$prop_nasc_robson6_a_9,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo2 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i7_indicador2 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_nasc_robson10",
        titulo = "Porcentagem de nascidos vivos no grupo 10 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$prop_nasc_robson10,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo2 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    ### Relacionadas à contribuição relativa de cada grupo de Robson para a taxa global de cesarianas ----
    output$caixa_b4_i1_indicador3 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "contrib_robson1_tx_cesariana",
        titulo = "Contribuição relativa do grupo 1 de Robson para a taxa global de cesarianas",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$contrib_robson1_tx_cesariana,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo3 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i2_indicador3 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "contrib_robson2_tx_cesariana",
        titulo = "Contribuição relativa do grupo 2 de Robson para a taxa global de cesarianas",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$contrib_robson2_tx_cesariana,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo3 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i3_indicador3 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "contrib_robson3_tx_cesariana",
        titulo = "Contribuição relativa do grupo 3 de Robson para a taxa global de cesarianas",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$contrib_robson3_tx_cesariana,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo3 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i4_indicador3 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "contrib_robson4_tx_cesariana",
        titulo = "Contribuição relativa do grupo 4 de Robson para a taxa global de cesarianas",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$contrib_robson4_tx_cesariana,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo3 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i5_indicador3 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "contrib_robson5_tx_cesariana",
        titulo = "Contribuição relativa do grupo 5 de Robson para a taxa global de cesarianas",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$contrib_robson5_tx_cesariana,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo3 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i6_indicador3 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "contrib_robson6_a_9_tx_cesariana",
        titulo = "Contribuição relativa dos grupos 6 a 9 de Robson para a taxa global de cesarianas",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$contrib_robson6_a_9_tx_cesariana,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo3 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i7_indicador3 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "contrib_robson10_tx_cesariana",
        titulo = "Contribuição relativa do grupo 10 de Robson para a taxa global de cesarianas",
        tem_meta = FALSE,
        valor_de_referencia = data4_resumo_referencia()$contrib_robson10_tx_cesariana,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        pagina = "bloco_4",
        #fonte_titulo = "15px",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo3 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    ### Relacionadas aos indicadores de deslocamento --------------------------
    output$caixa_b4_i1_deslocamento <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_municipio_res",
        titulo = "Porcentagem de partos ocorridos no município de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_municipio_res,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i2_deslocamento <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na região de saúde, mas fora do município de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i3_deslocamento <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_macro_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na macrorregião de saúde, mas fora da região de saúde de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_macro_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i4_deslocamento <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_fora_macro_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na UF, mas fora da macrorregião de saúde de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_fora_macro_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i5_deslocamento <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_fora_uf_res",
        titulo = "Porcentagem de partos ocorridos fora da UF de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_fora_uf_res,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i9_deslocamento_macro <- renderUI({
      tagList(
        div(
          style = "position: relative;",
          # Caixinha criada pela função cria_caixa_server
          cria_caixa_server(
            dados = data4_deslocamento_resumo(),
            indicador = "prop_partos_com_4mais_uti",
            titulo = "Porcentagem de nascidos vivos com peso <1500g ocorridos em serviço com quatro ou mais leitos de UTI neonatal",
            tem_meta = TRUE,
            valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_com_4mais_uti, # 16.3,
            tipo = "porcentagem",
            invertido = TRUE,
            tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
            #fonte_titulo = "15px",
            pagina = "bloco_4",
            tipo_referencia = "média nacional", #"HEALTHY PEOPLE, 2020",
            nivel_de_analise = ifelse(
              filtros()$comparar == "Não",
              filtros()$nivel,
              ifelse(
                input$localidade_resumo4 == "escolha1",
                filtros()$nivel,
                filtros()$nivel2
              )
            )
          ),
          # Botão de aviso posicionado no canto superior direito
          div(
            style = "position: absolute; top: 10px; right: 10px;",
            shinyWidgets::actionBttn(
              inputId = ns("aviso_desloc"),
              icon = icon("triangle-exclamation", style = "color: red"),
              color = "warning",
              style = "material-circle",
              size = "xs"
            )
          )
        )
      )
    })

    # output$caixa_b4_i10_deslocamento_macro <- renderUI({
    #   tagList(
    #     div(
    #       style = "position: relative;",
    #       # Caixinha criada pela função cria_caixa_server
    #       cria_caixa_server(
    #         dados = data4_deslocamento_resumo_referencia(),
    #         indicador = "percentil_95_partos_com_uti",
    #         titulo = "Percentil de 95 da distribuição da porcentagem de nascidos vivos com peso ao nascer < 1500 g nascidos em serviço com UTI neonatal",
    #         tem_meta = TRUE,
    #         valor_de_referencia = data4_deslocamento_resumo_referencia()$percentil_95_partos_com_uti, # 16.3,
    #         tipo = "porcentagem",
    #         invertido = TRUE,
    #         tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
    #         #fonte_titulo = "15px",
    #         pagina = "bloco_4",
    #         tipo_referencia = "média nacional", #"HEALTHY PEOPLE, 2020",
    #         nivel_de_analise = ifelse(
    #           filtros()$comparar == "Não",
    #           filtros()$nivel,
    #           ifelse(
    #             input$localidade_resumo4 == "escolha1",
    #             filtros()$nivel,
    #             filtros()$nivel2
    #           )
    #         )
    #       ),
    #       # Botão de aviso posicionado no canto superior direito
    #       div(
    #         style = "position: absolute; top: 10px; right: 10px;",
    #         shinyWidgets::actionBttn(
    #           inputId = ns("aviso_desloc"),
    #           icon = icon("triangle-exclamation", style = "color: red"),
    #           color = "warning",
    #           style = "material-circle",
    #           size = "xs"
    #         )
    #       )
    #     )
    #   )
    # })
    #
    # output$caixa_b4_i11_deslocamento_macro <- renderUI({
    #   tagList(
    #     div(
    #       style = "position: relative;",
    #       # Caixinha criada pela função cria_caixa_server
    #       cria_caixa_server(
    #         dados = data4_deslocamento_resumo_referencia(),
    #         indicador = "percentil_90_partos_com_uti",
    #         titulo = "Percentil de 90 da distribuição da porcentagem de nascidos vivos com peso ao nascer < 1500 g nascidos em serviço com UTI neonatal",
    #         tem_meta = TRUE,
    #         valor_de_referencia = data4_deslocamento_resumo_referencia()$percentil_90_partos_com_uti, # 16.3,
    #         tipo = "porcentagem",
    #         invertido = TRUE,
    #         tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
    #         #fonte_titulo = "15px",
    #         pagina = "bloco_4",
    #         tipo_referencia = "média nacional", #"HEALTHY PEOPLE, 2020",
    #         nivel_de_analise = ifelse(
    #           filtros()$comparar == "Não",
    #           filtros()$nivel,
    #           ifelse(
    #             input$localidade_resumo4 == "escolha1",
    #             filtros()$nivel,
    #             filtros()$nivel2
    #           )
    #         )
    #       ),
    #       # Botão de aviso posicionado no canto superior direito
    #       div(
    #         style = "position: absolute; top: 10px; right: 10px;",
    #         shinyWidgets::actionBttn(
    #           inputId = ns("aviso_desloc"),
    #           icon = icon("triangle-exclamation", style = "color: red"),
    #           color = "warning",
    #           style = "material-circle",
    #           size = "xs"
    #         )
    #       )
    #     )
    #   )
    # })

    # Exibe a mensagem quando o botão for clicado
    observeEvent(input$aviso_desloc, {
      shinyalert::shinyalert(
        title = "Atenção",
        text = "Esse indicador é condicional a ter nascido vivo.",
        type = "info"
      )
    })

    ### Relacionados aos indicadores de local de nascimento e profissionais ----

    output$caixa_b4_i1_profissional <- renderUI({
      cria_caixa_server(
        dados = data4_profissional_resumo(),
        indicador = "prop_nasc_local_fora_hospital",
        titulo = "Porcentagem de partos fora do hospital",
        tem_meta = FALSE,
        valor_de_referencia = data4_profissional_resumo_referencia()$prop_nasc_local_fora_hospital,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    ##TAGGG
    output$caixa_b4_i2_profissional <- renderUI({
      cria_caixa_server(
        dados = data4_profissional_resumo(),
        indicador = "prop_nasc_assistido_enf_obs",
        titulo = "Porcentagem de partos vaginais hospitalares assistidos por enfermeiras obstétricas",
        tem_meta = FALSE,
        valor_de_referencia = data4_profissional_resumo_referencia()$prop_nasc_assistido_enf_obs,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
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
    data4 <- reactive({
      validate(
        need(
          filtros()$ano2[2] >= 2014,
          "Os indicadores relacionados aos grupos de Robson só estão disponíveis a partir de 2014."
        )
      )
      bloco4 |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
        cria_indicadores(df_calcs = bloco4_calcs, filtros = filtros(), adicionar_localidade = FALSE) |>
        tidyr::pivot_longer(
          cols = starts_with("prop") | starts_with("contrib"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          ),
          tipo_indicador = as.factor(
            dplyr::case_when(
              stringr::str_detect(indicador, "^prop_nasc") ~ "indicador2",
              stringr::str_detect(indicador, "^prop_tx") | stringr::str_detect(indicador, "^prop_robson") ~ "indicador1",
              stringr::str_detect(indicador, "^contrib") ~ "indicador3"
            )
          ),
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_tx_cesariana_geral" ~ "Geral",
              grepl("faltante", indicador) ~ "Sem informação",
              grepl("robson1", indicador) & !grepl("robson10", indicador) ~ "Grupo 1 de Robson",
              grepl("robson2", indicador) ~ "Grupo 2 de Robson",
              grepl("robson3", indicador) ~ "Grupo 3 de Robson",
              grepl("robson4", indicador) ~ "Grupo 4 de Robson",
              grepl("robson5", indicador) ~ "Grupo 5 de Robson",
              grepl("robson6_a_9", indicador) ~ "Grupos 6 a 9 de Robson",
              grepl("robson10", indicador) ~ "Grupo 10 de Robson"
            ),
            levels = c(
              "Geral",
              "Grupo 1 de Robson",
              "Grupo 2 de Robson",
              "Grupo 3 de Robson",
              "Grupo 4 de Robson",
              "Grupo 5 de Robson",
              "Grupos 6 a 9 de Robson",
              "Grupo 10 de Robson",
              "Sem informação"
            )
          )
        )
    })

    data4_deslocamento_parto <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
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
        cria_indicadores(df_calcs = bloco4_deslocamento_calcs, filtros = filtros(), adicionar_localidade = TRUE) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_partos_municipio_res" ~ "No município de residência",
              indicador == "prop_partos_rsaude_res" ~ "Na região de saúde, mas fora do município de residência",
              indicador == "prop_partos_macro_rsaude_res" ~ "Na macrorregião de saúde, mas fora da região de saúde de residência",
              indicador == "prop_partos_fora_macro_rsaude_res" ~ "Fora da macrorregião de saúde, mas na UF de residência",
              indicador == "prop_partos_fora_uf_res" ~ "Fora da UF de residência"
            ),
            levels = c(
              "No município de residência", "Na região de saúde, mas fora do município de residência",
              "Na macrorregião de saúde, mas fora da região de saúde de residência",
              "Fora da macrorregião de saúde, mas na UF de residência",
              "Fora da UF de residência"
            )
          )
        )
    })

    data4_deslocamento_med <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo <- ""
      } else {
        req(input$localidade_resumo4)
        if (input$localidade_resumo4 == "escolha1") {
          sufixo <- ""
        } else {
          sufixo <- "2"
        }
      }

      validate(
        need(
          filtros()[[paste0("nivel", sufixo)]] %in% c("municipal", "estadual"),
          "Este indicador só está disponível para os níveis estadual e municipal."
        )
      )

      if (filtros()[[paste0("nivel", sufixo)]] == "municipal") {
        data_aux <- bloco4_deslocamento_muni
      } else {
        data_aux <- bloco4_deslocamento_uf
      }

      data_aux |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::filter(
          if (filtros()[[paste0("nivel", sufixo)]] == "estadual") {
            uf == filtros()[[paste0("estado", sufixo)]]
          } else if (filtros()[[paste0("nivel", sufixo)]] == "municipal") {
            municipio == filtros()[[paste0("municipio", sufixo)]] & uf == filtros()[[paste0("estado_municipio", sufixo)]]
          }
        ) |>
        dplyr::select(
          ano,
          no_local = glue::glue("km_partos_{input$local_med}"),
          baixa_complexidade = "km_partos_fora_municipio_baixa_complexidade",
          alta_complexidade = "km_partos_fora_municipio_alta_complexidade"
        ) |>
        dplyr::mutate(
          class = dplyr::case_when(
            filtros()[[paste0("nivel", sufixo)]] == "estadual" ~ filtros()[[paste0("estado", sufixo)]],
            filtros()[[paste0("nivel", sufixo)]] == "municipal" ~ filtros()[[paste0("municipio", sufixo)]]
          )
        )
    })


    data4_deslocamento_macro <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
          if (filtros()$nivel == "nacional")
            regiao %in% unique(tabela_aux_municipios$regiao)
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
          prop_partos_na_macro_com_4mais_uti = round(sum(partos_na_macro_com_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_4mais_uti = round(sum(partos_na_macro_sem_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_com_4mais_uti = round(sum(partos_fora_macro_com_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_4mais_uti = round(sum(partos_fora_macro_sem_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_inf = round(sum(partos_na_macro_sem_inf) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_inf = round(sum(partos_fora_macro_sem_inf) / sum(nascimentos) * 100, 1)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          indicador =
            factor(dplyr::case_when(
              indicador == "prop_partos_na_macro_com_4mais_uti" ~ "Na macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              indicador == "prop_partos_fora_macro_com_4mais_uti" ~ "Fora da macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              indicador == "prop_partos_na_macro_sem_4mais_uti" ~ "Na macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              indicador == "prop_partos_fora_macro_sem_4mais_uti" ~ "Fora da macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              indicador == "prop_partos_na_macro_sem_inf" ~ "Na macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal",
              indicador == "prop_partos_fora_macro_sem_inf" ~ "Fora da macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"
            ),
            levels = c(
              "Na macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              "Na macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              "Na macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"
            )
            ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    # [PROFF]

    data4_dist_local <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
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
        cria_indicadores(df_calcs = bloco4_profissional_calcs, filtros = filtros(), adicionar_localidade = TRUE) |>
        dplyr::select(!c(prop_nasc_local_fora_hospital,prop_nasc_assistido_enf_obs)) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_nasc_local_hospital" ~ "Hospital",
              indicador == "prop_nasc_local_outros_est_saude" ~ "Outros estabelecimentos de saúde",
              indicador == "prop_nasc_local_domicilio" ~ "Domicílio",
              indicador == "prop_nasc_local_outros" ~ "Outros",
              indicador == "prop_nasc_local_aldeia" ~ "Aldeia Indígena",
              indicador == "prop_nasc_local_sem_inf" ~ "Sem informação"
            ),
            levels = c(
              "Hospital",
              "Outros estabelecimentos de saúde",
              "Domicílio",
              "Aldeia Indígena",
              "Sem informação",
              "Outros"
            )
          )
        )
    })

    data4_dist_profissional <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
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
        # dplyr::select(ano, dplyr::contains("dist")) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs2, filtros = filtros(), adicionar_localidade = TRUE) |>
        tidyr::pivot_longer(
          cols = starts_with("dist"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
          indicador = factor(
            dplyr::case_when(
              indicador == "dist_medico" ~ "Médico",
              indicador == "dist_enf_obs" ~ "Enfermeira ou Obstetriz",
              indicador == "dist_parteira" ~ "Parteira",
              indicador == "dist_outros" ~ "Outros",
              indicador == "dist_ignorado" ~ "Ignorado",
              indicador == "dist_sem_inf" ~ "Sem informação"
            ),
            levels = c(
              "Enfermeira ou Obstetriz",
              "Médico",
              "Parteira",
              "Ignorado",
              "Sem informação",
              "Outros"
            )
          )
        )
    })

    # novo gráfico

    data4_dist_partos_vaginais <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
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
        #dplyr::select(ano, dplyr::contains("dist")) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs4, filtros = filtros(), adicionar_localidade = TRUE) |>
        tidyr::pivot_longer(
          cols = starts_with("dist"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
          indicador = factor(
            dplyr::case_when(
              indicador == "dist_partos_vaginais" ~ "Partos Vaginais",
              indicador == "dist_outros" ~ "Outros"
            ),
            levels = c(
              "Partos Vaginais",
              "Outros"
            )
          )
        )
    })

    ##marx

    # data4_total_profissional <- reactive({
    #   bloco4_profissional |>
    #     dplyr::filter(
    #       ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #     ) |>
    #     dplyr::filter(
    #       if (filtros()$nivel == "nacional")
    #         ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #       else if (filtros()$nivel == "regional")
    #         regiao == filtros()$regiao
    #       else if (filtros()$nivel == "estadual")
    #         uf == filtros()$estado
    #       else if (filtros()$nivel == "macro")
    #         macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
    #       else if(filtros()$nivel == "micro")
    #         r_saude == filtros()$micro & uf == filtros()$estado_micro
    #       else if(filtros()$nivel == "municipal")
    #         municipio == filtros()$municipio & uf == filtros()$estado_municipio
    #     ) |>
    #     dplyr::group_by(ano) |>
    #     # dplyr::select(ano, dplyr::contains("dist")) |>
    #     cria_indicadores(df_calcs = bloco4_profissional_calcs3, filtros = filtros(), adicionar_localidade = TRUE) |>
    #     tidyr::pivot_longer(
    #       cols = starts_with("dist"),
    #       names_to = "indicador",
    #       values_to = "total_indicador"
    #     ) |>
    #     dplyr::mutate(
    #       class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
    #       indicador = factor(
    #         dplyr::case_when(
    #           indicador == "dist_medico" ~ "Médico",
    #           indicador == "dist_enf_obs" ~ "Enfermeira ou Obstetriz",
    #           indicador == "dist_parteira" ~ "Parteira",
    #           indicador == "dist_outros" ~ "Outros",
    #           indicador == "dist_ignorado" ~ "Ignorado",
    #           indicador == "dist_sem_inf" ~ "Sem informação"
    #         ),
    #         levels = c(
    #           "Enfermeira ou Obstetriz",
    #           "Médico",
    #           "Parteira",
    #           "Ignorado",
    #           "Sem informação",
    #           "Outros"
    #         )
    #       )
    #     ) |>
    #     dplyr::select(-class)
    # })
    #
    # data4_total_profissional_comp <- reactive({
    #   bloco4_profissional |>
    #     dplyr::filter(
    #       ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #     ) |>
    #     dplyr::filter(
    #       if (filtros()$nivel2 == "nacional")
    #         ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #       else if (filtros()$nivel2 == "regional")
    #         regiao == filtros()$regiao2
    #       else if (filtros()$nivel2 == "estadual")
    #         uf == filtros()$estado2
    #       else if (filtros()$nivel2 == "macro")
    #         macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
    #       else if(filtros()$nivel2 == "micro")
    #         r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
    #       else if (filtros()$nivel2 == "municipal")
    #         municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
    #       else if (filtros()$nivel2 == "municipios_semelhantes")
    #         grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
    #     ) |>
    #     dplyr::group_by(ano) |>
    #     # dplyr::select(ano, dplyr::contains("dist")) |>
    #     cria_indicadores(df_calcs = bloco4_profissional_calcs3, filtros = filtros(), adicionar_localidade = TRUE) |>
    #     tidyr::pivot_longer(
    #       cols = starts_with("dist"),
    #       names_to = "indicador",
    #       values_to = "total_indicador"
    #     ) |>
    #     dplyr::mutate(
    #       class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
    #       indicador = factor(
    #         dplyr::case_when(
    #           indicador == "dist_medico" ~ "Médico",
    #           indicador == "dist_enf_obs" ~ "Enfermeira ou Obstetriz",
    #           indicador == "dist_parteira" ~ "Parteira",
    #           indicador == "dist_outros" ~ "Outros",
    #           indicador == "dist_ignorado" ~ "Ignorado",
    #           indicador == "dist_sem_inf" ~ "Sem informação"
    #         ),
    #         levels = c(
    #           "Enfermeira ou Obstetriz",
    #           "Médico",
    #           "Parteira",
    #           "Ignorado",
    #           "Sem informação",
    #           "Outros"
    #         )
    #       )
    #     ) |>
    #     dplyr::select(-class)
    # })
    #
    # data4_total_profissional_referencia <- reactive({
    #   names(bloco4_profissional)[startsWith(names(bloco4_profissional), "dist_")]
    #   bloco4_profissional |>
    #     dplyr::filter(
    #       ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #     ) |>
    #     dplyr::group_by(ano) |>
    #
    #     # dplyr::select(ano, dplyr::contains("dist")) |>
    #     cria_indicadores(df_calcs = bloco4_profissional_calcs3, filtros = filtros(), adicionar_localidade = TRUE) |>
    #     tidyr::pivot_longer(
    #       cols = starts_with("dist"),
    #       names_to = "indicador",
    #       values_to = "total_indicador_br"
    #     ) |>
    #     dplyr::mutate(
    #       class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
    #       indicador = factor(
    #         dplyr::case_when(
    #           indicador == "dist_medico" ~ "Médico",
    #           indicador == "dist_enf_obs" ~ "Enfermeira ou Obstetriz",
    #           indicador == "dist_parteira" ~ "Parteira",
    #           indicador == "dist_outros" ~ "Outros",
    #           indicador == "dist_ignorado" ~ "Ignorado",
    #           indicador == "dist_sem_inf" ~ "Sem informação"
    #         ),
    #         levels = c(
    #           "Enfermeira ou Obstetriz",
    #           "Médico",
    #           "Parteira",
    #           "Ignorado",
    #           "Sem informação",
    #           "Outros"
    #         )
    #       )
    #     ) |>
    #     dplyr::select(-class)
    # })


    ### Para a comparação selecionada -----------------------------------------
    data4_comp <- reactive({
      bloco4 |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "nacional")
            ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco4_calcs, filtros = filtros(), comp = TRUE, adicionar_localidade = FALSE) |>
        tidyr::pivot_longer(
          cols = starts_with("prop") | starts_with("contrib"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          localidade = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          ),
          tipo_indicador = as.factor(
            dplyr::case_when(
              stringr::str_detect(indicador, "^prop_nasc") ~ "indicador2",
              stringr::str_detect(indicador, "^prop_tx") | stringr::str_detect(indicador, "^prop_robson") ~ "indicador1",
              stringr::str_detect(indicador, "^contrib") ~ "indicador3"
            )
          ),
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_tx_cesariana_geral" ~ "Geral",
              grepl("faltante", indicador) ~ "Sem informação",
              grepl("robson1", indicador) & !grepl("robson10", indicador) ~ "Grupo 1 de Robson",
              grepl("robson2", indicador) ~ "Grupo 2 de Robson",
              grepl("robson3", indicador) ~ "Grupo 3 de Robson",
              grepl("robson4", indicador) ~ "Grupo 4 de Robson",
              grepl("robson5", indicador) ~ "Grupo 5 de Robson",
              grepl("robson6_a_9", indicador) ~ "Grupos 6 a 9 de Robson",
              grepl("robson10", indicador) ~ "Grupo 10 de Robson"
            ),
            levels = c(
              "Geral",
              "Grupo 1 de Robson",
              "Grupo 2 de Robson",
              "Grupo 3 de Robson",
              "Grupo 4 de Robson",
              "Grupo 5 de Robson",
              "Grupos 6 a 9 de Robson",
              "Grupo 10 de Robson",
              "Sem informação"
            )
          )
        )

    })

    data4_deslocamento_parto_comp <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::filter(
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
          else if (filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco4_deslocamento_calcs, filtros = filtros(), comp = TRUE, adicionar_localidade = TRUE) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_partos_municipio_res" ~ "No município de residência",
              indicador == "prop_partos_rsaude_res" ~ "Na região de saúde, mas fora do município de residência",
              indicador == "prop_partos_macro_rsaude_res" ~ "Na macrorregião de saúde, mas fora da região de saúde de residência",
              indicador == "prop_partos_fora_macro_rsaude_res" ~ "Fora da macrorregião de saúde, mas na UF de residência",
              indicador == "prop_partos_fora_uf_res" ~ "Fora da UF de residência"
            ),
            levels = c(
              "No município de residência", "Na região de saúde, mas fora do município de residência",
              "Na macrorregião de saúde, mas fora da região de saúde de residência",
              "Fora da macrorregião de saúde, mas na UF de residência",
              "Fora da UF de residência"
            )
          )
        )
    })

    data4_deslocamento_macro_comp <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
          if (filtros()$nivel2 == "nacional")
            regiao %in% unique(tabela_aux_municipios$regiao)
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
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          prop_partos_na_macro_com_4mais_uti = round(sum(partos_na_macro_com_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_4mais_uti = round(sum(partos_na_macro_sem_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_com_4mais_uti = round(sum(partos_fora_macro_com_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_4mais_uti = round(sum(partos_fora_macro_sem_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_inf = round(sum(partos_na_macro_sem_inf) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_inf = round(sum(partos_fora_macro_sem_inf) / sum(nascimentos) * 100, 1)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          indicador =
            factor(dplyr::case_when(
              indicador == "prop_partos_na_macro_com_4mais_uti" ~ "Na macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              indicador == "prop_partos_fora_macro_com_4mais_uti" ~ "Fora da macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              indicador == "prop_partos_na_macro_sem_4mais_uti" ~ "Na macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              indicador == "prop_partos_fora_macro_sem_4mais_uti" ~ "Fora da macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              indicador == "prop_partos_na_macro_sem_inf" ~ "Na macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal",
              indicador == "prop_partos_fora_macro_sem_inf" ~ "Fora da macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"
            ),
            levels = c(
              "Na macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              "Na macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              "Na macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"
            )
            ),
          class = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        )
    })

    # [PROFF]

    data4_dist_local_comp <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::filter(
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
          else if (filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs, filtros = filtros(), comp = TRUE, adicionar_localidade = TRUE) |>
        dplyr::select(!c(prop_nasc_local_fora_hospital,prop_nasc_assistido_enf_obs)) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_nasc_local_hospital" ~ "Hospital",
              indicador == "prop_nasc_local_outros_est_saude" ~ "Outros estabelecimentos de saúde",
              indicador == "prop_nasc_local_domicilio" ~ "Domicílio",
              indicador == "prop_nasc_local_outros" ~ "Outros",
              indicador == "prop_nasc_local_aldeia" ~ "Aldeia Indígena",
              indicador == "prop_nasc_local_sem_inf" ~ "Sem informação"
            ),
            levels = c(
              "Hospital",
              "Outros estabelecimentos de saúde",
              "Domicílio",
              "Aldeia Indígena",
              "Sem informação",
              "Outros"
            )
          )
        )
    })

    data4_dist_profissional_comp <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::filter(
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
          else if (filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        # dplyr::select(ano, dplyr::contains("dist")) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs2, filtros = filtros(), comp = TRUE, adicionar_localidade = TRUE) |>
        tidyr::pivot_longer(
          cols = starts_with("dist"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
          indicador = factor(
            dplyr::case_when(
              indicador == "dist_medico" ~ "Médico",
              indicador == "dist_enf_obs" ~ "Enfermeira ou Obstetriz",
              indicador == "dist_parteira" ~ "Parteira",
              indicador == "dist_outros" ~ "Outros",
              indicador == "dist_ignorado" ~ "Ignorado",
              indicador == "dist_sem_inf" ~ "Sem informação"
            ),
            levels = c(
              "Enfermeira ou Obstetriz",
              "Médico",
              "Parteira",
              "Ignorado",
              "Sem informação",
              "Outros"
            )
          )
        )
    })

    data4_dist_partos_vaginais_comp <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::filter(
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
          else if (filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        # dplyr::select(ano, dplyr::contains("dist")) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs4, filtros = filtros(), comp = TRUE, adicionar_localidade = TRUE) |>
        tidyr::pivot_longer(
          cols = starts_with("dist"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
          indicador = factor(
            dplyr::case_when(
              indicador == "dist_partos_vaginais" ~ "Partos Vaginais",
              indicador == "dist_outros" ~ "Outros"
              ),
            levels = c(
              "Partos Vaginais",
              "Outros"
            )
          )
        )
    })

    ### Para a referência -----------------------------------------------------
    data4_referencia <- reactive({
      bloco4 |>
        dplyr::filter(ano >= max(filtros()$ano2[1], 2014) & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco4_calcs, filtros = filtros(), adicionar_localidade = FALSE, referencia = TRUE) |>
        tidyr::pivot_longer(
          cols = starts_with("prop") | starts_with("contrib") | starts_with("percentil"),
          names_to = "indicador",
          values_to = "br_prop_indicador"
        ) |>
        #[REFF]
        dplyr::mutate(
          localidade_comparacao = "Média nacional",
          tipo_indicador = as.factor(
            dplyr::case_when(
              stringr::str_detect(indicador, "^prop_nasc") ~ "indicador2",
              stringr::str_detect(indicador, "^prop_tx") | stringr::str_detect(indicador, "^prop_robson") ~ "indicador1",
              stringr::str_detect(indicador, "^contrib") ~ "indicador3",
              stringr::str_detect(indicador, "^percentil_95") ~ "indicador4",
              stringr::str_detect(indicador, "^percentil_5") ~ "indicador5"
            )
          ),
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_tx_cesariana_geral" ~ "Geral",
              grepl("faltante", indicador) ~ "Sem informação",
              grepl("robson1", indicador) & !grepl("robson10", indicador) ~ "Grupo 1 de Robson",
              grepl("robson2", indicador) ~ "Grupo 2 de Robson",
              grepl("robson3", indicador) ~ "Grupo 3 de Robson",
              grepl("robson4", indicador) ~ "Grupo 4 de Robson",
              grepl("robson5", indicador) ~ "Grupo 5 de Robson",
              grepl("robson6_a_9", indicador) ~ "Grupos 6 a 9 de Robson",
              grepl("robson10", indicador) ~ "Grupo 10 de Robson",
            ),
            levels = c(
              "Geral",
              "Grupo 1 de Robson",
              "Grupo 2 de Robson",
              "Grupo 3 de Robson",
              "Grupo 4 de Robson",
              "Grupo 5 de Robson",
              "Grupos 6 a 9 de Robson",
              "Grupo 10 de Robson",
              "Sem informação"
            )
          )
        )
    })

    data4_completo <- reactive({
      dplyr::full_join(data4(), data4_referencia())
    })

    data4_comp_completo <- reactive({
      dplyr::full_join(data4_comp(), data4_referencia())
    })


    data4_deslocamento_parto_referencia <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco4_deslocamento_calcs, filtros = filtros(), referencia = TRUE, adicionar_localidade = FALSE) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "br_prop_indicador"
        ) |>
        dplyr::mutate(
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_partos_municipio_res" ~ "No município de residência",
              indicador == "prop_partos_rsaude_res" ~ "Na região de saúde, mas fora do município de residência",
              indicador == "prop_partos_macro_rsaude_res" ~ "Na macrorregião de saúde, mas fora da região de saúde de residência",
              indicador == "prop_partos_fora_macro_rsaude_res" ~ "Fora da macrorregião de saúde, mas na UF de residência",
              indicador == "prop_partos_fora_uf_res" ~ "Fora da UF de residência"
            ),
            levels = c(
              "No município de residência", "Na região de saúde, mas fora do município de residência",
              "Na macrorregião de saúde, mas fora da região de saúde de residência",
              "Fora da macrorregião de saúde, mas na UF de residência",
              "Fora da UF de residência"
            )
          )
        )
    })

    data4_deslocamento_parto_completo <- reactive({
      dplyr::full_join(data4_deslocamento_parto(), data4_deslocamento_parto_referencia())
    })

    data4_deslocamento_parto_comp_completo <- reactive({
      dplyr::full_join(data4_deslocamento_parto_comp(), data4_deslocamento_parto_referencia())
    })


    data4_deslocamento_macro_referencia <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          prop_partos_na_macro_com_4mais_uti = round(sum(partos_na_macro_com_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_4mais_uti = round(sum(partos_na_macro_sem_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_com_4mais_uti = round(sum(partos_fora_macro_com_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_4mais_uti = round(sum(partos_fora_macro_sem_4mais_uti) / sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_inf = round(sum(partos_na_macro_sem_inf) / sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_inf = round(sum(partos_fora_macro_sem_inf) / sum(nascimentos) * 100, 1)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "br_prop_indicador"
        ) |>
        dplyr::mutate(
          indicador =
            factor(dplyr::case_when(
              indicador == "prop_partos_na_macro_com_4mais_uti" ~ "Na macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              indicador == "prop_partos_fora_macro_com_4mais_uti" ~ "Fora da macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              indicador == "prop_partos_na_macro_sem_4mais_uti" ~ "Na macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              indicador == "prop_partos_fora_macro_sem_4mais_uti" ~ "Fora da macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              indicador == "prop_partos_na_macro_sem_inf" ~ "Na macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal",
              indicador == "prop_partos_fora_macro_sem_inf" ~ "Fora da macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"
            ),
            levels = c(
              "Na macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal",
              "Na macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal",
              "Na macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal",
              "Fora da macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"
            )
            )
        )
    })

    data4_deslocamento_macro_completo <- reactive({
      dplyr::full_join(data4_deslocamento_macro(), data4_deslocamento_macro_referencia())
    })

    data4_deslocamento_macro_comp_completo <- reactive({
      dplyr::full_join(data4_deslocamento_macro_comp(), data4_deslocamento_macro_referencia())
    })

    # [PROFF]

    data4_dist_local_referencia <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs, filtros = filtros(), referencia = TRUE,
                         adicionar_localidade = FALSE) |>
        dplyr::select(!c(prop_nasc_local_fora_hospital,prop_nasc_assistido_enf_obs)) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "br_prop_indicador"
        ) |>
        dplyr::mutate(
          indicador = factor(
            dplyr::case_when(
              indicador == "prop_nasc_local_hospital" ~ "Hospital",
              indicador == "prop_nasc_local_outros_est_saude" ~ "Outros estabelecimentos de saúde",
              indicador == "prop_nasc_local_domicilio" ~ "Domicílio",
              indicador == "prop_nasc_local_outros" ~ "Outros",
              indicador == "prop_nasc_local_aldeia" ~ "Aldeia Indígena",
              indicador == "prop_nasc_local_sem_inf" ~ "Sem informação"
            ),
            levels = c(
              "Hospital",
              "Outros estabelecimentos de saúde",
              "Domicílio",
              "Aldeia Indígena",
              "Sem informação",
              "Outros"
            )
          )
        )
    })

    data4_dist_local_completo <- reactive({
      dplyr::full_join(data4_dist_local(), data4_dist_local_referencia())
    })

    data4_dist_local_comp_completo <- reactive({
      dplyr::full_join(data4_dist_local_comp(), data4_dist_local_referencia())
    })

    data4_dist_profissional_referencia <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        # dplyr::select(ano, dplyr::contains("dist")) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs2, filtros = filtros(), referencia = TRUE,
                         adicionar_localidade = FALSE) |>
        tidyr::pivot_longer(
          cols = starts_with("dist"),
          names_to = "indicador",
          values_to = "br_prop_indicador"
        ) |>
        dplyr::mutate(
          indicador = factor(
            dplyr::case_when(
              indicador == "dist_medico" ~ "Médico",
              indicador == "dist_enf_obs" ~ "Enfermeira ou Obstetriz",
              indicador == "dist_parteira" ~ "Parteira",
              indicador == "dist_outros" ~ "Outros",
              indicador == "dist_ignorado" ~ "Ignorado",
              indicador == "dist_sem_inf" ~ "Sem informação"
            ),
            levels = c(
              "Enfermeira ou Obstetriz",
              "Médico",
              "Parteira",
              "Ignorado",
              "Sem informação",
              "Outros"
            )
          )
        )
    })

    data4_dist_profissional_completo <- reactive({
      dplyr::full_join(data4_dist_profissional(), data4_dist_profissional_referencia())
        # dplyr::left_join(data4_total_profissional(), by = c("ano", "indicador")) |>
        # dplyr::left_join(data4_total_profissional_referencia(), by = c("ano", "indicador"))
    })

    data4_dist_profissional_comp_completo <- reactive({
      dplyr::full_join(data4_dist_profissional_comp(), data4_dist_profissional_referencia())
        # dplyr::left_join(data4_total_profissional_comp(), by = c("ano", "indicador")) |>
        # dplyr::left_join(data4_total_profissional_referencia(), by = c("ano", "indicador"))

    })

    data4_dist_partos_vaginais_referencia <- reactive({
      bloco4_profissional |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        # dplyr::select(ano, dplyr::contains("dist")) |>
        cria_indicadores(df_calcs = bloco4_profissional_calcs4, filtros = filtros(), referencia = TRUE,
                         adicionar_localidade = FALSE) |>
        tidyr::pivot_longer(
          cols = starts_with("dist"),
          names_to = "indicador",
          values_to = "br_prop_indicador"
        ) |>
        dplyr::mutate(
          indicador = factor(
            dplyr::case_when(
              indicador == "dist_partos_vaginais" ~ "Partos Vaginais",
              indicador == "dist_outros" ~ "Outros"
            ),
            levels = c(
              "Partos Vaginais",
              "Outros"
            )
          )
        )
    })

    data4_dist_partos_vaginais_completo <- reactive({
      dplyr::full_join(data4_dist_partos_vaginais(), data4_dist_partos_vaginais_referencia())
    })

    data4_dist_partos_vaginais_comp_completo <- reactive({
      dplyr::full_join(data4_dist_partos_vaginais_comp(), data4_dist_partos_vaginais_referencia())

    })



    ## Criando os outputs dos gráficos ----------------------------------------
    ### Porcentagem de cesarianas por grupo de Robson -------------------------
    list_of_plots <- reactive({
      purrr::map(as.vector(unique(data4()$indicador[which(data4()$tipo_indicador == "indicador1")])), function(x) {

        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4() |> dplyr::filter(indicador == x, tipo_indicador == "indicador1"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = localidade),
            type = "column",
            color = c("#2c115f"),
            showInLegend = TRUE
          ) |>
          highcharter::hc_yAxis(min = 0, max = 100)

        if (filtros()$comparar == "Sim") {
          grafico_base |>
            highcharter::hc_add_series(
              data = data4_comp() |> dplyr::filter(indicador == x, tipo_indicador == "indicador1"),
              highcharter::hcaes(x = ano, y = prop_indicador, group = localidade),
              type = "column",
              color = c("#b73779"),
              showInLegend = TRUE
            ) |>
            highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
            highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
            highcharter::hc_yAxis(title = list(text = "% de nascidos vivos por cesariana"), min = 0, max = 100)
        } else {
          grafico_base
        }

      })
    })

    #### Criando os gráficos
    output$plot1_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[1]] |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("Geral", indicador)),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = 15, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("Geral", indicador)),
          type = "line",
          name = "Referência (meta ajustada para o Brasil)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = 25, group = localidade_comparacao),
          color = "#fc8961",
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    output$plot2_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[2]] |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("1 de Robson", indicador)),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_indicador, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    output$plot3_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[3]] |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("2 de Robson", indicador)),
          name = "Referência (meta OMS)",
          highcharter::hcaes(x = ano, low = 20, high = 35),
          type = "arearange",
          dashStyle = "ShortDot",
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          fillOpacity = 0.2,
          enableMouseTracking = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    output$plot4_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[4]] |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("3 de Robson", indicador)),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_indicador, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    output$plot5_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[5]] |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("4 de Robson", indicador)),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_indicador, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    output$plot6_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[6]] |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("5 de Robson", indicador)),
          name = "Referência (meta OMS)",
          highcharter::hcaes(x = ano, low = 50, high = 60),
          type = "arearange",
          dashStyle = "ShortDot",
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          fillOpacity = 0.2,
          enableMouseTracking = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    #[REFF]
    output$plot7_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[7]] |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("6 a 9 de Robson", indicador)),
          type = "line",
          name = "Referência (média nacional)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_indicador, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador4", grepl("6 a 9 de Robson", indicador)),
          type = "line",
          name = "Referência (Percentil 95)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_indicador, group = localidade_comparacao),
          color = "#fc8961",
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador5", grepl("6 a 9 de Robson", indicador)),
          type = "line",
          name = "Referência (Percentil 5)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_indicador, group = localidade_comparacao),
          # color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    output$plot8_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[8]] |>
        highcharter::hc_add_series(
          data = data4_referencia() |> dplyr::filter(tipo_indicador == "indicador1", grepl("10 de Robson", indicador)),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_indicador, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    #### Ativando os pop-ups com a descrição de cada grupo de Robson
    observeEvent(input$texto_robson1, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal' style = 'color: #656565'> Sobre o grupo 1 de Robson </div>",
        text = "
          <div style = 'text-align: justify; text-justify: inter-word;'>
            O grupo 1 de Robson é formado por nulíparas com gestação única, cefálica, \U2265 37 semanas e em trabalho de parto espontâneo.
          </div>",
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

    observeEvent(input$texto_robson2, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal' style = 'color: #656565'> Sobre o grupo 2 de Robson </div>",
        text = "
          <div style = 'text-align: justify; text-justify: inter-word;'>
            O grupo 2 de Robson é formado por nulíparas com gestação única, cefálica, \U2265 37 semanas, com indução ou cesárea anterior ao trabalho de parto.
          </div>",
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

    observeEvent(input$texto_robson3, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal' style = 'color: #656565'> Sobre o grupo 3 de Robson </div>",
        text = "
          <div style = 'text-align: justify; text-justify: inter-word;'>
            O grupo 3 de Robson é formado por multíparas sem antecedente de cesárea, com gestação única, cefálica, \U2265 37 semanas e em trabalho de parto espontâneo.
          </div>",
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

    observeEvent(input$texto_robson4, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal' style = 'color: #656565'> Sobre o grupo 4 de Robson </div>",
        text = "
          <div style = 'text-align: justify; text-justify: inter-word;'>
            O grupo 4 de Robson é formado por multíparas sem antecedente de cesárea, com gestação única, cefálica, \U2265 37 semanas, com indução ou cesárea realizada antes do início do trabalho de parto.
          </div>",
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

    observeEvent(input$texto_robson5, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal' style = 'color: #656565'> Sobre o grupo 5 de Robson </div>",
        text = "
          <div style = 'text-align: justify; text-justify: inter-word;'>
            O grupo 5 de Robson é formado por todas as multíparas com antecedente de cesárea, gestação única, cefálica e \U2265 37 semanas.
          </div>",
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

    observeEvent(input$texto_robson6_a_9, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal' style = 'color: #656565'> Sobre os grupos 6 a 9 de Robson </div>",
        text = "
          <div style = 'text-align: justify; text-justify: inter-word;'>
            O grupo 6 de Robson é formado por todas as nulíparas com partos pélvicos.
            <br>
            <br>
            O grupo 7 de Robson é formado por todas as multíparas com partos pélvicos (incluindo aquelas com antecedente de cesárea).
            <br>
            <br>
            O grupo 8 de Robson é formado por todas as mulheres com gestação múltipla (incluindo aquelas com antecedente de cesárea).
            <br>
            <br>
            O grupo 9 de Robson é formado por todas as gestantes com feto em apresentações anormais (incluindo aquelas com antecedente de cesárea).
          </div>",
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

    observeEvent(input$texto_robson10, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal' style = 'color: #656565'> Sobre o grupo 10 de Robson </div>",
        text = "
          <div style = 'text-align: justify; text-justify: inter-word;'>
            O grupo 10 de Robson é formado por todas as mulheres com gestação única, cefálica e < 37 semanas (incluindo aquelas com antecedente de cesárea).
          </div>",
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



    ### Porcentagem de nascidos vivos por grupo de Robson --------------------
    output$plot1_indicador2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador2"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            )) |>
          highcharter::hc_colors(viridis::magma(10, direction = -1)[-c(1, 10)])
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 1 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#FEC98DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_comp_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 1 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#FEC98DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 2 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#FD9567FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_comp_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 2 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#FD9567FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 3 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_comp_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 3 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 4 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#CD4071FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_comp_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 4 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#CD4071FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 5 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#9F2F7FFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_comp_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupo 5 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#9F2F7FFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupos 6 a 9 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#721F81FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_comp_completo() |> dplyr::filter(tipo_indicador == "indicador2" & indicador == "Grupos 6 a 9 de Robson"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#721F81FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data4_completo()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)
    })


    ### Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas ----
    output$plot1_indicador3 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador3"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            )
          )
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_completo() |> dplyr::filter(tipo_indicador == "indicador3"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_comp_completo() |> dplyr::filter(tipo_indicador == "indicador3"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.localidade})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(10, direction = -1)[-c(1, 10)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data4_completo()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)
    })

    ### Porcentagem de nascidos vivos segundo local de ocorrência do parto ----
    output$grafico_deslocamento_prop <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_completo(),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            )
          ) |>
          highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)])
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_completo() |> dplyr::filter(indicador == "No município de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#FEAF77FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_comp_completo() |> dplyr::filter(indicador == "No município de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#FEAF77FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_completo() |> dplyr::filter(indicador == "Na região de saúde, mas fora do município de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_comp_completo() |> dplyr::filter(indicador == "Na região de saúde, mas fora do município de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_completo() |> dplyr::filter(indicador == "Na macrorregião de saúde, mas fora da região de saúde de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#B63679FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_comp_completo() |> dplyr::filter(indicador == "Na macrorregião de saúde, mas fora da região de saúde de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#B63679FF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_completo() |> dplyr::filter(indicador == "Fora da macrorregião de saúde, mas na UF de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#721F81FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_comp_completo() |> dplyr::filter(indicador == "Fora da macrorregião de saúde, mas na UF de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#721F81FF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_completo() |> dplyr::filter(indicador == "Fora da UF de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#2D1160FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_parto_comp_completo() |> dplyr::filter(indicador == "Fora da UF de residência"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#2D1160FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(column = list(stacking = "percent")) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)
    })

    ### Porcentagem de nascidos vivos com peso < 1500g segundo região de ocorrência do parto ----
    output$grafico_deslocamento_macrorregiao <- highcharter::renderHighchart({ # [xxx]
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_completo(),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            )
          ) |>
          highcharter::hc_colors(viridis::magma(8, direction = -1)[-c(1, 8)])
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_completo() |> dplyr::filter(indicador == "Na macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#FEBA80FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_comp_completo() |> dplyr::filter(indicador == "Na macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#FEBA80FF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_completo() |> dplyr::filter(indicador == "Fora da macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#F8765CFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_comp_completo() |> dplyr::filter(indicador == "Fora da macrorregião de saúde e em serviço com quatro ou mais leitos de UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#F8765CFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_completo() |> dplyr::filter(indicador == "Na macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#D3436EFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_comp_completo() |> dplyr::filter(indicador == "Na macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#D3436EFF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_completo() |> dplyr::filter(indicador == "Fora da macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#982D80FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_comp_completo() |> dplyr::filter(indicador == "Fora da macrorregião de saúde e em serviço com menos de quatro leitos de UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#982D80FF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_completo() |> dplyr::filter(indicador == "Na macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#5F187FFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_comp_completo() |> dplyr::filter(indicador == "Na macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#5F187FFF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_completo() |> dplyr::filter(indicador == "Fora da macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#231151FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_comp_completo() |> dplyr::filter(indicador == "Fora da macrorregião de saúde e em serviço com informação ignorada sobre UTI neonatal"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#231151FF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data4_deslocamento_macro_completo()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos com peso < 1500g"), min = 0, max = 100)
    })

    ### Mediana de deslocamento para o destino, segundo destinos --------------
    output$grafico_deslocamento_med <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          name = ifelse(filtros()$comparar == "Não", "Total de partos", glue::glue("Total de partos <b>({data4_deslocamento_med()$class[1]})</b>")),
          data = data4_deslocamento_med(),
          type = "line",
          highcharter::hcaes(x = ano, y = no_local),
          legendIndex = 1,
          index = 1
        ) |>
        highcharter::hc_add_series(
          name = ifelse(filtros()$comparar == "Não", "Serviços de baixa complexidade", glue::glue("Serviços de baixa complexidade <b>({data4_deslocamento_med()$class[1]})</b>")),
          data = data4_deslocamento_med(),
          type = "line",
          highcharter::hcaes(x = ano, y = baixa_complexidade),
          legendIndex = 2,
          index = 2
        ) |>
        highcharter::hc_add_series(
          name = ifelse(filtros()$comparar == "Não", "Serviços de alta complexidade", glue::glue("Serviços de alta complexidade <b>({data4_deslocamento_med()$class[1]})</b>")),
          data = data4_deslocamento_med(),
          type = "line",
          highcharter::hcaes(x = ano, y = alta_complexidade),
          legendIndex = 3,
          index = 3
        ) |>
        highcharter::hc_tooltip(valueSuffix = " km", shared = TRUE, sort = TRUE, valueDecimals = 2) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "km"), min = 0) |>
        highcharter::hc_colors(cols)
    })

    # [PROFF]

    # indicador == "prop_nasc_local_hospital" ~ "Hospital",
    # indicador == "prop_nasc_local_outros_est_saude" ~ "Outros estabelecimentos de saúde",
    # indicador == "prop_nasc_local_domicilio" ~ "Domicílio",
    # indicador == "prop_nasc_local_outros" ~ "Outros",
    # indicador == "prop_nasc_local_aldeia" ~ "Aldeia Indígena",
    # indicador == "prop_nasc_local_sem_inf" ~ "Sem informação"

    # data4_dist_local_completo
    # data4_dist_local_comp_completo

    output$grafico_dist_local <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_dist_local_completo(),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            )
          ) |>
          highcharter::hc_colors(viridis::magma(8, direction = -1)[-c(1, 8)])
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_dist_local_completo() |> dplyr::filter(indicador == "Hospital"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#FEAF77FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_comp_completo() |> dplyr::filter(indicador == "Hospital"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#FEAF77FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_completo() |> dplyr::filter(indicador == "Outros estabelecimentos de saúde"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_comp_completo() |> dplyr::filter(indicador == "Outros estabelecimentos de saúde"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_completo() |> dplyr::filter(indicador == "Domicílio"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#B63679FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_comp_completo() |> dplyr::filter(indicador == "Domicílio"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#B63679FF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_completo() |> dplyr::filter(indicador == "Outros"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#721F81FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_comp_completo() |> dplyr::filter(indicador == "Outros"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#721F81FF",
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_completo() |> dplyr::filter(indicador == "Aldeia Indígena"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#2D1160FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_comp_completo() |> dplyr::filter(indicador == "Aldeia Indígena"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#2D1160FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_completo() |> dplyr::filter(indicador == "Sem informação"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#231151FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_local_comp_completo() |> dplyr::filter(indicador == "Sem informação"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#231151FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(column = list(stacking = "percent")) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)
    })

    # TPNASCASSI Nascimento foi assistido por? Valores: 1– Médico; 2– Enfermeira ou Obstetriz; 3–Parteira; 4– Outros; 9– Ignorado

    output$grafico_dist_profissional <- highcharter::renderHighchart({

      # print(seleciona(aba= 'profissional e local'))
      # print(input$local_nasc)
      # print(data4_dist_profissional_teste())
      # print(seleciona(aba= 'profissional e local') %in% input$local_nasc)
      # print(c("a", "b", "c", "d", "e", "f")[seleciona(aba= 'profissional e local') %in% input$local_nasc])
      # print(colnames(data4_total_profissional()))
      #
      # print(data4_total_profissional()$indicador)
      # print(data4_total_profissional()$total_indicador)

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_completo(),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            )
          )|>
          highcharter::hc_colors(viridis::magma(8, direction = -1)[-c(1, 8)])
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_completo() |> dplyr::filter(indicador == "Enfermeira ou Obstetriz"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#FEAF77FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_comp_completo() |> dplyr::filter(indicador == "Enfermeira ou Obstetriz"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#FEAF77FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_completo() |> dplyr::filter(indicador == "Médico"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_comp_completo() |> dplyr::filter(indicador == "Médico"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_completo() |> dplyr::filter(indicador == "Parteira"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#B63679FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_comp_completo() |> dplyr::filter(indicador == "Parteira"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#B63679FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_completo() |> dplyr::filter(indicador == "Outros"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#721F81FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_comp_completo() |> dplyr::filter(indicador == "Outros"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#721F81FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 1,
            linkedTo = ":previous"
          )|>
          highcharter::hc_add_series(
            data = data4_dist_profissional_completo() |> dplyr::filter(indicador == "Ignorado"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#2D1160FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_comp_completo() |> dplyr::filter(indicador == "Ignorado"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#2D1160FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_completo() |> dplyr::filter(indicador == "Sem informação"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#231151FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_profissional_comp_completo() |> dplyr::filter(indicador == "Sem informação"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#231151FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 1,
            linkedTo = ":previous"
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(column = list(stacking = "percent")) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)
    })

    # gráfico de nascimentos de partos vaginais

    output$grafico_dist_partos_vaginais <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_dist_partos_vaginais_completo(),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            )
          ) |>
          highcharter::hc_colors(viridis::magma(8, direction = -1)[-c(1, 8)])
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_dist_partos_vaginais_completo() |> dplyr::filter(indicador == "Partos Vaginais"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#FEAF77FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_partos_vaginais_comp_completo() |> dplyr::filter(indicador == "Partos Vaginais"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#FEAF77FF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1,
            linkedTo = ":previous"
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_partos_vaginais_completo() |> dplyr::filter(indicador == "Outros"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_dist_partos_vaginais_comp_completo() |> dplyr::filter(indicador == "Outros"),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            color = "#F1605DFF",
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b> "
            ),
            stack = 1,
            linkedTo = ":previous"
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(column = list(stacking = "percent")) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)
    })

    ### Tabela com informações adicionais -------------------------------------
    output$municipio_informacoes_adicionais <- renderUI({
      if (filtros()$comparar == "Não") {
        sufixo <- ""
      } else {
        req(input$localidade_resumo4)
        if (input$localidade_resumo4 == "escolha1") {
          sufixo <- ""
        } else {
          sufixo <- "2"
        }
      }

      glue::glue("({filtros()[[paste0('municipio', sufixo)]]})")
    })

    data_infos_deslocamento <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo <- ""
      } else {
        req(input$localidade_resumo4)
        if (input$localidade_resumo4 == "escolha1") {
          sufixo <- ""
        } else {
          sufixo <- "2"
        }
      }

      validate(
        need(
          filtros()[[paste0("nivel", sufixo)]] %in% c("municipal"),
          "Esta tabela de informações só está disponível para o nível municipal."
        )
      )

      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
          municipio == filtros()[[paste0("municipio", sufixo)]] & uf == filtros()[[paste0("estado_municipio", sufixo)]]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::mutate(
          prop_partos_muni_maior_ocorrencia = round(n_nasc1/nao_local * 100, 1),
          prop_partos_muni_2_maior_ocorrencia = round(n_nasc2/nao_local * 100, 1),
          prop_partos_muni_3_maior_ocorrencia = round(n_nasc3/nao_local * 100, 1),
          .keep = "unused"
        ) |>
        dplyr::ungroup()
    })


    output$infos_deslocamento_muni <- reactable::renderReactable({
      municipio1 <- municipio2 <- municipio3 <- NULL

      for (i in 1:length(filtros()$ano2[1]:min(filtros()$ano2[2], 2024))) {
        municipio1[i] <- ifelse(
          !is.na(data_infos_deslocamento()$codmunnasc1[i]),
          tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data_infos_deslocamento()$codmunnasc1[i])],
          NA
        )

        municipio2[i] <- ifelse(
          !is.na(data_infos_deslocamento()$codmunnasc2[i]),
          tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data_infos_deslocamento()$codmunnasc2[i])],
          NA
        )

        municipio3[i] <- ifelse(
          !is.na(data_infos_deslocamento()$codmunnasc3[i]),
          tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data_infos_deslocamento()$codmunnasc3[i])],
          NA
        )
      }

      partos_municipio1 <- data_infos_deslocamento()$prop_partos_muni_maior_ocorrencia
      partos_municipio2 <- data_infos_deslocamento()$prop_partos_muni_2_maior_ocorrencia
      partos_municipio3 <- data_infos_deslocamento()$prop_partos_muni_3_maior_ocorrencia
      cnes <- data_infos_deslocamento()$cnes
      estabelecimento <- data_infos_deslocamento()$nome_estabelecimento_fantasia
      partos_estabelecimento <- data_infos_deslocamento()$nasc_estab

      ano <- filtros()$ano2[1]:min(filtros()$ano2[2], 2024)
      infos_municipio1 <- dplyr::if_else(
        glue::glue("{municipio1} ({formatC(partos_municipio1, big.mark = '.', decimal.mark = ',')}%)") == "NA (NA%)",
        glue::glue("---"),
        glue::glue("{municipio1} ({formatC(partos_municipio1, big.mark = '.', decimal.mark = ',')}%)")
      )
      infos_municipio2 <- dplyr::if_else(
        glue::glue("{municipio2} ({formatC(partos_municipio2, big.mark = '.', decimal.mark = ',')}%)") == "NA (NA%)",
        glue::glue("---"),
        glue::glue("{municipio2} ({formatC(partos_municipio2, big.mark = '.', decimal.mark = ',')}%)")
      )
      infos_municipio3 <- dplyr::if_else(
        glue::glue("{municipio3} ({formatC(partos_municipio3, big.mark = '.', decimal.mark = ',')}%)") == "NA (NA%)",
        glue::glue("---"),
        glue::glue("{municipio3} ({formatC(partos_municipio3, big.mark = '.', decimal.mark = ',')}%)")
      )

      infos_estabelecimento <- glue::glue("{estabelecimento} (CNES {cnes}, com {partos_estabelecimento} partos)")

      data.frame(ano, infos_municipio1, infos_municipio2, infos_municipio3, infos_estabelecimento) |>
        reactable::reactable(
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            ano = reactable::colDef(
              name = "Ano",
              minWidth = 60
            ),
            infos_municipio1 = reactable::colDef(
              name = "Município com maior ocorrência de partos fora do município de residência da mulher (% de partos)",
              minWidth = 60
            ),
            infos_municipio2 = reactable::colDef(
              name = "Município com a segunda maior ocorrência de partos fora do município de residência da mulher (% de partos)",
              minWidth = 60
            ),
            infos_municipio3 = reactable::colDef(
              name = "Município com a terceira maior ocorrência de partos fora do município de residência da mulher (% de partos)",
              minWidth = 60
            ),
            infos_estabelecimento = reactable::colDef(
              name = "Hospital com maior número de partos ocorridos fora do município de residência da mulher",
              minWidth = 60
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 520
        )
    })

  })
}



## To be copied in the UI
# mod_bloco_4_ui("bloco_4_1")

## To be copied in the server
# mod_bloco_4_server("bloco_4_1")
