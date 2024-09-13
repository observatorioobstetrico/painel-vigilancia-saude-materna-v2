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
      h2(tags$b(HTML("Assistência ao parto: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset1"),
      width = 12,
      collapsible = FALSE,
      tabPanel(
        HTML("<b>Indicadores relacionados aos grupos de Robson</b>"),
        fluidRow(
          column(
            width = 5,
            selectizeInput(
              inputId = ns("indicador_robson"),
              label = HTML("<p style='font-size:19px; margin-bottom: 0px'>Indicador</p>"),
              options = list(placeholder = "Selecione o indicador relacionado aos grupos de Robson"),
              choices = c(
                "Porcentagem de cesarianas por grupo de Robson" = "indicador1",
                "Porcentagem de nascidos vivos por grupo de Robson" = "indicador2",
                "Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas" = "indicador3"
                ),
              width = "81%"
            )
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.indicador_robson == 'indicador1'",
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
                  uiOutput(ns("input_localidade_resumo1")),
                  align = "center"
                )
              ),
              fluidRow(
                bs4Dash::box(
                  width = 12,
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart1"), height = 530))
                )
              ),
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
                HTML("<b style='font-size:19px'> Porcentagem de cesarianas por grupo de Robson &nbsp;</b>"),
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
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador1"), height = 415)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_indicador1"), height = 415)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_indicador1"), height = 415)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot7_indicador1"), height = 415))
                  ),
                  column(
                    width = 6,
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_indicador1"), height = 415)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_indicador1"), height = 415)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_indicador1"), height = 415)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot8_indicador1"), height = 415))
                  )
                )
              )
            )
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.indicador_robson == 'indicador2'",
          conditionalPanel(
            ns = ns,
            condition = "output.comparar == 'Sim'",
            column(
              width = 12,
              HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
              HTML(
                "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para visualizar os valores referentes à localidade de comparação selecionada,
                passe o cursor do mouse sobre a barra que contém a categoria de interesse.
                </b> </div>"
              ),
              hr(),
              HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
            )
          ),
          fluidRow(
            column(
              width = 6,
              HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
              HTML("<b style='font-size:19px'> Resumo do período </b>"),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                  uiOutput(ns("input_localidade_resumo2")),
                  align = "center"
                )
              ),
              fluidRow(
                bs4Dash::box(
                  width = 12,
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart2"), height = 530))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_indicador2")), proxy.height = "293px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_indicador2")), proxy.height = "293px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_indicador2")), proxy.height = "293px")
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_indicador2")), proxy.height = "325px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_indicador2")), proxy.height = "325px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_indicador2")), proxy.height = "325px")
                )
              ),
              fluidRow(
                column(
                  offset = 4,
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_indicador2")), proxy.height = "325px")
                )
              )
            ),
            column(
              width = 6,
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 740px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                HTML("<b style='font-size:19px'> Porcentagem de nascidos vivos por grupo de Robson &nbsp;</b>"),
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
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador2"), height = 650))
              )
            )
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.indicador_robson == 'indicador3'",
          conditionalPanel(
            ns = ns,
            condition = "output.comparar == 'Sim'",
            column(
              width = 12,
              HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
              HTML(
                "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para visualizar os valores referentes à localidade de comparação selecionada,
                passe o cursor do mouse sobre a barra que contém a categoria de interesse.
                </b> </div>"
              ),
              hr(),
              HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
            )
          ),
          fluidRow(
            column(
              width = 6,
              HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
              HTML("<b style='font-size:19px'> Resumo do período </b>"),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                  uiOutput(ns("input_localidade_resumo3")),
                  align = "center"
                )
              ),
              fluidRow(
                bs4Dash::box(
                  width = 12,
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart3"), height = 530))
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_indicador3")), proxy.height = "293px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_indicador3")), proxy.height = "293px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_indicador3")), proxy.height = "293px")
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_indicador3")), proxy.height = "325px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_indicador3")), proxy.height = "325px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_indicador3")), proxy.height = "325px")
                )
              ),
              fluidRow(
                column(
                  offset = 4,
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_indicador3")), proxy.height = "325px")
                )
              )
            ),
            column(
              width = 6,
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 740px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                HTML("<b style='font-size:19px'> Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas &nbsp;</b>"),
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
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador3"), height = 650))
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b>Indicadores relacionados ao deslocamento para o parto</b>"),
        conditionalPanel(
          ns = ns,
          condition = "output.comparar == 'Sim'",
          column(
            width = 12,
            HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
            conditionalPanel(
              ns = ns,
              condition = "output.nivel != 'Estadual' & output.nivel != 'Municipal'",
              HTML(
                "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para visualizar os valores referentes à localidade de comparação selecionada,
                passe o cursor do mouse sobre a barra que contém a categoria de interesse.
                </b> </div>"
              )
            ),
            conditionalPanel(
              ns = ns,
              condition = "output.nivel == 'Estadual' | output.nivel == 'Municipal'",
              HTML(
                "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para visualizar os valores referentes à localidade de comparação selecionada,
                passe o cursor do mouse sobre a barra que contém a categoria de interesse. Para os indicadores de medianas de deslocamento,
                a comparação não está disponível.
                </b> </div>"
              )
            ),
            hr(),
            HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
          )
        ),
        shinyjs::hidden(
          fluidRow(
            id = ns("mostrar_card_indicadores_deslocamento_outros_niveis"),
            column(
              width = 4,
              HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
              HTML("<b style='font-size:19px'> Resumo do período </b>"),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                  uiOutput(ns("input_localidade_resumo4")),
                  align = "center"
                )
              ),
              fluidRow(
                bs4Dash::box(
                  width = 12,
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart4"), height = 530))
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_deslocamento_resto")), proxy.height = "300px")
                ),
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_deslocamento_resto")), proxy.height = "300px")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_deslocamento_resto")), proxy.height = "332px")
                ),
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_deslocamento_resto")), proxy.height = "332px")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_deslocamento_resto")), proxy.height = "332px")
                ),
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i9_deslocamento_macro")), proxy.height = "332px")
                )
              )
             #  fluidRow(
             #    column(
             #      offset = 3,
             #      width = 6,
             #      shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i10_deslocamento_macro")), proxy.height = "332px")
             #    )
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
                  style = "height: 740px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "display: flex; align-items: center;",
                    HTML(glue::glue("<b style = 'font-size: 19px'> Porcentagem de nascidos vivos segundo local de ocorrência do parto &nbsp;</b>")),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_resto_prop1"), height = "640px"))
                )
              ),
              fluidRow(
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 740px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "display: flex; align-items: center;",
                    HTML(glue::glue("<b style = 'font-size: 19px'> Porcentagem de nascidos vivos com peso < 1500g segundo local de ocorrência do parto &nbsp;</b>")),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_macrorregiao_1"), height = "650px"))
                )
              ),
            )
          )
        ),
        shinyjs::hidden(
          fluidRow(
            id = ns("mostrar_card_indicadores_deslocamento_muni_uf"),
            column(
              width = 4,
              HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
              HTML("<b style='font-size:19px'> Resumo do período </b>"),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                  uiOutput(ns("input_localidade_resumo5")),
                  align = "center"
                )
              ),
              fluidRow(
                bs4Dash::box(
                  width = 12,
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart5"), height = 530))
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_deslocamento_muni")), proxy.height = "300px")
                ),
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2_deslocamento_muni")), proxy.height = "300px")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3_deslocamento_muni")), proxy.height = "332px")
                ),
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4_deslocamento_muni")), proxy.height = "332px")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_deslocamento_muni")), proxy.height = "332px"),
                ),
                column(
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i9_deslocamento_resto")), proxy.height = "332px")
                )
              )#,
              # fluidRow(
              #   column(
              #     offset = 3,
              #     width = 6,
              #     shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i10_deslocamento_resto")), proxy.height = "332px")
              #   )
              # )
            ),
            shinyjs::hidden(
              column(
                id = ns("mostrar_card_indicadores_deslocamento_muni"),
                width = 8,
                fluidRow(
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 740px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                    div(
                      style = "display: flex; align-items: center;",
                      HTML(glue::glue("<b style = 'font-size: 19px'> Porcentagem de nascidos vivos segundo local de ocorrência do parto &nbsp;</b>")),
                      shinyjs::hidden(
                        span(
                          id = ns("mostrar_botao_deslocamento_prop3"),
                          shinyWidgets::actionBttn(
                            inputId = ns("botao_prop3"),
                            icon = icon("triangle-exclamation", style = "color: red"),
                            color = "warning",
                            style = "material-circle",
                            size = "xs"
                          )
                        )
                      )
                    ),
                    hr(),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_muni_prop1"), height = "650px"))
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    bs4Dash::bs4Card(
                      width = 12,
                      status = "primary",
                      collapsible = FALSE,
                      headerBorder = FALSE,
                      style = "height: 680px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                      div(
                        style = "display: flex; align-items: center;",
                        HTML(glue::glue("<b style = 'font-size: 19px'> Mediana de deslocamento para o destino, segundo destinos &nbsp;</b>")),
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
                        inputId = ns("local_med_muni"),
                        label = "Local de ocorrência do parto",
                        options = list(placeholder = "Selecione o local de ocorrência do parto"),
                        choices = c(
                          "Fora do município de residência da mulher" = "fora_municipio",
                          "Na microrregião de saúde, mas fora do município de residência da mulher" = "na_regiao",
                          "Na macrorregião de saúde, mas fora da microrregião de saúde de residência mulher" = "na_macrorregiao",
                          "Na UF, mas fora da macrorregião de saúde de residência mulher" = "fora_macrorregiao",
                          "Fora da UF de residência da mulher" = "fora_uf"
                        ),
                        width = "100%"
                      ),
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_muni_med1"), height = "500px"))
                    )
                  )
                ),
                fluidRow(
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 740px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                    div(
                      style = "display: flex; align-items: center;",
                      HTML(glue::glue("<b style = 'font-size: 19px'> Porcentagem de nascidos vivos com peso < 1500g segundo local de ocorrência do parto &nbsp;</b>")),
                      shinyjs::hidden(
                        span(
                          id = ns("mostrar_botao_deslocamento_prop4"),
                          shinyWidgets::actionBttn(
                            inputId = ns("botao_prop4"),
                            icon = icon("triangle-exclamation", style = "color: red"),
                            color = "warning",
                            style = "material-circle",
                            size = "xs"
                          )
                        )
                      )
                    ),
                    hr(),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_macrorregiao_2"), height = "650px"))
                  )
                )
              ),
              column(
                id = ns("mostrar_tabela_indicadores_deslocamento_muni"),
                offset = 4,
                width = 8,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 680px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "display: flex; align-items: center;",
                    HTML(glue::glue("<b style = 'font-size: 19px'> Informações adicionais &nbsp;</b>")),
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
                  div(
                    style = "height: 90%; overflow-y: auto;",
                    shinycssloaders::withSpinner(uiOutput(ns("infos_deslocamento_muni")))
                  )
                )
              )
            ),
            shinyjs::hidden(
              column(
                id = ns("mostrar_card_indicadores_deslocamento_uf"),
                width = 8,
                fluidRow(
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 740px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                    div(
                      style = "display: flex; align-items: center;",
                      HTML(glue::glue("<b style = 'font-size: 19px'> Porcentagem de nascidos vivos segundo local de ocorrência do parto &nbsp;</b>")),
                      shinyjs::hidden(
                        span(
                          id = ns("mostrar_botao_deslocamento_prop5"),
                          shinyWidgets::actionBttn(
                            inputId = ns("botao_prop5"),
                            icon = icon("triangle-exclamation", style = "color: red"),
                            color = "warning",
                            style = "material-circle",
                            size = "xs"
                          )
                        )
                      )
                    ),
                    hr(),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_uf_prop1"), height = "640px"))
                  )
                ),
                fluidRow(
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 680px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                    div(
                      style = "display: flex; align-items: center;",
                      HTML(glue::glue("<b style = 'font-size: 19px'> Mediana de deslocamento para o destino, segundo destinos &nbsp;</b>")),
                      shinyjs::hidden(
                        span(
                          id = ns("mostrar_botao_deslocamento_mediana2"),
                          shinyWidgets::actionBttn(
                            inputId = ns("botao_mediana2"),
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
                      inputId = ns("local_med_uf"),
                      label = "Local de ocorrência do parto",
                      options = list(placeholder = "Selecione o local de ocorrência do parto"),
                      choices = c(
                        "Fora do município de residência da mulher" = "fora_municipio",
                        "Na microrregião de saúde, mas fora do município de residência da mulher" = "na_regiao",
                        "Na macrorregião de saúde, mas fora da microrregião de saúde de residência mulher" = "na_macrorregiao",
                        "Na UF, mas fora da macrorregião de saúde de residência mulher" = "fora_macrorregiao",
                        "Fora da UF de residência da mulher" = "fora_uf"
                      ),
                      width = "100%"
                    ),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_uf_med1"), height = "500px"))
                  )
                ),
                fluidRow(
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 740px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                    div(
                      style = "display: flex; align-items: center;",
                      HTML(glue::glue("<b style = 'font-size: 19px'> Porcentagem de nascidos vivos com peso < 1500g segundo local de ocorrência do parto &nbsp;</b>")),
                      shinyjs::hidden(
                        span(
                          id = ns("mostrar_botao_deslocamento_prop6"),
                          shinyWidgets::actionBttn(
                            inputId = ns("botao_prop6"),
                            icon = icon("triangle-exclamation", style = "color: red"),
                            color = "warning",
                            style = "material-circle",
                            size = "xs"
                          )
                        )
                      )
                    ),
                    hr(),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_deslocamento_macrorregiao_3"), height = "650px"))
                  )
                )
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
      contrib_robson_faltante_tx_cesariana = rep("round((sum(mulheres_com_parto_cesariana) - sum(dplyr::across(dplyr::starts_with('total_cesariana')))) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2)
    )

    bloco4_deslocamento_calcs <- data.frame(
      tipo = c("local", "referencia"),
      prop_partos_municipio_res = rep("round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_fora_municipio_res = rep("round(sum(nao_local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_rsaude_res = rep("round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_macro_rsaude_res = rep("round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_fora_macro_rsaude_res = rep("round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_fora_uf_res = rep("round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
      prop_partos_na_macro_com_uti = rep("round(sum(partos_na_macro_com_uti)/sum(nascimentos) * 100, 1)", 2),
      prop_partos_na_macro_sem_uti = rep("round(sum(partos_na_macro_sem_uti)/sum(nascimentos) * 100, 1)", 2),
      prop_partos_fora_macro_com_uti = rep("round(sum(partos_fora_macro_com_uti)/sum(nascimentos) * 100, 1)", 2),
      prop_partos_fora_macro_sem_uti = rep("round(sum(partos_fora_macro_sem_uti)/sum(nascimentos) * 100, 1)", 2),
      prop_partos_na_macro_sem_inf = rep("round(sum(partos_na_macro_sem_inf)/sum(nascimentos) * 100, 1)", 2),
      prop_partos_fora_macro_sem_inf = rep("round(sum(partos_fora_macro_sem_inf)/sum(nascimentos) * 100, 1)", 2),
      prop_partos_sem_uti = rep("round((sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_sem_uti)) / (sum(partos_na_macro_com_uti) + sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_com_uti) + sum(partos_fora_macro_sem_uti)) * 100, 1)", 2)
    )

    bloco4_calcs_resumo <- dplyr::full_join(bloco4_calcs, bloco4_deslocamento_calcs) |>
      dplyr::mutate(
        prop_obitos_fetais_durante = rep("round(sum(fetal_durante) / sum(obitos_fetais_mais_22sem) * 100, 1)", 2),
        porc_obitos_fetais_evitaveis_parto = rep("round(sum(evitaveis_fetal_parto) / sum(obitos_fetais_totais) * 100, 1)", 2)
      )

    # Juntando as bases de deslocamento ---------------------------------------
    ## Isso deveria ter sido feito na parte de extração
    bloco4_deslocamento_muni <- dplyr::left_join(bloco4_deslocamento_muni, bloco4_deslocamento_macrorregiao)

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
      if (filtros()$nivel == "Municipal") {
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_outros_niveis", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_uf", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "mostrar_card_indicadores_deslocamento_muni_uf", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "mostrar_card_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "mostrar_tabela_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.8)
      } else if (filtros()$nivel == "Estadual") {
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_outros_niveis", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "mostrar_tabela_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "mostrar_card_indicadores_deslocamento_muni_uf", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "mostrar_card_indicadores_deslocamento_uf", anim = TRUE, animType = "slide", time = 0.8)
      } else {
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_uf", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_muni_uf", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "mostrar_tabela_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "mostrar_card_indicadores_deslocamento_muni", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "mostrar_card_indicadores_deslocamento_outros_niveis", anim = TRUE, animType = "slide", time = 0.8)
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

    ## Criando os outputs que receberão os nomes dos locais selecionados quando há comparação --------
    localidade_original <- reactive({
      dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )
    })

    localidade_comparacao <- reactive({
      dplyr::case_when(
        filtros()$nivel2 == "Nacional" ~ "Brasil",
        filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
        filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
        filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
        filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
        filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
        filtros()$nivel2 == "Municípios semelhantes" ~ "Média dos municípios semelhantes"
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

    output$input_localidade_resumo5 <- renderUI({
      if (filtros()$comparar == "Sim") {
        radioButtons(
          inputId = ns("localidade_resumo5"),
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

    ## Para os botões de alerta quanto à incompletude e cobertura --------------
    ### Calculando os indicadores de incompletude ------------------------------
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
          parto_tprobson = round(sum(parto_tprobson_incompletos, na.rm = TRUE)/sum(parto_tprobson_totais, na.rm = TRUE) * 100, 2),
          tprobson = round(sum(tprobson_incompletos, na.rm = TRUE)/sum(tprobson_totais, na.rm = TRUE) * 100, 2),
          prop_cnes_nao_preenchido = round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_preenchido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 2),
          prop_cnes_invalido = round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_valido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 2),
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
      dplyr::left_join(
        dplyr::left_join(bloco4, bloco4_deslocamento_muni),
        dplyr::left_join(
          bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
          bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        )
      ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
            req(
              input$indicador_robson,
              get('input')[[glue::glue("localidade_resumo{substr(input$indicador_robson, start = nchar(input$indicador_robson), stop = nchar(input$indicador_robson))}")]]
            )
            if (get('input')[[glue::glue("localidade_resumo{substr(input$indicador_robson, start = nchar(input$indicador_robson), stop = nchar(input$indicador_robson))}")]] == "escolha1") {
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
        cria_indicadores(
          df_calcs = bloco4_calcs_resumo,
          filtros = filtros(),
          localidade_resumo = get('input')[[glue::glue("localidade_resumo{substr(input$indicador_robson, start = nchar(input$indicador_robson), stop = nchar(input$indicador_robson))}")]]
        )
    })

    data4_deslocamento_resumo <- reactive({
      dplyr::left_join(
        dplyr::left_join(bloco4, bloco4_deslocamento_muni),
        dplyr::left_join(
          bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
          bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        )
      ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
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
            req(
              get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]]
            )
            if (get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]] == "escolha1") {
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
        cria_indicadores(
          df_calcs = bloco4_calcs_resumo,
          filtros = filtros(),
          localidade_resumo = get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]]
        )
    })

    ### Para a referência -----------------------------------------------------
    data4_resumo_referencia <- reactive({
      dplyr::left_join(
        dplyr::left_join(bloco4, bloco4_deslocamento_muni),
        dplyr::left_join(
          bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
          bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        )
      ) |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco4_calcs_resumo, filtros = filtros(), referencia = TRUE)
    })

    data4_deslocamento_resumo_referencia <- reactive({
      dplyr::left_join(
        dplyr::left_join(bloco4, bloco4_deslocamento_muni),
        dplyr::left_join(
          bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
          bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        )
      ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco4_calcs_resumo, filtros = filtros(), referencia = TRUE)
    })


    ## Criando o output do gráfico de radar -----------------------------------
    ### Definindo os indicadores que aparecerão no gráfico
    selected_indicators <- c(
      "prop_tx_cesariana_geral",
      "prop_partos_fora_macro_rsaude_res",
      "prop_partos_sem_uti",
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
              if (indicador_abrev %in% c("prop_partos_sem_uti", "prop_obitos_fetais_durante")) {
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
            "Porcentagem de óbitos fetais ocorridos durante o parto"
          } else if (indicador_abrev == "prop_partos_sem_uti") {
            "Porcentagem de nascidos vivos com peso < 1500 g nascidos em serviço sem UTI neonatal"
          } else {
            tabela_radar$indicador[tabela_radar$nome_abreviado == indicador_abrev]
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
          opacity = 0.7,
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
        valor_de_referencia = 25,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "287px", "300px"),
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
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "287px", "300px"),
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
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "287px", "300px"),
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
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "287px", "300px"),
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
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "287px", "300px"),
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
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "287px", "300px"),
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
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "287px", "300px"),
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
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "287px", "300px"),
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
        fonte_titulo = "15px",
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
    output$caixa_b4_i1_deslocamento_muni <- output$caixa_b4_i1_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_municipio_res",
        titulo = "Porcentagem de partos ocorridos no município de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_municipio_res,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]] == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i2_deslocamento_muni <- output$caixa_b4_i2_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na microrregião de saúde, mas fora do município de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]] == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i3_deslocamento_muni <- output$caixa_b4_i3_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_macro_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na macrorregião de saúde, mas fora da microrregião de saúde de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_macro_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]] == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i4_deslocamento_muni <- output$caixa_b4_i4_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_fora_macro_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na UF, mas fora da macrorregião de saúde de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_fora_macro_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]] == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i5_deslocamento_muni <- output$caixa_b4_i5_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento_resumo(),
        indicador = "prop_partos_fora_uf_res",
        titulo = "Porcentagem de partos ocorridos fora da UF de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_deslocamento_resumo_referencia()$prop_partos_fora_uf_res,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]] == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i9_deslocamento_macro <- output$caixa_b4_i9_deslocamento_resto <- renderUI({
      tagList(
        div(
          style = "position: relative;",
          # Caixinha criada pela função cria_caixa_server
          cria_caixa_server(
            dados = data4_deslocamento_resumo(),
            indicador = "prop_partos_sem_uti",
            titulo = "Porcentagem de nascidos vivos com peso < 1500 g nascidos em serviço sem UTI neonatal",
            tem_meta = TRUE,
            valor_de_referencia = 16.3, #data4_deslocamento_resumo_referencia()$prop_partos_sem_uti,
            tipo = "porcentagem",
            invertido = FALSE,
            tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
            fonte_titulo = "15px",
            pagina = "bloco_4",
            tipo_referencia = "HEALTHY PEOPLE, 2020",
            nivel_de_analise = ifelse(
              filtros()$comparar == "Não",
              filtros()$nivel,
              ifelse(
                get('input')[[glue::glue("localidade_resumo{ifelse(filtros()$nivel %in% c('Estadual', 'Municipal'), 5, 4)}")]] == "escolha1",
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

    # Exibe a mensagem quando o botão for clicado
    observeEvent(input$aviso_desloc, {
      shinyalert::shinyalert(
        title = "Atenção",
        text = "Esse indicador é condicional a ter nascido vivo",
        type = "info"
      )
    })


    # Para os gráficos --------------------------------------------------------
    cols <- c("#2c115f", "#b73779", "#fc8961")

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
          if (filtros()$nivel == "Nacional")
            ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
        cria_indicadores(df_calcs = bloco4_calcs, filtros = filtros(), adicionar_localidade = FALSE) |>
        dplyr::mutate(
          localidade = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    data4_deslocamento <- reactive({
      if (filtros()$nivel == "Estadual") {
        data_aux <- bloco4_deslocamento_uf
      } else {
        data_aux <- dplyr::left_join(
          bloco4_deslocamento_muni,
          dplyr::left_join(
            bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
            bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
          )
        ) |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0)))
      }
      data_aux |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
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
        cria_indicadores(df_calcs = bloco4_deslocamento_calcs, filtros = filtros(), adicionar_localidade = FALSE) |>
        dplyr::mutate(
          localidade = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    data4_deslocamento_med <- reactive({
      if (filtros()$nivel == "Municipal") {
        input_local_med <- input$local_med_muni
        data_aux <- bloco4_deslocamento_muni
      } else {
        input_local_med <- input$local_med_uf
        data_aux <- bloco4_deslocamento_uf
      }

      data4_deslocamento_med_aux <- data_aux |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::filter(
          if (filtros()$nivel == "Estadual")
            uf == filtros()$estado
          else if(filtros()$nivel == "Municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::select(
          ano,
          no_local = glue::glue("km_partos_{input_local_med}"),
          baixa_complexidade = "km_partos_fora_municipio_baixa_complexidade",
          alta_complexidade = "km_partos_fora_municipio_alta_complexidade"
        )
    })

    data4_deslocamento_macro <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
        dplyr::group_by(ano) |>
        dplyr::summarise(
          prop_partos_na_macro_com_uti = round(sum(partos_na_macro_com_uti)/sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_uti = round(sum(partos_na_macro_sem_uti)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_com_uti = round(sum(partos_fora_macro_com_uti)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_uti = round(sum(partos_fora_macro_sem_uti)/sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_inf = round(sum(partos_na_macro_sem_inf)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_inf = round(sum(partos_fora_macro_sem_inf)/sum(nascimentos) * 100, 1)
        ) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "prop_indicador"
        ) |>
        dplyr::mutate(
          indicador =
            dplyr::case_when(
              grepl("prop_partos_na_macro_com_uti", indicador) ~ "Na macrorregião de saúde e em estabelecimento que tem pelo menos um leito de UTI",
              grepl("prop_partos_na_macro_sem_uti", indicador) ~ "Na macrorregião de saúde, mas em estabelecimento que não tem leito de UTI",
              grepl("prop_partos_fora_macro_com_uti", indicador) ~ "Fora da macrorregião de saúde, mas em estabelecimento que tem pelo menos um leito de UTI",
              grepl("prop_partos_fora_macro_sem_uti", indicador) ~ "Fora da macrorregião de saúde e em estabelecimento que não tem leito de UTI",
              grepl("prop_partos_na_macro_sem_inf", indicador) ~ "Na macrorregião de saúde, mas sem informação sobre leito de UTI",
              grepl("prop_partos_fora_macro_sem_inf", indicador) ~ "Fora da macrorregião de saúde, mas sem informação sobre leito de UTI"
            ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, indicador, class) |>
        dplyr::summarise(
          prop_indicador = round(sum(prop_indicador), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          indicador = factor(indicador, levels = c(
            "Na macrorregião de saúde e em estabelecimento que tem pelo menos um leito de UTI",
            "Na macrorregião de saúde, mas em estabelecimento que não tem leito de UTI",
            "Fora da macrorregião de saúde, mas em estabelecimento que tem pelo menos um leito de UTI",
            "Fora da macrorregião de saúde e em estabelecimento que não tem leito de UTI",
            "Fora da macrorregião de saúde, mas sem informação sobre leito de UTI",
            "Na macrorregião de saúde, mas sem informação sobre leito de UTI"

          ))
        )
    })

    ### Para a comparação selecionada -----------------------------------------
    data4_comp <- reactive({
      bloco4 |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "Nacional")
            ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
        cria_indicadores(df_calcs = bloco4_calcs, filtros = filtros(), comp = TRUE, adicionar_localidade = FALSE) |>
        dplyr::mutate(
          localidade = dplyr::case_when(
            filtros()$nivel2 == "Nacional" ~ "Brasil",
            filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
            filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
            filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "Municípios semelhantes" ~ "Média dos municípios semelhantes"
          )
        )
    })

    data4_deslocamento_comp <- reactive({
      dplyr::left_join(
        bloco4_deslocamento_muni,
        dplyr::left_join(
          bloco7 |> dplyr::select(codmunres, ano, fetal_durante, obitos_fetais_mais_22sem),
          bloco8_graficos |> dplyr::select(codmunres, ano, evitaveis_fetal_parto, obitos_fetais_totais)
        )
      ) |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., is.na(.), 0))) |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
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
          else if (filtros()$nivel2 == "Municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "Municípios semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco4_deslocamento_calcs, filtros = filtros(), comp = TRUE, adicionar_localidade = FALSE) |>
        dplyr::rename_with(~paste0("br_", .x), dplyr::starts_with("prop")) |>
        dplyr::mutate(
          localidade_comparacao = dplyr::case_when(
            filtros()$nivel2 == "Nacional" ~ "Média nacional",
            filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
            filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
            filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "Municípios semelhantes" ~ "Média dos municípios semelhantes",
          )
        )
    })

    data4_deslocamento_juncao <- reactive({dplyr::full_join(data4_deslocamento(), data4_deslocamento_comp(), by = "ano")})

    data4_deslocamento_macro_comp <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
        dplyr::group_by(ano) |>
        dplyr::summarise(
          prop_partos_na_macro_com_uti = round(sum(partos_na_macro_com_uti)/sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_uti = round(sum(partos_na_macro_sem_uti)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_com_uti = round(sum(partos_fora_macro_com_uti)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_uti = round(sum(partos_fora_macro_sem_uti)/sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_inf = round(sum(partos_na_macro_sem_inf)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_inf = round(sum(partos_fora_macro_sem_inf)/sum(nascimentos) * 100, 1)) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "prop_indicador"
        )|>
        dplyr::mutate(
          indicador =
            dplyr::case_when(
              grepl("prop_partos_na_macro_com_uti", indicador) ~ "Na macrorregião de saúde e em estabelecimento que tem pelo menos um leito de UTI",
              grepl("prop_partos_na_macro_sem_uti", indicador) ~ "Na macrorregião de saúde, mas em estabelecimento que não tem leito de UTI",
              grepl("prop_partos_fora_macro_com_uti", indicador) ~ "Fora da macrorregião de saúde, mas em estabelecimento que tem pelo menos um leito de UTI",
              grepl("prop_partos_fora_macro_sem_uti", indicador) ~ "Fora da macrorregião de saúde e em estabelecimento que não tem leito de UTI",
              grepl("prop_partos_na_macro_sem_inf", indicador) ~ "Na macrorregião de saúde, mas sem informação sobre leito de UTI",
              grepl("prop_partos_fora_macro_sem_inf", indicador) ~ "Fora da macrorregião de saúde, mas sem informação sobre leito de UTI"
            ),
          class = dplyr::case_when(
            filtros()$nivel2 == "Nacional" ~ "Brasil",
            filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
            filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
            filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "Municípios semelhantes" ~ "Média dos municípios semelhantes"
          )
        )|>
        dplyr::ungroup() |>
        dplyr::group_by(ano, indicador, class) |>
        dplyr::summarise(
          prop_indicador = round(sum(prop_indicador), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          indicador = factor(indicador, levels = c(
            "Na macrorregião de saúde e em estabelecimento que tem pelo menos um leito de UTI",
            "Na macrorregião de saúde, mas em estabelecimento que não tem leito de UTI",
            "Fora da macrorregião de saúde, mas em estabelecimento que tem pelo menos um leito de UTI",
            "Fora da macrorregião de saúde e em estabelecimento que não tem leito de UTI",
            "Fora da macrorregião de saúde, mas sem informação sobre leito de UTI",
            "Na macrorregião de saúde, mas sem informação sobre leito de UTI"

          ))
        )
    })

    ### Para a referência -----------------------------------------------------
    data4_referencia <- reactive({
      bloco4 |>
        dplyr::filter(ano >= max(filtros()$ano2[1], 2014) & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco4_calcs, filtros = filtros(), adicionar_localidade = FALSE, referencia = TRUE) |>
        dplyr::rename_with(~paste0("br_", .x), dplyr::starts_with("prop") | dplyr::starts_with("contrib")) |>
        dplyr::mutate(
          localidade_comparacao = "Média nacional"
        )
    })

    data4_deslocamento_macro_referencia <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          prop_partos_na_macro_com_uti = round(sum(partos_na_macro_com_uti)/sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_uti = round(sum(partos_na_macro_sem_uti)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_com_uti = round(sum(partos_fora_macro_com_uti)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_uti = round(sum(partos_fora_macro_sem_uti)/sum(nascimentos) * 100, 1),
          prop_partos_na_macro_sem_inf = round(sum(partos_na_macro_sem_inf)/sum(nascimentos) * 100, 1),
          prop_partos_fora_macro_sem_inf = round(sum(partos_fora_macro_sem_inf)/sum(nascimentos) * 100, 1)) |>
        tidyr::pivot_longer(
          cols = starts_with("prop"),
          names_to = "indicador",
          values_to = "br_prop_indicador"
        ) |>
        dplyr::mutate(
          indicador =
            dplyr::case_when(
              grepl("prop_partos_na_macro_com_uti", indicador) ~ "Na macrorregião de saúde e em estabelecimento que tem pelo menos um leito de UTI",
              grepl("prop_partos_na_macro_sem_uti", indicador) ~ "Na macrorregião de saúde, mas em estabelecimento que não tem leito de UTI",
              grepl("prop_partos_fora_macro_com_uti", indicador) ~ "Fora da macrorregião de saúde, mas em estabelecimento que tem pelo menos um leito de UTI",
              grepl("prop_partos_fora_macro_sem_uti", indicador) ~ "Fora da macrorregião de saúde e em estabelecimento que não tem leito de UTI",
              grepl("prop_partos_na_macro_sem_inf", indicador) ~ "Na macrorregião de saúde, mas sem informação sobre leito de UTI",
              grepl("prop_partos_fora_macro_sem_inf", indicador) ~ "Fora da macrorregião de saúde, mas sem informação sobre leito de UTI"
            )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, indicador) |>
        dplyr::summarise(
          br_prop_indicador = round(sum(br_prop_indicador), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          indicador = factor(indicador, levels = c(
            "Na macrorregião de saúde e em estabelecimento que tem pelo menos um leito de UTI",
            "Na macrorregião de saúde, mas em estabelecimento que não tem leito de UTI",
            "Fora da macrorregião de saúde, mas em estabelecimento que tem pelo menos um leito de UTI",
            "Fora da macrorregião de saúde e em estabelecimento que não tem leito de UTI",
            "Fora da macrorregião de saúde, mas sem informação sobre leito de UTI",
            "Na macrorregião de saúde, mas sem informação sobre leito de UTI"

          ))
        )
    })

    data4_deslocamento_macro_completo <- reactive({
      dplyr::full_join(data4_deslocamento_macro(), data4_deslocamento_macro_referencia())
    })

    data4_deslocamento_macro_comp_completo <- reactive({
      dplyr::full_join(data4_deslocamento_macro_comp(), data4_deslocamento_macro_referencia())
    })

    ### Passando os dados da localidade escolhida para o formato long ---------
    data4_long <- reactive({
      data4() |>
        tidyr::pivot_longer(
          cols = c(
            "prop_nasc_robson1",
            "prop_nasc_robson2",
            "prop_nasc_robson3",
            "prop_nasc_robson4",
            "prop_nasc_robson5",
            "prop_nasc_robson6_a_9",
            "prop_nasc_robson10",
            "prop_nasc_robson_faltante",
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
            "contrib_robson_faltante_tx_cesariana"
          ),
          names_to = "grupo_robson",
          values_to = "value"
        ) |>
        dplyr::mutate(
          indicador = as.factor(
            dplyr::case_when(
              stringr::str_detect(grupo_robson, "^prop_nasc") ~ "indicador2",
              stringr::str_detect(grupo_robson, "^prop_tx") | stringr::str_detect(grupo_robson, "^prop_robson") ~ "indicador1",
              stringr::str_detect(grupo_robson, "^contrib") ~ "indicador3"
            )
          ),
          grupo_robson = as.factor(
            dplyr::case_when(
              grupo_robson == "prop_tx_cesariana_geral" ~ "Geral",
              stringr::str_detect(grupo_robson, "faltante") ~ "Sem informação",
              stringr::str_detect(grupo_robson, "robson1") & !stringr::str_detect(grupo_robson, "robson10") ~ "Grupo de Robson 1",
              stringr::str_detect(grupo_robson, "robson2") ~ "Grupo de Robson 2",
              stringr::str_detect(grupo_robson, "robson3") ~ "Grupo de Robson 3",
              stringr::str_detect(grupo_robson, "robson4") ~ "Grupo de Robson 4",
              stringr::str_detect(grupo_robson, "robson5") ~ "Grupo de Robson 5",
              stringr::str_detect(grupo_robson, "robson6") ~ "Grupos de Robson 6 a 9",
              stringr::str_detect(grupo_robson, "robson10") ~ "Grupo de Robson 10"
            )
          )
        ) |>
        dplyr::mutate(
          grupo_robson = factor(
            grupo_robson,
            levels = c(
              "Geral",
              "Sem informação",
              "Grupo de Robson 1",
              "Grupo de Robson 2",
              "Grupo de Robson 3",
              "Grupo de Robson 4",
              "Grupo de Robson 5",
              "Grupos de Robson 6 a 9",
              "Grupo de Robson 10"
            )
          )
        )
    })

    ### Passando os dados da comparação para o formato long -------------------
    data4_comp_long <- reactive({
      data4_comp() |>
        tidyr::pivot_longer(
          cols = c(
            "prop_nasc_robson1",
            "prop_nasc_robson2",
            "prop_nasc_robson3",
            "prop_nasc_robson4",
            "prop_nasc_robson5",
            "prop_nasc_robson6_a_9",
            "prop_nasc_robson10",
            "prop_nasc_robson_faltante",
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
            "contrib_robson_faltante_tx_cesariana"
          ),
          names_to = "grupo_robson",
          values_to = "value"
        ) |>
        dplyr::mutate(
          indicador = as.factor(
            dplyr::case_when(
              stringr::str_detect(grupo_robson, "^prop_nasc") ~ "indicador2",
              stringr::str_detect(grupo_robson, "^prop_tx") | stringr::str_detect(grupo_robson, "^prop_robson") ~ "indicador1",
              stringr::str_detect(grupo_robson, "^contrib") ~ "indicador3"
            )
          ),
          grupo_robson = as.factor(
            dplyr::case_when(
              grupo_robson == "prop_tx_cesariana_geral" ~ "Geral",
              stringr::str_detect(grupo_robson, "faltante") ~ "Sem informação",
              stringr::str_detect(grupo_robson, "robson1") & !stringr::str_detect(grupo_robson, "robson10") ~ "Grupo de Robson 1",
              stringr::str_detect(grupo_robson, "robson2") ~ "Grupo de Robson 2",
              stringr::str_detect(grupo_robson, "robson3") ~ "Grupo de Robson 3",
              stringr::str_detect(grupo_robson, "robson4") ~ "Grupo de Robson 4",
              stringr::str_detect(grupo_robson, "robson5") ~ "Grupo de Robson 5",
              stringr::str_detect(grupo_robson, "robson6") ~ "Grupos de Robson 6 a 9",
              stringr::str_detect(grupo_robson, "robson10") ~ "Grupo de Robson 10"
            )
          )
        ) |>
        dplyr::mutate(
          grupo_robson = factor(
            grupo_robson,
            levels = c(
              "Geral",
              "Sem informação",
              "Grupo de Robson 1",
              "Grupo de Robson 2",
              "Grupo de Robson 3",
              "Grupo de Robson 4",
              "Grupo de Robson 5",
              "Grupos de Robson 6 a 9",
              "Grupo de Robson 10"
            )
          )
        )
    })

    ### Passando os dados da junção entre as localidades para o formato long -------------------
    data4_long_juncao <- reactive({
      rbind(data4(), data4_comp()) |>
        tidyr::pivot_longer(
          cols = c(
            "prop_nasc_robson1",
            "prop_nasc_robson2",
            "prop_nasc_robson3",
            "prop_nasc_robson4",
            "prop_nasc_robson5",
            "prop_nasc_robson6_a_9",
            "prop_nasc_robson10",
            "prop_nasc_robson_faltante",
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
            "contrib_robson_faltante_tx_cesariana"
          ),
          names_to = "grupo_robson",
          values_to = "value"
        ) |>
        dplyr::mutate(
          indicador = as.factor(
            dplyr::case_when(
              stringr::str_detect(grupo_robson, "^prop_nasc") ~ "indicador2",
              stringr::str_detect(grupo_robson, "^prop_tx") | stringr::str_detect(grupo_robson, "^prop_robson") ~ "indicador1",
              stringr::str_detect(grupo_robson, "^contrib") ~ "indicador3"
            )
          ),
          grupo_robson = as.factor(
            dplyr::case_when(
              grupo_robson == "prop_tx_cesariana_geral" ~ "Geral",
              stringr::str_detect(grupo_robson, "faltante") ~ "Sem informação",
              stringr::str_detect(grupo_robson, "robson1") & !stringr::str_detect(grupo_robson, "robson10") ~ "Grupo de Robson 1",
              stringr::str_detect(grupo_robson, "robson2") ~ "Grupo de Robson 2",
              stringr::str_detect(grupo_robson, "robson3") ~ "Grupo de Robson 3",
              stringr::str_detect(grupo_robson, "robson4") ~ "Grupo de Robson 4",
              stringr::str_detect(grupo_robson, "robson5") ~ "Grupo de Robson 5",
              stringr::str_detect(grupo_robson, "robson6") ~ "Grupos de Robson 6 a 9",
              stringr::str_detect(grupo_robson, "robson10") ~ "Grupo de Robson 10"
            )
          )
        ) |>
        dplyr::mutate(
          grupo_robson = factor(
            grupo_robson,
            levels = c(
              "Geral",
              "Sem informação",
              "Grupo de Robson 1",
              "Grupo de Robson 2",
              "Grupo de Robson 3",
              "Grupo de Robson 4",
              "Grupo de Robson 5",
              "Grupos de Robson 6 a 9",
              "Grupo de Robson 10"
            )
          )
        )
    })


    ## Criando os outputs dos gráficos ----------------------------------------
    ### Porcentagem de cesarianas por grupo de Robson -------------------------
    list_of_plots <- reactive({
      purrr::map(as.vector(unique(data4_long()$grupo_robson[which(data4_long()$indicador == "indicador1")])), function(x) {
        if (filtros()$comparar == "Não") {

          filtered <- reactive({
            data4_long() |>
              dplyr::filter(grupo_robson == x, indicador == "indicador1")
          })

          highcharter::highchart() |>
            highcharter::hc_add_series(
              data = filtered(),
              highcharter::hcaes(x = ano, y = value, group = localidade),
              type = "column",
              color = c("#2c115f"),
              showInLegend = TRUE
            ) |>
            highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
            highcharter::hc_title(text = HTML(glue::glue("<b style='font-size:16px'> {x} </b>"))) |>
            highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
            highcharter::hc_yAxis(title = list(text = "% de nascidos vivos por cesariana"), min = 0, max = 100)

        } else if (filtros()$comparar == "Sim") {

          filtered <- reactive({
            data4_long_juncao() |>
              dplyr::filter(grupo_robson == x, indicador == "indicador1")
          })

          highcharter::highchart() |>
            highcharter::hc_add_series(
              data = filtered(),
              highcharter::hcaes(x = ano, y = value, group = localidade),
              type = "column",
              color = c("#2c115f", "#b73779"),
              showInLegend = TRUE
            ) |>
            highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
            highcharter::hc_title(text = HTML(glue::glue("<b style='font-size:16px'> {x} </b>"))) |>
            highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
            highcharter::hc_yAxis(title = list(text = "% de nascidos vivos por cesariana"), min = 0, max = 100)
        }

      })
    })

    output$plot1_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[1]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = 15, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (meta ajustada para o Brasil)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = 25, group = localidade_comparacao),
          color = "#fc8961",
          showInLegend = TRUE,
          opacity = 0.8
        )
    })

    output$plot2_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[2]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_robson1_tx_cesariana, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        )
    })

    output$plot3_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[3]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          name = "Referência (meta OMS)",
          highcharter::hcaes(x = ano, low = 20, high = 35),
          type = "arearange",
          dashStyle = "ShortDot",
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          fillOpacity = 0.2,
          enableMouseTracking = TRUE
        )
    })

    output$plot4_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[4]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_robson3_tx_cesariana, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        )
    })

    output$plot5_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[5]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_robson4_tx_cesariana, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        )
    })

    output$plot6_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[6]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          name = "Referência (meta OMS)",
          highcharter::hcaes(x = ano, low = 50, high = 60),
          type = "arearange",
          dashStyle = "ShortDot",
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          fillOpacity = 0.2,
          enableMouseTracking = TRUE
        )
    })

    output$plot7_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[7]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (média nacional)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_robson6_a_9_tx_cesariana, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        )
    })

    output$plot8_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[8]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_robson10_tx_cesariana, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8

        )
    })


    #### Porcentagem de nascidos vivos por grupo de Robson --------------------
    data4_juncao_aux <- reactive({
      if (filtros()$comparar == "Sim") {
        dplyr::full_join(
          data4(),
          data4_comp() |>
            dplyr::rename_with(~paste0("br_", .x), dplyr::starts_with("prop") | dplyr::starts_with("contrib")) |>
            dplyr::rename(localidade_comparacao = localidade),
          by = "ano"
        )
      } else {
        dplyr::full_join(data4(), data4_referencia(), by = "ano")
      }
    })

    output$plot1_indicador2 <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Sem informação",
          highcharter::hcaes(x = ano, y = prop_nasc_robson_faltante),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_nasc_robson_faltante:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 1 de Robson",
          highcharter::hcaes(x = ano, y = prop_nasc_robson1),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_nasc_robson1:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 2 de Robson",
          highcharter::hcaes(x = ano, y = prop_nasc_robson2),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_nasc_robson2:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 3 de Robson",
          highcharter::hcaes(x = ano, y = prop_nasc_robson3),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_nasc_robson3:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 4 de Robson",
          highcharter::hcaes(x = ano, y = prop_nasc_robson4),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_nasc_robson4:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 5 de Robson",
          highcharter::hcaes(x = ano, y = prop_nasc_robson5),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_nasc_robson5:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupos 6 a 9 de Robson",
          highcharter::hcaes(x = ano, y = prop_nasc_robson6_a_9),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_nasc_robson6_a_9:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 10 de Robson",
          highcharter::hcaes(x = ano, y = prop_nasc_robson10),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_nasc_robson10:,f}% </b>"
          )
        ) |>
        highcharter::hc_plotOptions(column = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(10, direction = -1)[-c(1, 10)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)


    })


    ### Contribuição relativa de cada grupo de Robson para a taxa global de cesarianas ----
    output$plot1_indicador3 <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Sem informação",
          highcharter::hcaes(x = ano, y = contrib_robson_faltante_tx_cesariana),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_contrib_robson_faltante_tx_cesariana:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 1 de Robson",
          highcharter::hcaes(x = ano, y = contrib_robson1_tx_cesariana),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_contrib_robson1_tx_cesariana:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 2 de Robson",
          highcharter::hcaes(x = ano, y = contrib_robson2_tx_cesariana),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_contrib_robson2_tx_cesariana:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 3 de Robson",
          highcharter::hcaes(x = ano, y = contrib_robson3_tx_cesariana),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_contrib_robson3_tx_cesariana:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 4 de Robson",
          highcharter::hcaes(x = ano, y = contrib_robson4_tx_cesariana),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_contrib_robson4_tx_cesariana:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 5 de Robson",
          highcharter::hcaes(x = ano, y = contrib_robson5_tx_cesariana),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_contrib_robson5_tx_cesariana:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupos 6 a 9 de Robson",
          highcharter::hcaes(x = ano, y = contrib_robson6_a_9_tx_cesariana),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_contrib_robson6_a_9_tx_cesariana:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data4_juncao_aux(),
          name = "Grupo 10 de Robson",
          highcharter::hcaes(x = ano, y = contrib_robson10_tx_cesariana),
          type = "column",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_contrib_robson10_tx_cesariana:,f}% </b>"
          )
        ) |>
        highcharter::hc_plotOptions(column = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(10, direction = -1)[-c(1, 10)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "Contribuição % para a taxa global de cesarianas"), min = 0, max = 100)


    })

    ### Porcentagem de nascidos vivos segundo local de ocorrência do parto ----
    output$grafico_deslocamento_muni_prop1 <- output$grafico_deslocamento_resto_prop1 <- output$grafico_deslocamento_uf_prop1 <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          name = "No município de residência",
          data = data4_deslocamento_juncao(),
          type = "column",
          highcharter::hcaes(x = ano, y = prop_partos_municipio_res),
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_partos_municipio_res:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Na microrregião de saúde, mas fora do município de residência",
          data = data4_deslocamento_juncao(),
          type = "column",
          highcharter::hcaes(x = ano, y = prop_partos_rsaude_res),
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_partos_rsaude_res:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Na macrorregião de saúde, mas fora da microrregião de saúde de residência",
          data = data4_deslocamento_juncao(),
          type = "column",
          highcharter::hcaes(x = ano, y = prop_partos_macro_rsaude_res),
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_partos_macro_rsaude_res:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Fora da macrorregião de saúde, mas na UF de residência",
          data = data4_deslocamento_juncao(),
          type = "column",
          highcharter::hcaes(x = ano, y = prop_partos_fora_macro_rsaude_res),
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_partos_fora_macro_rsaude_res:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Fora da UF de residência",
          data = data4_deslocamento_juncao(),
          type = "column",
          highcharter::hcaes(x = ano, y = prop_partos_fora_uf_res),
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_prop_partos_fora_uf_res:,f}% </b>"
          )
        ) |>
        highcharter::hc_plotOptions(column = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)
    })

    ### Mediana de deslocamento para o destino, segundo destinos --------------
    output$grafico_deslocamento_muni_med1 <- output$grafico_deslocamento_uf_med1 <- highcharter::renderHighchart({
      if (filtros()$nivel %in% c("Municipal", "Estadual")) {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            name = "Total de partos",
            data = data4_deslocamento_med(),
            type = "line",
            highcharter::hcaes(x = ano, y = no_local),
            legendIndex = 1,
            index = 1
          ) |>
          highcharter::hc_add_series(
            name = "Serviços de baixa complexidade",
            data = data4_deslocamento_med(),
            type = "line",
            highcharter::hcaes(x = ano, y = baixa_complexidade),
            legendIndex = 2,
            index = 2
          ) |>
          highcharter::hc_add_series(
            name = "Serviços de alta complexidade",
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
      }
    })

    ### Tabela com informações adicionais -------------------------------------
    output$infos_deslocamento_muni <- renderUI({
      if (filtros()$nivel == "Municipal") {
        data_infos_deslocamento <- bloco4_deslocamento_muni |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
          ) |>
          dplyr::group_by(ano) |>
          dplyr::mutate(
            prop_partos_muni_maior_ocorrencia = round(n_nasc1/nao_local * 100, 1),
            prop_partos_muni_2_maior_ocorrencia = round(n_nasc2/nao_local * 100, 1),
            prop_partos_muni_3_maior_ocorrencia = round(n_nasc3/nao_local * 100, 1),
            .keep = "unused"
          ) |>
          dplyr::ungroup()

        municipio1 <- municipio2 <- municipio3 <- NULL

        for (i in 1:length(filtros()$ano2[1]:min(filtros()$ano2[2], 2023))) {
          municipio1[i] <- ifelse(
            !is.na(data_infos_deslocamento$codmunnasc1[i]),
            tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data_infos_deslocamento$codmunnasc1[i])],
            NA
          )

          municipio2[i] <- ifelse(
            !is.na(data_infos_deslocamento$codmunnasc2[i]),
            tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data_infos_deslocamento$codmunnasc2[i])],
            NA
          )

          municipio3[i] <- ifelse(
            !is.na(data_infos_deslocamento$codmunnasc3[i]),
            tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data_infos_deslocamento$codmunnasc3[i])],
            NA
          )
        }

        partos_municipio1 <- data_infos_deslocamento$prop_partos_muni_maior_ocorrencia
        partos_municipio2 <- data_infos_deslocamento$prop_partos_muni_2_maior_ocorrencia
        partos_municipio3 <- data_infos_deslocamento$prop_partos_muni_3_maior_ocorrencia
        cnes <- data_infos_deslocamento$cnes
        estabelecimento <- data_infos_deslocamento$nome_estabelecimento_fantasia
        partos_estabelecimento <- data_infos_deslocamento$nasc_estab

        ano <- filtros()$ano2[1]:min(filtros()$ano2[2], 2023)
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
            height = 600
          )
      }
    })

    ### Porcentagem de nascidos vivos com peso < 1500g segundo local de ocorrência do parto ----
    output$grafico_deslocamento_macrorregiao_1 <- output$grafico_deslocamento_macrorregiao_2 <-
      output$grafico_deslocamento_macrorregiao_3 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_completo(),
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
            data = data4_deslocamento_macro_completo(),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data4_deslocamento_macro_comp_completo(),
            highcharter::hcaes(x = ano, y = prop_indicador, group = indicador),
            type = "column",
            showInLegend = FALSE,
            tooltip = list(

              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_indicador:,f}% </b>"
            ),
            stack = 1
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(8, direction = -1)[-c(1, 8)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data4_deslocamento_macro_completo()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos com peso < 1500g"), min = 0, max = 100)
    })

  })
}



## To be copied in the UI
# mod_bloco_4_ui("bloco_4_1")

## To be copied in the server
# mod_bloco_4_server("bloco_4_1")

