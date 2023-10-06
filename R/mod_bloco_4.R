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
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Assistência ao parto: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;"),
      style = "position: fixed; top: 56px; width: 93.75%; background-color: white; z-index: 100;"
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
              label = "Indicador",
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
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_indicador1"), height = 340)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_indicador1"), height = 340)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_indicador1"), height = 340)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot7_indicador1"), height = 339))
                  ),
                  column(
                    width = 6,
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_indicador1"), height = 340)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_indicador1"), height = 340)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_indicador1"), height = 340)),
                    shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot8_indicador1"), height = 339))
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
                  offset = 3,
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_deslocamento_resto")), proxy.height = "332px")
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
              )
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
                  offset = 3,
                  width = 6,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_deslocamento_muni")), proxy.height = "332px")
                )#,
                # column(
                #   width = 6,
                #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_deslocamento_muni")), proxy.height = "332px")
                # )
              )#,
              # fluidRow(
              #   column(
              #     width = 6,
              #     shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_deslocamento_muni")), proxy.height = "332px")
              #   ),
              #   column(
              #     width = 6,
              #     shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i8_deslocamento_muni")), proxy.height = "332px")
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

    output$comparar <- renderText({filtros()$comparar})
    output$nivel <- renderText({filtros()$nivel})

    outputOptions(output, "comparar", suspendWhenHidden = FALSE)
    outputOptions(output, "nivel", suspendWhenHidden = FALSE)

    ##### Criando o output que recebe a localidade e o ano escolhidos ####
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

    ##### Dados para o resumo do período para a localidade escolhida #####
    output$input_localidade_resumo1 <- renderUI({
      localidade_original <- dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )

      localidade_comparacao <- dplyr::case_when(
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
          inputId = ns("localidade_resumo1"),
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

    output$input_localidade_resumo2 <- renderUI({
      localidade_original <- dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )

      localidade_comparacao <- dplyr::case_when(
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
          inputId = ns("localidade_resumo2"),
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


    output$input_localidade_resumo3 <- renderUI({
      localidade_original <- dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )

      localidade_comparacao <- dplyr::case_when(
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
          inputId = ns("localidade_resumo3"),
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

    output$input_localidade_resumo4 <- renderUI({
      localidade_original <- dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )

      localidade_comparacao <- dplyr::case_when(
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
          inputId = ns("localidade_resumo4"),
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

    output$input_localidade_resumo5 <- renderUI({
      localidade_original <- dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )

      localidade_comparacao <- dplyr::case_when(
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
          inputId = ns("localidade_resumo5"),
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

    data4_resumo <- reactive({
      bloco4 |>
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
            req(input$indicador_robson)
            if (input$indicador_robson == "indicador1") {
              req(input$localidade_resumo1)
              if (input$localidade_resumo1 == "escolha1") {
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
            } else if (input$indicador_robson == "indicador2") {
              req(input$localidade_resumo2)
              if (input$localidade_resumo2 == "escolha1") {
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
            } else if (input$indicador_robson == "indicador3") {
              req(input$localidade_resumo3)
              if (input$localidade_resumo3 == "escolha1") {
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
          }
        ) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 1),
          prop_tx_cesariana_geral = round(mulheres_com_parto_cesariana/total_de_nascidos_vivos * 100, 1),
          prop_robson1_tx_cesariana = round((sum(total_cesariana_grupo_robson_1) / sum(mulheres_dentro_do_grupo_de_robson_1)) * 100, 1),
          prop_robson2_tx_cesariana = round((sum(total_cesariana_grupo_robson_2) / sum(mulheres_dentro_do_grupo_de_robson_2)) * 100, 1),
          prop_robson3_tx_cesariana = round((sum(total_cesariana_grupo_robson_3) / sum(mulheres_dentro_do_grupo_de_robson_3)) * 100, 1),
          prop_robson4_tx_cesariana = round((sum(total_cesariana_grupo_robson_4) / sum(mulheres_dentro_do_grupo_de_robson_4)) * 100, 1),
          prop_robson5_tx_cesariana = round((sum(total_cesariana_grupo_robson_5) / sum(mulheres_dentro_do_grupo_de_robson_5)) * 100, 1),
          prop_robson6_a_9_tx_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1),
          prop_robson10_tx_cesariana = round((sum(total_cesariana_grupo_robson_10) / sum(mulheres_dentro_do_grupo_de_robson_10)) * 100, 1),
          contrib_robson1_tx_cesariana = round(sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson2_tx_cesariana = round(sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson3_tx_cesariana = round(sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson4_tx_cesariana = round(sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson5_tx_cesariana = round(sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson6_a_9_tx_cesariana = round(sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson10_tx_cesariana = round(sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana * 100, 1)
        )
    })


    ##### Dados para o resumo do período para a comparação com o Brasil #####
    data4_brasil_resumo <- reactive({
      bloco4 |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 1),
          prop_tx_cesariana_geral = round(mulheres_com_parto_cesariana/total_de_nascidos_vivos * 100, 1),
          prop_robson1_tx_cesariana = round((sum(total_cesariana_grupo_robson_1) / sum(mulheres_dentro_do_grupo_de_robson_1)) * 100, 1),
          prop_robson2_tx_cesariana = round((sum(total_cesariana_grupo_robson_2) / sum(mulheres_dentro_do_grupo_de_robson_2)) * 100, 1),
          prop_robson3_tx_cesariana = round((sum(total_cesariana_grupo_robson_3) / sum(mulheres_dentro_do_grupo_de_robson_3)) * 100, 1),
          prop_robson4_tx_cesariana = round((sum(total_cesariana_grupo_robson_4) / sum(mulheres_dentro_do_grupo_de_robson_4)) * 100, 1),
          prop_robson5_tx_cesariana = round((sum(total_cesariana_grupo_robson_5) / sum(mulheres_dentro_do_grupo_de_robson_5)) * 100, 1),
          prop_robson6_a_9_tx_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1),
          prop_robson10_tx_cesariana = round((sum(total_cesariana_grupo_robson_10) / sum(mulheres_dentro_do_grupo_de_robson_10)) * 100, 1),
          contrib_robson1_tx_cesariana = round(sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson2_tx_cesariana = round(sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson3_tx_cesariana = round(sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson4_tx_cesariana = round(sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson5_tx_cesariana = round(sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson6_a_9_tx_cesariana = round(sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson10_tx_cesariana = round(sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana * 100, 1)
        )
    })


    ##### Dados dos valores de referência de cada indicador #####
    data4_referencia <- reactive({
      bloco4 |>
        dplyr::filter(ano >= max(filtros()$ano2[1], 2014) & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "Nacional")
            ano >= max(filtros()$ano2[1], 2014) & ano <= filtros()$ano2[2]
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
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          br_prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 1),
          br_prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 1),
          br_prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 1),
          br_prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 1),
          br_prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 1),
          br_prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 1),
          br_prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 1),
          br_prop_nasc_robson_faltante = round((total_de_nascidos_vivos - sum(dplyr::across(dplyr::starts_with("mulheres_dentro")))) / total_de_nascidos_vivos * 100, 1),
          br_prop_tx_cesariana_geral_oms = 15,
          br_prop_tx_cesariana_geral_ajustada = 25,
          br_prop_robson1_tx_cesariana = 10,
          br_prop_robson2_tx_cesariana = 35,
          br_prop_robson3_tx_cesariana = 3,
          br_prop_robson4_tx_cesariana = 15,
          br_prop_robson5_tx_cesariana = 60,
          br_prop_robson6_a_9_tx_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1),
          br_prop_robson10_tx_cesariana = 30,
          br_contrib_robson1_tx_cesariana = round(sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana * 100, 1),
          br_contrib_robson2_tx_cesariana = round(sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana * 100, 1),
          br_contrib_robson3_tx_cesariana = round(sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana * 100, 1),
          br_contrib_robson4_tx_cesariana = round(sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana * 100, 1),
          br_contrib_robson5_tx_cesariana = round(sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana * 100, 1),
          br_contrib_robson6_a_9_tx_cesariana = round(sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana * 100, 1),
          br_contrib_robson10_tx_cesariana = round(sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana * 100, 1),
          br_contrib_robson_faltante_tx_cesariana = round((mulheres_com_parto_cesariana - sum(dplyr::across(dplyr::starts_with("total_cesariana")))) / mulheres_com_parto_cesariana * 100, 1),
          localidade_comparacao = dplyr::if_else(
            filtros()$comparar == "Não",
            "Média nacional",
            dplyr::case_when(
              filtros()$nivel2 == "Nacional" ~ "Média nacional",
              filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
              filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
              filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
              filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
              filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
              filtros()$nivel2 == "Municípios semelhantes" ~ "Média dos municípios semelhantes",
            )
          )
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas da porcentagem de nascidos vivos por cesariana por grupo de Robson #####
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
        valor_de_referencia = data4_brasil_resumo()$prop_robson6_a_9_tx_cesariana,
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


    ##### Criando as caixinhas da porcentagem de nascidos vivos por grupo de Robson #####
    output$caixa_b4_i1_indicador2 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "prop_nasc_robson1",
        titulo = "Porcentagem de nascidos vivos no grupo 1 de Robson",
        tem_meta = FALSE,
        valor_de_referencia = data4_brasil_resumo()$prop_nasc_robson1,
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
        valor_de_referencia = data4_brasil_resumo()$prop_nasc_robson2,
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
        valor_de_referencia = data4_brasil_resumo()$prop_nasc_robson3,
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
        valor_de_referencia = data4_brasil_resumo()$prop_nasc_robson4,
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
        valor_de_referencia = data4_brasil_resumo()$prop_nasc_robson5,
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
        valor_de_referencia = data4_brasil_resumo()$prop_nasc_robson6_a_9,
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
        valor_de_referencia = data4_brasil_resumo()$prop_nasc_robson10,
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


    ##### Criando as caixinhas da contribuição relativa de cada grupo de Robson para a taxa global de cesarianas #####
    output$caixa_b4_i1_indicador3 <- renderUI({
      cria_caixa_server(
        dados = data4_resumo(),
        indicador = "contrib_robson1_tx_cesariana",
        titulo = "Contribuição relativa do grupo 1 de Robson para a taxa global de cesarianas",
        tem_meta = FALSE,
        valor_de_referencia = data4_brasil_resumo()$contrib_robson1_tx_cesariana,
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
        valor_de_referencia = data4_brasil_resumo()$contrib_robson2_tx_cesariana,
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
        valor_de_referencia = data4_brasil_resumo()$contrib_robson3_tx_cesariana,
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
        valor_de_referencia = data4_brasil_resumo()$contrib_robson4_tx_cesariana,
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
        valor_de_referencia = data4_brasil_resumo()$contrib_robson5_tx_cesariana,
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
        valor_de_referencia = data4_brasil_resumo()$contrib_robson6_a_9_tx_cesariana,
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
        valor_de_referencia = data4_brasil_resumo()$contrib_robson10_tx_cesariana,
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


    ##### Dados do quarto bloco de indicadores para a localidade escolhida #####
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson_faltante = round((total_de_nascidos_vivos - sum(dplyr::across(dplyr::starts_with("mulheres_dentro")))) / total_de_nascidos_vivos * 100, 1),
          prop_tx_cesariana_geral = round(mulheres_com_parto_cesariana/total_de_nascidos_vivos * 100, 1),
          prop_robson1_tx_cesariana = round((sum(total_cesariana_grupo_robson_1) / sum(mulheres_dentro_do_grupo_de_robson_1)) * 100, 1),
          prop_robson2_tx_cesariana = round((sum(total_cesariana_grupo_robson_2) / sum(mulheres_dentro_do_grupo_de_robson_2)) * 100, 1),
          prop_robson3_tx_cesariana = round((sum(total_cesariana_grupo_robson_3) / sum(mulheres_dentro_do_grupo_de_robson_3)) * 100, 1),
          prop_robson4_tx_cesariana = round((sum(total_cesariana_grupo_robson_4) / sum(mulheres_dentro_do_grupo_de_robson_4)) * 100, 1),
          prop_robson5_tx_cesariana = round((sum(total_cesariana_grupo_robson_5) / sum(mulheres_dentro_do_grupo_de_robson_5)) * 100, 1),
          prop_robson6_a_9_tx_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1),
          prop_robson10_tx_cesariana = round((sum(total_cesariana_grupo_robson_10) / sum(mulheres_dentro_do_grupo_de_robson_10)) * 100, 1),
          contrib_robson1_tx_cesariana = round(sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson2_tx_cesariana = round(sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson3_tx_cesariana = round(sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson4_tx_cesariana = round(sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson5_tx_cesariana = round(sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson6_a_9_tx_cesariana = round(sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson10_tx_cesariana = round(sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson_faltante_tx_cesariana = round((mulheres_com_parto_cesariana - sum(dplyr::across(dplyr::starts_with("total_cesariana")))) / mulheres_com_parto_cesariana * 100, 1),
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


    ##### Dados do quarto bloco de indicadores para quando há comparação #####
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson_faltante = round((total_de_nascidos_vivos - sum(dplyr::across(dplyr::starts_with("mulheres_dentro")))) / total_de_nascidos_vivos * 100, 1),
          prop_tx_cesariana_geral = round(mulheres_com_parto_cesariana/total_de_nascidos_vivos * 100, 1),
          prop_robson1_tx_cesariana = round((sum(total_cesariana_grupo_robson_1) / sum(mulheres_dentro_do_grupo_de_robson_1)) * 100, 1),
          prop_robson2_tx_cesariana = round((sum(total_cesariana_grupo_robson_2) / sum(mulheres_dentro_do_grupo_de_robson_2)) * 100, 1),
          prop_robson3_tx_cesariana = round((sum(total_cesariana_grupo_robson_3) / sum(mulheres_dentro_do_grupo_de_robson_3)) * 100, 1),
          prop_robson4_tx_cesariana = round((sum(total_cesariana_grupo_robson_4) / sum(mulheres_dentro_do_grupo_de_robson_4)) * 100, 1),
          prop_robson5_tx_cesariana = round((sum(total_cesariana_grupo_robson_5) / sum(mulheres_dentro_do_grupo_de_robson_5)) * 100, 1),
          prop_robson6_a_9_tx_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1),
          prop_robson10_tx_cesariana = round((sum(total_cesariana_grupo_robson_10) / sum(mulheres_dentro_do_grupo_de_robson_10)) * 100, 1),
          contrib_robson1_tx_cesariana = round(sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson2_tx_cesariana = round(sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson3_tx_cesariana = round(sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson4_tx_cesariana = round(sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson5_tx_cesariana = round(sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson6_a_9_tx_cesariana = round(sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson10_tx_cesariana = round(sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson_faltante_tx_cesariana = round((mulheres_com_parto_cesariana - sum(dplyr::across(dplyr::starts_with("total_cesariana")))) / mulheres_com_parto_cesariana * 100, 1),
          "localidade" = dplyr::case_when(
            filtros()$nivel2 == "Nacional" ~ "Brasil",
            filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
            filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
            filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "Municípios semelhantes" ~ "Média dos municípios semelhantes"
          )
        ) |>
        dplyr::ungroup()
    })


    ##### Junção das duas bases de dados (para quando há comparação) #####
    data4_juncao <- reactive({
      rbind(data4(), data4_comp())
    })


    ##### Passando os dados da localidade escolhida para o formato long p/ a construção dos gráficos de barras #####
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

    ##### Passando os dados da localidade de comparação para o formato long p/ a construção dos gráficos de barras #####
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


    ##### Passando os dados da junção entre as localidades para o formato long p/ a construção dos gráficos de barras #####
    data4_long_juncao <- reactive({
      data4_juncao() |>
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


    ##### Construindo os gráficos de barras para o primeiro indicador de Robson #####
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
            highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
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
            highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
            highcharter::hc_yAxis(title = list(text = "% de nascidos vivos por cesariana"), min = 0, max = 100)
        }

      })
    })


    ##### Criando os outputs para o primeiro indicador de Robson #####
    output$plot1_indicador1 <- highcharter::renderHighchart({
      list_of_plots()[[1]] |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (meta OMS)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_tx_cesariana_geral_oms, group = localidade_comparacao),
          color = dplyr::if_else(filtros()$comparar == "Não", true = "#b73779", false = "black"),
          showInLegend = TRUE,
          opacity = 0.8
        ) |>
        highcharter::hc_add_series(
          data = data4_referencia(),
          type = "line",
          name = "Referência (meta ajustada para o Brasil)",
          dashStyle = "ShortDot",
          highcharter::hcaes(x = ano, y = br_prop_tx_cesariana_geral_ajustada, group = localidade_comparacao),
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


    ##### Criando os outputs para o segundo indicador de Robson #####
    data4_juncao_aux <- reactive({dplyr::full_join(data4(), data4_referencia(), by = "ano")})

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
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)


    })


    ##### Criando os outputs para o terceiro indicador de Robson #####
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
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "Contribuição % para a taxa global de cesarianas"), min = 0, max = 100)


    })

    output$grafico_deslocamento_muni_prop1 <- output$grafico_deslocamento_resto_prop1 <- highcharter::renderHighchart({
      tryCatch({
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
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100) #|>
        #highcharter::hc_legend(width = 400, itemWidth = 200, align = "center", itemStyle = list(width = 250))
      },
      error = function(e) {}
      )
    })

    output$grafico_deslocamento_uf_prop1 <- highcharter::renderHighchart({
      tryCatch({
        highcharter::highchart() |>
          highcharter::hc_add_series(
            name = "No município de residência",
            data = data4_deslocamento_juncao(),
            type = "column",
            highcharter::hcaes(x = ano, y = prop_partos_municipio_res),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_partos_municipio_res:,f}% </b>"
            )
          ) |>
          highcharter::hc_add_series(
            name = "Na microrregião de saúde, mas fora do município de residência",
            data = data4_deslocamento_juncao(),
            type = "column",
            highcharter::hcaes(x = ano, y = prop_partos_rsaude_res),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_partos_rsaude_res:,f}% </b>"
            )
          ) |>
          highcharter::hc_add_series(
            name = "Na macrorregião de saúde, mas fora da microrregião de saúde de residência",
            data = data4_deslocamento_juncao(),
            type = "column",
            highcharter::hcaes(x = ano, y = prop_partos_macro_rsaude_res),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_partos_macro_rsaude_res:,f}% </b>"
            )
          ) |>
          highcharter::hc_add_series(
            name = "Fora da macrorregião de saúde, mas na UF de residência",
            data = data4_deslocamento_juncao(),
            type = "column",
            highcharter::hcaes(x = ano, y = prop_partos_fora_macro_rsaude_res),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_partos_fora_macro_rsaude_res:,f}% </b>"
            )
          ) |>
          highcharter::hc_add_series(
            name = "Fora da UF de residência",
            data = data4_deslocamento_juncao(),
            type = "column",
            highcharter::hcaes(x = ano, y = prop_partos_fora_uf_res),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_partos_fora_uf_res:,f}% </b>"
            )
          ) |>
          highcharter::hc_plotOptions(column = list(stacking = "percent")) |>
          highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Contribuição % para a taxa global de cesarianas"), min = 0, max = 100)
      },
      error = function(e) {}
      )
    })

    output$grafico_deslocamento_muni_med1 <- highcharter::renderHighchart({
      tryCatch({
        highcharter::highchart() |>
          highcharter::hc_add_series(
            name = "Total de partos",
            data = data4_deslocamento_muni_med(),
            type = "line",
            highcharter::hcaes(x = ano, y = no_local),
            legendIndex = 1,
            index = 1
          ) |>
          highcharter::hc_add_series(
            name = "Serviços de baixa complexidade",
            data = data4_deslocamento_muni_med(),
            type = "line",
            highcharter::hcaes(x = ano, y = baixa_complexidade),
            legendIndex = 2,
            index = 2
          ) |>
          highcharter::hc_add_series(
            name = "Serviços de alta complexidade",
            data = data4_deslocamento_muni_med(),
            type = "line",
            highcharter::hcaes(x = ano, y = alta_complexidade),
            legendIndex = 3,
            index = 3
          ) |>
          highcharter::hc_tooltip(valueSuffix = " km", shared = TRUE, sort = TRUE, valueDecimals = 2) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "km"), min = 0) |>
          #highcharter::hc_add_theme(highcharter::hc_theme_elementary()) |>
          highcharter::hc_colors(cols)
      },
      error = function(e) {}
      )
    })

    output$grafico_deslocamento_uf_med1 <- highcharter::renderHighchart({
      tryCatch({
        highcharter::highchart() |>
          highcharter::hc_add_series(
            name = "Total de partos",
            data = data4_deslocamento_uf_med(),
            type = "line",
            highcharter::hcaes(x = ano, y = no_local),
            legendIndex = 1,
            index = 1
          ) |>
          highcharter::hc_add_series(
            name = "Serviços de baixa complexidade",
            data = data4_deslocamento_uf_med(),
            type = "line",
            highcharter::hcaes(x = ano, y = baixa_complexidade),
            legendIndex = 2,
            index = 2
          ) |>
          highcharter::hc_add_series(
            name = "Serviços de alta complexidade",
            data = data4_deslocamento_uf_med(),
            type = "line",
            highcharter::hcaes(x = ano, y = alta_complexidade),
            legendIndex = 3,
            index = 3
          ) |>
          highcharter::hc_tooltip(valueSuffix = " km", shared = TRUE, sort = TRUE, valueDecimals = 2) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "km"), min = 0) |>
          #highcharter::hc_add_theme(highcharter::hc_theme_elementary()) |>
          highcharter::hc_colors(cols)
      },
      error = function(e) {}
      )
    })

    output$grafico_deslocamento_muni_med2 <- highcharter::renderHighchart({
      tryCatch({
        highcharter::highchart() |>
          highcharter::hc_add_series(
            name = "Total de partos",
            data = data4_deslocamento_muni_med2(),
            type = "line",
            highcharter::hcaes(x = ano, y = no_local),
            legendIndex = 1,
            index = 1
          ) |>
          highcharter::hc_add_series(
            name = "Serviços de baixa complexidade",
            data = data4_deslocamento_muni_med2(),
            type = "line",
            highcharter::hcaes(x = ano, y = baixa_complexidade),
            legendIndex = 2,
            index = 2
          ) |>
          highcharter::hc_add_series(
            name = "Serviços de alta complexidade",
            data = data4_deslocamento_muni_med2(),
            type = "line",
            highcharter::hcaes(x = ano, y = alta_complexidade),
            legendIndex = 3,
            index = 3
          ) |>
          highcharter::hc_tooltip(valueSuffix = " km", shared = TRUE, sort = TRUE, valueDecimals = 2) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "km"), min = 0) |>
          #highcharter::hc_add_theme(highcharter::hc_theme_elementary()) |>
          highcharter::hc_colors(cols)
      },
      error = function(e) {}
      )
    })

    output$infos_deslocamento_muni <- renderUI({
      if (filtros()$nivel == "Municipal") {
        municipio1 <- municipio2 <- municipio3 <- NULL

        for (i in 1:length(filtros()$ano2[1]:filtros()$ano2[2])) {
          municipio1[i] <- tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data4_deslocamento()$codmunnasc1[i])]
          #uf_municipio_1[i] <- tabela_aux_municipios$uf[which(tabela_aux_municipios$codmunres == data4_deslocamento$codmunnasc1[i])]
          municipio2[i] <- tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data4_deslocamento()$codmunnasc2[i])]
          #uf_municipio_2[i] <- tabela_aux_municipios$uf[which(tabela_aux_municipios$codmunres == data4_deslocamento$codmunnasc2[i])]
          municipio3[i] <- tabela_aux_municipios$municipio[which(tabela_aux_municipios$codmunres == data4_deslocamento()$codmunnasc3[i])]
          #uf_municipio_3[i] <- tabela_aux_municipios$uf[which(tabela_aux_municipios$codmunres == data4_deslocamento$codmunnasc3[i])]

        }

        partos_municipio1 <- data4_deslocamento()$prop_partos_muni_maior_ocorrencia
        partos_municipio2 <- data4_deslocamento()$prop_partos_muni_2_maior_ocorrencia
        partos_municipio3 <- data4_deslocamento()$prop_partos_muni_3_maior_ocorrencia
        cnes <- data4_deslocamento()$cnes
        estabelecimento <- data4_deslocamento()$nome_estabelecimento_fantasia
        partos_estabelecimento <- data4_deslocamento()$nasc_estab

        ano <- filtros()$ano2[1]:filtros()$ano2[2]
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
            pagination = FALSE
          )
      }
    })

    data4_deslocamento_resumo <- reactive({
      bloco4_deslocamento_muni |>
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
            if (filtros()$nivel != "Estadual" & filtros()$nivel != "Municipal") {
              req(input$localidade_resumo4)
              if (input$localidade_resumo4 == "escolha1") {
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
            } else {
              req(input$localidade_resumo5)
              if (input$localidade_resumo5 == "escolha1") {
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
          }
        ) |>
        dplyr::summarise(
          prop_partos_municipio_res = round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_municipio_res = round(sum(nao_local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_rsaude_res = round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_macro_rsaude_res = round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_uf_res = round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    data4_deslocamento_resumo_brasil <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(ano >= 2014 & ano <= filtros()$ano2[2]) |>
        dplyr::summarise(
          prop_partos_municipio_res = round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_municipio_res = round(sum(nao_local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_rsaude_res = round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_macro_rsaude_res = round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_uf_res = round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          localidade = "Brasil"
        ) |>
        dplyr::ungroup()
    })


    data4_deslocamento_resumo_med <- reactive({
      if (filtros()$nivel == "Municipal") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = paste0("km_partos_", input$local_med_muni),
            baixa_complexidade = paste0("km_partos_", input$local_med_muni, "_baixa_complexidade"),
            alta_complexidade = paste0("km_partos_", input$local_med_muni, "_alta_complexidade")
          ) |>
          dplyr::summarise(
            no_local = round(sum(no_local, na.rm = TRUE)/(length(filtros()$ano2[1]:filtros()$ano2[2]) + 1), 1),
            baixa_complexidade = round(sum(baixa_complexidade, na.rm = TRUE)/(length(filtros()$ano2[1]:filtros()$ano2[2]) + 1), 1),
            alta_complexidade = round(sum(alta_complexidade, na.rm = TRUE)/(length(filtros()$ano2[1]:filtros()$ano2[2]) + 1), 1)
          )
      } else if (filtros()$nivel == "Estadual") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = paste0("km_partos_", input$local_med_uf),
            baixa_complexidade = paste0("km_partos_", input$local_med_uf, "_baixa_complexidade"),
            alta_complexidade = paste0("km_partos_", input$local_med_uf, "_alta_complexidade")
          ) |>
          dplyr::summarise(
            no_local = round(sum(no_local, na.rm = TRUE)/(length(filtros()$ano2[1]:filtros()$ano2[2]) + 1), 1),
            baixa_complexidade = round(sum(baixa_complexidade, na.rm = TRUE)/(length(filtros()$ano2[1]:filtros()$ano2[2]) + 1), 1),
            alta_complexidade = round(sum(alta_complexidade, na.rm = TRUE)/(length(filtros()$ano2[1]:filtros()$ano2[2]) + 1), 1)
          )
      }

    })

    data4_deslocamento_resumo_med_brasil <- reactive({
      if (filtros()$nivel == "Municipal") {
        bloco4_deslocamento_uf |>
          dplyr::filter(
            uf == "Brasil",
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::select(
            ano,
            no_local = paste0("km_partos_", input$local_med_muni),
            baixa_complexidade = paste0("km_partos_", input$local_med_muni, "_baixa_complexidade"),
            alta_complexidade = paste0("km_partos_", input$local_med_muni, "_alta_complexidade")
          ) |>
          dplyr::summarise(
            no_local = round(sum(no_local, na.rm = TRUE)/length(filtros()$ano2[1]:filtros()$ano2[2]), 1),
            baixa_complexidade = round(sum(baixa_complexidade, na.rm = TRUE)/length(filtros()$ano2[1]:filtros()$ano2[2]), 1),
            alta_complexidade = round(sum(alta_complexidade, na.rm = TRUE)/length(filtros()$ano2[1]:filtros()$ano2[2]), 1)
          )
      } else if (filtros()$nivel == "Estadual") {
        bloco4_deslocamento_uf |>
          dplyr::filter(
            uf == "Brasil",
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::select(
            ano,
            no_local = paste0("km_partos_", input$local_med_uf),
            baixa_complexidade = paste0("km_partos_", input$local_med_uf, "_baixa_complexidade"),
            alta_complexidade = paste0("km_partos_", input$local_med_uf, "_alta_complexidade")
          ) |>
          dplyr::summarise(
            no_local = round(sum(no_local, na.rm = TRUE)/length(filtros()$ano2[1]:filtros()$ano2[2]), 1),
            baixa_complexidade = round(sum(baixa_complexidade, na.rm = TRUE)/length(filtros()$ano2[1]:filtros()$ano2[2]), 1),
            alta_complexidade = round(sum(alta_complexidade, na.rm = TRUE)/length(filtros()$ano2[1]:filtros()$ano2[2]), 1)
          )
      }

    })

    output$caixa_b4_i1_deslocamento_muni <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_municipio_res",
          titulo = "Porcentagem de partos ocorridos no município de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_municipio_res,
          tipo = "porcentagem",
          invertido = TRUE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
          pagina = "bloco_4",
          nivel_de_analise = ifelse(
            filtros()$comparar == "Não",
            filtros()$nivel,
            ifelse(
              input$localidade_resumo5 == "escolha1",
              filtros()$nivel,
              filtros()$nivel2
            )
          )
        )
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i1_deslocamento_resto <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_municipio_res",
          titulo = "Porcentagem de partos ocorridos no município de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_municipio_res,
          tipo = "porcentagem",
          invertido = TRUE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
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
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i2_deslocamento_muni <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_rsaude_res",
          titulo = "Porcentagem de partos ocorridos na microrregião de saúde, mas fora do município de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_rsaude_res,
          tipo = "porcentagem",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
          pagina = "bloco_4",
          nivel_de_analise = ifelse(
            filtros()$comparar == "Não",
            filtros()$nivel,
            ifelse(
              input$localidade_resumo5 == "escolha1",
              filtros()$nivel,
              filtros()$nivel2
            )
          )
        )
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i2_deslocamento_resto <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_rsaude_res",
          titulo = "Porcentagem de partos ocorridos na microrregião de saúde, mas fora do município de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_rsaude_res,
          tipo = "porcentagem",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
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
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i3_deslocamento_muni <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_macro_rsaude_res",
          titulo = "Porcentagem de partos ocorridos na macrorregião de saúde, mas fora da microrregião de saúde de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_macro_rsaude_res,
          tipo = "porcentagem",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
          pagina = "bloco_4",
          nivel_de_analise = ifelse(
            filtros()$comparar == "Não",
            filtros()$nivel,
            ifelse(
              input$localidade_resumo5 == "escolha1",
              filtros()$nivel,
              filtros()$nivel2
            )
          )
        )
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i3_deslocamento_resto <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_macro_rsaude_res",
          titulo = "Porcentagem de partos ocorridos na macrorregião de saúde, mas fora da microrregião de saúde de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_macro_rsaude_res,
          tipo = "porcentagem",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
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
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i4_deslocamento_muni <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_fora_macro_rsaude_res",
          titulo = "Porcentagem de partos ocorridos na UF, mas fora da macrorregião de saúde de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_fora_macro_rsaude_res,
          tipo = "porcentagem",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
          pagina = "bloco_4",
          nivel_de_analise = ifelse(
            filtros()$comparar == "Não",
            filtros()$nivel,
            ifelse(
              input$localidade_resumo5 == "escolha1",
              filtros()$nivel,
              filtros()$nivel2
            )
          )
        )
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i4_deslocamento_resto <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_fora_macro_rsaude_res",
          titulo = "Porcentagem de partos ocorridos na UF, mas fora da macrorregião de saúde de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_fora_macro_rsaude_res,
          tipo = "porcentagem",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
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
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i5_deslocamento_muni <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_fora_uf_res",
          titulo = "Porcentagem de partos ocorridos fora da UF de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_fora_uf_res,
          tipo = "porcentagem",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
          pagina = "bloco_4",
          nivel_de_analise = ifelse(
            filtros()$comparar == "Não",
            filtros()$nivel,
            ifelse(
              input$localidade_resumo5 == "escolha1",
              filtros()$nivel,
              filtros()$nivel2
            )
          )
        )
      },
      error = function(e) {}
      )
    })

    output$caixa_b4_i5_deslocamento_resto <- renderUI({
      tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo(),
          indicador = "prop_partos_fora_uf_res",
          titulo = "Porcentagem de partos ocorridos fora da UF de residência",
          tem_meta = FALSE,
          valor_de_referencia = data4_deslocamento_resumo_brasil()$prop_partos_fora_uf_res,
          tipo = "porcentagem",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
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
      },
      error = function(e) {}
      )
    })

    titulo_caixinhas_mediana <- reactive({
      if (filtros()$nivel == "Municipal") {
        dplyr::case_when(
          input$local_med_muni == "fora_municipio" ~ "fora do município de residência da mulher",
          input$local_med_muni == "na_regiao" ~ "na microrregião de saúde, mas fora do município de residência da mulher",
          input$local_med_muni == "na_macrorregiao" ~ "na macrorregião de saúde, mas fora da microrregião de saúde de residência mulher",
          input$local_med_muni == "fora_macrorregiao" ~ "na UF, mas fora da macrorregião de saúde de residência mulher",
          input$local_med_muni == "fora_uf" ~ "fora da UF de residência da mulher"
        )
      } else if (filtros()$nivel == "Estadual") {
        dplyr::case_when(
          input$local_med_uf == "fora_municipio" ~ "fora do município de residência da mulher",
          input$local_med_uf == "na_regiao" ~ "na microrregião de saúde, mas fora do município de residência da mulher",
          input$local_med_uf == "na_macrorregiao" ~ "na macrorregião de saúde, mas fora da microrregião de saúde de residência mulher",
          input$local_med_uf == "fora_macrorregiao" ~ "na UF, mas fora da macrorregião de saúde de residência mulher",
          input$local_med_uf == "fora_uf" ~ "fora da UF de residência da mulher"
        )
      }
    })

    output$caixa_b4_i6_deslocamento_muni <- renderUI({
      # tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo_med(),
          indicador = "no_local",
          titulo = glue::glue("Mediana de deslocamento do total de partos ocorridos {titulo_caixinhas_mediana()}"),
          tem_meta = FALSE,
          valor_de_referencia = NaN,
          tipo = "km",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
          pagina = "bloco_4",
          nivel_de_analise = ifelse(
            filtros()$comparar == "Não",
            filtros()$nivel,
            ifelse(
              input$localidade_resumo5 == "escolha1",
              filtros()$nivel,
              filtros()$nivel2
            )
          )
        )
      # },
      # error = function(e) {}
      # )
    })

    output$caixa_b4_i7_deslocamento_muni <- renderUI({
      # tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo_med(),
          indicador = "baixa_complexidade",
          titulo = glue::glue("Mediana de deslocamento para serviços de baixa complexidade do total de partos ocorridos {titulo_caixinhas_mediana()}"),
          tem_meta = FALSE,
          valor_de_referencia = NaN,
          tipo = "km",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
          pagina = "bloco_4",
          nivel_de_analise = ifelse(
            filtros()$comparar == "Não",
            filtros()$nivel,
            ifelse(
              input$localidade_resumo5 == "escolha1",
              filtros()$nivel,
              filtros()$nivel2
            )
          )
        )
      # },
      # error = function(e) {}
      # )
    })

    output$caixa_b4_i8_deslocamento_muni <- renderUI({
      # tryCatch({
        cria_caixa_server(
          dados = data4_deslocamento_resumo_med(),
          indicador = "alta_complexidade",
          titulo = glue::glue("Mediana de deslocamento para serviços de alta complexidade do total de partos ocorridos {titulo_caixinhas_mediana()}"),
          tem_meta = FALSE,
          valor_de_referencia = NaN,
          tipo = "km",
          invertido = FALSE,
          tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
          fonte_titulo = "15px",
          pagina = "bloco_4",
          nivel_de_analise = ifelse(
            filtros()$comparar == "Não",
            filtros()$nivel,
            ifelse(
              input$localidade_resumo5 == "escolha1",
              filtros()$nivel,
              filtros()$nivel2
            )
          )
        )
      # },
      # error = function(e) {}
      # )
    })


    data4_deslocamento <- reactive({
      if (filtros()$nivel == "Municipal") {
        bloco4_deslocamento_muni |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
          ) |>
          dplyr::group_by(ano) |>
          dplyr::mutate(
            prop_partos_municipio_res = round(local/destino_total * 100, 1),
            prop_partos_fora_municipio_res = round(nao_local/destino_total * 100, 1),
            prop_partos_rsaude_res = round(dentro_regiao_saude/destino_total * 100, 1),
            prop_partos_macro_rsaude_res = round(dentro_macrorregiao_saude/destino_total * 100, 1),
            prop_partos_fora_macro_rsaude_res = round(fora_macrorregiao_saude/destino_total * 100, 1),
            prop_partos_fora_uf_res = round(outra_uf/destino_total * 100, 1),
            prop_partos_muni_maior_ocorrencia = round(n_nasc1/nao_local * 100, 1),
            prop_partos_muni_2_maior_ocorrencia = round(n_nasc2/nao_local * 100, 1),
            prop_partos_muni_3_maior_ocorrencia = round(n_nasc3/nao_local * 100, 1),
            localidade = filtros()$municipio,
            .keep = "unused"
          ) |>
          dplyr::ungroup()
      } else if (filtros()$nivel == "Estadual") {
        bloco4_deslocamento_uf |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            uf == filtros()$estado
          ) |>
          dplyr::group_by(ano) |>
          dplyr::mutate(
            prop_partos_municipio_res = round(local/destino_total * 100, 1),
            prop_partos_fora_municipio_res = round(nao_local/destino_total * 100, 1),
            prop_partos_rsaude_res = round(dentro_regiao_saude/destino_total * 100, 1),
            prop_partos_macro_rsaude_res = round(dentro_macrorregiao_saude/destino_total * 100, 1),
            prop_partos_fora_macro_rsaude_res = round(fora_macrorregiao_saude/destino_total * 100, 1),
            prop_partos_fora_uf_res = round(outra_uf/destino_total * 100, 1),
            localidade = filtros()$estado,
            .keep = "unused"
          ) |>
          dplyr::ungroup()
      } else {
        bloco4_deslocamento_muni |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::filter(
            if (filtros()$nivel == "Nacional")
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            else if (filtros()$nivel == "Regional")
              regiao == filtros()$regiao
            else if (filtros()$nivel == "Macrorregião de saúde")
              macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
            else if(filtros()$nivel == "Microrregião de saúde")
              r_saude == filtros()$micro & uf == filtros()$estado_micro
          ) |>
          dplyr::group_by(ano) |>
          dplyr::summarise(
            prop_partos_municipio_res = round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_municipio_res = round(sum(nao_local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_rsaude_res = round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_macro_rsaude_res = round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_uf_res = round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            localidade = dplyr::case_when(
              filtros()$nivel == "Nacional" ~ "Brasil",
              filtros()$nivel == "Regional" ~ filtros()$regiao,
              filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
              filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro
            )
          ) |>
          dplyr::ungroup()
      }

    })

    data4_deslocamento_comp <- reactive({
      bloco4_deslocamento_muni |>
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
        dplyr::summarise(
          br_prop_partos_municipio_res = round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          br_prop_partos_fora_municipio_res = round(sum(nao_local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          br_prop_partos_rsaude_res = round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          br_prop_partos_macro_rsaude_res = round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          br_prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          br_prop_partos_fora_uf_res = round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          localidade_comparacao = dplyr::case_when(
            filtros()$nivel2 == "Nacional" ~ "Média nacional",
            filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
            filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
            filtros()$nivel2 == "Municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "Municípios semelhantes" ~ "Média dos municípios semelhantes",
          )
        ) |>
        dplyr::ungroup()
    })


    data4_deslocamento_juncao <- reactive({dplyr::full_join(data4_deslocamento(), data4_deslocamento_comp(), by = "ano")})

    ##### Dados de incompletude e cobertura para os indicadores do segundo bloco #####
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
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

    data_cobertura <- reactive({
      if (filtros()$nivel == "Municipal") {
        sub_registro_sinasc_muni_2015_2020 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "Estadual") {
        sub_registro_sinasc_uf_regioes_2015_2020 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$estado
          )
      } else if (filtros()$nivel == "Regional") {
        sub_registro_sinasc_uf_regioes_2015_2020 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$regiao
          )
      } else if (filtros()$nivel == "Nacional") {
        sub_registro_sinasc_uf_regioes_2015_2020 |>
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

    data_incompletude <- reactive({dplyr::full_join(data_incompletude_aux(), data_cobertura(), by = c("ano", "localidade"))})

    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$parto_tprobson,
        variavel_incompletude1 = "PARTO e TPROBSON",
        descricao_incompletude1 = "em branco ou sem informação",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$parto_tprobson > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
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

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$tprobson > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
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

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$parto_tprobson > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    #cores pros graficos
    cols <- c("#2c115f", "#b73779", "#fc8961")


    data_incompletude_deslocamento_aux <- reactive({
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

    data_incompletude_deslocamento <- reactive({dplyr::full_join(data_incompletude_deslocamento_aux(), data_cobertura(), by = c("ano", "localidade"))})

    observeEvent(
      c(
        input$botao_prop1,
        input$botao_prop2,
        input$botao_prop3,
        input$botao_mediana1,
        input$botao_mediana2,
        input$botao_infos
      ), {
      cria_modal_incompletude(
        incompletude1 = data_incompletude_deslocamento()$prop_cnes_nao_preenchido,
        incompletude2 = data_incompletude_deslocamento()$prop_cnes_invalido,
        df = data_incompletude_deslocamento(),
        cobertura = data_incompletude()$cobertura,
        bloco = "deslocamento"
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_prop3", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_mediana1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_mediana2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_deslocamento_infos", anim = TRUE, animType = "fade", time = 0.8)
      req(
        any(data_incompletude_deslocamento()$prop_cnes_nao_preenchido > 5, na.rm = TRUE) |
          any(data_incompletude_deslocamento()$prop_cnes_invalido > 5, na.rm = TRUE) |
          any(data_incompletude()$cobertura < 90, na.rm = TRUE)
      )
      shinyjs::show(id = "mostrar_botao_deslocamento_prop1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_prop2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_prop3", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_mediana1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_mediana2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_deslocamento_infos", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )


    ##Gráficos de indicadores de deslocamento para municípios (porcentagem)
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


    data4_deslocamento_muni_med <- reactive({
      if (input$local_med_muni == "fora_municipio") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_municipio",
            baixa_complexidade = "km_partos_fora_municipio_baixa_complexidade",
            alta_complexidade = "km_partos_fora_municipio_alta_complexidade"
          )
      } else if (input$local_med_muni == "na_regiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_na_regiao",
            baixa_complexidade = "km_partos_na_regiao_baixa_complexidade",
            alta_complexidade = "km_partos_na_regiao_alta_complexidade"
          )
      } else if (input$local_med_muni == "na_macrorregiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_na_macrorregiao",
            baixa_complexidade = "km_partos_na_macrorregiao_baixa_complexidade",
            alta_complexidade = "km_partos_na_macrorregiao_alta_complexidade"
          )
      } else if (input$local_med_muni == "fora_macrorregiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_macrorregiao",
            baixa_complexidade = "km_partos_fora_macrorregiao_baixa_complexidade",
            alta_complexidade = "km_partos_fora_macrorregiao_alta_complexidade"
          )
      } else if (input$local_med_muni == "fora_uf") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_uf",
            baixa_complexidade = "km_partos_fora_uf_baixa_complexidade",
            alta_complexidade = "km_partos_fora_uf_alta_complexidade"
          )
      }

    })

    data4_deslocamento_uf_med <- reactive({
      if (input$local_med_uf == "fora_municipio") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_municipio",
            baixa_complexidade = "km_partos_fora_municipio_baixa_complexidade",
            alta_complexidade = "km_partos_fora_municipio_alta_complexidade"
          )
      } else if (input$local_med_uf == "na_regiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_na_regiao",
            baixa_complexidade = "km_partos_na_regiao_baixa_complexidade",
            alta_complexidade = "km_partos_na_regiao_alta_complexidade"
          )
      } else if (input$local_med_uf == "na_macrorregiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_na_macrorregiao",
            baixa_complexidade = "km_partos_na_macrorregiao_baixa_complexidade",
            alta_complexidade = "km_partos_na_macrorregiao_alta_complexidade"
          )
      } else if (input$local_med_uf == "fora_macrorregiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_macrorregiao",
            baixa_complexidade = "km_partos_fora_macrorregiao_baixa_complexidade",
            alta_complexidade = "km_partos_fora_macrorregiao_alta_complexidade"
          )
      } else if (input$local_med_uf == "fora_uf") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_uf",
            baixa_complexidade = "km_partos_fora_uf_baixa_complexidade",
            alta_complexidade = "km_partos_fora_uf_alta_complexidade"
          )
      }

    })

    data4_deslocamento_muni_med2 <- reactive({
      if (input$local_med2 == "fora_municipio") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_municipio",
            baixa_complexidade = "km_partos_fora_municipio_baixa_complexidade",
            alta_complexidade = "km_partos_fora_municipio_alta_complexidade"
          )
      } else if (input$local_med2 == "na_regiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_na_regiao",
            baixa_complexidade = "km_partos_na_regiao_baixa_complexidade",
            alta_complexidade = "km_partos_na_regiao_alta_complexidade"
          )
      } else if (input$local_med2 == "na_macrorregiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_na_macrorregiao",
            baixa_complexidade = "km_partos_na_macrorregiao_baixa_complexidade",
            alta_complexidade = "km_partos_na_macrorregiao_alta_complexidade"
          )
      } else if (input$local_med2 == "fora_macrorregiao") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_macrorregiao",
            baixa_complexidade = "km_partos_fora_macrorregiao_baixa_complexidade",
            alta_complexidade = "km_partos_fora_macrorregiao_alta_complexidade"
          )
      } else if (input$local_med2 == "fora_uf") {
        data4_deslocamento() |>
          dplyr::select(
            ano,
            no_local = "km_partos_fora_uf",
            baixa_complexidade = "km_partos_fora_uf_baixa_complexidade",
            alta_complexidade = "km_partos_fora_uf_alta_complexidade"
          )
      }

    })




  })
}



## To be copied in the UI
# mod_bloco_4_ui("bloco_4_1")

## To be copied in the server
# mod_bloco_4_server("bloco_4_1")

