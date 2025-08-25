#' bloco_6 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_6_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(class = "fonte-titulos-pagina", tags$b(HTML("Mortalidade e morbidade materna: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset1"),
      width = 12,
      collapsible = FALSE,
      tabPanel(
        HTML("<b class = 'fonte-grande'>Relacionados à mortalidade materna</b>"),
        value = "tabpanel_mortalidade",
        fluidRow(
          column(
            width = 12,
            HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
            HTML(
              "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos maternos no país, incluindo desagregação de raça/cor, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-grav-puerp/' target = _blank>OOBr Óbitos de Gestantes e Puérperas</a>.
                </b> </div>"
            ),
            hr(),
            HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
          ),
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
                uiOutput(ns("input_localidade_resumo_mort")),
                align = "center"
              )
            ),
            # fluidRow(
            #   bs4Dash::box(
            #     width = 12,
            #     collapsible = FALSE,
            #     headerBorder = FALSE,
            #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
            #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart_mort"), height = 530))
            #   )
            # ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i2")), proxy.height = "300px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i3")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i4")), proxy.height = "325px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i5")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i6")), proxy.height = "325px")
              )
            ),
            fluidRow(
              column(
                offset = 3,
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i7")), proxy.height = "325px")
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
                    HTML("<b class = 'fonte-muito-grande'> Número de óbitos maternos &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_mort"), height = 460))
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
                    HTML("<b class = 'fonte-muito-grande'> Razão de mortalidade materna por 100.000 nascidos vivos &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_mort"), height = 460))
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
                    HTML("<b class = 'fonte-muito-grande'> Porcentagem de óbitos maternos por causas obstétricas diretas &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_mort"), height = 460))
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
                    HTML("<b class = 'fonte-muito-grande'> Porcentagem de óbitos maternos diretos por causas específicas &nbsp;</b>"),
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
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("causa_obito_mort"),
                        label = span(class = "fonte-grande", "Causa de óbito materno"),
                        options = list(placeholder = "Selecione a causa de óbito materno"),
                        choices = c(
                          "Aborto" = "prop_obitos_aborto",
                          "Causas hipertensivas" = "prop_obitos_hipertens",
                          "Causas hemorrágicas" = "prop_obitos_hemo",
                          "Infecção puerperal" = "prop_obitos_infec"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_mort"), height = 380))
                )
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b class = 'fonte-grande'>Relacionados à morbidade materna grave (MMG)</b>"),
        value = "tabpanel_morbidade",
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
                uiOutput(ns("input_localidade_resumo_morb")),
                align = "center"
              )
            ),
            # fluidRow(
            #   bs4Dash::box(
            #     width = 12,
            #     collapsible = FALSE,
            #     headerBorder = FALSE,
            #     HTML("<b style='font-size:16px'> Gráfico de radar </b>"),
            #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart_mmg"), height = 530))
            #   )
            # ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mmg_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mmg_i2")), proxy.height = "300px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mmg_i3")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mmg_i4")), proxy.height = "325px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mmg_i5")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mmg_i6")), proxy.height = "325px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mmg_i7")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mmg_i8")), proxy.height = "325px")
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
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b class = 'fonte-muito-grande'> Porcentagem de casos de morbidade materna grave &nbsp;</b>"),
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
                  div(
                    style = "display: flex; justify-content: center;",
                    shinyWidgets::prettyRadioButtons(
                      inputId = ns("input_porc_mmg"),
                      label = NULL,
                      inline = TRUE,
                      choices = c(
                        "No SUS" = "sus",
                        "Na saúde suplementar" = "ans"
                      ),
                      icon = icon("check", style = "background-color: #007bff;"),
                      animation = "rotate"
                    )
                  ),
                  hr(style = "margin-top: 0;"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_mmg"), height = 430))
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
                    HTML("<b class = 'fonte-muito-grande'> Porcentagem de casos de morbidade materna grave por causas específicas &nbsp;</b>"),
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
                  div(
                    style = "display: flex; justify-content: center;",
                    shinyWidgets::prettyRadioButtons(
                      inputId = ns("input_porc_mmg_causas_especificas"),
                      label = NULL,
                      inline = TRUE,
                      choices = c(
                        "No SUS" = "sus",
                        "Na saúde suplementar" = "ans"
                      ),
                      icon = icon("check", style = "background-color: #007bff;"),
                      animation = "rotate"
                    )
                  ),
                  hr(style = "margin-top: 0;"),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("causa_mmg"),
                        label = span(class = "fonte-grande", "Causa de morbidade materna grave"),
                        options = list(placeholder = "Selecione a causa de morbidade materna grave"),
                        choices = c(
                          "Hipertensão" = "porc_mmg_hipertensao",
                          "Hemorragia" = "porc_mmg_hemorragia",
                          "Infecção" = "porc_mmg_infeccao"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_mmg"), height = 350))
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
                    HTML("<b class = 'fonte-muito-grande'> Porcentagem de casos de morbidade materna grave com internação em UTI &nbsp;</b>"),
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
                  div(
                    style = "display: flex; justify-content: center;",
                    shinyWidgets::prettyRadioButtons(
                      inputId = ns("input_porc_mmg_uti"),
                      label = NULL,
                      inline = TRUE,
                      choices = c(
                        "No SUS" = "sus",
                        "Na saúde suplementar" = "ans"
                      ),
                      icon = icon("check", style = "background-color: #007bff;"),
                      animation = "rotate"
                    )
                  ),
                  hr(style = "margin-top: 0;"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_mmg"), height = 430))
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
                    HTML("<b class = 'fonte-muito-grande'> Porcentagem de casos de morbidade materna grave com tempo de permanência prolongado &nbsp;</b>"),
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
                  div(
                    style = "display: flex; justify-content: center;",
                    shinyWidgets::prettyRadioButtons(
                      inputId = ns("input_porc_mmg_tmp"),
                      label = NULL,
                      inline = TRUE,
                      choices = c(
                        "No SUS" = "sus",
                        "Na saúde suplementar" = "ans"
                      ),
                      icon = icon("check", style = "background-color: #007bff;"),
                      animation = "rotate"
                    )
                  ),
                  hr(style = "margin-top: 0;"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_mmg"), height = 430))
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
                    HTML("<b class = 'fonte-muito-grande'> Porcentagem de casos de morbidade materna grave com transfusão sanguínea &nbsp;</b>"),
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
                  div(
                    style = "display: flex; justify-content: center;",
                    shinyWidgets::prettyRadioButtons(
                      inputId = ns("input_porc_mmg_transfusao"),
                      label = NULL,
                      inline = TRUE,
                      choices = c(
                        "No SUS" = "sus",
                        "Na saúde suplementar" = "ans"
                      ),
                      icon = icon("check", style = "background-color: #007bff;"),
                      animation = "rotate"
                    )
                  ),
                  hr(style = "margin-top: 0;"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_mmg"), height = 430))
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
                    HTML("<b class = 'fonte-muito-grande'> Porcentagem de casos de morbidade materna grave com histerectomia (retirada do útero) &nbsp;</b>"),
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
                  div(
                    style = "display: flex; justify-content: center;",
                    shinyWidgets::prettyRadioButtons(
                      inputId = ns("input_porc_mmg_cirurgia"),
                      label = NULL,
                      inline = TRUE,
                      choices = c(
                        "No SUS" = "sus",
                        "Na saúde suplementar" = "ans"
                      ),
                      icon = icon("check", style = "background-color: #007bff;"),
                      animation = "rotate"
                    )
                  ),
                  hr(style = "margin-top: 0;"),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_mmg"), height = 430))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' bloco_6 Server Functions
#'
#' @noRd
mod_bloco_6_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco6_calcs <- data.frame(
      tipo = c("local", "referencia"),

      # Óbitos maternos
      soma_obitos_mat_totais = rep("sum(obitos_mat_totais)", 2),
      rmm = c("round(sum(obitos_mat_totais) / sum(nascidos) * 100000, 1)", "30"),
      prop_obitos_diretos = rep("round(sum(obitos_mat_diretos) / sum(obitos_mat_totais) * 100, 1)", 2),
      prop_obitos_aborto = rep("round(sum(obitos_mat_aborto) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_hipertens = rep("round(sum(obitos_mat_hipertensao) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_hemo = rep("round(sum(obitos_mat_hemorragia) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_infec = rep("round(sum(obitos_mat_infec_puerperal) / sum(obitos_mat_diretos) * 100, 1)", 2),

      # Morbidade materna no SUS
      soma_casos_mmg_sus = rep("sum(casos_mmg_sus)", 2),
      porc_mmg_sus = rep("round(sum(casos_mmg_sus) / sum(total_internacoes_sus) * 100, 1)", 2),
      porc_mmg_hipertensao_sus = rep("round(sum(casos_mmg_sus_hipertensao) / sum(casos_mmg_sus) * 100, 1)", 2),
      porc_mmg_hemorragia_sus = rep("round(sum(casos_mmg_sus_hemorragia) / sum(casos_mmg_sus) * 100, 1)", 2),
      porc_mmg_infeccao_sus = rep("round(sum(casos_mmg_sus_infeccoes) / sum(casos_mmg_sus) * 100, 1)", 2),
      porc_mmg_uti_sus = rep("round(sum(casos_mmg_sus_uti) / sum(casos_mmg_sus) * 100, 1)", 2),
      porc_mmg_tmp_sus = rep("round(sum(casos_mmg_sus_tmp) / sum(casos_mmg_sus) * 100, 1)", 2),
      porc_mmg_transfusao_sus = rep("round(sum(casos_mmg_sus_transfusao) / sum(casos_mmg_sus) * 100, 1)", 2),
      porc_mmg_cirurgia_sus = rep("round(sum(casos_mmg_sus_cirurgia) / sum(casos_mmg_sus) * 100, 1)", 2),

      # Morbidade materna na ANS
      soma_casos_mmg_ans = rep("sum(casos_mmg_ans[ano >= 2015])", 2),
      porc_mmg_ans = rep("round(sum(casos_mmg_ans[ano >= 2015]) / sum(total_internacoes_ans[ano >= 2015]) * 100, 1)", 2),
      porc_mmg_hipertensao_ans = rep("round(sum(casos_mmg_ans_hipertensao[ano >= 2015]) / sum(casos_mmg_ans[ano >= 2015]) * 100, 1)", 2),
      porc_mmg_hemorragia_ans = rep("round(sum(casos_mmg_ans_hemorragia[ano >= 2015]) / sum(casos_mmg_ans[ano >= 2015]) * 100, 1)", 2),
      porc_mmg_infeccao_ans = rep("round(sum(casos_mmg_ans_infeccoes[ano >= 2015]) / sum(casos_mmg_ans[ano >= 2015]) * 100, 1)", 2),
      porc_mmg_uti_ans = rep("round(sum(casos_mmg_ans_uti[ano >= 2015]) / sum(casos_mmg_ans[ano >= 2015]) * 100, 1)", 2),
      porc_mmg_tmp_ans = rep("round(sum(casos_mmg_ans_tmp[ano >= 2015]) / sum(casos_mmg_ans[ano >= 2015]) * 100, 1)", 2),
      porc_mmg_transfusao_ans = rep("round(sum(casos_mmg_ans_transfusao[ano >= 2015]) / sum(casos_mmg_ans[ano >= 2015]) * 100, 1)", 2),
      porc_mmg_cirurgia_ans = rep("round(sum(casos_mmg_ans_cirurgia[ano >= 2015]) / sum(casos_mmg_ans[ano >= 2015]) * 100, 1)", 2)
    )

    bloco6_calcs_resumo <- bloco6_calcs |>
      dplyr::mutate(
        prop_mif_investigado = c("round((sum(obito_mif_investigado_com_ficha_sintese[ano <= 2020], na.rm = TRUE) + sum(obito_mif_investigado_sem_ficha_sintese[ano <= 2020], na.rm = TRUE))/sum(total_obitos_mulher_idade_fertil[ano <= 2020], na.rm = TRUE) * 100, 1)", "100"),
        prop_obito_materno_investigado = c("round((sum(obito_materno_investigado_com_ficha_sintese[ano <= 2020], na.rm = TRUE) + sum(obito_materno_investigado_sem_ficha_sintese[ano <= 2020], na.rm = TRUE))/sum(total_obitos_maternos[ano <= 2020], na.rm = TRUE) * 100, 1)", "100")
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

    ## Criando os outputs que receberão os nomes dos locais selecionados quando há comparação --------
    output$input_localidade_resumo_mort <- renderUI({
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
          inputId = ns("localidade_resumo_mort"),
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

    output$input_localidade_resumo_morb <- renderUI({
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
          inputId = ns("localidade_resumo_morb"),
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

    ## Criando o output que receberá o nível selecionado ----------------------
    nivel_selecionado <- reactive({
      if (filtros()$comparar == "Não") {
        filtros()$nivel
      } else {
        if (input$tabset1 == "tabpanel_mortalidade") {
          req(input$localidade_resumo_mort)
          if (input$localidade_resumo_mort == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        } else {
          req(input$localidade_resumo_morb)
          if (input$localidade_resumo_morb == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        }
      }
    })

    ## Criando o pop-up com a informação sobre o resumo do período -------------
    observeEvent(c(input$botao_resumo1, input$botao_resumo2), {
      shinyalert::shinyalert(
        html = TRUE,
        title = '<div class = "fonte-titulos-modal" style = "color: #656565"> Sobre o "Resumo do período" </div>',
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
        animation = "slide-from-bottom",
        immediate = TRUE
      )
    },
    ignoreInit = TRUE
    )

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
          prop_mif_investigado = round((sum(obito_mif_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_mif_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_mulher_idade_fertil, na.rm = TRUE) * 100, 1),
          prop_obito_materno_investigado = round((sum(obito_materno_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_materno_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_maternos, na.rm = TRUE) * 100, 1),
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
        sub_registro_sim_muni_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "estadual") {
        sub_registro_sim_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$estado
          )
      } else if (filtros()$nivel == "regional") {
        sub_registro_sim_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$regiao
          )
      } else if (filtros()$nivel == "nacional") {
        sub_registro_sim_uf_regioes_2015_2021 |>
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
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao4", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$prop_mif_investigado < 90, na.rm = TRUE) | any(data_incompletude()$prop_obito_materno_investigado < 100, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao4", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$prop_mif_investigado,
        incompletude2 = data_incompletude()$prop_obito_materno_investigado,
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura,
        bloco = "bloco6"
      )
    })

    observeEvent(input$botao2, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$prop_mif_investigado,
        incompletude2 = data_incompletude()$prop_obito_materno_investigado,
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura,
        bloco = "bloco6"
      )
    })

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$prop_mif_investigado,
        incompletude2 = data_incompletude()$prop_obito_materno_investigado,
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura,
        bloco = "bloco6"
      )
    })

    observeEvent(input$botao4, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$prop_mif_investigado,
        incompletude2 = data_incompletude()$prop_obito_materno_investigado,
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura,
        bloco = "bloco6"
      )
    })


    # Para o resumo do período ------------------------------------------------
    ## Calculando uma média dos indicadores para o período selecionado --------
    ### Para a localidade selecionada -----------------------------------------
    data6_resumo <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo_inputs <- ""
      } else {
        if (input$tabset1 == "tabpanel_mortalidade") {
          req(input$localidade_resumo_mort)
          if (input$localidade_resumo_mort == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else {
          req(input$localidade_resumo_morb)
          if (input$localidade_resumo_morb == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        }
      }
      dplyr::left_join(bloco6, base_incompletude) |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (nivel_selecionado() == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (nivel_selecionado() == "regional")
            regiao == filtros()[[paste0("regiao", sufixo_inputs)]]
          else if (nivel_selecionado() == "estadual")
            uf == filtros()[[paste0("estado", sufixo_inputs)]]
          else if (nivel_selecionado() == "macro")
            macro_r_saude == filtros()[[paste0("macro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_macro", sufixo_inputs)]]
          else if(nivel_selecionado() == "micro")
            r_saude == filtros()[[paste0("micro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_micro", sufixo_inputs)]]
          else if(nivel_selecionado() == "municipal")
            municipio == filtros()[[paste0("municipio", sufixo_inputs)]] & uf == filtros()[[paste0("estado_municipio", sufixo_inputs)]]
          else if (nivel_selecionado() == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        cria_indicadores(
          df_calcs = bloco6_calcs_resumo,
          filtros = filtros(),
          adicionar_localidade = TRUE,
          localidade_resumo = get('input')[[glue::glue("localidade_resumo_{ifelse(grepl('mortalidade', input$tabset1), 'mort', 'morb')}")]]
        ) |>
        dplyr::mutate(
          localidade = dplyr::case_when(
            nivel_selecionado() == "nacional" ~ "Brasil",
            nivel_selecionado() == "regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
            nivel_selecionado() == "estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
            nivel_selecionado() == "macro" ~ filtros()[[paste0("macro", sufixo_inputs)]],
            nivel_selecionado() == "micro" ~ filtros()[[paste0("micro", sufixo_inputs)]],
            nivel_selecionado() == "municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]],
            nivel_selecionado() == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        )
    })

    data6_resumo_correcao_rmm <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo_inputs <- ""
      } else {
        if (input$tabset1 == "tabpanel_mortalidade") {
          req(input$localidade_resumo_mort)
          if (input$localidade_resumo_mort == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else {
          req(input$localidade_resumo_morb)
          if (input$localidade_resumo_morb == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        }
      }

      if (nivel_selecionado() %in% c("estadual", "regional", "nacional") & filtros()$ano2[2] < 2023){
        if (nivel_selecionado() == "estadual"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()[[paste0("estado", sufixo_inputs)]],
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(localidade) |>
            dplyr::summarise(
              RMM_C = mean(RMM)
            )
        } else if (nivel_selecionado() == "regional"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()[[paste0("regiao", sufixo_inputs)]],
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(localidade) |>
            dplyr::summarise(
              RMM_C = mean(RMM)
            )
        } else if (nivel_selecionado() == "nacional"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == "Brasil",
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(localidade) |>
            dplyr::summarise(
              RMM_C = mean(RMM)
            )
        }
      } else{
        data.frame(
          localidade = dplyr::case_when(
            nivel_selecionado() == "nacional" ~ "Brasil",
            nivel_selecionado() == "regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
            nivel_selecionado() == "estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
            nivel_selecionado() == "macro" ~ filtros()[[paste0("macro", sufixo_inputs)]],
            nivel_selecionado() == "micro" ~ filtros()[[paste0("micro", sufixo_inputs)]],
            nivel_selecionado() == "municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]],
            nivel_selecionado() == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        )
      }
    })

    data6_resumo_rmm_corrigida <- reactive({
      if (nivel_selecionado() %in% c("estadual", "regional", "nacional") & filtros()$ano2[2] < 2023) {
        dplyr::full_join(data6_resumo(), data6_resumo_correcao_rmm(), by = "localidade") |>
          dplyr::select(!rmm) |>
          dplyr::mutate(rmm = RMM_C)
      } else {
        dplyr::full_join(data6_resumo(), data6_resumo_correcao_rmm(), by = "localidade")
      }
    })

    ### Para a referência -----------------------------------------------------
    data6_resumo_referencia <- reactive({
      dplyr::left_join(bloco6, base_incompletude) |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco6_calcs_resumo, filtros = filtros(), referencia = TRUE)
    })


    ## Criando os outputs das caixinhas ---------------------------------------
    ### Para os indicadores de mortalidade materna ----------------------------
    #### Número de total óbitos maternos ---------------------------------------
    output$caixa_b6_mort_i1 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "soma_obitos_mat_totais",
        titulo = "Número de total óbitos maternos",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$soma_obitos_mat_totais,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Razão de mortalidade materna por 100.000 nascidos vivos ---------------
    output$caixa_b6_mort_i2 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo_rmm_corrigida(),
        indicador = "rmm",
        titulo = "Razão de mortalidade materna por 100.000 nascidos vivos",
        tem_meta = TRUE,
        valor_de_referencia = data6_resumo_referencia()$rmm,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        tipo_referencia = "meta ODS",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de óbitos maternos por causas obstétricas diretas ---------
    output$caixa_b6_mort_i3 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_obitos_diretos",
        titulo = "Porcentagem de óbitos maternos por causas obstétricas diretas",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_obitos_diretos,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de óbitos maternos diretos por aborto ---------------------
    output$caixa_b6_mort_i4 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_obitos_aborto",
        titulo = "Porcentagem de óbitos maternos diretos por aborto",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_obitos_aborto,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de óbitos maternos diretos por causas hemorrágicas --------
    output$caixa_b6_mort_i5 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_obitos_hemo",
        titulo = "Porcentagem de óbitos maternos diretos por causas hemorrágicas",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_obitos_hemo,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de óbitos maternos diretos por causas hipertensivas --------
    output$caixa_b6_mort_i6 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_obitos_hipertens",
        titulo = "Porcentagem de óbitos maternos diretos por causas hipertensivas",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_obitos_hipertens,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de óbitos maternos diretos por infecção puerperal ---------
    output$caixa_b6_mort_i7 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_obitos_infec",
        titulo = "Porcentagem de óbitos maternos diretos por infecção puerperal",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_obitos_infec,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })


    ### Para os indicadores de morbidade materna ------------------------------
    #### Porcentagem de casos de morbidade materna grave ----
    output$caixa_b6_mmg_i1 <- renderUI({
      sufixo_titulo <- ifelse(input$input_porc_mmg == "sus", "no SUS", "na saúde suplementar")
      indicador <- ifelse(input$input_porc_mmg == "sus", "porc_mmg_sus", "porc_mmg_ans")

      cria_caixa_server(
        dados = data6_resumo(),
        indicador = indicador,
        titulo = glue::glue("Porcentagem de casos de MMG {sufixo_titulo}"),
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()[[indicador]],
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave por hipertensão ------
    output$caixa_b6_mmg_i2 <- renderUI({
      sufixo_titulo <- ifelse(input$input_porc_mmg_causas_especificas == "sus", "no SUS", "na saúde suplementar")
      indicador <- ifelse(input$input_porc_mmg_causas_especificas == "sus", "porc_mmg_hipertensao_sus", "porc_mmg_hipertensao_ans")

      cria_caixa_server(
        dados = data6_resumo(),
        indicador = indicador,
        titulo = glue::glue("Porcentagem de casos de MMG por hipertensão {sufixo_titulo}"),
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()[[indicador]],
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave por hemorragia -------
    output$caixa_b6_mmg_i3 <- renderUI({
      sufixo_titulo <- ifelse(input$input_porc_mmg_causas_especificas == "sus", "no SUS", "na saúde suplementar")
      indicador <- ifelse(input$input_porc_mmg_causas_especificas == "sus", "porc_mmg_hemorragia_sus", "porc_mmg_hemorragia_ans")

      cria_caixa_server(
        dados = data6_resumo(),
        indicador = indicador,
        titulo = glue::glue("Porcentagem de casos de MMG por hemorragia {sufixo_titulo}"),
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()[[indicador]],
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave por infecção ---------
    output$caixa_b6_mmg_i4 <- renderUI({
      sufixo_titulo <- ifelse(input$input_porc_mmg_causas_especificas == "sus", "no SUS", "na saúde suplementar")
      indicador <- ifelse(input$input_porc_mmg_causas_especificas == "sus", "porc_mmg_infeccao_sus", "porc_mmg_infeccao_ans")

      cria_caixa_server(
        dados = data6_resumo(),
        indicador = indicador,
        titulo = glue::glue("Porcentagem de casos de MMG por infecção {sufixo_titulo}"),
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()[[indicador]],
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave com internação em UTI ----
    output$caixa_b6_mmg_i5 <- renderUI({
      sufixo_titulo <- ifelse(input$input_porc_mmg_uti == "sus", "no SUS", "na saúde suplementar")
      indicador <- ifelse(input$input_porc_mmg_uti == "sus", "porc_mmg_uti_sus", "porc_mmg_uti_ans")

      cria_caixa_server(
        dados = data6_resumo(),
        indicador = indicador,
        titulo = glue::glue("Porcentagem de casos de MMG com internação em UTI {sufixo_titulo}"),
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()[[indicador]],
        tipo = "porcentagem",
        invertido = FALSE,
        cor = "lightgrey",
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave com tempo de permanência prolongado ----
    output$caixa_b6_mmg_i6 <- renderUI({
      sufixo_titulo <- ifelse(input$input_porc_mmg_tmp == "sus", "no SUS", "na saúde suplementar")
      indicador <- ifelse(input$input_porc_mmg_tmp == "sus", "porc_mmg_tmp_sus", "porc_mmg_tmp_ans")

      cria_caixa_server(
        dados = data6_resumo(),
        indicador = indicador,
        titulo = glue::glue("Porcentagem de casos de MMG com tempo de permanência prolongado {sufixo_titulo}"),
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()[[indicador]],
        tipo = "porcentagem",
        cor = "lightgrey",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave com transfusão sanguínea ----
    output$caixa_b6_mmg_i7 <- renderUI({
      sufixo_titulo <- ifelse(input$input_porc_mmg_transfusao == "sus", "no SUS", "na saúde suplementar")
      indicador <- ifelse(input$input_porc_mmg_transfusao == "sus", "porc_mmg_transfusao_sus", "porc_mmg_transfusao_ans")

      cria_caixa_server(
        dados = data6_resumo(),
        indicador = indicador,
        titulo = glue::glue("Porcentagem de casos de MMG com transfusão sanguínea {sufixo_titulo}"),
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()[[indicador]],
        tipo = "porcentagem",
        cor = "lightgrey",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave com histerectomia (retirada do útero) ----
    output$caixa_b6_mmg_i8 <- renderUI({
      sufixo_titulo <- ifelse(input$input_porc_mmg_cirurgia == "sus", "no SUS", "na saúde suplementar")
      indicador <- ifelse(input$input_porc_mmg_cirurgia == "sus", "porc_mmg_cirurgia_sus", "porc_mmg_cirurgia_ans")

      cria_caixa_server(
        dados = data6_resumo(),
        indicador = indicador,
        titulo = glue::glue("Porcentagem de casos de MMG com histerectomia (retirada do útero) {sufixo_titulo}"),
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()[[indicador]],
        tipo = "porcentagem",
        cor = "lightgrey",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })


    # Para os gráficos --------------------------------------------------------
    cols <- c("#2c115f", "#b73779", "#fc8961", "#000004FF", "#f1605d")

    ## Calculando os indicadores para cada ano do período selecionado ---------
    ### Para a localidade selecionada -----------------------------------------
    data6 <- reactive({
      bloco6 |>
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
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros())
    })

    data6_rmm_corrigida_aux <- reactive({
      if (filtros()$nivel %in% c("estadual", "regional", "nacional")) {
        if (filtros()$nivel == "estadual") {
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$estado,
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        } else if (filtros()$nivel == "regional") {
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$regiao,
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        } else if (filtros()$nivel == "nacional") {
          rmm_corrigida |>
            dplyr::filter(
              localidade == "Brasil",
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        }
      }
      else{
        data.frame(ano = filtros()$ano2[1]:filtros()$ano2[2])
      }
    })

    data6_rmm_corrigida <- reactive({
      if (filtros()$nivel %in% c("estadual", "regional", "nacional")) {
        dplyr::full_join(data6(), data6_rmm_corrigida_aux(), by = "ano") |>
          dplyr::mutate(rmm = ifelse(ano <= 2022, RMM, rmm))
      } else {
        data6()
      }
    })

    ### Para a comparação selecionada -----------------------------------------
    data6_comp <- reactive({
      bloco6 |>
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
          else if(filtros()$nivel2 == "micro")
            r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
          else if(filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros(), comp = TRUE)
    })

    data6_comp_rmm_corrigida_aux <- reactive({
      if (filtros()$nivel2 %in% c("estadual", "regional", "nacional")) {
        if (filtros()$nivel2 == "estadual") {
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$estado2,
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        } else if (filtros()$nivel2 == "regional") {
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$regiao2,
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        } else if (filtros()$nivel2=="nacional") {
          rmm_corrigida |>
            dplyr::filter(
              localidade == "Brasil",
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        }
      }
      else{
        data.frame(ano = filtros()$ano2[1]:filtros()$ano2[2])
      }
    })

    data6_comp_rmm_corrigida <- reactive({
      if (filtros()$nivel2 %in% c("estadual", "regional", "nacional")) {
        dplyr::full_join(data6_comp(), data6_comp_rmm_corrigida_aux(), by = "ano") |>
          dplyr::mutate(rmm = ifelse(ano <= 2022, RMM, rmm))
      } else{
        data6_comp()
      }
    })

    ### Para a referência -----------------------------------------------------
    data6_referencia <- reactive({
      bloco6 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros(), referencia = TRUE)
    })


    ## Criando os outputs dos gráficos ----------------------------------------
    ### Para os indicadores de mortalidade materna ----------------------------
    #### Número de óbitos maternos --------------------------------------------
    output$plot1_mort <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6(),
            name = dplyr::if_else(filtros()$nivel == "nacional", "Brasil", unique(data6()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = soma_obitos_mat_totais, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Número de óbitos maternos"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6(),
            name = dplyr::if_else(filtros()$nivel == "nacional", "Brasil", unique(data6()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = soma_obitos_mat_totais, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp(),
            name = dplyr::if_else(filtros()$nivel == "nacional", "Brasil", unique(data6_comp()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = soma_obitos_mat_totais, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Número de óbitos maternos"), min = 0) |>
          highcharter::hc_colors(cols)
      }
    })

    #### Razão de mortalidade materna por 100.000 nascidos vivos --------------
    output$plot2_mort <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_rmm_corrigida(),
            type = "line",
            name = dplyr::if_else(filtros()$nivel == "nacional", "Brasil", unique(data6()$class)),
            highcharter::hcaes(x = ano, y = rmm, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_referencia(),
            type = "line",
            name = "Referência (meta ODS)",
            highcharter::hcaes(x = ano, y = rmm, group = class, colour = class),
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
          highcharter::hc_yAxis(title = list(text = "Óbitos maternos por 100 mil nascidos vivos"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_rmm_corrigida(),
            type = "line",
            name = dplyr::if_else(filtros()$nivel == "nacional", "Brasil", unique(data6()$class)),
            highcharter::hcaes(x = ano, y = rmm, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp_rmm_corrigida(),
            type = "line",
            name = dplyr::if_else(filtros()$nivel2 == "nacional", "Brasil", unique(data6_comp()$class)),
            highcharter::hcaes(x = ano, y = rmm, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Óbitos maternos por 100 mil nascidos vivos"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data6_referencia(),
            type = "line",
            name = "Referência (meta ODS)",
            highcharter::hcaes(x = ano, y = rmm, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.6
          )
        }

      }
    })

    #### Porcentagem de óbitos maternos por causas obstétricas diretas --------
    output$plot3_mort <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        validate(
          need(
            sum(data6()$soma_obitos_mat_totais) != 0,
            "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_obitos_diretos, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_obitos_diretos, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        validate(
          need(
            sum(data6()$soma_obitos_mat_totais) != 0 | sum(data6_comp()$soma_obitos_mat_totais) != 0,
            "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_obitos_diretos, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_obitos_diretos, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_obitos_diretos, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    #### Porcentagem de óbitos maternos diretos por causas específicas --------
    output$plot4_mort <- highcharter::renderHighchart({
      data6_aux <- data6() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$causa_obito_mort),
          class
        )

      data6_comp_aux <- data6_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$causa_obito_mort),
          class
        )

      data6_referencia_aux <- data6_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$causa_obito_mort),
          class
        )

      if (filtros()$comparar == "Não") {
        validate(
          need(
            sum(data6()$soma_obitos_mat_totais) != 0,
            "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
              data = data6_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        validate(
          need(
            sum(data6()$soma_obitos_mat_totais) != 0 | sum(data6_comp()$soma_obitos_mat_totais) != 0,
            "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
              data = data6_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })


    ### Para os indicadores de morbidade materna ------------------------------
    renomeia_indicador <- function(df, indicador, tipo) {
      if (tipo == "ans") {
        df <- df |> dplyr::filter(ano >= 2015)
      }

      # monta o nome da coluna dinamicamente: indicador_tipo
      col <- paste0(indicador, "_", tipo)

      df |>
        dplyr::mutate(!!rlang::sym(indicador) := !!rlang::sym(col))
    }

    #### Porcentagem de casos de morbidade materna grave ----
    output$plot1_mmg <- highcharter::renderHighchart({
      data6_mmg_plot <- renomeia_indicador(data6(), "porc_mmg", input$input_porc_mmg)
      data6_mmg_plot_comp <- renomeia_indicador(data6_comp(), "porc_mmg", input$input_porc_mmg)
      data6_mmg_plot_referencia <- renomeia_indicador(data6_referencia(), "porc_mmg", input$input_porc_mmg)

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_sus"]], data6_mmg_plot[["porc_mmg_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_sus"]], data6_mmg_plot_referencia[["porc_mmg_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_mmg_plot_comp,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_sus"]], data6_mmg_plot[["porc_mmg_ans"]],
              data6_mmg_plot_comp[["porc_mmg_sus"]], data6_mmg_plot_comp[["porc_mmg_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_sus"]], data6_mmg_plot_referencia[["porc_mmg_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave por causas específicas ----
    output$plot2_mmg <- highcharter::renderHighchart({
      data6_mmg_plot <- renomeia_indicador(data6(), input$causa_mmg, input$input_porc_mmg_causas_especificas)
      data6_mmg_plot_comp <- renomeia_indicador(data6_comp(), input$causa_mmg, input$input_porc_mmg_causas_especificas)
      data6_mmg_plot_referencia <- renomeia_indicador(data6_referencia(), input$causa_mmg, input$input_porc_mmg_causas_especificas)

      data6_aux2 <- data6_mmg_plot |>
        dplyr::select(
          ano,
          eixo_y2 = dplyr::all_of(input$causa_mmg),
          class
        )

      data6_comp_aux2 <- data6_mmg_plot_comp |>
        dplyr::select(
          ano,
          eixo_y2 = dplyr::all_of(input$causa_mmg),
          class
        )

      data6_referencia_aux2 <- data6_mmg_plot_referencia |>
        dplyr::select(
          ano,
          eixo_y2 = dplyr::all_of(input$causa_mmg),
          class
        )

      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_causas_especificas}")]] != 0,
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_causas_especificas == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_aux2,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y2, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[[glue::glue("{input$causa_mmg}_sus")]], data6_mmg_plot[[glue::glue("{input$causa_mmg}_ans")]],
              data6_mmg_plot_referencia[[glue::glue("{input$causa_mmg}_sus")]], data6_mmg_plot_referencia[[glue::glue("{input$causa_mmg}_ans")]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_referencia_aux2,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y2, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_causas_especificas}")]] | data6_mmg_plot_comp[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_causas_especificas}")]],
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_causas_especificas == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_aux2,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y2, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp_aux2,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y2, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[[glue::glue("{input$causa_mmg}_sus")]], data6_mmg_plot[[glue::glue("{input$causa_mmg}_ans")]],
              data6_mmg_plot_comp[[glue::glue("{input$causa_mmg}_sus")]], data6_mmg_plot_comp[[glue::glue("{input$causa_mmg}_ans")]],
              data6_mmg_plot_referencia[[glue::glue("{input$causa_mmg}_sus")]], data6_mmg_plot_referencia[[glue::glue("{input$causa_mmg}_ans")]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_referencia_aux2,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y2, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave com internação em UTI ----
    output$plot3_mmg <- highcharter::renderHighchart({
      data6_mmg_plot <- renomeia_indicador(data6(), "porc_mmg_uti", input$input_porc_mmg_uti)
      data6_mmg_plot_comp <- renomeia_indicador(data6_comp(), "porc_mmg_uti", input$input_porc_mmg_uti)
      data6_mmg_plot_referencia <- renomeia_indicador(data6_referencia(), "porc_mmg_uti", input$input_porc_mmg_uti)

      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_uti}")]] != 0,
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_uti == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_uti, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_uti_sus"]], data6_mmg_plot[["porc_mmg_uti_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_uti_sus"]], data6_mmg_plot_referencia[["porc_mmg_uti_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg_uti, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_uti}")]] | data6_mmg_plot_comp[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_uti}")]],
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_uti == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_uti, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_mmg_plot_comp,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_uti, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_uti_sus"]], data6_mmg_plot[["porc_mmg_uti_ans"]],
              data6_mmg_plot_comp[["porc_mmg_uti_sus"]], data6_mmg_plot_comp[["porc_mmg_uti_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_uti_sus"]], data6_mmg_plot_referencia[["porc_mmg_uti_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg_uti, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave com tempo de permanência prolongado ----
    output$plot4_mmg <- highcharter::renderHighchart({
      data6_mmg_plot <- renomeia_indicador(data6(), "porc_mmg_tmp", input$input_porc_mmg_tmp)
      data6_mmg_plot_comp <- renomeia_indicador(data6_comp(), "porc_mmg_tmp", input$input_porc_mmg_tmp)
      data6_mmg_plot_referencia <- renomeia_indicador(data6_referencia(), "porc_mmg_tmp", input$input_porc_mmg_tmp)

      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_tmp}")]] != 0,
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_tmp == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_tmp, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_tmp_sus"]], data6_mmg_plot[["porc_mmg_tmp_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_tmp_sus"]], data6_mmg_plot_referencia[["porc_mmg_tmp_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg_tmp, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_tmp}")]] | data6_mmg_plot_comp[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_tmp}")]],
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_tmp == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_tmp, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_mmg_plot_comp,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_tmp, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_tmp_sus"]], data6_mmg_plot[["porc_mmg_tmp_ans"]],
              data6_mmg_plot_comp[["porc_mmg_tmp_sus"]], data6_mmg_plot_comp[["porc_mmg_tmp_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_tmp_sus"]], data6_mmg_plot_referencia[["porc_mmg_tmp_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg_tmp, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave com transfusão sanguínea ----
    output$plot5_mmg <- highcharter::renderHighchart({
      data6_mmg_plot <- renomeia_indicador(data6(), "porc_mmg_transfusao", input$input_porc_mmg_transfusao)
      data6_mmg_plot_comp <- renomeia_indicador(data6_comp(), "porc_mmg_transfusao", input$input_porc_mmg_transfusao)
      data6_mmg_plot_referencia <- renomeia_indicador(data6_referencia(), "porc_mmg_transfusao", input$input_porc_mmg_transfusao)

      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_transfusao}")]] != 0,
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_transfusao == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_transfusao, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_transfusao_sus"]], data6_mmg_plot[["porc_mmg_transfusao_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_transfusao_sus"]], data6_mmg_plot_referencia[["porc_mmg_transfusao_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg_transfusao, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_transfusao}")]] | data6_mmg_plot_comp[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_transfusao}")]],
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_transfusao == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_transfusao, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_mmg_plot_comp,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_transfusao, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_transfusao_sus"]], data6_mmg_plot[["porc_mmg_transfusao_ans"]],
              data6_mmg_plot_comp[["porc_mmg_transfusao_sus"]], data6_mmg_plot_comp[["porc_mmg_transfusao_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_transfusao_sus"]], data6_mmg_plot_referencia[["porc_mmg_transfusao_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg_transfusao, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave com intervenção cirúrgica ----
    output$plot6_mmg <- highcharter::renderHighchart({
      data6_mmg_plot <- renomeia_indicador(data6(), "porc_mmg_cirurgia", input$input_porc_mmg_cirurgia)
      data6_mmg_plot_comp <- renomeia_indicador(data6_comp(), "porc_mmg_cirurgia", input$input_porc_mmg_cirurgia)
      data6_mmg_plot_referencia <- renomeia_indicador(data6_referencia(), "porc_mmg_cirurgia", input$input_porc_mmg_cirurgia)

      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_cirurgia}")]] != 0,
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_cirurgia == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_cirurgia, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_cirurgia_sus"]], data6_mmg_plot[["porc_mmg_cirurgia_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_cirurgia_sus"]], data6_mmg_plot_referencia[["porc_mmg_cirurgia_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg_cirurgia, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        validate(
          need(
            data6_mmg_plot[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_cirurgia}")]] | data6_mmg_plot_comp[[glue::glue("soma_casos_mmg_{input$input_porc_mmg_cirurgia}")]],
            glue::glue("Não foram registrados casos de morbidade materna grave {ifelse(input$input_porc_mmg_cirurgia == 'sus', 'no SUS', 'na saúde suplementar')} no período. Dessa forma, este indicador não se aplica.")
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data6_mmg_plot,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_cirurgia, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_mmg_plot_comp,
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mmg_cirurgia, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = min(data6_mmg_plot_referencia$ano):max(data6_mmg_plot_referencia$ano), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(
            title = list(text = "%"),
            min = 0,
            ceiling = 100,
            max = max(
              data6_mmg_plot[["porc_mmg_cirurgia_sus"]], data6_mmg_plot[["porc_mmg_cirurgia_ans"]],
              data6_mmg_plot_comp[["porc_mmg_cirurgia_sus"]], data6_mmg_plot_comp[["porc_mmg_cirurgia_ans"]],
              data6_mmg_plot_referencia[["porc_mmg_cirurgia_sus"]], data6_mmg_plot_referencia[["porc_mmg_cirurgia_ans"]],
              na.rm = TRUE
            ) + 1
          ) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_mmg_plot_referencia,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mmg_cirurgia, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })




  })
}


## To be copied in the UI
# mod_bloco_6_ui("bloco_6_1")

## To be copied in the server
# mod_bloco_6_server("bloco_6_1")
