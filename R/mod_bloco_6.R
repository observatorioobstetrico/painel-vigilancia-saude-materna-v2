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
      h2(tags$b(HTML("Mortalidade e morbidade materna: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset1"),
      width = 12,
      collapsible = FALSE,
      tabPanel(
        HTML("<b>Indicadores relacionados à mortalidade materna</b>"),
        value = "tabpanel_mortalidade",
        fluidRow(
          column(
            width = 12,
            HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
            HTML(
              "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos maternos no país, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-grav-puerp/' target = _blank>OOBr Óbitos de Gestantes e Puérperas</a>.
                </b> </div>"
            ),
            hr(),
            HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
          ),
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
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
                  style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Número de óbitos maternos &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_mort"), height = 450))
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
                    HTML("<b style='font-size:19px'> Razão de mortalidade materna por 100.000 nascidos vivos &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_mort"), height = 450))
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
                    HTML("<b style='font-size:19px'> Porcentagem de óbitos maternos por causas obstétricas diretas &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_mort"), height = 450))
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
                    HTML("<b style='font-size:19px'> Porcentagem de óbitos maternos diretos por causas específicas &nbsp;</b>"),
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
                        label = "Causa de óbito materno",
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_mort"), height = 365))
                )
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b>Indicadores relacionados à morbidade materna grave</b>"),
        value = "tabpanel_morbidade",
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
                  style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Porcentagem de casos de morbidade materna grave em internações obstétricas públicas &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_mmg"), height = 450))
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
                    HTML("<b style='font-size:19px'> Porcentagem de casos de morbidade materna grave por causas específicas &nbsp;</b>"),
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
                      width = 12,
                      selectizeInput(
                        inputId = ns("causa_mmg"),
                        label = "Causa de morbidade materna grave",
                        options = list(placeholder = "Selecione a causa de morbidade materna grave"),
                        choices = c(
                          "Hipertensão" = "prop_mmg_hipertensao",
                          "Hemorragia" = "prop_mmg_hemorragia",
                          "Infecção" = "prop_mmg_infeccao"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_mmg"), height = 365))
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
                    HTML("<b style='font-size:19px'> Porcentagem de casos de morbidade materna grave com internação em UTI &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_mmg"), height = 450))
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
                    HTML("<b style='font-size:19px'> Porcentagem de casos com morbidade materna grave com tempo de permanência prolongado &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_mmg"), height = 450))
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
                    HTML("<b style='font-size:19px'> Porcentagem de casos com morbidade materna grave com transfusão sanguínea &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_mmg"), height = 450))
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
                    HTML("<b style='font-size:19px'> Porcentagem de casos com morbidade materna grave com intervenções cirúrgicas &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_mmg"), height = 450))
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
      soma_obitos_mat_totais = rep("sum(obitos_mat_totais)", 2),
      rmm = c("round(sum(obitos_mat_totais) / sum(nascidos) * 100000, 1)", "30"),
      prop_obitos_diretos = rep("round(sum(obitos_mat_diretos) / sum(obitos_mat_totais) * 100, 1)", 2),
      prop_obitos_aborto = rep("round(sum(obitos_mat_aborto) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_hipertens = rep("round(sum(obitos_mat_hipertensao) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_hemo = rep("round(sum(obitos_mat_hemorragia) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_infec = rep("round(sum(obitos_mat_infec_puerperal) / sum(obitos_mat_diretos) * 100, 1)", 2),
      soma_casos_mmg = rep("sum(casos_mmg)", 2),
      prop_mmg_int_publicas = rep("round(sum(casos_mmg) / sum(total_internacoes) * 100, 1)", 2),
      prop_mmg_hipertensao = rep("round(sum(casos_mmg_hipertensao) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_hemorragia = rep("round(sum(casos_mmg_hemorragia) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_infeccao = rep("round(sum(casos_mmg_infeccoes) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_uti = rep("round(sum(casos_mmg_uti) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_tmp = rep("round(sum(casos_mmg_tmp) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_transfusao = rep("round(sum(casos_mmg_transfusao) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_cirurgia = rep("round(sum(casos_mmg_cirurgia) / sum(casos_mmg) * 100, 1)", 2)
    )

    bloco6_calcs_resumo <- bloco6_calcs |>
      dplyr::mutate(
        prop_mif_investigado = c("round((sum(obito_mif_investigado_com_ficha_sintese[ano <= 2020], na.rm = TRUE) + sum(obito_mif_investigado_sem_ficha_sintese[ano <= 2020], na.rm = TRUE))/sum(total_obitos_mulher_idade_fertil[ano <= 2020], na.rm = TRUE) * 100, 2)", "100"),
        prop_obito_materno_investigado = c("round((sum(obito_materno_investigado_com_ficha_sintese[ano <= 2020], na.rm = TRUE) + sum(obito_materno_investigado_sem_ficha_sintese[ano <= 2020], na.rm = TRUE))/sum(total_obitos_maternos[ano <= 2020], na.rm = TRUE) * 100, 2)", "100")
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
    output$input_localidade_resumo_mort <- renderUI({
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
          prop_mif_investigado = round((sum(obito_mif_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_mif_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_mulher_idade_fertil, na.rm = TRUE) * 100, 2),
          prop_obito_materno_investigado = round((sum(obito_materno_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_materno_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_maternos, na.rm = TRUE) * 100, 2),
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
        sub_registro_sim_muni_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "Estadual") {
        sub_registro_sim_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$estado
          )
      } else if (filtros()$nivel == "Regional") {
        sub_registro_sim_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$regiao
          )
      } else if (filtros()$nivel == "Nacional") {
        sub_registro_sim_uf_regioes_2015_2021 |>
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
          if (nivel_selecionado() == "Nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (nivel_selecionado() == "Regional")
            regiao == filtros()[[paste0("regiao", sufixo_inputs)]]
          else if (nivel_selecionado() == "Estadual")
            uf == filtros()[[paste0("estado", sufixo_inputs)]]
          else if (nivel_selecionado() == "Macrorregião de saúde")
            macro_r_saude == filtros()[[paste0("macro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_macro", sufixo_inputs)]]
          else if(nivel_selecionado() == "Microrregião de saúde")
            r_saude == filtros()[[paste0("micro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_micro", sufixo_inputs)]]
          else if(nivel_selecionado() == "Municipal")
            municipio == filtros()[[paste0("municipio", sufixo_inputs)]] & uf == filtros()[[paste0("estado_municipio", sufixo_inputs)]]
          else if (nivel_selecionado() == "Municípios semelhantes")
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
            nivel_selecionado() == "Nacional" ~ "Brasil",
            nivel_selecionado() == "Regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
            nivel_selecionado() == "Estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
            nivel_selecionado() == "Macrorregião de saúde" ~ filtros()[[paste0("macro", sufixo_inputs)]],
            nivel_selecionado() == "Microrregião de saúde" ~ filtros()[[paste0("micro", sufixo_inputs)]],
            nivel_selecionado() == "Municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]],
            nivel_selecionado() == "Municípios semelhantes" ~ "Média dos municípios semelhantes"
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

      if (nivel_selecionado() %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
        if (nivel_selecionado() == "Estadual"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()[[paste0("estado", sufixo_inputs)]],
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(localidade) |>
            dplyr::summarise(
              RMM_C = mean(RMM)
            )
        } else if (nivel_selecionado() == "Regional"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()[[paste0("regiao", sufixo_inputs)]],
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(localidade) |>
            dplyr::summarise(
              RMM_C = mean(RMM)
            )
        } else if (nivel_selecionado() == "Nacional"){
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
            nivel_selecionado() == "Nacional" ~ "Brasil",
            nivel_selecionado() == "Regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
            nivel_selecionado() == "Estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
            nivel_selecionado() == "Macrorregião de saúde" ~ filtros()[[paste0("macro", sufixo_inputs)]],
            nivel_selecionado() == "Microrregião de saúde" ~ filtros()[[paste0("micro", sufixo_inputs)]],
            nivel_selecionado() == "Municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]],
            nivel_selecionado() == "Municípios semelhantes" ~ "Média dos municípios semelhantes"
          )
        )
      }
    })

    data6_resumo_rmm_corrigida <- reactive({
      if (nivel_selecionado() %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023) {
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


    ## Criando o output do gráfico de radar -----------------------------------
    ### Definindo os indicadores que aparecerão no gráfico
    selected_indicators <- c(
      "rmm",
      "prop_obitos_diretos",
      "prop_mmg_int_publicas",
      "prop_mif_investigado",
      "prop_obito_materno_investigado"
    )

    ## Selecionando colunas relevantes nos dataframes de resumo e arrumando seus formatos
    df <- reactive({
      data6_resumo_rmm_corrigida()[, c('class', selected_indicators)] |>
        dplyr::mutate(
          class = ifelse(grepl("Brasil \\(valor de referência\\)", class), "Brasil", class)
        ) |>
        tidyr::pivot_longer(
          !class,
          names_to = "indicador",
          values_to = "values1"
        ) |>
        dplyr::mutate(
          values1 = round(values1, 1),
          sufixo = c("", rep("%", 4))
        )
    })

    df2 <- reactive({
      data6_resumo_referencia()[, selected_indicators] |>
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
              if (indicador_abrev %in% c("prop_mif_investigado", "prop_obito_materno_investigado")) {
                "incompletude"
              } else {
                tabela_indicadores$descricao_referencia[tabela_indicadores$nome_abreviado == indicador_abrev]
              }
            }
          ) |> unlist(),
          sufixo = c("", rep("%", 4))
        )
    })

    ### Criando o output
    output$spider_chart_mort <- output$spider_chart_mmg <- highcharter::renderHighchart({
      # Categorias para o eixo x
      categories <- lapply(
        selected_indicators,
        function(indicador_abrev) {
          if (indicador_abrev == "prop_mif_investigado") {
            "% de óbitos de mulheres em idade fértil investigados"
          } else if (indicador_abrev == "prop_obito_materno_investigado") {
            "% de óbitos maternos investigados"
          } else {
            gsub("Porcentagem", "%", tabela_radar$indicador[tabela_radar$nome_abreviado == indicador_abrev])
          }
        }) |> unlist()

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
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "303px",
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
        tamanho_caixa = "303px",
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
        tamanho_caixa = "303px",
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
        tamanho_caixa = "303px",
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
        tamanho_caixa = "303px",
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
        tamanho_caixa = "303px",
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
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })


    ### Para os indicadores de morbidade materna ------------------------------
    #### Porcentagem de casos de morbidade materna grave em internações obstétricas públicas ----
    output$caixa_b6_mmg_i1 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_mmg_int_publicas",
        titulo = "Porcentagem de casos de morbidade materna grave em internações obstétricas públicas",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_mmg_int_publicas,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave por hipertensão ------
    output$caixa_b6_mmg_i2 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_mmg_hipertensao",
        titulo = "Porcentagem de casos de morbidade materna grave por hipertensão",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_mmg_hipertensao,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave por hemorragia -------
    output$caixa_b6_mmg_i3 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_mmg_hemorragia",
        titulo = "Porcentagem de casos de morbidade materna grave por hemorragia",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_mmg_hemorragia,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave por infecção ---------
    output$caixa_b6_mmg_i4 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_mmg_infeccao",
        titulo = "Porcentagem de casos de morbidade materna grave por infecção",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_mmg_infeccao,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave com internação em UTI ----
    output$caixa_b6_mmg_i5 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_mmg_uti",
        titulo = "Porcentagem de casos de morbidade materna grave com internação em UTI",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_mmg_uti,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave com tempo de permanência prolongado ----
    output$caixa_b6_mmg_i6 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_mmg_tmp",
        titulo = "Porcentagem de casos de morbidade materna grave com tempo de permanência prolongado",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_mmg_tmp,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave com transfusão sanguínea ----
    output$caixa_b6_mmg_i7 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_mmg_transfusao",
        titulo = "Porcentagem de casos de morbidade materna grave com transfusão sanguínea",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_mmg_transfusao,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Porcentagem de casos de morbidade materna grave com intervenção cirúrgica ----
    output$caixa_b6_mmg_i8 <- renderUI({
      cria_caixa_server(
        dados = data6_resumo(),
        indicador = "prop_mmg_cirurgia",
        titulo = "Porcentagem de casos de morbidade materna grave com intervenção cirúrgica",
        tem_meta = FALSE,
        valor_de_referencia = data6_resumo_referencia()$prop_mmg_cirurgia,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_6",
        nivel_de_analise = nivel_selecionado()
      )
    })


    # Para os gráficos --------------------------------------------------------
    cols <- c("#2c115f", "#b73779", "#fc8961")

    ## Calculando os indicadores para cada ano do período selecionado ---------
    ### Para a localidade selecionada -----------------------------------------
    data6 <- reactive({
      bloco6 |>
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
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros())
    })

    data6_correcao_rmm <- reactive({
      if(filtros()$nivel %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
        if(filtros()$nivel == "Estadual"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$estado,
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        } else if(filtros()$nivel == "Regional"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$regiao,
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        } else if(filtros()$nivel=="Nacional"){
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
      if(filtros()$nivel %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
        dplyr::full_join(data6(), data6_correcao_rmm(), by= "ano") |>
          dplyr::select(!rmm) |>
          dplyr::mutate(rmm = RMM)
      } else{
        dplyr::full_join(data6(), data6_correcao_rmm(), by= "ano")
      }
    })

    ### Para a comparação selecionada -----------------------------------------
    data6_comp <- reactive({
      bloco6 |>
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
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros(), comp = TRUE)
    })

    data6_comp_correcao_rmm <- reactive({
      if(filtros()$nivel2 %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
        if(filtros()$nivel2 == "Estadual"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$estado2,
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        } else if(filtros()$nivel2 == "Regional"){
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$regiao2,
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        } else if(filtros()$nivel2=="Nacional"){
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
      if(filtros()$nivel2 %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
        dplyr::full_join(data6_comp(), data6_comp_correcao_rmm(), by= "ano") |>
          dplyr::select(!rmm) |>
          dplyr::mutate(rmm = RMM)
      } else{
        dplyr::full_join(data6_comp(), data6_comp_correcao_rmm(), by= "ano")
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
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = soma_obitos_mat_totais, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Número de óbitos maternos"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = soma_obitos_mat_totais, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = soma_obitos_mat_totais, group = class, colour = class)
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
          highcharter::hc_add_series(
            data = data6_rmm_corrigida(),
            type = "line",
            name = dplyr::if_else(filtros()$nivel == "Nacional", "Brasil", unique(data6()$class)),
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
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Óbitos maternos por 100 mil nascidos vivos"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6_rmm_corrigida(),
            type = "line",
            name = dplyr::if_else(filtros()$nivel == "Nacional", "Brasil", unique(data6()$class)),
            highcharter::hcaes(x = ano, y = rmm, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp_rmm_corrigida(),
            type = "line",
            name = dplyr::if_else(filtros()$nivel2 == "Nacional", "Brasil", unique(data6_comp()$class)),
            highcharter::hcaes(x = ano, y = rmm, group = class, colour = class)
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
            opacity = 0.7
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
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_obitos_diretos, group = class, colour = class)
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
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_obitos_diretos, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
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
          highcharter::hc_add_series(
            data = data6_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    ### Para os indicadores de morbidade materna ------------------------------
    #### Porcentagem de casos de morbidade materna grave em internações obstétricas públicas ----
    output$plot1_mmg <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_int_publicas, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_int_publicas, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_int_publicas, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_int_publicas, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_int_publicas, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave por causas específicas ----
    output$plot2_mmg <- highcharter::renderHighchart({
      data6_aux2 <- data6() |>
        dplyr::select(
          ano,
          eixo_y2 = dplyr::all_of(input$causa_mmg),
          class
        )

      data6_comp_aux2 <- data6_comp() |>
        dplyr::select(
          ano,
          eixo_y2 = dplyr::all_of(input$causa_mmg),
          class
        )

      data6_referencia_aux2 <- data6_referencia() |>
        dplyr::select(
          ano,
          eixo_y2 = dplyr::all_of(input$causa_mmg),
          class
        )

      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6()$soma_casos_mmg != 0,
            "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6_aux2,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y2, group = class, colour = class)
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
            data6()$soma_casos_mmg != 0 | data6_comp()$soma_casos_mmg != 0,
            "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
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
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data6_referencia_aux2,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y2, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave com internação em UTI ----
    output$plot3_mmg <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6()$soma_casos_mmg != 0,
            "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_uti, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_uti, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        need(
          data6()$soma_casos_mmg != 0 | data6_comp()$soma_casos_mmg != 0,
          "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_uti, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_uti, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_uti, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave com tempo de permanência prolongado ----
    output$plot4_mmg <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6()$soma_casos_mmg != 0,
            "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_tmp, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_tmp, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        need(
          data6()$soma_casos_mmg != 0 | data6_comp()$soma_casos_mmg != 0,
          "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_tmp, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_tmp, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_tmp, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave com transfusão sanguínea ----
    output$plot5_mmg <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6()$soma_casos_mmg != 0,
            "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_transfusao, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_transfusao, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        need(
          data6()$soma_casos_mmg != 0 | data6_comp()$soma_casos_mmg != 0,
          "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_transfusao, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_transfusao, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_transfusao, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #### Porcentagem de casos de morbidade materna grave com intervenção cirúrgica ----
    output$plot6_mmg <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        validate(
          need(
            data6()$soma_casos_mmg != 0,
            "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
          )
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_cirurgia, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_cirurgia, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        need(
          data6()$soma_casos_mmg != 0 | data6_comp()$soma_casos_mmg != 0,
          "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
        )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data6(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_cirurgia, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data6_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = prop_mmg_cirurgia, group = class, colour = class)
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
              data = data6_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = prop_mmg_cirurgia, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
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
