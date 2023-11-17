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
            offset = 3,
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i5")), proxy.height = "325px")
          ),

        ),
        fluidRow(
          column(
            offset = 3,
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i6")), proxy.height = "325px")
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
              style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Porcentagem de baixo peso ao nascer (< 2500 g) &nbsp;</b>"
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
                      "Menor que 1500 g" = "porc_peso_menor_1500",
                      "De 1500 a 1999 g" = "porc_peso_1500_a_1999",
                      "De 2000 a 2499 g" = "porc_peso_2000_a_2499",
                      "Menor que 2500 g" = "porc_baixo_peso"
                    ),
                    width = "100%", selected = "porc_baixo_peso"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1"), height = 345))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_1"), height = 410))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
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
                    label = "Idade Gestacional",
                    options = list(placeholder = "Selecione a idade gestacional"),
                    choices = c(
                      "Menos que 28 semanas" = "porc_menos_de_28_semanas",
                      "De 28 a 32 semanas" = "porc_28_a_32_semanas",
                      "De 33 a 34 semanas" = "porc_33_a_34_semanas",
                      "De 35 a 36 semanas" = "porc_35_a_36_semanas",
                      "Menos que 36 semanas" = "porc_premat"

                    ),
                    width = "100%", selected = "porc_premat"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2"), height = 345))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_1"), height = 410))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3"), height = 410))
            )
          ),
          column(
            width = 6,
            fluidRow(
              column(
                width = 12,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:18px'> Porcentagem de nascidos vivos com asfixia &nbsp;</b>"),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4"), height = 400))
                )
              )
            )
          )
        )
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
          style = "height: 630px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 15%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequência dos grupos de malformações prioritárias para vigilância &nbsp;</b>"),
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
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_malformacoes")))
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

    ##### Criando o output que recebe a localidade e o ano escolhidos #####
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

    #cores pros graficos
    cols <- c("#2c115f", "#b73779", "#fc8961")

    #dados com filtros e calculos porcentagens
    data5 <- reactive({
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_peso_menor_1500 = round(sum(nascidos_vivos_peso_menor_1500)/sum(nascidos_vivos_com_baixo_peso)  * 100, 1),
          porc_peso_1500_a_1999 = round(sum(nascidos_vivos_peso_1500_a_1999)/sum(nascidos_vivos_com_baixo_peso)  * 100, 1),
          porc_peso_2000_a_2499= round(sum(nascidos_vivos_peso_2000_a_2499)/sum(nascidos_vivos_com_baixo_peso)  * 100, 1),
          porc_menos_de_28_semanas= round(sum(nascidos_vivos_menos_de_28_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          porc_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          porc_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          porc_35_a_36_semanas= round(sum(nascidos_vivos_35_a_36_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          porc_premat_faltantes =  round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                               nascidos_vivos_28_a_32_semanas,
                                                                                               nascidos_vivos_33_a_34_semanas,
                                                                                               nascidos_vivos_35_a_36_semanas)))) / sum(nascidos_vivos_prematuros) * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1),
          class = dplyr::case_when(
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

    data5_serie <- reactive({
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_peso_menor_1500 = round(sum(nascidos_vivos_peso_menor_1500)/sum(total_de_nascidos_vivos)  * 100, 1),
          porc_peso_1500_a_1999 = round(sum(nascidos_vivos_peso_1500_a_1999)/sum(total_de_nascidos_vivos)  * 100, 1),
          porc_peso_2000_a_2499= round(sum(nascidos_vivos_peso_2000_a_2499)/sum(total_de_nascidos_vivos)  * 100, 1),
          porc_menos_de_28_semanas= round(sum(nascidos_vivos_menos_de_28_semanas)/sum(total_de_nascidos_vivos) * 100, 1),
          porc_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas)/sum(total_de_nascidos_vivos) * 100, 1),
          porc_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas)/sum(total_de_nascidos_vivos) * 100, 1),
          porc_35_a_36_semanas= round(sum(nascidos_vivos_35_a_36_semanas)/sum(total_de_nascidos_vivos) * 100, 1),
          porc_premat_faltantes =  round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                               nascidos_vivos_28_a_32_semanas,
                                                                                               nascidos_vivos_33_a_34_semanas,
                                                                                               nascidos_vivos_35_a_36_semanas)))) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1),
          class = dplyr::case_when(
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
    output$input_localidade_resumo <- renderUI({
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

    data_resumo <- reactive({
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1),
          porc_peso_menor_1500 = round(sum(nascidos_vivos_peso_menor_1500)/sum(nascidos_vivos_com_baixo_peso)  * 100, 1),
          porc_peso_1500_a_1999 = round(sum(nascidos_vivos_peso_1500_a_1999)/sum(nascidos_vivos_com_baixo_peso)  * 100, 1),
          porc_peso_2000_a_2499= round(sum(nascidos_vivos_peso_2000_a_2499)/sum(nascidos_vivos_com_baixo_peso)  * 100, 1),
          porc_menos_de_28_semanas= round(sum(nascidos_vivos_menos_de_28_semanas)/sum(nascidos_vivos_prematuros)  * 100, 1),
          porc_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas)/sum(nascidos_vivos_prematuros)  * 100, 1),
          porc_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas)/sum(nascidos_vivos_prematuros)  * 100, 1),
          porc_35_a_36_semanas= round(sum(nascidos_vivos_35_a_36_semanas)/sum(nascidos_vivos_prematuros)  * 100, 1),
          porc_premat_faltantes =  round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                               nascidos_vivos_28_a_32_semanas,
                                                                                               nascidos_vivos_33_a_34_semanas,
                                                                                               nascidos_vivos_35_a_36_semanas)))) / sum(nascidos_vivos_prematuros) * 100, 1)
        ) |>
        dplyr::ungroup()
    })

    data_resumo_referencia <- reactive({
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
          porc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1)
        )

    })


    ##### Dados de incompletude e cobertura para os indicadores do quinto bloco #####
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
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
          peso = round(sum(peso_incompletos, na.rm = TRUE)/sum(peso_totais, na.rm = TRUE) * 100, 2),
          gestacao = round(sum(gestacao_incompletos, na.rm = TRUE)/sum(gestacao_totais, na.rm = TRUE) * 100, 2),
          semagestac = round(sum(semagestac_incompletos, na.rm = TRUE)/sum(semagestac_totais, na.rm = TRUE) * 100, 2),
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


    observeEvent(c(input$botao1, input$botao2), {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$peso,
        variavel_incompletude1 = "PESO",
        descricao_incompletude1 = "em branco ou preenchida com 9999",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$peso > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
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

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$gestacao > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
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

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao4", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$gestacao > 5, na.rm = TRUE) | any(data_incompletude()$semagestac > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao4", anim = TRUE, animType = "fade", time = 0.8)
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

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao5", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$semagestac > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao5", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    #dados comparação 2 com filtros e calculos porcentagens (segunda seleção)
    data5_comp <- reactive({
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1),
          porc_peso_menor_1500 = round(sum(nascidos_vivos_peso_menor_1500)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_peso_1500_a_1999 = round(sum(nascidos_vivos_peso_1500_a_1999)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_peso_2000_a_2499= round(sum(nascidos_vivos_peso_2000_a_2499)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_menos_de_28_semanas= round(sum(nascidos_vivos_menos_de_28_semanas)/sum(nascidos_vivos_prematuros)  * 100, 1),
          porc_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas)/sum(nascidos_vivos_prematuros)  * 100, 1),
          porc_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas)/sum(nascidos_vivos_prematuros)  * 100, 1),
          porc_35_a_36_semanas= round(sum(nascidos_vivos_35_a_36_semanas)/sum(nascidos_vivos_prematuros)  * 100, 1),
          porc_premat_faltantes =  round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                               nascidos_vivos_28_a_32_semanas,
                                                                                               nascidos_vivos_33_a_34_semanas,
                                                                                               nascidos_vivos_35_a_36_semanas)))) / sum(nascidos_vivos_prematuros) * 100, 1),

          class = dplyr::case_when(
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
    data5_comp_series <- reactive({
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1),
          porc_peso_menor_1500 = round(sum(nascidos_vivos_peso_menor_1500)/sum(total_de_nascidos_vivos) * 100, 1),
          porc_peso_1500_a_1999 = round(sum(nascidos_vivos_peso_1500_a_1999)/sum(total_de_nascidos_vivos) * 100, 1),
          porc_peso_2000_a_2499= round(sum(nascidos_vivos_peso_2000_a_2499)/sum(total_de_nascidos_vivos) * 100, 1),
          porc_menos_de_28_semanas= round(sum(nascidos_vivos_menos_de_28_semanas)/sum(total_de_nascidos_vivos)  * 100, 1),
          porc_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas)/sum(total_de_nascidos_vivos)  * 100, 1),
          porc_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas)/sum(total_de_nascidos_vivos)  * 100, 1),
          porc_35_a_36_semanas= round(sum(nascidos_vivos_35_a_36_semanas)/sum(total_de_nascidos_vivos)  * 100, 1),
          porc_premat_faltantes =  round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                               nascidos_vivos_28_a_32_semanas,
                                                                                               nascidos_vivos_33_a_34_semanas,
                                                                                               nascidos_vivos_35_a_36_semanas)))) / sum(total_de_nascidos_vivos) * 100, 1),

          class = dplyr::case_when(
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

    data_referencia <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos, na.rm = TRUE),
          porc_premat = 10,
          porc_termo_precoce = 20,
          porc_peso_menor_1500 = round(sum(nascidos_vivos_peso_menor_1500)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_peso_1500_a_1999 = round(sum(nascidos_vivos_peso_1500_a_1999)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_peso_2000_a_2499 = round(sum(nascidos_vivos_peso_2000_a_2499)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_menos_de_28_semanas= round(sum(nascidos_vivos_menos_de_28_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          porc_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          porc_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          porc_35_a_36_semanas= round(sum(nascidos_vivos_35_a_36_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_referencia_baixo_peso_aux <- reactive({
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
          porc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_referencia_baixo_peso <- reactive({
      data.frame(
        ano = filtros()$ano2[1]:filtros()$ano2[2],
        porc_baixo_peso = data_referencia_baixo_peso_aux()$porc_baixo_peso,
        class = "Referência"
      )
    })

    data_referencia_baixo_peso_comp_aux <- reactive({
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
          porc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_referencia_baixo_peso_comp <- reactive({
      data.frame(
        ano = filtros()$ano2[1]:filtros()$ano2[2],
        porc_baixo_peso = data_referencia_baixo_peso_comp_aux()$porc_baixo_peso,
        class = "Referência"
      )
    })

    data5_referencia <- reactive({
      bloco5 |>
        dplyr::filter(ano >= max(filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "Nacional")
            ano >= max(filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
          br_porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          br_porc_peso_menor_1500 = round(sum(nascidos_vivos_peso_menor_1500)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          br_porc_peso_1500_a_1999 = round(sum(nascidos_vivos_peso_1500_a_1999)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          br_porc_peso_2000_a_2499= round(sum(nascidos_vivos_peso_2000_a_2499)/sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          br_porc_menos_de_28_semanas= round(sum(nascidos_vivos_menos_de_28_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          br_porc_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          br_porc_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          br_porc_35_a_36_semanas= round(sum(nascidos_vivos_35_a_36_semanas)/sum(nascidos_vivos_prematuros) * 100, 1),
          br_porc_premat_faltantes =  round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                               nascidos_vivos_28_a_32_semanas,
                                                                                               nascidos_vivos_33_a_34_semanas,
                                                                                               nascidos_vivos_35_a_36_semanas)))) / sum(nascidos_vivos_prematuros) * 100, 1),

          br_porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          br_porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1),
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

    data5_juncao_aux <-  reactive({dplyr::full_join(data5(), data5_referencia(), by = "ano")})
    data5_baixo_peso <- reactive(
      data5_serie() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$baixo_peso),
          class
        )
    )
    data5_comp_baixo_peso <- reactive(
      data5_comp_series() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$baixo_peso),
          class
        )
    )
    data5_comp_prematuridade <- reactive(
      data5_comp_series() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_prematuridade),
          class
        )
    )
    data5_prematuridade <- reactive(
      data5_serie() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_prematuridade),
          class
        )
    )

    ## asfixia
    data5_asfixia <- reactive({
      asfixia |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
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
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ dplyr::if_else(
              filtros()$comparar == "Não",
              "Brasil (valor de referência)",
              dplyr::if_else(
                filtros()$mostrar_referencia == "nao_mostrar_referencia",
                "Brasil",
                "Brasil (valor de referência)"
              )
            ),
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })

    asfixia_referencia <- reactive({
      asfixia |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_resumo_brasil_asfixia <- reactive({
      asfixia |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1)) |>
        dplyr::ungroup()
    })

    data5_comp_asfixia <- reactive({
      asfixia |>
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1),
          class = dplyr::case_when(
            filtros()$nivel2 == "Nacional" ~ dplyr::if_else(
              filtros()$comparar == "Não",
              "Brasil (valor de referência)",
              dplyr::if_else(
                filtros()$mostrar_referencia == "nao_mostrar_referencia",
                "Brasil",
                "Brasil (valor de referência)"
              )
            ),
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

    data_resumo_asfixia <- reactive({
      asfixia |>
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1)) |>
        dplyr::ungroup()
    })

    #nome da localidade para os gráficos sem comparações
    local <- reactive({
      dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )
    })
    output$comparar <- renderText({filtros()$comparar})
    outputOptions(output, "comparar", suspendWhenHidden = FALSE)

    output$b5_i1 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_baixo_peso",
        titulo = "Porcentagem de baixo peso ao nascer (< 2500 g)",
        tem_meta = TRUE,
        valor_de_referencia = data_resumo_referencia()$porc_baixo_peso,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
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

    output$b5_i2 <- renderUI({
      cria_caixa_conjunta_bloco5(
        dados = data_resumo(),
        indicador = "baixo peso",
        titulo = "Dentre os nascidos vivos com baixo peso (< 2500 g),"
      )
    })

    output$b5_i3 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_premat",
        titulo = "Porcentagem de nascimentos prematuros",
        tem_meta = TRUE,
        valor_de_referencia = 10,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
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

    output$b5_i4 <- renderUI({
      cria_caixa_conjunta_bloco5(
        dados = data_resumo(),
        indicador = "prematuridade",
        titulo = "Dentre os nascimentos prematuros,"
      )
    })

    output$b5_i5 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_28_a_32_semanas",
        titulo = "Porcentagem de nascimentos prematuros com 28 a 32 semanas",
        tem_meta = TRUE,
        valor_de_referencia = 20,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
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

    output$b5_i6 <- renderUI({
      cria_caixa_server(
        dados = data_resumo_asfixia(),
        indicador = "porc_nascidos_vivos_asfixia1",
        titulo = "Porcentagem de nascidos vivos com asfixia",
        tem_meta = FALSE,
        valor_de_referencia = data_resumo_brasil_asfixia()$porc_nascidos_vivos_asfixia1,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
        fonte_titulo = "16px",
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

    output$b5_i8 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_33_a_34_semanas",
        titulo = "Porcentagem de nascimentos prematuros com 32 a 34 semanas",
        tem_meta = TRUE,
        valor_de_referencia = 20,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
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

    output$b5_i9 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_35_a_36_semanas",
        titulo = "Porcentagem de nascimentos prematuros com 35 a 36 semanas",
        tem_meta = TRUE,
        valor_de_referencia = 20,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
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

    output$b5_i10 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_termo_precoce",
        titulo = "Porcentagem de nascimentos termo precoce",
        tem_meta = TRUE,
        valor_de_referencia = 20,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
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


    #gráfico porcentagem baixo peso
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

        if (input$baixo_peso == "porc_baixo_peso") {
          grafico_base <- grafico_base |>
            highcharter::hc_add_series(
              data = data_referencia_baixo_peso(),
              type = "line",
              name = "Referência para a localidade (meta de redução global)",
              highcharter::hcaes(x = ano, y = porc_baixo_peso, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        } else {
          grafico_base
          # grafico_base <- grafico_base |>
          #   highcharter::hc_add_series(
          #     data = data_referencia(),
          #     type = "line",
          #     name = "Referência (média nacional))",
          #     highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #     dashStyle = "ShortDot",
          #     opacity = 0.8
          #   )
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
              data = data_referencia_baixo_peso(),
              type = "line",
              name = glue::glue("Referência para {unique(data5()$class)} (meta de redução global)"),
              highcharter::hcaes(x = ano, y = porc_baixo_peso, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #gráfico porcentagem prematuros
    output$plot2 <- highcharter::renderHighchart({

      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_prematuridade(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_premat, group = class, colour = class),
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
            data = data_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_premat, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
    })

    #gráficos porcentagem termo precoce
    output$plot3 <- highcharter::renderHighchart({

      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class),
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
            data = data_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
    })

    data5_juncao_aux_invertido <- reactive({
      data5_juncao_aux() |>
        dplyr::arrange(dplyr::desc(ano)) |>
        dplyr::mutate(
          ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
        )
    })

    ##### Criando o gráfico de asfixia #####
    output$plot4 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_asfixia(),
            type = "line",
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
              data = asfixia_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5_asfixia(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = asfixia_comp(),
            type = "line",
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
              data = asfixia_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #grafico distribuicao do baixo peso
    output$plot1_1 <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "De 2000 a 2499 g",
          data =  data5_juncao_aux_invertido(),
          highcharter::hcaes(x = ano, y = porc_peso_2000_a_2499),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_peso_2000_a_2499:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500 a 1999 g",
          data =  data5_juncao_aux_invertido(),
          highcharter::hcaes(x = ano, y = porc_peso_1500_a_1999),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_peso_1500_a_1999:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1500 g",
          data =  data5_juncao_aux_invertido(),
          highcharter::hcaes(x = ano, y = porc_peso_menor_1500),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_peso_menor_1500:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(5, direction = -1)[-c(1, 5)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data5_juncao_aux_invertido()$ano), allowDecimals = FALSE, reversed = TRUE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100)

    })

    #grafico distribuicao da prematuridade
    output$plot2_1 <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          data = data5_juncao_aux_invertido(),
          name = "Sem informação",
          highcharter::hcaes(x = ano, y = porc_premat_faltantes),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_premat_faltantes:,f}% </b>"
          )
        )  |>
        highcharter::hc_add_series(
          data = data5_juncao_aux_invertido(),
          name = "De 35 a 36 semanas",
          highcharter::hcaes(x = ano, y = porc_35_a_36_semanas ),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_35_a_36_semanas:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data5_juncao_aux_invertido(),
          name = "De 33 a 34 semanas",
          highcharter::hcaes(x = ano, y = porc_33_a_34_semanas),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_33_a_34_semanas:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data5_juncao_aux_invertido(),
          name = "De 28 a 32 semanas",
          highcharter::hcaes(x = ano , y = porc_28_a_32_semanas),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_28_a_32_semanas:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          data = data5_juncao_aux_invertido(),
          name = "Menos de 28 semanas",
          highcharter::hcaes(x = ano, y = porc_menos_de_28_semanas),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_menos_de_28_semanas:,f}% </b>"
          )
        ) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""),categories = unique(data5_juncao_aux()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos"), min = 0, max = 100) |>
        highcharter::hc_legend(reversed = TRUE)


    })

    ##### Criando tabela #####
    data5_nascidos_vivos <- reactive({
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
        dplyr::summarize(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos)
        ) |>
        dplyr::ungroup()
    })

    data5_malformacao <- reactive({
      malformacao |>
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
        dplyr::group_by(grupo_de_anomalias_congenitas, codigo_cid, descricao, ano) |>
        dplyr::summarize(
          frequencia = sum(nascidos_vivos_anomalia)
        ) |>
        dplyr::ungroup() |>
        dplyr::right_join(data5_nascidos_vivos()) |>
        dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
        dplyr::mutate(anomalia_descricao = paste(codigo_cid, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

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
              format = list(aggregated = reactable::colFormat(prefix = glue::glue("{filtros()$ano2[1]} a {filtros()$ano2[2]}")))
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
