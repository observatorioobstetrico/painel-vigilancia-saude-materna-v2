#' bloco_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_2_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Planejamento Reprodutivo: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;"),
      style = "position: fixed; top: 56px; width: 93.75%; background-color: white; z-index: 100;"
    ),
    fluidRow(
      column(
        width = 4,
        HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
        HTML("<b style='font-size:18px'> Resumo do período </b>"),
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
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i1")), proxy.height = "300px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i2")), proxy.height = "300px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i3")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i4")), proxy.height = "325px")
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
              style = "height: 520px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil) &nbsp;</b>"),
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
              style = "height: 520px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Porcentagem de mulheres com mais de 3 partos anteriores &nbsp;</b>"),
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
              style = "height: 520px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Taxa de abortos inseguros por mil mulheres em idade fértil &nbsp;</b>"),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3"), height = 400))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 520px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Razão de abortos inseguros por 100 nascidos vivos &nbsp;</b>"),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4"), height = 400))
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

mod_bloco_2_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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


    ##### Dados para o resumo do perído para a localidade escolhida #####
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
      bloco2 |>
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
          porc_menor20 = round(sum(nvm_menor_que_20)/sum(pop_feminina_10_a_19) * 1000, 1),
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos * 100, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1)
        ) |>
        dplyr::ungroup()
    })


    data_resumo2 <- reactive({
      bloco2 |>
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
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Dados para o resumo do perído para a comparação com o valor nacional/de referência #####
    data_resumo_brasil <- reactive({
      bloco2 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_menor20 = 30,
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos * 100, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Dados do segundo bloco de indicadores para a localidade escolhida #####
    data2 <- reactive({
      bloco2 |>
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
          porc_menor20 = round(sum(nvm_menor_que_20)/sum(pop_feminina_10_a_19) * 1000, 1),
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos * 100, 1),
          tx_abortos_mil_mulheres_lim_inf = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 2) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 1)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_mil_mulheres_lim_sup = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 4) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 3)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_lim_inf = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*2)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*1))/sum(total_de_nascidos_vivos) *100, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1),
          tx_abortos_cem_nascidos_vivos_lim_sup = round(((((sum(abortos_sus_menor_30)*0.90)+ (sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*4) + (((sum(abortos_ans_menor_30)*0.90)+(sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75))*3))/sum(total_de_nascidos_vivos) * 100, 1),
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


    ##### Dados do segundo bloco de indicadores para quando há comparação #####
    data2_comp <- reactive({
      bloco2 |>
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
          porc_menor20 = round(sum(nvm_menor_que_20)/sum(pop_feminina_10_a_19) * 1000, 1),
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos * 100, 1),
          tx_abortos_mil_mulheres_lim_inf = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 2) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 1)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_mil_mulheres_lim_sup = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 4) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 3)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_lim_inf = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*2)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*1))/sum(total_de_nascidos_vivos) *100, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1),
          tx_abortos_cem_nascidos_vivos_lim_sup = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*4)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*3))/sum(total_de_nascidos_vivos) *100, 1),
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


    ##### Valores nacionais/de referência para os gráficos #####
    data2_referencia <- reactive({
      bloco2 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_menor20 = 30,
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos * 100, 1),
          tx_abortos_mil_mulheres_lim_inf = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 2) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 1)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_mil_mulheres_lim_sup = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 4) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 3)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_lim_inf = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*2)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*1))/sum(total_de_nascidos_vivos) *100, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1),
          tx_abortos_cem_nascidos_vivos_lim_sup = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*4)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*3))/sum(total_de_nascidos_vivos) *100, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })


    ##### Dados de incompletude e cobertura para os indicadores do segundo bloco #####
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
          idademae = round(sum(idademae_incompletos, na.rm = TRUE)/sum(idademae_totais, na.rm = TRUE) * 100, 2),
          qtdpartces = round(sum(qtdpartces_incompletos, na.rm = TRUE)/sum(qtdpartces_totais, na.rm = TRUE) * 100, 2),
          qtdpartnor = round(sum(qtdpartnor_incompletos, na.rm = TRUE)/sum(qtdpartnor_totais, na.rm = TRUE) * 100, 2),
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


    ##### Criando as caixinhas para os indicadores do segundo bloco #####
    output$caixa_b2_i1 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_menor20",
        titulo = "Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil)",
        tem_meta = TRUE,
        valor_de_referencia = 30,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "300px",
        fonte_titulo = "15px",
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

    output$caixa_b2_i2 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_mais_3pt",
        titulo = "Porcentagem de nascidos vivos de mulheres com mais de 3 partos anteriores",
        tem_meta = FALSE,
        valor_de_referencia = data_resumo_brasil()$porc_mais_3pt,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
        fonte_titulo = "15px",
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

    output$caixa_b2_i3 <- renderUI({
      cria_caixa_server(
        dados = data_resumo2(),
        indicador = "tx_abortos_mil_mulheres_valor_medio",
        titulo = "Valor médio da taxa de abortos inseguros por mil MIF",
        tem_meta = FALSE,
        valor_de_referencia = data_resumo_brasil()$tx_abortos_mil_mulheres_valor_medio,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "300px",
        fonte_titulo = "15px",
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

    output$caixa_b2_i4 <- renderUI({
      cria_caixa_server(
        dados = data_resumo2(),
        indicador = "tx_abortos_cem_nascidos_vivos_valor_medio",
        titulo = "Valor médio da razão de abortos inseguros por 100 nascidos vivos",
        tem_meta = FALSE,
        valor_de_referencia = data_resumo_brasil()$tx_abortos_cem_nascidos_vivos_valor_medio,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "300px",
        fonte_titulo = "15px",
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


    ##### Criando os modais para quando há incompletude #####
    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$idademae,
        variavel_incompletude1 = "IDADEMAE",
        descricao_incompletude1 = "ignorados, em branco ou maiores que 55",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$idademae > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    })

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

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$qtdpartces > 5, na.rm = TRUE) | any(data_incompletude()$qtdpartnor > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )


    ##### Definindo as cores para os gráficos #####
    cols <- c("#2c115f", "#b73779", "#fc8961")


    ##### Criando o gráfico da taxa específica de fecundidade #####
    output$plot1 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data2(),
            name = dplyr::if_else(filtros()$nivel == "Nacional", "Brasil", unique(data2()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_menor20, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data2_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_menor20, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data2(),
            name = dplyr::if_else(filtros()$nivel == "Nacional", "Brasil", unique(data2()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_menor20, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data2_comp(),
            name = dplyr::if_else(filtros()$nivel2 == "Nacional", "Brasil", unique(data2_comp()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_menor20, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data2_referencia(),
              type = "line",
              name = "Referência (países desenvolvidos)",
              highcharter::hcaes(x = ano, y = porc_menor20, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    ##### Criando o gráfico da porcentagem de mulheres com mais de 3 partos anteriores #####
    output$plot2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data2(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_mais_3pt, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
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
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data2_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_mais_3pt, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    ##### Criando o gráfico da taxa de abortos inseguros por mil mulheres em idade fértil #####
    output$plot3 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2015,
          "Este indicador só está disponível a partir de 2014."
        )
      )
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data2() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_mil_mulheres_lim_inf:,f} - {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              name = "Referência (média nacional)",
              data = data2_referencia() |> dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_mil_mulheres_lim_inf:,f} - {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
              ),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data2() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_mil_mulheres_lim_inf:,f} - {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_add_series(
            data = data2_comp() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_mil_mulheres_lim_inf:,f} - {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              name = "Referência (média nacional)",
              data = data2_referencia() |> dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_mil_mulheres_lim_inf:,f} - {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
              ),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      }
    })


    ##### Criando o gráfico da taxa de abortos inseguros por 100 nascidos vivos #####

    output$plot4 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2015,
          "Este indicador só está disponível a partir de 2014."
        )
      )
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data2() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} - {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              name = "Referência (média nacional)",
              data = data2_referencia() |> dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} - {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
              ),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data2() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} - {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_add_series(
            data = data2_comp() |> dplyr::filter(ano >= 2015),
            type = "line",
            highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio, group = class, colour = class),
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} - {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              name = "Referência (média nacional)",
              data = data2_referencia() |> dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} - {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
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
