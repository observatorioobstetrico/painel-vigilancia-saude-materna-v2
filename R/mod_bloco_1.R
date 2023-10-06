#' bloco_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyalert::useShinyalert(force = TRUE),  # Set up shinyalert
    div(
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Condições socioeconômicas e de acesso ao serviço de saúde: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;"),
      style = "position: fixed; top: 56px; width: 93.75%; background-color: white; z-index: 100;"
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
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i3")), proxy.height = "293px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i1")), proxy.height = "293px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i4")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i5")), proxy.height = "325px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i6")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i7")), proxy.height = "325px")
          )
        ),
        fluidRow(
          column(
            offset = 3,
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i8")), proxy.height = "325px")
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
                HTML("<b style='font-size:19px'> Porcentagem de nascidos vivos por faixa etária da mãe &nbsp;</b>"),
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
                    inputId = ns("faixa_et"),
                    label = "Faixa etária da mãe",
                    options = list(placeholder = "Selecione a faixa etária da mãe"),
                    choices = c(
                      "Menor que 20 anos" = "porc_nvm_menor_que_20_anos",
                      "Entre 20 e 34 anos" = "porc_nvm_entre_20_e_34_anos",
                      "Maior que 34 anos" = "porc_nvm_maior_que_34_anos"
                    ),
                    width = "100%"
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
                HTML("<b style='font-size:19px'> Porcentagem de nascidos vivos por raça/cor da mãe &nbsp;</b>"),
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
              fluidRow(
                column(
                  width = 12,
                  selectizeInput(
                    inputId = ns("raca"),
                    label = "Raça/cor da mãe",
                    options = list(placeholder = "Selecione a raça/cor da mãe"),
                    choices = c(
                      "Amarela" = "porc_nvm_com_cor_da_pele_amarela",
                      "Branca" = "porc_nvm_com_cor_da_pele_branca",
                      "Indígena" = "porc_nvm_indigenas",
                      "Parda" = "porc_nvm_com_cor_da_pele_parda",
                      "Preta" = "porc_nvm_com_cor_da_pele_preta"
                    ),
                    width = "100%"
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
                HTML("<b style='font-size:19px'> Porcentagem de nascidos vivos por escolaridade da mãe &nbsp;</b>"),
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
                    inputId = ns("esc"),
                    label = "Escolaridade da mãe",
                    options = list(placeholder = "Selecione a escolaridade da mãe"),
                    choices = c(
                      "Até 3 anos de estudo" = "porc_nvm_com_escolaridade_ate_3",
                      "4 a 7 anos de estudo" = "porc_nvm_com_escolaridade_de_4_a_7",
                      "8 a 11 anos de estudo" = "porc_nvm_com_escolaridade_de_8_a_11",
                      "Acima de 11 anos de estudo" = "porc_nvm_com_escolaridade_acima_de_11"
                    ),
                    width = "100%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3"), height = 345))
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
                HTML("<b style='font-size:19px'> Porcentagem de mulheres em idade fértil usuárias exclusivas do SUS &nbsp;</b>"),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4"), height = 420))
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
              style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:19px'> Cobertura populacional com equipes de Saúde da Família &nbsp;</b>"),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5"), height = 420))
            )
          )
        )
      )
    )
  )
}

#' bloco_1 Server Functions
#'
#' @noRd
mod_bloco_1_server <- function(id, filtros){
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


    ##### Definindo as cores para os gráficos #####
    cols <- c("#2c115f", "#b73779", "#fc8961")


    ##### Dados do primeiro bloco de indicadores para a localidade escolhida #####
    data1 <- reactive({
      bloco1 |>
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
          populacao_feminina_10_a_49 = sum(populacao_feminina_10_a_49),
          media_populacao_feminina_10_a_49 = round(populacao_feminina_10_a_49/(filtros()$ano2[2] - filtros()$ano2[1] + 1)),
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = round(sum(media_cobertura_esf)/sum(populacao_total) * 100, 1),
          porc_nvm_menor_que_20_anos = round(sum(nvm_menor_que_20_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1),
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


    ##### Dados do primeiro bloco de indicadores para quando há comparação #####
    data1_comp <- reactive({
      bloco1 |>
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
          populacao_feminina_10_a_49 = sum(populacao_feminina_10_a_49),
          media_populacao_feminina_10_a_49 = round(populacao_feminina_10_a_49/(filtros()$ano2[2] - filtros()$ano2[1] + 1)),
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = round(sum(media_cobertura_esf)/sum(populacao_total) * 100, 1),
          porc_nvm_menor_que_20_anos = round(sum(nvm_menor_que_20_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1),
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


    ##### Dados para o resumo do período para a localidade escolhida #####
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
      bloco1 |>
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
          populacao_feminina_10_a_49 = sum(populacao_feminina_10_a_49),
          media_populacao_feminina_10_a_49 = round(populacao_feminina_10_a_49/(filtros()$ano2[2] - filtros()$ano2[1] + 1)),
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = round(sum(media_cobertura_esf)/sum(populacao_total) * 100, 1),
          porc_nvm_menor_que_20_anos = round(sum(nvm_menor_que_20_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1)
        ) |>
        dplyr::ungroup()
    })

    data_brasil_resumo <- reactive({
      bloco1 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          populacao_feminina_10_a_49 = sum(populacao_feminina_10_a_49),
          media_populacao_feminina_10_a_49 = round(populacao_feminina_10_a_49/(filtros()$ano2[2] - filtros()$ano2[1] + 1)),
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = round(sum(media_cobertura_esf)/sum(populacao_total) * 100, 1),
          porc_nvm_menor_que_20_anos = round(sum(nvm_menor_que_20_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1)
        ) |>
        dplyr::ungroup()
    })

    output$caixa_b1_i1 <- renderUI({

      if (filtros()$comparar == "Não") {
        if (filtros()$nivel == "Municipal") {
          idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
          posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        } else if (filtros()$nivel == "Estadual") {
          idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)]))
          posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)])
        } else if (filtros()$nivel == "Nacional") {
          idhm <- 0.727
        } else {
          idhm <- NaN
        }

        if (is.na(idhm)) {
          texto_comp <- "Classificação não aplicável"
        } else {
          texto_posicao <- dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "",
            filtros()$nivel == "Estadual" ~ "({posicao_idhm}º lugar entre os 27 estados brasileiros)",
            filtros()$nivel == "Municipal" ~ "({posicao_idhm}º lugar entre os 5.570 municípios brasileiros)",
          )
          texto_comp <- dplyr::case_when(
            as.numeric(idhm) <= 0.499 ~ "Muito baixo {glue::glue(texto_posicao)}",
            as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo {glue::glue(texto_posicao)}",
            as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio {glue::glue(texto_posicao)}",
            as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto {glue::glue(texto_posicao)}",
            as.numeric(idhm) > 0.8 ~ "Muito alto {glue::glue(texto_posicao)}"
          )
        }
      } else {
        req(input$localidade_resumo)
        if (input$localidade_resumo == "escolha1") {
          if (filtros()$nivel == "Municipal") {
            idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
            posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
          } else if (filtros()$nivel == "Estadual") {
            idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)]))
            posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)])
          } else if (filtros()$nivel == "Nacional") {
            idhm <- 0.727
          } else {
            idhm <- NaN
          }

          if (is.na(idhm)) {
            texto_comp <- "Classificação não aplicável"
          } else {
            texto_posicao <- dplyr::case_when(
              filtros()$nivel == "Nacional" ~ "",
              filtros()$nivel == "Estadual" ~ "({posicao_idhm}º lugar entre os 27 estados brasileiros)",
              filtros()$nivel == "Municipal" ~ "({posicao_idhm}º lugar entre os 5.570 municípios brasileiros)",
            )
            texto_comp <- dplyr::case_when(
              as.numeric(idhm) <= 0.499 ~ "Muito baixo {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto {glue::glue(texto_posicao)}",
              as.numeric(idhm) > 0.8 ~ "Muito alto {glue::glue(texto_posicao)}"
            )
          }
        } else {
          if (filtros()$nivel2 == "Municipal") {
            idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio2 & tabela_aux_municipios$uf == filtros()$estado_municipio2)])
            posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio2 & tabela_aux_municipios$uf == filtros()$estado_municipio2)]
          } else if (filtros()$nivel2 == "Estadual") {
            idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado2)]))
            posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado2)])
          } else if (filtros()$nivel2 == "Nacional") {
            idhm <- 0.727
          } else {
            idhm <- NaN
          }

          if (is.na(idhm)) {
            texto_comp <- "Classificação não aplicável"
          } else {
            texto_posicao <- dplyr::case_when(
              filtros()$nivel2 == "Nacional" ~ "",
              filtros()$nivel2 == "Estadual" ~ "({posicao_idhm}º lugar entre os 27 estados brasileiros)",
              filtros()$nivel2 == "Municipal" ~ "({posicao_idhm}º lugar entre os 5.570 municípios brasileiros)",
            )
            texto_comp <- dplyr::case_when(
              as.numeric(idhm) <= 0.499 ~ "Muito baixo {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto {glue::glue(texto_posicao)}",
              as.numeric(idhm) > 0.8 ~ "Muito alto {glue::glue(texto_posicao)}"
            )
          }
        }
      }

      cor_comp <- dplyr::case_when(
        is.na(idhm) ~ "lightgrey",
        as.numeric(idhm) <= 0.499 ~ "#d998a0",  #vermelho
        as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "#d8b382",  #laranja
        as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "#f1eb99",  #amarelo
        as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "#a2e4b8",  #verde
        as.numeric(idhm) > 0.8 ~ "#cbd6ff"  #azul
      )

      cria_caixa_server(
        dados = NULL,
        indicador = NULL,
        titulo = dplyr::if_else(filtros()$nivel == "Nacional", true = "IDH", false = "IDHM"),
        tem_meta = FALSE,
        valor_de_referencia = 0.727,
        valor_indicador = idhm,
        tipo = "taxa",
        texto_footer = glue::glue(texto_comp),
        cor = cor_comp,
        invertido = TRUE,
        tamanho_caixa = "303px",
        pagina = "bloco_1",
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

    output$caixa_b1_i2 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "media_populacao_feminina_10_a_49",
        titulo = "Média da população feminina entre 10 e 49 anos",
        tem_meta = FALSE,
        valor_de_referencia = data_brasil_resumo()$media_populacao_feminina_10_a_49,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        #cor = dplyr::if_else(filtros()$nivel == "Nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (a média nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% da média nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} mulheres"
        ),
        tamanho_caixa = "303px",
        pagina = "bloco_1",
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

    output$caixa_b1_i3 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "total_de_nascidos_vivos",
        titulo = "Nascidos vivos",
        tem_meta = FALSE,
        valor_de_referencia = data_brasil_resumo()$total_de_nascidos_vivos,
        tipo = "número",
        invertido = FALSE,
        cor = dplyr::if_else(filtros()$nivel == "Nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} nascidos vivos"
        ),
        tamanho_caixa = "303px",
        pagina = "bloco_1",
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

    titulo_faixa_etaria <- reactive({
      dplyr::case_when(
        input$faixa_et == "porc_nvm_menor_que_20_anos" ~ "Porcentagem de nascidos vivos de mães com idade inferior a 20 anos",
        input$faixa_et == "porc_nvm_entre_20_e_34_anos" ~ "Porcentagem de nascidos vivos de mães com idade entre 20 e 34 anos",
        input$faixa_et == "porc_nvm_maior_que_34_anos" ~ "Porcentagem de nascidos vivos de mães com idade maior que 34 anos"
      )
    })

    output$caixa_b1_i4 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = input$faixa_et,
        titulo = titulo_faixa_etaria(),
        tem_meta = FALSE,
        valor_de_referencia = data_brasil_resumo()[[input$faixa_et]],
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "303px",
        pagina = "bloco_1",
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

    titulo_racacor <- reactive({
      dplyr::case_when(
        input$raca == "porc_nvm_com_cor_da_pele_amarela" ~ "Porcentagem de nascidos vivos de mães de raça/cor amarela",
        input$raca == "porc_nvm_com_cor_da_pele_branca" ~ "Porcentagem de nascidos vivos de mães de raça/cor branca",
        input$raca == "porc_nvm_indigenas" ~ "Porcentagem de nascidos vivos de mães indígenas",
        input$raca == "porc_nvm_com_cor_da_pele_parda" ~ "Porcentagem de nascidos vivos de mães de raça/cor parda",
        input$raca == "porc_nvm_com_cor_da_pele_preta" ~ "Porcentagem de nascidos vivos de mães de raça/cor preta"
      )
    })

    output$caixa_b1_i5 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = input$raca,
        titulo = titulo_racacor(),
        tem_meta = FALSE,
        valor_de_referencia = data_brasil_resumo()[[input$raca]],
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "303px",
        pagina = "bloco_1",
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

    titulo_escmae <- reactive({
      dplyr::case_when(
        input$esc == "porc_nvm_com_escolaridade_ate_3" ~ "Porcentagem de nascidos vivos de mães com menos de 4 anos de estudo",
        input$esc == "porc_nvm_com_escolaridade_de_4_a_7" ~ "Porcentagem de nascidos vivos de mães com 4 a 7 anos de estudo",
        input$esc == "porc_nvm_com_escolaridade_de_8_a_11" ~ "Porcentagem de nascidos vivos de mães com 8 a 11 anos de estudo",
        input$esc == "porc_nvm_com_escolaridade_acima_de_11" ~ "Porcentagem de nascidos vivos de mães de com mais de 11 anos de estudo"
      )
    })

    output$caixa_b1_i6 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = input$esc,
        titulo = titulo_escmae(),
        tem_meta = FALSE,
        valor_de_referencia = data_brasil_resumo()[[input$esc]],
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "303px",
        pagina = "bloco_1",
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

    output$caixa_b1_i7 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_dependentes_sus",
        titulo = "Porcentagem de mulheres de 10 a 49 anos usuárias exclusivas do SUS",
        tem_meta = FALSE,
        valor_de_referencia = data_brasil_resumo()$porc_dependentes_sus,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_1",
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

    output$caixa_b1_i8 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_cobertura_esf",
        titulo = "Cobertura populacional com equipes de Saúde da Família",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "303px",
        pagina = "bloco_1",
        tipo_referencia = "meta ODS",
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
          racacor = round(sum(racacor_incompletos, na.rm = TRUE)/sum(racacor_totais, na.rm = TRUE) * 100, 2),
          escmae = round(sum(escmae_incompletos, na.rm = TRUE)/sum(escmae_totais, na.rm = TRUE) * 100, 2),
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
            filtros()$nivel == "Nacional" ~ "Brasil (valor de referência)",
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
        incompletude1 = data_incompletude()$racacor,
        variavel_incompletude1 = "RACACOR",
        descricao_incompletude1 = "ignorados ou em branco",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$racacor > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    })

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$escmae,
        variavel_incompletude1 = "ESCMAE",
        descricao_incompletude1 = "ignorados ou em branco",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$escmae > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    })


    data_referencia <- reactive({
      bloco1 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          populacao_feminina_10_a_49 = sum(populacao_feminina_10_a_49),
          media_populacao_feminina_10_a_49 = round(populacao_feminina_10_a_49/(filtros()$ano2[2] - filtros()$ano2[1] + 1)),
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = 95,
          porc_nvm_menor_que_20_anos = round(sum(nvm_menor_que_20_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })


    data1_idademae <- reactive(
      data1() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_et),
          class
        )
    )

    data1_idademae_comp <- reactive(
      data1_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_et),
          class
        )
    )

    data1_idademae_referencia <- reactive({
      data_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_et),
          class
        )
    })

    #gráfico porcentagem fertilidade <20 anos
    output$plot1 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1_idademae(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
              data = data1_idademae_referencia(),
              name = "Referência (média nacional)",
              type = "line",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1_idademae(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_idademae_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia"))  {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data1_idademae_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    data1_racacor <- reactive(
      data1() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$raca),
          class
        )
    )

    data1_racacor_comp <- reactive(
      data1_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$raca),
          class
        )
    )

    data1_racacor_referencia <- reactive({
      data_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$raca),
          class
        )
    })

    #gráfico porcentagem por raça
    output$plot2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1_racacor(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data1_racacor_referencia(),
            type = "line",
            name = "Referência (média nacional)",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1_racacor(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_racacor_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data1_racacor_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    data1_esc <- reactive(
      data1() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$esc),
          class
        )
    )

    data1_esc_comp <- reactive(
      data1_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$esc),
          class
        )
    )

    data1_esc_referencia <- reactive({
      data_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$esc),
          class
        )
    })

    ### plot porcentagem escolaridade
    output$plot3 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1_esc(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data1_esc_referencia(),
            type = "line",
            name = "Referência (média nacional)",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1_esc(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_esc_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data1_esc_referencia(),
            type = "line",
            name = "Referência (média nacional)",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
      # },
      # error = function(e) {}
      # )
    })

    #gráficos porcentagem cobetura SUS
    output$plot4 <- highcharter::renderHighchart({
      # tryCatch({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }

      }
      # },
      # error = function(e) {
      # })
    })

    #gráficos porcentagem cobetura SUS
    output$plot5 <- highcharter::renderHighchart({
      # tryCatch({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data_referencia(),
            type = "line",
            name = "Referência (meta ODS)",
            highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data1(),
            name = dplyr::if_else(filtros()$nivel == "Nacional", "Brasil", unique(data1()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_comp(),
            name = dplyr::if_else(filtros()$nivel2 == "Nacional", "Brasil", unique(data1_comp()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data_referencia(),
              type = "line",
              name = "Referência (meta ODS)",
              highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }

      }
      # },
      # error = function(e) {
      # })
    })

  })
}

## To be copied in the UI
# mod_bloco_1_ui("bloco_1_1")

## To be copied in the server
# mod_bloco_1_server("bloco_1_1")
