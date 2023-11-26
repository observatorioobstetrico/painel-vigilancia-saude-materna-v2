#' bloco_7 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_7_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Mortalidade fetal, perinatal e neonatal: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset1"),
      width = 12,
      collapsible = FALSE,
      tabPanel(
        HTML("<b>Indicadores relacionados à mortalidade fetal </b>"),
        value = "tabpanel_fetal",
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
                uiOutput(ns("input_localidade_resumo_fetal")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_fetal_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_fetal_i2")), proxy.height = "300px")
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
                    HTML("<b style='font-size:19px'> Número de óbitos fetais &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("parto_fetal"),
                        label = "Momento do óbito",
                        options = list(placeholder = "Selecione o momento do óbito"),
                        choices = c(
                          "Geral" = "fetal_parto_geral",
                          "Antes do trabalho de parto" = "antes",
                          "Durante o trabalho de parto" = "durante",
                          "Depois do trabaho de parto" = "depois"
                        ),
                        width = "100%"
                      ),
                      selectizeInput(
                        inputId = ns("faixa_peso_fetal"),
                        label = "Faixa de peso",
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "peso_fetal",
                          "Menor que 1500g" = "fetal_menos1500",
                          "De 1500g a 1999g" = "fetal_1500_1999",
                          "De 2000g a 2499g" = "fetal_2000_2499",
                          "Maior ou igual a 2500g" = "fetal_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_fetal"), height = 430))
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
                    HTML("<b style='font-size:19px'> Taxa de mortalidade fetal  &nbsp;</b>")
                  ),
                  hr(),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_fetal"), height = 430))
                )
              )
            )
          )
        )
      ),

      tabPanel(
        HTML("<b>Indicadores relacionados à mortalidade perinatal </b>"),
        value = "tabpanel_perinatal",
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
                uiOutput(ns("input_localidade_resumo_perinatal")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i2")), proxy.height = "300px")
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
                    HTML("<b style='font-size:19px'> Número de óbitos perinatais totais &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_perinatal_total"),
                        label = "Faixa de peso",
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "obitos_perinatal_total",
                          "Menor que 1500g" = "perinatal_total_menos1500",
                          "De 1500g a 1999g" = "perinatal_total_1500_1999",
                          "De 2000g a 2499g" = "perinatal_total_2000_2499",
                          "Maior ou igual a 2500g" = "perinatal_total_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_perinatal"), height = 430))
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
                    HTML("<b style='font-size:19px'> Número de óbitos perinatais segundo a OMS  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_perinatal_oms"),
                        label = "Faixa de peso",
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "obitos_perinatal_oms",
                          "Menor que 1500g" = "perinatal_oms_menos1500",
                          "De 1500g a 1999g" = "perinatal_oms_1500_1999",
                          "De 2000g a 2499g" = "perinatal_oms_2000_2499",
                          "Maior ou igual a 2500g" = "perinatal_oms_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_perinatal"), height = 430))
                )
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b>Indicadores relacionados à mortalidade neonatal</b>"),
        value = "tabpanel_neonatal",
        fluidRow(
          # column(
          #   width = 12,
          #   HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
          #   HTML(
          #     "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
          #       <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos maternos no país, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-grav-puerp/' target = _blank>OOBr Óbitos de Gestantes e Puérperas</a>.
          #       </b> </div>"
          #   ),
          #   hr(),
          #   HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
          # ),
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
            hr(),
            fluidRow(
              column(
                width = 12,
                HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                uiOutput(ns("input_localidade_resumo_neonat")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i2")), proxy.height = "300px")
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i3")), proxy.height = "325px")
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
                    HTML("<b style='font-size:19px'> Mortalidade neonatal por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso"),
                        label = "Faixa de peso ao nascer",
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat",
                          "Menor que 1500g" = "mort_neonat_menos1500",
                          "De 1500g a 1999g" = "mort_neonat_1500_1999",
                          "De 2000g a 2499g" = "mort_neonat_2000_2499",
                          "Maior ou igual a 2500g" = "mort_neonat_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_neonat"), height = 345))
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
                    HTML("<b style='font-size:19px'> Mortalidade neonatal precoce por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_precoc"),
                        label = "Faixa de peso ao nascer",
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat_precoc",
                          "Menor que 1500g" = "mort_neonat_precoc_menos1500",
                          "De 1500g a 1999g" = "mort_neonat_precoc_1500_1999",
                          "De 2000g a 2499g" = "mort_neonat_precoc_2000_2499",
                          "Maior ou igual a 2500g" = "mort_neonat_precoc_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_neonat"), height = 345))
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
                    HTML("<b style='font-size:19px'> Mortalidade neonatal tardia por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_tardia"),
                        label = "Faixa de peso ao nascer",
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat_tardia",
                          "Menor que 1500g" = "mort_neonat_tardia_menos1500",
                          "De 1500g a 1999g" = "mort_neonat_tardia_1500_1999",
                          "De 2000g a 2499g" = "mort_neonat_tardia_2000_2499",
                          "Maior ou igual a 2500g" = "mort_neonat_tardia_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_neonat"), height = 345))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' bloco_7 Server Functions
#'
#' @noRd
mod_bloco_7_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Códigos compartilhados para os dois blocos ------------------------------

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


    ##### Definindo as cores para os gráficos #####
    cols <- c("#2c115f", "#b73779", "#fc8961")


    ##### Dados do resumo do período para a localidade escolhida #####
    nivel_selecionado <- reactive({
      if (filtros()$comparar == "Não") {
        filtros()$nivel
      } else {
        if (input$tabset1 == "tabpanel_neonatal") {
          req(input$localidade_resumo_neonat)
          if (input$localidade_resumo_neonat == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        } else if (input$tabset1 == "tabpanel_fetal"){
          req(input$localidade_resumo_fetal)
          if (input$localidade_resumo_fetal == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        } else {
          req(input$localidade_resumo_perinatal)
          if (input$localidade_resumo_perinatal == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        } }
    })



    data7_resumo <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo_inputs <- ""
      } else {
        if (input$tabset1 == "tabpanel_neonatal") {
          req(input$localidade_resumo_neonat)
          if (input$localidade_resumo_neonat == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else if (input$tabset1 == "tabpanel_fetal") {
          req(input$localidade_resumo_fetal)
          if (input$localidade_resumo_fetal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else  {
          req(input$localidade_resumo_perinatal)
          if (input$localidade_resumo_perinatal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        }
      }
      bloco7 |>
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
        dplyr::summarise(
          mort_neonat = round(sum(obitos_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_precoc = round(sum(obitos_6dias)/sum(nascidos) *1000, 2),
          mort_neonat_tardia = round(sum(obitos_7_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_menos1500 = round(sum(obitos_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_precoc_menos1500 = round(sum(obitos_6dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_tardia_menos1500 = round(sum(obitos_7_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_1500_1999 = round(sum(obitos_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_precoc_1500_1999 = round(sum(obitos_6dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_tardia_1500_1999 = round(sum(obitos_7_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_2000_2499 = round(sum(obitos_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_precoc_2000_2499 = round(sum(obitos_6dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_tardia_2000_2499 = round(sum(obitos_7_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_mais2500 = round(sum(obitos_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_precoc_mais2500 = round(sum(obitos_6dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_tardia_mais2500 = round(sum(obitos_7_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          obitos_fetais = sum(obitos_fetais_mais_22sem),
          fetal_peso_menos_1500 = sum(fetal_peso_menos_1500),
          fetal_peso_1500_1999 = sum(fetal_peso_1500_1999),
          fetal_peso_2000_2499 = sum(fetal_peso_2000_2499),
          fetal_peso_mais_2500 = sum(fetal_peso_mais_2500),
          fetal_antes = sum(fetal_antes),
          fetal_durante = sum(fetal_durante),
          fetal_depois = sum(fetal_depois),
          fetal_antes_peso_menos_1500 = sum(fetal_antes_peso_menos_1500),
          fetal_antes_peso_1500_1999 = sum(fetal_antes_peso_1500_1999),
          fetal_antes_peso_2000_2499 = sum(fetal_antes_peso_2000_2499),
          fetal_antes_peso_mais_2500 = sum(fetal_antes_peso_mais_2500),
          fetal_durante_peso_menos_1500 = sum(fetal_durante_peso_menos_1500),
          fetal_durante_peso_1500_1999 = sum(fetal_durante_peso_1500_1999),
          fetal_durante_peso_2000_2499 = sum(fetal_durante_peso_2000_2499),
          fetal_durante_peso_mais_2500 = sum(fetal_durante_peso_mais_2500),
          fetal_depois_peso_menos_1500 = sum(fetal_depois_peso_menos_1500),
          fetal_depois_peso_1500_1999 = sum(fetal_depois_peso_1500_1999),
          fetal_depois_peso_2000_2499 = sum(fetal_depois_peso_2000_2499),
          fetal_depois_peso_mais_2500 = sum(fetal_depois_peso_mais_2500),
          taxa_mort_fetal = round(sum(obitos_fetais_mais_22sem)/(sum(nascidos)+sum(obitos_fetais_mais_22sem)) *1000, 2),
          obitos_perinatal_total = sum(obitos_fetais_mais_22sem) + sum(obitos_6dias),
          perinatal_total_menos1500 = sum(fetal_peso_menos_1500) + sum(obitos_6dias_menos1500),
          perinatal_total_1500_1999 = sum(fetal_peso_1500_1999) + sum(obitos_6dias_1500_1999),
          perinatal_total_2000_2499 = sum(fetal_peso_2000_2499) + sum(obitos_6dias_2000_2499),
          perinatal_total_mais2500 = sum(fetal_peso_mais_2500) + sum(obitos_6dias_mais2500),
          obitos_perinatal_oms = sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias),
          perinatal_oms_menos1500 = sum(peso_menos_1500_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1500),
          perinatal_oms_1500_1999 = sum(peso_1500_1999_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_1999),
          perinatal_oms_2000_2499 = sum(peso_2000_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_2000_2499),
          perinatal_oms_mais2500 = sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500),
          localidade = dplyr::case_when(
            nivel_selecionado() == "Nacional" ~ "Brasil",
            nivel_selecionado() == "Regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
            nivel_selecionado() == "Estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
            nivel_selecionado() == "Macrorregião de saúde" ~ filtros()[[paste0("macro", sufixo_inputs)]],
            nivel_selecionado() == "Microrregião de saúde" ~ filtros()[[paste0("micro", sufixo_inputs)]],
            nivel_selecionado() == "Municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]]
          )
        ) |>
        dplyr::ungroup()
    })


    ##### Calculando os valores de referência para as caixinhas #####
    data7_resumo_referencia <- reactive({
      bloco7 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::summarise(
          mort_neonat = round(sum(obitos_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_precoc = round(sum(obitos_6dias)/sum(nascidos) *1000, 2),
          mort_neonat_tardia = round(sum(obitos_7_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_menos1500 = round(sum(obitos_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_precoc_menos1500 = round(sum(obitos_6dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_tardia_menos1500 = round(sum(obitos_7_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_1500_1999 = round(sum(obitos_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_precoc_1500_1999 = round(sum(obitos_6dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_tardia_1500_1999 = round(sum(obitos_7_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_2000_2499 = round(sum(obitos_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_precoc_2000_2499 = round(sum(obitos_6dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_tardia_2000_2499 = round(sum(obitos_7_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_mais2500 = round(sum(obitos_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_precoc_mais2500 = round(sum(obitos_6dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_tardia_mais2500 = round(sum(obitos_7_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          obitos_fetais = sum(obitos_fetais_mais_22sem),
          fetal_peso_menos_1500 = sum(fetal_peso_menos_1500),
          fetal_peso_1500_1999 = sum(fetal_peso_1500_1999),
          fetal_peso_2000_2499 = sum(fetal_peso_2000_2499),
          fetal_peso_mais_2500 = sum(fetal_peso_mais_2500),
          fetal_antes = sum(fetal_antes),
          fetal_durante = sum(fetal_durante),
          fetal_depois = sum(fetal_depois),
          fetal_antes_peso_menos_1500 = sum(fetal_antes_peso_menos_1500),
          fetal_antes_peso_1500_1999 = sum(fetal_antes_peso_1500_1999),
          fetal_antes_peso_2000_2499 = sum(fetal_antes_peso_2000_2499),
          fetal_antes_peso_mais_2500 = sum(fetal_antes_peso_mais_2500),
          fetal_durante_peso_menos_1500 = sum(fetal_durante_peso_menos_1500),
          fetal_durante_peso_1500_1999 = sum(fetal_durante_peso_1500_1999),
          fetal_durante_peso_2000_2499 = sum(fetal_durante_peso_2000_2499),
          fetal_durante_peso_mais_2500 = sum(fetal_durante_peso_mais_2500),
          fetal_depois_peso_menos_1500 = sum(fetal_depois_peso_menos_1500),
          fetal_depois_peso_1500_1999 = sum(fetal_depois_peso_1500_1999),
          fetal_depois_peso_2000_2499 = sum(fetal_depois_peso_2000_2499),
          fetal_depois_peso_mais_2500 = sum(fetal_depois_peso_mais_2500),
          taxa_mort_fetal = round(sum(obitos_fetais_mais_22sem)/(sum(nascidos)+sum(obitos_fetais_mais_22sem)) *1000, 2),
          obitos_perinatal_total = sum(obitos_fetais_mais_22sem) + sum(obitos_6dias),
          perinatal_total_menos1500 = sum(fetal_peso_menos_1500) + sum(obitos_6dias_menos1500),
          perinatal_total_1500_1999 = sum(fetal_peso_1500_1999) + sum(obitos_6dias_1500_1999),
          perinatal_total_2000_2499 = sum(fetal_peso_2000_2499) + sum(obitos_6dias_2000_2499),
          perinatal_total_mais2500 = sum(fetal_peso_mais_2500) + sum(obitos_6dias_mais2500),
          obitos_perinatal_oms = sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias),
          perinatal_oms_menos1500 = sum(peso_menos_1500_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1500),
          perinatal_oms_1500_1999 = sum(peso_1500_1999_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_1999),
          perinatal_oms_2000_2499 = sum(peso_2000_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_2000_2499),
          perinatal_oms_mais2500 = sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500)
        ) |>
        dplyr::ungroup()
    })


    ##### Calculando todos os indicadores para a localidade escolhida #####
    data7 <- reactive({
      bloco7 |>
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
          mort_neonat = round(sum(obitos_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_precoc = round(sum(obitos_6dias)/sum(nascidos) *1000, 2),
          mort_neonat_tardia = round(sum(obitos_7_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_menos1500 = round(sum(obitos_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_precoc_menos1500 = round(sum(obitos_6dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_tardia_menos1500 = round(sum(obitos_7_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_1500_1999 = round(sum(obitos_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_precoc_1500_1999 = round(sum(obitos_6dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_tardia_1500_1999 = round(sum(obitos_7_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_2000_2499 = round(sum(obitos_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_precoc_2000_2499 = round(sum(obitos_6dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_tardia_2000_2499 = round(sum(obitos_7_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_mais2500 = round(sum(obitos_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_precoc_mais2500 = round(sum(obitos_6dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_tardia_mais2500 = round(sum(obitos_7_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          obitos_fetais = sum(obitos_fetais_mais_22sem),
          fetal_peso_menos_1500 = sum(fetal_peso_menos_1500),
          fetal_peso_1500_1999 = sum(fetal_peso_1500_1999),
          fetal_peso_2000_2499 = sum(fetal_peso_2000_2499),
          fetal_peso_mais_2500 = sum(fetal_peso_mais_2500),
          fetal_antes = sum(fetal_antes),
          fetal_durante = sum(fetal_durante),
          fetal_depois = sum(fetal_depois),
          fetal_antes_peso_menos_1500 = sum(fetal_antes_peso_menos_1500),
          fetal_antes_peso_1500_1999 = sum(fetal_antes_peso_1500_1999),
          fetal_antes_peso_2000_2499 = sum(fetal_antes_peso_2000_2499),
          fetal_antes_peso_mais_2500 = sum(fetal_antes_peso_mais_2500),
          fetal_durante_peso_menos_1500 = sum(fetal_durante_peso_menos_1500),
          fetal_durante_peso_1500_1999 = sum(fetal_durante_peso_1500_1999),
          fetal_durante_peso_2000_2499 = sum(fetal_durante_peso_2000_2499),
          fetal_durante_peso_mais_2500 = sum(fetal_durante_peso_mais_2500),
          fetal_depois_peso_menos_1500 = sum(fetal_depois_peso_menos_1500),
          fetal_depois_peso_1500_1999 = sum(fetal_depois_peso_1500_1999),
          fetal_depois_peso_2000_2499 = sum(fetal_depois_peso_2000_2499),
          fetal_depois_peso_mais_2500 = sum(fetal_depois_peso_mais_2500),
          taxa_mort_fetal = round(sum(obitos_fetais_mais_22sem)/(sum(nascidos)+sum(obitos_fetais_mais_22sem)) *1000, 2),
          obitos_perinatal_total = sum(obitos_fetais_mais_22sem) + sum(obitos_6dias),
          perinatal_total_menos1500 = sum(fetal_peso_menos_1500) + sum(obitos_6dias_menos1500),
          perinatal_total_1500_1999 = sum(fetal_peso_1500_1999) + sum(obitos_6dias_1500_1999),
          perinatal_total_2000_2499 = sum(fetal_peso_2000_2499) + sum(obitos_6dias_2000_2499),
          perinatal_total_mais2500 = sum(fetal_peso_mais_2500) + sum(obitos_6dias_mais2500),
          obitos_perinatal_oms = sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias),
          perinatal_oms_menos1500 = sum(peso_menos_1500_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1500),
          perinatal_oms_1500_1999 = sum(peso_1500_1999_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_1999),
          perinatal_oms_2000_2499 = sum(peso_2000_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_2000_2499),
          perinatal_oms_mais2500 = sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500),
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


    ##### Calculando todos os indicadores para a localidade de comparação #####
    data7_comp <- reactive({
      bloco7 |>
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
          mort_neonat = round(sum(obitos_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_precoc = round(sum(obitos_6dias)/sum(nascidos) *1000, 2),
          mort_neonat_tardia = round(sum(obitos_7_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_menos1500 = round(sum(obitos_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_precoc_menos1500 = round(sum(obitos_6dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_tardia_menos1500 = round(sum(obitos_7_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_1500_1999 = round(sum(obitos_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_precoc_1500_1999 = round(sum(obitos_6dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_tardia_1500_1999 = round(sum(obitos_7_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_2000_2499 = round(sum(obitos_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_precoc_2000_2499 = round(sum(obitos_6dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_tardia_2000_2499 = round(sum(obitos_7_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_mais2500 = round(sum(obitos_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_precoc_mais2500 = round(sum(obitos_6dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_tardia_mais2500 = round(sum(obitos_7_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          obitos_fetais = sum(obitos_fetais_mais_22sem),
          fetal_peso_menos_1500 = sum(fetal_peso_menos_1500),
          fetal_peso_1500_1999 = sum(fetal_peso_1500_1999),
          fetal_peso_2000_2499 = sum(fetal_peso_2000_2499),
          fetal_peso_mais_2500 = sum(fetal_peso_mais_2500),
          fetal_antes = sum(fetal_antes),
          fetal_durante = sum(fetal_durante),
          fetal_depois = sum(fetal_depois),
          fetal_antes_peso_menos_1500 = sum(fetal_antes_peso_menos_1500),
          fetal_antes_peso_1500_1999 = sum(fetal_antes_peso_1500_1999),
          fetal_antes_peso_2000_2499 = sum(fetal_antes_peso_2000_2499),
          fetal_antes_peso_mais_2500 = sum(fetal_antes_peso_mais_2500),
          fetal_durante_peso_menos_1500 = sum(fetal_durante_peso_menos_1500),
          fetal_durante_peso_1500_1999 = sum(fetal_durante_peso_1500_1999),
          fetal_durante_peso_2000_2499 = sum(fetal_durante_peso_2000_2499),
          fetal_durante_peso_mais_2500 = sum(fetal_durante_peso_mais_2500),
          fetal_depois_peso_menos_1500 = sum(fetal_depois_peso_menos_1500),
          fetal_depois_peso_1500_1999 = sum(fetal_depois_peso_1500_1999),
          fetal_depois_peso_2000_2499 = sum(fetal_depois_peso_2000_2499),
          fetal_depois_peso_mais_2500 = sum(fetal_depois_peso_mais_2500),
          taxa_mort_fetal = round(sum(obitos_fetais_mais_22sem)/(sum(nascidos)+sum(obitos_fetais_mais_22sem)) *1000, 2),
          obitos_perinatal_total = sum(obitos_fetais_mais_22sem) + sum(obitos_6dias),
          perinatal_total_menos1500 = sum(fetal_peso_menos_1500) + sum(obitos_6dias_menos1500),
          perinatal_total_1500_1999 = sum(fetal_peso_1500_1999) + sum(obitos_6dias_1500_1999),
          perinatal_total_2000_2499 = sum(fetal_peso_2000_2499) + sum(obitos_6dias_2000_2499),
          perinatal_total_mais2500 = sum(fetal_peso_mais_2500) + sum(obitos_6dias_mais2500),
          obitos_perinatal_oms = sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias),
          perinatal_oms_menos1500 = sum(peso_menos_1500_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1500),
          perinatal_oms_1500_1999 = sum(peso_1500_1999_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_1999),
          perinatal_oms_2000_2499 = sum(peso_2000_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_2000_2499),
          perinatal_oms_mais2500 = sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500),
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


    ##### Calculando todos os valores de referência para os gráficos #####
    data7_referencia <- reactive({
      bloco7 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          mort_neonat = round(sum(obitos_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_precoc = round(sum(obitos_6dias)/sum(nascidos) *1000, 2),
          mort_neonat_tardia = round(sum(obitos_7_27dias)/sum(nascidos) *1000, 2),
          mort_neonat_menos1500 = round(sum(obitos_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_precoc_menos1500 = round(sum(obitos_6dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_tardia_menos1500 = round(sum(obitos_7_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2),
          mort_neonat_1500_1999 = round(sum(obitos_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_precoc_1500_1999 = round(sum(obitos_6dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_tardia_1500_1999 = round(sum(obitos_7_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2),
          mort_neonat_2000_2499 = round(sum(obitos_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_precoc_2000_2499 = round(sum(obitos_6dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_tardia_2000_2499 = round(sum(obitos_7_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2),
          mort_neonat_mais2500 = round(sum(obitos_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_precoc_mais2500 = round(sum(obitos_6dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          mort_neonat_tardia_mais2500 = round(sum(obitos_7_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2),
          obitos_fetais = sum(obitos_fetais_mais_22sem),
          fetal_peso_menos_1500 = sum(fetal_peso_menos_1500),
          fetal_peso_1500_1999 = sum(fetal_peso_1500_1999),
          fetal_peso_2000_2499 = sum(fetal_peso_2000_2499),
          fetal_peso_mais_2500 = sum(fetal_peso_mais_2500),
          fetal_antes = sum(fetal_antes),
          fetal_durante = sum(fetal_durante),
          fetal_depois = sum(fetal_depois),
          fetal_antes_peso_menos_1500 = sum(fetal_antes_peso_menos_1500),
          fetal_antes_peso_1500_1999 = sum(fetal_antes_peso_1500_1999),
          fetal_antes_peso_2000_2499 = sum(fetal_antes_peso_2000_2499),
          fetal_antes_peso_mais_2500 = sum(fetal_antes_peso_mais_2500),
          fetal_durante_peso_menos_1500 = sum(fetal_durante_peso_menos_1500),
          fetal_durante_peso_1500_1999 = sum(fetal_durante_peso_1500_1999),
          fetal_durante_peso_2000_2499 = sum(fetal_durante_peso_2000_2499),
          fetal_durante_peso_mais_2500 = sum(fetal_durante_peso_mais_2500),
          fetal_depois_peso_menos_1500 = sum(fetal_depois_peso_menos_1500),
          fetal_depois_peso_1500_1999 = sum(fetal_depois_peso_1500_1999),
          fetal_depois_peso_2000_2499 = sum(fetal_depois_peso_2000_2499),
          fetal_depois_peso_mais_2500 = sum(fetal_depois_peso_mais_2500),
          taxa_mort_fetal = round(sum(obitos_fetais_mais_22sem)/(sum(nascidos)+sum(obitos_fetais_mais_22sem)) *1000, 2),
          obitos_perinatal_total = sum(obitos_fetais_mais_22sem) + sum(obitos_6dias),
          perinatal_total_menos1500 = sum(fetal_peso_menos_1500) + sum(obitos_6dias_menos1500),
          perinatal_total_1500_1999 = sum(fetal_peso_1500_1999) + sum(obitos_6dias_1500_1999),
          perinatal_total_2000_2499 = sum(fetal_peso_2000_2499) + sum(obitos_6dias_2000_2499),
          perinatal_total_mais2500 = sum(fetal_peso_mais_2500) + sum(obitos_6dias_mais2500),
          obitos_perinatal_oms = sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias),
          perinatal_oms_menos1500 = sum(peso_menos_1500_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1500),
          perinatal_oms_1500_1999 = sum(peso_1500_1999_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_1999),
          perinatal_oms_2000_2499 = sum(peso_2000_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_2000_2499),
          perinatal_oms_mais2500 = sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })


    ##### Calculando os indicadores de incompletude e cobertura para todos os indicadores #####
    # data_incompletude_aux <- reactive({
    #   base_incompletude |>
    #     dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
    #     dplyr::filter(
    #       if (filtros()$nivel == "Nacional")
    #         ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #       else if (filtros()$nivel == "Regional")
    #         regiao == filtros()$regiao
    #       else if (filtros()$nivel == "Estadual")
    #         uf == filtros()$estado
    #       else if (filtros()$nivel == "Macrorregião de saúde")
    #         macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
    #       else if(filtros()$nivel == "Microrregião de saúde")
    #         r_saude == filtros()$micro & uf == filtros()$estado_micro
    #       else if(filtros()$nivel == "Municipal")
    #         municipio == filtros()$municipio & uf == filtros()$estado_municipio
    #     ) |>
    #     dplyr::group_by(ano) |>
    #     dplyr::summarise(
    #       prop_mif_investigado = round((sum(obito_mif_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_mif_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_mulher_idade_fertil, na.rm = TRUE) * 100, 2),
    #       prop_obito_materno_investigado = round((sum(obito_materno_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_materno_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_maternos, na.rm = TRUE) * 100, 2),
    #       localidade = dplyr::case_when(
    #         filtros()$nivel == "Nacional" ~ "Brasil",
    #         filtros()$nivel == "Regional" ~ filtros()$regiao,
    #         filtros()$nivel == "Estadual" ~ filtros()$estado,
    #         filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
    #         filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
    #         filtros()$nivel == "Municipal" ~ filtros()$municipio
    #       )
    #     ) |>
    #     dplyr::ungroup()
    # })

    # data_cobertura <- reactive({
    #   if (filtros()$nivel == "Municipal") {
    #     sub_registro_sim_muni_2015_2020 |>
    #       dplyr::filter(
    #         ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
    #         municipio == filtros()$municipio,
    #         uf == filtros()$estado_municipio
    #       ) |>
    #       dplyr::rename(
    #         localidade = municipio
    #       )
    #   } else if (filtros()$nivel == "Estadual") {
    #     sub_registro_sim_uf_regioes_2015_2020 |>
    #       dplyr::filter(
    #         ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
    #         localidade == filtros()$estado
    #       )
    #   } else if (filtros()$nivel == "Regional") {
    #     sub_registro_sim_uf_regioes_2015_2020 |>
    #       dplyr::filter(
    #         ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
    #         localidade == filtros()$regiao
    #       )
    #   } else if (filtros()$nivel == "Nacional") {
    #     sub_registro_sim_uf_regioes_2015_2020 |>
    #       dplyr::filter(
    #         ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
    #         localidade == "Brasil"
    #       )
    #   } else {
    #     data.frame(
    #       ano = filtros()$ano2[1]:filtros()$ano2[2],
    #       localidade = dplyr::case_when(
    #         filtros()$nivel == "Nacional" ~ "Brasil",
    #         filtros()$nivel == "Regional" ~ filtros()$regiao,
    #         filtros()$nivel == "Estadual" ~ filtros()$estado,
    #         filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
    #         filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
    #         filtros()$nivel == "Municipal" ~ filtros()$municipio
    #       ),
    #       cobertura = 100
    #     )
    #   }
    # })
    #
    # data_incompletude <- reactive({dplyr::full_join(data_incompletude_aux(), data_cobertura(), by = c("ano", "localidade"))})


    ##### Definindo os modais de incompletude e quando os botões de aviso devem aparecer #####
    # observeEvent(input$botao1, {
    #   cria_modal_incompletude(
    #     incompletude1 = data_incompletude()$prop_mif_investigado,
    #     incompletude2 = data_incompletude()$prop_obito_materno_investigado,
    #     df = data_incompletude(),
    #     cobertura = data_incompletude()$cobertura,
    #     bloco = "bloco6"
    #   )
    # })
    #
    # observeEvent(filtros()$pesquisar, {
    #   shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    #   req(any(data_incompletude()$prop_mif_investigado < 90, na.rm = TRUE) | any(data_incompletude()$prop_obito_materno_investigado < 100, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
    #   shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    # },
    # ignoreNULL = FALSE
    # )
    #
    # observeEvent(input$botao2, {
    #   cria_modal_incompletude(
    #     incompletude1 = data_incompletude()$prop_mif_investigado,
    #     incompletude2 = data_incompletude()$prop_obito_materno_investigado,
    #     df = data_incompletude(),
    #     cobertura = data_incompletude()$cobertura,
    #     bloco = "bloco6"
    #   )
    # })
    #
    # observeEvent(filtros()$pesquisar, {
    #   shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    #   req(any(data_incompletude()$prop_mif_investigado < 90, na.rm = TRUE) | any(data_incompletude()$prop_obito_materno_investigado < 100, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
    #   shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    # },
    # ignoreNULL = FALSE
    # )
    #
    # observeEvent(input$botao3, {
    #   cria_modal_incompletude(
    #     incompletude1 = data_incompletude()$prop_mif_investigado,
    #     incompletude2 = data_incompletude()$prop_obito_materno_investigado,
    #     df = data_incompletude(),
    #     cobertura = data_incompletude()$cobertura,
    #     bloco = "bloco6"
    #   )
    # })
    #
    # observeEvent(filtros()$pesquisar, {
    #   shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    #   req((sum(data6()$obitos_mat_totais) != 0) & (any(data_incompletude()$prop_mif_investigado < 90, na.rm = TRUE) | any(data_incompletude()$prop_obito_materno_investigado < 100, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE)))
    #   shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    # },
    # ignoreNULL = FALSE
    # )
    #
    # observeEvent(input$botao4, {
    #   cria_modal_incompletude(
    #     incompletude1 = data_incompletude()$prop_mif_investigado,
    #     incompletude2 = data_incompletude()$prop_obito_materno_investigado,
    #     df = data_incompletude(),
    #     cobertura = data_incompletude()$cobertura,
    #     bloco = "bloco6"
    #   )
    # })
    #
    # observeEvent(filtros()$pesquisar, {
    #   shinyjs::hide(id = "mostrar_botao4", anim = TRUE, animType = "fade", time = 0.8)
    #   req((sum(data6()$obitos_mat_totais) != 0) & (any(data_incompletude()$prop_mif_investigado < 90, na.rm = TRUE) | any(data_incompletude()$prop_obito_materno_investigado < 100, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE)))
    #   shinyjs::show(id = "mostrar_botao4", anim = TRUE, animType = "fade", time = 0.8)
    # },
    # ignoreNULL = FALSE
    # )



    # Para os indicadores de mortalidade neonatal ------------------------------

    ##### Criando o input para selecionar a localidade do resumo quando há comparação #####
    output$input_localidade_resumo_neonat <- renderUI({
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
          inputId = ns("localidade_resumo_neonat"),
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


    ##### Calculando a RMM corrigida para o resumo do período #####
    # data7_resumo_fator_de_correcao <- reactive({
    #   if (filtros()$comparar == "Não") {
    #     sufixo_inputs <- ""
    #   } else {
    #     if (input$tabset1 == "tabpanel_mortalidade") {
    #       req(input$localidade_resumo_mort)
    #       if (input$localidade_resumo_mort == "escolha1") {
    #         sufixo_inputs <- ""
    #       } else {
    #         sufixo_inputs <- "2"
    #       }
    #     } else {
    #       req(input$localidade_resumo_morb)
    #       if (input$localidade_resumo_morb == "escolha1") {
    #         sufixo_inputs <- ""
    #       } else {
    #         sufixo_inputs <- "2"
    #       }
    #     }
    #   }
    #   if (nivel_selecionado() %in% c("Estadual", "Regional", "Nacional")) {
    #     if (nivel_selecionado() == "Estadual") {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == filtros()[[paste0("estado", sufixo_inputs)]],
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         ) |>
    #         dplyr::group_by(localidade) |>
    #         dplyr::summarise(
    #           fator_de_correcao = mean(fator_de_correcao)
    #         )
    #     } else if (nivel_selecionado() == "Regional") {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == filtros()[[paste0("regiao", sufixo_inputs)]],
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         ) |>
    #         dplyr::group_by(localidade) |>
    #         dplyr::summarise(
    #           fator_de_correcao = mean(fator_de_correcao)
    #         )
    #     } else {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == "Brasil",
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         ) |>
    #         dplyr::group_by(localidade) |>
    #         dplyr::summarise(
    #           fator_de_correcao = mean(fator_de_correcao)
    #         )
    #     }
    #   } else {
    #     data.frame(
    #       fator_de_correcao = 1,
    #       localidade = dplyr::case_when(
    #         nivel_selecionado() == "Nacional" ~ "Brasil",
    #         nivel_selecionado() == "Regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
    #         nivel_selecionado() == "Estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
    #         nivel_selecionado() == "Macrorregião de saúde" ~ filtros()[[paste0("macro", sufixo_inputs)]],
    #         nivel_selecionado() == "Microrregião de saúde" ~ filtros()[[paste0("micro", sufixo_inputs)]],
    #         nivel_selecionado() == "Municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]]
    #       )
    #     )
    #   }
    #
    # })
    #
    # data6_resumo_rmm_corrigida <- reactive({
    #   dplyr::full_join(data6_resumo(), data6_resumo_fator_de_correcao(), by = "localidade") |>
    #     dplyr::mutate(
    #       rmm = round(rmm*fator_de_correcao, 1)
    #     )
    # })
    #
    #
    # ##### Calculando a RMM corrigida para os gráficos #####
    # data6_fator_de_correcao <- reactive({
    #   if (filtros()$nivel %in% c("Estadual", "Regional", "Nacional")) {
    #     if (filtros()$nivel == "Estadual") {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == filtros()$estado,
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     } else if (filtros()$nivel == "Regional") {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == filtros()$regiao,
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     } else {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == "Brasil",
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     }
    #   } else {
    #     data.frame(
    #       ano = filtros()$ano2[1]:filtros()$ano2[2],
    #       fator_de_correcao = rep(1, length(filtros()$ano2[1]:filtros()$ano2[2]))
    #     )
    #   }
    # })
    #
    # data6_fator_de_correcao_comp <- reactive({
    #   if (filtros()$nivel2 %in% c("Estadual", "Regional", "Nacional")) {
    #     if (filtros()$nivel2 == "Estadual") {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == filtros()$estado,
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     } else if (filtros()$nivel2 == "Regional") {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == filtros()$regiao,
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     } else {
    #       rmm_fator_de_correcao |>
    #         dplyr::filter(
    #           localidade == "Brasil",
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     }
    #   } else {
    #     data.frame(
    #       ano = filtros()$ano2[1]:filtros()$ano2[2],
    #       fator_de_correcao = rep(1, length(filtros()$ano2[1]:filtros()$ano2[2]))
    #     )
    #   }
    # })
    #
    # data6_rmm_corrigida <- reactive({
    #   dplyr::full_join(data6(), data6_fator_de_correcao(), by = "ano") |>
    #     dplyr::mutate(
    #       rmm = round(rmm*fator_de_correcao, 1)
    #     )
    # })
    #
    # data6_comp_rmm_corrigida <- reactive({
    #   dplyr::full_join(data6_comp(), data6_fator_de_correcao_comp(), by = "ano") |>
    #     dplyr::mutate(
    #       rmm = round(rmm*fator_de_correcao, 1)
    #     )
    # })


    ##### Criando as caixinhas do resumo do período #####

    titulo_caixa_neonat <- reactive({
      dplyr::case_when(
        input$faixa_peso == "mort_neonat" ~ "Mortalidade neonatal por 1000 nascidos vivos",
        input$faixa_peso == "mort_neonat_menos1500" ~ "Mortalidade neonatatl por 1000 nascidos vivos para peso ao nascer menor que 1500g",
        input$faixa_peso == "mort_neonat_1500_1999" ~ "Mortalidade neonatal por 1000 nascidos vivos para peso ao nascer de 1500 a 1999g",
        input$faixa_peso == "mort_neonat_2000_2499" ~ "Mortalidade neonatal por 1000 nascidos vivos para peso ao nascer de 2000 a 2499g",
        input$faixa_peso == "mort_neonat_mais2500" ~ "Mortalidade neonatal por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500g"
      )
    })

    output$caixa_b7_neonat_i1 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso,
        titulo = titulo_caixa_neonat(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso]],
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    titulo_caixa_neonat_precoc <- reactive({
      dplyr::case_when(
        input$faixa_peso_precoc == "mort_neonat_precoc" ~ "Mortalidade neonatal precoce por 1000 nascidos vivos",
        input$faixa_peso_precoc == "mort_neonat_precoc_menos1500" ~ "Mortalidade neonatal precoce por 1000 nascidos vivos para peso ao nascer menor que 1500g",
        input$faixa_peso_precoc == "mort_neonat_precoc_1500_1999" ~ "Mortalidade neonatal precoce por 1000 nascidos vivos para peso ao nascer de 1500 a 1999g",
        input$faixa_peso_precoc == "mort_neonat_precoc_2000_2499" ~ "Mortalidade neonatal precoce por 1000 nascidos vivos para peso ao nascer de 2000 a 2499g",
        input$faixa_peso_precoc == "mort_neonat_precoc_mais2500" ~ "Mortalidade neonatal precoce por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500g"
      )
    })

    output$caixa_b7_neonat_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_precoc,
        titulo = titulo_caixa_neonat_precoc(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_precoc]],
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    titulo_caixa_neonat_tardia <- reactive({
      dplyr::case_when(
        input$faixa_peso_tardia == "mort_neonat_tardia" ~ "Mortalidade neonatal tardia por 1000 nascidos vivos",
        input$faixa_peso_tardia == "mort_neonat_tardia_menos1500" ~ "Mortalidade neonatal tardia por 1000 nascidos vivos para peso ao nascer menor que 1500g",
        input$faixa_peso_tardia == "mort_neonat_tardia_1500_1999" ~ "Mortalidade neonatal tardia por 1000 nascidos vivos para peso ao nascer de 1500 a 1999g",
        input$faixa_peso_tardia == "mort_neonat_tardia_2000_2499" ~ "Mortalidade neonatal tardia por 1000 nascidos vivos para peso ao nascer de 2000 a 2499g",
        input$faixa_peso_tardia == "mort_neonat_tardia_mais2500" ~ "Mortalidade neonatal tardia por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500g"
      )
    })

    output$caixa_b7_neonat_i3 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_tardia,
        titulo = titulo_caixa_neonat_tardia(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_tardia]],
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    # output$caixa_b7_neonat_i4 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_menos1500",
    #     titulo = "Mortalidade neonatal para peso ao nascer menor que 1500g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_menos1500,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i5 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_precoc_menos1500",
    #     titulo = "Mortalidade neonatal precoce para peso ao nascer menor que 1500g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_precoc_menos1500,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i6 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_tardia_menos1500",
    #     titulo = "Mortalidade neonatal tardia para peso ao nascer menor que 1500g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_tardia_menos1500,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i7 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_1500_1999",
    #     titulo = "Mortalidade neonatal para peso ao nascer de 1500g a 1999g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_1500_1999,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i8 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_precoc_1500_1999",
    #     titulo = "Mortalidade neonatal precoce para peso ao nascer de 1500g a 1999g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_precoc_1500_1999,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i9 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_tardia_1500_1999",
    #     titulo = "Mortalidade neonatal tardia para peso ao nascer de 1500g a 1999g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_tardia_1500_1999,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i10 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_2000_2499",
    #     titulo = "Mortalidade neonatal para peso ao nascer de 2000g a 2499g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_2000_2499,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i11 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_precoc_2000_2499",
    #     titulo = "Mortalidade neonatal precoce para peso ao nascer de 2000g a 2499g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_precoc_2000_2499,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i12 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_tardia_2000_2499",
    #     titulo = "Mortalidade neonatal tardia para peso ao nascer de 2000g a 2499g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_tardia_2000_2499,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i13 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_mais2500",
    #     titulo = "Mortalidade neonatal para peso ao nascer maior ou igual a 2500g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_mais2500,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i14 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_precoc_mais2500",
    #     titulo = "Mortalidade neonatal precoce para peso ao nascer maior ou igual a 2500g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_precoc_mais2500,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })
    #
    # output$caixa_b7_neonat_i15 <- renderUI({
    #   cria_caixa_server(
    #     dados = data7_resumo(),
    #     indicador = "mort_neonat_tardia_mais2500",
    #     titulo = "Mortalidade neonatal tardia para peso ao nascer maior ou igual a 2500g",
    #     tem_meta = FALSE,
    #     valor_de_referencia = data7_resumo_referencia()$mort_neonat_tardia_mais2500,
    #     tipo = "taxa",
    #     invertido = FALSE,
    #     tamanho_caixa = "303px",
    #     pagina = "bloco_7",
    #     nivel_de_analise = nivel_selecionado()
    #   )
    # })


    ##### Criando o gráfico de linhas para a mortalidade neonatal, incluido faixas de peso #####

    output$plot1_neonat <- highcharter::renderHighchart({
      data7_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso),
          class
        )

      data7_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso),
          class
        )

      data7_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso),
          class
        )

      if (filtros()$comparar == "Não") {
        # validate(
        #   need(
        #     sum(data7()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        # validate(
        #   need(
        #     sum(data6()$obitos_mat_totais) != 0 | sum(data6_comp()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ##### Criando o gráfico de linhas para a mortalidade neonatal precoce, incluido faixas de peso #####

    output$plot2_neonat <- highcharter::renderHighchart({
      data7_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_precoc),
          class
        )

      data7_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_precoc),
          class
        )

      data7_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_precoc),
          class
        )

      if (filtros()$comparar == "Não") {
        # validate(
        #   need(
        #     sum(data7()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        # validate(
        #   need(
        #     sum(data6()$obitos_mat_totais) != 0 | sum(data6_comp()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ##### Criando o gráfico de linhas para a mortalidade neonatal tardia, incluido faixas de peso #####

    output$plot3_neonat <- highcharter::renderHighchart({
      data7_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_tardia),
          class
        )

      data7_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_tardia),
          class
        )

      data7_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_tardia),
          class
        )

      if (filtros()$comparar == "Não") {
        # validate(
        #   need(
        #     sum(data7()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        # validate(
        #   need(
        #     sum(data6()$obitos_mat_totais) != 0 | sum(data6_comp()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })



    # Para os indicadores de mortalidade fetal --------------------------------

    ##### Criando o input para selecionar a localidade do resumo quando há comparação #####
    output$input_localidade_resumo_fetal <- renderUI({
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
          inputId = ns("localidade_resumo_fetal"),
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

    numero_obitos_fetais <- reactive({
      dplyr::case_when(
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "peso_fetal") ~ "obitos_fetais",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_menos1500") ~ "fetal_peso_menos_1500",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "fetal_peso_1500_1999",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "fetal_peso_2000_2499",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_peso_mais_2500",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_antes",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_menos1500") ~ "fetal_antes_peso_menos_1500",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "fetal_antes_peso_1500_1999",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "fetal_antes_peso_2000_2499",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_antes_peso_mais_2500",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_durante",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_menos1500") ~ "fetal_durante_peso_menos_1500",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "fetal_durante_peso_1500_1999",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "fetal_durante_peso_2000_2499",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_durante_peso_mais_2500",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_depois",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_menos1500") ~ "fetal_depois_peso_menos_1500",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "fetal_depois_peso_1500_1999",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "fetal_depois_peso_2000_2499",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_depois_peso_mais_2500",
      )
    })

    titulo_obitos_fetais <- reactive({
      dplyr::case_when(
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_menos1500") ~ "Número de óbitos fetais com peso menor que 1500g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "Número de óbitos fetais com peso de 1500 a 1999g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "Número de óbitos fetais com peso de 2000 a 2499g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais com peso maior ou igual a 2500g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais antes do trabalho de parto",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_menos1500") ~ "Número de óbitos fetais antes do trabalho de parto com peso menor que 1500g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "Número de óbitos fetais antes do trabalho de parto com peso de 1500 a 1999g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "Número de óbitos fetais antes do trabalho de parto com peso de 2000 a 2499g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais antes do trabalho de parto com peso maior ou igual a 2500g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais durante o trabalho de parto",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_menos1500") ~ "Número de óbitos fetais durante o trabalho de parto com peso menor que 1500g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "Número de óbitos fetais durante o trabalho de parto com peso de 1500 a 1999g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "Número de óbitos fetais durante o trabalho de parto com peso de 2000 a 2499g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais durante o trabalho de parto com peso maior ou igual a 2500g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais depois do trabalho de parto",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_menos1500") ~ "Número de óbitos fetais depois do trabalho de parto com peso menor que 1500g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "Número de óbitos fetais depois do trabalho de parto com peso de 1500 a 1999g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "Número de óbitos fetais depois do trabalho de parto com peso de 2000 a 2499g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais depois do trabalho de parto com peso maior ou igual a 2500g",
      )
    })

    dados_obitos_fetais <- reactive({
      dplyr::case_when(
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "peso_fetal") ~ obitos_fetais,
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_menos1500") ~ fetal_peso_menos_1500,
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1500_1999") ~ fetal_peso_1500_1999,
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_2000_2499") ~ fetal_peso_2000_2499,
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_mais2500") ~ fetal_peso_mais_2500,
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "peso_fetal") ~ fetal_antes,
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_menos1500") ~ fetal_antes_peso_menos_1500,
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1500_1999") ~ fetal_antes_peso_1500_1999,
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_2000_2499") ~ fetal_antes_peso_2000_2499,
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_mais2500") ~ fetal_antes_peso_mais_2500,
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "peso_fetal") ~ fetal_durante,
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_menos1500") ~ fetal_durante_peso_menos_1500,
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1500_1999") ~ fetal_durante_peso_1500_1999,
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_2000_2499") ~ fetal_durante_peso_2000_2499,
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_mais2500") ~ fetal_durante_peso_mais_2500,
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "peso_fetal") ~ fetal_depois,
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_menos1500") ~ fetal_depois_peso_menos_1500,
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1500_1999") ~ fetal_depois_peso_1500_1999,
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_2000_2499") ~ fetal_depois_peso_2000_2499,
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_mais2500") ~ fetal_depois_peso_mais_2500,
      )
    })


    output$caixa_b7_fetal_i1 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = numero_obitos_fetais(),
        titulo = titulo_obitos_fetais(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[numero_obitos_fetais()]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        #cor = dplyr::if_else(filtros()$nivel == "Nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "303px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b7_fetal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = "taxa_mort_fetal",
        titulo = "Taxa de mortalidade fetal",
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()$taxa_mort_fetal,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })


    #### Gráfico de linhas para o número de óbitos fetais ######

    # output$plot1_fetal_antigo <- highcharter::renderHighchart({
    #   if (filtros()$comparar == "Não") {
    #     # validate(
    #     #   need(
    #     #     data6()$casos_mmg != 0,
    #     #     "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
    #     #   )
    #     # )
    #     grafico_base <- highcharter::highchart() |>
    #       highcharter::hc_add_series(
    #         data = data7(),
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = dados_obitos_fetais(), group = class, colour = class)
    #       ) |>
    #       highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
    #       highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
    #       highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
    #       highcharter::hc_colors(cols)
    #     if (filtros()$nivel == "Nacional") {
    #       grafico_base
    #     } else {
    #       grafico_base |>
    #         highcharter::hc_add_series(
    #           data = data7_referencia(),
    #           type = "line",
    #           name = "Referência (média nacional)",
    #           highcharter::hcaes(x = ano, y = dados_obitos_fetais(), group = class, colour = class),
    #           dashStyle = "ShortDot",
    #           opacity = 0.8
    #         )
    #     }
    #   } else {
    #     # need(
    #     #   data6()$casos_mmg != 0 | data6_comp()$casos_mmg != 0,
    #     #   "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
    #     # )
    #     grafico_base <- highcharter::highchart() |>
    #       highcharter::hc_add_series(
    #         data = data7(),
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = numero_obitos_fetais(), group = class, colour = class)
    #       ) |>
    #       highcharter::hc_add_series(
    #         data = data7_comp(),
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = numero_obitos_fetais(), group = class, colour = class)
    #       ) |>
    #       highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
    #       highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
    #       highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
    #       highcharter::hc_colors(cols)
    #     if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
    #       grafico_base
    #     } else {
    #       grafico_base |>
    #         highcharter::hc_add_series(
    #           data = data7_referencia(),
    #           type = "line",
    #           name = "Referência (média nacional)",
    #           highcharter::hcaes(x = ano, y = numero_obitos_fetais(), group = class, colour = class),
    #           dashStyle = "ShortDot",
    #           opacity = 0.7
    #         )
    #     }
    #   }
    # })


    output$plot1_fetal <- highcharter::renderHighchart({
      data7_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais()),
          class
        )

      data7_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais()),
          class
        )

      data7_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais()),
          class
        )

      if (filtros()$comparar == "Não") {
        # validate(
        #   need(
        #     sum(data7()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (total nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        # validate(
        #   need(
        #     sum(data6()$obitos_mat_totais) != 0 | sum(data6_comp()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (total nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #### Gráfico de linhas para a taxa de mortalidade fetal ######


    output$plot2_fetal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        # validate(
        #   need(
        #     data6()$casos_mmg != 0,
        #     "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7(),
            type = "line",
            highcharter::hcaes(x = ano, y = taxa_mort_fetal, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = taxa_mort_fetal, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        # need(
        #   data6()$casos_mmg != 0 | data6_comp()$casos_mmg != 0,
        #   "Não foram registrados casos de morbidade materna grave no período. Dessa forma, este indicador não se aplica."
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7(),
            type = "line",
            highcharter::hcaes(x = ano, y = taxa_mort_fetal, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = taxa_mort_fetal, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = taxa_mort_fetal, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


############ Para a aba de mortalidade perinatal

    ##### Criando o input para selecionar a localidade do resumo quando há comparação #####
    output$input_localidade_resumo_perinatal <- renderUI({
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
          inputId = ns("localidade_resumo_perinatal"),
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

    titulo_caixa_perinatal_total <- reactive({
      dplyr::case_when(
        input$faixa_peso_perinatal_total == "obitos_perinatal_total" ~ "Número de óbitos perinatais",
        input$faixa_peso_perinatal_total == "perinatal_total_menos1500" ~ "Número de óbitos perinatais de peso menor que 1500g",
        input$faixa_peso_perinatal_total == "perinatal_total_1500_1999" ~ "Número de óbitos perinatais de peso de 1500 a 1999g",
        input$faixa_peso_perinatal_total == "perinatal_total_2000_2499" ~ "Número de óbitos perinatais de peso de 2000 a 2499g",
        input$faixa_peso_perinatal_total == "perinatal_total_mais2500" ~ "Número de óbitos perinatais de peso maior ou igual a 2500g",
      )
    })

    output$caixa_b7_perinatal_i1 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_perinatal_total,
        titulo = titulo_caixa_perinatal_total(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_perinatal_total]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "303px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    titulo_caixa_perinatal_oms <- reactive({
      dplyr::case_when(
        input$faixa_peso_perinatal_oms == "obitos_perinatal_oms" ~ "Número de óbitos perinatais segundo a OMS",
        input$faixa_peso_perinatal_oms == "perinatal_oms_menos1500" ~ "Número de óbitos perinatais segundo a OMS de peso menor que 1500g",
        input$faixa_peso_perinatal_oms == "perinatal_oms_1500_1999" ~ "Número de óbitos perinatais segundo a OMS de peso de 1500 a 1999g",
        input$faixa_peso_perinatal_oms == "perinatal_oms_2000_2499" ~ "Número de óbitos perinatais segundo a OMS de peso de 2000 a 2499g",
        input$faixa_peso_perinatal_oms == "perinatal_oms_mais2500" ~ "Número de óbitos perinatais segundo a OMS de peso maior ou igual a 2500g",
      )
    })

    output$caixa_b7_perinatal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_perinatal_oms,
        titulo = titulo_caixa_perinatal_oms(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_perinatal_oms]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "303px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    ####### Gráfico de óbitos perinatais totais

    output$plot1_perinatal <- highcharter::renderHighchart({
      data7_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_total),
          class
        )

      data7_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_total),
          class
        )

      data7_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_total),
          class
        )

      if (filtros()$comparar == "Não") {
        # validate(
        #   need(
        #     sum(data7()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (total nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        # validate(
        #   need(
        #     sum(data6()$obitos_mat_totais) != 0 | sum(data6_comp()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (total nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    output$plot2_perinatal <- highcharter::renderHighchart({
      data7_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_oms),
          class
        )

      data7_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_oms),
          class
        )

      data7_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_oms),
          class
        )

      if (filtros()$comparar == "Não") {
        # validate(
        #   need(
        #     sum(data7()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (total nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        # validate(
        #   need(
        #     sum(data6()$obitos_mat_totais) != 0 | sum(data6_comp()$obitos_mat_totais) != 0,
        #     "Não foram registrados óbitos maternos no período. Dessa forma, este indicador não se aplica."
        #   )
        # )
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_aux,
              type = "line",
              name = "Referência (total nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })





  })
}


## To be copied in the UI
# mod_bloco_7_ui("bloco_7_1")

## To be copied in the server
# mod_bloco_7_server("bloco_7_1")
