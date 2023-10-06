#' nivel_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nivel_3_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Visão detalhada dos indicadores")), htmlOutput(ns("titulo_pagina"), inline = TRUE), style = "padding-left: 0.4em; font-size: 30px"),
      hr(style = "margin-bottom: 0px;"),
      style = "position: fixed; top: 56px; width: 93.75%; background-color: white; z-index: 100;"
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset1"),
      width = 12,
      collapsible = FALSE,
      tabPanel(
        HTML("<b>Visualizações</b>"),
        conditionalPanel(
          ns = ns,
          condition = "output.bloco_selecionado == 'bloco6'",
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
          )
        ),
        fluidRow(
          column(
            width = 4,
            fluidRow(
              column(
                width = 12,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 380px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Resumo da qualidade da informação &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao"),
                          icon = icon("triangle-exclamation", style = "color: red"),
                          color = "warning",
                          style = "material-circle",
                          size = "xs"
                        )
                      )
                    ),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_infos"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_infos"),
                          icon = icon("info"),
                          color = "primary",
                          style = "material-circle",
                          size = "xs"
                        )
                      )
                    )
                  ),
                  hr(),
                  shinycssloaders::withSpinner(uiOutput(ns("gauge1")), proxy.height = "250px")
                )
              ),
              tags$style(
                HTML("
                  .html-widget.gauge svg {
                    height: 260px; /*or try sth like 320px instead of 100%, whatever you prefer*/
                  }"
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
                  style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: hidden",
                  conditionalPanel(
                    style = "height: 15%; display: flex; align-items: center;",
                    ns = ns,
                    condition = "output.bloco_selecionado != 'bloco6' & output.num_indicadores_incompletude != '2'",
                    HTML("<b style='font-size:19px'> Incompletude da informação </b>")
                  ),
                  conditionalPanel(
                    style = "height: 20%;",
                    ns = ns,
                    condition = "output.num_indicadores_incompletude == '2'",
                    conditionalPanel(
                      ns = ns,
                      condition = "output.bloco_selecionado != 'bloco6'",
                      HTML("<b style='font-size:19px'> Incompletude da informação </b>")
                    ),
                    conditionalPanel(
                      ns = ns,
                      condition = "output.bloco_selecionado == 'bloco6'",
                      HTML("<b style='font-size:19px'> Percentual de óbitos investigados </b>")
                    ),
                    br(),
                    column(
                      width = 12,
                      radioButtons(
                        inputId = ns("variavel_incompletude"),
                        label = NULL,
                        choiceNames = list(textOutput(ns("escolha1")), textOutput(ns("escolha2"))),
                        choiceValues = list("escolha1", "escolha2"),
                        inline = TRUE
                      ),
                      align = "center"
                    )
                  ),
                  hr(),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_incompletude")))
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
                  style = "height: 550px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  conditionalPanel(
                    style = "height: 15%; display: flex; align-items: center;",
                    ns = ns,
                    condition = "output.nome_abreviado != 'rmm'",
                    HTML("<b style='font-size:19px'> Cobertura dos sistemas de informação &nbsp; </b>")
                  ),
                  conditionalPanel(
                    style = "height: 18%;",
                    ns = ns,
                    condition = "output.nome_abreviado == 'rmm'",
                    div(
                      HTML(
                        "<b style='font-size:19px'> Cobertura dos sistemas de informação &nbsp;</b>"
                      )
                    ),
                    br(),
                    column(
                      width = 12,
                      radioButtons(
                        inputId = ns("sistema_cobertura"),
                        label = NULL,
                        choices = c("SIM", "SINASC"),
                        inline = TRUE
                      ),
                      align = "center"
                    )
                  ),
                  hr(),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_cobertura")))
                )
              )
            )
          ),
          column(
            width = 8,
            fluidRow(
              bs4Dash::bs4Card(
                width = 6,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML(
                    "<b style='font-size:19px'> Distribuição do indicador por região do país ao longo do período &nbsp;</b>"
                  )
                ),
                hr(),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_regioes"), height = 450))
              ),
              bs4Dash::bs4Card(
                width = 6,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML(
                    "<b style='font-size:19px'> Evolução do indicador ao longo do período &nbsp;</b>"
                  )
                ),
                hr(),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_serie"), height = 450))
              )
            ),
            fluidRow(
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 923px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%",
                  br(),
                  HTML(
                    "<b style='font-size:19px'> Distribuição do indicador por UF, macrorregião de saúde e município ao longo do período &nbsp;</b>"
                  ),
                  br(),
                  br(),
                  fluidRow(
                    column(
                      width = 12,
                      tags$style(HTML(".radio-inline, .checkbox-inline {overflow-x: auto}")),
                      radioButtons(
                        inputId = ns("opcoes_tab1"),
                        label = NULL,
                        choiceNames = list(
                          HTML("<span> Mostrar informações referentes a todo o país </span>"),
                          HTML("<span> Mostrar informações referentes apenas à localidade escolhida </span>")
                        ),
                        choiceValues = list("escolha1", "escolha2"),
                        selected = "escolha2",
                        inline = TRUE
                      ),
                      align = "center"
                    )
                  )
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela1")), proxy.height = "600px")
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b>Documentação do indicador</b>"),
        fluidRow(
          shinycssloaders::withSpinner(uiOutput(ns("documentacao")), proxy.height = "310px")
        )
      )
    )
  )
}

#' nivel_3 Server Functions
#'
#' @noRd
mod_nivel_3_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##### Criando o vetor que recebe os indicadores que precisam ser criados um a um #####
    indicadores_sem_incompletude <- tabela_indicadores$nome_abreviado[which(tabela_indicadores$num_indicadores_incompletude == 0)]

    ##### Criando o vetor que recebe os indicadores que precisam ser criados um a um #####
    indicadores_especiais <- tabela_indicadores$nome_abreviado[which(tabela_indicadores$numerador == "exceção")]

    ##### Criando o vetor que recebe os indicadores que só estão disponíveis a partir de 2013
    indicadores_sem_2013 <- c(
      "tx_abortos_mil_mulheres_valor_medio",
      "tx_abortos_cem_nascidos_vivos_valor_medio",
      "cobertura_pre_natal",
      "porc_7",
      tabela_indicadores$nome_abreviado[tabela_indicadores$bloco == "bloco4"][-1]
    )


    output$gauge1 <- renderUI({
      if (infos_indicador()$num_indicadores_incompletude == 0 & !(infos_indicador()$nome_abreviado %in% c("tx_abortos_cem_nascidos_vivos_valor_medio", "porc_sc"))) {
        if (infos_indicador()$nome_abreviado == "porc_dependentes_sus") {
          div(
            style = "text-align: center; height: 260px; display: flex; align-items:center; justify-content:center; text-align: center;",
            HTML(
              "
          Este indicador depende da qualidade dos dados de beneficiárias de plano de saúde e das
          estimativas populacionais, que se tornam mais imprecisas quando o período
          intercensitário é maior."
            )
          )
        } else if (infos_indicador()$nome_abreviado == "porc_cobertura_esf") {
          div(
            style = "text-align: center; height: 260px; display: flex; align-items:center; justify-content:center; text-align: center;",
            HTML(
              "
          Este indicador depende da qualidade da informação registrada no CNES e das estimativas populacionais
            do IBGE, que se tornam mais imprecisas quando o período intercensitário é maior."
            )
          )
        } else if (infos_indicador()$nome_abreviado == "tx_abortos_mil_mulheres_valor_medio") {
          div(
            style = "text-align: center; height: 260px; display: flex; align-items:center; justify-content:center; text-align: center;",
            HTML(
              "
          Este indicador depende da cobertura e da qualidade do preenchimento do SIH/SUS e das internações
          hospitalares em serviços do sistema de saúde suplementar, bem como do cálculo das estimativas populacionais, que
            se tornam mais imprecisas conforme aumento o período intercensitário."
            )
          )
        } else if (infos_indicador()$bloco == "bloco6_morbidade") {
          div(
            style = "text-align: center; height: 260px; display: flex; align-items:center; justify-content:center; text-align: center;",
            HTML(
              "
          Este indicador depende da qualidade do preenchimento da Autorização de Internação Hospitalar."
            )
          )
        }
      } else {
        if (infos_indicador()$num_indicadores_incompletude == 1) {
          if (infos_indicador()$bloco != "bloco6") {
            anos_incompletude <- data_grafico_incompletude1()$ano[which(data_grafico_incompletude1()$proporcao > 5)]
          } else {
            anos_incompletude <- data_grafico_incompletude1()$ano[which(data_grafico_incompletude1()$proporcao < 90)]
          }
          anos_cobertura <- data_cobertura()$ano[which(data_cobertura()$cobertura < 90)]
          valor <- round(length(unique(c(anos_incompletude, anos_cobertura)))/length(filtros()$ano2[1]:filtros()$ano2[2]) * 100)
        } else if (infos_indicador()$num_indicadores_incompletude == 2) {
          if (infos_indicador()$bloco != "bloco6") {
            anos_incompletude1 <- data_grafico_incompletude1()$ano[which(data_grafico_incompletude1()$proporcao > 5)]
            anos_incompletude2 <- data_grafico_incompletude2()$ano[which(data_grafico_incompletude2()$proporcao > 5)]
          } else {
            anos_incompletude1 <- data_grafico_incompletude1()$ano[which(data_grafico_incompletude1()$proporcao < 90)]
            anos_incompletude2 <- data_grafico_incompletude2()$ano[which(data_grafico_incompletude2()$proporcao < 100)]
          }
          anos_cobertura <- data_cobertura()$ano[which(data_cobertura()$cobertura < 90)]
          valor <- round(length(unique(c(anos_incompletude1, anos_incompletude2, anos_cobertura)))/length(filtros()$ano2[1]:filtros()$ano2[2]) * 100)
        } else {
          anos_cobertura <- data_cobertura()$ano[which(data_cobertura()$cobertura < 90)]
          valor <- round(length(anos_cobertura)/length(filtros()$ano2[1]:filtros()$ano2[2]) * 100)
        }

        div(
          style = 'text-align: center;',
          flexdashboard::renderGauge({
            flexdashboard::gauge(
              valor,
              min = 0,
              max = 100,
              symbol = '%',
              flexdashboard::gaugeSectors(
                success = c(0, 1),
                warning = c(2, 49),
                danger = c(50, 100),
                colors = c("green", "orange", "#b22222") # "#f2db45"
              )
            )
          }),
          HTML("dos anos considerados apresentam problemas de qualidade nas variáveis necessárias para a construção do indicador")
        )
      }


    })


    observeEvent(input$botao, {
      descricao_incompletude1 <- dplyr::case_when(
        infos_indicador()$numerador_incompletude1 == "idademae_incompletos" ~ "não preenchidos, preenchidos com 99 ou maiores que 55 anos",
        infos_indicador()$numerador_incompletude1 == "racacor_incompletos" ~ "em branco ou ignorados",
        infos_indicador()$numerador_incompletude1 == "escmae_incompletos" ~ "em branco ou ignorados (ESCMAE = 9)",
        infos_indicador()$numerador_incompletude1 == "qtdpartces_incompletos" ~ "não preenchidos ou preenchidos com 99",
        infos_indicador()$numerador_incompletude1 == "qtdpartnor_incompletos" ~ "não preenchidos ou preenchidos com 99",
        infos_indicador()$numerador_incompletude1 == "consprenat_incompletos" ~ "em branco",
        infos_indicador()$numerador_incompletude1 == "mesprenat_incompletos" ~ "em branco",
        infos_indicador()$numerador_incompletude1 == "parto_incompletos" ~ "em branco ou ignorados (PARTO = 9)",
        infos_indicador()$numerador_incompletude1 == "tprobson_incompletos" ~ "em branco ou ignorados (TPROBSON = 11 ou 12)",
        infos_indicador()$numerador_incompletude1 == "peso_incompletos" ~ "em branco ou preenchidos com 9999",
        infos_indicador()$numerador_incompletude1 == "gestacao_incompletos" ~ "em branco ou sem informação",
        infos_indicador()$numerador_incompletude1 == "semagestac_incompletos" ~ "em branco ou sem informação",
        infos_indicador()$numerador_incompletude1 == "parto_tprobson_incompletos" ~ "em branco ou sem informação",
      )

      descricao_incompletude2 <- dplyr::case_when(
        infos_indicador()$numerador_incompletude2 == "idademae_incompletos" ~ "não preenchidos, preenchidos com 99 ou maiores que 55 anos",
        infos_indicador()$numerador_incompletude2 == "racacor_incompletos" ~ "em branco ou ignorados",
        infos_indicador()$numerador_incompletude2 == "escmae_incompletos" ~ "em branco ou ignorados (ESCMAE = 9)",
        infos_indicador()$numerador_incompletude2 == "qtdpartces_incompletos" ~ "não preenchidos ou preenchidos com 99",
        infos_indicador()$numerador_incompletude2 == "qtdpartnor_incompletos" ~ "não preenchidos ou preenchidos com 99",
        infos_indicador()$numerador_incompletude2 == "consprenat_incompletos" ~ "em branco",
        infos_indicador()$numerador_incompletude2 == "mesprenat_incompletos" ~ "em branco",
        infos_indicador()$numerador_incompletude2 == "parto_incompletos" ~ "em branco ou ignorados (PARTO = 9)",
        infos_indicador()$numerador_incompletude2 == "tprobson_incompletos" ~ "em branco ou ignorados (TPROBSON = 11 ou 12)",
        infos_indicador()$numerador_incompletude2 == "peso_incompletos" ~ "em branco ou preenchidos com 9999",
        infos_indicador()$numerador_incompletude2 == "gestacao_incompletos" ~ "em branco ou sem informação",
        infos_indicador()$numerador_incompletude2 == "semagestac_incompletos" ~ "em branco ou sem informação"
      )

      cria_modal_incompletude(
        incompletude1 = data_grafico_incompletude1()$proporcao,
        variavel_incompletude1 = ifelse(
          infos_indicador()$numerador_incompletude1 == "parto_tprobson_incompletos",
          "PARTO e TPROBSON",
          stringr::str_remove(unlist(strsplit(infos_indicador()$nome_incompletude1, ' '))[4], ',')
        ),
        descricao_incompletude1 = descricao_incompletude1,
        incompletude2 = data_grafico_incompletude2()$proporcao,
        variavel_incompletude2 = stringr::str_remove(unlist(strsplit(infos_indicador()$nome_incompletude2, ' '))[4], ','),
        descricao_incompletude2 = descricao_incompletude2,
        df = data_grafico_incompletude1(),
        cobertura = data_cobertura()$cobertura,
        nivel = 3,
        bloco = dplyr::case_when(
          infos_indicador()$bloco == "bloco6" ~ "bloco6",
          infos_indicador()$bloco == "bloco4_deslocamento" ~ "deslocamento",
          !(infos_indicador()$bloco %in% c("bloco6", "bloco4_deslocamento")) ~ "geral"
        )
      )
    })

    ##### Buscando as informações do indicador selecionado na tabela_indicadores #####
    infos_indicador <- reactive({
      if (!(filtros()$bloco %in% c("bloco4", "bloco6"))) {
        tabela_indicadores |>
          dplyr::filter(indicador == filtros()$indicador)
      } else {
        tabela_indicadores |>
          dplyr::filter(indicador == filtros()$indicador_blocos4_6)
      }

    })

    output$nome_abreviado <- renderText(infos_indicador()$nome_abreviado)
    output$bloco_selecionado <- renderText({infos_indicador()$bloco})
    output$num_indicadores_incompletude <- renderText({infos_indicador()$num_indicadores_incompletude})
    output$escolha1 <- renderText({
      dplyr::case_when(
        infos_indicador()$bloco == "bloco6" ~ "Óbitos de MIF",
        infos_indicador()$bloco == "bloco4_deslocamento" ~ "DN sem CNES preenchido",
        !(infos_indicador()$bloco %in% c("bloco6", "bloco4_deslocamento")) ~ stringr::str_remove(unlist(strsplit(infos_indicador()$nome_incompletude1, ' '))[4], ',')
      )
    })
    output$escolha2 <- renderText({
      dplyr::case_when(
        infos_indicador()$bloco == "bloco6" ~ "Óbitos maternos",
        infos_indicador()$bloco == "bloco4_deslocamento" ~ "DN com CNES inválido",
        !(infos_indicador()$bloco %in% c("bloco6", "bloco4_deslocamento")) ~ stringr::str_remove(unlist(strsplit(infos_indicador()$nome_incompletude2, ' '))[4], ',')
      )
    })

    outputOptions(output, "nome_abreviado", suspendWhenHidden = FALSE)
    outputOptions(output, "bloco_selecionado", suspendWhenHidden = FALSE)
    outputOptions(output, "num_indicadores_incompletude", suspendWhenHidden = FALSE)
    outputOptions(output, "escolha1", suspendWhenHidden = FALSE)
    outputOptions(output, "escolha2", suspendWhenHidden = FALSE)

    ##### Buscando a documentação do indicador selecionado #####
    output$documentacao <- renderUI({
      includeHTML(glue::glue("inst/app/www/html/{infos_indicador()$nome_abreviado}.html"))
    })


    ##### Dados de cobertura para o indicador selecionado #####
    data_cobertura <- reactive({
      if (infos_indicador()$bloco == "bloco6") {
        if (infos_indicador()$nome_abreviado == "rmm" & input$sistema_cobertura == "SINASC") {
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
              ano = filtros()$ano,
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
        } else {
          if (filtros()$nivel == "Municipal") {
            sub_registro_sim_muni_2015_2020 |>
              dplyr::filter(
                ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2],
                municipio == filtros()$municipio,
                uf == filtros()$estado_municipio
              ) |>
              dplyr::rename(
                localidade = municipio
              )
          } else if (filtros()$nivel == "Estadual") {
            sub_registro_sim_uf_regioes_2015_2020 |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                localidade == filtros()$estado
              )
          } else if (filtros()$nivel == "Regional") {
            sub_registro_sim_uf_regioes_2015_2020 |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                localidade == filtros()$regiao
              )
          } else if (filtros()$nivel == "Nacional") {
            sub_registro_sim_uf_regioes_2015_2020 |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                localidade == "Brasil"
              )
          } else {
            data.frame(
              ano = filtros()$ano,
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
        }
      } else {
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
            ano = filtros()$ano,
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
      }

    })


    ##### Dados de incompletude para o indicador selecionado #####
    data_grafico_incompletude1 <- reactive({
      if (infos_indicador()$num_indicadores_incompletude != 0) {
        if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013) {
          base_incompletude <- base_incompletude |>
            dplyr::filter(
              ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
            )
        } else {
          base_incompletude <- base_incompletude |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        }
        if (!(infos_indicador()$bloco %in% c("bloco6", "bloco4_deslocamento"))) {
          base_incompletude |>
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
            dplyr::group_by(ano) |>
            dplyr::summarise(
              numerador := sum(.data[[infos_indicador()$numerador_incompletude1]], na.rm = TRUE),
              denominador := sum(.data[[infos_indicador()$denominador_incompletude1]], na.rm = TRUE),
              proporcao = round(numerador/denominador * {infos_indicador()$fator_incompletude}, 2),
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
        } else {
          if (infos_indicador()$bloco == "bloco6") {
            base_incompletude |>
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
              dplyr::group_by(ano) |>
              dplyr::summarise(
                proporcao = round((sum(obito_mif_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_mif_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_mulher_idade_fertil, na.rm = TRUE) * 100, 2),
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
          } else if (infos_indicador()$bloco == "bloco4_deslocamento") {
            base_incompletude |>
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
              dplyr::group_by(ano) |>
              dplyr::summarise(
                proporcao = round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_preenchido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 2),
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
          }
        }
      }
    })

    data_grafico_incompletude2 <- reactive({
      if (infos_indicador()$num_indicadores_incompletude == 2) {
        if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013) {
          base_incompletude <- base_incompletude |>
            dplyr::filter(
              ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
            )
        } else {
          base_incompletude <- base_incompletude |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            )
        }
        if (!(infos_indicador()$bloco %in% c("bloco6", "bloco4_deslocamento"))) {
          base_incompletude |>
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
            dplyr::group_by(ano) |>
            dplyr::summarise(
              numerador := sum(.data[[infos_indicador()$numerador_incompletude2]], na.rm = TRUE),
              denominador := sum(.data[[infos_indicador()$denominador_incompletude2]], na.rm = TRUE),
              proporcao = round(numerador/denominador * {infos_indicador()$fator_incompletude}, 2),
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
        } else {
          if (infos_indicador()$bloco == "bloco6") {
            base_incompletude |>
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
              dplyr::group_by(ano) |>
              dplyr::summarise(
                proporcao = round((sum(obito_materno_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_materno_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_maternos, na.rm = TRUE) * 100, 2),
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
          } else if (infos_indicador()$bloco == "bloco4_deslocamento") {
            base_incompletude |>
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
              dplyr::group_by(ano) |>
              dplyr::summarise(
                proporcao = round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_valido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 2),
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
          }

        }
      }

    })

    data_referencia_incompletude <- reactive({
      if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013) {
        if (!(infos_indicador()$bloco %in% c("bloco6"))) {
          data.frame(
            ano = max(2014, max(2014, filtros()$ano2[1])):filtros()$ano2[2],
            bom = rep(10, times = length(max(2014, filtros()$ano2[1]):filtros()$ano2[2])),
            excelente = rep(5, times = length(max(2014, filtros()$ano2[1]):filtros()$ano2[2])),
            class_bom = rep("Bom", times = length(max(2014, filtros()$ano2[1]):filtros()$ano2[2])),
            class_excelente = rep("Excelente", times = length(max(2014, filtros()$ano2[1]):filtros()$ano2[2]))
          )
        } else {
          if (infos_indicador()$bloco == "bloco6") {
            data.frame(
              ano = max(2014, max(2014, filtros()$ano2[1])):filtros()$ano2[2],
              ideal_mif = rep(90, times = length(max(2014, filtros()$ano2[1]):filtros()$ano2[2])),
              ideal_maternos = rep(100, times = length(max(2014, filtros()$ano2[1]):filtros()$ano2[2])),
              class = rep("Ideal", times = length(max(2014, filtros()$ano2[1]):filtros()$ano2[2]))
            )
          }
        }
      } else {
        if (!(infos_indicador()$bloco %in% c("bloco6"))) {
          data.frame(
            ano = filtros()$ano2[1]:filtros()$ano2[2],
            bom = rep(10, times = length(filtros()$ano2[1]:filtros()$ano2[2])),
            excelente = rep(5, times = length(filtros()$ano2[1]:filtros()$ano2[2])),
            class_bom = rep("Bom", times = length(filtros()$ano2[1]:filtros()$ano2[2])),
            class_excelente = rep("Excelente", times = length(filtros()$ano2[1]:filtros()$ano2[2]))
          )
        } else {
          if (infos_indicador()$bloco == "bloco6") {
            data.frame(
              ano = filtros()$ano2[1]:filtros()$ano2[2],
              ideal_mif = rep(90, times = length(filtros()$ano2[1]:filtros()$ano2[2])),
              ideal_maternos = rep(100, times = length(filtros()$ano2[1]:filtros()$ano2[2])),
              class = rep("Ideal", times = length(filtros()$ano2[1]:filtros()$ano2[2]))
            )
          }
        }
      }

    })

    data_referencia_cobertura <- reactive({
      data.frame(
        ano = max(2015, filtros()$ano2[1]):filtros()$ano2[2],
        referencia = 90,
        localidade = "Referência (Ministério da Saúde)"
      )
    })


    ##### Dados para a construção do gráfico de barras para o indicador selecionado #####
    data_grafico_regioes <- reactive({
      validate(
        need(
          !startsWith(infos_indicador()$indicador, "Medianas"),
          "Os indicadores de medianas de deslocamento para o parto não estão disponíveis para microrregiões e macrorregiões de saúde, para regiões do país e para o nível de análise nacional. Dessa forma, esta visualização não se aplica."
        )
      )
      if (!(infos_indicador()$nome_abreviado %in% indicadores_especiais)) {
        if (infos_indicador()$bloco != "bloco4_deslocamento") {
          get(filtros()$bloco) |>
            dplyr::filter(
              if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013)
                ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
              else
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(regiao) |>
            dplyr::summarise(
              numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
              denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
              proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1)
            ) |>
            dplyr::ungroup()
        } else {
          if (!base::startsWith(infos_indicador()$indicador, "Medianas")) {
            if (filtros()$nivel != "Estadual") {
              bloco4_deslocamento_muni |>
                dplyr::filter(
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
                ) |>
                dplyr::group_by(regiao) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1)
                ) |>
                dplyr::ungroup()
            } else {
              bloco4_deslocamento_uf |>
                dplyr::filter(
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
                ) |>
                dplyr::group_by(regiao) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1)
                ) |>
                dplyr::ungroup()
            }
          }
        }
      } else {
        if (infos_indicador()$nome_abreviado == "porc_dependentes_sus") {
          bloco1 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(regiao) |>
            dplyr::summarise(
              numerador := sum(populacao_feminina_10_a_49, na.rm = TRUE) - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE),
              denominador := sum(populacao_feminina_10_a_49, na.rm = TRUE),
              proporcao = round(numerador/denominador * 100, 1)
            ) |>
            dplyr::ungroup()
        } else if (infos_indicador()$nome_abreviado == "porc_cobertura_esf") {
          #Fazer pro resto dos indicadores no vetor "indicadores_especiais"
          bloco1 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(regiao) |>
            dplyr::summarise(
              numerador := sum(media_cobertura_esf, na.rm = TRUE),
              denominador := sum(populacao_total, na.rm = TRUE),
              proporcao = round(numerador/denominador * 100, 1)
            ) |>
            dplyr::ungroup()
        }
        else if (infos_indicador()$nome_abreviado == "tx_abortos_mil_mulheres_valor_medio") {
        bloco2 |>
          dplyr::filter(
            ano >= max(filtros()$ano2[1], 2015) & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(regiao) |>
          dplyr::summarise(
            numerador := ((((sum(abortos_sus_menor_30, na.rm = TRUE)*0.90) + (sum(abortos_sus_30_a_39, na.rm = TRUE)*0.85) + (sum(abortos_sus_40_a_49, na.rm = TRUE)*0.75)) * 3) + (((sum(abortos_ans_menor_30, na.rm = TRUE)*0.90) + (sum(abortos_ans_30_a_39, na.rm = TRUE)*0.85) + (sum(abortos_ans_40_a_49, na.rm = TRUE)*0.75)) * 2)),
            denominador := sum(pop_fem_10_49, na.rm = TRUE),
            proporcao = round(numerador/denominador * 1000, 1)
          ) |>
          dplyr::ungroup()
        } else if (infos_indicador()$nome_abreviado == "tx_abortos_cem_nascidos_vivos_valor_medio") {
          bloco2 |>
            dplyr::filter(
              ano >= max(filtros()$ano2[1], 2015) & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(regiao) |>
            dplyr::summarise(
              numerador := ((((sum(abortos_sus_menor_30, na.rm = TRUE)*0.9)+(sum(abortos_sus_30_a_39, na.rm = TRUE)*0.85)+(sum(abortos_sus_40_a_49, na.rm = TRUE)*0.75))*3)+(((sum(abortos_ans_menor_30, na.rm = TRUE)*0.9)+(sum(abortos_ans_30_a_39, na.rm = TRUE)*0.85)+(sum(abortos_ans_40_a_49, na.rm = TRUE)*0.75))*2)),
              denominador := sum(total_de_nascidos_vivos, na.rm = TRUE),
              proporcao = round(numerador/denominador * 100, 1)
            ) |>
            dplyr::ungroup()
        } else if (infos_indicador()$nome_abreviado == "obitos_mat_totais") {
          bloco6 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(regiao) |>
            dplyr::summarise(
              numerador := sum(obitos_mat_totais, na.rm = TRUE),
              denominador := 1,
              proporcao = round(numerador/denominador * 1, 1)
            ) |>
            dplyr::ungroup()
        }
      }
    })

    ##### Dados para a construção do gráfico de linhas para o indicador selecionado #####
    data_grafico_serie <- reactive({
      if (!(infos_indicador()$nome_abreviado %in% indicadores_especiais)) {
        if (infos_indicador()$bloco != "bloco4_deslocamento") {
          get(filtros()$bloco) |>
            dplyr::filter(
              if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013)
                ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
              else
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
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
            dplyr::group_by(ano) |>
            dplyr::summarise(
              numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
              denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
              proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1),
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
        } else {
          if (!base::startsWith(infos_indicador()$indicador, "Medianas")) {
            if (filtros()$nivel != "Estadual") {
              bloco4_deslocamento_muni |>
                dplyr::filter(
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                  if (filtros()$nivel == "Nacional")
                    ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
                  else if (filtros()$nivel == "Regional")
                    regiao == filtros()$regiao
                  else if (filtros()$nivel == "Macrorregião de saúde")
                    macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
                  else if(filtros()$nivel == "Microrregião de saúde")
                    r_saude == filtros()$micro & uf == filtros()$estado_micro
                  else if(filtros()$nivel == "Municipal")
                    municipio == filtros()$municipio & uf == filtros()$estado_municipio
                ) |>
                dplyr::group_by(ano) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1),
                  class = dplyr::case_when(
                    filtros()$nivel == "Nacional" ~ "Brasil",
                    filtros()$nivel == "Regional" ~ filtros()$regiao,
                    filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
                    filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
                    filtros()$nivel == "Municipal" ~ filtros()$municipio
                  )
                ) |>
                dplyr::ungroup()
            } else {
              bloco4_deslocamento_uf |>
                dplyr::filter(
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                  uf == filtros()$estado
                ) |>
                dplyr::group_by(ano) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1),
                  class = filtros()$estado,
                ) |>
                dplyr::ungroup()
            }
          }
        }
      } else {
        if (infos_indicador()$nome_abreviado == "porc_dependentes_sus") {
          bloco1 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
              numerador := sum(populacao_feminina_10_a_49, na.rm = TRUE) - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE),
              denominador := sum(populacao_feminina_10_a_49, na.rm = TRUE),
              proporcao = round(numerador/denominador * 100, 1),
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
        } else if (infos_indicador()$nome_abreviado == "porc_cobertura_esf") {
          bloco1 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
              numerador := sum(media_cobertura_esf, na.rm = TRUE),
              denominador := sum(populacao_total, na.rm = TRUE),
              proporcao = round(numerador/denominador * 100, 1),
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
        } else if (infos_indicador()$nome_abreviado == "tx_abortos_mil_mulheres_valor_medio") {
          bloco2 |>
            dplyr::filter(
              ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2],
              if (filtros()$nivel == "Nacional")
                ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
              tx_abortos_mil_mulheres_lim_inf = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 2) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 1)) / sum(pop_fem_10_49) * 1000, 1),
              tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
              tx_abortos_mil_mulheres_lim_sup = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 4) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 3)) / sum(pop_fem_10_49) * 1000, 1),
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

        } else if (infos_indicador()$nome_abreviado == "tx_abortos_cem_nascidos_vivos_valor_medio") {
          bloco2 |>
            dplyr::filter(
              ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2],
              if (filtros()$nivel == "Nacional")
                ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
              tx_abortos_cem_nascidos_vivos_lim_inf = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*2)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*1))/sum(total_de_nascidos_vivos) *100, 1),
              tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1),
              tx_abortos_cem_nascidos_vivos_lim_sup = round(((((sum(abortos_sus_menor_30)*0.90)+ (sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*4) + (((sum(abortos_ans_menor_30)*0.90)+(sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75))*3))/sum(total_de_nascidos_vivos) * 100, 1),
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

        } else if (infos_indicador()$nome_abreviado == "obitos_mat_totais") {
          bloco6 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
              numerador := sum(obitos_mat_totais, na.rm = TRUE),
              denominador := 1,
              proporcao = round(numerador/denominador * 1, 1),
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
        } else if (startsWith(infos_indicador()$indicador, "Medianas")) {
          if (filtros()$nivel != "Estadual") {
            bloco4_deslocamento_muni |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                if (filtros()$nivel == "Nacional")
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
                else if (filtros()$nivel == "Regional")
                  regiao == filtros()$regiao
                else if (filtros()$nivel == "Macrorregião de saúde")
                  macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
                else if(filtros()$nivel == "Microrregião de saúde")
                  r_saude == filtros()$micro & uf == filtros()$estado_micro
                else if(filtros()$nivel == "Municipal")
                  municipio == filtros()$municipio & uf == filtros()$estado_municipio
              ) |>
              dplyr::group_by(ano) |>
              dplyr::mutate(
                km_total := .data[[infos_indicador()$nome_abreviado]],
                km_baixa_complexidade := .data[[paste0(infos_indicador()$nome_abreviado, "_baixa_complexidade")]],
                km_alta_complexidade := .data[[paste0(infos_indicador()$nome_abreviado, "_alta_complexidade")]],
                class = dplyr::case_when(
                  filtros()$nivel == "Nacional" ~ "Brasil",
                  filtros()$nivel == "Regional" ~ filtros()$regiao,
                  filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
                  filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
                  filtros()$nivel == "Municipal" ~ filtros()$municipio
                )
              ) |>
              dplyr::ungroup()
          } else {
            bloco4_deslocamento_uf |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                uf == filtros()$estado
              ) |>
              dplyr::group_by(ano) |>
              dplyr::mutate(
                km_total := .data[[infos_indicador()$nome_abreviado]],
                km_baixa_complexidade := .data[[paste0(infos_indicador()$nome_abreviado, "_baixa_complexidade")]],
                km_alta_complexidade := .data[[paste0(infos_indicador()$nome_abreviado, "_alta_complexidade")]],
                class = filtros()$estado
              ) |>
              dplyr::ungroup()
          }
        }
      }

    })


    data_fator_de_correcao <- reactive({
      if (infos_indicador()$nome_abreviado == "rmm") {
        if (filtros()$nivel %in% c("Estadual", "Regional", "Nacional")) {
          if (filtros()$nivel == "Estadual") {
            rmm_fator_de_correcao |>
              dplyr::filter(
                localidade == filtros()$estado,
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              )
          } else if (filtros()$nivel == "Regional") {
            rmm_fator_de_correcao |>
              dplyr::filter(
                localidade == filtros()$regiao,
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              )
          } else {
            rmm_fator_de_correcao |>
              dplyr::filter(
                localidade == "Brasil",
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              )
          }
        } else {
          data.frame(
            ano = filtros()$ano2[1]:filtros()$ano2[2],
            fator_de_correcao = rep(1, length(filtros()$ano2[1]:filtros()$ano2[2]))
          )
        }
      }
    })

    data_rmm_corrigida_aux <- reactive({
      if (infos_indicador()$nome_abreviado == "rmm") {
        dplyr::full_join(data_grafico_serie(), data_fator_de_correcao(), by = "ano")
      }
    })

    data_rmm_corrigida <- reactive({
      if (infos_indicador()$nome_abreviado == "rmm") {
        data_rmm_corrigida_aux() |>
          dplyr::mutate(
            rmm = round(proporcao*fator_de_correcao, 1)
          )
      }

    })

    ##### Dados para a construção da linha de referência para o indicador selecionado #####
    data_referencia <- reactive({
      if (!(infos_indicador()$nome_abreviado %in% indicadores_especiais)) {
        if (infos_indicador()$bloco != "bloco4_deslocamento") {
          if (infos_indicador()$referencia == "Nacional") {
            get(filtros()$bloco) |>
              dplyr::filter(
                if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013)
                  ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
                else
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              ) |>
              dplyr::group_by(ano) |>
              dplyr::summarise(
                numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1),
                class = "Referência"
              ) |>
              dplyr::ungroup()
          } else {
            get(filtros()$bloco) |>
              dplyr::filter(
                if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013)
                  ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
                else
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              ) |>
              dplyr::group_by(ano) |>
              dplyr::summarise(
                proporcao = {as.numeric(infos_indicador()$referencia)},
                class = "Referência"
              ) |>
              dplyr::ungroup()
          }
        } else {
          if (!base::startsWith(infos_indicador()$indicador, "Medianas")) {
            if (filtros()$nivel != "Estadual") {
              bloco4_deslocamento_muni |>
                dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
                dplyr::group_by(ano) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1),
                  class = "Referência"
                ) |>
                dplyr::ungroup()
            } else {
              bloco4_deslocamento_uf |>
                dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
                dplyr::group_by(ano) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao = round(numerador/denominador * {infos_indicador()$fator}, 1),
                  class = "Referência"
                ) |>
                dplyr::ungroup()
            }
          }
        }

      } else {
        if (infos_indicador()$nome_abreviado == "porc_dependentes_sus") {
          bloco1 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(ano) |>
            dplyr::summarise(
              numerador := sum(populacao_feminina_10_a_49, na.rm = TRUE) - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE),
              denominador := sum(populacao_feminina_10_a_49, na.rm = TRUE),
              proporcao = round(numerador/denominador * 100, 1),
              class = "Referência (média nacional)"
            ) |>
            dplyr::ungroup()
        } else if (infos_indicador()$nome_abreviado == "porc_cobertura_esf") {
          bloco1 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(ano) |>
            dplyr::summarise(
              numerador := 95,
              denominador := 100,
              proporcao = round(numerador/denominador * 100, 1),
              class = "Referência (meta ODS)"
            ) |>
            dplyr::ungroup()

        } else if (infos_indicador()$nome_abreviado == "tx_abortos_mil_mulheres_valor_medio") {
          bloco2 |>
            dplyr::filter(
              ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(ano) |>
            dplyr::summarise(
              tx_abortos_mil_mulheres_lim_inf = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 2) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 1)) / sum(pop_fem_10_49) * 1000, 1),
              tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
              tx_abortos_mil_mulheres_lim_sup = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 4) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 3)) / sum(pop_fem_10_49) * 1000, 1),
              numerador := ((((sum(abortos_sus_menor_30, na.rm = TRUE)*0.90) + (sum(abortos_sus_30_a_39, na.rm = TRUE)*0.85) + (sum(abortos_sus_40_a_49, na.rm = TRUE)*0.75)) * 3) + (((sum(abortos_ans_menor_30, na.rm = TRUE)*0.90) + (sum(abortos_ans_30_a_39, na.rm = TRUE)*0.85) + (sum(abortos_ans_40_a_49, na.rm = TRUE)*0.75)) * 2)),
              denominador := sum(pop_fem_10_49, na.rm = TRUE),
              proporcao = round(numerador/denominador * 1000, 1),
              class = "Referência (média nacional)"
            ) |>
            dplyr::ungroup()

        } else if (infos_indicador()$nome_abreviado == "tx_abortos_cem_nascidos_vivos_valor_medio") {
          bloco2 |>
            dplyr::filter(
              ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(ano) |>
            dplyr::summarise(
              tx_abortos_cem_nascidos_vivos_lim_inf = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*2)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*1))/sum(total_de_nascidos_vivos) *100, 1),
              tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1),
              tx_abortos_cem_nascidos_vivos_lim_sup = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*4)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*3))/sum(total_de_nascidos_vivos) *100, 1),
              numerador := ((((sum(abortos_sus_menor_30, na.rm = TRUE)*0.9)+(sum(abortos_sus_30_a_39, na.rm = TRUE)*0.85)+(sum(abortos_sus_40_a_49, na.rm = TRUE)*0.75))*3)+(((sum(abortos_ans_menor_30, na.rm = TRUE)*0.9)+(sum(abortos_ans_30_a_39, na.rm = TRUE)*0.85)+(sum(abortos_ans_40_a_49, na.rm = TRUE)*0.75))*2)),
              denominador := sum(total_de_nascidos_vivos, na.rm = TRUE),
              proporcao = round(numerador/denominador * 100, 1),
              class = "Referência (média nacional)"
            ) |>
            dplyr::ungroup()

        } else if (infos_indicador()$nome_abreviado == "obitos_mat_totais") {
          bloco6 |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(ano) |>
            dplyr::summarise(
              numerador := sum(obitos_mat_totais, na.rm = TRUE),
              denominador := 1,
              proporcao = round(numerador/denominador * 1, 1),
              class = "Referência (média nacional)"
            ) |>
            dplyr::ungroup()

        }
      }
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


    ##### Dados para a construção da tabela para o indicador selecionado (aqui você mexe) #####
    data_tabela1 <- eventReactive(c(input$opcoes_tab1, filtros()$pesquisar), {
      validate(
        need(
          !startsWith(infos_indicador()$indicador, "Medianas"),
          "Os indicadores de medianas de deslocamento para o parto não estão disponíveis para microrregiões e macrorregiões de saúde, para regiões do país e para o nível de análise nacional. Além disso, mesmo que esses indicadores sejam calculados para municípios e estados, calcular um valor médio para representar o resumo do período não é aconselhável. Dessa forma, esta visualização não se aplica."
        )
      )
      if (!(infos_indicador()$nome_abreviado %in% indicadores_especiais)) {
        if (input$opcoes_tab1 == "escolha1") {
          if (infos_indicador()$bloco != "bloco4_deslocamento") {
            get(filtros()$bloco) |>
              dplyr::filter(
                if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013)
                  ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
                else
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              ) |>
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador = sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                denominador = sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          } else {
            if (filtros()$nivel != "Estadual") {
              bloco4_deslocamento_muni |>
                dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
                dplyr::group_by(uf, macro_r_saude, municipio) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao := dplyr::if_else(
                    condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                    true = numerador/denominador,
                    false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                  )
                ) |>
                dplyr::ungroup()
            } else {
              bloco4_deslocamento_uf |>
                dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
                dplyr::group_by(uf, macro_r_saude, municipio) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao := dplyr::if_else(
                    condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                    true = numerador/denominador,
                    false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                  )
                ) |>
                dplyr::ungroup()
            }
          }

        } else {
          if (infos_indicador()$bloco != "bloco4_deslocamento") {
            get(filtros()$bloco) |>
              dplyr::filter(
                if (infos_indicador()$nome_abreviado %in% indicadores_sem_2013)
                  ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
                else
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              ) |>
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
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador = sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                denominador = sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          } else {
            if (filtros()$nivel != "Estadual") {
              bloco4_deslocamento_muni |>
                dplyr::filter(
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                  if (filtros()$nivel == "Nacional")
                    ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
                  else if (filtros()$nivel == "Regional")
                    regiao == filtros()$regiao
                  else if (filtros()$nivel == "Macrorregião de saúde")
                    macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
                  else if(filtros()$nivel == "Microrregião de saúde")
                    r_saude == filtros()$micro & uf == filtros()$estado_micro
                  else if(filtros()$nivel == "Municipal")
                    municipio == filtros()$municipio & uf == filtros()$estado_municipio
                ) |>
                dplyr::group_by(uf, macro_r_saude, municipio) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao := dplyr::if_else(
                    condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                    true = numerador/denominador,
                    false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                  )
                ) |>
                dplyr::ungroup()
            } else {
              bloco4_deslocamento_uf |>
                dplyr::filter(
                  ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
                  uf == filtros()$estado
                ) |>
                dplyr::group_by(uf, macro_r_saude, municipio) |>
                dplyr::summarise(
                  numerador := sum(.data[[infos_indicador()$numerador]], na.rm = TRUE),
                  denominador := sum(.data[[infos_indicador()$denominador]], na.rm = TRUE),
                  proporcao := dplyr::if_else(
                    condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                    true = numerador/denominador,
                    false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                  )
                ) |>
                dplyr::ungroup()
            }
          }
        }
      } else {
        if (infos_indicador()$nome_abreviado == "porc_dependentes_sus") {
          if (input$opcoes_tab1 == "escolha1") {
            bloco1 |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              ) |>
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador := sum(populacao_feminina_10_a_49, na.rm = TRUE) - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE),
                denominador := sum(populacao_feminina_10_a_49, na.rm = TRUE),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          } else {
            bloco1 |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador := sum(populacao_feminina_10_a_49, na.rm = TRUE) - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE),
                denominador := sum(populacao_feminina_10_a_49, na.rm = TRUE),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          }
        } else if (infos_indicador()$nome_abreviado == "porc_cobertura_esf") {
          if (input$opcoes_tab1 == "escolha1") {
            bloco1 |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              ) |>
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador := sum(media_cobertura_esf, na.rm = TRUE),
                denominador := sum(populacao_total, na.rm = TRUE),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          } else {
            bloco1 |>
              dplyr::filter(
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador := sum(media_cobertura_esf, na.rm = TRUE),
                denominador := sum(populacao_total, na.rm = TRUE),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          }
        } else if (infos_indicador()$nome_abreviado == "tx_abortos_mil_mulheres_valor_medio") {
          if (input$opcoes_tab1 == "escolha1") {
            bloco2 |>
              dplyr::filter(
                ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
              ) |>
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador := ((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)),
                denominador := sum(pop_fem_10_49, na.rm = TRUE),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          } else {
            bloco2 |>
              dplyr::filter(
                ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2],
                if (filtros()$nivel == "Nacional")
                  ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador := ((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)),
                denominador := sum(pop_fem_10_49, na.rm = TRUE),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          }
        } else if (infos_indicador()$nome_abreviado == "tx_abortos_cem_nascidos_vivos_valor_medio") {
          if (input$opcoes_tab1 == "escolha1") {
            bloco2 |>
              dplyr::filter(
                ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
              ) |>
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador := ((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2)),
                denominador := sum(total_de_nascidos_vivos),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          } else {
            bloco2 |>
              dplyr::filter(
                ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2],
                if (filtros()$nivel == "Nacional")
                  ano >= max(2015, filtros()$ano2[1]) & ano <= filtros()$ano2[2]
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
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                numerador := ((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2)),
                denominador := sum(total_de_nascidos_vivos),
                proporcao := dplyr::if_else(
                  condition = infos_indicador()$tipo_do_indicador == "porcentagem",
                  true = numerador/denominador,
                  false = round(numerador/denominador * {infos_indicador()$fator}, 1)
                )
              ) |>
              dplyr::ungroup()
          }
        } else if (infos_indicador()$nome_abreviado == "obitos_mat_totais") {
          if (input$opcoes_tab1 == "escolha1") {
            bloco6 |>
              dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                obitos_mat_totais = sum(obitos_mat_totais)
              ) |>
              dplyr::ungroup()
          } else {
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
              dplyr::group_by(uf, macro_r_saude, municipio) |>
              dplyr::summarise(
                obitos_mat_totais = sum(obitos_mat_totais)
              ) |>
              dplyr::ungroup()
          }
        }
      }
    },
    ignoreNULL = FALSE
    )

    #cores pros graficos
    cols <- c("#2c115f", "#b73779", "#fc8961")

    output$titulo_pagina <- renderUI({
      if (length(filtros()$ano2[1]:filtros()$ano2[2]) > 1) {
        ano <- glue::glue("{filtros()$ano2[1]} a {filtros()$ano2[2]}")
      } else {
        ano <- filtros()$ano2[1]
      }

      local1 <- dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )

      tags$b(paste("-", infos_indicador()$indicador, glue::glue("({local1}, {ano})")), style = "font-size: 30px")
    })


    output$grafico_incompletude <- highcharter::renderHighchart({
      validate(
        need(
          infos_indicador()$num_indicadores_incompletude != 0,
          "Informações a respeito da incompletude das variáveis necessárias para a construção deste indicador não estão disponíveis."
        )
      )
      if (infos_indicador()$num_indicadores_incompletude == 1) {
        data_grafico_incompletude <- data_grafico_incompletude1()
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_grafico_incompletude,
            type = "line",
            highcharter::hcaes(x = ano, y = proporcao, group = localidade, colour = localidade)
          ) |>
          highcharter::hc_add_series(
            data = data_referencia_incompletude(),
            type = "line",
            highcharter::hcaes(x = ano, y = excelente, group = class_excelente, colour = class_excelente),
            dashStyle = "ShortDot",
            opacity = 0.8
            #tooltip = list(valuePrefix = "<")
          ) |>
          highcharter::hc_add_series(
            data = data_referencia_incompletude(),
            type = "line",
            highcharter::hcaes(x = ano, y = bom, group = class_bom, colour = class_bom),
            dashStyle = "ShortDot",
            opacity = 0.8
            #tooltip = list(valuePrefix = "<")
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_title(text = HTML(glue::glue("<b style = 'font-size: 16px'> {infos_indicador()$nome_incompletude1} </b>"))) |>
          highcharter::hc_colors(cols)
      } else {
        if (input$variavel_incompletude == "escolha1") {
          data_grafico_incompletude <- data_grafico_incompletude1()
        } else {
          data_grafico_incompletude <- data_grafico_incompletude2()
        }
        if (infos_indicador()$bloco != "bloco6") {
          highcharter::highchart() |>
            highcharter::hc_add_series(
              data = data_grafico_incompletude,
              type = "line",
              highcharter::hcaes(x = ano, y = proporcao, group = localidade, colour = localidade)
            ) |>
            highcharter::hc_add_series(
              data = data_referencia_incompletude(),
              type = "line",
              highcharter::hcaes(x = ano, y = excelente, group = class_excelente, colour = class_excelente),
              dashStyle = "ShortDot",
              opacity = 0.8
              #tooltip = list(valuePrefix = "<")
            ) |>
            highcharter::hc_add_series(
              data = data_referencia_incompletude(),
              type = "line",
              highcharter::hcaes(x = ano, y = bom, group = class_bom, colour = class_bom),
              dashStyle = "ShortDot",
              opacity = 0.8
              #tooltip = list(valuePrefix = "<")
            ) |>
            highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
            highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
            highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
            highcharter::hc_title(
              text = HTML(
                glue::glue("<b style = 'font-size: 16px'> {dplyr::if_else(input$variavel_incompletude == 'escolha1', infos_indicador()$nome_incompletude1, infos_indicador()$nome_incompletude2)}</b>")
              )
            ) |>
            highcharter::hc_colors(cols)
        } else if (infos_indicador()$bloco == "bloco6") {
          if (input$variavel_incompletude == "escolha1") {
            highcharter::highchart() |>
              highcharter::hc_add_series(
                data = data_grafico_incompletude,
                type = "line",
                highcharter::hcaes(x = ano, y = proporcao, group = localidade, colour = localidade)
              ) |>
              highcharter::hc_add_series(
                data = data_referencia_incompletude(),
                type = "line",
                highcharter::hcaes(x = ano, y = ideal_mif, group = class, colour = class),
                dashStyle = "ShortDot",
                opacity = 0.8
                #tooltip = list(valuePrefix = "<")
              ) |>
              highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
              highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
              highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
              highcharter::hc_title(
                text = HTML(
                  glue::glue("<b style = 'font-size: 16px'> {dplyr::if_else(input$variavel_incompletude == 'escolha1', infos_indicador()$nome_incompletude1, infos_indicador()$nome_incompletude2)}</b>")
                )
              ) |>
              highcharter::hc_colors(cols)
          } else {
            highcharter::highchart() |>
              highcharter::hc_add_series(
                data = data_grafico_incompletude,
                type = "line",
                highcharter::hcaes(x = ano, y = proporcao, group = localidade, colour = localidade)
              ) |>
              highcharter::hc_add_series(
                data = data_referencia_incompletude(),
                type = "line",
                highcharter::hcaes(x = ano, y = ideal_maternos, group = class, colour = class),
                dashStyle = "ShortDot",
                opacity = 0.8
                #tooltip = list(valuePrefix = "<")
              ) |>
              highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
              highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
              highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
              highcharter::hc_title(
                text = HTML(
                  glue::glue("<b style = 'font-size: 16px'> {dplyr::if_else(input$variavel_incompletude == 'escolha1', infos_indicador()$nome_incompletude1, infos_indicador()$nome_incompletude2)}</b>")
                )
              ) |>
              highcharter::hc_colors(cols)
          }
        }
      }

    })


    output$grafico_cobertura <- highcharter::renderHighchart({
      if (infos_indicador()$num_indicadores_incompletude == 0) {
        if (infos_indicador()$nome_abreviado != "porc_sc" & infos_indicador()$nome_abreviado != "tx_abortos_cem_nascidos_vivos_valor_medio") {
          validate("Informações a respeito da cobertura dos sistemas de informação utilizados para a construção deste indicador não estão disponíveis.")
        }
      }
      base <- dplyr::if_else(
        infos_indicador()$bloco == "bloco6",
        dplyr::if_else(infos_indicador()$nome_abreviado == "rmm", input$sistema_cobertura, "SIM"),
        "SINASC"
      )
      validate(
        need(
          filtros()$nivel != "Microrregião de saúde" & filtros()$nivel != "Macrorregião de saúde",
          glue::glue("A cobertura do {base} não está disponível para o nível de análise selecionado.")
        )
      )
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_cobertura(),
          type = "line",
          highcharter::hcaes(x = ano, y = cobertura, group = localidade, colour = localidade)
        ) |>
        highcharter::hc_add_series(
          data = data_referencia_cobertura(),
          type = "line",
          highcharter::hcaes(x = ano, y = referencia, group = localidade, colour = localidade),
          dashStyle = "ShortDot",
          opacity = 0.8
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_title(text = HTML(glue::glue("<b style = 'font-size: 16px'> Cobertura do {base} </b>"))) |>
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
        highcharter::hc_colors(cols)
    })


    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao_infos", anim = TRUE, animType = "fade", time = 0.7)
      if (infos_indicador()$nome_abreviado %in% c("porc_sc", "porc_menor20")) {
        shinyjs::show(id = "mostrar_botao_infos", anim = TRUE, animType = "fade", time = 0.7)
      }
      if (infos_indicador()$bloco != "bloco6") {
        if (infos_indicador()$num_indicadores_incompletude == 1) {
          shinyjs::hide(id = "mostrar_botao", anim = TRUE, animType = "fade", time = 0.7)
          req(any(data_grafico_incompletude1()$proporcao > 5, na.rm = TRUE) | any(data_cobertura()$cobertura < 90, na.rm = TRUE))
          shinyjs::show(id = "mostrar_botao", anim = TRUE, animType = "fade", time = 0.7)
        } else {
          shinyjs::hide(id = "mostrar_botao", anim = TRUE, animType = "fade", time = 0.7)
          req(any(data_grafico_incompletude1()$proporcao > 5, na.rm = TRUE) | any(data_grafico_incompletude2()$proporcao > 5, na.rm = TRUE) | any(data_cobertura()$cobertura < 90, na.rm = TRUE))
          shinyjs::show(id = "mostrar_botao", anim = TRUE, animType = "fade", time = 0.7)
        }
      } else {
        shinyjs::hide(id = "mostrar_botao", anim = TRUE, animType = "fade", time = 0.7)
        req(any(data_grafico_incompletude1()$proporcao < 90, na.rm = TRUE) | any(data_grafico_incompletude2()$proporcao < 100, na.rm = TRUE) | any(data_cobertura()$cobertura < 90, na.rm = TRUE))
        shinyjs::show(id = "mostrar_botao", anim = TRUE, animType = "fade", time = 0.7)
      }
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao_infos, {
      if (infos_indicador()$nome_abreviado == "porc_sc") {
        texto <- "Dados a respeito da cobertura do SINAN não estão disponíveis."
      } else if (infos_indicador()$nome_abreviado == "porc_menor20") {
        texto <- "Este indicador depende, também, do cálculo das estimativas
          populacionais, que se tornam mais imprecisas conforme aumenta o período
          intercensitário."
      }
      shinyalert::shinyalert(
        html = TRUE,
        text = texto,
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


    output$grafico_regioes <- highcharter::renderHighchart({
        proporcoes <- data_grafico_regioes()$proporcao
        regioes <- data_grafico_regioes()$regiao
        df <- data.frame(regioes, proporcoes)

        highcharter::hchart(
          df,
          type = "column",
          name = stringr::str_replace(infos_indicador()$indicador, "Porcentagem", "%"),
          # name = dplyr::case_when(
          #   infos_indicador()$tipo_do_indicador == "porcentagem" ~ "Porcentagem",
          #   infos_indicador()$tipo_do_indicador == "taxa" ~ "Taxa",
          #   infos_indicador()$tipo_do_indicador == "absoluto" ~ "Frequência"
          # ),
          highcharter::hcaes(
            x = regioes,
            y = proporcoes,
            color = c("#FE9F6DFF", "#DE4968FF", "#8C2981FF", "#3B0F70FF", "#000004FF")
          )
        ) |>
          highcharter::hc_tooltip(valueSuffix = dplyr::if_else(infos_indicador()$tipo_do_indicador == "porcentagem", "%", "")) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(
            # text = dplyr::case_when(
            #   infos_indicador()$tipo_do_indicador == "porcentagem" ~ "%",
            #   infos_indicador()$tipo_do_indicador == "taxa" ~ "Taxa",
            #   infos_indicador()$tipo_do_indicador == "absoluto" ~ "Frequência"
            #   )
            text = stringr::str_replace(infos_indicador()$indicador, "Porcentagem", "%")
            ),
            min = 0
          )

    })

    output$grafico_serie <- highcharter::renderHighchart({
      if (!(infos_indicador()$nome_abreviado %in% c("tx_abortos_mil_mulheres_valor_medio", "tx_abortos_cem_nascidos_vivos_valor_medio")) & !(base::startsWith(infos_indicador()$indicador, "Medianas"))) {
        if (infos_indicador()$nome_abreviado == "rmm") {
          grafico_base <- highcharter::highchart() |>
            highcharter::hc_add_series(
              data = data_rmm_corrigida(),
              type = "line",
              highcharter::hcaes(x = ano, y = rmm, group = class, colour = class)
            ) |>
            highcharter::hc_tooltip(valueSuffix = dplyr::if_else(infos_indicador()$tipo_do_indicador == "porcentagem", "%", ""), shared = TRUE, sort = TRUE) |>
            highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
            highcharter::hc_yAxis(
              title = list(
                text = stringr::str_replace(infos_indicador()$indicador, "Porcentagem", "%")
              ),
              min = 0
            ) |>
            highcharter::hc_colors(cols)
        } else {
          grafico_base <- highcharter::highchart() |>
            highcharter::hc_add_series(
              data = data_grafico_serie(),
              type = "line",
              highcharter::hcaes(x = ano, y = proporcao, group = class, colour = class)
            ) |>
            highcharter::hc_tooltip(valueSuffix = dplyr::if_else(infos_indicador()$tipo_do_indicador == "porcentagem", "%", ""), shared = TRUE, sort = TRUE) |>
            highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
            highcharter::hc_yAxis(
              title = list(
                text = stringr::str_replace(infos_indicador()$indicador, "Porcentagem", "%")
              ),
              min = 0
            ) |>
            highcharter::hc_colors(cols)
        }
      } else {
        if (infos_indicador()$nome_abreviado == "tx_abortos_mil_mulheres_valor_medio") {
          grafico_base <- highcharter::highchart() |>
            highcharter::hc_add_series(
              data = data_grafico_serie() |> dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio, group = class, colour = class),
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_mil_mulheres_lim_inf:,f} - {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
              ),
              color = '#2c115f'
            ) |>
            highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
            highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
            highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0)
        } else if (infos_indicador()$nome_abreviado == "tx_abortos_cem_nascidos_vivos_valor_medio") {
          grafico_base <- highcharter::highchart() |>
            highcharter::hc_add_series(
              data = data_grafico_serie() |> dplyr::filter(ano >= 2015),
              type = "line",
              highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio, group = class, colour = class),
              color = '#2c115f',
              tooltip = list(
                pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} - {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
              )
            ) |>
            highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
            highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
            highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0)
        } else if (base::startsWith(infos_indicador()$indicador, "Medianas")) {
          grafico_base <- highcharter::highchart() |>
            highcharter::hc_add_series(
              name = "Total de partos",
              data = data_grafico_serie(),
              type = "line",
              highcharter::hcaes(x = ano, y = km_total),
              legendIndex = 1,
              index = 1
            ) |>
            highcharter::hc_add_series(
              name = "Serviços de baixa complexidade",
              data = data_grafico_serie(),
              type = "line",
              highcharter::hcaes(x = ano, y = km_baixa_complexidade),
              legendIndex = 2,
              index = 2
            ) |>
            highcharter::hc_add_series(
              name = "Serviços de alta complexidade",
              data = data_grafico_serie(),
              type = "line",
              highcharter::hcaes(x = ano, y = km_alta_complexidade),
              legendIndex = 3,
              index = 3
            ) |>
            highcharter::hc_tooltip(valueSuffix = " km", shared = TRUE, sort = TRUE, valueDecimals = 2) |>
            highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
            highcharter::hc_yAxis(title = list(text = "km"), min = 0) |>
            highcharter::hc_colors(cols)
        }
      }
      if ((filtros()$nivel == "Nacional" & infos_indicador()$descricao_referencia == "média nacional") | (infos_indicador()$nome_abreviado == "obitos_mat_totais") | (base::startsWith(infos_indicador()$indicador, "Medianas"))) {
        grafico_base
      } else {
        if (infos_indicador()$nome_abreviado != "porc_baixo_peso") {
          if (infos_indicador()$nome_abreviado == "prop_robson2_tx_cesariana") {
            grafico_base |> highcharter::hc_add_series(
              data = data_referencia(),
              name = "Referência (meta OMS)",
              highcharter::hcaes(x = ano, low = 20, high = 35),
              type = "arearange",
              dashStyle = "ShortDot",
              color = "#721f81",
              fillOpacity = 0.2,
              enableMouseTracking = TRUE
            )
          } else if (infos_indicador()$nome_abreviado == "prop_robson5_tx_cesariana") {
            grafico_base |> highcharter::hc_add_series(
              data = data_referencia(),
              name = "Referência (meta OMS)",
              highcharter::hcaes(x = ano, low = 50, high = 60),
              type = "arearange",
              dashStyle = "ShortDot",
              color = "#721f81",
              fillOpacity = 0.2,
              enableMouseTracking = TRUE
            )
          } else if (infos_indicador()$nome_abreviado == "tx_abortos_mil_mulheres_valor_medio") {
            grafico_base |>
              highcharter::hc_add_series(
                name = "Referência (média nacional)",
                data = data_referencia() |> dplyr::filter(ano >= 2015),
                type = "line",
                highcharter::hcaes(x = ano, y = tx_abortos_mil_mulheres_valor_medio),
                tooltip = list(
                  pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_mil_mulheres_lim_inf:,f} - {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>"
                ),
                dashStyle = "ShortDot",
                color = "#721F81FF",
                opacity = 0.8
              )
          } else if (infos_indicador()$nome_abreviado == "tx_abortos_cem_nascidos_vivos_valor_medio") {
            grafico_base |>
              highcharter::hc_add_series(
                name = "Referência (média nacional)",
                data = data_referencia() |> dplyr::filter(ano >= 2015),
                type = "line",
                highcharter::hcaes(x = ano, y = tx_abortos_cem_nascidos_vivos_valor_medio),
                tooltip = list(
                  pointFormat = "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} ({point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} - {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>"
                ),
                dashStyle = "ShortDot",
                color = "#721F81FF",
                opacity = 0.8
              )
          } else {
            grafico_base |>
              highcharter::hc_add_series(
                data = data_referencia(),
                type = "line",
                highcharter::hcaes(x = ano, y = proporcao, group = class, colour = class),
                name = glue::glue("Referência ({infos_indicador()$descricao_referencia})"),
                dashStyle = "ShortDot",
                opacity = 0.8
              )
          }

        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data_referencia_baixo_peso(),
              type = "line",
              highcharter::hcaes(x = ano, y = porc_baixo_peso, group = class, colour = class),
              name = "Referência para a localidade (meta de redução global)",
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }

      }
    })

    output$tabela1 <- reactable::renderReactable({
      proporcao_geral <- function(numerador, denominador, fator, tipo_do_indicador) {
        reactable::JS(
          paste0(
            "function(values, rows) {
              var numerator = 0
              var denominator = 0

              rows.forEach(function(row, index) {
                numerator += row['", numerador, "']
                denominator += row['", denominador, "']
              })

              if ('", fator, "' == 1000) {
                return numerator / denominator * 1000
              } else if ('", fator, "' == 100000) {
                return numerator / denominator * 100000
              } else if ('", fator, "' == 100 && '", tipo_do_indicador, "' != 'porcentagem') {
                return numerator / denominator * 100
              } else {
                return numerator / denominator
              }
            }"
          )
        )
      }

      if (infos_indicador()$nome_abreviado != "obitos_mat_totais") {
        data_tabela1() |>
          reactable::reactable(
            defaultColDef = reactable::colDef(
              minWidth = 60,
              footerStyle = list(fontWeight = "bold"),
              align = "center"
            ),
            groupBy = c("uf", "macro_r_saude"),
            columns = list(
              uf = reactable::colDef(
                name = "UF",
                aggregate = reactable::JS("function() { return ''}"),
                format = list(aggregated = reactable::colFormat(prefix = "Todas"))
              ),
              macro_r_saude = reactable::colDef(
                name = "Macrorregião de saúde",
                aggregate = reactable::JS("function() { return ''}"),
                format = list(aggregated = reactable::colFormat(prefix = "Todas"))
              ),
              municipio = reactable::colDef(
                name = "Município",
                aggregate = reactable::JS("function() { return ''}"),
                format = list(aggregated = reactable::colFormat(prefix = "Todos"))
              ),
              numerador = reactable::colDef(
                name = infos_indicador()$nome_numerador,
                aggregate = "sum",
                format = reactable::colFormat(
                  digits = dplyr::if_else(infos_indicador()$numerador == "media_cobertura_esf", true = 2, false = 0)
                )
              ),
              denominador = reactable::colDef(
                name = infos_indicador()$nome_denominador,
                aggregate = "sum",
                format = reactable::colFormat(
                  digits = 0
                )
              ),
              proporcao = reactable::colDef(
                name = dplyr::if_else(
                  !(infos_indicador()$nome_abreviado %in% c("tx_abortos_mil_mulheres_valor_medio", "tx_abortos_cem_nascidos_vivos_valor_medio")),
                  infos_indicador()$indicador,
                  paste("Valor médio da", tolower(infos_indicador()$indicador))
                  ),
                aggregate = proporcao_geral("numerador", "denominador", infos_indicador()$fator, infos_indicador()$tipo_do_indicador),
                format = reactable::colFormat(
                  digits = 1,
                  percent = dplyr::if_else(infos_indicador()$tipo_do_indicador == "porcentagem", true = TRUE, false = FALSE)
                )
              )
            ),
            sortable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            striped = TRUE,
            bordered = FALSE,
            pagination = FALSE,
            height = 730,
            rowStyle = reactable::JS(
              "function(rowInfo) {
              if (rowInfo.aggregated === true) {
                return { fontWeight: 'bold' }
              }
            }"
            )
          )
      } else {
        data_tabela1() |>
          reactable::reactable(
            defaultColDef = reactable::colDef(
              minWidth = 60,
              footerStyle = list(fontWeight = "bold"),
              align = "center"
            ),
            groupBy = c("uf", "macro_r_saude"),
            columns = list(
              uf = reactable::colDef(
                name = "UF",
                aggregate = reactable::JS("function() { return ''}"),
                format = list(aggregated = reactable::colFormat(prefix = "Todas"))
              ),
              macro_r_saude = reactable::colDef(
                name = "Macrorregião de saúde",
                aggregate = reactable::JS("function() { return ''}"),
                format = list(aggregated = reactable::colFormat(prefix = "Todas"))
              ),
              municipio = reactable::colDef(
                name = "Município",
                aggregate = reactable::JS("function() { return ''}"),
                format = list(aggregated = reactable::colFormat(prefix = "Todos"))
              ),
              obitos_mat_totais = reactable::colDef(
                name = "Óbitos maternos",
                aggregate = "sum"
              )
            ),
            sortable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            striped = TRUE,
            bordered = FALSE,
            pagination = FALSE,
            height = 730,
            rowStyle = reactable::JS(
              "function(rowInfo) {
              if (rowInfo.aggregated === true) {
                return { fontWeight: 'bold' }
              }
            }"
            )
          )
      }


    })


  })
}



## To be copied in the UI
# mod_nivel_3_ui("nivel_3_1")

## To be copied in the server
# mod_nivel_3_server("nivel_3_1")
