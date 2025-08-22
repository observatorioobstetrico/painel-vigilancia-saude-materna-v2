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
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(class = "fonte-titulos-pagina", tags$b(HTML("Visão detalhada dos indicadores")), htmlOutput(ns("titulo_localidade"), inline = TRUE), class = 'fonte-titulos-pagina', style = "padding-left: 0.4em;"),
      hr(style = "margin-bottom: 0px;")
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
              "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
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
                  style = "height: 480px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b class = 'fonte-muito-grande'> Resumo da qualidade da informação &nbsp;</b>"),
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
              tags$style(HTML(".html-widget.gauge svg {height: 230px;}"))
            ),
            fluidRow(
              column(
                width = 12,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: hidden",
                  conditionalPanel(
                    style = "height: 15%; display: flex; align-items: center;",
                    ns = ns,
                    condition = "output.bloco_selecionado != 'bloco6' & (output.num_indicadores_incompletude == '0' | output.num_indicadores_incompletude == '1')",
                    HTML("<b class = 'fonte-muito-grande'> Incompletude da informação </b>")
                  ),
                  conditionalPanel(
                    style = "height: 20%;",
                    ns = ns,
                    condition = "output.num_indicadores_incompletude == '2' | output.num_indicadores_incompletude == '3'",
                    conditionalPanel(
                      ns = ns,
                      condition = "output.bloco_selecionado != 'bloco6'",
                      HTML("<b class = 'fonte-muito-grande'> Incompletude da informação </b>")
                    ),
                    conditionalPanel(
                      ns = ns,
                      condition = "output.bloco_selecionado == 'bloco6'",
                      HTML("<b class = 'fonte-muito-grande'> Percentual de óbitos investigados </b>")
                    ),
                    hr(),
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("variavel_incompletude"),
                        label = "Selecione a variável de incompletude de interesse:",
                        options = list(placeholder = "Selecione uma variável de incompletude de interesse"),
                        choices = NULL,
                        width = "95%"
                      )
                    ),
                    br(),
                    br()
                  ),
                  hr(),
                  br(),
                  highcharter::highchartOutput(ns("grafico_incompletude"))
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
                  style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  conditionalPanel(
                    style = "height: 15%; display: flex; align-items: center;",
                    ns = ns,
                    condition = "output.nome_abreviado != 'rmm'",
                    HTML("<b class = 'fonte-muito-grande'> Cobertura dos sistemas de informação &nbsp; </b>")
                  ),
                  conditionalPanel(
                    style = "height: 18%;",
                    ns = ns,
                    condition = "output.nome_abreviado == 'rmm'",
                    div(
                      HTML(
                        "<b class = 'fonte-muito-grande'> Cobertura dos sistemas de informação &nbsp;</b>"
                      )
                    ),
                    br(),
                    div(
                      style = "display: flex; justify-content: center;",
                      shinyWidgets::prettyRadioButtons(
                        inputId = ns("sistema_cobertura"),
                        label = NULL,
                        inline = TRUE,
                        choices = c("SIM", "SINASC"),
                        icon = icon("check", style = "background-color: #007bff;"),
                        animation = "rotate"
                      )
                    )
                  ),
                  hr(),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("grafico_cobertura")))
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  ns = ns,
                  condition = "output.bloco7_selecionado == 'bloco7_fetal'",
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow: hidden",
                    div(
                      style = "height: 15%; display: flex; align-items: center;",
                      HTML("<b class = 'fonte-muito-grande'> Porcentagem de óbitos fetais preenchidos com garbage codes </b>")
                    ),
                    hr(),
                    div(
                      style = "overflow: auto",
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("porcentagem_garbage_fetal")))
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  ns = ns,
                  condition = "output.bloco7_selecionado == 'bloco7_perinatal'",
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow: hidden",
                    div(
                      style = "height: 15%; display: flex; align-items: center;",
                      HTML("<b class = 'fonte-muito-grande'> Porcentagem de óbitos perinatais preenchidos com garbage codes </b>")
                    ),
                    hr(),
                    div(
                      style = "overflow: auto",
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("porcentagem_garbage_perinatal")))
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  ns = ns,
                  condition = "output.bloco7_selecionado == 'bloco7_neonatal'",
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow: hidden",
                    div(
                      style = "height: 15%; display: flex; align-items: center;",
                      HTML("<b class = 'fonte-muito-grande'> Porcentagem de óbitos neonatais preenchidos com garbage codes </b>")
                    ),
                    hr(),
                    div(
                      style = "overflow: auto",
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("porcentagem_garbage_neonatal")))
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  ns = ns,
                  condition = "output.bloco_selecionado == 'bloco6'",
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow: hidden",
                    div(
                      style = "height: 15%; display: flex; align-items: center;",
                      HTML("<b class = 'fonte-muito-grande'> Porcentagem de óbitos maternos preenchidos com garbage codes </b>")
                    ),
                    hr(),
                    div(
                      style = "overflow: auto",
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("porcentagem_garbage_materno")))
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  ns = ns,
                  condition = "output.bloco_selecionado == 'bloco6'",
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow: hidden",
                    div(
                      style = "height: 8%; display: flex; align-items: center;",
                      HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos garbage codes nos óbitos maternos </b>")
                    ),
                    hr(),
                    div(
                      style = "overflow: auto",
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_materno"), height = 600))
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  ns = ns,
                  condition = "output.bloco7_selecionado == 'bloco7_fetal'",
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow: hidden",
                    div(
                      style = "height: 8%; display: flex; align-items: center;",
                      HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos garbage codes nos óbitos fetais </b>")
                    ),
                    hr(),
                    div(
                      style = "overflow: auto",
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_fetal"), height = 600))
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  ns = ns,
                  condition = "output.bloco7_selecionado == 'bloco7_perinatal'",
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow: hidden",
                    div(
                      style = "height: 8%; display: flex; align-items: center;",
                      HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos garbage codes nos óbitos perinatais </b>")
                    ),
                    hr(),
                    div(
                      style = "overflow: auto",
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_perinatal"), height = 600))
                    )
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                conditionalPanel(
                  ns = ns,
                  condition = "output.bloco7_selecionado == 'bloco7_neonatal'",
                  bs4Dash::bs4Card(
                    width = 12,
                    status = "primary",
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow: hidden",
                    div(
                      style = "height: 8%; display: flex; align-items: center;",
                      HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos garbage codes nos óbitos neonatais </b>")
                    ),
                    hr(),
                    div(
                      style = "overflow: auto",
                      shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_neonatal"), height = 600))
                    )
                  )
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
                    "<b class = 'fonte-muito-grande'> Distribuição do indicador por região do país em todo o período &nbsp;</b>"
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
                    "<b class = 'fonte-muito-grande'> Evolução do indicador ao longo do período &nbsp;</b>"
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
                id = "tabela",
                uiOutput(ns("css_tabela")),
                div(
                  br(),
                  fluidRow(
                    column(
                      width = 10,
                      HTML(
                        "<b class = 'fonte-muito-grande'> Distribuição do indicador por UF, macrorregião de saúde estadual e município em todo o período &nbsp;</b>"
                      )
                    ),
                    column(
                      width = 2,
                      downloadButton(ns("download_tabela"), " Download")
                    )
                  ),
                  br()
                ),
                fluidRow(
                  column(
                    width = 12,
                    hr(),
                    br(),
                    div(
                      style = "display: flex; justify-content: center;",
                      shinyWidgets::prettyRadioButtons(
                        inputId = ns("opcoes_tab1"),
                        label = NULL,
                        inline = TRUE,
                        choiceNames = list(
                          HTML("<span> Mostrar informações referentes a todo o país </span>"),
                          HTML("<span> Mostrar informações referentes apenas à localidade escolhida </span>")
                        ),
                        choiceValues = list("escolha1", "escolha2"),
                        selected = "escolha2",
                        icon = icon("check", style = "background-color: #007bff;"),
                        animation = "rotate"
                      )
                    )
                  )
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela1")), proxy.height = "855px")
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
mod_nivel_3_server <- function(id, filtros, titulo_localidade_aux){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Objetos auxiliares ------------------------------------------------------
    ## Buscando as informações do indicador selecionado na tabela_indicadores --------
    indicadores_caixinha_adicional_bloco5 <- c(
      "Porcentagem de nascidos vivos com baixo peso ao nascer",
      "Porcentagem de nascidos vivos prematuros"
    )

    indicadores_uma_caixinha_adicional_bloco7 <- c(
      "Número de óbitos neonatais",
      "Taxa de mortalidade neonatal por 1000 nascidos vivos",
      "Taxa de mortalidade neonatal precoce (0 a 6 dias) por 1000 nascidos vivos",
      "Taxa de mortalidade neonatal tardia (7 a 27 dias) por 1000 nascidos vivos",
      "Número de óbitos perinatais",
      "Taxa de mortalidade perinatal por 1000 nascidos vivos",
      "Taxa de mortalidade neonatal por 1000 nascidos vivos ",
      "Taxa de mortalidade neonatal precoce (0 a 6 dias) por 1000 nascidos vivos  ",
      "Taxa de mortalidade neonatal tardia (7 a 27 dias) por 1000 nascidos vivos  ",
      "Porcentagem de óbitos fetais por grupos de causas evitáveis",
      "Porcentagem de óbitos perinatais por grupos de causas evitáveis",
      "Porcentagem de óbitos neonatais por grupos de causas evitáveis",
      "Porcentagem de óbitos fetais por grupos de causas",
      "Porcentagem de óbitos perinatais por grupos de causas",
      "Porcentagem de óbitos neonatais por grupos de causas",
      "Porcentagem de internações neonatais por grupos de causas",

      ## Bloco4
      "Porcentagem de nascidos vivos segundo local de ocorrência do parto",
      "Medianas de deslocamento segundo o local de ocorrência do parto",
      #"Porcentagem de partos com peso < 1500g segundo local de ocorrência do parto"

      "Porcentagem de nascidos vivos de partos por local de ocorrência",
      "Porcentagem de nascicidos vivos de partos ocorridos em hospital por profissional de assistência"
    )

    indicadores_duas_caixinhas_adicionais_bloco7 <- c(
      "Número de óbitos fetais",
      "Taxa de mortalidade fetal por 1000 nascidos vivos",
      "Porcentagem de internações neonatais (até o 27º dia de vida) em relação ao total de partos no SUS",
      "Porcentagem de internações neonatais (até o 27º dia de vida) em UTI em relação ao total de partos no SUS",

      ## Bloco4
      "Porcentagem de partos com peso < 1500g segundo região de ocorrência e disponibilidade de pelo menos quatro leitos de UTI neonatal"
    )

    infos_indicador <- reactive({
      if (filtros()$bloco %in% c("bloco4", "bloco6")) {
        if (filtros()$indicador_blocos4_6_7 %in% indicadores_uma_caixinha_adicional_bloco7) {
          nome_abreviado <- tabela_indicadores |> dplyr::filter(indicador == filtros()$indicador_blocos4_6_7) |> dplyr::pull(nome_abreviado)

          tabela_indicadores |>
            dplyr::filter(
              nome_abreviado == !!glue::glue("{nome_abreviado}_{filtros()$indicador_uma_caixinha_adicional_bloco7}")
            )
        } else if (filtros()$indicador_blocos4_6_7 %in% indicadores_duas_caixinhas_adicionais_bloco7) {
          nome_abreviado <- tabela_indicadores |> dplyr::filter(trimws(indicador) == filtros()$indicador_blocos4_6_7) |> dplyr::pull(nome_abreviado)

          tabela_indicadores |>
            dplyr::filter(
              nome_abreviado == !!glue::glue("{nome_abreviado}_{filtros()$indicador_duas_caixinhas_adicionais1}_{filtros()$indicador_duas_caixinhas_adicionais2}")
            ) |>
            dplyr::mutate(indicador = trimws(indicador))

        } else {
          tabela_indicadores |>
            dplyr::filter(indicador == filtros()$indicador_blocos4_6_7)
        }

      } else if (filtros()$bloco == "bloco5") {
        if (filtros()$indicador %in% indicadores_caixinha_adicional_bloco5) {
          nome_abreviado <- tabela_indicadores |> dplyr::filter(indicador == filtros()$indicador) |> dplyr::pull(nome_abreviado)

          tabela_indicadores |>
            dplyr::filter(
              nome_abreviado == !!glue::glue("{nome_abreviado}_{filtros()$indicador_uma_caixinha_adicional_bloco5}")
            )
        } else {
          tabela_indicadores |>
            dplyr::filter(indicador == filtros()$indicador)
        }

      } else if (filtros()$bloco == "bloco7") {
        if (filtros()$indicador_blocos4_6_7 %in% indicadores_uma_caixinha_adicional_bloco7) {
          nome_abreviado <- tabela_indicadores |> dplyr::filter(trimws(indicador) == filtros()$indicador_blocos4_6_7) |> dplyr::pull(nome_abreviado)

          tabela_indicadores |>
            dplyr::filter(
              nome_abreviado == !!glue::glue("{nome_abreviado}_{filtros()$indicador_uma_caixinha_adicional_bloco7}")
            ) |>
            dplyr::mutate(indicador = trimws(indicador))

        } else if (filtros()$indicador_blocos4_6_7 %in% indicadores_duas_caixinhas_adicionais_bloco7) {
          nome_abreviado <- tabela_indicadores |> dplyr::filter(trimws(indicador) == filtros()$indicador_blocos4_6_7) |> dplyr::pull(nome_abreviado)

            tabela_indicadores |>
              dplyr::filter(
                nome_abreviado == !!glue::glue("{nome_abreviado}_{filtros()$indicador_duas_caixinhas_adicionais1}_{filtros()$indicador_duas_caixinhas_adicionais2}")
              ) |>
              dplyr::mutate(indicador = trimws(indicador))

        } else {
          tabela_indicadores |>
            dplyr::filter(trimws(indicador) == filtros()$indicador_blocos4_6_7) |>
            dplyr::mutate(indicador = trimws(indicador))
        }

      } else {
        tabela_indicadores |>
          dplyr::filter(indicador == filtros()$indicador)
      }

    })

    base_bloco_selecionado <- reactive({
      if (infos_indicador()$bloco %in% c("bloco4_deslocamento_parto", "bloco4_deslocamento_medianas")) {
        if (filtros()$nivel != "estadual") {
          bloco4_deslocamento_muni
        } else {
          bloco4_deslocamento_uf
        }
      } else if (infos_indicador()$bloco == "bloco4_deslocamento_macro") {
        bloco4_deslocamento_macrorregiao
      } else if (infos_indicador()$bloco == "bloco4_profissional") {
        bloco4_profissional
      } else if (infos_indicador()$bloco == "bloco7_neonatal_evitaveis") {
        bloco8_grafico_evitaveis_neonatal
      } else if(infos_indicador()$bloco == "bloco7_morbidade_neonatal_grupos"){
        bloco7_dist_morbidade
      } else if (grepl("evitaveis|grupo", infos_indicador()$nome_abreviado)) {
        bloco7_distribuicao_cids
      } else {
        get(filtros()$bloco)
      }
    })

    ## Criando o output que recebe o título da página --------------------------
    output$titulo_localidade <- renderUI({
      if (length(filtros()$ano2[1]:filtros()$ano2[2]) > 1) {
        ano <- glue::glue("{filtros()$ano2[1]} a {filtros()$ano2[2]}")
      } else {
        ano <- filtros()$ano2[1]
      }

      local1 <- dplyr::case_when(
        filtros()$nivel == "nacional" ~ "Brasil",
        filtros()$nivel == "regional" ~ filtros()$regiao,
        filtros()$nivel == "estadual" ~ filtros()$estado,
        filtros()$nivel == "macro" ~ filtros()$macro,
        filtros()$nivel == "micro" ~ filtros()$micro,
        filtros()$nivel == "municipal" ~ filtros()$municipio
      )

      tags$b(paste("-", infos_indicador()$indicador, glue::glue("({local1}, {ano})")), class = "fonte-titulos-pagina")
    })

    ## Criando alguns outputs de texto que precisam ser usados na UI ----------
    output$indicador <- renderUI(HTML(glue::glue("<p class = 'fonte-indicador-nivel3'> <b>Indicador: </b> {filtros()$indicador}")))

    output$nome_abreviado <- renderText(infos_indicador()$nome_abreviado)

    output$bloco_selecionado <- renderText({infos_indicador()$bloco})

    output$bloco7_selecionado <- renderText({
      dplyr::case_when(
        grepl("bloco7_fetal", infos_indicador()$bloco) ~ "bloco7_fetal",
        grepl("bloco7_perinatal", infos_indicador()$bloco) ~ "bloco7_perinatal",
        grepl("bloco7_morbidade_neonatal", infos_indicador()$bloco) ~ "bloco7_morbidade_neonatal",
        grepl("bloco7_neonatal", infos_indicador()$bloco) ~ "bloco7_neonatal",
        TRUE ~ "NA"

      )
    })

    output$num_indicadores_incompletude <- renderText({infos_indicador()$num_indicadores_incompletude})

    outputOptions(output, "nome_abreviado", suspendWhenHidden = FALSE)
    outputOptions(output, "bloco_selecionado", suspendWhenHidden = FALSE)
    outputOptions(output, "bloco7_selecionado", suspendWhenHidden = FALSE)
    outputOptions(output, "num_indicadores_incompletude", suspendWhenHidden = FALSE)

    ## Buscando a documentação do indicador selecionado -----------------------
    output$documentacao <- renderUI({
      includeHTML(glue::glue("inst/app/www/html/documentacao_nivel3/{substr(infos_indicador()$bloco, start = 1, stop = 6)}/{infos_indicador()$nome_documentacao}.html"))
    })

    ## Criando vetores que recebem os indicadores que só estão disponíveis a partir de ou até certos anos --------
    indicadores_2014 <- c(
      "cobertura_pre_natal",
      "porc_7",
      "porc_consultas_adequadas",
      tabela_indicadores$nome_abreviado[tabela_indicadores$bloco == "bloco4"][-1]
    )

    indicadores_2015 <- tabela_indicadores$nome_abreviado[grep("abortos|ans", tabela_indicadores$nome_abreviado)]

    indicadores_2024 <- c(
      "porc_sc",
      tabela_indicadores$nome_abreviado[grep("abortos", tabela_indicadores$nome_abreviado)],
      "sus_tx_abortos_cem_nascidos_vivos_valor_medio",
      "ans_tx_abortos_cem_nascidos_vivos_valor_medio"
    )

    ## Criando um vetor com a lista de anos disponíveis para o indicador selecionado --------
    anos_disponiveis <- reactive({
      if (infos_indicador()$nome_abreviado %in% indicadores_2014) {
        anos_disponiveis_aux <- max(2014, filtros()$ano2[1]):filtros()$ano2[2]
      } else if (infos_indicador()$nome_abreviado %in% indicadores_2015) {
        anos_disponiveis_aux <- max(2015, filtros()$ano2[1]):filtros()$ano2[2]
      } else {
        anos_disponiveis_aux <- filtros()$ano2[1]:filtros()$ano2[2]
      }

      if (infos_indicador()$nome_abreviado %in% indicadores_2024) {
        anos_disponiveis_aux <- anos_disponiveis_aux[anos_disponiveis_aux <= 2023]
      } else {
        anos_disponiveis_aux
      }
    })

    ## Definindo as cores dos gráficos ----------------------------------------
    cols <- c("#2c115f", "#b73779", "#fc8961")

    # Incompletude e cobertura ------------------------------------------------
    ## Criando os data.frames com os dados de cobertura -----------------------
    data_cobertura <- reactive({
      base_cobertura <- reactive({
        if (infos_indicador()$bloco == "bloco6" | startsWith(infos_indicador()$bloco, "bloco7")) {
          if (infos_indicador()$nome_abreviado == "rmm") {
            get(
              ifelse(
                filtros()$nivel == "municipal",
                glue::glue("sub_registro_{tolower(input$sistema_cobertura)}_muni_2015_2021"),
                glue::glue("sub_registro_{tolower(input$sistema_cobertura)}_uf_regioes_2015_2021")
              )
            )
          } else {
            get(
              ifelse(
                filtros()$nivel == "municipal",
                glue::glue("sub_registro_sim_muni_2015_2021"),
                glue::glue("sub_registro_sim_uf_regioes_2015_2021")
              )
            )
          }
        } else {
          get(
            ifelse(
              filtros()$nivel == "municipal",
              glue::glue("sub_registro_sinasc_muni_2015_2021"),
              glue::glue("sub_registro_sinasc_uf_regioes_2015_2021")
            )
          )
        }
      })

      if (filtros()$nivel %in% c("municipal", "estadual", "regional", "nacional")) {
        base_cobertura() |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            if (filtros()$nivel == "municipal") {
              municipio == filtros()$municipio & uf == filtros()$estado_municipio
            } else {
              if (filtros()$nivel == "estadual") {
                localidade == filtros()$estado
              } else if (filtros()$nivel == "regional") {
                localidade == filtros()$regiao
              } else if (filtros()$nivel == "nacional") {
                localidade == "Brasil"
              }
            }
          ) |>
          dplyr::rename_at(dplyr::vars(dplyr::starts_with("municipio")), ~ "localidade")
      }
    })

    data_referencia_cobertura <- reactive({
      data.frame(
        ano = max(2015, filtros()$ano2[1]):min(2022, filtros()$ano2[2]),
        referencia = 90,
        localidade = "Referência (Ministério da Saúde)"
      )
    })

    ## Criando o gráfico de linhas para a cobertura ---------------------------
    output$grafico_cobertura <- highcharter::renderHighchart({
      base <- dplyr::case_when(
        (infos_indicador()$bloco == "bloco6" & infos_indicador()$nome_abreviado != "rmm") | (startsWith(infos_indicador()$bloco, "bloco7") & !grepl("bloco7_morbidade_neonatal", infos_indicador()$bloco)) ~ "SIM",
        infos_indicador()$nome_abreviado == "rmm" ~ input$sistema_cobertura,
        grepl("bloco7_morbidade_neonatal", infos_indicador()$bloco) ~ "SIH",
        T ~ "SINASC"
      )

      validate(
        need(
          filtros()$nivel != "micro" & filtros()$nivel != "macro",
          glue::glue("A cobertura do {base} não está disponível para o nível de análise selecionado.")
        ),
        need(
          infos_indicador()$num_indicadores_incompletude != 0 | startsWith(infos_indicador()$bloco, "bloco7") |
            startsWith(infos_indicador()$bloco, "bloco5") | (infos_indicador()$nome_abreviado == "porc_sc" |
               grepl("tx_abortos_cem_nascidos_vivos_valor_medio", infos_indicador()$nome_abreviado)),
          "Informações a respeito da cobertura dos sistemas de informação utilizados para a construção deste indicador não estão disponíveis."
        )
      )

      highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
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
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_title(text = HTML(glue::glue("<b class = 'fonte-grande'> Cobertura do {base} </b>"))) |>
        highcharter::hc_xAxis(
          title = list(text = ""),
          categories = anos_disponiveis(),
          allowDecimals = FALSE
        ) |>
        highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
        highcharter::hc_colors(cols)
    })

    ## Criando os data.frames com os dados de incompletude --------------------
    data_grafico_incompletude1 <- reactive({
      if (infos_indicador()$num_indicadores_incompletude != 0) {
        base_incompletude |>
          dplyr::filter(
            ano %in% anos_disponiveis(),
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
            numerador := ifelse(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco),
              sum(.data[[infos_indicador()$numerador_incompletude1]], na.rm = TRUE),
              NA
            ),
            denominador := ifelse(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco),
              sum(.data[[infos_indicador()$denominador_incompletude1]], na.rm = TRUE),
              NA
            ),
            proporcao = dplyr::case_when(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco) ~ round(numerador/denominador * {infos_indicador()$fator_incompletude}, 1),
              infos_indicador()$bloco == "bloco6" ~ round((sum(obito_mif_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_mif_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_mulher_idade_fertil, na.rm = TRUE) * 100, 1),
              grepl("deslocamento", infos_indicador()$bloco) ~ round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_preenchido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 1)
            ),
            localidade = dplyr::case_when(
              filtros()$nivel == "nacional" ~ "Brasil",
              filtros()$nivel == "regional" ~ filtros()$regiao,
              filtros()$nivel == "estadual" ~ filtros()$estado,
              filtros()$nivel == "macro" ~ filtros()$macro,
              filtros()$nivel == "micro" ~ filtros()$micro,
              filtros()$nivel == "municipal" ~ filtros()$municipio
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(ano <= ifelse(infos_indicador()$bloco == "bloco6" | grepl("bloco4_deslocamento", infos_indicador()$bloco), 2020, 2023))
      }
    })

    data_grafico_incompletude2 <- reactive({
      if (infos_indicador()$num_indicadores_incompletude >= 2) {
        base_incompletude |>
          dplyr::filter(
            ano %in% anos_disponiveis(),
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
            numerador := ifelse(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco),
              sum(.data[[infos_indicador()$numerador_incompletude2]], na.rm = TRUE),
              NA
            ),
            denominador := ifelse(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco),
              sum(.data[[infos_indicador()$denominador_incompletude2]], na.rm = TRUE),
              NA
            ),
            proporcao = dplyr::case_when(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco) ~ round(numerador/denominador * {infos_indicador()$fator_incompletude}, 1),
              infos_indicador()$bloco == "bloco6" ~ round((sum(obito_materno_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_materno_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_maternos, na.rm = TRUE) * 100, 1),
              grepl("deslocamento", infos_indicador()$bloco) ~ round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_valido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 1)
            ),
            localidade = dplyr::case_when(
              filtros()$nivel == "nacional" ~ "Brasil",
              filtros()$nivel == "regional" ~ filtros()$regiao,
              filtros()$nivel == "estadual" ~ filtros()$estado,
              filtros()$nivel == "macro" ~ filtros()$macro,
              filtros()$nivel == "micro" ~ filtros()$micro,
              filtros()$nivel == "municipal" ~ filtros()$municipio
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(ano <= ifelse(infos_indicador()$bloco == "bloco6" | grepl("bloco4_deslocamento", infos_indicador()$bloco), 2020, 2023))
      }
    })

    data_grafico_incompletude3 <- reactive({
      if (infos_indicador()$num_indicadores_incompletude == 3) {
        base_incompletude |>
          dplyr::filter(
            ano %in% anos_disponiveis(),
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
            numerador := ifelse(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco),
              sum(.data[[infos_indicador()$numerador_incompletude3]], na.rm = TRUE),
              NA
            ),
            denominador := ifelse(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco),
              sum(.data[[infos_indicador()$denominador_incompletude3]], na.rm = TRUE),
              NA
            ),
            proporcao = dplyr::case_when(
              infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco) ~ round(numerador/denominador * {infos_indicador()$fator_incompletude}, 1),
              infos_indicador()$bloco == "bloco6" ~ round((sum(obito_materno_investigado_com_ficha_sintese, na.rm = TRUE) + sum(obito_materno_investigado_sem_ficha_sintese, na.rm = TRUE))/sum(total_obitos_maternos, na.rm = TRUE) * 100, 1),
              grepl("deslocamento", infos_indicador()$bloco) ~ round((sum(dn_hospital_id_fertil, na.rm = TRUE)-sum(dn_hosp_id_fertil_cnes_valido, na.rm = TRUE))/sum(dn_hospital_id_fertil, na.rm = TRUE) * 100, 1)
            ),
            localidade = dplyr::case_when(
              filtros()$nivel == "nacional" ~ "Brasil",
              filtros()$nivel == "regional" ~ filtros()$regiao,
              filtros()$nivel == "estadual" ~ filtros()$estado,
              filtros()$nivel == "macro" ~ filtros()$macro,
              filtros()$nivel == "micro" ~ filtros()$micro,
              filtros()$nivel == "municipal" ~ filtros()$municipio
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(ano <= ifelse(infos_indicador()$bloco == "bloco6" | grepl("bloco4_deslocamento", infos_indicador()$bloco), 2020, 2023))
      }
    })

    data_referencia_incompletude_blocos_gerais <- reactive({
      data.frame(
        ano = anos_disponiveis(),
        valor = c(rep(10, times = length(anos_disponiveis())), rep(5, times = length(anos_disponiveis()))),
        class = c(rep("Bom", times = length(anos_disponiveis())), rep("Excelente", times = length(anos_disponiveis())))
      ) |>
        dplyr::filter(ano <= ifelse(grepl("bloco4_deslocamento", infos_indicador()$bloco), 2020, 2023))
    })

    data_referencia_incompletude_blocos6 <- reactive({
      data.frame(
        ano = anos_disponiveis(),
        valor = c(rep(90, times = length(anos_disponiveis())), rep(100, times = length(anos_disponiveis()))),
        indicador = c(rep(escolha1(), times = length(anos_disponiveis())), rep(escolha2(), times = length(anos_disponiveis()))),
        class = rep("Ideal", times = length(anos_disponiveis()))
      ) |>
        dplyr::filter(ano <= 2020)
    })

    ## Criando os gráficos de linhas para a incompletude ----------------------
    ### Criando objetos que recebem o nome das variáveis de incompletude, para atualizar o selectizeInput
    escolha1 <- reactive({
      dplyr::case_when(
        infos_indicador()$bloco == "bloco6" ~ "Óbitos de MIF",
        grepl("deslocamento", infos_indicador()$bloco) ~ "DN sem CNES preenchido",
        infos_indicador()$numerador_incompletude1 == "incompletude_fetal_peso_ig" ~ "GESTACAO, SEMAGESTAC e PESO fetal",
        infos_indicador()$numerador_incompletude1 == "incompletude_fetal_peso" ~ "PESO fetal",
        infos_indicador()$numerador_incompletude1 == "incompletude_neonatal_peso" ~ "PESO neonatal",
        infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco)  ~ stringr::str_remove(unlist(strsplit(infos_indicador()$nome_incompletude1, ' '))[4], ',')
      )
    })
    escolha2 <- reactive({
      dplyr::case_when(
        infos_indicador()$numerador_incompletude2 == "incompletude_fetal_peso_ig" ~ "GESTACAO, SEMAGESTAC e PESO fetal",
        infos_indicador()$numerador_incompletude2 == "incompletude_fetal_peso" ~ "PESO fetal",
        infos_indicador()$numerador_incompletude2 == "incompletude_neonatal_peso" ~ "PESO neonatal",
        infos_indicador()$bloco == "bloco6" ~ "Óbitos maternos",
        grepl("deslocamento", infos_indicador()$bloco) ~ "DN com CNES inválido",
        infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco) ~ stringr::str_remove(unlist(strsplit(infos_indicador()$nome_incompletude2, ' '))[4], ',')
      )
    })
    escolha3 <- reactive({
      dplyr::case_when(
        infos_indicador()$numerador_incompletude3 == "incompletude_fetal_peso_ig" ~ "GESTACAO, SEMAGESTAC e PESO fetal",
        infos_indicador()$numerador_incompletude3 == "incompletude_fetal_peso" ~ "PESO fetal",
        infos_indicador()$numerador_incompletude3 == "incompletude_neonatal_peso" ~ "PESO neonatal"
      )
    })

    ### Atualizando as opções do selectizeInput
    observe({
      if (infos_indicador()$num_indicadores_incompletude == 2) {
        updateSelectizeInput(
          inputId = "variavel_incompletude",
          label = ifelse(infos_indicador()$bloco == "bloco6", "Selecione o tipo de óbitos de interesse:", "Selecione a variável de incompletude de interesse:"),
          choices = c(escolha1(), escolha2())
        )
      } else if (infos_indicador()$num_indicadores_incompletude == 3) {
        updateSelectizeInput(
          inputId = "variavel_incompletude",
          choices = c(escolha1(), escolha2(), escolha3())
        )
      }
    })

    ### Criando o gráfico de linhas
    output$grafico_incompletude <- highcharter::renderHighchart({
      validate(
        need(
          !(grepl("bloco7_mortalidade_neonatal", infos_indicador()$bloco)) | grepl("00", infos_indicador()$nome_abreviado),
          "Informações a respeito da incompletude das variáveis não se aplicam a este indicador."
        )
      )
      validate(
        need(
          !(grepl("bloco7_morbidade_neonatal", infos_indicador()$bloco)) | infos_indicador()$nome_abreviado == "porc_condicoes_ameacadoras",
          "Informações a respeito da incompletude das variáveis não se aplicam a este indicador."
        )
      )
      validate(
        need(
          infos_indicador()$num_indicadores_incompletude != 0,
          "Informações a respeito da incompletude das variáveis necessárias para a construção deste indicador não estão disponíveis."
        )
      )

      if (infos_indicador()$num_indicadores_incompletude > 1) {
        req(input$variavel_incompletude)
      }

      #### Decidindo qual dataframe de incompletude usar (para quando há mais de um indicador de incompletude)
      if (infos_indicador()$num_indicadores_incompletude == 1 | input$variavel_incompletude == escolha1()) {
        data_grafico_incompletude <- data_grafico_incompletude1()
      } else if (input$variavel_incompletude == escolha2()) {
        data_grafico_incompletude <- data_grafico_incompletude2()
      } else if (input$variavel_incompletude == escolha3()) {
        data_grafico_incompletude <- data_grafico_incompletude3()
      }

      #### Decidindo qual dataframe de referência de incompletude usar
      if (infos_indicador()$bloco == "bloco6") {
        data_referencia_incompletude <- data_referencia_incompletude_blocos6() |>
          dplyr::filter(indicador == input$variavel_incompletude, ano <= 2020)
      } else {
        data_referencia_incompletude <- data_referencia_incompletude_blocos_gerais()
      }

      #### Criando um objeto com o título do gráfico
      titulo_grafico_incompletude <- dplyr::case_when(
        infos_indicador()$num_indicadores_incompletude == 1 | input$variavel_incompletude == escolha1() ~ infos_indicador()$nome_incompletude1,
        input$variavel_incompletude == escolha2() ~ infos_indicador()$nome_incompletude2,
        input$variavel_incompletude == escolha3() ~ infos_indicador()$nome_incompletude3
      )

      highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_add_series(
          data = data_grafico_incompletude,
          type = "line",
          highcharter::hcaes(x = ano, y = proporcao, group = localidade, colour = localidade)
        ) |>
        highcharter::hc_add_series(
          data = data_referencia_incompletude,
          type = "line",
          highcharter::hcaes(x = ano, y = valor, group = class, colour = class),
          dashStyle = "ShortDot",
          opacity = 0.8
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(
          title = list(text = ""),
          categories = anos_disponiveis(),
          allowDecimals = FALSE
        ) |>
        highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
        highcharter::hc_title(
          text = HTML(
            glue::glue("<b class = 'fonte-grande'>{titulo_grafico_incompletude}</b>")
          )
        ) |>
        highcharter::hc_colors(cols)
    })

    ## Criando o output do velocímetro (gauge) --------------------------------
    output$gauge1 <- renderUI({
      if (infos_indicador()$num_indicadores_incompletude == 0 & !(startsWith(infos_indicador()$bloco, "bloco5")) &
          !(startsWith(infos_indicador()$bloco, "bloco7")) & !(grepl("tx_abortos_cem", infos_indicador()$nome_abreviado)) &
          !(infos_indicador()$nome_abreviado %in% c(indicadores_2014[1:6], "porc_sc"))) {
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
        } else if (grepl("tx_abortos_mil_mulheres_valor_medio", infos_indicador()$nome_abreviado)) {
          div(
            style = "text-align: center; height: 260px; display: flex; align-items:center; justify-content:center; text-align: center;",
            HTML(
              "
              Este indicador depende da cobertura e da qualidade do preenchimento do SIH/SUS e das internações
              hospitalares em serviços do sistema de saúde suplementar, bem como do cálculo das estimativas populacionais, que
              se tornam mais imprecisas conforme aumento o período intercensitário."
            )
          )
        } else if (infos_indicador()$bloco == "bloco6_morbidade_sus") {
          div(
            style = "text-align: center; height: 260px; display: flex; align-items:center; justify-content:center; text-align: center;",
            HTML(
              "
          Este indicador depende da qualidade do preenchimento da Autorização de Internação Hospitalar."
            )
          )
        } else if (infos_indicador()$bloco == "bloco6_morbidade_ans") {
          div(
            style = "text-align: center; height: 260px; display: flex; align-items:center; justify-content:center; text-align: center;",
            HTML(
              "
          Este indicador depende da qualidade do preenchimento das internações hospitalares em serviços do sistema de saúde suplementar."
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
          valor <- round(length(unique(c(anos_incompletude, anos_cobertura)))/length(anos_disponiveis()) * 100)
        } else if (infos_indicador()$num_indicadores_incompletude == 2) {
          if (infos_indicador()$bloco != "bloco6") {
            anos_incompletude1 <- data_grafico_incompletude1()$ano[which(data_grafico_incompletude1()$proporcao > 5)]
            anos_incompletude2 <- data_grafico_incompletude2()$ano[which(data_grafico_incompletude2()$proporcao > 5)]
          } else {
            anos_incompletude1 <- data_grafico_incompletude1()$ano[which(data_grafico_incompletude1()$proporcao < 90)]
            anos_incompletude2 <- data_grafico_incompletude2()$ano[which(data_grafico_incompletude2()$proporcao < 100)]
          }
          anos_cobertura <- data_cobertura()$ano[which(data_cobertura()$cobertura < 90)]
          valor <- round(length(unique(c(anos_incompletude1, anos_incompletude2, anos_cobertura)))/length(anos_disponiveis()) * 100)
        } else if(infos_indicador()$num_indicadores_incompletude == 3){
          anos_incompletude1 <- data_grafico_incompletude1()$ano[which(data_grafico_incompletude1()$proporcao > 5)]
          anos_incompletude2 <- data_grafico_incompletude2()$ano[which(data_grafico_incompletude2()$proporcao > 5)]
          anos_incompletude3 <- data_grafico_incompletude3()$ano[which(data_grafico_incompletude3()$proporcao > 5)]
          anos_cobertura <- data_cobertura()$ano[which(data_cobertura()$cobertura < 90)]
          valor <- round(length(unique(c(anos_incompletude1, anos_incompletude2, anos_incompletude3, anos_cobertura)))/length(anos_disponiveis()) * 100)

        }else if(infos_indicador()$num_indicadores_incompletude == 4){
          anos_incompletude1 <- data_grafico_incompletude1()$ano[which(data_grafico_incompletude1()$proporcao > 5)]
          anos_incompletude2 <- data_grafico_incompletude2()$ano[which(data_grafico_incompletude2()$proporcao > 5)]
          anos_incompletude3 <- data_grafico_incompletude3()$ano[which(data_grafico_incompletude3()$proporcao > 5)]
          anos_incompletude4 <- data_grafico_incompletude3()$ano[which(data_grafico_incompletude3()$proporcao > 5)]

          anos_cobertura <- data_cobertura()$ano[which(data_cobertura()$cobertura < 90)]
          valor <- round(length(unique(c(anos_incompletude1, anos_incompletude2, anos_incompletude3, anos_incompletude4, anos_cobertura)))/length(anos_disponiveis()) * 100)

        } else{
          anos_cobertura <- data_cobertura()$ano[which(data_cobertura()$cobertura < 90)]
          valor <- round(length(anos_cobertura)/length(anos_disponiveis()) * 100)
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
          br(),
          br(),
          HTML("dos anos considerados apresentam problemas de qualidade em alguma das variáveis necessárias para a construção do indicador")
        )
      }
    })

    ## Criando o output do botão de alerta  -----------------------------------
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
        infos_indicador()$numerador_incompletude1 == "idanomal_incompletos" ~ "em branco ou ignorados (IDANOMAL = 9)",
        infos_indicador()$numerador_incompletude1 == "incompletude_fetal_peso_ig" ~ "GESTACAO em branco ou ignorada (GESTACAO=9) e PESO em branco e SEMAGESTAC em branco ou ignorada (SEMAGESTAC == 99)",
        infos_indicador()$numerador_incompletude1 == "incompletude_neonatal_idade" ~ "em branco"
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
        infos_indicador()$numerador_incompletude2 == "semagestac_incompletos" ~ "em branco ou sem informação",
        infos_indicador()$numerador_incompletude2 == "idanomal_incompletos" ~ "em branco ou ignorados (IDANOMAL = 9)",
        infos_indicador()$numerador_incompletude2 == "incompletude_fetal_peso_ig" ~ "GESTACAO em branco ou ignorada (GESTACAO=9) e PESO em branco e SEMAGESTAC em branco ou ignorada (SEMAGESTAC == 99)",
        infos_indicador()$numerador_incompletude2 == "incompletude_fetal_peso" ~ "em branco",
        infos_indicador()$numerador_incompletude2 == "incompletude_fetal_momento" ~ "em branco ou ignorada (OBITOPARTO = 9)",
        infos_indicador()$numerador_incompletude2 == "incompletude_neonatal_peso" ~ "em branco"

      )

      descricao_incompletude3 <- dplyr::case_when(
        infos_indicador()$numerador_incompletude3 == "incompletude_fetal_peso" ~ "em branco",
        infos_indicador()$numerador_incompletude3 == "incompletude_neonatal_peso" ~ "em branco"

      )

      descricao_incompletude4 <- dplyr::case_when(
        infos_indicador()$numerador_incompletude3 == "incompletude_fetal_peso" ~ "em branco"
      )

      cria_modal_incompletude(
        incompletude1 = data_grafico_incompletude1()$proporcao,
        variavel_incompletude1 = ifelse(
          infos_indicador()$numerador_incompletude1 == "parto_tprobson_incompletos",
          "PARTO e TPROBSON",
          ifelse(
            infos_indicador()$numerador_incompletude1 == "incompletude_fetal_peso_ig",
            "GESTACAO, SEMAGESTAC e PESO",
            sub("^([^ ]+ ){3}([^,]+).*", "\\2", infos_indicador()$nome_incompletude1)
          )
        ),
        descricao_incompletude1 = descricao_incompletude1,
        incompletude2 = data_grafico_incompletude2()$proporcao,
        variavel_incompletude2 = ifelse(
          infos_indicador()$numerador_incompletude2 == "incompletude_fetal_peso_ig",
          "GESTACAO, SEMAGESTAC e PESO",
          sub("^([^ ]+ ){3}([^,]+).*", "\\2", infos_indicador()$nome_incompletude2)
        ),
        descricao_incompletude2 = descricao_incompletude2,
        incompletude3 = data_grafico_incompletude3()$proporcao,
        variavel_incompletude3 = ifelse(
          infos_indicador()$numerador_incompletude3 == "incompletude_fetal_peso_ig",
          "GESTACAO, SEMAGESTAC e PESO",
          sub("^([^ ]+ ){3}([^,]+).*", "\\2", infos_indicador()$nome_incompletude3)
        ),
        descricao_incompletude3 = descricao_incompletude3,
        incompletude4 = data_grafico_incompletude3()$proporcao,
        variavel_incompletude4 = ifelse(
          infos_indicador()$numerador_incompletude4 == "incompletude_fetal_peso_ig",
          "GESTACAO, SEMAGESTAC e PESO",
          sub("^([^ ]+ ){3}([^,]+).*", "\\2", infos_indicador()$nome_incompletude4)
        ),
        descricao_incompletude4 = descricao_incompletude4,
        df = data_grafico_incompletude1(),
        cobertura = data_cobertura()$cobertura,
        nivel = 3,
        bloco = dplyr::case_when(
          infos_indicador()$bloco == "bloco6" ~ "bloco6",
          grepl("deslocamento", infos_indicador()$bloco) ~ "deslocamento",
          infos_indicador()$bloco != "bloco6" & !grepl("deslocamento", infos_indicador()$bloco) ~ "geral"
        )
      )
    })

    ## Criando o output do botão de informações -------------------------------
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

    ## Criando os data.frames para a construção do gráfico de garbage codes p/ óbitos maternos --------
    data_filtrada_aux <- reactive({
      base_garbage_code |>
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
        dplyr::group_by(ano)
    })

    data_plot_garbage_materno <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_materno")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_maternos_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_materno")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_materno")), ~ round((. / obitos_maternos_garbage * 100), 1)) |>
        dplyr::select(!obitos_maternos_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_materno"),
          names_to = "cid",
          values_to = "prop_garbage_code"
        ) |>
        dplyr::filter(prop_garbage_code != 0) |>
        dplyr::mutate(
          prop_garbage_code = round(prop_garbage_code, 1),
          cid = toupper(substr(cid, nchar("garbage_materno_") + 1, nchar(cid))),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          )
        )
    })


    data_plot_garbage_materno_referencia <- reactive({
      base_garbage_code |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_materno")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_maternos_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_materno")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_materno")), ~ round((. / obitos_maternos_garbage * 100), 1)) |>
        dplyr::select(!obitos_maternos_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_materno"),
          names_to = "cid",
          values_to = "br_prop_garbage_code"
        ) |>
        dplyr::filter(br_prop_garbage_code != 0) |>
        dplyr::mutate(
          br_prop_garbage_code = round(br_prop_garbage_code, 1),
          cid = toupper(substr(cid, nchar("garbage_materno_") + 1, nchar(cid)))
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          )
        )
    })

    data_plot_garbage_materno_completo <- reactive({
      validate(
        need(
          nrow(data_plot_garbage_materno()) != 0,
          "Não existem ocorrências de óbitos maternos preenchidos com garbage codes para a localidade e períodos selecionados."
        )
      )
      dplyr::left_join(data_plot_garbage_materno(), data_plot_garbage_materno_referencia())
    })

    ## Criando o gráfico da distribuição percentual de garbage codes p/ óbitos maternos -------
    output$plot_garbage_materno <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_materno_completo(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = causabas_subcategoria),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {point.cid}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_garbage_code:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = FALSE, title = list(text = "Garbage code (CID-10)")) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_garbage_materno_completo()$cid)) + 2, direction = 1)[-c(1, length(unique(data_plot_garbage_materno_completo()$cid)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_garbage_materno_completo()$ano), allowDecimals = FALSE, reversed = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos maternos preenchidos com garbage codes"), min = 0, max = 100)
    })


    ## Criando os data.frames para a construção do gráfico de garbage codes p/ óbitos fetais --------
    data_plot_garbage_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_fetal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetal_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_fetal")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_fetal")), ~ round((. / obitos_fetal_garbage * 100), 1)) |>
        dplyr::select(!obitos_fetal_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_fetal"),
          names_to = "cid",
          values_to = "prop_garbage_code_fetal"
        ) |>
        dplyr::filter(prop_garbage_code_fetal != 0) |>
        dplyr::mutate(
          prop_garbage_code = round(prop_garbage_code_fetal, 1),
          cid = toupper(substr(cid, nchar("garbage_fetal_") + 1, nchar(cid))),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          ),
          causabas_subcategoria = ifelse(cid != "OUTROS", causabas_subcategoria, "Outros")
        )
    })


    data_plot_garbage_fetal_referencia <- reactive({
      base_garbage_code |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_fetal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetal_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_fetal")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_fetal")), ~ round((. / obitos_fetal_garbage * 100), 1)) |>
        dplyr::select(!obitos_fetal_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_fetal"),
          names_to = "cid",
          values_to = "br_prop_garbage_code_fetal"
        ) |>
        dplyr::filter(br_prop_garbage_code_fetal != 0) |>
        dplyr::mutate(
          br_prop_garbage_code_fetal = round(br_prop_garbage_code_fetal, 1),
          cid = toupper(substr(cid, nchar("garbage_fetal_") + 1, nchar(cid)))
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          ),
          causabas_subcategoria = ifelse(cid != "OUTROS", causabas_subcategoria, "Outros")
        )
    })


    data_plot_garbage_fetal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_garbage_fetal()) != 0,
          "Não existem ocorrências de óbitos fetais preenchidos com garbage codes para a localidade e períodos selecionados."
        )
      )
      dplyr::left_join(data_plot_garbage_fetal(), data_plot_garbage_fetal_referencia())
    })


    ## Criando o gráfico da distribuição percentual de garbage codes p/ óbitos fetais -------
    output$plot_garbage_fetal <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_fetal_completo(),
          highcharter::hcaes(x = ano, y = prop_garbage_code_fetal, group = causabas_subcategoria),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {point.cid}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_garbage_code_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE, title = list(text = "Garbage code (CID-10)")) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_garbage_fetal_completo()$cid)) + 2, direction = 1)[-c(1, length(unique(data_plot_garbage_fetal_completo()$cid)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_garbage_fetal_completo()$ano), allowDecimals = FALSE, reversed = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos fetais preenchidos com garbage codes"), min = 0, max = 100)
    })


    ## Criando os data.frames para a construção do gráfico de garbage codes p/ óbitos perinatais --------
    data_plot_garbage_perinatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_perinatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatal_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_perinatal")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_perinatal")), ~ round((. / obitos_perinatal_garbage * 100), 1)) |>
        dplyr::select(!obitos_perinatal_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_perinatal"),
          names_to = "cid",
          values_to = "prop_garbage_code_perinatal"
        ) |>
        dplyr::filter(prop_garbage_code_perinatal != 0) |>
        dplyr::mutate(
          prop_garbage_code_perinatal = round(prop_garbage_code_perinatal, 1),
          cid = toupper(substr(cid, nchar("garbage_perinatal_") + 1, nchar(cid))),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          ),
          causabas_subcategoria = ifelse(cid != "OUTROS", causabas_subcategoria, "Outros")
        )
    })

    data_plot_garbage_perinatal_referencia <- reactive({
      base_garbage_code |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_perinatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatal_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_perinatal")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_perinatal")), ~ round((. / obitos_perinatal_garbage * 100), 1)) |>
        dplyr::select(!obitos_perinatal_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_perinatal"),
          names_to = "cid",
          values_to = "br_prop_garbage_code_perinatal"
        ) |>
        dplyr::filter(br_prop_garbage_code_perinatal != 0) |>
        dplyr::mutate(
          br_prop_garbage_code_perinatal = round(br_prop_garbage_code_perinatal, 1),
          cid = toupper(substr(cid, nchar("garbage_perinatal_") + 1, nchar(cid)))
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          ),
          causabas_subcategoria = ifelse(cid != "OUTROS", causabas_subcategoria, "Outros")
        )
    })

    data_plot_garbage_perinatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_garbage_perinatal()) != 0,
          "Não existem ocorrências de óbitos perinatais preenchidos com garbage codes para a localidade e períodos selecionados."
        )
      )
      dplyr::left_join(data_plot_garbage_perinatal(), data_plot_garbage_perinatal_referencia())
    })

    ## Criando o gráfico da distribuição percentual de garbage codes p/ óbitos perinatais -------
    output$plot_garbage_perinatal <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_perinatal_completo(),
          highcharter::hcaes(x = ano, y = prop_garbage_code_perinatal, group = causabas_subcategoria),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {point.cid}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_garbage_code_perinatal:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE, title = list(text = "Garbage code (CID-10)")) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_garbage_perinatal_completo()$cid)) + 2, direction = 1)[-c(1, length(unique(data_plot_garbage_perinatal_completo()$cid)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_garbage_perinatal_completo()$ano), allowDecimals = FALSE, reversed = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos perinatais preenchidos com garbage codes"), min = 0, max = 100)
    })


    ## Criando os data.frames para a construção do gráfico de garbage codes p/ óbitos neonatais --------
    data_plot_garbage_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_neonatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatal_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_neonatal")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_neonatal")), ~ round((. / obitos_neonatal_garbage * 100), 1)) |>
        dplyr::select(!obitos_neonatal_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_neonatal"),
          names_to = "cid",
          values_to = "prop_garbage_code_neonatal"
        ) |>
        dplyr::filter(prop_garbage_code_neonatal != 0) |>
        dplyr::mutate(
          prop_garbage_code_neonatal = round(prop_garbage_code_neonatal, 1),
          cid = toupper(substr(cid, nchar("garbage_neonatal_") + 1, nchar(cid))),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          ),
          causabas_subcategoria = ifelse(cid != "OUTROS", causabas_subcategoria, "Outros")
        )
    })

    data_plot_garbage_neonatal_referencia <- reactive({
      base_garbage_code |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_neonatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatal_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_neonatal")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_neonatal")), ~ round((. / obitos_neonatal_garbage * 100), 1)) |>
        dplyr::select(!obitos_neonatal_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_neonatal"),
          names_to = "cid",
          values_to = "br_prop_garbage_code_neonatal"
        ) |>
        dplyr::filter(br_prop_garbage_code_neonatal != 0) |>
        dplyr::mutate(
          br_prop_garbage_code_neonatal = round(br_prop_garbage_code_neonatal, 1),
          cid = toupper(substr(cid, nchar("garbage_neonatal_") + 1, nchar(cid)))
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          ),
          causabas_subcategoria = ifelse(cid != "OUTROS", causabas_subcategoria, "Outros")
        )
    })

    data_plot_garbage_neonatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_garbage_neonatal()) != 0,
          "Não existem ocorrências de óbitos neonatais preenchidos com garbage codes para a localidade e períodos selecionados."
        )
      )
      dplyr::left_join(data_plot_garbage_neonatal(), data_plot_garbage_neonatal_referencia())
    })

    ## Criando o gráfico da distribuição percentual de garbage codes p/ óbitos neonatais -------
    output$plot_garbage_neonatal <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_neonatal_completo(),
          highcharter::hcaes(x = ano, y = prop_garbage_code_neonatal, group = causabas_subcategoria),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {point.cid}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_garbage_code_neonatal:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE, title = list(text = "Garbage code (CID-10)")) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_garbage_neonatal_completo()$cid)) + 2, direction = 1)[-c(1, length(unique(data_plot_garbage_neonatal_completo()$cid)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_garbage_neonatal_completo()$ano), allowDecimals = FALSE, reversed = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos neonatais preenchidos com garbage codes"), min = 0, max = 100)
    })


    ########## Distribuição de internações neonatais preenchidos com garbage code


    data_plot_garbage_morbidade_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_morbidade_neonatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatal_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_morbidade_neonatal")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_morbidade_neonatal")), ~ round((. / internacoes_neonatal_garbage * 100), 1)) |>
        dplyr::select(!internacoes_neonatal_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_morbidade_neonatal"),
          names_to = "cid",
          values_to = "prop_garbage_code_morbidade_neonatal"
        ) |>
        dplyr::filter(prop_garbage_code_morbidade_neonatal != 0) |>
        dplyr::mutate(
          prop_garbage_code_morbidade_neonatal = round(prop_garbage_code_morbidade_neonatal, 1),
          cid = toupper(substr(cid, nchar("garbage_morbidade_neonatal_") + 1, nchar(cid))),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          )
        )
    })

    data_plot_garbage_morbidade_neonatal_referencia <- reactive({
      base_garbage_code |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("garbage_morbidade_neonatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatal_garbage = sum(dplyr::c_across(dplyr::starts_with("garbage_morbidade_neonatal")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("garbage_morbidade_neonatal")), ~ round((. / internacoes_neonatal_garbage * 100), 1)) |>
        dplyr::select(!internacoes_neonatal_garbage) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("garbage_morbidade_neonatal"),
          names_to = "cid",
          values_to = "br_prop_garbage_code_morbidade_neonatal"
        ) |>
        dplyr::filter(br_prop_garbage_code_morbidade_neonatal != 0) |>
        dplyr::mutate(
          br_prop_garbage_code_morbidade_neonatal = round(br_prop_garbage_code_morbidade_neonatal, 1),
          cid = toupper(substr(cid, nchar("garbage_morbidade_neonatal_") + 1, nchar(cid)))
        ) |>
        dplyr::left_join(df_cid10 |> dplyr::select(cid = causabas, causabas_subcategoria)) |>
        dplyr::mutate(
          cid = ifelse(
            nchar(cid) == 4,
            paste(substr(cid, 1, 3), ".", substr(cid, 4, 4), sep = ""),
            cid
          )
        )
    })

    data_plot_garbage_morbidade_neonatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_garbage_morbidade_neonatal()) != 0,
          "Não existem ocorrências de internações neonatais preenchidos com garbage codes para a localidade e períodos selecionados."
        )
      )
      dplyr::left_join(data_plot_garbage_morbidade_neonatal(), data_plot_garbage_morbidade_neonatal_referencia())
    })

    ## Criando o gráfico da distribuição percentual de garbage codes p/ óbitos maternos -------
    output$plot_garbage_morbidade_neonatal <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_morbidade_neonatal_completo(),
          highcharter::hcaes(x = ano, y = prop_garbage_code_morbidade_neonatal, group = causabas_subcategoria),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {point.cid}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_prop_garbage_code_morbidade_neonatal:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = FALSE, title = list(text = "Garbage code (CID-10)")) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_garbage_morbidade_neonatal_completo()$cid)) + 2, direction = 1)[-c(1, length(unique(data_plot_garbage_morbidade_neonatal_completo()$cid)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_garbage_morbidade_neonatal_completo()$ano), allowDecimals = FALSE, reversed = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de internações neonatais preenchidos com garbage codes"), min = 0, max = 100)
    })


    ############ Porcentagem de garbage codes em relação ao total de óbitos

    garbage_porcentagens <- reactive({
      base_garbage_code |>
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
          prop_garbage_fetal = round(sum(total_garbage_codes_fetais, na.rm = T)/sum(obitos_fetais_totais, na.rm=T) *100, 2),
          prop_garbage_neonatal = round(sum(total_garbage_codes_neonatais, na.rm = T)/sum(obitos_neonatais_totais, na.rm=T) *100, 2),
          prop_garbage_perinatal = round(sum(total_garbage_codes_perinatais, na.rm = T)/sum(obitos_perinatais_totais, na.rm=T) *100, 2),
          prop_garbage_materno = round(sum(total_garbage_codes_maternos, na.rm = T)/sum(obitos_maternos_totais, na.rm=T) *100, 2)
        )|>
        dplyr::mutate(
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )|>
        dplyr::ungroup()
    })

    output$porcentagem_garbage_fetal <- highcharter::renderHighchart({

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_add_series(
          data = garbage_porcentagens(),
          type = "line",
          highcharter::hcaes(x = ano, y = prop_garbage_fetal, group = class, colour = class)
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = ""), min = 0)|>
        highcharter::hc_colors(cols)

    })



    output$porcentagem_garbage_neonatal <- highcharter::renderHighchart({

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_add_series(
          data = garbage_porcentagens(),
          type = "line",
          highcharter::hcaes(x = ano, y = prop_garbage_neonatal, group = class, colour = class)
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = ""), min = 0)|>
        highcharter::hc_colors(cols)

    })



    output$porcentagem_garbage_perinatal <- highcharter::renderHighchart({

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_add_series(
          data = garbage_porcentagens(),
          type = "line",
          highcharter::hcaes(x = ano, y = prop_garbage_perinatal, group = class, colour = class)
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = ""), min = 0)|>
        highcharter::hc_colors(cols)

    })



    output$porcentagem_garbage_materno <- highcharter::renderHighchart({

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_add_series(
          data = garbage_porcentagens(),
          type = "line",
          highcharter::hcaes(x = ano, y = prop_garbage_materno, group = class, colour = class)
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = ""), min = 0)|>
        highcharter::hc_colors(cols)

    })





    # Gráfico de barras das regiões -------------------------------------------
    ## Criando um data.frame que recebe a distribuição do indicador selecionado entre as regiões do país --------
    data_grafico_regioes <- reactive({
      validate(
        need(
          !startsWith(infos_indicador()$indicador, "Medianas"),
          "Os indicadores de medianas de deslocamento para o parto não estão disponíveis para o nível de análise nacional, para regiões do país e para macrorregiões e regiões de saúde estaduais. Dessa forma, esta visualização não se aplica."
        )
      )
      req(base_bloco_selecionado())
      base_bloco_selecionado() |>
        dplyr::filter(ano %in% anos_disponiveis()) |>
        dplyr::group_by(regiao) |>
        dplyr::summarise(
          indicador = !!rlang::parse_expr(infos_indicador()$calculo)
        ) |>
        dplyr::ungroup()

    })

    ## Criando o gráfico de barras --------------------------------------------
    output$grafico_regioes <- highcharter::renderHighchart({
      proporcoes <- data_grafico_regioes()$indicador
      regioes <- factor(data_grafico_regioes()$regiao)
      levels(regioes) <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
      df <- data.frame(regioes, proporcoes)

      highcharter::hchart(
        df,
        type = "column",
        name = stringr::str_replace(infos_indicador()$indicador, "Porcentagem", "%"),
        highcharter::hcaes(
          x = regioes,
          y = proporcoes,
          color = c("#FE9F6DFF", "#DE4968FF", "#8C2981FF", "#3B0F70FF", "#000004FF")
        )
      ) |>
        highcharter::hc_tooltip(valueSuffix = dplyr::if_else(infos_indicador()$tipo_do_indicador == "porcentagem", "%", "")) |>
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(
          text = stringr::str_replace(infos_indicador()$indicador, "Porcentagem", "%")
        ),
        min = 0,
        ceiling = ifelse(infos_indicador()$tipo_do_indicador == "porcentagem", 100, "undefined")
        )
    })


    # Gráfico de linhas do indicador ------------------------------------------
    ## Criando data.frames com os dados do indicador ---------------------------
    data_grafico_serie <- reactive({
      data_grafico_serie_aux <- base_bloco_selecionado() |>
        dplyr::filter(
          ano %in% anos_disponiveis(),
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
          indicador = !!rlang::parse_expr(infos_indicador()$calculo),
          km_total = ifelse(
            grepl("km_", infos_indicador()$nome_abreviado),
            .data[[infos_indicador()$nome_abreviado]],
            NA
          ),
          km_baixa_complexidade = ifelse(
            grepl("km_", infos_indicador()$nome_abreviado),
            .data[[paste0(infos_indicador()$nome_abreviado, "_baixa_complexidade")]],
            NA
          ),
          km_alta_complexidade = ifelse(
            grepl("km_", infos_indicador()$nome_abreviado),
            .data[[paste0(infos_indicador()$nome_abreviado, "_alta_complexidade")]],
            NA
          ),
          tx_abortos_mil_mulheres_lim_inf = ifelse(
            grepl("tx_abortos_mil_mulheres_valor_medio", infos_indicador()$nome_abreviado),
            dplyr::case_when(
              infos_indicador()$nome_abreviado == "geral_tx_abortos_mil_mulheres_valor_medio" ~ round(((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 3) + (((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5)) / sum(pop_fem_10_49) * 1000, 1),
              infos_indicador()$nome_abreviado == "sus_tx_abortos_mil_mulheres_valor_medio" ~ round((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 3) / sum(pop_fem_sus_10_49) * 1000, 1),
              infos_indicador()$nome_abreviado == "ans_tx_abortos_mil_mulheres_valor_medio" ~ round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 4) / sum(pop_fem_ans_10_49) * 1000, 1),
            ),
            NA
          ),
          tx_abortos_mil_mulheres_lim_sup = ifelse(
            grepl("tx_abortos_mil_mulheres_valor_medio", infos_indicador()$nome_abreviado),
            dplyr::case_when(
              infos_indicador()$nome_abreviado == "geral_tx_abortos_mil_mulheres_valor_medio" ~ round(((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 5) + (((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 7)) / sum(pop_fem_10_49) * 1000, 1),
              infos_indicador()$nome_abreviado == "sus_tx_abortos_mil_mulheres_valor_medio" ~ round((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 5) / sum(pop_fem_sus_10_49) * 1000, 1),
              infos_indicador()$nome_abreviado == "ans_tx_abortos_mil_mulheres_valor_medio" ~ round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 6) / sum(pop_fem_ans_10_49) * 1000, 1)
            ),
            NA
          ),
          tx_abortos_cem_nascidos_vivos_lim_inf = ifelse(
            grepl("tx_abortos_cem_nascidos_vivos_valor_medio", infos_indicador()$nome_abreviado),
            dplyr::case_when(
              infos_indicador()$nome_abreviado == "geral_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round(((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 3) + (((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49) * 100, 1),
              infos_indicador()$nome_abreviado == "sus_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 3) / sum(total_de_nascidos_vivos_10_a_49_sus) * 100, 1),
              infos_indicador()$nome_abreviado == "ans_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_ans) * 100, 1)
            ),
            NA
          ),
          tx_abortos_cem_nascidos_vivos_lim_sup = ifelse(
            grepl("tx_abortos_cem_nascidos_vivos_valor_medio", infos_indicador()$nome_abreviado),
            dplyr::case_when(
              infos_indicador()$nome_abreviado == "geral_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round(((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 5) + (((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 7)) / sum(total_de_nascidos_vivos_10_a_49) * 100, 1),
              infos_indicador()$nome_abreviado == "sus_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_sus) * 100, 1),
              infos_indicador()$nome_abreviado == "ans_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 6) / sum(total_de_nascidos_vivos_10_a_49_ans) * 100, 1)
            ),
            NA
          ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ dplyr::if_else(
              infos_indicador()$descricao_referencia == "média nacional",
              "Brasil (valor de referência)",
              "Brasil"
            ),
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()

      data_grafico_serie_aux
    })

    data_rmm_corrigida_aux <- reactive({
      if (infos_indicador()$nome_abreviado == "rmm") {
        if(filtros()$nivel %in% c("estadual", "regional", "nacional")){
          if(filtros()$nivel == "estadual"){
            rmm_corrigida |>
              dplyr::filter(
                localidade == filtros()$estado,
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              )
          } else if(filtros()$nivel == "regional"){
            rmm_corrigida |>
              dplyr::filter(
                localidade == filtros()$regiao,
                ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
              )
          } else if(filtros()$nivel=="nacional"){
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
      }
    })

    data_rmm_corrigida <- reactive({
      if (infos_indicador()$nome_abreviado == "rmm") {
        if (filtros()$nivel %in% c("estadual", "regional", "nacional")) {
          dplyr::full_join(data_grafico_serie(), data_rmm_corrigida_aux(), by = "ano") |>
            dplyr::mutate(
              rmm = ifelse(ano <= 2022, RMM, indicador)
            )
        } else {
          data_grafico_serie() |>
            dplyr::rename(rmm = indicador)
        }
      }
    })

    data_referencia_serie <- reactive({
      base_bloco_selecionado() |>
        dplyr::filter(
          ano %in% anos_disponiveis()
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          indicador = ifelse(
            infos_indicador()$referencia == "Nacional",
            !!rlang::parse_expr(infos_indicador()$calculo),
            ifelse(
              infos_indicador()$nome_abreviado != "porc_nasc_baixo_peso_menos_1000",
              {as.numeric(infos_indicador()$referencia)},
              NA
            )
          ),
          tx_abortos_mil_mulheres_lim_inf = ifelse(
            grepl("tx_abortos_mil_mulheres_valor_medio", infos_indicador()$nome_abreviado),
            dplyr::case_when(
              infos_indicador()$nome_abreviado == "geral_tx_abortos_mil_mulheres_valor_medio" ~ round(((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 3) + (((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5)) / sum(pop_fem_10_49) * 1000, 1),
              infos_indicador()$nome_abreviado == "sus_tx_abortos_mil_mulheres_valor_medio" ~ round((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 3) / sum(pop_fem_sus_10_49) * 1000, 1),
              infos_indicador()$nome_abreviado == "ans_tx_abortos_mil_mulheres_valor_medio" ~ round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5) / sum(pop_fem_ans_10_49) * 1000, 1),
            ),
            NA
          ),
          tx_abortos_mil_mulheres_lim_sup = ifelse(
            grepl("tx_abortos_mil_mulheres_valor_medio", infos_indicador()$nome_abreviado),
            dplyr::case_when(
              infos_indicador()$nome_abreviado == "geral_tx_abortos_mil_mulheres_valor_medio" ~ round(((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 5) + (((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 7)) / sum(pop_fem_10_49) * 1000, 1),
              infos_indicador()$nome_abreviado == "sus_tx_abortos_mil_mulheres_valor_medio" ~ round((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 5) / sum(pop_fem_sus_10_49) * 1000, 1),
              infos_indicador()$nome_abreviado == "ans_tx_abortos_mil_mulheres_valor_medio" ~ round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 7) / sum(pop_fem_ans_10_49) * 1000, 1)
            ),
            NA
          ),
          tx_abortos_cem_nascidos_vivos_lim_inf = ifelse(
            grepl("tx_abortos_cem_nascidos_vivos_valor_medio", infos_indicador()$nome_abreviado),
            dplyr::case_when(
              infos_indicador()$nome_abreviado == "geral_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round(((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 3) + (((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49) * 100, 1),
              infos_indicador()$nome_abreviado == "sus_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 3) / sum(total_de_nascidos_vivos_10_a_49_sus) * 100, 1),
              infos_indicador()$nome_abreviado == "ans_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans) * 100, 1)
            ),
            NA
          ),
          tx_abortos_cem_nascidos_vivos_lim_sup = ifelse(
            grepl("tx_abortos_cem_nascidos_vivos_valor_medio", infos_indicador()$nome_abreviado),
            dplyr::case_when(
              infos_indicador()$nome_abreviado == "geral_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round(((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 5) + (((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 7)) / sum(total_de_nascidos_vivos_10_a_49) * 100, 1),
              infos_indicador()$nome_abreviado == "sus_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round((((sum(abortos_sus_menor_30) * 0.9) + (sum(abortos_sus_30_a_39) * 0.85) + (sum(abortos_sus_40_a_49) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_sus) * 100, 1),
              infos_indicador()$nome_abreviado == "ans_tx_abortos_cem_nascidos_vivos_valor_medio" ~ round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 7) / sum(total_de_nascidos_vivos_10_a_49_ans) * 100, 1)
            ),
            NA
          ),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_referencia_baixo_peso_aux <- reactive({
      base_referencia_baixo_peso |>
        dplyr::filter(
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(nascidos, na.rm = TRUE),
          porc_nasc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_referencia_baixo_peso <- reactive({
      data.frame(
        ano = anos_disponiveis(),
        porc_nasc_baixo_peso = data_referencia_baixo_peso_aux()$porc_nasc_baixo_peso,
        class = "Referência"
      )
    })

    ## Criando o gráfico de linhas --------------------------------------------
    output$grafico_serie <- highcharter::renderHighchart({
      if (!(base::startsWith(infos_indicador()$indicador, "Medianas"))) {
        if (infos_indicador()$nome_abreviado == "rmm") {
          grafico_base <- highcharter::highchart() |>
            highcharter::hc_add_dependency("modules/series-label.js") |>
            highcharter::hc_add_series(
              data = data_rmm_corrigida(),
              type = "line",
              highcharter::hcaes(x = ano, y = rmm, group = class, colour = class)
            ) |>
            highcharter::hc_plotOptions(
              series = list(
                label = list(enabled = TRUE),
                allowPointSelect = TRUE
              )
            ) |>
            highcharter::hc_tooltip(valueSuffix = dplyr::if_else(infos_indicador()$tipo_do_indicador == "porcentagem", "%", ""), shared = TRUE, sort = TRUE) |>
            highcharter::hc_xAxis(
              title = list(text = ""),
              categories = anos_disponiveis(),
              allowDecimals = FALSE
            ) |>
            highcharter::hc_yAxis(
              title = list(
                text = stringr::str_replace(infos_indicador()$indicador, "Porcentagem", "%")
              ),
              min = 0
            ) |>
            highcharter::hc_colors(cols)
        } else {
          grafico_base <- highcharter::highchart() |>
            highcharter::hc_add_dependency("modules/series-label.js") |>
            highcharter::hc_add_series(
              data = data_grafico_serie(),
              type = "line",
              highcharter::hcaes(x = ano, y = indicador, group = class, colour = class),
              tooltip = list(
                pointFormat = dplyr::case_when(
                  grepl("tx_abortos_cem_nascidos_vivos_valor_medio", infos_indicador()$nome_abreviado) ~ "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} (limite inferior de {point.tx_abortos_cem_nascidos_vivos_lim_inf:,f} e limite superior de {point.tx_abortos_cem_nascidos_vivos_lim_sup:,f})</b> </br>",
                  grepl("tx_abortos_mil_mulheres_valor_medio", infos_indicador()$nome_abreviado) ~ "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} (limite inferior de {point.tx_abortos_mil_mulheres_lim_inf:,f} e limite superior de {point.tx_abortos_mil_mulheres_lim_sup:,f})</b> </br>",
                  !grepl("tx_abortos", infos_indicador()$nome_abreviado) ~ "<span style = 'color: {series.color}'>&#9679</span> {series.name}: <b> {point.y} </b> </br>"
                )
              )
            ) |>
            highcharter::hc_plotOptions(
              series = list(
                label = list(enabled = TRUE),
                allowPointSelect = TRUE
              )
            ) |>
            highcharter::hc_tooltip(valueSuffix = dplyr::if_else(infos_indicador()$tipo_do_indicador == "porcentagem", "%", ""), shared = TRUE, sort = TRUE) |>
            highcharter::hc_xAxis(
              title = list(text = ""),
              categories = anos_disponiveis(),
              allowDecimals = FALSE
            ) |>
            highcharter::hc_yAxis(
              title = list(
                text = stringr::str_replace(infos_indicador()$indicador, "Porcentagem", "%")
              ),
              min = 0,
              ceiling = ifelse(infos_indicador()$tipo_do_indicador == "porcentagem", 100, "undefined")
            ) |>
            highcharter::hc_colors(cols)
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
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
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = " km", shared = TRUE, sort = TRUE, valueDecimals = 2) |>
          highcharter::hc_xAxis(
            title = list(text = ""),
            categories = anos_disponiveis(),
            allowDecimals = FALSE
          ) |>
          highcharter::hc_yAxis(title = list(text = "km"), min = 0) |>
          highcharter::hc_colors(cols)
      }
      if ((filtros()$nivel == "nacional" & infos_indicador()$descricao_referencia == "média nacional") | (infos_indicador()$descricao_referencia == "sem referência")) {
        grafico_base
      } else {
        if (infos_indicador()$nome_abreviado == "prop_robson2_tx_cesariana") {
          grafico_base |> highcharter::hc_add_series(
            data = data_referencia_serie(),
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
            data = data_referencia_serie(),
            name = "Referência (meta OMS)",
            highcharter::hcaes(x = ano, low = 50, high = 60),
            type = "arearange",
            dashStyle = "ShortDot",
            color = "#721f81",
            fillOpacity = 0.2,
            enableMouseTracking = TRUE
          )
        } else if (infos_indicador()$nome_abreviado == "porc_nasc_baixo_peso_menos_2500") {
          grafico_base |>
            highcharter::hc_add_series(
              data = data_referencia_baixo_peso(),
              type = "line",
              highcharter::hcaes(x = ano, y = porc_nasc_baixo_peso, group = class, colour = class),
              name = "Referência para a localidade (meta de redução global)",
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        } else if (infos_indicador()$nome_abreviado == "porc_nasc_malformacoes") {
          grafico_base |> highcharter::hc_add_series(
            data = data_referencia_serie(),
            name = "Referência (mundo)",
            highcharter::hcaes(x = ano, low = 3, high = 6),
            type = "arearange",
            dashStyle = "ShortDot",
            color = "#721f81",
            fillOpacity = 0.2,
            enableMouseTracking = TRUE
            ) |>
            highcharter::hc_yAxis(
              minPadding = 0.1,
              maxPadding = 0.1
            )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data_referencia_serie() |>
                dplyr::filter(
                  ano <= filtros()$ano2[2]
                ),
              type = "line",
              highcharter::hcaes(x = ano, y = indicador, group = class, colour = class),
              name = glue::glue("Referência ({infos_indicador()$descricao_referencia})"),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      }
    })


    # Tabela ------------------------------------------------------------------
    ## Criando um data.frame com as informações necessárias para a construção da tabela --------
    data_tabela1 <- eventReactive(c(input$opcoes_tab1, filtros()$pesquisar), {
      validate(
        need(
          !startsWith(infos_indicador()$indicador, "Medianas"),
          "Os indicadores de medianas de deslocamento para o parto não estão disponíveis para microrregiões e macrorregiões de saúde estaduais, para regiões do país e para o nível de análise nacional. Além disso, mesmo que esses indicadores sejam calculados para municípios e estados, calcular um valor médio para representar o resumo do período não é aconselhável. Dessa forma, esta visualização não se aplica."
        )
      )
      if (input$opcoes_tab1 == "escolha1") {
        base_bloco_selecionado() |>
          dplyr::filter(ano %in% anos_disponiveis()) |>
          dplyr::group_by(uf, macro_r_saude, municipio) |>
          dplyr::summarise(
            numerador = !!rlang::parse_expr(infos_indicador()$numerador),
            denominador = !!rlang::parse_expr(infos_indicador()$denominador),
            indicador := dplyr::if_else(
              condition = infos_indicador()$tipo_do_indicador == "porcentagem",
              true = numerador/denominador,
              false = round(numerador/denominador * {infos_indicador()$fator}, 1)
            )
          )
      } else {
        base_bloco_selecionado() |>
          dplyr::filter(
            ano %in% anos_disponiveis(),
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
          dplyr::group_by(uf, macro_r_saude, municipio) |>
          dplyr::summarise(
            numerador = !!rlang::parse_expr(infos_indicador()$numerador),
            denominador = !!rlang::parse_expr(infos_indicador()$denominador),
            indicador := dplyr::if_else(
              condition = infos_indicador()$tipo_do_indicador == "porcentagem",
              true = numerador/denominador,
              false = round(numerador/denominador * {infos_indicador()$fator}, 1)
            )
          )
      }
    },
    ignoreNULL = FALSE
    )

    ## Criando um data.frame a versão da tabela que vai para download ----------
    data_tabela_download <- eventReactive(c(filtros()$pesquisar), {
      base_bloco_selecionado() |>
        dplyr::filter(ano %in% anos_disponiveis()) |>
        dplyr::group_by(codmunres, municipio, r_saude, macro_r_saude, uf, regiao) |>
        dplyr::summarise(
          periodo = paste(max(filtros()$ano2[1], anos_disponiveis()[1]), "a", min(filtros()$ano2[2], anos_disponiveis()[length(anos_disponiveis())])),
          numerador = !!rlang::parse_expr(infos_indicador()$numerador),
          denominador = !!rlang::parse_expr(infos_indicador()$denominador),
          indicador := round(numerador/denominador * {infos_indicador()$fator}, 1)
        ) |>
        dplyr::rename(
          !!infos_indicador()$nome_numerador := numerador,
          !!infos_indicador()$nome_denominador := denominador,
          !!infos_indicador()$indicador := indicador
        )
    },
    ignoreNULL = FALSE
    )

    ## Criando o output de download da tabela ----------------------------------
    output$download_tabela <- downloadHandler(
      filename = function() {
        paste0("dados_tabela_nivel3_", infos_indicador()$nome_abreviado, ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write.csv(data_tabela_download(), file, row.names = FALSE)
      }
    )

    ## Criando a tabela --------------------------------------------------------
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

      if (infos_indicador()$tipo_do_indicador != "absoluto") {
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
                name = "Macrorregião de saúde estadual",
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
              indicador = reactable::colDef(
                name = dplyr::if_else(
                  !(grepl("tx_abortos_", infos_indicador()$nome_abreviado)),
                  infos_indicador()$indicador,
                  paste0("Valor médio da ", tolower(substr(infos_indicador()$indicador, 1, 1)), substr(infos_indicador()$indicador, 2, nchar(infos_indicador()$indicador)))
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
            height = ifelse(infos_indicador()$bloco != "bloco6" & !grepl("bloco7", infos_indicador()$bloco), 730, 1440),
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
          dplyr::select(!c(numerador, denominador)) |>
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
                name = "Macrorregião de saúde estadual",
                aggregate = reactable::JS("function() { return ''}"),
                format = list(aggregated = reactable::colFormat(prefix = "Todas"))
              ),
              municipio = reactable::colDef(
                name = "Município",
                aggregate = reactable::JS("function() { return ''}"),
                format = list(aggregated = reactable::colFormat(prefix = "Todos"))
              ),
              indicador = reactable::colDef(
                name = infos_indicador()$nome_numerador,
                aggregate = "sum"
              )
            ),
            sortable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            striped = TRUE,
            bordered = FALSE,
            pagination = FALSE,
            height = ifelse(infos_indicador()$bloco != "bloco6" & !grepl("bloco7", infos_indicador()$bloco), 810, 1410),
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

    ## Criando um output auxiliar que define o tamanho da tabela ---------------
    output$css_tabela <- renderUI({
      if (infos_indicador()$bloco == "bloco6") {
        tags$style(HTML("#tabela {height: 1793px; padding-top: 0; padding-bottom: 00px; overflow-y: auto}"))
      } else if (grepl("bloco7", infos_indicador()$bloco)) {
        tags$style(HTML("#tabela {height: 1793px; padding-top: 0; padding-bottom: 00px; overflow-y: auto}"))
      } else {
        tags$style(HTML("#tabela {height: 1150px; padding-top: 0; padding-bottom: 00px; overflow-y: auto}"))
      }
    })


  })
}



## To be copied in the UI
# mod_nivel_3_ui("nivel_3_1")

## To be copied in the server
# mod_nivel_3_server("nivel_3_1")
