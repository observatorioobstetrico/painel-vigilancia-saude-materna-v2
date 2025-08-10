#' bloco_7_perinatal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_7_perinatal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(
        "
        .multicol {
          -webkit-column-count: 2; /* Chrome, Safari, Opera */
          -moz-column-count: 2;    /* Firefox */
          column-count: 2;
          -moz-column-fill: auto;
          -column-fill: auto;
        }

        .shiny-input-checkboxgroup label~.shiny-options-group, .shiny-input-radiogroup label~.shiny-options-group {
          margin-top: 0px;
        }
        "
      )
    ),
    fluidRow(
      column(
        width=12,
        HTML(
          "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
            Neste painel, consideramos apenas óbitos perinatais com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500 g ou idade de até 6 dias de vida.
            <span style='display: block; margin-bottom: 15px;'> </span>
            <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos fetais e neonatais no país, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-fetais-neonatais/' target = _blank>OOBr Óbitos Fetais e Neonatais</a>.
            </b> </div>"
        ),
        hr(),
        HTML("<span style='display: block; margin-bottom: 27px;'> </span>")
      )
    ),
    fluidRow(
      column(
        width = 4,
        HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
        div(
          HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
          shinyWidgets::actionBttn(
            inputId = ns('botao_resumo'),
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
            uiOutput(ns("input_localidade_resumo")),
            align = "center"
          )
        ),
        fluidRow(
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_perinatal_num_obitos"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 31%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Número de óbitos perinatais&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_num_obitos"),
                      actionButton(
                        inputId = ns("info_btn_num_obitos"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_num_obitos")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_perinatal_taxa_de_mortalidade"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 31%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Taxa de mortalidade perinatal por 1000 nascidos vivos&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_taxa_de_mortalidade"),
                      actionButton(
                        inputId = ns("info_btn_taxa_de_mortalidade"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_taxa_de_mortalidade")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_perinatal_distribuicao_momento"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 15%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Dentre os óbitos perinatais selecionados,&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_distribuicao_momento"),
                      actionButton(
                        inputId = ns("info_btn_distribuicao_momento"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_distribuicao_momento")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_perinatal_distribuicao_peso"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 15%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Dentre os óbitos perinatais selecionados,&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_distribuicao_peso"),
                      actionButton(
                        inputId = ns("info_btn_distribuicao_peso"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_distribuicao_peso")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_perinatal_principais"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 15%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Dentre os óbitos perinatais selecionados,&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_principais"),
                      actionButton(
                        inputId = ns("info_btn_principais"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_principais")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_perinatal_evitaveis"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 31%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Porcentagem de óbitos perinatais potencialmente evitáveis&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_evitaveis"),
                      actionButton(
                        inputId = ns("info_btn_evitaveis"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_evitaveis")), proxy.height = "276px")
            )
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
                HTML("<b class = 'fonte-muito-grande'> Número de óbitos perinatais &nbsp;</b>")
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  shinyWidgets::pickerInput(
                    inputId = ns("input_num_obitos_peso"),
                    label = span(class = "fonte-grande", "Faixas de peso"),
                    options = list(placeholder = "Selecione, aqui, as faixas de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "Menor que 1000 g" = "menos_1000",
                      "De 1000 g a 1499 g" = "1000_1499",
                      "De 1500 g a 2499 g" = "1500_2499",
                      "Maior ou igual a 2500 g" = "2500_mais",
                      "Sem informação" = "sem_info"
                    ),
                    selected = c("menos_1000", "1000_1499", "1500_2499", "2500_mais", "sem_info"),
                    multiple = TRUE,
                    width = "99%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_obitos"), height = 380))
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
                HTML("<b class = 'fonte-muito-grande'> Taxa de mortalidade perinatal por 1000 nascidos vivos &nbsp;</b>")
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  shinyWidgets::pickerInput(
                    inputId = ns("input_taxa_de_mortalidade_peso"),
                    label = span(class = "fonte-grande", "Faixas de peso"),
                    options = list(placeholder = "Selecione, aqui, as faixas de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "Menor que 1000 g" = "menos_1000",
                      "De 1000 g a 1499 g" = "1000_1499",
                      "De 1500 g a 2499 g" = "1500_2499",
                      "Maior ou igual a 2500 g" = "2500_mais",
                      "Sem informação" = "sem_info"
                    ),
                    selected = c("menos_1000", "1000_1499", "1500_2499", "2500_mais", "sem_info"),
                    multiple = TRUE,
                    width = "99%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_taxa_de_mortalidade"), height = 380))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 580px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 10%; display: flex; align-items: center;",
                HTML("<b class = 'fonte-muito-grande'> Distribuição percentual do momento do óbito perinatal por faixa de peso &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao_comparacao1"),
                    actionButton(
                      inputId = ns("info_btn_comparacao1"),
                      label = NULL,
                      icon = icon("info-circle", class = "info-icon"),
                      class = "btn btn-sm btn-no-style info-btn fonte-media"
                    )
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  strong(p("Selecione as faixas de peso:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                  tags$div(
                    align = 'left',
                    class = 'multicol',
                    checkboxGroupInput(
                      inputId = ns("input_distribuicao_peso"),
                      label    = NULL,
                      choices = c(
                        "Menor que 1000 g" = "menos_1000",
                        "De 1000 a 1499 g" = "1000_1499",
                        "De 1500 a 2499 g" = "1500_2499",
                        "Maior ou igual a 2500 g" = "2500_mais"
                      ),
                      selected = c(
                        "menos_1000", "1000_1499",
                        "1500_2499", "2500_mais"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_distribuicao_momento"), height = 360))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 580px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 10%; display: flex; align-items: center;",
                HTML("<b class = 'fonte-muito-grande'> Distribuição percentual das faixas de peso por momento do óbito perinatal &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao_comparacao2"),
                    actionButton(
                      inputId = ns("info_btn_comparacao2"),
                      label = NULL,
                      icon = icon("info-circle", class = "info-icon"),
                      class = "btn btn-sm btn-no-style info-btn fonte-media"
                    )
                  )
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  strong(p("Selecione os momentos de óbito:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                  tags$div(
                    align = 'left',
                    class = 'multicol',
                    checkboxGroupInput(
                      inputId = ns("input_distribuicao_momento"),
                      label    = NULL,
                      choices = c(
                        "Antes do parto" = "antes",
                        "Durante o parto" = "durante",
                        "Dia 0 de vida" = "0_dias",
                        "De 1 a 6 dias de vida" = "1_a_6_dias"
                      ),
                      selected = c("antes", "durante", "0_dias", "1_a_6_dias")
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_distribuicao_peso"), height = 370))
            )
          ),
          column(
            width = 12,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 7%; display: flex; align-items: center;",
                HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos perinatais por grupos de causas segundo <a href = https://www.scielo.br/j/csp/a/Ss5zQXrmrGrGJvcVMKmJdqR/?format=pdf&lang=pt , target = _blank>França e Lansky (2009)</a> &nbsp;</b>")
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  shinyWidgets::pickerInput(
                    inputId = ns("input_principais_grupos"),
                    label = HTML("<span class = 'fonte-grande'>Selecione, aqui, os grupos de interesse:</span>"),
                    options = list(placeholder = "Selecione, aqui, os grupos de interesse:",
                                   `actions-box` = TRUE,
                                   `deselect-all-text` = "Desselecionar todas",
                                   `select-all-text` = "Selecionar todas",
                                   `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "Prematuridade" = "prematuridade",
                      "Infecções" = "infeccoes",
                      "Asfixia/Hipóxia" = "asfixia",
                      "Anomalia congênita" = "ma_formacao",
                      "Afecções respiratórias dos recém nascidos" = "respiratorias",
                      "Fatores maternos relacionados à gravidez " = "gravidez",
                      "Afecções originais no período perinatal" = "afeccoes",
                      "Mal definidas" = "mal_definida",
                      "Demais causas" = "outros"

                    ),
                    selected = c(
                      "prematuridade",
                      "infeccoes",
                      "asfixia",
                      "ma_formacao",
                      "respiratorias",
                      "gravidez",
                      "afeccoes",
                      "mal_definida",
                      "outros"
                    ),
                    multiple = TRUE,
                    width = "99%"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                  tags$div(
                    align = 'left',
                    class = 'multicol',
                    checkboxGroupInput(
                      inputId = ns("input_principais_momento"),
                      label    = NULL,
                      choices = c(
                        "Antes do parto" = "antes",
                        "Durante o parto" = "durante",
                        "Dia 0 de vida" = "0_dias",
                        "De 1 a 6 dias de vida" = "1_a_6_dias",
                        "Sem informação" = "sem_info_parto"
                      ),
                      selected = c("antes", "sem_info_parto", "durante", "0_dias", "1_a_6_dias")
                    )
                  )
                ),
                column(
                  width = 6,
                  shinyWidgets::pickerInput(
                    inputId = ns("input_principais_peso"),
                    label = HTML("<span class = 'fonte-grande'>Selecione, aqui, as faixas de peso consideradas:</span>"),
                    options = list(placeholder = "Selecione, aqui, as faixas de peso consideradas",
                                   `actions-box` = TRUE,
                                   `deselect-all-text` = "Desselecionar todas",
                                   `select-all-text` = "Selecionar todas",
                                   `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "Menor que 1000 g" = "menos_1000",
                      "De 1000 g a 1499 g" = "1000_a_1499",
                      "De 1500g a 2499g" = "1500_a_2499",
                      "Maior ou igual a 2500 g" = "2500_mais",
                      "Sem informação" = "sem_informacao"
                    ),
                    selected = c("menos_1000","1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
                    multiple = TRUE,
                    width = "98%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_causas_principais"), height = 390))
            )
          ),
          column(
            width = 12,
            bs4Dash::bs4Card(
              width = 12 ,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 7%; display: flex; align-items: center;",
                HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos perinatais por análise de evitabilidade (Fonte: <a href = http://tabnet.datasus.gov.br/cgi/sim/Obitos_Evitaveis_0_a_4_anos.pdf , target = _blank>link</a>) &nbsp;</b>")),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  shinyWidgets::pickerInput(
                    inputId = ns("input_evitaveis_grupos"),
                    label = HTML("<span class = 'fonte-grande'>Selecione, aqui, os grupos de interesse:</span>"),
                    options = list(placeholder = "Selecione, aqui, os grupos de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "Reduzível pelas ações de imunização" = "imunoprevencao",
                      "Reduzíveis por adequada atenção à mulher na gestação" = "mulher_gestacao",
                      "Reduzíveis por adequada atenção à mulher no parto" = "parto",
                      "Reduzíveis por adequada atenção ao recém-nascido" = "recem_nascido",
                      "Reduzíveis por ações de diagnóstico e tratamento adequado" = "tratamento",
                      "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "saude",
                      "Causas mal definidas" = "mal_definidas",
                      "Causas não claramente evitáveis" = "outros"
                    ),
                    selected = c(
                      "imunoprevencao",
                      "mulher_gestacao",
                      "parto",
                      "recem_nascido",
                      "tratamento",
                      "saude",
                      "mal_definidas",
                      "outros"
                    ),
                    multiple = TRUE,
                    width = "99%"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                  tags$div(
                    align = 'left',
                    class = 'multicol',
                    checkboxGroupInput(
                      inputId = ns("input_evitaveis_momento"),
                      label    = NULL,
                      choices = c(
                        "Antes do parto" = "antes",
                        "Durante o parto" = "durante",
                        "Dia 0 de vida" = "0_dias",
                        "De 1 a 6 dias de vida" = "1_a_6_dias",
                        "Sem informação" = "sem_info_parto"
                      ),
                      selected = c("antes", "sem_info_parto", "durante", "0_dias", "1_a_6_dias")
                    )
                  )
                ),
                column(
                  width = 6,
                  shinyWidgets::pickerInput(
                    inputId = ns("input_evitaveis_peso"),
                    label = HTML("<span class = 'fonte-grande'>Selecione, aqui, as faixas de peso consideradas:</span>"),
                    options = list(placeholder = "Selecione, aqui, as faixas de peso consideradas",
                                   `actions-box` = TRUE,
                                   `deselect-all-text` = "Desselecionar todas",
                                   `select-all-text` = "Selecionar todas",
                                   `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "Menor que 1000 g" = "menos_1000",
                      "De 1000 g a 1499 g" = "1000_a_1499",
                      "De 1500g a 2499g" = "1500_a_2499",
                      "Maior ou igual a 2500 g" = "2500_mais",
                      "Sem informação" = "sem_informacao"
                    ),
                    selected = c("menos_1000","1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
                    multiple = TRUE,
                    width = "98%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_causas_evitaveis"), height = 390))
            )
          )
        )
      )
    )
  )
}

#' bloco_7_perinatal Server Functions
#'
#' @noRd
mod_bloco_7_perinatal_server <- function(id, filtros){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Criando alguns outputs para a UI ----------------------------------------
    ## Criando os outputs que receberão os nomes dos locais selecionados quando há comparação --------
    output$input_localidade_resumo <- renderUI({
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


    ## Criando o output que receberá o nível selecionado ----------------------
    nivel_selecionado <- reactive({
      if (filtros()$comparar == "Não") {
        filtros()$nivel
      } else {
        req(input$localidade_resumo)
        if (input$localidade_resumo == "escolha1") {
          filtros()$nivel
        } else {
          filtros()$nivel2
        }
      }
    })


    ## Para os botões de informação em alguns indicadores ----------------------
    ### Mostrando o botão com a informação sobre a comparação nos gráficos de distribuição proporcional
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao_comparacao1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_comparacao2", anim = TRUE, animType = "fade", time = 0.8)
      req(filtros()$comparar == "Sim")
      shinyjs::show(id = "mostrar_botao_comparacao1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_comparacao2", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    ### Criando as tooltips com a informação sobre a comparação nos gráficos de distribuição proporcional
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_comparacao1",
      options = list(
        title = '<span class = "fonte-media">Para visualizar os valores referentes à localidade de comparação, passe o cursor do mouse sobre a barra com a categoria de interesse. </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_comparacao2",
      options = list(
        title = '<span class = "fonte-media">Para visualizar os valores referentes à localidade de comparação, passe o cursor do mouse sobre a barra com a categoria de interesse.</span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )


    ## Criando o pop-up com a informação sobre o resumo do período -------------
    observeEvent(c(input$botao_resumo), {
      shinyalert::shinyalert(
        html = TRUE,
        title = '<div class = "fonte-titulos-modal" style = "color: #656565"> Sobre o "Resumo do período" </div>',
        text = '
          <div style = "text-align: justify; text-justify: inter-word;">
            Todas as caixinhas que estão sob o "Resumo do período", na esquerda da página, referem-se aos valores dos indicadores calculados considerando todo o período selecionado.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando alguma comparação é feita, o usuário pode selecionar para qual localidade o resumo do período será calculado clicando em um dos botões que irão aparecer em cima das caixinhas.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Para este bloco, as caixinhas relacionadas ao número de óbitos ou às taxas de mortalidade mudam de acordo com o momento do óbito e a faixa de peso selecionados nos gráficos.
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
          peso = round(sum(peso_incompletos, na.rm = TRUE) / sum(peso_totais, na.rm = TRUE) * 100, 1),
          gestacao = round(sum(gestacao_incompletos, na.rm = TRUE) / sum(gestacao_totais, na.rm = TRUE) * 100, 1),
          semagestac = round(sum(semagestac_incompletos, na.rm = TRUE) / sum(semagestac_totais, na.rm = TRUE) * 100, 1),
          idanomal = round(sum(idanomal_incompletos, na.rm = TRUE) / sum(idanomal_totais,na.rm = TRUE) * 100, 1),
          condicoes_ameacadoras = round(sum(condicoes_ameacadoras_incompletos_intersecao, na.rm = TRUE) / sum(condicoes_ameacadoras_totais, na.rm = TRUE) * 100, 1),
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
        sub_registro_sinasc_muni_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "estadual") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$estado
          )
      } else if (filtros()$nivel == "regional") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$regiao
          )
      } else if (filtros()$nivel == "nacional") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
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


    # Calculando os indicadores para cada ano do período selecionado ----------
    ## Criando funções auxiliares com as colunas e os cálculos dos indicadores
    ### Para número de óbitos e taxa de mortalidade
    cols_num_obitos_taxa <- function(input) {
      list(
        tot = sprintf("obitos_perinatais_%s_todos", input$input_num_obitos_peso),
        num_taxa = sprintf("obitos_perinatais_%s_todos", input$input_taxa_de_mortalidade_peso),
        den_taxa = sprintf("nv_peso_%s", input$input_taxa_de_mortalidade_peso)
      )
    }

    indicadores_num_obitos_taxa <- function(s) {
      taxa <- round(s$num_taxa / (s$num_taxa + s$den_taxa) * 1000, 1)
      data.table::as.data.table(list(
        obitos_perinatais_totais = s$tot,
        taxa_de_mortalidade = taxa
      ))
    }


    ### Para distribuição dos óbitos por faixa de peso
    cols_distribuicao_peso <- function(input) {
      list(
        p_m1000 = sprintf("obitos_perinatais_menos_1000_%s", input$input_distribuicao_momento),
        p_1k15 = sprintf("obitos_perinatais_1000_1499_%s", input$input_distribuicao_momento),
        p_15k25 = sprintf("obitos_perinatais_1500_2499_%s", input$input_distribuicao_momento),
        p_25mais = sprintf("obitos_perinatais_2500_mais_%s", input$input_distribuicao_momento),
        p_todos = sprintf("obitos_perinatais_todos_%s", input$input_distribuicao_momento)
      )
    }

    indicadores_distribuicao_peso <- function(s) {
      porc_m1000 <- round(s$p_m1000 / s$p_todos * 100, 1)
      porc_1k15 <- round(s$p_1k15 / s$p_todos * 100, 1)
      porc_15k25 <- round(s$p_15k25 / s$p_todos * 100, 1)
      porc_25mais <- round(s$p_25mais / s$p_todos * 100, 1)
      porc_sem_peso <- round(100 - (porc_m1000 + porc_1k15 + porc_15k25 + porc_25mais), 1)
      data.table::as.data.table(list(
        porc_obito_perinatal_menos_1000 = porc_m1000,
        porc_obito_perinatal_1000_1499 = porc_1k15,
        porc_obito_perinatal_1500_2499 = porc_15k25,
        porc_obito_perinatal_2500_mais = porc_25mais,
        porc_obito_perinatal_peso_faltante = porc_sem_peso
      ))
    }

    ### Para distribuição dos óbitos por momento do óbito
    cols_distribuicao_momento <- function(input) {
      list(
        antes = sprintf("obitos_perinatais_%s_antes", input$input_distribuicao_peso),
        durante = sprintf("obitos_perinatais_%s_durante", input$input_distribuicao_peso),
        dia_0 = sprintf("obitos_perinatais_%s_0_dias", input$input_distribuicao_peso),
        dia_1_a_6 = sprintf("obitos_perinatais_%s_1_a_6_dias", input$input_distribuicao_peso),
        todos = sprintf("obitos_perinatais_%s_todos", input$input_distribuicao_peso)
      )
    }

    indicadores_distribuicao_momento <- function(s) {
      porc_antes <- round(s$antes / s$todos * 100, 1)
      porc_durante <- round(s$durante / s$todos * 100, 1)
      porc_0_dias <- round(s$dia_0 / s$todos * 100, 1)
      porc_1_a_6_dias <- round(s$dia_1_a_6 / s$todos * 100, 1)
      porc_sem_mom <- round(100 - (porc_antes + porc_durante + porc_0_dias + porc_1_a_6_dias), 1)
      data.table::as.data.table(list(
        porc_obito_perinatal_antes = porc_antes,
        porc_obito_perinatal_durante = porc_durante,
        porc_obito_perinatal_0_dias = porc_0_dias,
        porc_obito_perinatal_1_a_6_dias = porc_1_a_6_dias,
        porc_obito_perinatal_momento_faltante = porc_sem_mom
      ))
    }


    ## Pré-filtrando os dados da localidade, comparação e refeência ------------
    bloco7_perinatal_localidade <- reactive(filtra_localidade(bloco7_perinatal, filtros()))
    bloco7_perinatal_comp <- reactive(filtra_localidade(bloco7_perinatal, filtros(), comp = TRUE))
    bloco7_perinatal_referencia <- reactive(filtra_localidade(bloco7_perinatal, filtros(), referencia = TRUE))


    ## Calculando os indicadores para a localidade escolhida ------------------
    ### A função processa_bloco7 retorna dataframes do resumo e dos gráficos
    dfs_localidade_obitos_taxa <- reactive(processa_bloco7(bloco7_perinatal_localidade(), input, filtros(), cols_num_obitos_taxa, indicadores_num_obitos_taxa))
    dfs_localidade_distribuicao_peso <- reactive(processa_bloco7(bloco7_perinatal_localidade(), input, filtros(), cols_distribuicao_peso, indicadores_distribuicao_peso))
    dfs_localidade_distribuicao_momento <- reactive(processa_bloco7(bloco7_perinatal_localidade(), input, filtros(), cols_distribuicao_momento, indicadores_distribuicao_momento))
    dfs_localidade_evitaveis <- reactive(processa_causas(bloco7_evitaveis_perinatal, filtros(), pesos = input$input_evitaveis_peso, momentos = input$input_evitaveis_momento, grupos = input$input_evitaveis_grupos, prefixo_coluna = "evitaveis_perinatal"))
    dfs_localidade_principais <- reactive(processa_causas(bloco7_principais_perinatal, filtros(), pesos = input$input_principais_peso, momentos = input$input_principais_momento, grupos = input$input_principais_grupos, prefixo_coluna = "principais_perinatal"))

    ### Criando dataframes separados para o resumo
    data7_resumo_localidade_num_obitos <- eventReactive(c(filtros()$pesquisar, input$input_num_obitos_peso), {
      dfs_localidade_obitos_taxa()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_localidade_taxa_de_mortalidade <- eventReactive(c(filtros()$pesquisar, input$input_taxa_de_mortalidade_peso), {
      dfs_localidade_obitos_taxa()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_localidade_distribuicao_peso <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_momento), {
      dfs_localidade_distribuicao_peso()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_localidade_distribuicao_momento <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_peso), {
      dfs_localidade_distribuicao_momento()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_localidade_evitaveis <- reactive(dfs_localidade_evitaveis()$resumo)
    data7_resumo_localidade_principais <- reactive(dfs_localidade_principais()$resumo)

    ### Criando dataframes separados para os gráficos
    data7_num_obitos <- eventReactive(c(filtros()$pesquisar, input$input_num_obitos_peso), {
      dfs_localidade_obitos_taxa()$graficos |>
        dplyr::mutate(class = ifelse(class == "Brasil (valor de referência)", "Brasil", class))
    }, ignoreNULL = FALSE)

    data7_taxa_de_mortalidade <- eventReactive(c(filtros()$pesquisar, input$input_taxa_de_mortalidade_peso), {
      dfs_localidade_obitos_taxa()$graficos
    }, ignoreNULL = FALSE)

    data7_distribuicao_peso <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_momento), {
      dfs_localidade_distribuicao_peso()$graficos
    }, ignoreNULL = FALSE)

    data7_distribuicao_momento <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_peso), {
      dfs_localidade_distribuicao_momento()$graficos
    }, ignoreNULL = FALSE)

    data7_evitaveis <- reactive(dfs_localidade_evitaveis()$graficos)
    data7_principais <- reactive(dfs_localidade_principais()$graficos)


    ## Para a comparação selecionada ------------------------------------------
    dfs_comp_obitos_taxa <- reactive(processa_bloco7(bloco7_perinatal_comp(), input, filtros(), comp = TRUE, cols_num_obitos_taxa, indicadores_num_obitos_taxa))
    dfs_comp_distribuicao_peso <- reactive(processa_bloco7(bloco7_perinatal_comp(), input, filtros(), comp = TRUE, cols_distribuicao_peso, indicadores_distribuicao_peso))
    dfs_comp_distribuicao_momento <- reactive(processa_bloco7(bloco7_perinatal_comp(), input, filtros(), comp = TRUE, cols_distribuicao_momento, indicadores_distribuicao_momento))
    dfs_comp_evitaveis <- reactive(processa_causas(bloco7_evitaveis_perinatal, filtros(), comp = TRUE, pesos = input$input_evitaveis_peso, momentos = input$input_evitaveis_momento, grupos = input$input_evitaveis_grupos, prefixo_coluna = "evitaveis_perinatal"))
    dfs_comp_principais <- reactive(processa_causas(bloco7_principais_perinatal, filtros(), comp = TRUE, pesos = input$input_principais_peso, momentos = input$input_principais_momento, grupos = input$input_principais_grupos, prefixo_coluna = "principais_perinatal"))

    ### Criando dataframes separados para o resumo
    data7_resumo_comp_num_obitos <- eventReactive(c(filtros()$pesquisar, input$input_num_obitos_peso), {
      dfs_comp_obitos_taxa()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_comp_taxa_de_mortalidade <- eventReactive(c(filtros()$pesquisar, input$input_taxa_de_mortalidade_peso), {
      dfs_comp_obitos_taxa()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_comp_distribuicao_peso <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_momento), {
      dfs_comp_distribuicao_peso()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_comp_distribuicao_momento <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_peso), {
      dfs_comp_distribuicao_momento()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_comp_evitaveis <- reactive(dfs_comp_evitaveis()$resumo)
    data7_resumo_comp_principais <- reactive(dfs_comp_principais()$resumo)

    ### Criando dataframes separados para os gráficos
    data7_comp_num_obitos <- eventReactive(c(filtros()$pesquisar, input$input_num_obitos_peso), {
      dfs_comp_obitos_taxa()$graficos |>
        dplyr::mutate(class = ifelse(class == "Brasil (valor de referência)", "Brasil", class))
    }, ignoreNULL = FALSE)

    data7_comp_taxa_de_mortalidade <- eventReactive(c(filtros()$pesquisar, input$input_taxa_de_mortalidade_peso), {
      dfs_comp_obitos_taxa()$graficos
    }, ignoreNULL = FALSE)

    data7_comp_distribuicao_peso <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_momento), {
      dfs_comp_distribuicao_peso()$graficos
    }, ignoreNULL = FALSE)

    data7_comp_distribuicao_momento <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_peso), {
      dfs_comp_distribuicao_momento()$graficos
    }, ignoreNULL = FALSE)

    data7_comp_evitaveis <- reactive(dfs_comp_evitaveis()$graficos)
    data7_comp_principais <- reactive(dfs_comp_principais()$graficos)


    ## Para a referência ------------------------------------------------------
    ### A função processa_bloco7 retorna dataframes do resumo e dos gráficos
    dfs_referencia_obitos_taxa <- reactive(processa_bloco7(bloco7_perinatal_referencia(), input, filtros(), referencia = TRUE, cols_num_obitos_taxa, indicadores_num_obitos_taxa))
    dfs_referencia_distribuicao_peso <- reactive(processa_bloco7(bloco7_perinatal_referencia(), input, filtros(), referencia = TRUE, cols_distribuicao_peso, indicadores_distribuicao_peso))
    dfs_referencia_distribuicao_momento <- reactive(processa_bloco7(bloco7_perinatal_referencia(), input, filtros(), referencia = TRUE, cols_distribuicao_momento, indicadores_distribuicao_momento))
    dfs_referencia_evitaveis <- reactive(processa_causas(bloco7_evitaveis_perinatal, filtros(), referencia = TRUE, pesos = input$input_evitaveis_peso, momentos = input$input_evitaveis_momento, grupos = input$input_evitaveis_grupos, prefixo_coluna = "evitaveis_perinatal"))
    dfs_referencia_principais <- reactive(processa_causas(bloco7_principais_perinatal, filtros(), referencia = TRUE, pesos = input$input_principais_peso, momentos = input$input_principais_momento, grupos = input$input_principais_grupos, prefixo_coluna = "principais_perinatal"))

    ### Criando dataframes separados para o resumo
    data7_resumo_referencia_num_obitos <- eventReactive(c(filtros()$pesquisar, input$input_num_obitos_peso), {
      dfs_referencia_obitos_taxa()$resumo
    }, ignoreNULL = FALSE)

    referencia_oms <- reactive(ifelse(length(input$input_taxa_de_mortalidade_peso) == 5, TRUE, FALSE))

    data7_resumo_referencia_taxa_de_mortalidade <- eventReactive(c(filtros()$pesquisar, input$input_taxa_de_mortalidade_peso), {
      dfs_referencia_obitos_taxa()$resumo |>
        dplyr::mutate(taxa_de_mortalidade = ifelse(referencia_oms() == TRUE, 8.7, taxa_de_mortalidade))
    }, ignoreNULL = FALSE)

    data7_resumo_referencia_distribuicao_peso <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_momento), {
      dfs_referencia_distribuicao_peso()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_referencia_distribuicao_momento <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_peso), {
      dfs_referencia_distribuicao_momento()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_referencia_evitaveis <- reactive(dfs_referencia_evitaveis()$resumo)
    data7_resumo_referencia_principais <- reactive(dfs_referencia_principais()$resumo)

    ### Criando dataframes separados para os gráficos
    renomear_condicional <- function(df) {
      dplyr::rename_with(df, ~paste0("br_", .x), dplyr::contains("porc_obito"))
    }

    data7_referencia_taxa_de_mortalidade <- eventReactive(c(filtros()$pesquisar, input$input_taxa_de_mortalidade_peso), {
      dfs_referencia_obitos_taxa()$graficos |>
        dplyr::mutate(taxa_de_mortalidade = ifelse(referencia_oms() == TRUE, 8.7, taxa_de_mortalidade))
    },
    ignoreNULL = FALSE
    )

    data7_referencia_distribuicao_peso <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_momento), {
      dfs_referencia_distribuicao_peso()$graficos |>
        dplyr::mutate(localidade_comparacao = "Média nacional") |>
        renomear_condicional()
    },
    ignoreNULL = FALSE
    )

    data7_referencia_distribuicao_momento <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_peso), {
      dfs_referencia_distribuicao_momento()$graficos |>
        dplyr::mutate(localidade_comparacao = "Média nacional") |>
        renomear_condicional()
    },
    ignoreNULL = FALSE
    )

    data7_referencia_evitaveis <- reactive(
      dfs_referencia_evitaveis()$graficos |>
        renomear_condicional() |>
        dplyr::select(!class)
    )
    data7_referencia_principais <- reactive(
      dfs_referencia_principais()$graficos |>
        renomear_condicional() |>
        dplyr::select(!class)
    )


    ## Decidindo qual resumo mostrar ------------------------------------------
    escolhe_resumo <- function(comparar, localidade_resumo, df_localidade, df_comparacao) {
      if (comparar == "Não") {
        df_localidade
      } else {
        req(localidade_resumo)
        if (localidade_resumo == "escolha1") {
          df_localidade
        } else {
          df_comparacao
        }
      }
    }

    data7_resumo_num_obitos <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_num_obitos(), data7_resumo_comp_num_obitos()))
    data7_resumo_taxa_de_mortalidade <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_taxa_de_mortalidade(), data7_resumo_comp_taxa_de_mortalidade()))
    data7_resumo_distribuicao_peso <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_distribuicao_peso(), data7_resumo_comp_distribuicao_peso()))
    data7_resumo_distribuicao_momento <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_distribuicao_momento(), data7_resumo_comp_distribuicao_momento()))
    data7_resumo_evitaveis <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_evitaveis(), data7_resumo_comp_evitaveis()))
    data7_resumo_principais <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_principais(), data7_resumo_comp_principais()))


    ## Criando os outputs das caixinhas ---------------------------------------
    ### Número de óbitos perinatais -----------------------------------------------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_num_obitos",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Número de óbitos perinatais". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_num_obitos <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_num_obitos_peso), {
      output_pronto_num_obitos(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_num_obitos", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_perinatal_num_obitos <- renderUI({
      output_pronto_num_obitos(TRUE)
      cria_caixa_server(
        dados = data7_resumo_num_obitos(),
        indicador = "obitos_perinatais_totais",
        titulo = "Número de óbitos perinatais",
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia_num_obitos()[["obitos_perinatais_totais"]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          nivel_selecionado() == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        pagina = "bloco_7",
        tamanho_caixa = 330,
        nivel_de_analise = nivel_selecionado(),
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_num_obitos(), {
      if (output_pronto_num_obitos()) {
        shinyjs::show(id = "mostrar_botao_num_obitos", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    ### Taxa de mortalidade perinatal ---------------------------------------------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_taxa_de_mortalidade",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Taxa de mortalidade perinatal por 1000 nascidos vivos". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_taxa_de_mortalidade <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_taxa_de_mortalidade_peso), {
      output_pronto_taxa_de_mortalidade(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_taxa_de_mortalidade", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_perinatal_taxa_de_mortalidade <- renderUI({
      output_pronto_taxa_de_mortalidade(TRUE)
      cria_caixa_server(
        dados = data7_resumo_taxa_de_mortalidade(),
        indicador = "taxa_de_mortalidade",
        titulo = "Taxa de mortalidade perinatal por 1000 nascidos vivos",
        tem_meta = ifelse(referencia_oms() == TRUE, TRUE, FALSE),
        tipo_referencia = ifelse(referencia_oms() == TRUE, "meta ODS", ""),
        valor_de_referencia = ifelse(
          data7_resumo_referencia_taxa_de_mortalidade()[["taxa_de_mortalidade"]] > 0,
          data7_resumo_referencia_taxa_de_mortalidade()[["taxa_de_mortalidade"]],
          NaN
        ),
        tipo = "taxa",
        invertido = FALSE,
        pagina = "bloco_7",
        tamanho_caixa = 330,
        nivel_de_analise = nivel_selecionado(),
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_taxa_de_mortalidade(), {
      if (output_pronto_taxa_de_mortalidade()) {
        shinyjs::show(id = "mostrar_botao_taxa_de_mortalidade", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    ### Distribuição percentual do momento do óbito por faixa de peso ---------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_distribuicao_peso",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Distribuição percentual das faixas de peso por momento do óbito perinatal". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_distribuicao_peso <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_distribuicao_momento), {
      output_pronto_distribuicao_peso(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_distribuicao_peso", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_perinatal_distribuicao_peso <- renderUI({
      output_pronto_distribuicao_peso(TRUE)
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo_distribuicao_peso(),
        indicador = "perinatal peso por momento do obito",
        titulo = "Dentre os óbitos perinatais selecionados,",
        tamanho_caixa = 330,
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_distribuicao_peso(), {
      if (output_pronto_distribuicao_peso()) {
        shinyjs::show(id = "mostrar_botao_distribuicao_peso", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    ### Distribuição percentual das faixas de peso por momento do óbito -------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_distribuicao_momento",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Distribuição percentual do momento do óbito perinatal por faixa de peso". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_distribuicao_momento <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_distribuicao_peso), {
      output_pronto_distribuicao_momento(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_distribuicao_momento", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_perinatal_distribuicao_momento <- renderUI({
      output_pronto_distribuicao_momento(TRUE)
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo_distribuicao_momento(),
        indicador = "perinatal momento do obito por peso",
        titulo = "Dentre os óbitos perinatais selecionados,",
        tamanho_caixa = 330,
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_distribuicao_momento(), {
      if (output_pronto_distribuicao_momento()) {
        shinyjs::show(id = "mostrar_botao_distribuicao_momento", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    #### Distribuição percentual do óbito segundo análise de evitabilidade ----
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_evitaveis",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Distribuição percentual dos óbitos perinatais segundo análise de evitabilidade". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_evitaveis <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_evitaveis_peso, input$input_evitaveis_momento, input$input_evitaveis_grupos), {
      output_pronto_evitaveis(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_evitaveis", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_perinatal_evitaveis <- renderUI({
      output_pronto_evitaveis(TRUE)
      cria_caixa_server(
        dados = data7_resumo_evitaveis(),
        indicador = "porc_obitos_evitaveis",
        titulo = "Porcentagem de óbitos perinatais potencialmente evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia_evitaveis()$porc_obitos_evitaveis,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "bloco_7",
        tamanho_caixa = 330,
        nivel_de_analise = nivel_selecionado(),
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_evitaveis(), {
      if (output_pronto_evitaveis()) {
        shinyjs::show(id = "mostrar_botao_evitaveis", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    #### Distribuição percentual do óbito por grupo de causas -----------------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_principais",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Distribuição percentual dos óbitos perinatais por grupos de causas". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_principais <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_principais_peso, input$input_principais_momento, input$input_principais_grupos), {
      output_pronto_principais(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_principais", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_perinatal_principais <- renderUI({
      output_pronto_principais(TRUE)
      cria_caixa_principais_evitaveis_bloco7(
        dados = data7_resumo_principais(),
        titulo = "Dentre os óbitos perinatais selecionados,",
        tamanho_caixa = 330,
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_principais(), {
      if (output_pronto_principais()) {
        shinyjs::show(id = "mostrar_botao_principais", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    # Criando os outputs dos gráficos -----------------------------------------
    ## Definindo as cores usadas nos gráficos ---------------------------------
    cols <- c("#51127CFF", "#b73779", "#fc8961", "#000004FF", "#f1605d")

    ## Número de óbitos perinatais ------------------------------------------------
    output$plot_obitos <- highcharter::renderHighchart({
      grafico_base <- highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_add_series(
          data = data7_num_obitos(),
          type = "line",
          highcharter::hcaes(x = ano, y = obitos_perinatais_totais, group = class, colour = class)
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
        highcharter::hc_colors(cols)

      if (filtros()$comparar == "Não") {
        grafico_base
      } else {
        grafico_base <- grafico_base |>
          highcharter::hc_add_series(
            data = data7_comp_num_obitos(),
            type = "line",
            highcharter::hcaes(x = ano, y = obitos_perinatais_totais, group = class, colour = class)
          )
      }
    })


    ## Taxa de mortalidade perinatal por 1000 nascidos vivos ----------------------
    output$plot_taxa_de_mortalidade <- highcharter::renderHighchart({
      grafico_base <- highcharter::highchart() |>
        highcharter::hc_add_dependency("modules/series-label.js") |>
        highcharter::hc_add_series(
          data = data7_taxa_de_mortalidade(),
          type = "line",
          name = ifelse(referencia_oms() == FALSE, data7_taxa_de_mortalidade()$class, gsub(" \\(valor de referência\\)", "", data7_taxa_de_mortalidade()$class)),
          highcharter::hcaes(x = ano, y = taxa_de_mortalidade, group = class, colour = class)
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            label = list(enabled = TRUE),
            allowPointSelect = TRUE
          )
        ) |>
        highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
        highcharter::hc_colors(cols)

      if ((any(c(filtros()$nivel, filtros()$nivel2) == "nacional") & referencia_oms() == FALSE) | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
        grafico_base
      } else {
        grafico_base <- grafico_base |>
          highcharter::hc_add_series(
            data = data7_referencia_taxa_de_mortalidade(),
            type = "line",
            name = ifelse(referencia_oms() == FALSE, "Referência (média nacional)", "Referência (meta ODS)"),
            highcharter::hcaes(x = ano, y = taxa_de_mortalidade, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
      }

      if (filtros()$comparar == "Não") {
        grafico_base
      } else {
        grafico_base <- grafico_base |>
          highcharter::hc_add_series(
            data = data7_comp_taxa_de_mortalidade(),
            type = "line",
            name = ifelse(referencia_oms() == FALSE, data7_comp_taxa_de_mortalidade()$class, gsub(" \\(valor de referência\\)", "", data7_comp_taxa_de_mortalidade()$class)),
            highcharter::hcaes(x = ano, y = taxa_de_mortalidade, group = class, colour = class)
          )
      }
    })

    ## Distribuição percentual do momento do óbito perinatal por faixa de peso ----
    data7_distribuicao_momento_juncao <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_peso), {
      if (filtros()$comparar == "Não") {
        dplyr::full_join(
          data7_distribuicao_momento(),
          data7_referencia_distribuicao_momento() |> dplyr::rename(class2 = class),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]),
            class = ifelse(class == "Brasil (valor de referência)", "Brasil", class)
          )
      } else {
        dplyr::full_join(
          data7_distribuicao_momento(),
          data7_comp_distribuicao_momento() |>
            dplyr::rename(localidade_comparacao = class) |>
            dplyr::rename_with(~paste0("br_", .x), dplyr::contains("porc_obito_perinatal")),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]),
            class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
            localidade_comparacao = ifelse(localidade_comparacao == "Brasil (valor de referência)", "Brasil", localidade_comparacao)
          )
      }
    },
    ignoreNULL = FALSE
    )

    output$plot_distribuicao_momento <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "Sem informação",
          data = data7_distribuicao_momento_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_momento_faltante),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_momento_faltante:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1 a 6 dias de vida",
          data =  data7_distribuicao_momento_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_1_a_6_dias),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_1_a_6_dias:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Dia 0 de vida",
          data =  data7_distribuicao_momento_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_0_dias),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_0_dias:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Durante o parto",
          data = data7_distribuicao_momento_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_durante),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_durante:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Antes do parto",
          data = data7_distribuicao_momento_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_antes),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_antes:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_distribuicao_momento_juncao()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos"), min = 0, max = 100)
    })


    ## Distribuição percentual das faixas de peso por momento do óbito perinatal ----
    data7_distribuicao_peso_juncao <- eventReactive(c(filtros()$pesquisar, input$input_distribuicao_momento), {
      if (filtros()$comparar == "Não") {
        dplyr::full_join(
          data7_distribuicao_peso(),
          data7_referencia_distribuicao_peso() |> dplyr::rename(class2 = class),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]),
            class = ifelse(class == "Brasil (valor de referência)", "Brasil", class)
          )
      } else {
        dplyr::full_join(
          data7_distribuicao_peso(),
          data7_comp_distribuicao_peso() |>
            dplyr::rename(localidade_comparacao = class) |>
            dplyr::rename_with(~paste0("br_", .x), dplyr::contains("porc_obito_perinatal")),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]),
            class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
            localidade_comparacao = ifelse(localidade_comparacao == "Brasil (valor de referência)", "Brasil", localidade_comparacao)
          )
      }
    },
    ignoreNULL = FALSE
    )

    output$plot_distribuicao_peso <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "Sem informação",
          data =  data7_distribuicao_peso_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_peso_faltante),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_peso_faltante:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Maior ou igual a 2500g",
          data =  data7_distribuicao_peso_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_2500_mais),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_2500_mais:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500 a 2499g",
          data =  data7_distribuicao_peso_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_1500_2499),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_1500_2499:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1000 a 1499g",
          data =  data7_distribuicao_peso_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_1000_1499),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_1000_1499:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1000g",
          data =  data7_distribuicao_peso_juncao(),
          highcharter::hcaes(x = ano, y = porc_obito_perinatal_menos_1000),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_porc_obito_perinatal_menos_1000:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_distribuicao_peso_juncao()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos"), min = 0, max = 100)
    })


    ## Distribuição percentual dos óbitos perinatais por grupos de causas ---------
    data7_principais_completo <- reactive({
      validate(
        need(
          nrow(data7_principais()) != 0,
          "Não existem ocorrências de óbitos perinatais por grupos de causas para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data7_principais(), data7_referencia_principais())
    })

    data7_comp_principais_completo <- reactive({
      validate(
        need(
          nrow(data7_comp_principais()) != 0,
          "Não existem ocorrências de óbitos perinatais por grupos de causas para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data7_comp_principais(), data7_referencia_principais())
    })

    output$plot_causas_principais <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_principais_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            )
          )
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_principais_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_principais_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = FALSE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 1
          )

      }

      grafico_base |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_legend(reversed = FALSE, title = list(text = "Grupo de causas")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data7_principais_completo()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data7_principais_completo()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_principais_completo()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos perinatais"), min = 0, max = 100)
    })


    ## Distribuição percentual dos óbitos perinatais segundo análise de evitabilidade ---------
    data7_evitaveis_completo <- reactive({
      validate(
        need(
          nrow(data7_evitaveis()) != 0,
          "Não existem ocorrências de óbitos perinatais por grupos de causas para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data7_evitaveis(), data7_referencia_evitaveis())
    })

    data7_comp_evitaveis_completo <- reactive({
      validate(
        need(
          nrow(data7_comp_evitaveis()) != 0,
          "Não existem ocorrências de óbitos perinatais por grupos de causas para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data7_comp_evitaveis(), data7_referencia_evitaveis())
    })

    output$plot_causas_evitaveis <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_evitaveis_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            )
          )
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_evitaveis_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_evitaveis_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = FALSE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 1
          )
      }

      grafico_base |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_legend(reversed = FALSE, title = list(text = "Grupo por análise de evitabilidade")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data7_evitaveis_completo()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data7_evitaveis_completo()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_evitaveis_completo()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos perinatais"), min = 0, max = 100)

    })
  })
}

## To be copied in the UI
# mod_bloco_7_perinatal_ui("bloco_7_perinatal_1")

## To be copied in the server
# mod_bloco_7_perinatal_server("bloco_7_perinatal_1")
