#' bloco_8 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_8_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Garbage codes, causas principais e causas evitáveis: tabelas"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    fluidRow(
      id = ns("sem_comparacao"),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências dos garbage codes para óbito materno &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_materno_garbage")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências dos garbage codes para óbito fetal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_fetal_garbage")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências dos garbage codes para óbito neonatal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_neonat_garbage")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências das causas principais para óbito fetal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_fetal_causas")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências das causas principais para óbito neonatal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_neonat_causas")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências das causas evitáveis para óbito fetal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_fetal_evitaveis")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências das causas evitáveis para óbito neonatal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_neonat_evitaveis")))
        )
      ),

      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências dos grupos de causa para óbito fetal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_fetal_grupos")))
        )
      ),

      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências dos grupos de causa para óbito neonatal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_neonat_grupos")))
        )
      )

    ),
    shinyjs::hidden(
      fluidRow(
        id = ns("com_comparacao"),
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
              HTML("<b style='font-size:19px'> Porcentagem de óbitos maternos preenchidos com garbage codes &nbsp;</b>"),
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
                shinyWidgets::pickerInput(
                  inputId = ns("cids_garbage_materno"),
                  label = "Garbage codes",
                  options = list(placeholder = "Selecione os garbage codes", `actions-box` = TRUE),
                  choices = c(
                    "D39 Neoplasia de comportamento incerto ou desconhecido dos órgãos genitais femininos" = "garbage_materno_d39",
                    "F53 Transtornos mentais e comportamentais associados ao puerpério, não classificados em outra parte" = "garbage_materno_f53",
                    "O94 Sequelas de complicações da gravidez, parto e puerpério" = "garbage_materno_o94",
                    "095 Morte obstétrica de causa não especificada" = "garbage_materno_o95"
                  ),
                  selected = names(bloco8_graficos)[grepl("garbage_materno", names(bloco8_graficos))],
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_materno"), height = 360))
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
              HTML("<b style='font-size:19px'> Porcentagem de óbitos fetais preenchidos com garbage codes &nbsp;</b>"),
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
                shinyWidgets::pickerInput(
                  inputId = ns("cids_garbage_fetal"),
                  label = "Grupos de garbage codes",
                  options = list(placeholder = "Selecione os grupos de garbage codes", `actions-box` = TRUE),
                  choices = c(
                    "(P90-P96) Outros transtornos originados no período perinatal" = "garbage_fetal_p90_p96",
                    "(Q10-Q18) Malformações congênitas do olho, do ouvido, da face e do pescoço" = "garbage_fetal_q10_q18",
                    "(Q35-Q37) Fenda labial e fenda palatina" = "garbage_fetal_q35_q37",
                    "(Q80-Q89) Outras malformações congênitas" = "garbage_fetal_q80_q89"
                  ),
                  selected = names(bloco8_graficos)[grepl("garbage_fetal", names(bloco8_graficos))],
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_fetal"), height = 360))
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
              HTML("<b style='font-size:19px'> Porcentagem de principais causas para óbitos fetais &nbsp;</b>"),
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
                shinyWidgets::pickerInput(
                  inputId = ns("cids_causas_fetal"),
                  label = "Grupos de principais causas",
                  options = list(placeholder = "Selecione os grupos de principais causas", `actions-box` = TRUE),
                  choices = c(
                    "(P00-P04) Feto e recém-nascido afetados por fatores maternos e por complicações da gravidez, do trabalho de parto e do parto" = "fetal_causas_p00_p04",
                    "(P05-P08) Transtornos relacionados com a duração da gestação e com o crescimento fetal" = "fetal_causas_p05_p08",
                    "(P10-P15) Traumatismo de parto" = "fetal_causas_p10_p15",
                    "(P20-P29) Transtornos respiratórios e cardiovasculares específicos do período perinatal" = "fetal_causas_p20_p29",
                    "(P35-P39) Infecções específicas do período perinatal" = "fetal_causas_p35_p39",
                    "(P50-P61) Transtornos hemorrágicos e hematológicos do feto e do recém-nascido" = "fetal_causas_p50_p61",
                    "(P70-P74) Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido" = "fetal_causas_p70_p74",
                    "(P75-P78) Transtornos do aparelho digestivo do feto ou do recém-nascido" = "fetal_causas_p75_p78",
                    "(P80-P83) Afecções comprometendo o tegumento e a regulação térmica do feto e do recém-nascido" = "fetal_causas_p80_p83",
                    "(P90-P96) Outros transtornos originados no período perinatal" = "fetal_causas_p90_p96",
                    "(Q00-Q99) Anomalias congênitas" = "fetal_causas_q00_q99",
                    "(J00-J99) Respiratórias" = "fetal_causas_j00_j99",
                    "(A00-B99) Infecciosas" = "fetal_causas_a00_b99",
                    "Outros" = "fetal_causas_outros"

                  ),
                  selected = names(bloco8_graficos)[grepl("fetal_causas", names(bloco8_graficos))],
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_causas_fetal"), height = 360))
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
              HTML("<b style='font-size:19px'> Porcentagem de principais causas para óbitos neonatais &nbsp;</b>"),
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
                shinyWidgets::pickerInput(
                  inputId = ns("cids_causas_neonatal"),
                  label = "Grupos de principais causas",
                  options = list(placeholder = "Selecione os grupos de principais causas", `actions-box` = TRUE),
                  choices = c(
                    "(P00-P04) Feto e recém-nascido afetados por fatores maternos e por complicações da gravidez, do trabalho de parto e do parto" = "neonatal_causas_p00_p04",
                    "(P05-P08) Transtornos relacionados com a duração da gestação e com o crescimento neonatal" = "neonatal_causas_p05_p08",
                    "(P10-P15) Traumatismo de parto" = "neonatal_causas_p10_p15",
                    "(P20-P29) Transtornos respiratórios e cardiovasculares específicos do período perinatal" = "neonatal_causas_p20_p29",
                    "(P35-P39) Infecções específicas do período perinatal" = "neonatal_causas_p35_p39",
                    "(P50-P61) Transtornos hemorrágicos e hematológicos do feto e do recém-nascido" = "neonatal_causas_p50_p61",
                    "(P70-P74) Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido" = "neonatal_causas_p70_p74",
                    "(P75-P78) Transtornos do aparelho digestivo do feto ou do recém-nascido" = "neonatal_causas_p75_p78",
                    "(P80-P83) Afecções comprometendo o tegumento e a regulação térmica do feto e do recém-nascido" = "neonatal_causas_p80_p83",
                    "(P90-P96) Outros transtornos originados no período perinatal" = "neonatal_causas_p90_p96",
                    "(Q00-Q99) Anomalias congênitas" = "neonatal_causas_q00_q99",
                    "(J00-J99) Respiratórias" = "neonatal_causas_j00_j99",
                    "(A00-B99) Infecciosas" = "neonatal_causas_a00_b99",
                    "Outros" = "neonatal_causas_outros"

                  ),
                  selected = names(bloco8_graficos)[grepl("neonatal_causas", names(bloco8_graficos))],
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_causas_neonatal"), height = 360))
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
              HTML("<b style='font-size:19px'> Porcentagem de causas evitáveis para óbitos fetais &nbsp;</b>"),
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
                shinyWidgets::pickerInput(
                  inputId = ns("cids_evitaveis_fetal"),
                  label = "Grupos de principais causas",
                  options = list(placeholder = "Selecione os grupos de principais causas", `actions-box` = TRUE),
                  choices = c(
                    "Reduzível pelas ações de imunização" = "fetal_evitaveis_imunoprevencao",
                    "Reduzíveis por adequada atenção à mulher na gestação" = "fetal_evitaveis_mulher_gestacao",
                    "Reduzíveis por adequada atenção à mulher no parto" = "fetal_evitaveis_parto",
                    "Reduzíveis por adequada atenção ao recém-nascido" = "fetal_evitaveis_recem_nascido",
                    "Reduzíveis por ações de diagnóstico e tratamento adequado" = "fetal_evitaveis_tratamento",
                    "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "fetal_evitaveis_saude",
                    "Causas mal definidas" = "fetal_evitaveis_mal_definidas",
                    "Demais causas (não claramente evitáveis)" = "fetal_evitaveis_outros"
                  ),
                  selected = names(bloco8_graficos)[grepl("fetal_evitaveis", names(bloco8_graficos))],
                  multiple = TRUE,
                  width = "100%"
                ),
                shinyWidgets::pickerInput(
                  inputId = ns("peso_evitaveis_fetal"),
                  label = "Grupos de principais causas",
                  options = list(placeholder = "Selecione o peso", `actions-box` = TRUE),
                  choices = c(
                    "Sem informação", ">= 1000 g", ">= 1500 g"
                  ),
                  selected = c(
                    "Sem informação", ">= 1000 g", ">= 1500 g"
                  ),
                  multiple = TRUE,
                  width = "100%"
                ),

              )
            ),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_fetal"), height = 360))
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
              HTML("<b style='font-size:19px'> Porcentagem de causas evitáveis para óbitos neonatais &nbsp;</b>"),
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
                shinyWidgets::pickerInput(
                  inputId = ns("cids_evitaveis_neonatal"),
                  label = "Grupos de principais causas",
                  options = list(placeholder = "Selecione os grupos de principais causas", `actions-box` = TRUE),
                  choices = c(
                    "Reduzível pelas ações de imunização" = "neonatal_evitaveis_imunoprevencao",
                    "Reduzíveis por adequada atenção à mulher na gestação" = "neonatal_evitaveis_mulher_gestacao",
                    "Reduzíveis por adequada atenção à mulher no parto" = "neonatal_evitaveis_parto",
                    "Reduzíveis por adequada atenção ao recém-nascido" = "neonatal_evitaveis_recem_nascido",
                    "Reduzíveis por ações de diagnóstico e tratamento adequado" = "neonatal_evitaveis_tratamento",
                    "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "neonatal_evitaveis_saude",
                    "Causas mal definidas" = "neonatal_evitaveis_mal_definidas",
                    "Demais causas (não claramente evitáveis)" = "neonatal_evitaveis_outros"
                  ),
                  selected = names(bloco8_graficos)[grepl("neonatal_evitaveis", names(bloco8_graficos))],
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_neonatal"), height = 360))
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
              HTML("<b style='font-size:19px'> Porcentagem de grupos de causas para óbitos fetais &nbsp;</b>"),
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
                shinyWidgets::pickerInput(
                  inputId = ns("cids_grupos_fetal"),
                  label = "Grupos de causas",
                  options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE),
                  choices = c(
                    "Prematuridade" = "fetal_grupos_prematuridade",
                    "Infecções" = "fetal_grupos_infeccoes",
                    "Asfixia/Hipóxia" = "fetal_grupos_asfixia",
                    "Má formação congênita" = "fetal_grupos_ma_formacao",
                    "Afecções respiratórias dos recém nascidos" = "fetal_grupos_respiratorias",
                    "Fatores maternos relacionados à gravidez " = "fetal_grupos_gravidez",
                    "Transtornos cardiorrespiratórios originados do período perinatal" = "fetal_grupos_cardiorrespiratorias",
                    "Afecções originais no período perinatal" = "fetal_grupos_afeccoes_perinatal",
                    "Mal definidas" = "fetal_grupos_mal_definida",
                    "Demais causas" = "fetal_grupos_outros"
                  ),
                  selected = names(bloco8_graficos)[grepl("fetal_grupos", names(bloco8_graficos))],
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_fetal"), height = 360))
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
              HTML("<b style='font-size:19px'> Porcentagem de grupos de causas para óbitos neonatais &nbsp;</b>"),
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
                shinyWidgets::pickerInput(
                  inputId = ns("cids_grupos_neonat"),
                  label = "Grupos de causas",
                  options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE),
                  choices = c(
                    "Prematuridade" = "neonat_grupos_prematuridade",
                    "Infecções" = "neonat_grupos_infeccoes",
                    "Asfixia/Hipóxia" = "neonat_grupos_asfixia",
                    "Má formação congênita" = "neonat_grupos_ma_formacao",
                    "Afecções respiratórias dos recém nascidos" = "neonat_grupos_respiratorias",
                    "Fatores maternos relacionados à gravidez " = "neonat_grupos_gravidez",
                    "Transtornos cardiorrespiratórios originados do período perinatal" = "neonat_grupos_cardiorrespiratorias",
                    "Afecções originais no período perinatal" = "neonat_grupos_afeccoes_perinatal",
                    "Mal definidas" = "neonat_grupos_mal_definida",
                    "Demais causas" = "neonat_grupos_outros"
                  ),
                  selected = names(bloco8_graficos)[grepl("neonat_grupos", names(bloco8_graficos))],
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_neonat"), height = 360))
          )
        ),
        # Adicionar outras columns com width 6 a partir daqui (ele já vai entender que vão ficar dois gráficos por linha)
      )
    )
  )
}

#' bloco_8 Server Functions
#'
#' @noRd

mod_bloco_8_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(filtros()$pesquisar, {
      if (filtros()$comparar == "Não") {
        shinyjs::hide(id = "com_comparacao", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "sem_comparacao", anim = TRUE, animType = "slide", time = 0.8)
      } else {
        shinyjs::hide(id = "sem_comparacao", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "com_comparacao", anim = TRUE, animType = "slide", time = 0.8)
      }
    }, ignoreNULL = FALSE)


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



    ##### Definindo as cores para os gráficos #####
    cols <- c("#2c115f", "#b73779", "#fc8961")



    # tabela garbage codes obitos maternos
    data8_materno_garbage <- reactive({
      bloco8_materno_garbage |>
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
        dplyr::group_by(capitulo_cid10, causabas_categoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_materno_garbage <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_materno_garbage()) != 0,
          "Não existem ocorrências de garbage codes para a localidade e períodos selecionados."
        )
      )
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

      data8_materno_garbage() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "causabas_categoria"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
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



    # tabela garbage codes obitos fetais
    bloco8_fetal_garbage$causabas_grupo <- factor(bloco8_fetal_garbage$causabas_grupo,
                                                  levels = c("Outros transtornos originados no período perinatal",
                                                             "Malformações congênitas do olho, do ouvido, da face e do pescoço",
                                                             "Fenda labial e fenda palatina",
                                                             "Outras malformações congênitas",
                                                             "Anomalias cromossômicas não classificadas em outra parte",
                                                             "Transtornos respiratórios e cardiovasculares específicos do período perinatal"))

    data8_fetal_garbage <- reactive({
      bloco8_fetal_garbage |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_fetal_garbage <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_fetal_garbage()) != 0,
          "Não existem ocorrências de garbage codes para a localidade e períodos selecionados."
        )
      )
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

      data8_fetal_garbage() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
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

    # tabela garbage codes obitos neonatais
    data8_neonat_garbage <- reactive({
      bloco8_neonat_garbage |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano, idade_dias) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_neonat_garbage <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_neonat_garbage()) != 0,
          "Não existem ocorrências de garbage codes para a localidade e períodos selecionados."
        )
      )
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

      data8_neonat_garbage() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria", "idade_dias"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            idade_dias = reactable::colDef(
              name = "Grupo de idade",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos"))
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


    # tabela principais causas obitos fetais
    data8_fetal_causas <- reactive({
      bloco8_fetal_causas |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_fetal_causas <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_fetal_causas()) != 0,
          "Não existem ocorrências de óbitos fetais pelas causas mais comuns para a localidade e períodos selecionados."
        )
      )
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

      data8_fetal_causas() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
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

    # tabela principais causas obitos neonatais
    data8_neonat_causas <- reactive({
      bloco8_neonat_causas |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano, idade_dias) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_neonat_causas <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_neonat_causas()) != 0,
          "Não existem ocorrências de óbitos neonatais pelas causas mais comuns para a localidade e períodos selecionados."
        )
      )
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

      data8_neonat_causas() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria", "idade_dias"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            idade_dias = reactable::colDef(
              name = "Grupo de idade",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos"))
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

    # tabela evitaveis obitos fetais
    data8_fetal_evitaveis <- reactive({
      bloco8_fetal_evitaveis |>
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
        dplyr::group_by(capitulo_cid10, grupo_cid10, causabas_categoria, grupo_cid, faixa_de_peso, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_fetal_evitaveis <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_fetal_evitaveis()) != 0,
          "Não existem ocorrências de óbitos fetais por causas evitáveis para a localidade e períodos selecionados."
        )
      )
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

      data8_fetal_evitaveis() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "grupo_cid10", "causabas_categoria", "grupo_cid", "faixa_de_peso"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            grupo_cid = reactable::colDef(
              name = "Grupo de causas",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            faixa_de_peso = reactable::colDef(
              name = "Faixa de peso",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = glue::glue("Todos")))
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

    # tabela evitaveis obitos neonatais
    data8_neonat_evitaveis <- reactive({
      bloco8_neonat_evitaveis |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano, idade_dias, grupo) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_neonat_evitaveis <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_neonat_evitaveis()) != 0,
          "Não existem ocorrências de óbitos neonatais por causas evitáveis para a localidade e períodos selecionados."
        )
      )
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

      data8_neonat_evitaveis() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria", "grupo", "idade_dias"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            grupo = reactable::colDef(
              name = "Grupo de causas",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos"))
            ),
            idade_dias = reactable::colDef(
              name = "Grupo de idade",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos"))
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

    data_filtrada_aux <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
        dplyr::group_by(ano)
    })

    data_filtrada_comp_aux <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
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
        dplyr::group_by(ano)
    })

    data_plot_garbage_materno <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input$cids_garbage_materno))),
          obitos_maternos_totais = sum(obitos_maternos_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_maternos_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_garbage_materno_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input$cids_garbage_materno))),
          obitos_maternos_totais = sum(obitos_maternos_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_maternos_totais * 100, 1),
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

    data_plot_garbage_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input$cids_garbage_fetal))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_fetais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })


    data_plot_garbage_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input$cids_garbage_fetal))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_fetais_totais * 100, 1),
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

    observe(print(data_filtrada_aux()))
    observe(print(names(data_filtrada_aux())))
    observe(print(input$cids_garbage_materno))

    # Criando o gráfico da porcentagem de garbage codes p/ óbitos maternos --------
    output$plot_garbage_materno <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_materno(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_garbage_materno_comp(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos preenchidos com garbage codes"), min = 0, max = 100)

    })


    # Criando o gráfico da porcentagem de garbage codes p/ óbitos fetais --------
    output$plot_garbage_fetal <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_fetal(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_garbage_fetal_comp(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos preenchidos com garbage codes"), min = 0, max = 100)

    })

    # Criando o gráfico da porcentagem de principais causas p/ óbitos fetais --------
    data_plot_causas_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_causas_fetal = sum(dplyr::across(dplyr::all_of(input$cids_causas_fetal))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_causas_fetal = round(obitos_causas_fetal / obitos_fetais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    observe(print(data_plot_causas_fetal()))

    data_plot_causas_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_causas_fetal = sum(dplyr::across(dplyr::all_of(input$cids_causas_fetal))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_causas_fetal = round(obitos_causas_fetal / obitos_fetais_totais * 100, 1),
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

    output$plot_causas_fetal <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_causas_fetal(),
          highcharter::hcaes(x = ano, y = prop_causas_fetal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_causas_fetal_comp(),
          highcharter::hcaes(x = ano, y = prop_causas_fetal, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos principais causas"), min = 0, max = 100)

    })

    # Criando o gráfico da porcentagem de principais causas p/ óbitos neonatais --------

    data_plot_causas_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_causas_neonatal = sum(dplyr::across(dplyr::all_of(input$cids_causas_neonatal))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_causas_neonatal = round(obitos_causas_neonatal / obitos_neonatais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    observe(print(data_plot_causas_neonatal()))

    data_plot_causas_neonatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_causas_neonatal = sum(dplyr::across(dplyr::all_of(input$cids_causas_neonatal))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_causas_neonatal = round(obitos_causas_neonatal / obitos_neonatais_totais * 100, 1),
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

    output$plot_causas_neonatal <- highcharter::renderHighchart({



      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_causas_neonatal(),
          highcharter::hcaes(x = ano, y = prop_causas_neonatal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_causas_neonatal_comp(),
          highcharter::hcaes(x = ano, y = prop_causas_neonatal, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos principais causas"), min = 0, max = 100)

    })


    # Criando o gráfico da porcentagem de causas evitáveis p/ óbitos fetais --------
    data_plot_evitaveis_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_evitaveis_fetal = sum(dplyr::across(dplyr::all_of(input$cids_evitaveis_fetal))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_evitaveis_fetal = round(obitos_evitaveis_fetal / obitos_fetais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    observe(print(data_plot_evitaveis_fetal()))

    data_plot_evitaveis_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_evitaveis_fetal = sum(dplyr::across(dplyr::all_of(input$cids_evitaveis_fetal))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_evitaveis_fetal = round(obitos_evitaveis_fetal / obitos_fetais_totais * 100, 1),
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

    output$plot_evitaveis_fetal <- highcharter::renderHighchart({



      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_evitaveis_fetal(),
          highcharter::hcaes(x = ano, y = prop_evitaveis_fetal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_evitaveis_fetal_comp(),
          highcharter::hcaes(x = ano, y = prop_evitaveis_fetal, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos causas evitáveis"), min = 0, max = 100)

    })

    # Criando o gráfico da porcentagem de causas evitáveis p/ óbitos neonatais --------
    data_plot_evitaveis_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_evitaveis_neonatal = sum(dplyr::across(dplyr::all_of(input$cids_evitaveis_neonatal))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_evitaveis_neonatal = round(obitos_evitaveis_neonatal / obitos_neonatais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    observe(print(data_plot_evitaveis_neonatal()))

    data_plot_evitaveis_neonatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_evitaveis_neonatal = sum(dplyr::across(dplyr::all_of(input$cids_evitaveis_neonatal))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_evitaveis_neonatal = round(obitos_evitaveis_neonatal / obitos_neonatais_totais * 100, 1),
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

    output$plot_evitaveis_neonatal <- highcharter::renderHighchart({



      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_evitaveis_neonatal(),
          highcharter::hcaes(x = ano, y = prop_evitaveis_neonatal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_evitaveis_neonatal_comp(),
          highcharter::hcaes(x = ano, y = prop_evitaveis_neonatal, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos causas evitáveis"), min = 0, max = 100)

    })


    # Criando o gráfico da porcentagem de grupos de causas p/ óbitos fetais --------
    data_plot_grupos_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_grupos_fetal = sum(dplyr::across(dplyr::all_of(input$cids_grupos_fetal))),
          obitos_fetais_totais = sum(obitos_neonatais_totais),
          prop_grupos_fetal = round(obitos_grupos_fetal / obitos_fetais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    observe(print(data_plot_grupos_fetal()))

    data_plot_grupos_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_grupos_fetal = sum(dplyr::across(dplyr::all_of(input$cids_grupos_fetal))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_grupos_fetal = round(obitos_grupos_fetal / obitos_fetais_totais * 100, 1),
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

    output$plot_grupos_fetal <- highcharter::renderHighchart({



      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_grupos_fetal(),
          highcharter::hcaes(x = ano, y = prop_grupos_fetal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_grupos_fetal_comp(),
          highcharter::hcaes(x = ano, y = prop_grupos_fetal, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos de grupos de causas"), min = 0, max = 100)

    })

    # Criando o gráfico da porcentagem de grupos de causas p/ óbitos neonatais --------
    data_plot_grupos_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_grupos_neonatal = sum(dplyr::across(dplyr::all_of(input$cids_grupos_neonat))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_grupos_neonatal = round(obitos_grupos_neonatal / obitos_neonatais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
    })

    observe(print(data_plot_grupos_neonatal()))

    data_plot_grupos_neonatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_grupos_neonatal = sum(dplyr::across(dplyr::all_of(input$cids_grupos_neonat))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_grupos_neonatal = round(obitos_grupos_neonatal / obitos_neonatais_totais * 100, 1),
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

    output$plot_grupos_neonat <- highcharter::renderHighchart({



      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_grupos_neonatal(),
          highcharter::hcaes(x = ano, y = prop_grupos_neonatal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_grupos_neonatal_comp(),
          highcharter::hcaes(x = ano, y = prop_grupos_neonatal, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos de grupos de causas"), min = 0, max = 100)

    })


  })
}


## To be copied in the UI
# mod_bloco_8_ui("bloco_8_1")

## To be copied in the server
# mod_bloco_8_server("bloco_8_1")
