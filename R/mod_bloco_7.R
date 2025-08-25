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
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(class = "fonte-titulos-pagina", tags$b(HTML("Mortalidade fetal, perinatal, neonatal e morbidade neonatal: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset1"),
      width = 12,
      collapsible = FALSE,
      tabPanel(
        HTML("<b class = 'fonte-grande'>Relacionados à mortalidade fetal </b>"),
        mod_bloco_7_fetal_ui(ns("bloco_7_fetal_1"))
      ),
      tabPanel(
        HTML("<b class = 'fonte-grande'>Relacionados à mortalidade perinatal </b>"),
        mod_bloco_7_perinatal_ui(ns("bloco_7_perinatal_1"))
      ),
      tabPanel(
        HTML("<b class = 'fonte-grande'>Relacionados à mortalidade neonatal </b>"),
        mod_bloco_7_neonatal_ui(ns("bloco_7_neonatal_1"))
      ),
      tabPanel(
        HTML("<b class = 'fonte-grande'>Relacionados à morbidade neonatal </b>"),
        mod_bloco_7_morbidade_neonatal_ui(ns("bloco_7_morbidade_neonatal_1"))
      )
    )
  )
}

#' bloco_7 Server Functions
#'
#' @noRd
mod_bloco_7_server <- function(id, filtros){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    mod_bloco_7_fetal_server("bloco_7_fetal_1", filtros = filtros)
    mod_bloco_7_perinatal_server("bloco_7_perinatal_1", filtros = filtros)
    mod_bloco_7_neonatal_server("bloco_7_neonatal_1", filtros = filtros)
    mod_bloco_7_morbidade_neonatal_server("bloco_7_morbidade_neonatal_1", filtros = filtros)

  })
}

## To be copied in the UI
# mod_bloco_7_ui("bloco_7_1")

## To be copied in the server
# mod_bloco_7_server("bloco_7_1")
