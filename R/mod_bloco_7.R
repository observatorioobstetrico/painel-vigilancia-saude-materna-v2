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

    ## Criando o output que recebe a localidade e o ano escolhidos ------------
    output$titulo_localidade <- renderUI({
      if (length(filtros()$ano2[1]:filtros()$ano2[2]) > 1) {
        ano <- glue::glue("{filtros()$ano2[1]} a {filtros()$ano2[2]}")
      } else {
        ano <- filtros()$ano2[1]
      }

      if (filtros()$comparar == "Não") {
        local1 <- dplyr::case_when(
          filtros()$nivel == "nacional" ~ "Brasil",
          filtros()$nivel == "regional" ~ filtros()$regiao,
          filtros()$nivel == "estadual" ~ filtros()$estado,
          filtros()$nivel == "macro" ~ filtros()$macro,
          filtros()$nivel == "micro" ~ filtros()$micro,
          filtros()$nivel == "municipal" ~ filtros()$municipio
        )
        texto <- glue::glue("({local1}, {ano})")
      } else {
        local1 <- dplyr::case_when(
          filtros()$nivel == "nacional" ~ "Brasil",
          filtros()$nivel == "regional" ~ filtros()$regiao,
          filtros()$nivel == "estadual" ~ filtros()$estado,
          filtros()$nivel == "macro" ~ filtros()$macro,
          filtros()$nivel == "micro" ~ filtros()$micro,
          filtros()$nivel == "municipal" ~ filtros()$municipio
        )
        local2 <- dplyr::case_when(
          filtros()$nivel2 == "nacional" ~ "Brasil",
          filtros()$nivel2 == "regional" ~ filtros()$regiao2,
          filtros()$nivel2 == "estadual" ~ filtros()$estado2,
          filtros()$nivel2 == "macro" ~ filtros()$macro2,
          filtros()$nivel2 == "micro" ~ filtros()$micro2,
          filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
          filtros()$nivel2 == "municipios_semelhantes" ~ "municípios semelhantes"
        )
        texto <- glue::glue("({local1} e {local2}, {ano})")
      }

      tags$b(texto, class = "fonte-titulos-pagina")
    })

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
