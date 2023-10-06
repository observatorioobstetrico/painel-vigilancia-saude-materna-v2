#' documentacao UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_documentacao_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML("<h2><b>Documentação dos indicadores</b></h2>"),
    # fluidRow(
    #   column(
    #     width = 12,
    #     shinyWidgets::downloadBttn(
    #       outputId = ns("report"),
    #       color = "primary",
    #       label = HTML("&nbsp; Fazer download do arquivo de documentação (provisório)"),
    #       style = "unite",
    #       size = "sm"
    #     ),
    #     align = "center"
    #   )
    # ),
    # fluidRow(
    #   HTML(
    #     "
    #       <p align='justify'; style='font-size:18px'>
    #       O arquivo disponível abaixo contém toda a documentação dos indicadores existentes neste painel. Nele, estão disponíveis
    #       as definição, métodos de cálculo, fontes, informações sobre a qualidade da informação e as referências utilizadas
    #       para a construção de cada indicador. Até o momento, os indicadores foram calculados para o período de 2012 a 2020. O painel
    #       será atualizado anualmente, após a consolidação das bases de dados utilizadas em sua construção.
    #       </p>
    #       "
    #   )
    # ),
    tags$iframe(
      style = "display: block;       /* iframes are inline by default */
    background: #000;
    margin: auto;
    height: 85vh;        /* Viewport-relative units */
    width: 90vw;",
      src = "www/Documentação dos indicadores.pdf",
      align = "center"
    )
  )


}


#' documentacao Server Functions
#'
#' @noRd
mod_documentacao_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$report <- downloadHandler(
      filename = "documentacao_indicadores.pdf",
      content = function(file) {

        tempReport <- file.path(tempdir(), "documentacao_dos_indicadores.Rmd")
        file.copy("documentacao_dos_indicadores.Rmd", tempReport, overwrite = TRUE)

        arquivo_html <- tempfile(
          fileext = ".html"
        )

        withProgress(message = "Renderizando o HTML...", {

          incProgress(0.2)

          rmarkdown::render(
            input = "documentacao_dos_indicadores.Rmd",
            output_file = arquivo_html
          )

          incProgress(0.5, message = "Renderizando o PDF...")

          pagedown::chrome_print(
            input = arquivo_html,
            output = file,
            extra_args = c("--no-sandbox")
          )

          incProgress(0.3)

        })



      }
    )

  })
}



## To be copied in the UI
# mod_documentacao_ui("documentacao_1")

## To be copied in the server
# mod_documentacao_server("documentacao_1")
