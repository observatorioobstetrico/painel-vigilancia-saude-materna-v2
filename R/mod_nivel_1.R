#' nivel_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @import bs4Dash
#'
#' @import highcharter
mod_nivel_1_ui <- function(id) {
  ns <- NS(id)
  options(spinner.color = "grey", spinner.color.background = "#ffffff", spinner.size = 0.5, spinner.type = 6)
  tagList(
    div(
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Resumo dos blocos de indicadores"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;"),
      style = "position: fixed; top: 56px; width: 93.75%; background-color: white; z-index: 100;"
    ),
    fluidRow(
      column(
        width = 12,
        shinyWidgets::downloadBttn(
          outputId = ns("report"),
          color = "primary",
          label = HTML("<span style = 'font-size: 17px'> &nbsp; Fazer download do relatório contendo o resumo dos indicadores para a localidade e ano escolhidos </span>"),
          style = "unite",
          size = "sm"
        ),
        align = "center"
      )
    ),
    HTML("<span style='display: block; margin-bottom: 12px;'> </span>"),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(HTML("1 - Condições socioeconômicas e de acesso ao serviço de saúde &nbsp;"), style = "font-size: 22px;", a(icon("chart-line", color = "#007bff"), onclick = "openTab('bloco_1')", href="#")),
          #icon = icon("1"),
          status = "primary",
          collapsible = FALSE,
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' style = 'font-size: 17px'>
                As condições socioeconômicas e de acesso a serviços de saúde são os determinantes mais distais
                do óbito materno, por interferirem no futuro da saúde reprodutiva.
                <span style='display: block; margin-bottom: 14px;'> </span>
                A garantia de acesso a serviços de saúde de qualidade e políticas integradas visando a promoção
                da saúde podem diminuir o risco de morte materna.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Neste bloco, o gestor pode acompanhar as condições socioeconômicas e de acesso a serviços de
                saúde do seu município através dos indicadores apresentados.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML("<span style = 'font-size: 17px'> &nbsp; Ver série histórica dos indicadores </span>"),
                  onclick = "openTab('bloco_1')",
                  href = "#"
                )
              ),
              HTML("<span style='display: block; margin-bottom: 1em;'> </span>"),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b1"),
                  label = HTML("<span style = 'font-size: 17px'> &nbsp; Como interpretar os indicadores desse bloco? </span>"),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i1")), proxy.height = "270px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i3")), proxy.height = "270px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i4")), proxy.height = "270px")
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i2")), proxy.height = "360px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i5")), proxy.height = "360px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i6")), proxy.height = "360px")
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3"), height = "280px"), proxy.height = "340px")
            ),
            column(
              width = 8,
              fluidRow(
                column(
                  width = 6,
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1"), height = "280px"), proxy.height = "340px")
                ),
                column(
                  width = 6,
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2"), height = "280px"), proxy.height = "340px")
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(HTML("2 - Planejamento reprodutivo &nbsp;"), style = "font-size: 22px;", a(icon("chart-line", color = "#007bff"), onclick = "openTab('bloco_2')", href="#")),
          #icon = icon("2"),
          status = "primary",
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' style = 'font-size: 17px'>
                O acesso ao planejamento reprodutivo e a prevenção de gestações indesejadas ou de alto risco
                é fundamental para que as mulheres tenham uma gestação segura.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Dados sobre o acesso a métodos contraceptivos não estão disponíveis nos sistemas de informação
                brasileiros de uso rotineiro.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Os indicadores deste bloco apresentam as necessidades não atendidas de contracepção de forma indireta,
                por meio da taxa de fecundidade em adolescentes, da porcentagem de mulheres com número elevado de partos,
                e das taxas de aborto inseguro.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML("<span style = 'font-size: 17px'> &nbsp; Ver série histórica dos indicadores </span>"),
                  onclick = "openTab('bloco_2')",
                  href = "#"
                )
              ),
              HTML("<span style='display: block; margin-bottom: 1em;'> </span>"),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b2"),
                  label = HTML("<span style = 'font-size: 17px'> &nbsp; Como interpretar os indicadores desse bloco? </span>"),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              fluidRow(
                column(
                  offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i1")), proxy.height = "270px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i2")), proxy.height = "270px")
                ),
                column(
                  offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i3")), proxy.height = "270px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i4")), proxy.height = "280px")
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(HTML("3 - Assistência pré-natal &nbsp;"), style = "font-size: 22px;", a(icon("chart-line", color = "#007bff"), onclick = "openTab('bloco_3')", href="#")),
          #icon = icon("3"),
          status = "primary",
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' style = 'font-size: 17px'>
                A assistência pré-natal é uma ação de saúde efetiva para a redução da mortalidade materna ao permitir:
                <ul align='justify' style = 'font-size: 17px'>
                  <li> O diagnóstico e o tratamento precoce de doenças pré-existentes e de complicações na gravidez (tais como
                  hipertensão arterial, diabetes, sífilis e outras doenças infecciosas); </li>
                  <li> A adoção de medidas preventivas, como vacinas e suplementos alimentares; </li>
                  <li> O fornecimento de orientações e preparação para o parto e o aleitamento
                  materno, bem como para redução/cessação do fumo, do uso do álcool e de outras drogas.</li>
                </ul>
                </p>

                <p align='justify' style = 'font-size: 17px'>
                Neste bloco, o gestor pode acompanhar a porcentagem de mulheres que receberam assistência pré-natal, a porcentagem
                de mulheres com início precoce do acompanhamento pré-natal e a porcentagem de mulheres que recebeu o número mínimo
                de consultas recomendado pela Organização Mundial de Saúde. Pode também avaliar a incidência de sífilis congênita,
                que é considerado um evento sentinela da qualidade da assistência pré-natal, por ser um desfecho negativo evitável
                com ações realizadas exclusivamente durante essa assistência.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML("<span style = 'font-size: 17px'> &nbsp; Ver série histórica dos indicadores </span>"),
                  onclick = "openTab('bloco_3')",
                  href = "#"
                )
              ),
              HTML("<span style='display: block; margin-bottom: 1em;'> </span>"),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b3"),
                  label = HTML("<span style = 'font-size: 17px'> &nbsp; Como interpretar os indicadores desse bloco? </span>"),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              fluidRow(
                column(
                  offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i1")), proxy.height = "270px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i2")), proxy.height = "270px")
                ),
                column(
                  offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i3")), proxy.height = "280px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i4")), proxy.height = "280px")
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(HTML("4 - Assistência ao parto &nbsp;"), style = "font-size: 22px;", a(icon("chart-line", color = "#007bff"), onclick = "openTab('bloco_4')", href="#")),
          #icon = icon("4"),
          status = "primary",
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' style = 'font-size: 17px'>
                A adequada assistência ao parto, com recursos disponíveis e atendimento oportuno, é essencial para
                o manejo de complicações e redução da mortalidade materna. O uso apropriado de tecnologias médicas
                e o cuidado centrado nas necessidades da mulher e da sua família estão entre as recomendações mais
                recentes da Organização Mundial de Saúde para a assistência ao parto.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Neste bloco, apresentamos a porcentagem total de nascimentos por cesarianas, bem como a
                distribuição das mulheres segundo grupos de Robson, a taxa de cesariana em cada grupo de
                Robson e a contribuição de cada grupo para a taxa global de cesariana. Apresentamos, também,
                a porcentagem de mulheres que precisam se deslocar de seu município de residência para ter
                assistência ao parto, e a distância percorrida para chegar nos serviços de saúde.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML("<span style = 'font-size: 17px'> &nbsp; Ver série histórica dos indicadores </span>"),
                  onclick = "openTab('bloco_4')",
                  href = "#"
                )
              ),
              HTML("<span style='display: block; margin-bottom: 1em;'> </span>"),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b4"),
                  label = HTML("<span style = 'font-size: 17px'> &nbsp; Como interpretar os indicadores desse bloco? </span>"),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              status = "primary",
              collapsible = FALSE,
              #style = "height: 360px; overflow-y: auto",
              shinycssloaders::withSpinner(reactable::reactableOutput(ns("table4")), proxy.height = "320px"),
              HTML("<span style='display: block; margin-bottom: 10px;'> </span>"),
              shinyjs::hidden(
                fluidRow(
                  id = ns("caixa_bloco4_resto"),
                  # column(
                  #   offset = 4,
                  #   width = 4,
                  #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_resto")), proxy.height = "280px")
                  # ),
                  column(
                    offset = 4,
                    width = 4,
                    shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_resto")), proxy.height = "280px")
                  )
                )
              )
            )
          ),
          shinyjs::hidden(
            fluidRow(
              id = ns("caixa_bloco4_muni_uf"),
              column(
                offset = 1,
                width = 10,
                fluidRow(
                  # column(
                  #   width = 3,
                  #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i1_muni_uf")), proxy.height = "280px")
                  # ),
                  # column(
                  #   width = 3,
                  #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i2")), proxy.height = "280px")
                  # ),
                  # column(
                  #   width = 3,
                  #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i3")), proxy.height = "280px")
                  # ),
                  # column(
                  #   width = 3,
                  #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i4")), proxy.height = "280px")
                  # ),
                  column(
                    width = 3,
                    shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i5_muni_uf")), proxy.height = "280px")
                  ),
                  column(
                    width = 3,
                    shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i6_muni_uf")), proxy.height = "280px")
                  ),
                  column(
                    width = 3,
                    shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i7_muni_uf")), proxy.height = "280px")
                  ),
                  column(
                    width = 3,
                    shinycssloaders::withSpinner(uiOutput(ns("caixa_b4_i8_muni_uf")), proxy.height = "280px")
                  )
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(HTML("5 - Condições de nascimento &nbsp;"), style = "font-size: 22px;", a(icon("chart-line", color = "#007bff"), onclick = "openTab('bloco_5')", href="#")),
          #icon = icon("5"),
          status = "primary",
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' style = 'font-size: 17px'>
                As condições de nascimento do recém-nato dependem da saúde materna. Embora sejam indicadores relacionados ao bebê,
                devem ser monitorados por também refletirem a qualidade dos cuidados recebidos pela gestante durante a assistência
                pré-natal e ao parto.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Neste bloco apresentamos a porcentagem de nascimentos prematuros (com menos de 37 semanas gestacionais) e com baixo
                peso ao nascer (<2500g), principais determinantes da mortalidade infantil. Apresentamos também os nascimentos a termo
                precoce (bebês nascidos com 37 e 38 semanas gestacionais), que apresentam maior risco de complicações e que são mais
                frequentes em locais com taxa elevada de cesariana.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML("<span style = 'font-size: 17px'> &nbsp; Ver série histórica dos indicadores </span>"),
                  onclick = "openTab('bloco_5')",
                  href = "#"
                )
              ),
              HTML("<span style='display: block; margin-bottom: 1em;'> </span>"),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b5"),
                  label = HTML("<span style = 'font-size: 17px'> &nbsp; Como interpretar os indicadores desse bloco? </span>"),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b5_i1")), proxy.height = "270px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b5_i2")), proxy.height = "270px")
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(uiOutput(ns("caixa_b5_i3")), proxy.height = "270px")
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(HTML("6 - Mortalidade e morbidade materna &nbsp;"), style = "font-size: 22px;", a(icon("chart-line", color = "#007bff"), onclick = "openTab('bloco_6')", href="#")),
          #icon = icon("6"),
          status = "primary",
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' style = 'font-size: 17px'>
                A morte materna e a morbidade materna grave são os desfechos adversos de curto prazo para a saúde da mulher e são
                resultantes da cadeia de determinantes apresentados nos blocos anteriores.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Neste bloco, apresentamos o número absoluto de óbitos maternos e a Razão de Mortalidade Materna. Apresentamos também
                a porcentagem de óbitos maternos por causas obstétricas diretas, que são as que podem ser evitadas por uma adequada
                assistência pré-natal e ao parto, e as principais causas desses óbitos.
                <span style='display: block; margin-bottom: 14px;'> </span>
                A morbidade materna grave, ou seja, mulher que apresentou uma complicação grave durante a gestação, parto ou pós parto,
                também é apresentada, sendo um indicador importante em locais com número reduzido de nascimentos, onde óbitos maternos
                são pouco frequentes. Além da porcentagem de internações obstétricas com morbidade materna grave, apresentamos as principais
                causas dessa morbidade e os principais indicadores de manejo de gravidade.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML("<span style = 'font-size: 17px'> &nbsp; Ver série histórica dos indicadores </span>"),
                  onclick = "openTab('bloco_6')",
                  href = "#"
                )
              ),
              HTML("<span style='display: block; margin-bottom: 1em;'> </span>"),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b6"),
                  label = HTML("<span style = 'font-size: 17px'> &nbsp; Como interpretar os indicadores desse bloco? </span>"),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              bs4Dash::bs4TabCard(
                id = ns("tabset1"),
                width = 12,
                collapsible = FALSE,
                tabPanel(
                  HTML("<b>Mortalidade materna</b>"),
                  fluidRow(
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i1")), proxy.height = "270px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i2")), proxy.height = "270px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i3")), proxy.height = "270px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i4")), proxy.height = "280px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i5")), proxy.height = "280px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i6")), proxy.height = "280px")
                    ),
                    column(
                      offset = 4,
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_mort_i7")), proxy.height = "290px")
                    )
                  )
                ),
                tabPanel(
                  HTML("<b>Morbidade materna grave</b>"),
                  fluidRow(
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_morb_i1")), proxy.height = "270px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_morb_i2")), proxy.height = "270px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_morb_i3")), proxy.height = "270px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_morb_i4")), proxy.height = "280px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_morb_i5")), proxy.height = "280px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_morb_i6")), proxy.height = "280px")
                    ),
                    column(
                      offset = 2,
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_morb_i7")), proxy.height = "290px")
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(uiOutput(ns("caixa_b6_morb_i8")), proxy.height = "290px")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_nivel_1_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##### Criando o output que recebe a localidade e o ano escolhidos ####
    output$titulo_localidade <- renderUI({

      ano <- filtros()$ano

      texto <- dplyr::case_when(
        filtros()$nivel == "Municipal" ~ glue::glue("({filtros()$municipio}, {ano})"),
        filtros()$nivel == "Estadual" ~ glue::glue("({filtros()$estado}, {filtros()$ano})"),
        filtros()$nivel == "Macrorregião de saúde" ~ glue::glue("({filtros()$macro}, {ano})"),
        filtros()$nivel == "Microrregião de saúde" ~ glue::glue("({filtros()$micro}, {ano})"),
        filtros()$nivel == "Regional" ~ glue::glue("({filtros()$regiao}, {ano})"),
        filtros()$nivel == "Nacional" ~ glue::glue("(Brasil, {ano})")
      )

      tags$b(texto, style = "font-size: 33px")
    })


    ##### Criando os modais com as informações sobre os indicadores de cada bloco ####
    observeEvent(input$popup_b1, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div style = 'font-size: 25px;'> Interpretação dos indicadores do Bloco 1: Condições socioeconômicas e de acesso ao serviço de saúde </div>",
        includeHTML("inst/app/www/html/texto_popup_b1.html"),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE
      )
    })

    observeEvent(input$popup_b2, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div style = 'font-size: 25px;'> Interpretação dos indicadores do Bloco 2: Planejamento reprodutivo </div>",
        includeHTML("inst/app/www/html/texto_popup_b2.html"),
        size = "m",
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

    observeEvent(input$popup_b3, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div style = 'font-size: 25px;'> Interpretação dos indicadores do Bloco 3: Assistência pré-natal </div>",
        includeHTML("inst/app/www/html/texto_popup_b3.html"),
        size = "m",
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

    observeEvent(input$popup_b4, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div style = 'font-size: 25px;'> Interpretação dos indicadores do Bloco 4: Assistência ao parto </div>",
        includeHTML("inst/app/www/html/texto_popup_b4.html"),
        size = "m",
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

    observeEvent(input$popup_b5, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div style = 'font-size: 25px;'> Interpretação dos indicadores do Bloco 5: Condições de nascimento </div>",
        includeHTML("inst/app/www/html/texto_popup_b5.html"),
        size = "m",
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

    observeEvent(input$popup_b6, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div style = 'font-size: 25px;'> Interpretação dos indicadores do Bloco 6: Mortalidade e morbidade materna </div>",
        includeHTML("inst/app/www/html/texto_popup_b6.html"),
        size = "m",
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

    localidade_relatorio <- eventReactive(filtros()$pesquisar, {
      dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "brasil",
        filtros()$nivel == "Regional" ~ janitor::make_clean_names(filtros()$regiao),
        filtros()$nivel == "Estadual" ~ janitor::make_clean_names(filtros()$estado),
        filtros()$nivel == "Macrorregião de saúde" ~ janitor::make_clean_names(filtros()$macro),
        filtros()$nivel == "Microrregião de saúde" ~ janitor::make_clean_names(filtros()$micro),
        filtros()$nivel == "Municipal" ~ janitor::make_clean_names(filtros()$municipio)
      )
    }, ignoreNULL = FALSE)

    #### Criando o output que recebe o arquivo para impressão #####
    output$report <- downloadHandler(
      filename = reactive(paste0("relatorio_indicadores_", localidade_relatorio(), "_", filtros()$ano, ".pdf")),
      content = function(file) {

        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        arquivo_html <- tempfile(
          fileext = ".html"
        )

        if (filtros()$nivel == "Nacional") {

          localidade <- "Brasil"
          idhm <- 0.727
          posicao_idhm <- ""
          comparacao_idhm <- "---"
          classificacao_idhm <- "Alto"

        } else if (filtros()$nivel == "Regional") {

          localidade <- filtros()$regiao
          idhm <- "---"
          posicao_idhm <- ""
          comparacao_idhm <- "---"
          classificacao_idhm <- "Classificação não aplicável"

        } else if (filtros()$nivel == "Estadual") {

          localidade <- filtros()$estado
          idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)]))
          posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)])
          comparacao_idhm <- glue::glue("{posicao_idhm}º lugar entre os 27 estados brasileiros")
          classificacao_idhm <- dplyr::case_when(
            as.numeric(idhm) <= 0.499 ~ "Muito baixo",
            as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo",
            as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio",
            as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto",
            as.numeric(idhm) > 0.8 ~ "Muito alto"
          )

        } else if (filtros()$nivel == "Macrorregião de saúde") {

          localidade <- glue::glue("Macrorregião de saúde {filtros()$macro} ({filtros()$estado_macro})")
          idhm <- "---"
          posicao_idhm <- ""
          comparacao_idhm <- "---"
          classificacao_idhm <- "Classificação não aplicável"

        } else if (filtros()$nivel == "Microrregião de saúde") {

          localidade <- glue::glue("Microrregião de saúde {filtros()$micro} ({filtros()$estado_micro})")
          idhm <- "---"
          posicao_idhm <- ""
          comparacao_idhm <- "---"
          classificacao_idhm <- "Classificação não aplicável"

        } else if (filtros()$nivel == "Municipal") {

          localidade <- glue::glue("Município de {filtros()$municipio} ({filtros()$estado_municipio})")
          idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
          posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
          comparacao_idhm <- glue::glue("{posicao_idhm}º lugar entre os 5.570 municípios brasileiros")
          classificacao_idhm <- dplyr::case_when(
            as.numeric(idhm) <= 0.499 ~ "Muito baixo",
            as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo",
            as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio",
            as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto",
            as.numeric(idhm) > 0.8 ~ "Muito alto"
          )
        }

        withProgress(message = "Renderizando o HTML...", {

          incProgress(0.2)

          rmarkdown::render(
            input = "report.Rmd",
            output_file = arquivo_html,
            params = list(
              localidade = localidade,
              nivel = filtros()$nivel,
              ano = filtros()$ano,
              data1 = data1(),
              idhm = idhm,
              classificacao_idhm = classificacao_idhm,
              comparacao_idhm = comparacao_idhm,
              posicao_idhm = posicao_idhm,
              data1_comp = data1_comp(),
              data2 = data2(),
              data2_comp = data2_comp(),
              data3 = data3(),
              data3_comp = data3_comp(),
              data4 = data4(),
              data4_comp = data4_comp,
              data4_deslocamento = data4_deslocamento(),
              data4_comp_deslocamento = data4_comp_deslocamento(),
              data5 = data5(),
              data5_comp = data5_comp(),
              data5_comp_baixo_peso = data5_comp_baixo_peso(),
              data6 = data6(),
              data6_rmm_corrigida = data6_rmm_corrigida(),
              data6_comp = data6_comp(),
              data_incompletude = data_incompletude()
            )
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


    ##### Dados de incompletude e cobertura para a localidade escolhida #####
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano == filtros()$ano
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
          escmae = round(sum(escmae_incompletos, na.rm = TRUE)/sum(escmae_totais, na.rm = TRUE) * 100, 2),
          racacor = round(sum(racacor_incompletos, na.rm = TRUE)/sum(racacor_totais, na.rm = TRUE) * 100, 2),
          qtdpartnor = round(sum(qtdpartnor_incompletos, na.rm = TRUE)/sum(qtdpartnor_totais, na.rm = TRUE) * 100, 2),
          qtdpartces = round(sum(qtdpartces_incompletos, na.rm = TRUE)/sum(qtdpartces_totais, na.rm = TRUE) * 100, 2),
          consprenat = round(sum(consprenat_incompletos, na.rm = TRUE)/sum(consprenat_totais, na.rm = TRUE) * 100, 2),
          mesprenat = round(sum(mesprenat_incompletos, na.rm = TRUE)/sum(mesprenat_totais, na.rm = TRUE) * 100, 2),
          parto = round(sum(parto_incompletos, na.rm = TRUE)/sum(parto_totais, na.rm = TRUE) * 100, 2),
          tprobson = round(sum(tprobson_incompletos, na.rm = TRUE)/sum(tprobson_totais, na.rm = TRUE) * 100, 2),
          peso = round(sum(peso_incompletos, na.rm = TRUE)/sum(peso_totais, na.rm = TRUE) * 100, 2),
          gestacao = round(sum(gestacao_incompletos, na.rm = TRUE)/sum(gestacao_totais, na.rm = TRUE) * 100, 2),
          semagestac = round(sum(semagestac_incompletos, na.rm = TRUE)/sum(semagestac_totais, na.rm = TRUE) * 100, 2),
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
        base_cobertura_muni_2015_2020 |>
          dplyr::filter(
            ano == dplyr::if_else(
              filtros()$ano %in% c(2012, 2013, 2014),
              2015,
              filtros()$ano
            ),
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::mutate(
            ano = filtros()$ano
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "Estadual") {
        base_cobertura_uf_regioes_2015_2020 |>
          dplyr::filter(
            ano == dplyr::if_else(
              filtros()$ano %in% c(2012, 2013, 2014),
              2015,
              filtros()$ano
            ),
            localidade == filtros()$estado
          ) |>
          dplyr::mutate(
            ano = filtros()$ano
          )
      } else if (filtros()$nivel == "Regional") {
        base_cobertura_uf_regioes_2015_2020 |>
          dplyr::filter(
            ano == dplyr::if_else(
              filtros()$ano %in% c(2012, 2013, 2014),
              2015,
              filtros()$ano
            ),
            localidade == filtros()$regiao
          ) |>
          dplyr::mutate(
            ano = filtros()$ano
          )
      } else if (filtros()$nivel == "Nacional") {
        base_cobertura_uf_regioes_2015_2020 |>
          dplyr::filter(
            ano == dplyr::if_else(
              filtros()$ano %in% c(2012, 2013, 2014),
              2015,
              filtros()$ano
            ),
            localidade == "Brasil"
          ) |>
          dplyr::mutate(
            ano = filtros()$ano
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
    })

    data_incompletude <- reactive({
      dplyr::full_join(data_incompletude_aux(), data_cobertura(), by = c("ano", "localidade"))
    })


    ##### Dados do primeiro bloco de indicadores para a localidade escolhida #####
    data1 <- reactive({
      bloco1 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano == filtros()$ano
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
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = round(sum(media_cobertura_esf)/sum(populacao_total) * 100, 1),
          porc_nvm_menor_que_20_anos = round(sum(nvm_menor_que_20_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_7 = round((sum(nvm_com_escolaridade_ate_3) + sum(nvm_com_escolaridade_de_4_a_7))/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil (valor de referência)",
            filtros()$nivel == "Regional" ~ paste("Região ",filtros()$regiao),
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })


    ##### Dados do primeiro bloco de indicadores para a comparação com o Brasil #####
    data1_comp <- reactive({
      bloco1 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          populacao_feminina_10_a_49 = sum(populacao_feminina_10_a_49),
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = 95,
          porc_nvm_menor_que_20_anos = round(sum(nvm_menor_que_20_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_7 = round((sum(nvm_com_escolaridade_ate_3) + sum(nvm_com_escolaridade_de_4_a_7))/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas para os indicadores do primeiro bloco #####
    output$caixa_b1_i1 <- renderUI({

      if (filtros()$nivel == "Municipal") {
        uf <- unique(tabela_aux_municipios$uf[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        micro <- unique(tabela_aux_municipios$r_saude[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        macro <- unique(tabela_aux_municipios$macro_r_saude[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        regiao <- unique(tabela_aux_municipios$regiao[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        texto <-
          "
      <b style='{dplyr::if_else(stringr::str_length(filtros()$municipio) > 11, 'font-size:33px', 'font-size:40px')}'> {filtros()$municipio} </b>
      <br>
      <b> Microrregião de saúde: </b> {micro}
      <br>
      <b> Macrorregião de saúde: </b> {macro}
      <br>
      <b> UF: </b> {uf}
      <br>
      <b> Região do país: </b> {regiao}
      "
      } else if (filtros()$nivel == "Microrregião de saúde") {
        uf <- unique(tabela_aux_municipios$uf[which(tabela_aux_municipios$r_saude == filtros()$micro & tabela_aux_municipios$uf == filtros()$estado_micro)])
        macro <- unique(tabela_aux_municipios$macro_r_saude[which(tabela_aux_municipios$r_saude == filtros()$micro & tabela_aux_municipios$uf == filtros()$estado_micro)])
        regiao <- unique(tabela_aux_municipios$regiao[which(tabela_aux_municipios$r_saude == filtros()$micro & tabela_aux_municipios$uf == filtros()$estado_micro)])
        texto <-
          "
      <b style='{dplyr::if_else(stringr::str_length(filtros()$micro) > 11, 'font-size:33px', 'font-size:40px')}'> {filtros()$micro} </b>
      <br>
      <b> Macrorregião de saúde: </b> {macro}
      <br>
      <b> UF: </b> {uf}
      <br>
      <b> Região do país: </b> {regiao}
      "
      } else if (filtros()$nivel == "Macrorregião de saúde") {
        uf <- unique(tabela_aux_municipios$uf[which(tabela_aux_municipios$macro_r_saude == filtros()$macro & tabela_aux_municipios$uf == filtros()$estado_macro)])
        regiao <- unique(tabela_aux_municipios$regiao[which(tabela_aux_municipios$macro_r_saude == filtros()$macro & tabela_aux_municipios$uf == filtros()$estado_macro)])
        texto <-
          "
      <b style='{dplyr::if_else(stringr::str_length(filtros()$macro) > 11, 'font-size:33px', 'font-size:40px')}'> {filtros()$macro} </b>
      <br>
      <b> UF: </b> {uf}
      <br>
      <b> Região do país: </b> {regiao}
      "
      } else if (filtros()$nivel == "Estadual") {
        regiao <- unique(tabela_aux_municipios$regiao[which(tabela_aux_municipios$uf == filtros()$estado)])
        texto <-
          "
      <b style='{dplyr::if_else(stringr::str_length(filtros()$estado) > 11, 'font-size:33px', 'font-size:40px')}'> {filtros()$estado} </b>
      <br>
      <b> Região do país: </b> {regiao}
      "
      } else if (filtros()$nivel == "Regional") {
        texto <- "<b style='{dplyr::if_else(stringr::str_length(filtros()$regiao) > 11, 'font-size:33px', 'font-size:40px')}'> {filtros()$regiao} </b>"
      } else if (filtros()$nivel == "Nacional") {
        texto <- "<b style='font-size:40px'> Brasil </b>"
      }

      bs4Dash::box(
        style = "height: 300px; overflow-y: auto; padding-top:0",
        width = 12,
        collapsible = FALSE,
        headerBorder = FALSE,
        HTML(glue::glue(paste(texto)))
      )
    })

    output$caixa_b1_i2 <- renderUI({

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

      cor_comp <- dplyr::case_when(
        is.na(idhm) ~ "lightgrey",
        as.numeric(idhm) <= 0.499 ~ "#d998a0",  #vermelho
        as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "#d8b382",  #laranja
        as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "#f1eb99",  #amarelo
        as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "#a2e4b8",  #verde
        as.numeric(idhm) > 0.8 ~ "#cbd6ff"  #azul
      )

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
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b1_i3 <- renderUI({
      cria_caixa_server(
        dados = data1(),
        indicador = "populacao_feminina_10_a_49",
        titulo = "População feminina entre 10 e 49 anos",
        tem_meta = FALSE,
        valor_de_referencia = data1_comp()$populacao_feminina_10_a_49,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        #cor = dplyr::if_else(filtros()$nivel == "Nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (o número nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} mulheres"
        ),
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b1_i4 <- renderUI({
      cria_caixa_server(
        dados = data1(),
        indicador = "total_de_nascidos_vivos",
        titulo = "Nascidos vivos",
        tem_meta = FALSE,
        valor_de_referencia = data1_comp()$total_de_nascidos_vivos,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        #cor = dplyr::if_else(filtros()$nivel == "Nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} nascidos vivos"
        ),
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b1_i5 <- renderUI({
      cria_caixa_server(
        dados = data1(),
        indicador = "porc_dependentes_sus",
        titulo = "Porcentagem de mulheres de 10 a 49 anos usuárias exclusivas do SUS",
        tem_meta = FALSE,
        valor_de_referencia = data1_comp()$porc_dependentes_sus,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b1_i6 <- renderUI({
      cria_caixa_server(
        dados = data1(),
        indicador = "porc_cobertura_esf",
        titulo = "Cobertura populacional com equipes de Saúde da Família",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "meta ODS",
        nivel_de_analise = filtros()$nivel
      )
    })


    ##### Definindo as cores para os gráficos de barra #####
    cols <- c("#2c115f", "#b73779", "#fc8961")


    ##### Criando os gráficos de barras para os indicadores de porcentagem de nascidos vivos #####
    output$plot1 <- highcharter::renderHighchart({

      df1 <- reactive({
        data.frame(
          "total" = c(
            data1()$porc_nvm_com_escolaridade_ate_3,
            data1()$porc_nvm_com_escolaridade_de_4_a_7,
            data1()$porc_nvm_com_escolaridade_de_8_a_11,
            data1()$porc_nvm_com_escolaridade_acima_de_11,
            data1_comp()$porc_nvm_com_escolaridade_ate_3,
            data1_comp()$porc_nvm_com_escolaridade_de_4_a_7,
            data1_comp()$porc_nvm_com_escolaridade_de_8_a_11,
            data1_comp()$porc_nvm_com_escolaridade_acima_de_11
          ),
          "categorias" = factor(rep(c("0 a 3 anos", "4 a 7 anos", "8 a 11 anos", "> 11 anos"), times = 2)),
          "local" = rep(c(data1()$class, "Referência (média nacional)"), each = 4)
        )
      })

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_xAxis(categories = df1()$categorias) |>
        highcharter::hc_add_series(
          data = df1() |> dplyr::filter(local == data1()$class),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%") |>
        highcharter::hc_title(text = HTML("<b style='font-size:16px'> Porcentagem de nascidos vivos por escolaridade da mãe </b>")) |>
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos")) |>
        highcharter::hc_colors(cols)

      if (filtros()$nivel == "Nacional") {
        grafico_base
      } else {
        grafico_base |> highcharter::hc_add_series(
          data = df1() |> dplyr::filter(local == "Referência (média nacional)"),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        )
      }
    })

    output$plot2 <- highcharter::renderHighchart({

      df2 <- reactive({
        data.frame(
          "total" = c(
            data1()$porc_nvm_menor_que_20_anos,
            data1()$porc_nvm_entre_20_e_34_anos,
            data1()$porc_nvm_maior_que_34_anos,
            data1_comp()$porc_nvm_menor_que_20_anos,
            data1_comp()$porc_nvm_entre_20_e_34_anos,
            data1_comp()$porc_nvm_maior_que_34_anos
          ),
          "categorias" = rep(c("< 20 anos", "20 a 34 anos", "> 34 anos"), times = 2),
          "local" = rep(c(data1()$class, "Referência (média nacional)"), each = 3)
        )
      })

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_xAxis(categories = df2()$categorias) |>
        highcharter::hc_add_series(
          data = df2() |> dplyr::filter(local == data1()$class),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%") |>
        highcharter::hc_title(text = HTML("<b style='font-size:16px'> Porcentagem de nascidos vivos por faixa etária da mãe </b>")) |>
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos")) |>
        highcharter::hc_colors(cols)

      if (filtros()$nivel == "Nacional") {
        grafico_base
      } else {
        grafico_base |> highcharter::hc_add_series(
          data = df2() |> dplyr::filter(local == "Referência (média nacional)"),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        )
      }

    })

    output$plot3 <- highcharter::renderHighchart({

      df3 <- reactive({
        data.frame(
          "total" = c(
            data1()$porc_nvm_com_cor_da_pele_amarela,
            data1()$porc_nvm_com_cor_da_pele_branca,
            data1()$porc_nvm_indigenas,
            data1()$porc_nvm_com_cor_da_pele_parda,
            data1()$porc_nvm_com_cor_da_pele_preta,
            data1_comp()$porc_nvm_com_cor_da_pele_amarela,
            data1_comp()$porc_nvm_com_cor_da_pele_branca,
            data1_comp()$porc_nvm_indigenas,
            data1_comp()$porc_nvm_com_cor_da_pele_parda,
            data1_comp()$porc_nvm_com_cor_da_pele_preta
          ),
          "categorias" = rep(c("Amarela", "Branca",  "Indígena", "Parda", "Preta"), times = 2),
          "local" = rep(c(data1()$class, "Referência (média nacional)"), each = 5)
        )
      })

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_xAxis(categories = df3()$categorias) |>
        highcharter::hc_add_series(
          data = df3() |> dplyr::filter(local == data1()$class),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%") |>
        highcharter::hc_title(text = HTML("<b style='font-size:16px'> Porcentagem de nascidos vivos por raça/cor da mãe</b>")) |>
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos")) |>
        highcharter::hc_colors(cols)


      if (filtros()$nivel == "Nacional") {
        grafico_base
      } else {
        grafico_base |> highcharter::hc_add_series(
          data = df3() |> dplyr::filter(local == "Referência (média nacional)"),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        )
      }

    })


    ##### Dados do segundo bloco de indicadores para a localidade escolhida #####
    data2 <- reactive({
      bloco2 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano == filtros()$ano
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
          porc_menor20 = round(sum(nvm_menor_que_20)/sum(pop_feminina_10_a_19)*1000, 1),
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos*100, 1),
          tx_abortos_mil_mulheres_lim_inf = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 2) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 1)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_mil_mulheres_lim_sup = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 4) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 3)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_lim_inf = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*2)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*1))/sum(total_de_nascidos_vivos) *100, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1),
          tx_abortos_cem_nascidos_vivos_lim_sup = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*4)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*3))/sum(total_de_nascidos_vivos) *100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Dados do segundo bloco de indicadores para a comparação com o Brasil #####
    data2_comp <- reactive({
      bloco2 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_menor20 = 30,
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos*100, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30)*0.90) + (sum(abortos_sus_30_a_39)*0.85) + (sum(abortos_sus_40_a_49)*0.75)) * 3) + (((sum(abortos_ans_menor_30)*0.90) + (sum(abortos_ans_30_a_39)*0.85) + (sum(abortos_ans_40_a_49)*0.75)) * 2)) / sum(pop_fem_10_49) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30)*0.9)+(sum(abortos_sus_30_a_39)*0.85)+(sum(abortos_sus_40_a_49)*0.75))*3)+(((sum(abortos_ans_menor_30)*0.9)+(sum(abortos_ans_30_a_39)*0.85)+(sum(abortos_ans_40_a_49)*0.75))*2))/sum(total_de_nascidos_vivos) *100, 1),
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas para os indicadores do segundo bloco #####
    output$caixa_b2_i1 <- renderUI({
      cria_caixa_server(
        dados = data2(),
        indicador = "porc_menor20",
        titulo = "Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil)",
        tem_meta = TRUE,
        valor_de_referencia = 30,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "países desenvolvidos",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b2_i2 <- renderUI({
      cria_caixa_server(
        dados = data2(),
        indicador = "porc_mais_3pt",
        titulo = "Porcentagem de nascidos vivos de mulheres com mais de 3 partos anteriores",
        tem_meta = FALSE,
        valor_de_referencia = data2_comp()$porc_mais_3pt,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b2_i3 <- renderUI({
      cria_caixa_server(
        dados = data2() |> dplyr::filter(ano >= 2015),
        indicador = "tx_abortos_mil_mulheres_valor_medio",
        titulo = "Valor médio da taxa de abortos inseguros por mil MIF",
        tem_meta = FALSE,
        valor_de_referencia = data2_comp()$tx_abortos_mil_mulheres_valor_medio,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b2_i4 <- renderUI({
      cria_caixa_server(
        dados = data2() |> dplyr::filter(ano >= 2015),
        indicador = "tx_abortos_cem_nascidos_vivos_valor_medio",
        titulo = "Valor médio da razão de abortos inseguros por 100 nascidos vivos",
        tem_meta = FALSE,
        valor_de_referencia = data2_comp()$tx_abortos_cem_nascidos_vivos_valor_medio,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })


    ##### Dados do terceiro bloco de indicadores para a localidade escolhida #####
    data3 <- reactive({
      bloco3 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano == filtros()$ano
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
          porc_inicio_prec = round(sum(mulheres_com_inicio_precoce_do_prenatal)/total_de_nascidos_vivos * 100, 1),
          cobertura_pre_natal = round(sum(mulheres_com_pelo_menos_uma_consulta_prenatal)/total_de_nascidos_vivos * 100, 1),
          porc_7 = round(sum(mulheres_com_mais_de_sete_consultas_prenatal)/total_de_nascidos_vivos * 100, 1),
          porc_sc = round(sum(casos_sc)/total_de_nascidos_vivos * 1000, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Dados do terceiro bloco de indicadores para a comparação com o Brasil #####
    data3_comp <- reactive({
      bloco3 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          cobertura_pre_natal = 95,
          porc_inicio_prec = 95,
          porc_7 = 95,
          porc_sc = 0.5
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas para os indicadores do terceiro bloco #####
    output$caixa_b3_i1 <- renderUI({
      cria_caixa_server(
        dados = data3() |> dplyr::filter(ano >= 2014),
        indicador = "cobertura_pre_natal",
        titulo = "Cobertura de assistência pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b3_i2 <- renderUI({
      cria_caixa_server(
        dados = data3(),
        indicador = "porc_inicio_prec",
        titulo = "Porcentagem de mulheres com início precoce do pré-natal (até 12 semanas de gestação)",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b3_i3 <- renderUI({
      cria_caixa_server(
        dados = data3() |> dplyr::filter(ano >= 2014),
        indicador = "porc_7",
        titulo = "Porcentagem de mulheres com mais de sete consultas de pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b3_i4 <- renderUI({
      cria_caixa_server(
        dados = data3(),
        indicador = "porc_sc",
        titulo = "Incidência de sífilis congênita por mil nascidos vivos",
        tem_meta = TRUE,
        valor_de_referencia = 0.5,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "meta OMS",
        nivel_de_analise = filtros()$nivel
      )
    })


    ##### Dados do quarto bloco de indicadores para a localidade escolhida #####
    data4 <- reactive({
      bloco4 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano == filtros()$ano
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
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson_faltante = round((total_de_nascidos_vivos - sum(dplyr::across(dplyr::starts_with("mulheres_dentro")))) / total_de_nascidos_vivos * 100, 1),
          prop_tx_cesariana_geral = round(mulheres_com_parto_cesariana/total_de_nascidos_vivos * 100, 1),
          prop_robson1_tx_cesariana = round((sum(total_cesariana_grupo_robson_1) / sum(mulheres_dentro_do_grupo_de_robson_1)) * 100, 1),
          prop_robson2_tx_cesariana = round((sum(total_cesariana_grupo_robson_2) / sum(mulheres_dentro_do_grupo_de_robson_2)) * 100, 1),
          prop_robson3_tx_cesariana = round((sum(total_cesariana_grupo_robson_3) / sum(mulheres_dentro_do_grupo_de_robson_3)) * 100, 1),
          prop_robson4_tx_cesariana = round((sum(total_cesariana_grupo_robson_4) / sum(mulheres_dentro_do_grupo_de_robson_4)) * 100, 1),
          prop_robson5_tx_cesariana = round((sum(total_cesariana_grupo_robson_5) / sum(mulheres_dentro_do_grupo_de_robson_5)) * 100, 1),
          prop_robson6_a_9_tx_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1),
          prop_robson10_tx_cesariana = round((sum(total_cesariana_grupo_robson_10) / sum(mulheres_dentro_do_grupo_de_robson_10)) * 100, 1),
          contrib_robson1_tx_cesariana = round(sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson2_tx_cesariana = round(sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson3_tx_cesariana = round(sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson4_tx_cesariana = round(sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson5_tx_cesariana = round(sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson6_a_9_tx_cesariana = round(sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson10_tx_cesariana = round(sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson_faltante_tx_cesariana = round((mulheres_com_parto_cesariana - sum(dplyr::across(dplyr::starts_with("total_cesariana")))) / mulheres_com_parto_cesariana * 100, 1),
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

    data4_deslocamento <- reactive({
      if (filtros()$nivel != "Estadual" & filtros()$nivel != "Municipal") {
        bloco4_deslocamento_muni |>
          dplyr::filter(
            ano == filtros()$ano
          ) |>
          dplyr::filter(
            if (filtros()$nivel == "Nacional")
              ano == filtros()$ano
            else if (filtros()$nivel == "Regional")
              regiao == filtros()$regiao
            else if (filtros()$nivel == "Macrorregião de saúde")
              macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
            else if(filtros()$nivel == "Microrregião de saúde")
              r_saude == filtros()$micro & uf == filtros()$estado_micro
          ) |>
          dplyr::group_by(ano) |>
          dplyr::summarise(
            prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_mun_res = round(sum(nao_local, na.rm=TRUE)/sum(destino_total, na.rm=TRUE)*100, 1),
            localidade = dplyr::case_when(
              filtros()$nivel == "Nacional" ~ "Brasil",
              filtros()$nivel == "Regional" ~ filtros()$regiao,
              filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
              filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro
            )
          ) |>
          dplyr::ungroup()
      } else if (filtros()$nivel == "Municipal") {
        bloco4_deslocamento_muni |>
          dplyr::filter(
            ano == filtros()$ano,
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
          ) |>
          dplyr::group_by(ano) |>
          dplyr::mutate(
            prop_partos_fora_macro_rsaude_res = round(fora_macrorregiao_saude/destino_total * 100, 1),
            prop_partos_fora_mun_res = round(sum(nao_local, na.rm=TRUE)/sum(destino_total, na.rm=TRUE)*100, 1),

            localidade = filtros()$municipio,
            .keep = "unused"
          ) |>
          dplyr::ungroup()
      } else if (filtros()$nivel == "Estadual") {
        bloco4_deslocamento_uf |>
          dplyr::filter(
            ano == filtros()$ano,
            uf == filtros()$estado
          ) |>
          dplyr::group_by(ano) |>
          dplyr::mutate(
            prop_partos_fora_macro_rsaude_res = round(fora_macrorregiao_saude/destino_total * 100, 1),
            prop_partos_fora_mun_res = round(sum(nao_local, na.rm=TRUE)/sum(destino_total, na.rm=TRUE)*100, 1),
            localidade = filtros()$estado,
            .keep = "unused"
          ) |>
          dplyr::ungroup()
      }

    })

    ##### Dados do quarto bloco de indicadores para a comparação com o Brasil #####
    data4_comp <- reactive({
      bloco4 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          prop_tx_cesariana_geral = "25%",
          prop_robson1_tx_cesariana = "10%",
          prop_robson2_tx_cesariana = "20 a 35%",
          prop_robson3_tx_cesariana = "3%",
          prop_robson4_tx_cesariana = "15%",
          prop_robson5_tx_cesariana = "50 a 60%",
          prop_robson6_a_9_tx_cesariana = "---",
          prop_robson10_tx_cesariana = "30%"
        ) |>
        dplyr::ungroup()
    })

    data4_comp_deslocamento <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::summarise(
          prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_mun_res = round(sum(nao_local, na.rm=TRUE)/sum(destino_total, na.rm=TRUE)*100, 1),
        )
    })


    ##### Criando a tabela para os indicadores do quarto bloco #####
    output$table4 <- reactable::renderReactable({
      grupo_robson <-  c(
        "Geral",
        "Grupo de Robson 1",
        "Grupo de Robson 2",
        "Grupo de Robson 3",
        "Grupo de Robson 4",
        "Grupo de Robson 5",
        "Grupos de Robson 6 a 9",
        "Grupo de Robson 10"
      )
      porc_nascidos <- c(
        "---",
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson1, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson2, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson3, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson4, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson5, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson6_a_9, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson10, big.mark = ".", decimal.mark = ","), "%"), "---")
      )
      porc_cesareas <- c(
        paste0(formatC(data4()$prop_tx_cesariana_geral, big.mark = ".", decimal.mark = ","), "%"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson1_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson2_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson3_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson4_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson5_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson6_a_9_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson10_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---")
      )
      referencia_porc_cesareas <- c(
        "15%",
        "10%",
        "20 a 35%",
        "3%",
        "15%",
        "50 a 60%",
        "---",
        "30%"
      )

      infos_comp <- data.frame(
        valores_comp = c(
          round(100 - 100*data4()$prop_tx_cesariana_geral/15, 1),
          ifelse(filtros()$ano >= 2014, round(100 - 100*data4()$prop_robson1_tx_cesariana/10, 1), NaN),
          ifelse(
            filtros()$ano >= 2014,
            dplyr::case_when(
              data4()$prop_robson2_tx_cesariana < 20 ~ round(100 - 100*data4()$prop_robson2_tx_cesariana/20, 1),
              data4()$prop_robson2_tx_cesariana > 35 ~ round(100 - 100*data4()$prop_robson2_tx_cesariana/35, 1),
              dplyr::between(data4()$prop_robson2_tx_cesariana, 20, 35) ~ 0
            ),
            NaN
          ),
          ifelse(filtros()$ano >= 2014, round(100 - 100*data4()$prop_robson3_tx_cesariana/3, 1), NaN),
          ifelse(filtros()$ano >= 2014, round(100 - 100*data4()$prop_robson4_tx_cesariana/15, 1), NaN),
          ifelse(
            filtros()$ano >= 2014,
            dplyr::case_when(
              data4()$prop_robson5_tx_cesariana < 50 ~ round(100 - 100*data4()$prop_robson5_tx_cesariana/50, 1),
              data4()$prop_robson5_tx_cesariana > 60 ~ round(100 - 100*data4()$prop_robson5_tx_cesariana/60, 1),
              dplyr::between(data4()$prop_robson5_tx_cesariana, 50, 60) ~ 0
            ),
            NaN
          ),
          NaN,
          ifelse(filtros()$ano >= 2014, round(100 - 100*data4()$prop_robson10_tx_cesariana/30, 1), NaN)
        ),
        invertido = c(
          rep(FALSE, 8)
        ),
        tem_meta = c(
          rep(TRUE, 8)
        ),
        tem_faixa_de_referencia = c(
          FALSE,
          FALSE,
          TRUE,
          FALSE,
          FALSE,
          TRUE,
          FALSE,
          FALSE
        )
      )

      cria_textos <- function(infos_comp) {
        textos_aux <- apply(infos_comp, 1, function(linha) {

          if (linha[2] == TRUE) {
            icone_comp <- dplyr::case_when(
              linha[1] > 0 ~ glue::glue("{fontawesome::fa('caret-down', fill = '#800000')}"),  #vermelho
              linha[1] < 0 ~ glue::glue("{fontawesome::fa('caret-up', fill = '#008000')}"),  #verde
              linha[1] == 0 | is.nan(linha[1]) ~ ""
            )
          } else {
            if (linha[4] == FALSE) {
              icone_comp <- dplyr::case_when(
                linha[1] < 0 ~ glue::glue("{fontawesome::fa('caret-up', fill = '#800000')}"),  #vermelho
                linha[1] > 0 ~ glue::glue("{fontawesome::fa('caret-down', fill = '#008000')}"),  #verde
                linha[1] == 0 | is.nan(linha[1]) ~ ""
              )
            } else {
              icone_comp <- dplyr::case_when(
                linha[1] < 0 ~ glue::glue("{fontawesome::fa('caret-up', fill = '#800000')}"),  #vermelho
                linha[1] > 0 ~ glue::glue("{fontawesome::fa('caret-down', fill = '#800000')}"),  #vermelho
                linha[1] == 0 | is.nan(linha[1]) ~ ""
              )
            }
          }

          if (isTruthy(linha[1])) {
            valor_comp_formatado <- formatC(abs(linha[1]), big.mark = '.', decimal.mark = ',')
          } else {
            valor_comp_formatado <- NaN
          }

          if (isTruthy(linha[1])) {
            razao <- round((100 - linha[1])/100, 1)
          } else {
            razao <- 0
          }

          if (razao >= 2) {
            texto_comp <- glue::glue("{icone_comp} {formatC(razao, big.mark = '.', decimal.mark = ',')} vezes maior que o valor de referência")
          } else {
            if (linha[4] == FALSE) {
              texto_comp <- dplyr::case_when(
                linha[1] < 0 ~ glue::glue("{icone_comp} {valor_comp_formatado}% maior que o valor de referência"),
                linha[1] > 0 ~ glue::glue("{icone_comp} {valor_comp_formatado}% menor que o valor de referência"),
                linha[1] == 0 & linha[3] == TRUE ~ glue::glue("{fontawesome::fa('check', fill = '#008000')} &nbsp; Igual ao valor de referência"),
                linha[1] == 0 & linha[3] == FALSE & filtros()$nivel != "Nacional" ~ glue::glue("{fontawesome::fa('check', fill = '#008000')} &nbsp; Igual ao valor de referência"),
                linha[1] == 0 & linha[3] == FALSE & filtros()$nivel == "Nacional" ~ glue::glue("Comparação não aplicável (a média nacional é o valor de referência)"),
                is.nan(linha[1]) ~ "Comparação não aplicável"
              )
            } else {
              texto_comp <- dplyr::case_when(
                linha[1] < 0 ~ glue::glue("{icone_comp} {valor_comp_formatado}% maior que o valor de referência máximo"),
                linha[1] > 0 ~ glue::glue("{icone_comp} {valor_comp_formatado}% menor que o valor de referência mínimo"),
                linha[1] == 0 & linha[3] == TRUE ~ glue::glue("{fontawesome::fa('check', fill = '#008000')} &nbsp; Dentro da faixa de referência"),
                linha[1] == 0 & linha[3] == FALSE & filtros()$nivel != "Nacional" ~ glue::glue("{fontawesome::fa('check', fill = '#008000')} &nbsp; Dentro da faixa de referência"),
                linha[1] == 0 & linha[3] == FALSE & filtros()$nivel == "Nacional" ~ glue::glue("Comparação não aplicável (a média nacional é o valor de referência)"),
                is.nan(linha[1]) ~ "Comparação não aplicável"
              )
            }

          }

          return(texto = texto_comp)
        }
        )
        return(texto = textos_aux)
      }

      comparacao_porc_cesareas <- c(
        cria_textos(infos_comp)[1],
        cria_textos(infos_comp)[2],
        cria_textos(infos_comp)[3],
        cria_textos(infos_comp)[4],
        cria_textos(infos_comp)[5],
        cria_textos(infos_comp)[6],
        cria_textos(infos_comp)[7],
        cria_textos(infos_comp)[8]
      )



      data.frame(grupo_robson, porc_nascidos, porc_cesareas, referencia_porc_cesareas, comparacao_porc_cesareas) |>
        reactable::reactable(
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            grupo_robson = reactable::colDef(
              name = "Categoria",
              minWidth = 60
            ),
            porc_nascidos = reactable::colDef(
              name = "Porcentagem de nascidos vivos",
              minWidth = 50
            ),
            porc_cesareas = reactable::colDef(
              name = "Porcentagem de cesarianas",
              minWidth = 50
            ),
            referencia_porc_cesareas = reactable::colDef(
              name = "Valor de referência para a % de cesarianas (OMS)",
              minWidth = 60
            ),
            comparacao_porc_cesareas = reactable::colDef(
              name = "Comparação para a % de cesarianas",
              minWidth = 80,
              html = TRUE
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
        )
    })


    ##### Criando as caixinhas para os indicadores do quarto bloco #####
    output$caixa_b4_i1_muni_uf <- output$caixa_b4_i1_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "prop_partos_fora_macro_rsaude_res",
        titulo = "Porcentagem de partos ocorridos fora da macrorregião de saúde, mas dentro da UF  de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_deslocamento()$prop_partos_fora_macro_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = dplyr::if_else(filtros()$nivel == "Municipal" | filtros()$nivel == "Estadual", 11, 12)
      )
    })

    output$caixa_b4_i2 <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_macrorregiao",
        titulo = "Mediana de deslocamento do total de partos ocorridos fora da macrorregião de saúde, mas na UF de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i3 <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_macrorregiao_baixa_complexidade",
        titulo = "Mediana de deslocamento para serviços de baixa complexidade em partos ocorridos fora da macrorregião de saúde, mas na UF de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i4 <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_macrorregiao_alta_complexidade",
        titulo = "Mediana de deslocamento para serviços de alta complexidade em partos ocorridos fora da macrorregião de saúde, mas na UF de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i5_muni_uf <- output$caixa_b4_i5_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "prop_partos_fora_mun_res",
        titulo = "Porcentagem do total de partos ocorridos fora do município de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_deslocamento()$prop_partos_fora_mun_res,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i6_muni_uf <- output$caixa_b4_i6_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_municipio",
        titulo = "Mediana de deslocamento do total de partos ocorridos fora do município de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i7_muni_uf <- output$caixa_b4_i7_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_municipio_baixa_complexidade",
        titulo = "Mediana de deslocamento para serviços de baixa complexidade do total de partos ocorridos fora do município de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i8_muni_uf <- output$caixa_b4_i8_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_municipio_alta_complexidade",
        titulo = "Mediana de deslocamento para serviços de alta complexidade do total de partos ocorridos fora do município de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    observeEvent(filtros()$pesquisar, {
      if (filtros()$nivel != "Municipal" & filtros()$nivel != "Estadual") {
        shinyjs::hide(id = "caixa_bloco4_muni_uf", anim = TRUE, animType = "fade", time = 0.001)
        shinyjs::show(id = "caixa_bloco4_resto", anim = TRUE, animType = "fade", time = 0.8)
      } else {
        shinyjs::hide(id = "caixa_bloco4_resto", anim = TRUE, animType = "fade", time = 0.001)
        shinyjs::show(id = "caixa_bloco4_muni_uf", anim = TRUE, animType = "fade", time = 0.8)
      }
    },
    ignoreNULL = FALSE
    )


    ##### Dados do quinto bloco de indicadores para a localidade escolhida #####
    data5 <- reactive({
      bloco5 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano == filtros()$ano
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
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Dados do quinto bloco de indicadores para a comparação com o Brasil #####
    data5_comp <- reactive({
      bloco5 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_premat = 10,
          porc_termo_precoce = 20
        ) |>
        dplyr::ungroup()
    })

    data5_comp_baixo_peso <- reactive({
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
          porc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100, 1)*0.7
        )
    })


    ##### Criando as caixinhas para os indicadores do quinto bloco #####
    output$caixa_b5_i1 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_baixo_peso",
        titulo = "Porcentagem de baixo peso ao nascer",
        tem_meta = TRUE,
        valor_de_referencia = data5_comp_baixo_peso()$porc_baixo_peso,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        tipo_referencia = "meta de redução global"
      )
    })

    output$caixa_b5_i2 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_premat",
        titulo = "Porcentagem de nascimentos prematuros",
        tem_meta = TRUE,
        valor_de_referencia = 10,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "países desenvolvidos",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b5_i3 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_termo_precoce",
        titulo = "Porcentagem de nascimentos termo precoce",
        tem_meta = TRUE,
        valor_de_referencia = 20,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "países desenvolvidos",
        nivel_de_analise = filtros()$nivel
      )
    })


    ##### Dados do sexto bloco para a localidade escolhida #####
    data6 <- reactive({
      bloco6 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano == filtros()$ano
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
          total_de_nascidos_vivos = sum(nascidos),
          obitos_mat_totais = sum(obitos_mat_totais),
          rmm = round(sum(obitos_mat_totais)/total_de_nascidos_vivos * 100000, 1),
          prop_obitos_diretos = round(sum(obitos_mat_diretos)/sum(obitos_mat_totais) * 100, 1),
          prop_obitos_aborto = round(sum(obitos_mat_aborto)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_hipertens = round(sum(obitos_mat_hipertensao)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_hemo = round(sum(obitos_mat_hemorragia)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_infec = round(sum(obitos_mat_infec_puerperal)/sum(obitos_mat_diretos) * 100, 1),
          casos_mmg = sum(casos_mmg),
          prop_mmg_int_publicas = round(casos_mmg/sum(total_internacoes) * 100, 1),
          prop_mmg_hipertensao = round(sum(casos_mmg_hipertensao)/casos_mmg * 100, 1),
          prop_mmg_hemorragia = round(sum(casos_mmg_hemorragia)/casos_mmg * 100, 1),
          prop_mmg_infeccao = round(sum(casos_mmg_infeccoes)/casos_mmg * 100, 1),
          prop_mmg_uti = round(sum(casos_mmg_uti)/casos_mmg * 100, 1),
          prop_mmg_tmp = round(sum(casos_mmg_tmp)/casos_mmg * 100, 1),
          prop_mmg_transfusao = round(sum(casos_mmg_transfusao)/casos_mmg * 100, 1),
          prop_mmg_cirurgia = round(sum(casos_mmg_cirurgia)/casos_mmg * 100, 1)
        ) |>
        dplyr::ungroup()
    })

    data6_fator_de_correcao <- reactive({
      if (filtros()$nivel %in% c("Estadual", "Regional", "Nacional")) {
        if (filtros()$nivel == "Estadual") {
          rmm_fator_de_correcao |>
            dplyr::filter(
              localidade == filtros()$estado,
              ano == filtros()$ano
            )
        } else if (filtros()$nivel == "Regional") {
          rmm_fator_de_correcao |>
            dplyr::filter(
              localidade == filtros()$regiao,
              ano == filtros()$ano
            )
        } else {
          rmm_fator_de_correcao |>
            dplyr::filter(
              localidade == "Brasil",
              ano == filtros()$ano
            )
        }
      } else {
        data.frame(
          fator_de_correcao = 1,
          ano = filtros()$ano,
          localidade = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        )
      }
    })

    data6_rmm_corrigida_aux <- reactive({
      dplyr::full_join(data6(), data6_fator_de_correcao(), by = "ano")
    })

    data6_rmm_corrigida <- reactive({
      data6_rmm_corrigida_aux() |>
        dplyr::mutate(
          rmm = round(rmm*fator_de_correcao, 1)
        )
    })


    ##### Dados do sexto bloco para a comparação com o Brasil #####
    data6_comp <- reactive({
      bloco6 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(nascidos),
          obitos_mat_totais = sum(obitos_mat_totais),
          rmm = 30,
          prop_obitos_diretos = round(sum(obitos_mat_diretos)/sum(obitos_mat_totais) * 100, 1),
          prop_obitos_aborto = round(sum(obitos_mat_aborto)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_hipertens = round(sum(obitos_mat_hipertensao)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_hemo = round(sum(obitos_mat_hemorragia)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_infec = round(sum(obitos_mat_infec_puerperal)/sum(obitos_mat_diretos) * 100, 1),
          casos_mmg = sum(casos_mmg),
          prop_mmg_int_publicas = round(casos_mmg/sum(total_internacoes) * 100, 1),
          prop_mmg_hipertensao = round(sum(casos_mmg_hipertensao)/casos_mmg * 100, 1),
          prop_mmg_hemorragia = round(sum(casos_mmg_hemorragia)/casos_mmg * 100, 1),
          prop_mmg_infeccao = round(sum(casos_mmg_infeccoes)/casos_mmg * 100, 1),
          prop_mmg_uti = round(sum(casos_mmg_uti)/casos_mmg * 100, 1),
          prop_mmg_tmp = round(sum(casos_mmg_tmp)/casos_mmg * 100, 1),
          prop_mmg_transfusao = round(sum(casos_mmg_transfusao)/casos_mmg * 100, 1),
          prop_mmg_cirurgia = round(sum(casos_mmg_cirurgia)/casos_mmg * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas para os indicadores do sexto bloco #####
    output$caixa_b6_mort_i1 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "obitos_mat_totais",
        titulo = "Número de óbitos maternos",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$obitos_mat_totais,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        #cor = dplyr::if_else(filtros()$nivel == "Nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i2 <- renderUI({
      cria_caixa_server(
        dados = data6_rmm_corrigida(),
        indicador = "rmm",
        titulo = "Razão de mortalidade materna por 100.000 nascidos vivos",
        tem_meta = TRUE,
        valor_de_referencia = 30,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "meta ODS",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i3 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_diretos",
        titulo = "Porcentagem de óbitos maternos por causas obstétricas diretas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_diretos,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i4 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_aborto",
        titulo = "Porcentagem de óbitos maternos diretos por aborto",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_aborto,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i5 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_hemo",
        titulo = "Porcentagem de óbitos maternos diretos por causas hemorrágicas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_hemo,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i6 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_hipertens",
        titulo = "Porcentagem de óbitos maternos diretos por causas hipertensivas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_hipertens,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i7 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_infec",
        titulo = "Porcentagem de óbitos maternos diretos por infecção puerperal",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_infec,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i1 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_int_publicas",
        titulo = "Porcentagem de casos de morbidade materna grave em internações obstétricas públicas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_int_publicas,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i2 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_hipertensao",
        titulo = "Porcentagem de casos de morbidade materna grave por hipertensão",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_hipertensao,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i3 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_hemorragia",
        titulo = "Porcentagem de casos de morbidade materna grave por hemorragia",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_hemorragia,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i4 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_infeccao",
        titulo = "Porcentagem de casos de morbidade materna grave por infecção",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_infeccao,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i5 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_uti",
        titulo = "Porcentagem de casos de morbidade materna grave com internação em UTI",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_uti,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i6 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_tmp",
        titulo = "Porcentagem de casos de morbidade materna grave com Tempo de Permanência Prolongada",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_tmp,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i7 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_transfusao",
        titulo = "Porcentagem de casos de morbidade materna grave com transfusão sanguínea",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_transfusao,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i8 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_cirurgia",
        titulo = "Porcentagem de casos de morbidade materna grave com intervenções cirúrgicas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_cirurgia,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

  })
}

## To be copied in the UI
# mod_nivel_1_ui("nivel_1_1")

## To be copied in the server
# mod_nivel_1_server("nivel_1_1")
