#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#'
#' @noRd
#'
#' @import bs4Dash
#'
#' @import highcharter

app_ui <- function(request) {
  tagList(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: black;
        display: flex;
        align-items: center;
        justify-content:center;
        text-align: center;
        width: 100%
      }
    ")),
    # Leave this function for adding external resources
    golem_add_external_resources(
    ),
    bs4Dash::bs4DashPage(
      help = NULL,
      dark = NULL,
      title = "Painel de Vigilância da Saúde Materna",
      bs4Dash::bs4DashNavbar(
        fixed = TRUE,
        title = bs4Dash::bs4DashBrand(
          title = HTML("<b> Painel de Vigilância da Saúde Materna </b>"),
          color = "primary",
          href = "https://observatorioobstetricobr.org/",
          image = "www/logo-oobr2.png"
        ),
        status = "primary"
      ),
      bs4Dash::bs4DashSidebar(
        style = "z-index: 9999;",
        width = "470px",
        status = "navy",
        skin = "light",
        collapsed = TRUE,
        bs4Dash::bs4SidebarMenu(
          id = "abas",
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b> Sobre o painel </b>"),
            tabName = "sobre",
            icon = icon("circle-info")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b> Resumo dos blocos de indicadores </b>"),
            tabName = "nivel_1",
            icon = icon("chart-gantt")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b> Séries históricas </b>"),
            icon = icon("chart-line"),
            startExpanded = TRUE,
            bs4Dash::bs4SidebarMenuSubItem(
              text = HTML("- Condições socioeconômicas e de acesso ao serviço de saúde"),
              tabName = "bloco_1",
              icon = icon("1")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Planejamento reprodutivo",
              tabName = "bloco_2",
              icon = icon("2")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Assistência pré-natal",
              tabName = "bloco_3",
              icon = icon("3")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Assistência ao parto",
              tabName = "bloco_4",
              icon = icon("4")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Condições de nascimento",
              tabName = "bloco_5",
              icon = icon("5")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Mortalidade e morbidade materna",
              tabName = "bloco_6",
              icon = icon("6")
            )
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b> Visão detalhada dos indicadores </b>"),
            tabName = "nivel_3",
            icon = icon("chart-column")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b> Documentação dos indicadores </b>"),
            tabName = "documentacao",
            icon = icon("file-circle-question")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b> A história de Aparecida </b>"),
            tabName = "aparecida",
            icon = icon("person-pregnant")
          )
        )
      ),
      bs4Dash::bs4DashBody(
        conditionalPanel(
          condition = "input.abas != 'sobre' & input.abas != 'documentacao' & input.abas != 'aparecida'",
          div(style = "margin-top: 71px;"),
          bs4Dash::bs4Card(
            width = 12,
            title = HTML("<b style='font-size:22px'> Filtros </b>"),
            icon = icon("filter"),
            status = "primary",
            fluidRow(
              column(
                width = 3,
                conditionalPanel(
                  condition = "input.abas == 'nivel_1'",
                  numericInput(
                    inputId = "ano",
                    label = HTML("<span style = 'font-size: 17'> Ano </span>"),
                    value = 2020,
                    min = 2012,
                    max = 2020,
                    width = "95%"
                  ),
                  style = "display: none;"
                ),
                conditionalPanel(
                  condition = "input.abas != 'nivel_1'",
                  sliderInput(
                    inputId = "ano2",
                    label = HTML("<span style = 'font-size: 17'> Intervalo de anos </span>"),
                    min = 2012,
                    max = 2020,
                    value = c(2012, 2020),
                    sep = '',
                    width = "90%"
                  ),
                  style = "display: none;"
                )
              ),
              column(
                width = 3,
                selectizeInput(
                  inputId = "nivel",
                  label = HTML("<span style = 'font-size: 17'> Nível de análise </span>"),
                  options = list(placeholder = "Selecione o nível de análise"),
                  choices = c("Nacional", "Regional", "Estadual", "Macrorregião de saúde", "Microrregião de saúde", "Municipal"),
                  width = "95%"
                )
              ),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.nivel == 'Regional'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "regiao",
                        label = HTML("<span style = 'font-size: 17'> Região </span>"),
                        options = list(placeholder = "Selecione uma região do país"),
                        choices = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.nivel == 'Macrorregião de saúde'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "estado_macro",
                        label = HTML("<span style = 'font-size: 17'> Estado </span>"),
                        choices = sort(estados_choices),
                        options = list(placeholder = "Selecione um estado"),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "macro",
                        label = HTML("<span style = 'font-size: 17'> Macrorregião de saúde </span>"),
                        choices = NULL,
                        options = list(placeholder = "Selecione uma macrorregião de saúde"),
                        width = "95%"
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.nivel == 'Microrregião de saúde'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "estado_micro",
                        label = HTML("<span style = 'font-size: 17'> Estado </span>"),
                        choices = sort(estados_choices),
                        options = list(placeholder = "Selecione um estado"),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "micro",
                        label = HTML("<span style = 'font-size: 17'> Microrregião de saúde </span>"),
                        choices = NULL,
                        options = list(placeholder = "Selecione uma microregião de saúde"),
                        width = "95%"
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.nivel == 'Estadual'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "estado",
                        label = HTML("<span style = 'font-size: 17'> Estado </span>"),
                        choices = sort(estados_choices),
                        options = list(placeholder = "Selecione um estado"),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.nivel == 'Municipal'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "estado_municipio",
                        label = HTML("<span style = 'font-size: 17'> Estado </span>"),
                        choices = sort(estados_choices),
                        options = list(placeholder = "Selecione um estado"),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "municipio",
                        label = HTML("<span style = 'font-size: 17'> Município </span>"),
                        choices = NULL,
                        options = list(placeholder = "Selecione um município"),
                        width = "95%"
                      )
                    )
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.abas != 'nivel_1' & input.abas != 'nivel_3'",
              fluidRow(
                column(
                  width = 3,
                  selectizeInput(
                    inputId = "comparar",
                    label = HTML("<span style = 'font-size: 17'> Comparar com outra localidade? </span>"),
                    choices = c("Sim", "Não"),
                    selected = "Não",
                    width = "95%"
                  )
                ),
                column(
                  width = 3,
                  conditionalPanel(
                    condition = "input.comparar == 'Sim'",
                    selectizeInput(
                      inputId = "nivel2",
                      label = HTML(glue::glue("<span style = 'font-size: 17'> {uiOutput('label_nivel_comp')} </span>")),
                      options = list(placeholder = "Selecione o nível de análise"),
                      choices = c("Nacional", "Regional", "Macrorregião de saúde", "Microrregião de saúde", "Estadual", "Municipal"),
                      width = "95%"
                    )
                  )
                ),
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "input.nivel2 == 'Regional' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "regiao2",
                          label = HTML("<span style = 'font-size: 17'> Região </span>"),
                          options = list(placeholder = "Selecione uma região do país"),
                          choices = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.nivel2 == 'Macrorregião de saúde' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "estado_macro2",
                          label = HTML("<span style = 'font-size: 17'> Estado </span>"),
                          options = list(placeholder = "Selecione um estado"),
                          choices = sort(estados_choices),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "macro2",
                          label = HTML("<span style = 'font-size: 17'> Macrorregião de saúde </span>"),
                          options = list(placeholder = "Selecione uma macrorregião de saúde"),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.nivel2 == 'Microrregião de saúde' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "estado_micro2",
                          label = HTML("<span style = 'font-size: 17'> Estado </span>"),
                          options = list(placeholder = "Selecione um estado"),
                          choices = sort(estados_choices),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "micro2",
                          label = HTML("<span style = 'font-size: 17'> Microrregião de saúde </span>"),
                          options = list(placeholder = "Selecione uma microregião de saúde"),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.nivel2 == 'Estadual' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "estado2",
                          label = HTML("<span style = 'font-size: 17'> Estado </span>"),
                          options = list(placeholder = "Selecione um estado"),
                          choices = sort(estados_choices),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.nivel2 == 'Municipal' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "estado_municipio2",
                          label = HTML("<span style = 'font-size: 17'> Estado </span>"),
                          options = list(placeholder = "Selecione um estado"),
                          choices = sort(estados_choices),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "municipio2",
                          label = HTML("<span style = 'font-size: 17'> Município </span>"),
                          options = list(placeholder = "Selecione um município"),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.comparar == 'Sim' & input.abas != 'bloco_4'",
                fluidRow(
                  column(
                    width = 12,
                    radioButtons(
                      inputId = "mostrar_referencia",
                      label = NULL,
                      choiceNames = list(
                        HTML(glue::glue("<span style = 'font-weight: normal'> Mostrar a linha relacionada ao valor de referência </span>")),
                        HTML(glue::glue("<span style = 'font-weight: normal'> Não mostrar a linha relacionada ao valor de referência </span>"))
                      ),
                      choiceValues = list("mostrar_referencia", "nao_mostrar_referencia"),
                      selected = "mostrar_referencia",
                      inline = TRUE
                    ),
                    align = "center"
                  )
                )
              ),
              style = "display: none;"
            ),
            conditionalPanel(
              condition = "input.abas == 'nivel_3'",
              fluidRow(
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "bloco",
                    label = HTML("<span style = 'font-size: 17'> Bloco de indicadores </span>"),
                    options = list(placeholder = "Selecione o bloco de indicadores"),
                    choices = c(
                      "1 - Condições socioeconômicas e de acesso ao serviço de saúde" = "bloco1",
                      "2 - Planejamento reprodutivo" = "bloco2",
                      "3 - Assistência pré-natal" = "bloco3",
                      "4 - Assistência ao parto" = "bloco4",
                      "5 - Condições de nascimento" = "bloco5",
                      "6 - Mortalidade e morbidade materna" = "bloco6"
                    ),
                    width = "98%"
                  )
                ),
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "input.bloco != 'bloco4' & input.bloco != 'bloco6'",
                    selectizeInput(
                      inputId = "indicador",
                      label = HTML("<span style = 'font-size: 17'> Indicador </span>"),
                      options = list(placeholder = "Selecione o indicador"),
                      choices = tabela_indicadores$indicador[which(tabela_indicadores$bloco == "bloco1")],
                      width = "98%"
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      conditionalPanel(
                        condition = "input.bloco == 'bloco4' | input.bloco == 'bloco6'",
                        selectizeInput(
                          inputId = "tipo_do_indicador_blocos4_6",
                          label = HTML("<span style = 'font-size: 17'> Selecione o grupo de indicadores </span>"),
                          options = list(placeholder = "Selecione um grupo de indicadores"),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    ),
                    column(
                      width = 6,
                      conditionalPanel(
                        condition = "input.bloco == 'bloco4' | input.bloco == 'bloco6'",
                        selectizeInput(
                          inputId = "indicador_blocos4_6",
                          label = HTML("<span style = 'font-size: 17'> Indicador </span>"),
                          options = list(placeholder = "Selecione o indicador"),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    )
                  )
                )
              ),
              style = "display: none;"
            ),
            fluidRow(
              column(
                width = 12,
                shinyWidgets::actionBttn(
                  inputId = "pesquisar",
                  icon = icon("magnifying-glass"),
                  color = "primary",
                  label = HTML("<span style = 'font-size: 17'> &nbsp; Atualizar resultados </span>"),
                  style = "unite",
                  size = "sm"
                ),
                align = "center"
              )
            )
          ),
          style = "display: none;"
        ),
        tags$script(HTML("
          var openTab = function(tabName){
            $('a', $('.sidebar')).each(function() {
              if(this.getAttribute('data-value') == tabName) {
                this.click()
              };
            });
          }
        ")),
        shinyjs::useShinyjs(),
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "sobre",
            mod_sobre_ui("sobre_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "nivel_1",
            mod_nivel_1_ui("nivel_1_1"),
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_1",
            mod_bloco_1_ui("bloco_1_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_2",
            mod_bloco_2_ui("bloco_2_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_3",
            mod_bloco_3_ui("bloco_3_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_4",
            mod_bloco_4_ui("bloco_4_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_5",
            mod_bloco_5_ui("bloco_5_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_6",
            mod_bloco_6_ui("bloco_6_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "nivel_3",
            mod_nivel_3_ui("nivel_3_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "documentacao",
            mod_documentacao_ui("documentacao_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "aparecida",
            h2(tags$b("A história de Aparecida")),
            HTML(
              "
              <p align='justify'; style='font-size:18px'>
              Para melhor entender os resultados dos indicadores apresentados neste painel em diferentes contextos e em como eles
              refletem as situações de vulnerabilidade da mulher ao óbito materno, acesse, clicando na imagem abaixo ou
              <a href = 'https://observatorioobstetricobr.org/a-historia-de-aparecida/' target = _blank>neste link</a>, a história de Aparecida,
              uma mulher preta, que mora num município pequeno, localizado no interior de um estado brasileiro. A história de Aparecida,
              apesar de não ser uma história real, retrata as condições de vida e saúde de muitas brasileiras, evidenciando a grande
              vulnerabilidade à morte materna a qual essas mulheres estão submetidas.
              </p>
              "
            ),
            HTML(
              '
              <div style = "display: flex; justify-content: center; align-items: center;">
                <div class="card2 blue2" style = "width: 55vw; height: 67vh;">
                <a href = "https://observatorioobstetricobr.org/a-historia-de-aparecida/" target = "_blank">
                  <image src = "www/aparecida.png" style = "width: 100%; margin: auto; display: block;">
                </a>
              </div>
              </div>
              '
            )
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Painel de vigilância da saúde materna"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
