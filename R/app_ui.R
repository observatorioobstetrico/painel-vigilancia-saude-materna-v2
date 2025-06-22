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
    includeCSS("inst/app/www/global/custom.css"),
    tags$head(tags$script(src = "funcoes_javascript.js")),
    tags$style(HTML(
      "
      .shiny-output-error-validation {
        color: black;
        display: flex;
        align-items: center;
        justify-content:center;
        text-align: center;
        width: 100%
      }
    "
    )),
    tags$style(HTML(
      "
      .dropdown-menu {
        width: inherit;
        position: absolute;
        will-change: transform;
        top: 0px;
        left: 0px;
        transform: translate3d(0px, 0px, 0px)!important;
      }
    "
    )),
    tags$style(HTML(
      "
      .btn-light {
        background-color: #ebeff2;
        border-color: #ebeff2;
      }
    "
    )),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bs4Dash::bs4DashPage(
      help = NULL,
      dark = NULL,
      title = "Painel de Vigilância da Saúde Materna e Perinatal",
      bs4Dash::bs4DashNavbar(
        fixed = TRUE,
        title = bs4Dash::bs4DashBrand(
          title = HTML(
            "<b> Painel de Vigilância da Saúde Materna e Perinatal </b>"
          ),
          color = "primary",
          href = "https://observatorioobstetricobr.org/",
          image = "www/logos/logo-oobr-curto.png"
        ),
        status = "primary",
        skin = "light"
      ),
      bs4Dash::bs4DashSidebar(
        #style = "z-index: 9999;",
        width = "500px",
        status = "navy",
        skin = "light",
        collapsed = TRUE,
        bs4Dash::bs4SidebarMenu(
          id = "abas",
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b>Sobre o painel </b>"),
            tabName = "sobre",
            icon = icon("circle-info")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b>Nível 1: Resumo dos blocos de indicadores </b>"),
            tabName = "nivel_1",
            icon = icon("chart-gantt")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b>Nível 2: Séries históricas </b>"),
            icon = icon("chart-line"),
            startExpanded = TRUE,
            bs4Dash::bs4SidebarMenuSubItem(
              text = HTML(
                "- Condições socioeconômicas e de acesso ao serviço de saúde"
              ),
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
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Mortalidade fetal, perinatal e neonatal e morbidade neonatal",
              tabName = "bloco_7",
              icon = icon("7")
            ) #,
            # bs4Dash::bs4SidebarMenuSubItem(
            #   text = "- Gráfico de radar",
            #   tabName = "bloco_9",
            #   icon = icon("8")
            # )
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b>Nível 3: Visão detalhada dos indicadores </b>"),
            tabName = "nivel_3",
            icon = icon("chart-column")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b>Documentação dos indicadores </b>"),
            tabName = "documentacao",
            icon = icon("file-circle-question")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = HTML("<b>A história de Aparecida </b>"),
            tabName = "aparecida",
            icon = icon("person-pregnant")
          )
        )
      ),
      bs4Dash::bs4DashBody(
        div(class = "div-dependente"),
        conditionalPanel(
          condition = "input.abas != 'sobre' & input.abas != 'documentacao' & input.abas != 'aparecida'",
          bs4Dash::bs4Card(
            width = 12,
            title = HTML("<b class = 'fonte-indicador-nivel3'> Selecione os filtros de interesse: </b>"),
            icon = icon("filter"),
            status = "primary",
            fluidRow(
              column(
                width = 3,
                conditionalPanel(
                  condition = "input.abas == 'nivel_1'",
                  numericInput(
                    inputId = "ano",
                    label = HTML("<span class = 'fonte-muito-grande'> Ano </span>"),
                    value = 2023,
                    min = 2012,
                    max = 2024,
                    step = 1,
                    width = "95%"
                  ),
                  style = "display: none;"
                ),
                conditionalPanel(
                  condition = "input.abas != 'nivel_1'",
                  sliderInput(
                    inputId = "ano2",
                    label = HTML(
                      "<span class = 'fonte-muito-grande'> Intervalo de anos </span>"
                    ),
                    min = 2012,
                    max = 2024,
                    value = c(2012, 2023),
                    step = 1,
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
                  label = HTML(
                    "<span class = 'fonte-muito-grande'> Nível de análise </span>"
                  ),
                  options = list(placeholder = "Selecione o nível de análise"),
                  choices = c(
                    "Nacional" = "nacional",
                    "Região do país" = "regional",
                    "Estadual" = "estadual",
                    "Macrorregião de saúde estadual" = "macro",
                    "Região de saúde estadual" = "micro",
                    "Municipal" = "municipal"
                  ),
                  width = "95%"
                )
              ),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.nivel == 'regional'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "regiao",
                        label = HTML(
                          "<span class = 'fonte-muito-grande'> Região </span>"
                        ),
                        options = list(
                          placeholder = "Selecione uma região do país"
                        ),
                        choices = c(
                          "Centro-Oeste",
                          "Nordeste",
                          "Norte",
                          "Sudeste",
                          "Sul"
                        ),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.nivel == 'macro'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "estado_macro",
                        label = HTML(
                          "<span class = 'fonte-muito-grande'> Estado </span>"
                        ),
                        choices = sort(estados_choices),
                        options = list(placeholder = "Selecione um estado"),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "macro",
                        label = HTML(
                          "<span class = 'fonte-muito-grande'> Macrorregião de saúde estadual </span>"
                        ),
                        choices = NULL,
                        options = list(
                          placeholder = "Selecione uma macrorregião de saúde estadual estadual"
                        ),
                        width = "95%"
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.nivel == 'micro'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "estado_micro",
                        label = HTML(
                          "<span class = 'fonte-muito-grande'> Estado </span>"
                        ),
                        choices = sort(estados_choices),
                        options = list(placeholder = "Selecione um estado"),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "micro",
                        label = HTML(
                          "<span class = 'fonte-muito-grande'> Região de saúde estadual </span>"
                        ),
                        choices = NULL,
                        options = list(
                          placeholder = "Selecione uma região de saúde estadual"
                        ),
                        width = "95%"
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.nivel == 'estadual'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "estado",
                        label = HTML(
                          "<span class = 'fonte-muito-grande'> Estado </span>"
                        ),
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
                  condition = "input.nivel == 'municipal'",
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "estado_municipio",
                        label = HTML(
                          "<span class = 'fonte-muito-grande'> Estado </span>"
                        ),
                        choices = sort(estados_choices),
                        options = list(placeholder = "Selecione um estado"),
                        width = "95%"
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = "municipio",
                        label = HTML(
                          "<span class = 'fonte-muito-grande'> Município </span>"
                        ),
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
              condition = "(input.abas == 'nivel_1' & input.ano == 2024)  | (input.abas != 'nivel_1' & input.ano2[1] == 2024)",
              fluidRow(
                column(
                  width = 3,
                  HTML(
                    "
                    <div style = 'text-align: left;'> <b class = 'fonte-grande'>
                        <i class='fa-solid fa-circle-info'></i> &nbsp; Os dados de 2024 são preliminares (atualizados em 09 de maio de 2025)
                    </b> </div>
                    <span style='display: block; margin-bottom: 15px;'> </span>
                  "
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
                    label = HTML(
                      "<span class = 'fonte-muito-grande'> Comparar com outra localidade? </span>"
                    ),
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
                      label = HTML(glue::glue(
                        "<span class = 'fonte-muito-grande'> {uiOutput('label_nivel_comp')} </span>"
                      )),
                      options = list(
                        placeholder = "Selecione o nível de análise"
                      ),
                      choices = c(
                        "Nacional" = "nacional",
                        "Região do país" = "regional",
                        "Estadual" = "estadual",
                        "Macrorregião de saúde estadual" = "macro",
                        "Região de saúde estadual" = "micro",
                        "Municipal" = "municipal"
                      ),
                      width = "95%"
                    )
                  )
                ),
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "input.nivel2 == 'regional' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "regiao2",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Região </span>"
                          ),
                          options = list(
                            placeholder = "Selecione uma região do país"
                          ),
                          choices = c(
                            "Centro-Oeste",
                            "Nordeste",
                            "Norte",
                            "Sudeste",
                            "Sul"
                          ),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.nivel2 == 'macro' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "estado_macro2",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Estado </span>"
                          ),
                          options = list(placeholder = "Selecione um estado"),
                          choices = sort(estados_choices),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "macro2",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Macrorregião de saúde estadual </span>"
                          ),
                          options = list(
                            placeholder = "Selecione uma macrorregião de saúde estadual estadual"
                          ),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.nivel2 == 'micro' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "estado_micro2",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Estado </span>"
                          ),
                          options = list(placeholder = "Selecione um estado"),
                          choices = sort(estados_choices),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "micro2",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Região de saúde estadual </span>"
                          ),
                          options = list(
                            placeholder = "Selecione uma região de saúde estadual"
                          ),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.nivel2 == 'estadual' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "estado2",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Estado </span>"
                          ),
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
                    condition = "input.nivel2 == 'municipal' & input.comparar == 'Sim'",
                    fluidRow(
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "estado_municipio2",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Estado </span>"
                          ),
                          options = list(placeholder = "Selecione um estado"),
                          choices = sort(estados_choices),
                          width = "95%"
                        )
                      ),
                      column(
                        width = 6,
                        selectizeInput(
                          inputId = "municipio2",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Município </span>"
                          ),
                          options = list(
                            placeholder = "Selecione um município"
                          ),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.comparar == 'Sim' & input.abas != 'bloco_4' & input.abas != 'bloco_9'",
                fluidRow(
                  column(
                    width = 12,
                    radioButtons(
                      inputId = "mostrar_referencia",
                      label = NULL,
                      choiceNames = list(
                        HTML(glue::glue(
                          "<span style = 'font-weight: normal'> Mostrar a linha relacionada ao valor de referência </span>"
                        )),
                        HTML(glue::glue(
                          "<span style = 'font-weight: normal'> Não mostrar a linha relacionada ao valor de referência </span>"
                        ))
                      ),
                      choiceValues = list(
                        "mostrar_referencia",
                        "nao_mostrar_referencia"
                      ),
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
                    label = HTML(
                      "<span class = 'fonte-muito-grande'> Bloco de indicadores </span>"
                    ),
                    options = list(
                      placeholder = "Selecione o bloco de indicadores"
                    ),
                    choices = c(
                      "1 - Condições socioeconômicas e de acesso ao serviço de saúde" = "bloco1",
                      "2 - Planejamento reprodutivo" = "bloco2",
                      "3 - Assistência pré-natal" = "bloco3",
                      "4 - Assistência ao parto" = "bloco4",
                      "5 - Condições de nascimento" = "bloco5",
                      "6 - Mortalidade e morbidade materna" = "bloco6",
                      "7 - Mortalidade fetal, perinatal e neonatal e morbidade neonatal" = "bloco7"
                    ),
                    width = "98%"
                  )
                ),
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "input.bloco != 'bloco4' & input.bloco != 'bloco6' & input.bloco != 'bloco7'",
                    selectizeInput(
                      inputId = "indicador",
                      label = HTML(
                        "<span class = 'fonte-muito-grande'> Indicador </span>"
                      ),
                      options = list(placeholder = "Selecione o indicador"),
                      choices = tabela_indicadores$indicador[which(
                        tabela_indicadores$bloco == "bloco1"
                      )],
                      width = "98%"
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      conditionalPanel(
                        condition = "input.bloco == 'bloco4' | input.bloco == 'bloco6' | input.bloco == 'bloco7'",
                        selectizeInput(
                          inputId = "tipo_do_indicador_blocos4_6_7",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Selecione o grupo de indicadores </span>"
                          ),
                          options = list(
                            placeholder = "Selecione um grupo de indicadores"
                          ),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    ),
                    column(
                      width = 6,
                      conditionalPanel(
                        condition = "input.bloco == 'bloco4' | input.bloco == 'bloco6' | input.bloco == 'bloco7'",
                        selectizeInput(
                          inputId = "indicador_blocos4_6_7",
                          label = HTML(
                            "<span class = 'fonte-muito-grande'> Indicador </span>"
                          ),
                          options = list(placeholder = "Selecione o indicador"),
                          choices = NULL,
                          width = "95%"
                        )
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  offset = 6,
                  width = 6,
                  conditionalPanel(
                    condition = {
                      indicadores_caixinha_adicional <- c(
                        "'Porcentagem de nascidos vivos com baixo peso ao nascer'",
                        "'Porcentagem de nascidos vivos prematuros'"
                      )

                      glue::glue(
                        "[{paste(indicadores_caixinha_adicional, collapse = ', ')}].includes(input.indicador)"
                      )
                    },
                    selectizeInput(
                      inputId = "indicador_uma_caixinha_adicional_bloco5",
                      label = HTML(
                        "<span class = 'fonte-muito-grande'> Aguarde... </span>"
                      ),
                      choices = NULL,
                      width = "99%"
                    )
                  )
                ),
                column(
                  offset = 6,
                  width = 6,
                  conditionalPanel(
                    condition = {
                      indicadores_uma_caixinha_adicional_bloco7 <- c(
                        "'Número de óbitos neonatais'",
                        "'Taxa de mortalidade neonatal por 1000 nascidos vivos'",
                        "'Taxa de mortalidade neonatal precoce (0 a 6 dias) por 1000 nascidos vivos'",
                        "'Taxa de mortalidade neonatal tardia (7 a 27 dias) por 1000 nascidos vivos'",
                        "'Número de óbitos perinatais'",
                        "'Taxa de mortalidade perinatal por 1000 nascidos vivos'",
                        "'Taxa de mortalidade neonatal por 1000 nascidos vivos '",
                        "'Taxa de mortalidade neonatal precoce por 1000 nascidos vivos  '",
                        "'Taxa de mortalidade neonatal tardia por 1000 nascidos vivos  '",
                        "'Porcentagem de óbitos neonatais por grupos de causas evitáveis'",
                        "'Porcentagem de óbitos neonatais por grupos de causas'",
                        "'Porcentagem de nascidos vivos segundo local de ocorrência do parto'",
                        "'Medianas de deslocamento segundo o local de ocorrência do parto'",
                        #"'Porcentagem de partos com peso < 1500g segundo local de ocorrência do parto'",
                        "'Porcentagem de internações neonatais por grupos de causas'",
                        "'Porcentagem de óbitos fetais por grupos de causas evitáveis'",
                        "'Porcentagem de óbitos perinatais por grupos de causas evitáveis'",
                        "'Porcentagem de óbitos fetais por grupos de causas'",
                        "'Porcentagem de óbitos perinatais por grupos de causas'",
                        "'Porcentagem de nascidos vivos de partos por local de ocorrência'",
                        "'Porcentagem de nascicidos vivos de partos ocorridos em hospital por profissional de assistência'"
                      )

                      glue::glue(
                        "[{paste(indicadores_uma_caixinha_adicional_bloco7, collapse = ', ')}].includes(input.indicador_blocos4_6_7) & (input.bloco == 'bloco7' | input.bloco == 'bloco4')"
                      )
                    },
                    selectizeInput(
                      inputId = "indicador_uma_caixinha_adicional_bloco7",
                      label = HTML(
                        "<span class = 'fonte-muito-grande'> Aguarde... </span>"
                      ),
                      choices = NULL,
                      width = "97%"
                    )
                  )
                ),
                column(
                  offset = 6,
                  width = 3,
                  conditionalPanel(
                    condition = {
                      indicadores_duas_caixinhas_adicionais <- c(
                        "'Número de óbitos fetais'",
                        "'Taxa de mortalidade fetal por 1000 nascidos vivos'",
                        "'Porcentagem de internações neonatais (até o 27º dia de vida) em relação ao total de partos no SUS'",
                        "'Porcentagem de internações neonatais (até o 27º dia de vida) em UTI em relação ao total de partos no SUS'",

                        "'Porcentagem de partos com peso < 1500g segundo região de ocorrência e disponibilidade de pelo menos quatro leitos de UTI neonatal'"
                      )

                      glue::glue(
                        "[{paste(indicadores_duas_caixinhas_adicionais, collapse = ', ')}].includes(input.indicador_blocos4_6_7) & (input.bloco == 'bloco7'|input.bloco == 'bloco4')"
                      )
                    },
                    selectizeInput(
                      inputId = "indicador_duas_caixinhas_adicionais1",
                      label = HTML(
                        "<span class = 'fonte-muito-grande'> Aguarde... </span>"
                      ),
                      choices = NULL,
                      width = "94%"
                    )
                  )
                ),
                column(
                  width = 3,
                  conditionalPanel(
                    condition = glue::glue(
                      "[{paste(indicadores_duas_caixinhas_adicionais, collapse = ', ')}].includes(input.indicador_blocos4_6_7) & (input.bloco == 'bloco7'|input.bloco == 'bloco4')"
                    ),
                    selectizeInput(
                      inputId = "indicador_duas_caixinhas_adicionais2",
                      label = HTML(
                        "<span class = 'fonte-muito-grande'> Aguarde... </span>"
                      ),
                      choices = NULL,
                      width = "94%"
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
                  label = HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Atualizar resultados </span>"
                  ),
                  style = "unite",
                  size = "sm"
                ),
                align = "center"
              )
            )
          ),
          style = "display: none;"
        ),

        tags$script(HTML(
          "
          var openTab = function(tabName){
            $('a', $('.sidebar')).each(function() {
              if(this.getAttribute('data-value') == tabName) {
                this.click()
              };
            });
          }
        "
        )),
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
            tabName = "bloco_7",
            mod_bloco_7_ui("bloco_7_1")
          ),
          # bs4Dash::bs4TabItem(
          #   tabName = "bloco_8",
          #   mod_bloco_8_ui("bloco_8_1")
          # ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_9",
            mod_bloco_9_ui("bloco_9_1")
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
            div(
              class = "div-titulo",
              HTML(
                "<span style='display: block; margin-bottom: 15px;'> </span>"
              ),
              h2(
                tags$b("A história de Aparecida"),
                style = "padding-left: 0.3em"
              ),
              hr(style = "margin-bottom: 0px;")
            ),
            HTML(
              "
              <p align='justify'; class = 'fonte-muito-grande' style='padding: 0 0.5em'>
              Para melhor entender os resultados dos indicadores apresentados neste painel em diferentes contextos e em como eles
              refletem as situações de vulnerabilidade da mulher ao óbito materno, acesse, clicando na imagem abaixo ou
              <a href = 'https://observatorioobstetricobr.org/projetos/aparecida-uma-historia-sobre-a-vunerabilidade-da-mulher-brasileira-a-morte-materna/' target = _blank>neste link</a>, a história de Aparecida -
              uma mulher preta, que mora num município pequeno, localizado no interior de um estado brasileiro. A história de Aparecida,
              apesar de não ser uma história real, retrata as condições de vida e saúde de muitas brasileiras, evidenciando a grande
              vulnerabilidade à morte materna a qual essas mulheres estão submetidas.
              </p>
              "
            ),
            HTML(
              '
              <div style = "display: flex; justify-content: center; align-items: center;">
                <div class="card2 blue2" style = "display: flex; justify-content: center; align-items: center; width: 55vw; height: 67vh;">
                  <a href = "https://observatorioobstetricobr.org/projetos/aparecida-uma-historia-sobre-a-vunerabilidade-da-mulher-brasileira-a-morte-materna/" target = "_blank">
                    <image src = "www/aparecida.png" style = "max-width: 100%; max-height: 100%;">
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
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www/global"),
      app_title = "Painel de Vigilância da Saúde Materna e Perinatal"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
