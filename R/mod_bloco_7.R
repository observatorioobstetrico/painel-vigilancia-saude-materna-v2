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

        #bloco_6_1-tabset1 .nav-link {
          border-style: solid
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
        HTML("<b>Relacionados à mortalidade fetal </b>"),
        value = "tabpanel_fetal",
        fluidRow(
          column(
            width=12,
            HTML(
              "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
                Neste painel, consideramos apenas óbitos fetais com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500 g.
                <span style='display: block; margin-bottom: 15px;'> </span>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos fetais e neonatais no país, incluindo desagregação de raça/cor, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-fetais-neonatais/' target = _blank>OOBr Óbitos Fetais e Neonatais</a>.
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
                inputId = ns('botao_resumo1'),
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
                uiOutput(ns("input_localidade_resumo_fetal")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_fetal_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_fetal_i2")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_fetal_i4")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_fetal_i3")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_evitaveis_fetal")), proxy.height = "300px") #[zzz]
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_principais_fetal")), proxy.height = "300px")
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
                     HTML("<b class = 'fonte-muito-grande'> Número de óbitos fetais &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = ns("parto_fetal"),
                        label = span(class = "fonte-grande", "Momento do óbito"),
                        options = list(placeholder = "Selecione o momento do óbito"),
                        choices = c(
                          "Geral" = "fetal_parto_geral",
                          "Antes do trabalho de parto" = "antes",
                          "Durante o trabalho de parto" = "durante"
                        ),
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = ns("faixa_peso_fetal"),
                        label = span(class = "fonte-grande", "Faixa de peso"),
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "peso_fetal",
                          "Menor que 1000 g" = "fetal_menos1000",
                          "De 1000 g a 1499 g" = "fetal_1000_1499",
                          "De 1500 g a 2499 g" = "fetal_1500_2499",
                          "Maior ou igual a 2500 g" = "fetal_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_fetal"), height = 380))
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
                     HTML("<b class = 'fonte-muito-grande'> Taxa de mortalidade fetal por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = ns("parto_fetal2"),
                        label = span(class = "fonte-grande", "Momento do óbito"),
                        options = list(placeholder = "Selecione o momento do óbito"),
                        choices = c(
                          "Geral" = "fetal_parto_geral",
                          "Antes do trabalho de parto" = "antes",
                          "Durante o trabalho de parto" = "durante"
                        ),
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = ns("faixa_peso_fetal2"),
                        label = span(class = "fonte-grande", "Faixa de peso"),
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "peso_fetal",
                          "Menor que 1000 g" = "fetal_menos1000",
                          "De 1000 g a 1499 g" = "fetal_1000_1499",
                          "De 1500 g a 2499 g" = "fetal_1500_2499",
                          "Maior ou igual a 2500 g" = "fetal_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_fetal"), height = 380))
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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual do momento do óbito fetal por faixa de peso &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_comparacao_fetal1"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_comparacao_fetal1"),
                          icon = icon("info"),
                          color = "primary",
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
                      strong(p("Selecione as faixas de peso:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("faixa_peso_dist_moment_obit_fetal"),
                          label    = NULL,
                          choices = c(
                            "Menor que 1000 g" = "dist_moment_obito_fetal_menos1000",
                            "De 1500 a 2499 g" = "dist_moment_obito_fetal_1500_2499",
                            "De 1000 a 1499 g" = "dist_moment_obito_fetal_1000_1499",
                            "Maior ou igual a 2500 g" = "dist_moment_obito_fetal_mais2500"
                          ),
                          selected = c(
                            "dist_moment_obito_fetal_menos1000", "dist_moment_obito_fetal_1000_1499",
                            "dist_moment_obito_fetal_1500_2499", "dist_moment_obito_fetal_mais2500"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_fetal"), height = 360))
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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual das faixas de peso por momento do óbito fetal &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_comparacao_fetal2"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_comparacao_fetal2"),
                          icon = icon("info"),
                          color = "primary",
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
                      strong(p("Selecione os momentos de óbito:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_dist_peso_fetal"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "dist_peso_fetal_antes",
                            "Durante o parto" = "dist_peso_fetal_durante"
                          ),
                          selected = c("dist_peso_fetal_antes", "dist_peso_fetal_durante")
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_fetal"), height = 390))
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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos fetais por grupos de causas segundo <a href = https://www.scielo.br/j/csp/a/Ss5zQXrmrGrGJvcVMKmJdqR/?format=pdf&lang=pt , target = _blank>França e Lansky (2009)</a> &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_grupos_fetal"),
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
                          #"Afecções respiratórias dos recém nascidos" = "respiratorias",
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
                      shinyWidgets::pickerInput(
                        inputId = ns("faixa_peso_fetal_grupos"),
                        label = HTML("<span class = 'fonte-grande'>Selecione, aqui, as faixas de peso consideradas:</span>"),
                        options = list(placeholder = "Selecione, aqui, as faixas de peso consideradas",
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = "Desselecionar todas",
                                       `select-all-text` = "Selecionar todas",
                                       `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Menor que 1000 g" = "menor_1000",
                          "De 1000 g a 1499 g" = "1000_a_1499",
                          "De 1500g a 2499g" = "1500_a_2499",
                          "Maior ou igual a 2500 g" = "2500_mais",
                          "Sem informação" = "sem_informacao"
                        ),
                        selected = c("menor_1000","1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
                        multiple = TRUE,
                        width = "98%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_fetal_grupos"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "fetal_grupos_antes", #[eee]
                            "Sem informação" = "fetal_grupos_sem_info_parto",
                            "Durante o parto" = "fetal_grupos_durante"
                          ),
                          selected = c("fetal_grupos_antes", "fetal_grupos_sem_info_parto", "fetal_grupos_durante")
                        )
                      )
                    ),
                    # column(
                    #   width = 6,
                    #   shinyWidgets::pickerInput(
                    #     inputId = ns("momento_obito_fetal_grupos"),
                    #     label = "Selecione, aqui, os momentos de óbito considerados:",
                    #     options = list(placeholder = "Selecione, aqui, os momentos de óbito considerados",
                    #                    `actions-box` = TRUE,
                    #                    `deselect-all-text` = "Desselecionar todas",
                    #                    `select-all-text` = "Selecionar todas",
                    #                    `none-selected-text` = "Nenhuma opção selecionada"),
                    #     choices = c(
                    #       "Antes do parto" = "fetal_grupos_antes", #[eee]
                    #       "Sem informação" = "fetal_grupos_sem_info_parto",
                    #       "Durante o parto" = "fetal_grupos_durante"
                    #     ),
                    #     selected = c("fetal_grupos_antes", "fetal_grupos_sem_info_parto", "fetal_grupos_durante"),
                    #     multiple = TRUE,
                    #     width = "98%"
                    #   )
                    # )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_fetal"), height = 420))
                )
              ),
              # column(
              #   width = 12,
              #   bs4Dash::bs4Card(
              #     width = 12,
              #     status = "primary",
              #     collapsible = FALSE,
              #     headerBorder = FALSE,
              #     style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              #     div(
              #       style = "height: 10%; display: flex; align-items: center;",
              #        HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos fetais segundo análise de evitabilidade (Fonte: <a href = 'http://tabnet.datasus.gov.br/cgi/sim/Obitos_Evitaveis_0_a_4_anos.pdf' , target = _blank>link</a>) &nbsp;</b>")
              #     ),
              #     hr(),
              #     fluidRow(
              #       column(
              #         width = 6,
              #         shinyWidgets::pickerInput(
              #           inputId = ns("cids_evitaveis_fetal"),
              #           label = HTML("<span class = 'fonte-grande'>Selecione, aqui, os grupos de interesse:</span>"),
              #           options = list(placeholder = "Selecione, aqui, os grupos de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
              #           choices = c(
              #             "Reduzível pelas ações de imunização" = "imunoprevencao",
              #             "Reduzíveis por adequada atenção à mulher na gestação" = "mulher_gestacao",
              #             "Reduzíveis por adequada atenção à mulher no parto" = "parto",
              #             "Reduzíveis por adequada atenção ao recém-nascido" = "recem_nascido",
              #             "Reduzíveis por ações de diagnóstico e tratamento adequado" = "tratamento",
              #             "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "saude",
              #             "Causas mal definidas" = "mal_definidas",
              #             "Demais causas (não claramente evitáveis)" = "outros"
              #           ),
              #           selected = c(
              #             "imunoprevencao",
              #             "mulher_gestacao",
              #             "parto",
              #             "recem_nascido",
              #             "tratamento",
              #             "saude",
              #             "mal_definidas",
              #             "outros"
              #           ),
              #           multiple = TRUE,
              #           width = "100%"
              #         )
              #       ),
              #       column(
              #         width = 6,
              #         strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
              #         tags$div(
              #           align = 'left',
              #           class = 'multicol',
              #           checkboxGroupInput(
              #             inputId = ns("momento_obito_fetal_evitaveis"),
              #             label    = NULL,
              #             choices = c(
              #               "Antes do parto" = "evitaveis_fetal_antes",
              #               "Durante o parto" = "evitaveis_fetal_durante"#,
              #               #"Faltante" = "evitaveis_fetal_faltantes"
              #             ),
              #             selected = c(
              #               "evitaveis_fetal_antes", "evitaveis_fetal_durante"
              #             )
              #           )
              #         )
              #       )
              #     ),
              #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_fetal"), height = 490))
              #   )
              # ),
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
                    HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos fetais segundo análise de evitabilidade do <a href = 'https://www.scielo.br/j/ress/a/cF66ngM4VB3YXV7Js8WynXC/?format=pdf&lang=pt' target = _blank>artigo de Vieira et Al (2011)</a> &nbsp;</b>")),
                   hr(),
                  fluidRow(
                    column(
                      width = 12,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_evitaveis_fetal2"),
                        label = HTML("<span class = 'fonte-grande'>Selecione, aqui, os grupos de interesse:</span>"),
                        options = list(placeholder = "Selecione, aqui, os grupos de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Imunoprevenção" = "imunoprevencao2",
                          "Mortes reduzíveis por adequada atenção à mulher na gestação" = "mulher_gestacao2",
                          "Mortes reduzíveis por adequada atenção à mulher no parto" = "parto2",
                          "Causas de morte mal-definidas" = "mal_definidas2",
                          "Causa básica não se aplica ao óbito fetal" = "nao_aplica2",
                          "Causas não claramente evitáveis" = "outros2"
                        ),
                        selected = c(
                          "imunoprevencao2",
                          "mulher_gestacao2",
                          "parto2",
                          "nao_aplica2",
                          "mal_definidas2",
                          "outros2"
                        ),
                        multiple = TRUE,
                        width = "99%"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("faixa_peso_fetal_evitaveis2"),
                        label = HTML("<span class = 'fonte-grande'>Selecione, aqui, as faixas de peso consideradas:</span>"),
                        options = list(placeholder = "Selecione, aqui, as faixas de peso consideradas",
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = "Desselecionar todas",
                                       `select-all-text` = "Selecionar todas",
                                       `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Menor que 1000 g" = "menor_1000",
                          "De 1000 g a 1499 g" = "1000_a_1499",
                          "De 1500g a 2499g" = "1500_a_2499",
                          "Maior ou igual a 2500 g" = "2500_mais",
                          "Sem informação" = "sem_informacao"
                        ),
                        selected = c("menor_1000","1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
                        multiple = TRUE,
                        width = "98%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_fetal_evitaveis2"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "evitaveis_fetal_antes", #[eee]
                            "Sem informação" = "evitaveis_fetal_sem_info_parto",
                            "Durante o parto" = "evitaveis_fetal_durante"
                          ),
                          selected = c(
                            "evitaveis_fetal_antes", "evitaveis_fetal_sem_info_parto", "evitaveis_fetal_durante"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_fetal2"), height = 420))
                 )
              )
            )
          )
        )
      ),
      #),

      tabPanel(
        HTML("<b>Relacionados à mortalidade perinatal </b>"),
        value = "tabpanel_perinatal",
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
                inputId = ns('botao_resumo2'),
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
                uiOutput(ns("input_localidade_resumo_perinatal")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i3")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i5")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i6")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_evitaveis_perinatal")), proxy.height = "300px") #[zzz]
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_principais_perinatal")), proxy.height = "300px")
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
                      selectizeInput(
                        inputId = ns("faixa_peso_perinatal_total"),
                        label = span(class = "fonte-grande", "Faixa de peso"),
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "perinatal_todos_total",
                          "Menor que 1000 g" = "perinatal_todos_peso_menos_1000",
                          "De 1000 g a 1499 g" = "perinatal_todos_peso_1000_1499",
                          "De 1500 g a 2499 g" = "perinatal_todos_peso_1500_2499",
                          "Maior ou igual a 2500 g" = "perinatal_todos_peso_mais_2500"
                        ),
                        # selected = c("perinatal_todos_total", "perinatal_total_menos1000", "perinatal_total_1000_1499",
                        #              "perinatal_total_1500_2499", "perinatal_total_mais2500"),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_perinatal"), height = 380))
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
                    HTML("<b class = 'fonte-muito-grande'> Taxa de mortalidade perinatal por 1000 nascidos vivos  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_perinatal_taxa_total"),
                        label = span(class = "fonte-grande", "Faixa de peso"),
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "taxa_perinatal_total",
                          "Menor que 1000 g" = "taxa_perinatal_total_menos1000",
                          "De 1000 g a 1499 g" = "taxa_perinatal_total_1000_1499",
                          "De 1500 g a 2499 g" = "taxa_perinatal_total_1500_2499",
                          "Maior ou igual a 2500 g" = "taxa_perinatal_total_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_perinatal"), height = 380))
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
                    HTML("<b class = 'fonte-muito-grande'> Distribuição percentual do momento do óbito perinatal por faixa de peso &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_comparacao_perinatal1"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_comparacao_perinatal1"),
                          icon = icon("info"),
                          color = "primary",
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
                      strong(p("Selecione as faixas de peso:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("faixa_peso_dist_moment_obit_perinat"),
                          label    = NULL,
                          choices = c(
                            "Menor que 1000 g" = "dist_moment_obito_perinat_menos1000",
                            "De 1500 a 2499 g" = "dist_moment_obito_perinat_1500_2499",
                            "De 1000 a 1499 g" = "dist_moment_obito_perinat_1000_1499",
                            "Maior ou igual a 2500 g" = "dist_moment_obito_perinat_mais2500"
                          ),
                          selected = c(
                            "dist_moment_obito_perinat_menos1000", "dist_moment_obito_perinat_1000_1499",
                            "dist_moment_obito_perinat_1500_2499", "dist_moment_obito_perinat_mais2500"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_perinatal"), height = 360))
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
                    HTML("<b class = 'fonte-muito-grande'> Distribuição percentual das faixas de peso por momento do óbito perinatal &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_comparacao_perinatal2"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_comparacao_perinatal2"),
                          icon = icon("info"),
                          color = "primary",
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
                      strong(p("Selecione os momentos de óbito:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_dist_peso_perinat"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "dist_peso_perinat_antes_parto",
                            "Durante o parto" = "dist_peso_perinat_durante_parto",
                            "Dia 0 de vida" = "dist_peso_perinat_dia_0",
                            "De 1 a 6 dias de vida" = "dist_peso_perinat_dia_1_6"
                          ),
                          selected = c(
                            "dist_peso_perinat_antes_parto", "dist_peso_perinat_durante_parto",
                            "dist_peso_perinat_dia_0", "dist_peso_perinat_dia_1_6"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_perinatal"), height = 360))
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
                        inputId = ns("cids_grupos_perinatal"),
                        label = HTML("<span class = 'fonte-grande'>Selecione, aqui, os grupos de interesse:</span>"),
                        options = list(placeholder = "Selecione, aqui, os grupos de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
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
                        width = "100%"
                      )
                    ),
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("faixa_peso_perinatal_grupos"),
                        label = HTML("<span class = 'fonte-grande'>Selecione, aqui, as faixas de peso consideradas:</span>"),
                        options = list(placeholder = "Selecione, aqui, as faixas de peso consideradas",
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = "Desselecionar todas",
                                       `select-all-text` = "Selecionar todas",
                                       `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Menor que 1000 g" = "menor_1000",
                          "De 1000 g a 1499 g" = "1000_a_1499",
                          "De 1500g a 2499g" = "1500_a_2499",
                          "Maior ou igual a 2500 g" = "2500_mais",
                          "Sem informação" = "sem_informacao"
                        ),
                        selected = c("menor_1000","1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
                        multiple = TRUE,
                        width = "98%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_perinatal_grupos"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "perinatal_grupos_antes",
                            "Durante o parto" = "perinatal_grupos_durante",
                            "Dia 0 de vida" = "perinatal_grupos_0_dias",
                            "De 1 a 6 dias de vida" = "perinatal_grupos_1_6_dias",
                            "Sem informação" = "perinatal_grupos_sem_info"
                          ),
                          selected = c(
                            "perinatal_grupos_antes", "perinatal_grupos_durante",
                            "perinatal_grupos_0_dias", "perinatal_grupos_1_6_dias",
                            "perinatal_grupos_sem_info"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_perinatal"), height = 390))
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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos perinatais por análise de evitabilidade (Fonte: <a href = http://tabnet.datasus.gov.br/cgi/sim/Obitos_Evitaveis_0_a_4_anos.pdf , target = _blank>link</a>) &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_evitaveis_perinatal"),
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
                        width = "100%"
                      )
                    ),
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("faixa_peso_perinatal_evitaveis"),
                        label = HTML("<span class = 'fonte-grande'>Selecione, aqui, as faixas de peso consideradas:</span>"),
                        options = list(placeholder = "Selecione, aqui, as faixas de peso consideradas",
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = "Desselecionar todas",
                                       `select-all-text` = "Selecionar todas",
                                       `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Menor que 1000 g" = "menor_1000",
                          "De 1000 g a 1499 g" = "1000_a_1499",
                          "De 1500g a 2499g" = "1500_a_2499",
                          "Maior ou igual a 2500 g" = "2500_mais",
                          "Sem informação" = "sem_informacao"
                        ),
                        selected = c("menor_1000","1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
                        multiple = TRUE,
                        width = "98%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_perinatal_evitaveis"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "evitaveis_perinatal_antes",
                            "Durante o parto" = "evitaveis_perinatal_durante",
                            "Dia 0 de vida" = "evitaveis_perinatal_0_dias",
                            "De 1 a 6 dias de vida" = "evitaveis_perinatal_1_6_dias",
                            "Sem informação" = "evitaveis_perinatal_sem_info"
                          ),
                          selected = c(
                            "evitaveis_perinatal_antes", "evitaveis_perinatal_durante",
                            "evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias",
                            "evitaveis_perinatal_sem_info"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_perinatal"), height = 390))
                )
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b>Relacionados à mortalidade neonatal</b>"),
        value = "tabpanel_neonatal",
        fluidRow(
          column(
            width = 12,
            HTML(
              "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos fetais e neonatais no país, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-fetais-neonatais/' target = _blank>OOBr Óbitos Fetais e Neonatais</a>.
                </b> </div>"
            ),
            hr(),
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>")
          )
        ),
        fluidRow(
          # column(
          #   width = 12,
          #   HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
          #   HTML(
          #     "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
          #       <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos maternos no país, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-grav-puerp/' target = _blank>OOBr Óbitos de Gestantes e Puérperas</a>.
          #       </b> </div>"
          #   ),
          #   hr(),
          #   HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
          # ),
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            div(
              HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
              shinyWidgets::actionBttn(
                inputId = ns('botao_resumo3'),
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
                uiOutput(ns("input_localidade_resumo_neonat")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i4")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i2")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i3")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i5")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_neonat_i6")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_evitaveis_neonatal")), proxy.height = "325px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_principais_neonatal")), proxy.height = "325px")
              )
            ),
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
                     HTML("<b class = 'fonte-muito-grande'> Número de óbitos neonatais &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("obitos_faixa_peso"),
                        label = span(class = "fonte-grande", "Faixa de peso ao nascer"),
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "obitos_neonat",
                          "Menor que 1000 g" = "obitos_neonat_menos1000",
                          "De 1000 g a 1499 g" = "obitos_neonat_1000_1499",
                          "De 1500 g a 2499 g" = "obitos_neonat_1500_2499",
                          "Maior ou igual a 2500 g" = "obitos_neonat_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_neonat"), height = 380))
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
                     HTML("<b class = 'fonte-muito-grande'> Taxa de mortalidade neonatal por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso"),
                        label = span(class = "fonte-grande", "Faixa de peso ao nascer"),
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat",
                          "Menor que 1000 g" = "mort_neonat_menos1000",
                          "De 1000 g a 1499 g" = "mort_neonat_1000_1499",
                          "De 1500 g a 2499 g" = "mort_neonat_1500_2499",
                          "Maior ou igual a 2500 g" = "mort_neonat_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_neonat"), height = 380))
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
                     HTML("<b class = 'fonte-muito-grande'> Taxa de mortalidade neonatal com idade 0 a 6 dias por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_precoc"),
                        label = span(class = "fonte-grande", "Faixa de peso ao nascer"),
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat_precoc",
                          "Menor que 1000 g" = "mort_neonat_precoc_menos1000",
                          "De 1000 g a 1499 g" = "mort_neonat_precoc_1000_1499",
                          "De 1500 g a 2499 g" = "mort_neonat_precoc_1500_2499",
                          "Maior ou igual a 2500 g" = "mort_neonat_precoc_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_neonat"), height = 380))
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
                     HTML("<b class = 'fonte-muito-grande'> Taxa de mortalidade neonatal com idade 7 a 27 dias por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_tardia"),
                        label = span(class = "fonte-grande", "Faixa de peso ao nascer"),
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat_tardia",
                          "Menor que 1000 g" = "mort_neonat_tardia_menos1000",
                          "De 1000 g a 1499 g" = "mort_neonat_tardia_1000_1499",
                          "De 1500 g a 2499 g" = "mort_neonat_tardia_1500_2499",
                          "Maior ou igual a 2500 g" = "mort_neonat_tardia_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_neonat"), height = 380))
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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual do momento do óbito neonatal por faixa de peso &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_comparacao_neonatal1"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_comparacao_neonatal1"),
                          icon = icon("info"),
                          color = "primary",
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
                      strong(p("Selecione as faixas de peso:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("faixa_peso_dist_moment_obit_neonat"),
                          label    = NULL,
                          choices = c(
                            "Menor que 1000 g" = "dist_moment_obito_neonat_menos1000",
                            "De 1000 g a 1499 g" = "dist_moment_obito_neonat_1000_1499",
                            "De 1500 g a 2499 g" = "dist_moment_obito_neonat_1500_2499",
                            "Maior ou igual a 2500 g" = "dist_moment_obito_neonat_mais2500"
                          ),
                          selected = c(
                            "dist_moment_obito_neonat_menos1000", "dist_moment_obito_neonat_1000_1499",
                            "dist_moment_obito_neonat_1500_2499", "dist_moment_obito_neonat_mais2500"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_neonat"), height = 360))
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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual das faixas de peso por momento do óbito neonatal &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_comparacao_neonatal2"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_comparacao_neonatal2"),
                          icon = icon("info"),
                          color = "primary",
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
                      strong(p("Selecione os momentos de óbito:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_dist_peso_neonat"),
                          label    = NULL,
                          choices = c(
                            "Dia 0 de vida" = "dist_peso_neonat_dia_0",
                            "De 1 a 6 dias de vida" = "dist_peso_neonat_dia_1_6",
                            "De 7 a 27 dias de vida" = "dist_peso_neonat_dia_7_28"
                          ),
                          selected = c(
                            "dist_peso_neonat_dia_0",
                            "dist_peso_neonat_dia_1_6",
                            "dist_peso_neonat_dia_7_28"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_neonat"), height = 370))
                )
              ),
              # column(
              #   width = 12,
              #   bs4Dash::bs4Card(
              #     width = 12,
              #     status = "primary",
              #     collapsible = FALSE,
              #     headerBorder = FALSE,
              #     style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              #     div(
              #       style = "height: 10%; display: flex; align-items: center;",
              #        HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos neonatais por causas principais definidas pelo DATASUS (Fonte: <a href = http://www2.datasus.gov.br/cid10/V2008/WebHelp/p00_p96.htm , target = _blank>link</a>) &nbsp;</b>")
              #     ),
              #     hr(),
              #     fluidRow(
              #       column(
              #         width = 12,
              #         shinyWidgets::pickerInput(
              #           inputId = ns("cids_principais_neonatal"),
              #           label = "Selecione os grupos de causas principais da CID-10:",
              #           options = list(placeholder = "Selecione os grupos de causas principais", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
              #           choices = c(
              #             "(A00-B99) Infecciosas" = "principais_neonatal_a00_b99",
              #             #"(J00-J99) Respiratórias" = "principais_neonatal_j00_j99",
              #             "(P00-P04) Feto e recém nascido afetado por fatores maternos e por condições da gravidez, do trabalho de parto e do parto" = "principais_neonatal_p00_p04",
              #             "(P05-P08) Transtornos relacionados com a duração da gestação e com o crescimento fetal" = "principais_neonatal_p05_p08",
              #             "(P10-P15) Traumatismo de parto" = "principais_neonatal_p10_p15",
              #             "(P20-P29) Transtornos respiratórios e cardiovasculares específicos do período neonatal" = "principais_neonatal_p20_p29",
              #             "(P35-P39) Infeccções específicas do período neonatal" = "principais_neonatal_p35_p39",
              #             "(P50-P61) Transtornos hemorrágicos e hematológicos do feto e do recém-nascido" = "principais_neonatal_p50_p61",
              #             "(P70-P74) Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido" = "principais_neonatal_p70_p74",
              #             "(P75-P78) Transtornos do aparelho digestivo do feto ou do recém-nascido" = "principais_neonatal_p75_p78",
              #             "(P80-P83) Afecções comprometendo o tegumento e a regulação térmica do feto e do recém-nascido" = "principais_neonatal_p80_p83",
              #             "(P90-P96) Outros transtornos originados do período neonatal" = "principais_neonatal_p90_p96",
              #             "(Q00-Q99) Anomalias congênitas" = "principais_neonatal_q00_q99",
              #             "Outros" = "principais_neonatal_outros"
              #           ),
              #           selected = NULL,
              #           multiple = TRUE,
              #           width = "100%"
              #         )
              #       )
              #     ),
              #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_principais_neonatal"), height = 500))
              #   )
              # ),

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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos neonatais por grupos de causas segundo <a href = https://www.scielo.br/j/csp/a/Ss5zQXrmrGrGJvcVMKmJdqR/?format=pdf&lang=pt , target = _blank>França e Lansky (2009)</a> &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_grupos_neonatal"),
                        label = HTML("<span class = 'fonte-grande'>Selecione os grupos de causas:</span>"),
                        options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
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
                      shinyWidgets::pickerInput(
                        inputId = ns("faixa_peso_neonatal_grupos"),
                        label = HTML("<span class = 'fonte-grande'>Selecione, aqui, as faixas de peso consideradas:</span>"),
                        options = list(placeholder = "Selecione, aqui, as faixas de peso consideradas",
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = "Desselecionar todas",
                                       `select-all-text` = "Selecionar todas",
                                       `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Menor que 1000 g" = "menor_1000",
                          "De 1000 g a 1499 g" = "1000_a_1499",
                          "De 1500g a 2499g" = "1500_a_2499",
                          "Maior ou igual a 2500 g" = "2500_mais",
                          "Sem informação" = "sem_informacao"
                        ),
                        selected = c("menor_1000","1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
                        multiple = TRUE,
                        width = "98%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_neonatal_grupos"),
                          label    = NULL,
                          choices = c(
                            "Dia 0 de vida" = "0_dias",
                            "De 1 a 6 dias de vida" = "1_6_dias",
                            "De 7 a 27 dias de vida" = "7_27_dias"
                          ),
                          selected = c(
                            "0_dias",
                            "1_6_dias",
                            "7_27_dias"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_neonatal"), height = 420))
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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual dos óbitos neonatais por análise de evitabilidade (Fonte: <a href = http://tabnet.datasus.gov.br/cgi/sim/Obitos_Evitaveis_0_a_4_anos.pdf , target = _blank>link</a>) &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_evitaveis_neonatal"),
                        label = HTML("<span class = 'fonte-grande'>Selecione os grupos por análise de evitabilidade:</span>"),
                        options = list(placeholder = "Selecione os grupos por análise de evitabilidade", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
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
                      shinyWidgets::pickerInput(
                        inputId = ns("faixa_peso_neonatal_evitaveis"),
                        label = HTML("<span class = 'fonte-grande'>Selecione, aqui, as faixas de peso consideradas:</span>"),
                        options = list(placeholder = "Selecione, aqui, as faixas de peso consideradas",
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = "Desselecionar todas",
                                       `select-all-text` = "Selecionar todas",
                                       `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Menor que 1000 g" = "menor_1000",
                          "De 1000 g a 1499 g" = "1000_a_1499",
                          "De 1500g a 2499g" = "1500_a_2499",
                          "Maior ou igual a 2500 g" = "2500_mais",
                          "Sem informação" = "sem_informacao"
                        ),
                        selected = c("menor_1000","1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
                        multiple = TRUE,
                        width = "98%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_neonatal_evitaveis"),
                          label    = NULL,
                          choices = c(
                            "Dia 0 de vida" = "evitaveis_neonatal_0_dias",
                            "De 1 a 6 dias de vida" = "evitaveis_neonatal_1_6_dias",
                            "De 7 a 27 dias de vida" = "evitaveis_neonatal_7_27_dias"
                          ),
                          selected = c(
                            "evitaveis_neonatal_0_dias",
                            "evitaveis_neonatal_1_6_dias",
                            "evitaveis_neonatal_7_27_dias"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_neonatal"), height = 420))
                )
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b>Relacionados à morbidade neonatal</b>"),
        value = "tabpanel_morbidade_neonatal",
        fluidRow(
          column(
            width = 12,
            HTML(
              "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
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
                inputId = ns('botao_resumo4'),
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
                uiOutput(ns("input_localidade_resumo_morbidade_neonatal")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_morbidade_neonatal_i1")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_morbidade_neonatal_i2")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_morbidade_neonatal_i3")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_principais_morbidade_neonatal")), proxy.height = "300px")
              )
            ),
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
                     HTML("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida &nbsp;</b>"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao_explicacao_indicador"),
                      icon = icon("info"),
                      color = "primary",
                      style = "material-circle",
                      size = "xs"
                    ),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao_morbidade1"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao_morbidade1"),
                          icon = icon("triangle-exclamation", style = "color: red"),
                          color = "warning",
                          style = "material-circle",
                          size = "xs"
                        )
                      )
                    )
                  ),
                  hr(),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_morbidade_neonatal"), height = 460))
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
                     HTML("<b class = 'fonte-muito-grande'> Porcentagem de internações neonatais (até o 27º dia de vida) em relação ao total de partos no SUS &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = ns("local_internacao_sih"),
                        label = span(class = "fonte-grande", "Local da internação"),
                        options = list(placeholder = "Selecione o local de internação"),
                        choices = c(
                          "Todos" = "geral",
                          "Dentro da macrorregião de saúde estadual" = "na_macro",
                          "Fora da macrorregião de saúde estadual" = "fora_macro"
                        ),
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Idade, em dias, do bebê", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("idade_dias_sih"),
                          label = NULL,
                          choices = c(
                            "0 dias" = "0_dias",
                            "1 a 6 dias" = "1_a_6_dias",
                            "7 a 27 dias" = "7_a_27_dias"
                          ),
                          selected = c("0_dias", "1_a_6_dias", "7_a_27_dias")
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_morbidade_neonatal"), height = 360))
                )
              ),

              column(
                width = 6,
                offset = 3,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                     HTML("<b class = 'fonte-muito-grande'> Porcentagem de internações neonatais (até o 27º dia de vida) em UTI em relação ao total de partos no SUS &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = ns("local_internacao_uti_sih"),
                        label = span(class = "fonte-grande", "Local da internação"),
                        options = list(placeholder = "Selecione o local de internação"),
                        choices = c(
                          "Todos" = "geral",
                          "Dentro da macrorregião de saúde estadual" = "na_macro",
                          "Fora da macrorregião de saúde estadual" = "fora_macro"
                        ),
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Idade, em dias, do bebê", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("idade_dias_uti_sih"),
                          label = NULL,
                          choices = c(
                            "0 dias" = "0_dias",
                            "1 a 6 dias" = "1_a_6_dias",
                            "7 a 27 dias" = "7_a_27_dias"
                          ),
                          selected = c("0_dias", "1_a_6_dias", "7_a_27_dias")
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_morbidade_neonatal"), height = 360))
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
                     HTML("<b class = 'fonte-muito-grande'> Distribuição percentual das internações neonatais por grupos de causas segundo <a href = https://www.scielo.br/j/csp/a/Ss5zQXrmrGrGJvcVMKmJdqR/?format=pdf&lang=pt , target = _blank>França e Lansky (2009)</a> &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_grupos_morbidade_neonatal"),
                        label = HTML("<span class = 'fonte-grande'>Selecione os grupos de causas:</span>"),
                        options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Prematuridade" = "prematuridade",
                          "Infecções" = "infeccoes",
                          "Asfixia/Hipóxia" = "asfixia",
                          "Anomalia congênita" = "ma_formacao",
                          "Afecções respiratórias do recém-nascido" = "afeccoes_respiratorias",
                          "Fatores maternos relacionados à gravidez" = "fatores_maternos",
                          "Afecções originais no período perinatal" = "afeccoes_perinatal",
                          "Mal definidas" = "mal_definidas",
                          "Icterícia neonatal" = "ictericia",
                          "Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido" = "endocrinos",
                          "Problemas de alimentação do recém-nascido"= "alimentacao",
                          "Transtornos cardíacos originados no período perinatal" = "cardiacos_perinatal",
                          "Demais causas" = "outros"

                        ),
                        selected = c(
                          "prematuridade",
                          "infeccoes",
                          "asfixia",
                          "ma_formacao",
                          "afeccoes_respiratorias",
                          "fatores_maternos",
                          "afeccoes_perinatal",
                          "mal_definidas",
                          "outros",
                          "ictericia",
                          "alimentacao",
                          "endocrinos",
                          "cardiacos_perinatal"
                        ),
                        multiple = TRUE,
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de internação considerados:", style = "margin-bottom: 0.5rem", class = "fonte-grande")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_internacao_neonatal_grupos"),
                          label    = NULL,
                          choices = c(
                            "Dia 0 de vida" = "morbidade_neonatal_grupos_0_dias",
                            "De 1 a 6 dias de vida" = "morbidade_neonatal_grupos_1_6_dias",
                            "De 7 a 27 dias de vida" = "morbidade_neonatal_grupos_7_27_dias"
                          ),
                          selected = c(
                            "morbidade_neonatal_grupos_0_dias",
                            "morbidade_neonatal_grupos_1_6_dias",
                            "morbidade_neonatal_grupos_7_27_dias"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_morbidade_neonatal"), height = 480))
                )
              )
            )
          )
        )
      )
    ) #fim do tabpanel morbidade neonatal
  )
}

#' bloco_7 Server Functions
#'
#' @noRd
mod_bloco_7_server <- function(id, filtros){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Criando um data.frame com os cálculos dos indicadores -------------------
    ## Tive que fazer um tratamento especial para algumas variáveis. Ver no arquivo funcoes_globais.R
    bloco7_calcs <- reactive({
      df_calcs_aux1 <- data.frame(
        tipo = c("local", "referencia"),

        obitos_neonat = rep("sum(obitos_27dias, na.rm = T)", 2),
        obitos_neonat_menos1000 = rep("sum(obitos_27dias_menos1000, na.rm = T)", 2),
        obitos_neonat_1000_1499 = rep("sum(obitos_27dias_1000_1499, na.rm = T)", 2),
        obitos_neonat_1500_2499 = rep("sum(obitos_27dias_1500_2499, na.rm = T)", 2),
        obitos_neonat_mais2500 = rep("sum(obitos_27dias_mais2500, na.rm = T)", 2),

        mort_neonat = c("round(sum(obitos_27dias, na.rm = T)/sum(nascidos, na.rm = T) *1000, 1)", "5"),
        mort_neonat_precoc = c("round(sum(obitos_6dias, na.rm = T)/sum(nascidos, na.rm = T) *1000, 1)", "3.75"),
        mort_neonat_tardia = c("round(sum(obitos_7_27dias, na.rm = T)/sum(nascidos, na.rm = T) *1000, 1)", "1.25"),
        mort_neonat_menos1000 = rep("round(sum(obitos_27dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1)", 2),
        mort_neonat_precoc_menos1000 = rep("round(sum(obitos_6dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1)", 2),
        mort_neonat_tardia_menos1000 = rep("round(sum(obitos_7_27dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1)", 2),
        mort_neonat_1000_1499 = rep("round(sum(obitos_27dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1)", 2),
        mort_neonat_precoc_1000_1499 = rep("round(sum(obitos_6dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1)", 2),
        mort_neonat_tardia_1000_1499 = rep("round(sum(obitos_7_27dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1)", 2),
        mort_neonat_1500_2499 = rep("round(sum(obitos_27dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1)", 2),
        mort_neonat_precoc_1500_2499 = rep("round(sum(obitos_6dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1)", 2),
        mort_neonat_tardia_1500_2499 = rep("round(sum(obitos_7_27dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1)", 2),
        mort_neonat_mais2500 = rep("round(sum(obitos_27dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1)", 2),
        mort_neonat_precoc_mais2500 = rep("round(sum(obitos_6dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1)", 2),
        mort_neonat_tardia_mais2500 = rep("round(sum(obitos_7_27dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1)", 2),

        obitos_fetais = rep("sum(obitos_fetais_mais_22sem, na.rm = T)", 2),
        fetal_peso_menos_1000 = rep("sum(fetal_peso_menos_1000, na.rm = T)", 2),
        fetal_peso_1000_1499 = rep("sum(fetal_peso_1000_1499, na.rm = T)", 2),
        fetal_peso_1500_2499 = rep("sum(fetal_peso_1500_2499, na.rm = T)", 2),
        fetal_peso_mais_2500 = rep("sum(fetal_peso_mais_2500, na.rm = T)", 2),
        fetal_antes = rep("sum(fetal_antes, na.rm = T)", 2),
        fetal_durante = rep("sum(fetal_durante, na.rm = T)", 2),
        fetal_depois = rep("sum(fetal_depois, na.rm = T)", 2),
        fetal_antes_peso_menos_1000 = rep("sum(fetal_antes_peso_menos_1000, na.rm = T)", 2),
        fetal_antes_peso_1000_1499 = rep("sum(fetal_antes_peso_1000_1499, na.rm = T)", 2),
        fetal_antes_peso_1500_2499 = rep("sum(fetal_antes_peso_1500_2499, na.rm = T)", 2),
        fetal_antes_peso_mais_2500 = rep("sum(fetal_antes_peso_mais_2500, na.rm = T)", 2),
        fetal_durante_peso_menos_1000 = rep("sum(fetal_durante_peso_menos_1000, na.rm = T)", 2),
        fetal_durante_peso_1000_1499 = rep("sum(fetal_durante_peso_1000_1499, na.rm = T)", 2),
        fetal_durante_peso_1500_2499 = rep("sum(fetal_durante_peso_1500_2499, na.rm = T)", 2),
        fetal_durante_peso_mais_2500 = rep("sum(fetal_durante_peso_mais_2500, na.rm = T)", 2),
        fetal_depois_peso_menos_1000 = rep("sum(fetal_depois_peso_menos_1000, na.rm = T)", 2),
        fetal_depois_peso_1000_1499 = rep("sum(fetal_depois_peso_1000_1499, na.rm = T)", 2),
        fetal_depois_peso_1500_2499 = rep("sum(fetal_depois_peso_1500_2499, na.rm = T)", 2),
        fetal_depois_peso_mais_2500 = rep("sum(fetal_depois_peso_mais_2500, na.rm = T)", 2),

        taxa_mort_fetal = c("round(sum(obitos_fetais_mais_22sem, na.rm = T)/(sum(nascidos, na.rm = T)+sum(obitos_fetais_mais_22sem, na.rm = T)) *1000, 1)", "5"),
        taxa_mort_fetal_peso_menos_1000 = rep("round(sum(fetal_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_peso_menos_1000, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_peso_1000_1499 = rep("round(sum(fetal_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_peso_1000_1499, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_peso_1500_2499 = rep("round(sum(fetal_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_peso_1500_2499, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_peso_mais_2500 = rep("round(sum(fetal_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_peso_mais_2500, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_antes = rep("round(sum(fetal_antes, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_antes, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_durante = rep("round(sum(fetal_durante, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_durante, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_depois = rep("round(sum(fetal_depois, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_depois, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_antes_peso_menos_1000 = rep("round(sum(fetal_antes_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_antes_peso_menos_1000, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_antes_peso_1000_1499 = rep("round(sum(fetal_antes_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_antes_peso_1000_1499, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_antes_peso_1500_2499 = rep("round(sum(fetal_antes_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_antes_peso_1500_2499, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_antes_peso_mais_2500 = rep("round(sum(fetal_antes_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_antes_peso_mais_2500, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_durante_peso_menos_1000 = rep("round(sum(fetal_durante_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_durante_peso_menos_1000, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_durante_peso_1000_1499 = rep("round(sum(fetal_durante_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_durante_peso_1000_1499, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_durante_peso_1500_2499 = rep("round(sum(fetal_durante_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_durante_peso_1500_2499, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_durante_peso_mais_2500 = rep("round(sum(fetal_durante_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_durante_peso_mais_2500, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_depois_peso_menos_1000 = rep("round(sum(fetal_depois_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_depois_peso_menos_1000, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_depois_peso_1000_1499 = rep("round(sum(fetal_depois_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_depois_peso_1000_1499, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_depois_peso_1500_2499 = rep("round(sum(fetal_depois_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_depois_peso_1500_2499, na.rm = T)) *1000, 1)", 2),
        taxa_mort_fetal_depois_peso_mais_2500 = rep("round(sum(fetal_depois_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_depois_peso_mais_2500, na.rm = T)) *1000, 1)", 2),

        # Variáveis número de óbitos fetais mais de 28 semanas (critério oms)
        # obitos_fetais_oms = rep("sum(obitos_fetais_mais_28sem, na.rm=T)", 2),
        # fetal_oms_peso_menos_1000 = rep("sum(peso_menos_1000_mais_28sem, na.rm = T)", 2),
        # fetal_oms_peso_1000_1499 = rep("sum(peso_1000_1499_mais_28sem, na.rm=T)", 2),
        # fetal_oms_peso_1500_2499 = rep("sum(peso_1500_2499_mais_28sem, na.rm=T)", 2),
        # fetal_oms_peso_mais_2500 = rep("sum(peso_mais_2500_mais_28sem, na.rm=T)", 2),
        # fetal_oms_antes = rep("sum(perinatal_antes)", 2),
        # fetal_oms_durante = rep("sum(perinatal_durante)", 2),
        # fetal_oms_antes_peso_menos_1000 = rep("sum(perinatal_antes_peso_menos_1000)", 2),
        # fetal_oms_antes_peso_1000_1499 = rep("sum(perinatal_antes_peso_1000_1499)", 2),
        # fetal_oms_antes_peso_1500_2499 = rep("sum(perinatal_antes_peso_1500_2499)", 2),
        # fetal_oms_antes_peso_mais_2500 = rep("sum(perinatal_antes_peso_mais_2500)", 2),
        # fetal_oms_durante_peso_menos_1000 = rep("sum(perinatal_durante_peso_menos_1000)", 2),
        # fetal_oms_durante_peso_1000_1499 = rep("sum(perinatal_durante_peso_1000_1499)", 2),
        # fetal_oms_durante_peso_1500_2499 = rep("sum(perinatal_durante_peso_1500_2499)", 2),
        # fetal_oms_durante_peso_mais_2500 = rep("sum(perinatal_durante_peso_mais_2500)", 2),

        # Variáveis sobre taxa de mortalidades fetal para mais de 28 semanas (critério oms)
        # taxa_mort_fetal_oms = c("round(sum(obitos_fetais_mais_28sem)/(sum(nascidos)+sum(obitos_fetais_mais_28sem)) *1000, 1)", "5"),
        # taxa_mort_fetal_oms_peso_menos_1000 = rep("round(sum(peso_menos_1000_mais_28sem)/(sum(nascidos_menos1000)+sum(peso_menos_1000_mais_28sem))*1000, 1)", 2),
        # taxa_mort_fetal_oms_peso_1000_1499 = rep("round(sum(peso_1000_1499_mais_28sem)/(sum(nascidos_1000_1499)+sum(peso_1000_1499_mais_28sem))*1000, 1)", 2),
        # taxa_mort_fetal_oms_peso_1500_2499 = rep("round(sum(peso_1500_2499_mais_28sem)/(sum(nascidos_1500_2499)+sum(peso_1500_2499_mais_28sem))*1000, 1)", 2),
        # taxa_mort_fetal_oms_peso_mais_2500 = rep("round(sum(peso_mais_2500_mais_28sem)/(sum(nascidos_mais2500)+sum(peso_mais_2500_mais_28sem)) *1000, 1)", 2),
        # taxa_mort_fetal_oms_antes = rep("round(sum(perinatal_antes)/(sum(nascidos)+sum(perinatal_antes))*1000, 1)", 2),
        # taxa_mort_fetal_oms_durante = rep("round(sum(perinatal_durante)/(sum(nascidos)+sum(perinatal_durante))*1000, 1)", 2),
        # taxa_mort_fetal_oms_antes_peso_menos_1000 = rep("round(sum(perinatal_antes_peso_menos_1000)/(sum(nascidos_menos1000)+sum(perinatal_antes_peso_menos_1000))*1000, 1)", 2),
        # taxa_mort_fetal_oms_antes_peso_1000_1499 = rep("round(sum(perinatal_antes_peso_1000_1499)/(sum(nascidos_1000_1499)+sum(perinatal_antes_peso_1000_1499))*1000, 1)",2),
        # taxa_mort_fetal_oms_antes_peso_1500_2499 = rep("round(sum(perinatal_antes_peso_1500_2499)/(sum(nascidos_1500_2499)+sum(perinatal_antes_peso_1500_2499))*1000, 1)", 2),
        # taxa_mort_fetal_oms_antes_peso_mais_2500 = rep("round(sum(perinatal_antes_peso_mais_2500)/(sum(nascidos_mais2500)+sum(perinatal_antes_peso_mais_2500))*1000, 1)",2),
        # taxa_mort_fetal_oms_durante_peso_menos_1000 = rep("round(sum(perinatal_durante_peso_menos_1000)/(sum(nascidos_menos1000)+sum(perinatal_durante_peso_menos_1000))*1000, 1)", 2),
        # taxa_mort_fetal_oms_durante_peso_1000_1499 = rep("round(sum(perinatal_durante_peso_1000_1499)/(sum(nascidos_1000_1499)+sum(perinatal_durante_peso_1000_1499))*1000, 1)", 2),
        # taxa_mort_fetal_oms_durante_peso_1500_2499 = rep("round(sum(perinatal_durante_peso_1500_2499)/(sum(nascidos_1500_2499)+sum(perinatal_durante_peso_1500_2499))*1000, 1)", 2),
        # taxa_mort_fetal_oms_durante_peso_mais_2500 = rep("round(sum(perinatal_durante_peso_mais_2500)/(sum(nascidos_mais2500)+sum(perinatal_durante_peso_mais_2500))*1000, 1)", 2),

        perinatal_todos_total = rep("sum(perinatal_todos_total, na.rm=T)", 2),
        perinatal_todos_peso_menos_1000 = rep("sum(perinatal_todos_peso_menos_1000, na.rm=T)", 2),
        perinatal_todos_peso_1000_1499 = rep("sum(perinatal_todos_peso_1000_1499, na.rm=T)", 2),
        perinatal_todos_peso_1500_2499 = rep("sum(perinatal_todos_peso_1500_2499, na.rm=T)", 2),
        perinatal_todos_peso_mais_2500 = rep("sum(perinatal_todos_peso_mais_2500, na.rm=T)", 2),
        perinatal_todos_antes = rep("sum(perinatal_todos_antes, na.rm=T)", 2),
        perinatal_todos_durante = rep("sum(perinatal_todos_durante, na.rm=T)", 2),
        perinatal_todos_0dias = rep("sum(perinatal_todos_0dias, na.rm=T)", 2),
        perinatal_todos_1_6dias = rep("sum(perinatal_todos_1_6dias, na.rm=T)", 2),
        perinatal_todos_0_6dias = rep("sum(perinatal_todos_0_6dias, na.rm=T)", 2),
        perinatal_todos_antes_menos_1000 = rep("sum(perinatal_todos_antes_menos_1000, na.rm=T)", 2),
        perinatal_todos_antes_1000_1499 = rep("sum(perinatal_todos_antes_1000_1499, na.rm=T)", 2),
        perinatal_todos_antes_1500_2499 = rep("sum(perinatal_todos_antes_1500_2499, na.rm=T)", 2),
        perinatal_todos_antes_mais_2500 = rep("sum(perinatal_todos_antes_mais_2500, na.rm=T)", 2),
        perinatal_todos_durante_menos_1000 = rep("sum(perinatal_todos_durante_menos_1000, na.rm=T)", 2),
        perinatal_todos_durante_1000_1499 = rep("sum(perinatal_todos_durante_1000_1499, na.rm=T)", 2),
        perinatal_todos_durante_1500_2499 = rep("sum(perinatal_todos_durante_1500_2499, na.rm=T)", 2),
        perinatal_todos_durante_mais_2500 = rep("sum(perinatal_todos_durante_mais_2500, na.rm=T)", 2),
        perinatal_todos_0dias_menos_1000 = rep("sum(perinatal_todos_0dias_menos_1000, na.rm=T)", 2),
        perinatal_todos_0dias_1000_1499 = rep("sum(perinatal_todos_0dias_1000_1499, na.rm=T)", 2),
        perinatal_todos_0dias_1500_2499 = rep("sum(perinatal_todos_0dias_1500_2499, na.rm=T)", 2),
        perinatal_todos_0dias_mais_2500 = rep("sum(perinatal_todos_0dias_mais_2500, na.rm=T)", 2),
        perinatal_todos_1_6dias_menos_1000 = rep("sum(perinatal_todos_1_6dias_menos_1000, na.rm=T)", 2),
        perinatal_todos_1_6dias_1000_1499 = rep("sum(perinatal_todos_1_6dias_1000_1499, na.rm=T)", 2),
        perinatal_todos_1_6dias_1500_2499 = rep("sum(perinatal_todos_1_6dias_1500_2499, na.rm=T)", 2),
        perinatal_todos_1_6dias_mais_2500 = rep("sum(perinatal_todos_1_6dias_mais_2500, na.rm=T)", 2),
        perinatal_todos_0_6dias_menos_1000 = rep("sum(perinatal_todos_0_6dias_menos_1000, na.rm=T)", 2),
        perinatal_todos_0_6dias_1000_1499 = rep("sum(perinatal_todos_0_6dias_1000_1499, na.rm=T)", 2),
        perinatal_todos_0_6dias_1500_2499 = rep("sum(perinatal_todos_0_6dias_1500_2499, na.rm=T)", 2),
        perinatal_todos_0_6dias_mais_2500 = rep("sum(perinatal_todos_0_6dias_mais_2500, na.rm=T)", 2),

        # obitos_perinatal_oms = rep("sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias)", 2),
        # perinatal_oms_menos1000 = rep("sum(peso_menos_1000_mais_28sem, na.rm = T) + sum(obitos_6dias_menos1000)", 2),
        # perinatal_oms_1000_1499 = rep("sum(peso_1000_1499_mais_28sem, na.rm=T) + sum(obitos_6dias_1000_1499)", 2),
        # perinatal_oms_1500_2499 = rep("sum(peso_1500_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_2499)", 2),
        # perinatal_oms_mais2500 = rep("sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500)", 2),

        taxa_perinatal_total = c("round((sum(obitos_fetais_mais_22sem, na.rm=T) + sum(obitos_6dias, na.rm=T))/(sum(obitos_fetais_mais_22sem, na.rm=T) + sum(nascidos, na.rm=T) )*1000, 1)", "8.7"),
        taxa_perinatal_total_menos1000 = rep("round((sum(fetal_peso_menos_1000, na.rm=T) + sum(obitos_6dias_menos1000, na.rm=T))/(sum(fetal_peso_menos_1000, na.rm=T)+ sum(nascidos_menos1000, na.rm=T))*1000, 1)", 2),
        taxa_perinatal_total_1000_1499 = rep("round((sum(fetal_peso_1000_1499, na.rm=T) + sum(obitos_6dias_1000_1499, na.rm=T))/(sum(fetal_peso_1000_1499, na.rm=T)+sum(nascidos_1000_1499, na.rm=T))*1000, 1)", 2),
        taxa_perinatal_total_1500_2499 = rep("round((sum(fetal_peso_1500_2499, na.rm=T)+sum(obitos_6dias_1500_2499, na.rm=T))/(sum(fetal_peso_1500_2499, na.rm=T)+sum(nascidos_1500_2499, na.rm=T))*1000, 1)", 2),
        taxa_perinatal_total_mais2500 = rep("round((sum(fetal_peso_mais_2500, na.rm=T)+sum(obitos_6dias_mais2500, na.rm=T))/(sum(fetal_peso_mais_2500, na.rm=T)+sum(nascidos_mais2500, na.rm=T))*1000, 1)", 2),
        # taxa_perinatal_oms = c("round((sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias))/(sum(obitos_fetais_mais_28sem, na.rm=T) + sum(nascidos) )*1000, 1)", "8.7"),
        # taxa_perinatal_oms_menos1000 = rep("round((sum(peso_menos_1000_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1000))/(sum(peso_menos_1000_mais_28sem, na.rm=T)+ sum(nascidos_menos1000))*1000, 1)", 2),
        # taxa_perinatal_oms_1000_1499 = rep("round((sum(peso_1000_1499_mais_28sem, na.rm=T) + sum(obitos_6dias_1000_1499))/(sum(peso_1000_1499_mais_28sem, na.rm=T)+sum(nascidos_1000_1499))*1000, 1)", 2),
        # taxa_perinatal_oms_1500_2499 = rep("round((sum(peso_1500_2499_mais_28sem, na.rm=T)+sum(obitos_6dias_1500_2499))/(sum(peso_1500_2499_mais_28sem, na.rm=T)+sum(nascidos_1500_2499))*1000, 1)", 2),
        # taxa_perinatal_oms_mais2500 = rep("round((sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(obitos_6dias_mais2500))/(sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(nascidos_mais2500))*1000, 1)", 2),

        obitos_0dias = rep("sum(obitos_0dias, na.rm=T)", 2),
        obitos_0dias_menos1000 = rep("sum(obitos_0dias_menos1000, na.rm=T)", 2),
        obitos_0dias_1000_1499 = rep("sum(obitos_0dias_1000_1499, na.rm=T)", 2),
        obitos_0dias_1500_2499 = rep("sum(obitos_0dias_1500_2499, na.rm=T)", 2),
        obitos_0dias_mais2500 = rep("sum(obitos_0dias_mais2500, na.rm=T)", 2),
        obitos_1_6dias = rep("sum(obitos_1_6dias, na.rm=T)", 2),
        obitos_1_6dias_menos1000 = rep("sum(obitos_1_6dias_menos1000, na.rm=T)", 2),
        obitos_1_6dias_1000_1499 = rep("sum(obitos_1_6dias_1000_1499, na.rm=T)", 2),
        obitos_1_6dias_1500_2499 = rep("sum(obitos_1_6dias_1500_2499, na.rm=T)", 2),
        obitos_1_6dias_mais2500 = rep("sum(obitos_1_6dias_mais2500, na.rm=T)", 2),
        obitos_6dias = rep("sum(obitos_6dias, na.rm=T)", 2),
        obitos_6dias_menos1000 = rep("sum(obitos_6dias_menos1000, na.rm=T)", 2),
        obitos_6dias_1000_1499 = rep("sum(obitos_6dias_1000_1499, na.rm=T)", 2),
        obitos_6dias_1500_2499 = rep("sum(obitos_6dias_1500_2499, na.rm=T)", 2),
        obitos_6dias_mais2500 = rep("sum(obitos_6dias_mais2500, na.rm=T)", 2),
        obitos_27dias = rep("sum(obitos_27dias, na.rm=T)", 2),
        obitos_27dias_menos1000 = rep("sum(obitos_27dias_menos1000, na.rm=T)", 2),
        obitos_27dias_1000_1499 = rep("sum(obitos_27dias_1000_1499, na.rm=T)", 2),
        obitos_27dias_1500_2499 = rep("sum(obitos_27dias_1500_2499, na.rm=T)", 2),
        obitos_27dias_mais2500 = rep("sum(obitos_27dias_mais2500, na.rm=T)", 2),
        obitos_7_27dias = rep("sum(obitos_7_27dias, na.rm=T)", 2),
        obitos_7_27dias_menos1000 = rep("sum(obitos_7_27dias_menos1000, na.rm=T)", 2),
        obitos_7_27dias_1000_1499 = rep("sum(obitos_7_27dias_1000_1499, na.rm=T)", 2),
        obitos_7_27dias_1500_2499 = rep("sum(obitos_7_27dias_1500_2499, na.rm=T)", 2),
        obitos_7_27dias_mais2500 = rep("sum(obitos_7_27dias_mais2500, na.rm=T)", 2),
        porc_condicoes_ameacadoras = rep("round(sum(nascidos_condicoes_ameacadoras, na.rm=T) / sum(nascidos, na.rm=T) * 100, 1)", 2),
        #porc_internacoes_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)", 2),
        porc_internacoes_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral, na.rm=T) / sum(nascidos_estabelecimentos_publicos_sih, na.rm=T) * 100, 1)", 2),
        porc_internacoes_uti_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral_internado_uti, na.rm=T) / sum(nascidos_estabelecimentos_publicos_sih, na.rm=T) * 100, 1)", 2)
      )

       if (is.null(input$idade_dias_sih[1])) {
         df_calcs_aux3 <- data.frame(
           tipo = c("local", "referencia"),
           porc_internacoes_menores_28_dias_sih = "NA",
           percentil_porc_internacoes_menores_28_dias_sih = "NA"

         )
       } else {
         df_calcs_aux3 <- data.frame(
           tipo = c("local", "referencia"),
           porc_internacoes_menores_28_dias_sih = rep(glue::glue(
             "ifelse(
               length(input$idade_dias_sih) == 3,
               round(sum(internacoes_{input$local_internacao_sih}_geral) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
               ifelse(
               length(input$idade_dias_sih) == 2,
               round((sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_sih[1]}) + sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_sih[2]})) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
               round(sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_sih[1]}) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)

               )
             )"
            ), 2)
         )
       }

       if (is.null(input$idade_dias_uti_sih[1])) {
         df_calcs_aux4 <- data.frame(
           tipo = c("local", "referencia"),
           porc_internacoes_uti_menores_28_dias_sih = "NA",
           percentil_porc_internacoes_uti_menores_28_dias_sih = "NA"

         )
       } else {
         df_calcs_aux4 <- data.frame(
           tipo = c("local", "referencia"),
           porc_internacoes_uti_menores_28_dias_sih = rep(glue::glue(
              "ifelse(
                length(input$idade_dias_uti_sih) == 3,
                round(sum(internacoes_{input$local_internacao_uti_sih}_geral_internado_uti) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
                ifelse(
                length(input$idade_dias_uti_sih) == 2,
                round((sum(internacoes_{input$local_internacao_uti_sih}_{input$idade_dias_uti_sih[1]}_internado_uti) + sum(internacoes_{input$local_internacao_uti_sih}_{input$idade_dias_uti_sih[2]}_internado_uti)) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
                round(sum(internacoes_{input$local_internacao_uti_sih}_{input$idade_dias_uti_sih[1]}_internado_uti) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
                )
              )"
            ), 2)
         )
       }

       dplyr::full_join(
         df_calcs_aux1,
         dplyr::full_join(df_calcs_aux3, df_calcs_aux4)
       )

    })

    bloco7_calcs_dist <- data.frame(
      tipo = c("local", "referencia"),
      antes_dist_moment_obito_fetal = rep("round(
        sum(c(fetal_antes_peso_menos_1000, fetal_antes_peso_1000_1499, fetal_antes_peso_1500_2499, fetal_antes_peso_mais_2500, fetal_antes)[seleciona(aba = 'fetal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_fetal) %in% input$faixa_peso_dist_moment_obit_fetal], na.rm=T)/
          sum(c(fetal_peso_menos_1000, fetal_peso_1000_1499, fetal_peso_1500_2499, fetal_peso_mais_2500, obitos_fetais)[seleciona(aba = 'fetal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_fetal) %in% input$faixa_peso_dist_moment_obit_fetal], na.rm=T)
        *100, 1)", 2),

      durante_dist_moment_obito_fetal = rep("round(
        sum(c(fetal_durante_peso_menos_1000, fetal_durante_peso_1000_1499, fetal_durante_peso_1500_2499, fetal_durante_peso_mais_2500, fetal_durante)[seleciona(aba = 'fetal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_fetal) %in% input$faixa_peso_dist_moment_obit_fetal], na.rm=T)/
          sum(c(fetal_peso_menos_1000, fetal_peso_1000_1499, fetal_peso_1500_2499, fetal_peso_mais_2500, obitos_fetais)[seleciona(aba = 'fetal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_fetal) %in% input$faixa_peso_dist_moment_obit_fetal], na.rm=T)
        *100, 1)", 2),

      faltante_dist_moment_obito_fetal = rep("round(100-antes_dist_moment_obito_fetal-durante_dist_moment_obito_fetal, 1)", 2),

      antes_dist_moment_obito_perinat = rep("round(
        sum(c(perinatal_todos_antes_menos_1000, perinatal_todos_antes_1000_1499, perinatal_todos_antes_1500_2499, perinatal_todos_antes_mais_2500, perinatal_todos_antes)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm=T)/
          sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm=T)
        *100, 1)", 2),

      durante_dist_moment_obito_perinat = rep("round(
        sum(c(perinatal_todos_durante_menos_1000, perinatal_todos_durante_1000_1499, perinatal_todos_durante_1500_2499, perinatal_todos_durante_mais_2500, perinatal_todos_durante)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm=T)/
          sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm=T)
        *100, 1)", 2),

      dia_0_dist_moment_obito_perinat = rep("round(
        sum(c(obitos_0dias_menos1000, obitos_0dias_1000_1499, obitos_0dias_1500_2499, obitos_0dias_mais2500, obitos_0dias)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm=T)/
          sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm=T)
        *100, 1)", 2),

      dia_1_6_dist_moment_obito_perinat = rep("round(
        sum(c(obitos_1_6dias_menos1000, obitos_1_6dias_1000_1499, obitos_1_6dias_1500_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm=T)/
          sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm=T)
        *100, 1)", 2),

      faltante_dist_moment_obito_perinat = rep("round(100 -antes_dist_moment_obito_perinat -durante_dist_moment_obito_perinat -dia_0_dist_moment_obito_perinat -dia_1_6_dist_moment_obito_perinat, 1)", 2),

      dia_0_dist_moment_obito_neonat = rep("round(
        sum(c(obitos_0dias_menos1000, obitos_0dias_1000_1499, obitos_0dias_1500_2499, obitos_0dias_mais2500, obitos_0dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat], na.rm=T)/
          sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat], na.rm=T)
        *100, 1)", 2),

      dia_1_6dist_moment_obito_neonat = rep("round(
        sum(c(obitos_1_6dias_menos1000, obitos_1_6dias_1000_1499, obitos_1_6dias_1500_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat], na.rm=T)/
          sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat], na.rm=T)
        *100, 1)", 2),

      dia_7_27dist_moment_obito_neonat = rep("round(
        sum(c(obitos_7_27dias_menos1000, obitos_7_27dias_1000_1499, obitos_7_27dias_1500_2499, obitos_7_27dias_mais2500, obitos_7_27dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat], na.rm=T)/
          sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat], na.rm=T)
        *100, 1)", 2),

      faltante_moment_obito_neonat = rep("round(100 -dia_0_dist_moment_obito_neonat -dia_1_6dist_moment_obito_neonat -dia_7_27dist_moment_obito_neonat, 1)", 2),

      menos_1000_dist_peso_fetal = rep("round(
        sum(c(fetal_antes_peso_menos_1000, fetal_durante_peso_menos_1000, fetal_peso_menos_1000)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal], na.rm=T)/
          sum(c(fetal_antes, fetal_durante, obitos_fetais)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal], na.rm=T)
        *100, 1)", 2),

      de_1000_1499_dist_peso_fetal = rep("round(
        sum(c(fetal_antes_peso_1000_1499, fetal_durante_peso_1000_1499, fetal_peso_1000_1499)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal], na.rm=T)/
          sum(c(fetal_antes, fetal_durante, obitos_fetais)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal], na.rm=T)
        *100, 1)", 2),

      de_1500_2499_dist_peso_fetal = rep("round(
        sum(c(fetal_antes_peso_1500_2499, fetal_durante_peso_1500_2499, fetal_peso_1500_2499)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal], na.rm=T)/
          sum(c(fetal_antes, fetal_durante, obitos_fetais)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal], na.rm=T)
        *100, 1)", 2),

      mais_2500_dist_peso_fetal = rep("round(
        sum(c(fetal_antes_peso_mais_2500, fetal_durante_peso_mais_2500, fetal_peso_mais_2500)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal], na.rm=T)/
          sum(c(fetal_antes, fetal_durante, obitos_fetais)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal], na.rm=T)
        *100, 1)", 2),

      faltante_dist_peso_fetal = rep("round(100 -menos_1000_dist_peso_fetal-de_1000_1499_dist_peso_fetal-de_1500_2499_dist_peso_fetal -mais_2500_dist_peso_fetal, 1)", 2),

      menos_1000_dist_peso_perinat = rep("round(
        sum(c(perinatal_todos_antes_menos_1000, perinatal_todos_durante_menos_1000, obitos_0dias_menos1000, obitos_1_6dias_menos1000, perinatal_todos_peso_menos_1000)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat], na.rm=T)/
          sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat], na.rm=T)
        *100, 1)", 2),

      de_1000_1499_dist_peso_perinat = rep("round(
        sum(c(perinatal_todos_antes_1000_1499, perinatal_todos_durante_1000_1499, obitos_0dias_1000_1499, obitos_1_6dias_1000_1499, perinatal_todos_peso_1000_1499)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat], na.rm=T)/
          sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat], na.rm=T)
        *100, 1)", 2),


      de_1500_2499_dist_peso_perinat = rep("round(
        sum(c(perinatal_todos_antes_1500_2499, perinatal_todos_durante_1500_2499, obitos_0dias_1500_2499, obitos_1_6dias_1500_2499, perinatal_todos_peso_1500_2499)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat], na.rm=T)/
          sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat], na.rm=T)
        *100, 1)", 2),

      mais_2500_dist_peso_perinat = rep("round(
        sum(c(perinatal_todos_antes_mais_2500 , perinatal_todos_durante_mais_2500, obitos_0dias_mais2500, obitos_1_6dias_mais2500, perinatal_todos_peso_mais_2500)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat], na.rm=T)/
          sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat], na.rm=T)
        *100, 1)", 2),

      faltante_dist_peso_perinat = rep("round(100 -menos_1000_dist_peso_perinat -de_1000_1499_dist_peso_perinat -de_1500_2499_dist_peso_perinat -mais_2500_dist_peso_perinat, 1)", 2),


      menos_1000_dist_peso_neonat = rep("round(
        sum(c(obitos_0dias_menos1000, obitos_1_6dias_menos1000, obitos_7_27dias_menos1000, obitos_27dias_menos1000)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat], na.rm=T)/
          sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat], na.rm=T)
        *100, 1)", 2),

      de_1000_1499_dist_peso_neonat = rep("round(
        sum(c(obitos_0dias_1000_1499, obitos_1_6dias_1000_1499, obitos_7_27dias_1000_1499, obitos_27dias_1000_1499)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat], na.rm=T)/
          sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat], na.rm=T)
        *100, 1)", 2),

      de_1500_2499_dist_peso_neonat = rep("round(
        sum(c(obitos_0dias_1500_2499, obitos_1_6dias_1500_2499, obitos_7_27dias_1500_2499, obitos_27dias_1500_2499)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat], na.rm=T)/
          sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat], na.rm=T)
        *100, 1)", 2),

      mais_2500_dist_peso_neonat = rep("round(
        sum(c(obitos_0dias_mais2500, obitos_1_6dias_mais2500, obitos_7_27dias_mais2500, obitos_27dias_mais2500)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat], na.rm=T)/
          sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat], na.rm=T)
        *100, 1)", 2),

      faltante_dist_peso_neonat = rep("round(100 -menos_1000_dist_peso_neonat -de_1000_1499_dist_peso_neonat -de_1500_2499_dist_peso_neonat -mais_2500_dist_peso_neonat, 1)", 2)
    )

    # data7_percentil <- reactive({
    #   bloco7 |>
    #     dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
    #     dplyr::group_by(ano, codmunres) |>
    #     dplyr::summarise(
    #       taxa_mort_fetal = round(sum(obitos_fetais_mais_22sem)/(sum(nascidos)+sum(obitos_fetais_mais_22sem)) *1000, 1),
    #       taxa_mort_fetal_peso_menos_1000 =  round(sum(fetal_peso_menos_1000)/(sum(nascidos_menos1000)+sum(fetal_peso_menos_1000)) *1000, 1) ,
    #       taxa_mort_fetal_peso_1000_1499 =  round(sum(fetal_peso_1000_1499)/(sum(nascidos_1000_1499)+sum(fetal_peso_1000_1499)) *1000, 1) ,
    #       taxa_mort_fetal_peso_1500_2499 =  round(sum(fetal_peso_1500_2499)/(sum(nascidos_1500_2499)+sum(fetal_peso_1500_2499)) *1000, 1) ,
    #       taxa_mort_fetal_peso_mais_2500 =  round(sum(fetal_peso_mais_2500)/(sum(nascidos_mais2500)+sum(fetal_peso_mais_2500)) *1000, 1) ,
    #       taxa_mort_fetal_antes =  round(sum(fetal_antes)/(sum(nascidos) + sum(fetal_antes)) *1000, 1) ,
    #       taxa_mort_fetal_durante =  round(sum(fetal_durante)/(sum(nascidos) + sum(fetal_durante)) *1000, 1) ,
    #       taxa_mort_fetal_depois =  round(sum(fetal_depois)/(sum(nascidos) + sum(fetal_depois)) *1000, 1) ,
    #       taxa_mort_fetal_antes_peso_menos_1000 =  round(sum(fetal_antes_peso_menos_1000)/(sum(nascidos_menos1000)+sum(fetal_antes_peso_menos_1000)) *1000, 1) ,
    #       taxa_mort_fetal_antes_peso_1000_1499 =  round(sum(fetal_antes_peso_1000_1499)/(sum(nascidos_1000_1499)+sum(fetal_antes_peso_1000_1499)) *1000, 1) ,
    #       taxa_mort_fetal_antes_peso_1500_2499 =  round(sum(fetal_antes_peso_1500_2499)/(sum(nascidos_1500_2499)+sum(fetal_antes_peso_1500_2499)) *1000, 1) ,
    #       taxa_mort_fetal_antes_peso_mais_2500 =  round(sum(fetal_antes_peso_mais_2500)/(sum(nascidos_mais2500)+sum(fetal_antes_peso_mais_2500)) *1000, 1) ,
    #       taxa_mort_fetal_durante_peso_menos_1000 =  round(sum(fetal_durante_peso_menos_1000)/(sum(nascidos_menos1000)+sum(fetal_durante_peso_menos_1000)) *1000, 1) ,
    #       taxa_mort_fetal_durante_peso_1000_1499 =  round(sum(fetal_durante_peso_1000_1499)/(sum(nascidos_1000_1499)+sum(fetal_durante_peso_1000_1499)) *1000, 1) ,
    #       taxa_mort_fetal_durante_peso_1500_2499 =  round(sum(fetal_durante_peso_1500_2499)/(sum(nascidos_1500_2499)+sum(fetal_durante_peso_1500_2499)) *1000, 1) ,
    #       taxa_mort_fetal_durante_peso_mais_2500 =  round(sum(fetal_durante_peso_mais_2500)/(sum(nascidos_mais2500)+sum(fetal_durante_peso_mais_2500)) *1000, 1) ,
    #       taxa_mort_fetal_depois_peso_menos_1000 =  round(sum(fetal_depois_peso_menos_1000)/(sum(nascidos_menos1000)+sum(fetal_depois_peso_menos_1000)) *1000, 1) ,
    #       taxa_mort_fetal_depois_peso_1000_1499 =  round(sum(fetal_depois_peso_1000_1499)/(sum(nascidos_1000_1499)+sum(fetal_depois_peso_1000_1499)) *1000, 1) ,
    #       taxa_mort_fetal_depois_peso_1500_2499 =  round(sum(fetal_depois_peso_1500_2499)/(sum(nascidos_1500_2499)+sum(fetal_depois_peso_1500_2499)) *1000, 1) ,
    #       taxa_mort_fetal_depois_peso_mais_2500 =  round(sum(fetal_depois_peso_mais_2500)/(sum(nascidos_mais2500)+sum(fetal_depois_peso_mais_2500)) *1000, 1) ,
    #
    #       taxa_mort_fetal_oms =  round(sum(obitos_fetais_mais_28sem)/(sum(nascidos)+sum(obitos_fetais_mais_28sem)) *1000, 1) ,
    #       taxa_mort_fetal_oms_peso_menos_1000 =  round(sum(peso_menos_1000_mais_28sem)/(sum(nascidos_menos1000)+sum(peso_menos_1000_mais_28sem))*1000, 1) ,
    #       taxa_mort_fetal_oms_peso_1000_1499 =  round(sum(peso_1000_1499_mais_28sem)/(sum(nascidos_1000_1499)+sum(peso_1000_1499_mais_28sem))*1000, 1) ,
    #       taxa_mort_fetal_oms_peso_1500_2499 =  round(sum(peso_1500_2499_mais_28sem)/(sum(nascidos_1500_2499)+sum(peso_1500_2499_mais_28sem))*1000, 1) ,
    #       taxa_mort_fetal_oms_peso_mais_2500 =  round(sum(peso_mais_2500_mais_28sem)/(sum(nascidos_mais2500)+sum(peso_mais_2500_mais_28sem))*1000, 1) ,
    #       taxa_mort_fetal_oms_antes =  round(sum(perinatal_antes)/(sum(nascidos)+sum(perinatal_antes)) *1000, 1) ,
    #       taxa_mort_fetal_oms_durante =  round(sum(perinatal_durante)/(sum(nascidos)+sum(perinatal_durante))*1000, 1) ,
    #       taxa_mort_fetal_oms_antes_peso_menos_1000 =  round(sum(perinatal_antes_peso_menos_1000)/(sum(nascidos_menos1000)+sum(perinatal_antes_peso_menos_1000))*1000, 1) ,
    #       taxa_mort_fetal_oms_antes_peso_1000_1499 =  round(sum(perinatal_antes_peso_1000_1499)/(sum(nascidos_1000_1499)+sum(perinatal_antes_peso_1000_1499))*1000, 1) ,
    #       taxa_mort_fetal_oms_antes_peso_1500_2499 =  round(sum(perinatal_antes_peso_1500_2499)/(sum(nascidos_1500_2499)+sum(perinatal_antes_peso_1500_2499))*1000, 1) ,
    #       taxa_mort_fetal_oms_antes_peso_mais_2500 =  round(sum(perinatal_antes_peso_mais_2500)/(sum(nascidos_mais2500)+sum(perinatal_antes_peso_mais_2500))*1000, 1) ,
    #       taxa_mort_fetal_oms_durante_peso_menos_1000 =  round(sum(perinatal_durante_peso_menos_1000)/(sum(nascidos_menos1000)+sum(perinatal_durante_peso_menos_1000))*1000, 1) ,
    #       taxa_mort_fetal_oms_durante_peso_1000_1499 =  round(sum(perinatal_durante_peso_1000_1499)/(sum(nascidos_1000_1499)+sum(perinatal_durante_peso_1000_1499))*1000, 1) ,
    #       taxa_mort_fetal_oms_durante_peso_1500_2499 =  round(sum(perinatal_durante_peso_1500_2499)/(sum(nascidos_1500_2499)+sum(perinatal_durante_peso_1500_2499))*1000, 1) ,
    #       taxa_mort_fetal_oms_durante_peso_mais_2500 =  round(sum(perinatal_durante_peso_mais_2500)/(sum(nascidos_mais2500)+sum(perinatal_durante_peso_mais_2500))*1000, 1) ,
    #
    #       taxa_perinatal_total = round((sum(obitos_fetais_mais_22sem) + sum(obitos_6dias))/(sum(obitos_fetais_mais_22sem) + sum(nascidos) )*1000, 1),
    #       taxa_perinatal_total_menos1000 = round((sum(fetal_peso_menos_1000) + sum(obitos_6dias_menos1000))/(sum(fetal_peso_menos_1000)+ sum(nascidos_menos1000))*1000, 1),
    #       taxa_perinatal_total_1000_1499 = round((sum(fetal_peso_1000_1499) + sum(obitos_6dias_1000_1499))/(sum(fetal_peso_1000_1499)+sum(nascidos_1000_1499))*1000, 1),
    #       taxa_perinatal_total_1500_2499 = round((sum(fetal_peso_1500_2499)+sum(obitos_6dias_1500_2499))/(sum(fetal_peso_1500_2499)+sum(nascidos_1500_2499))*1000, 1),
    #       taxa_perinatal_total_mais2500 = round((sum(fetal_peso_mais_2500)+sum(obitos_6dias_mais2500))/(sum(fetal_peso_mais_2500)+sum(nascidos_mais2500))*1000, 1),
    #       taxa_perinatal_oms = round((sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias))/(sum(obitos_fetais_mais_28sem, na.rm=T) + sum(nascidos) )*1000, 1),
    #       taxa_perinatal_oms_menos1000 = round((sum(peso_menos_1000_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1000))/(sum(peso_menos_1000_mais_28sem, na.rm=T)+ sum(nascidos_menos1000))*1000, 1),
    #       taxa_perinatal_oms_1000_1499 = round((sum(peso_1000_1499_mais_28sem, na.rm=T) + sum(obitos_6dias_1000_1499))/(sum(peso_1000_1499_mais_28sem, na.rm=T)+sum(nascidos_1000_1499))*1000, 1),
    #       taxa_perinatal_oms_1500_2499 = round((sum(peso_1500_2499_mais_28sem, na.rm=T)+sum(obitos_6dias_1500_2499))/(sum(peso_1500_2499_mais_28sem, na.rm=T)+sum(nascidos_1500_2499))*1000, 1),
    #       taxa_perinatal_oms_mais2500 = round((sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(obitos_6dias_mais2500))/(sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(nascidos_mais2500))*1000, 1),
    #
    #       mort_neonat = round(sum(obitos_27dias)/sum(nascidos) *1000, 1),
    #       mort_neonat_precoc = round(sum(obitos_6dias)/sum(nascidos) *1000, 1),
    #       mort_neonat_tardia = round(sum(obitos_7_27dias)/sum(nascidos) *1000, 1),
    #       mort_neonat_menos1000 = round(sum(obitos_27dias_menos1000)/sum(nascidos_menos1000) *1000, 1),
    #       mort_neonat_precoc_menos1000 = round(sum(obitos_6dias_menos1000)/sum(nascidos_menos1000) *1000, 1),
    #       mort_neonat_tardia_menos1000 = round(sum(obitos_7_27dias_menos1000)/sum(nascidos_menos1000) *1000, 1),
    #       mort_neonat_1000_1499 = round(sum(obitos_27dias_1000_1499)/sum(nascidos_1000_1499) *1000, 1),
    #       mort_neonat_precoc_1000_1499 = round(sum(obitos_6dias_1000_1499)/sum(nascidos_1000_1499) *1000, 1),
    #       mort_neonat_tardia_1000_1499 = round(sum(obitos_7_27dias_1000_1499)/sum(nascidos_1000_1499) *1000, 1),
    #       mort_neonat_1500_2499 = round(sum(obitos_27dias_1500_2499)/sum(nascidos_1500_2499) *1000, 1),
    #       mort_neonat_precoc_1500_2499 = round(sum(obitos_6dias_1500_2499)/sum(nascidos_1500_2499) *1000, 1),
    #       mort_neonat_tardia_1500_2499 = round(sum(obitos_7_27dias_1500_2499)/sum(nascidos_1500_2499) *1000, 1),
    #       mort_neonat_mais2500 = round(sum(obitos_27dias_mais2500)/sum(nascidos_mais2500) *1000, 1),
    #       mort_neonat_precoc_mais2500 = round(sum(obitos_6dias_mais2500)/sum(nascidos_mais2500) *1000, 1),
    #       mort_neonat_tardia_mais2500 = round(sum(obitos_7_27dias_mais2500)/sum(nascidos_mais2500) *1000, 1),
    #       porc_condicoes_ameacadoras = round(sum(nascidos_condicoes_ameacadoras) / sum(nascidos) * 100, 1),
    #
    #       porc_internacoes_menores_28_dias_sih = (if(length(input$idade_dias_sih) == 3){
    #         round(
    #           sum(get(paste0("internacoes_", input$local_internacao_sih, "_geral"))) /
    #             sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
    #       } else if(length(input$idade_dias_sih) == 2){
    #         round(
    #           (sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_sih[1]))) +
    #              sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_sih[2])))) /
    #             sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
    #       } else if(length(input$idade_dias_sih) == 1){
    #         round(
    #           sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_sih[1]))) /
    #             sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
    #       }),
    #
    #       # porc_internacoes_menores_28_dias_sih = ifelse(
    #       #   length(input$idade_dias_sih) == 3,
    #       #   round(
    #       #     sum(get(paste0("internacoes_", input$local_internacao_sih, "_geral"))) /
    #       #       sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
    #       #
    #       #   ifelse(
    #       #     length(input$idade_dias_sih) == 2,
    #       #     round(
    #       #       (sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_sih[1]))) +
    #       #          sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_sih[2])))) /
    #       #         sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
    #       #
    #       #     round(
    #       #       sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_sih[1]))) /
    #       #         sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
    #       #   )
    #       # ),
    #
    #       porc_internacoes_uti_menores_28_dias_sih = (if(length(input$idade_dias_uti_sih) == 3){
    #         round(
    #               sum(get(paste0("internacoes_", input$local_internacao_uti_sih, "_geral_internado_uti"))) /
    #               sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
    #       } else if(length(input$idade_dias_uti_sih) == 2){
    #         round(
    #                 (sum(get(paste0("internacoes_", input$local_internacao_uti_sih, "_", input$idade_dias_uti_sih[1], "_internado_uti"))) +
    #                     sum(get(paste0("internacoes_", input$local_internacao_uti_sih, "_", input$idade_dias_uti_sih[2], "_internado_uti")))) /
    #                    sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
    #       } else if(length(input$idade_dias_uti_sih) == 1){
    #         round(
    #                sum(get(paste0("internacoes_", input$local_internacao_uti_sih, "_", input$idade_dias_uti_sih[1], "_internado_uti"))) /
    #                  sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
    #       })
    #
    #       # porc_internacoes_uti_menores_28_dias_sih = ifelse(
    #       #   length(input$idade_dias_sih) == 3,
    #       #   round(
    #       #     sum(get(paste0("internacoes_", input$local_internacao_sih, "_geral_internado_uti"))) /
    #       #       sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
    #       #
    #       #   ifelse(
    #       #     length(input$idade_dias_sih) == 2,
    #       #     round(
    #       #       (sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_uti_sih[1], "_internado_uti"))) +
    #       #          sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_uti_sih[2], "_internado_uti")))) /
    #       #         sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
    #       #
    #       #     round(
    #       #       sum(get(paste0("internacoes_", input$local_internacao_sih, "_", input$idade_dias_uti_sih[1], "_internado_uti"))) /
    #       #         sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
    #       #   )
    #       # )
    #
    #     )|>
    #     dplyr::ungroup()|>
    #     dplyr::group_by(ano)|>
    #     dplyr::summarise(dplyr::across(dplyr::everything(), list(
    #       percentil5 = ~ round(quantile(.x, probs = 0.05, na.rm = TRUE), 1),
    #       percentil95 = ~ round(quantile(.x, probs = 0.95, na.rm = TRUE), 1)
    #     ), .names = "{fn}_{col}"))|>
    #     dplyr::mutate(
    #       class = "Percentil",
    #       localidade_comparacao = "Percentil"
    #     )
    #
    # })

    # Criando alguns outputs para a UI ----------------------------------------
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


    ## Criando os outputs que receberão os nomes dos locais selecionados quando há comparação --------
    output$input_localidade_resumo_fetal <- renderUI({
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
          inputId = ns("localidade_resumo_fetal"),
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

    output$input_localidade_resumo_neonat <- renderUI({
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
          inputId = ns("localidade_resumo_neonat"),
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

    output$input_localidade_resumo_perinatal <- renderUI({
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
          inputId = ns("localidade_resumo_perinatal"),
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


    output$input_localidade_resumo_morbidade_neonatal <- renderUI({
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
          inputId = ns("localidade_resumo_morbidade_neonatal"),
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
        if (input$tabset1 == "tabpanel_neonatal") {
          req(input$localidade_resumo_neonat)
          if (input$localidade_resumo_neonat == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        } else if (input$tabset1 == "tabpanel_fetal"){
          req(input$localidade_resumo_fetal)
          if (input$localidade_resumo_fetal == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        } else if (input$tabset1 == "tabpanel_perinatal"){
          req(input$localidade_resumo_perinatal)
          if (input$localidade_resumo_perinatal == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        } else {
          req(input$localidade_resumo_morbidade_neonatal)
          if (input$localidade_resumo_morbidade_neonatal == "escolha1") {
            filtros()$nivel
          } else {
            filtros()$nivel2
          }
        }
      }
    })


    ## Para os botões de informação em alguns indicadores ----------------------
    ### Mostrando o botão com a informação sobre a comparação nos gráficos de distribuição proporcional
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao_comparacao_fetal1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_comparacao_fetal2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_comparacao_perinatal1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_comparacao_perinatal2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_comparacao_neonatal1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::hide(id = "mostrar_botao_comparacao_neonatal2", anim = TRUE, animType = "fade", time = 0.8)
      req(filtros()$comparar == "Sim")
      shinyjs::show(id = "mostrar_botao_comparacao_fetal1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_comparacao_fetal2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_comparacao_perinatal1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_comparacao_perinatal2", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_comparacao_neonatal1", anim = TRUE, animType = "fade", time = 0.8)
      shinyjs::show(id = "mostrar_botao_comparacao_neonatal2", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    ### Criando o pop-up com a informação sobre a comparação nos gráficos de distribuição proporcional
    observeEvent(
      c(input$botao_comparacao_fetal1, input$botao_comparacao_fetal2,
        input$botao_comparacao_perinatal1, input$botao_comparacao_perinatal2,
        input$botao_comparacao_neonatal1, input$botao_comparacao_perinatal2
      ),
      {
        shinyalert::shinyalert(
          html = TRUE,
          title = "<div class = 'fonte-titulos-modal'> Sobre a comparação </div>",
          text = glue::glue(
            "<div style = 'text-align: justify; text-justify: inter-word;'>
             Para visualizar os valores referentes à localidade de comparação selecionada, passe o cursor do mouse sobre a barra que contém a categoria de interesse.
           </div>"
          ),
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          type = "info",
          showConfirmButton = TRUE,
          confirmButtonText = "OK",
          confirmButtonCol = "#007bff",
          animation = TRUE
        )
      },
      ignoreInit = TRUE
    )

    ## Criando o pop-up com a informação sobre o resumo do período -------------
    observeEvent(c(input$botao_resumo1, input$botao_resumo2, input$botao_resumo3), {
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

    observeEvent(c(input$botao_resumo4), {
      shinyalert::shinyalert(
        html = TRUE,
        title = '<div class = "fonte-titulos-modal" style = "color: #656565"> Sobre o "Resumo do período" </div>',
        text = '
          <div style = "text-align: justify; text-justify: inter-word;">
            Todas as caixinhas que estão sob o "Resumo do período", na esquerda da página, referem-se aos valores dos indicadores calculados considerando todo o período selecionado.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando alguma comparação é feita, o usuário pode selecionar para qual localidade o resumo do período será calculado clicando em um dos botões que irão aparecer em cima das caixinhas.
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

    ### Criando o pop-up com a informação sobre o cálculo do indicador de condições ameaçadoras
    observeEvent(input$botao_explicacao_indicador, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal'> Sobre este indicador </div>",
        text = glue::glue(
          "<div style = 'text-align: justify; text-justify: inter-word;'>
          Foram considerados nascidos com condições potencialmente ameaçadoras à vida aqueles com peso < 1500 g, com idade gestacional < 32 semanas ou com Apgar de quinto minuto < 7.
         </div>"
        ),
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE
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


    ### Ativando os botões de alerta quando necessário ------------------------
    #### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao_morbidade1", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$condicoes_ameacadoras > 5, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao_morbidade1", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao_morbidade1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$condicoes_ameacadoras,
        variavel_incompletude1 = "PESO, GESTACAO, SEMAGESTAC e APGAR5",
        descricao_incompletude1 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })


    # Para o resumo do período ------------------------------------------------
    ## Calculando uma média dos indicadores para o período selecionado --------
    ### Para a localidade selecionada -----------------------------------------
    data7_resumo_aux <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo_inputs <- ""
      } else {
        if (input$tabset1 == "tabpanel_neonatal") {
          req(input$localidade_resumo_neonat)
          if (input$localidade_resumo_neonat == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else if (input$tabset1 == "tabpanel_fetal") {
          req(input$localidade_resumo_fetal)
          if (input$localidade_resumo_fetal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else if (input$tabset1 == "tabpanel_perinatal"){
          req(input$localidade_resumo_perinatal)
          if (input$localidade_resumo_perinatal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else {
          req(input$localidade_resumo_morbidade_neonatal)
          if (input$localidade_resumo_morbidade_neonatal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        }
      }
      bloco7 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (nivel_selecionado() == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (nivel_selecionado() == "regional")
            regiao == filtros()[[paste0("regiao", sufixo_inputs)]]
          else if (nivel_selecionado() == "estadual")
            uf == filtros()[[paste0("estado", sufixo_inputs)]]
          else if (nivel_selecionado() == "macro")
            macro_r_saude == filtros()[[paste0("macro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_macro", sufixo_inputs)]]
          else if(nivel_selecionado() == "micro")
            r_saude == filtros()[[paste0("micro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_micro", sufixo_inputs)]]
          else if(nivel_selecionado() == "municipal")
            municipio == filtros()[[paste0("municipio", sufixo_inputs)]] & uf == filtros()[[paste0("estado_municipio", sufixo_inputs)]]
          else if (nivel_selecionado() == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        cria_indicadores(df_calcs = bloco7_calcs(), df_calcs_dist_bloco7 = bloco7_calcs_dist, bloco  = "bloco7", input = input, filtros = filtros(), adicionar_localidade = FALSE) |>
        dplyr::mutate(
          localidade = dplyr::case_when(
            nivel_selecionado() == "nacional" ~ "Brasil",
            nivel_selecionado() == "regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
            nivel_selecionado() == "estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
            nivel_selecionado() == "macro" ~ filtros()[[paste0("macro", sufixo_inputs)]],
            nivel_selecionado() == "micro" ~ filtros()[[paste0("micro", sufixo_inputs)]],
            nivel_selecionado() == "municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]],
            nivel_selecionado() == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        )
    })

    # Não queremos que as caixinhas se atualizem quando os inputs dos gráficos de distribuição percentual mudarem
    data7_resumo <- eventReactive(
      c(filtros()$pesquisar, input$tabset1, input$tabset2, input$tabset3, input$tabset4, input$localidade_resumo_fetal, input$localidade_resumo_neonat, input$localidade_resumo_perinatal, input$localidade_resumo_morbidade_neonatal),
      data7_resumo_aux(),
      ignoreNULL = FALSE
    )

    ### Para a referência -----------------------------------------------------
    data7_resumo_referencia_aux <- reactive({
      bloco7 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco7_calcs(), df_calcs_dist_bloco7 = bloco7_calcs_dist, bloco  = "bloco7", input = input,  filtros = filtros(), referencia = TRUE)
    })

    # Não queremos que as caixinhas se atualizem quando os inputs dos gráficos de distribuição percentual mudarem
    data7_resumo_referencia <- eventReactive(
      c(filtros()$pesquisar, input$tabset1, input$tabset2, input$tabset3, input$tabset4, input$localidade_resumo_fetal, input$localidade_resumo_neonat, input$localidade_resumo_perinatal, input$localidade_resumo_morbidade_neonatal),
      data7_resumo_referencia_aux(),
      ignoreNULL = FALSE
    )

    ## Criando os outputs das caixinhas ---------------------------------------
    ### Para os indicadores de mortalidade fetal ------------------------------
    #### Número de óbitos fetais (critério 1) ---------------------------------
    numero_obitos_fetais <- reactive({
      dplyr::case_when(
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "peso_fetal") ~ "obitos_fetais",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_menos1000") ~ "fetal_peso_menos_1000",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1000_1499") ~ "fetal_peso_1000_1499",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1500_2499") ~ "fetal_peso_1500_2499",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_peso_mais_2500",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_antes",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_menos1000") ~ "fetal_antes_peso_menos_1000",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1000_1499") ~ "fetal_antes_peso_1000_1499",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1500_2499") ~ "fetal_antes_peso_1500_2499",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_antes_peso_mais_2500",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_durante",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_menos1000") ~ "fetal_durante_peso_menos_1000",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1000_1499") ~ "fetal_durante_peso_1000_1499",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1500_2499") ~ "fetal_durante_peso_1500_2499",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_durante_peso_mais_2500",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_depois",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_menos1000") ~ "fetal_depois_peso_menos_1000",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1000_1499") ~ "fetal_depois_peso_1000_1499",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1500_2499") ~ "fetal_depois_peso_1500_2499",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_depois_peso_mais_2500",
      )
    })

    titulo_obitos_fetais_aux <- reactive({
      dplyr::case_when(
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_menos1000") ~ "Número de óbitos fetais com peso menor que 1000 g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1000_1499") ~ "Número de óbitos fetais com peso de 1000 a 1499 g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1500_2499") ~ "Número de óbitos fetais com peso de 1500 a 2499 g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais com peso maior ou igual a 2500 g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais antes do trabalho de parto",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_menos1000") ~ "Número de óbitos fetais antes do trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1000_1499") ~ "Número de óbitos fetais antes do trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1500_2499") ~ "Número de óbitos fetais antes do trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais antes do trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais durante o trabalho de parto",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_menos1000") ~ "Número de óbitos fetais durante o trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1000_1499") ~ "Número de óbitos fetais durante o trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1500_2499") ~ "Número de óbitos fetais durante o trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais durante o trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais depois do trabalho de parto",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_menos1000") ~ "Número de óbitos fetais depois do trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1000_1499") ~ "Número de óbitos fetais depois do trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1500_2499") ~ "Número de óbitos fetais depois do trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais depois do trabalho de parto com peso maior ou igual a 2500 g",
      )
    })

    titulo_obitos_fetais <- reactive({
      titulo_obitos_fetais_aux()
    })

    output$caixa_b7_fetal_i1 <- renderUI({ #[vvv]
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = numero_obitos_fetais(),
        titulo = titulo_obitos_fetais(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[numero_obitos_fetais()]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          nivel_selecionado() == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Número de óbitos fetais (critério 2 - oms) ---------------------------

    numero_obitos_fetais_oms <- reactive({
      dplyr::case_when(
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "obitos_fetais_oms",
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "fetal_oms_peso_menos_1000",
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "fetal_oms_peso_1000_1499",
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "fetal_oms_peso_1500_2499",
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "fetal_oms_peso_mais_2500",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "fetal_oms_antes",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "fetal_oms_antes_peso_menos_1000",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "fetal_oms_antes_peso_1000_1499",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "fetal_oms_antes_peso_1500_2499",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "fetal_oms_antes_peso_mais_2500",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "fetal_oms_durante",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "fetal_oms_durante_peso_menos_1000",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "fetal_oms_durante_peso_1000_1499",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "fetal_oms_durante_peso_1500_2499",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "fetal_oms_durante_peso_mais_2500",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "fetal_oms_depois",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "fetal_oms_depois_peso_menos_1000",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "fetal_oms_depois_peso_1000_1499",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "fetal_oms_depois_peso_1500_2499",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "fetal_oms_depois_peso_mais_2500",
      )
    })

    # titulo_obitos_fetais_oms_aux <- reactive({
    #   dplyr::case_when(
    #     (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "Número de óbitos fetais (mais de 28 semanas)",
    #     (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "Número de óbitos fetais (mais de 28 semanas) com peso menor que 1000 g",
    #     (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "Número de óbitos fetais (mais de 28 semanas) com peso de 1000 a 1499 g",
    #     (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "Número de óbitos fetais (mais de 28 semanas) com peso de 1500 a 2499 g",
    #     (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "Número de óbitos fetais (mais de 28 semanas) com peso maior ou igual a 2500 g",
    #     (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "Número de óbitos fetais (mais de 28 semanas) antes do trabalho de parto",
    #     (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "Número de óbitos fetais (mais de 28 semanas) antes do trabalho de parto com peso menor que 1000 g",
    #     (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "Número de óbitos fetais (mais de 28 semanas) antes do trabalho de parto com peso de 1000 a 1499 g",
    #     (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "Número de óbitos fetais (mais de 28 semanas) antes do trabalho de parto com peso de 1500 a 2499 g",
    #     (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "Número de óbitos fetais (mais de 28 semanas) antes do trabalho de parto com peso maior ou igual a 2500 g",
    #     (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "Número de óbitos fetais (mais de 28 semanas) durante o trabalho de parto",
    #     (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "Número de óbitos fetais (mais de 28 semanas) durante o trabalho de parto com peso menor que 1000 g",
    #     (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "Número de óbitos fetais (mais de 28 semanas) durante o trabalho de parto com peso de 1000 a 1499 g",
    #     (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "Número de óbitos fetais (mais de 28 semanas) durante o trabalho de parto com peso de 1500 a 2499 g",
    #     (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "Número de óbitos fetais (mais de 28 semanas) durante o trabalho de parto com peso maior ou igual a 2500 g",
    #     (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "Número de óbitos fetais (mais de 28 semanas) depois do trabalho de parto",
    #     (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "Número de óbitos fetais (mais de 28 semanas) depois do trabalho de parto com peso menor que 1000 g",
    #     (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "Número de óbitos fetais (mais de 28 semanas) depois do trabalho de parto com peso de 1000 a 1499 g",
    #     (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "Número de óbitos fetais (mais de 28 semanas) depois do trabalho de parto com peso de 1500 a 2499 g",
    #     (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "Número de óbitos fetais (mais de 28 semanas) depois do trabalho de parto com peso maior ou igual a 2500 g",
    #   )
    # })

    titulo_obitos_fetais_oms_aux <- reactive({
      dplyr::case_when(
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "Número de óbitos fetais",
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "Número de óbitos fetais com peso menor que 1000 g",
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "Número de óbitos fetais com peso de 1000 a 1499 g",
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "Número de óbitos fetais com peso de 1500 a 2499 g",
        (input$parto_fetal_oms == "fetal_parto_geral" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "Número de óbitos fetais com peso maior ou igual a 2500 g",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "Número de óbitos fetais antes do trabalho de parto",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "Número de óbitos fetais antes do trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "Número de óbitos fetais antes do trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "Número de óbitos fetais antes do trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal_oms == "antes" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "Número de óbitos fetais antes do trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "Número de óbitos fetais durante o trabalho de parto",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "Número de óbitos fetais durante o trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "Número de óbitos fetais durante o trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "Número de óbitos fetais durante o trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal_oms == "durante" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "Número de óbitos fetais durante o trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "peso_fetal") ~ "Número de óbitos fetais depois do trabalho de parto",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_menos1000") ~ "Número de óbitos fetais depois do trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_1000_1499") ~ "Número de óbitos fetais depois do trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_1500_2499") ~ "Número de óbitos fetais depois do trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal_oms == "depois" & input$faixa_peso_fetal_oms == "fetal_mais2500") ~ "Número de óbitos fetais depois do trabalho de parto com peso maior ou igual a 2500 g",
      )
    })

    titulo_obitos_fetais_oms <- reactive({
      titulo_obitos_fetais_oms_aux()
    })

    output$caixa_b7_fetal_i5 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = numero_obitos_fetais_oms(),
        titulo = titulo_obitos_fetais_oms(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[numero_obitos_fetais_oms()]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          nivel_selecionado() == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Taxa de mortalidade fetal (definição 1) -------------------------------
    taxa_mortalidade_fetal <- reactive({
      dplyr::case_when(
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "peso_fetal") ~ "taxa_mort_fetal",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "taxa_mort_fetal_peso_menos_1000",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "taxa_mort_fetal_peso_1000_1499",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "taxa_mort_fetal_peso_1500_2499",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "taxa_mort_fetal_peso_mais_2500",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "peso_fetal") ~ "taxa_mort_fetal_antes",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "taxa_mort_fetal_antes_peso_menos_1000",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "taxa_mort_fetal_antes_peso_1000_1499",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "taxa_mort_fetal_antes_peso_1500_2499",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "taxa_mort_fetal_antes_peso_mais_2500",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "peso_fetal") ~ "taxa_mort_fetal_durante",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "taxa_mort_fetal_durante_peso_menos_1000",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "taxa_mort_fetal_durante_peso_1000_1499",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "taxa_mort_fetal_durante_peso_1500_2499",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "taxa_mort_fetal_durante_peso_mais_2500",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "peso_fetal") ~ "taxa_mort_fetal_depois",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "taxa_mort_fetal_depois_peso_menos_1000",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "taxa_mort_fetal_depois_peso_1000_1499",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "taxa_mort_fetal_depois_peso_1500_2499",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "taxa_mort_fetal_depois_peso_mais_2500",
      )
    })

    # percentil5_taxa_mortalidade_fetal <- reactive({
    #   dplyr::case_when(
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "peso_fetal") ~ "percentil5_taxa_mort_fetal",
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "percentil5_taxa_mort_fetal_peso_menos_1000",
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "percentil5_taxa_mort_fetal_peso_1000_1499",
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "percentil5_taxa_mort_fetal_peso_1500_2499",
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "percentil5_taxa_mort_fetal_peso_mais_2500",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "peso_fetal") ~ "percentil5_taxa_mort_fetal_antes",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "percentil5_taxa_mort_fetal_antes_peso_menos_1000",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "percentil5_taxa_mort_fetal_antes_peso_1000_1499",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "percentil5_taxa_mort_fetal_antes_peso_1500_2499",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "percentil5_taxa_mort_fetal_antes_peso_mais_2500",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "peso_fetal") ~ "percentil5_taxa_mort_fetal_durante",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "percentil5_taxa_mort_fetal_durante_peso_menos_1000",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "percentil5_taxa_mort_fetal_durante_peso_1000_1499",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "percentil5_taxa_mort_fetal_durante_peso_1500_2499",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "percentil5_taxa_mort_fetal_durante_peso_mais_2500",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "peso_fetal") ~ "percentil5_taxa_mort_fetal_depois",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "percentil5_taxa_mort_fetal_depois_peso_menos_1000",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "percentil5_taxa_mort_fetal_depois_peso_1000_1499",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "percentil5_taxa_mort_fetal_depois_peso_1500_2499",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "percentil5_taxa_mort_fetal_depois_peso_mais_2500",
    #   )
    # })

    # percentil95_taxa_mortalidade_fetal <- reactive({
    #   dplyr::case_when(
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "peso_fetal") ~ "percentil95_taxa_mort_fetal",
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "percentil95_taxa_mort_fetal_peso_menos_1000",
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "percentil95_taxa_mort_fetal_peso_1000_1499",
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "percentil95_taxa_mort_fetal_peso_1500_2499",
    #     (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "percentil95_taxa_mort_fetal_peso_mais_2500",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "peso_fetal") ~ "percentil95_taxa_mort_fetal_antes",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "percentil95_taxa_mort_fetal_antes_peso_menos_1000",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "percentil95_taxa_mort_fetal_antes_peso_1000_1499",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "percentil95_taxa_mort_fetal_antes_peso_1500_2499",
    #     (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "percentil95_taxa_mort_fetal_antes_peso_mais_2500",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "peso_fetal") ~ "percentil95_taxa_mort_fetal_durante",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "percentil95_taxa_mort_fetal_durante_peso_menos_1000",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "percenti95_taxa_mort_fetal_durante_peso_1000_1499",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "percenti95_taxa_mort_fetal_durante_peso_1500_2499",
    #     (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "percentil95_taxa_mort_fetal_durante_peso_mais_2500",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "peso_fetal") ~ "percentil95_taxa_mort_fetal_depois",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "percentil95_taxa_mort_fetal_depois_peso_menos_1000",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "percentil95_taxa_mort_fetal_depois_peso_1000_1499",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "percentil95_taxa_mort_fetal_depois_peso_1500_2499",
    #     (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "percentil95_taxa_mort_fetal_depois_peso_mais_2500",
    #   )
    # })

    titulo_taxa_mortalidade_fetal_aux <- reactive({
      dplyr::case_when(
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "peso_fetal") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos com peso menor que 1000 g",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos com peso de 1000 a 1499 g",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos com peso de 1500 a 2499 g",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos com peso maior ou igual a 2500 g",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "peso_fetal") ~ "Taxa de mortalidade fetal antes do trabalho de parto",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "peso_fetal") ~ "Taxa de mortalidade fetal durante o trabalho de parto",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "peso_fetal") ~ "Taxa de mortalidade fetal depois do trabalho de parto",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_menos1000") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1000_1499") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1500_2499") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso maior ou igual a 2500 g",
      )
    })

    titulo_taxa_mortalidade_fetal <- reactive({
      titulo_taxa_mortalidade_fetal_aux()
    })

    output$caixa_b7_fetal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = taxa_mortalidade_fetal(),
        titulo = titulo_taxa_mortalidade_fetal(),
        #tem_meta = ifelse(taxa_mortalidade_fetal() == "taxa_mort_fetal", TRUE, FALSE),
        tem_meta = TRUE,
        #tipo_referencia = ifelse(taxa_mortalidade_fetal() == "taxa_mort_fetal", "meta ODS", ""),
        tipo_referencia = "meta ODS",
        valor_de_referencia = ifelse(
          data7_resumo_referencia()[[taxa_mortalidade_fetal()]] > 0,
          data7_resumo_referencia()[[taxa_mortalidade_fetal()]],
          NaN
        ),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Taxa de mortalidade fetal (definição 2)--------------------------------
    taxa_mortalidade_fetal_oms <- reactive({
      dplyr::case_when(
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "taxa_mort_fetal_oms",
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "taxa_mort_fetal_oms_peso_menos_1000",
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "taxa_mort_fetal_oms_peso_1000_1499",
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "taxa_mort_fetal_oms_peso_1500_2499",
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "taxa_mort_fetal_oms_peso_mais_2500",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "taxa_mort_fetal_oms_antes",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "taxa_mort_fetal_oms_antes_peso_menos_1000",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "taxa_mort_fetal_oms_antes_peso_1000_1499",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "taxa_mort_fetal_oms_antes_peso_1500_2499",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "taxa_mort_fetal_oms_antes_peso_mais_2500",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "taxa_mort_fetal_oms_durante",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "taxa_mort_fetal_oms_durante_peso_menos_1000",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "taxa_mort_fetal_oms_durante_peso_1000_1499",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "taxa_mort_fetal_oms_durante_peso_1500_2499",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "taxa_mort_fetal_oms_durante_peso_mais_2500",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "taxa_mort_fetal_oms_depois",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "taxa_mort_fetal_oms_depois_peso_menos_1000",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "taxa_mort_fetal_oms_depois_peso_1000_1499",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "taxa_mort_fetal_oms_depois_peso_1500_2499",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "taxa_mort_fetal_oms_depois_peso_mais_2500",
      )
    })

    # percentil5_taxa_mortalidade_fetal_oms <- reactive({
    #   dplyr::case_when(
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "percentil5_taxa_mort_fetal_oms",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "percentil5_taxa_mort_fetal_oms_peso_menos_1000",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "percentil5_taxa_mort_fetal_oms_peso_1000_1499",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "percentil5_taxa_mort_fetal_oms_peso_1500_2499",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "percentil5_taxa_mort_fetal_oms_peso_mais_2500",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "percentil5_taxa_mort_fetal_oms_antes",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "percentil5_taxa_mort_fetal_oms_antes_peso_menos_1000",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "percentil5_taxa_mort_fetal_oms_antes_peso_1000_1499",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "percentil5_taxa_mort_fetal_oms_antes_peso_1500_2499",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "percentil5_taxa_mort_fetal_oms_antes_peso_mais_2500",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "percentil5_taxa_mort_fetal_oms_durante",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "percentil5_taxa_mort_fetal_oms_durante_peso_menos_1000",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "percentil5_taxa_mort_fetal_oms_durante_peso_1000_1499",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "percentil5_taxa_mort_fetal_oms_durante_peso_1500_2499",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "percentil5_taxa_mort_fetal_oms_durante_peso_mais_2500",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "percentil5_taxa_mort_fetal_oms_depois",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "percentil5_taxa_mort_fetal_oms_depois_peso_menos_1000",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "percentil5_taxa_mort_fetal_oms_depois_peso_1000_1499",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "percentil5_taxa_mort_fetal_oms_depois_peso_1500_2499",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "percentil5_taxa_mort_fetal_oms_depois_peso_mais_2500",
    #   )
    # })

    # percentil95_taxa_mortalidade_fetal_oms <- reactive({
    #   dplyr::case_when(
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "percentil95_taxa_mort_fetal_oms",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "percentil95_taxa_mort_fetal_oms_peso_menos_1000",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "percentil95_taxa_mort_fetal_oms_peso_1000_1499",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "percentil95_taxa_mort_fetal_oms_peso_1500_2499",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "percentil95_taxa_mort_fetal_oms_peso_mais_2500",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "percentil95_taxa_mort_fetal_oms_antes",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "percentil95_taxa_mort_fetal_oms_antes_peso_menos_1000",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "percentil95_taxa_mort_fetal_oms_antes_peso_1000_1499",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "percentil95_taxa_mort_fetal_oms_antes_peso_1500_2499",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "percentil95_taxa_mort_fetal_oms_antes_peso_mais_2500",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "percentil95_taxa_mort_fetal_oms_durante",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "percentil95_taxa_mort_fetal_oms_durante_peso_menos_1000",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "percentil95_taxa_mort_fetal_oms_durante_peso_1000_1499",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "percentil95_taxa_mort_fetal_oms_durante_peso_1500_2499",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "percentil95_taxa_mort_fetal_oms_durante_peso_mais_2500",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "percentil95_taxa_mort_fetal_oms_depois",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "percentil95_taxa_mort_fetal_oms_depois_peso_menos_1000",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "percentil95_taxa_mort_fetal_oms_depois_peso_1000_1499",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "percentil95_taxa_mort_fetal_oms_depois_peso_1500_2499",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "percentil95_taxa_mort_fetal_oms_depois_peso_mais_2500",
    #   )
    # })


    # titulo_taxa_mortalidade_fetal_oms_aux <- reactive({
    #   dplyr::case_when(
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "Taxa de mortalidade fetal (mais de 28 semanas)",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "Taxa de mortalidade fetal (mais de 28 semanas) com peso menor que 1000 g",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "Taxa de mortalidade fetal (mais de 28 semanas) com peso de 1000 a 1499 g",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "Taxa de mortalidade fetal (mais de 28 semanas) com peso de 1500 a 2499 g",
    #     (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "Taxa de mortalidade fetal (mais de 28 semanas) com peso maior ou igual a 2500 g",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "Taxa de mortalidade fetal (mais de 28 semanas) antes do trabalho de parto",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "Taxa de mortalidade fetal (mais de 28 semanas) antes do trabalho de parto com peso menor que 1000 g",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "Taxa de mortalidade fetal (mais de 28 semanas) antes do trabalho de parto com peso de 1000 a 1499 g",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "Taxa de mortalidade fetal (mais de 28 semanas) antes do trabalho de parto com peso de 1500 a 2499 g",
    #     (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "Taxa de mortalidade fetal (mais de 28 semanas) antes do trabalho de parto com peso maior ou igual a 2500 g",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "Taxa de mortalidade fetal (mais de 28 semanas) durante o trabalho de parto",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "Taxa de mortalidade fetal (mais de 28 semanas) durante o trabalho de parto com peso menor que 1000 g",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "Taxa de mortalidade fetal (mais de 28 semanas) durante o trabalho de parto com peso de 1000 a 1499 g",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "Taxa de mortalidade fetal (mais de 28 semanas) durante o trabalho de parto com peso de 1500 a 2499 g",
    #     (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "Taxa de mortalidade fetal (mais de 28 semanas) durante o trabalho de parto com peso maior ou igual a 2500 g",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "Taxa de mortalidade fetal (mais de 28 semanas) depois do trabalho de parto",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "Taxa de mortalidade fetal (mais de 28 semanas) depois do trabalho de parto com peso menor que 1000 g",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "Taxa de mortalidade fetal (mais de 28 semanas) depois do trabalho de parto com peso de 1000 a 1499 g",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "Taxa de mortalidade fetal (mais de 28 semanas) depois do trabalho de parto com peso de 1500 a 2499 g",
    #     (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "Taxa de mortalidade fetal (mais de 28 semanas) depois do trabalho de parto com peso maior ou igual a 2500 g",
    #   )
    # })

    titulo_taxa_mortalidade_fetal_oms_aux <- reactive({
      dplyr::case_when(
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "Taxa de mortalidade fetal",
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos com peso menor que 1000 g",
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos com peso de 1000 a 1499 g",
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos com peso de 1500 a 2499 g",
        (input$parto_fetal2_oms == "fetal_parto_geral" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "Taxa de mortalidade fetal por 1000 nascidos vivos com peso maior ou igual a 2500 g",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "Taxa de mortalidade fetal antes do trabalho de parto",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal2_oms == "antes" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "Taxa de mortalidade fetal durante o trabalho de parto",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal2_oms == "durante" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "peso_fetal") ~ "Taxa de mortalidade fetal depois do trabalho de parto",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_menos1000") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso menor que 1000 g",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1000_1499") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso de 1000 a 1499 g",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_1500_2499") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso de 1500 a 2499 g",
        (input$parto_fetal2_oms == "depois" & input$faixa_peso_fetal2_oms == "fetal_mais2500") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso maior ou igual a 2500 g",
      )
    })

    titulo_taxa_mortalidade_fetal_oms <- reactive({
      titulo_taxa_mortalidade_fetal_oms_aux()
    })

    output$caixa_b7_fetal_i6 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = taxa_mortalidade_fetal_oms(),
        titulo = titulo_taxa_mortalidade_fetal_oms(),
        tem_meta = TRUE,
        tipo_referencia = "meta ODS",
        valor_de_referencia = dplyr::if_else(data7_resumo_referencia()[[taxa_mortalidade_fetal_oms()]] >0 ,
                                             data7_resumo_referencia()[[taxa_mortalidade_fetal_oms()]], NaN),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$caixa_b7_fetal_i3 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "fetal peso por idade gestacional",
        titulo = "Dentre os óbitos fetais,",
        tamanho_caixa = "350px"
      )
    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$caixa_b7_fetal_i4 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "fetal momento do obito por peso",
        titulo = "Dentre os óbitos fetais,",
        tamanho_caixa = "350px"
      )
    })


    ### Para os indicadores de mortalidade neonatal ---------------------------
    #### Taxa de mortalidade neonatal por 1000 nascidos vivos -----------------
    titulo_caixa_neonat <- reactive({
      dplyr::case_when(
        input$faixa_peso == "mort_neonat" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos",
        input$faixa_peso == "mort_neonat_menos1000" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos para peso ao nascer menor que 1000 g",
        input$faixa_peso == "mort_neonat_1000_1499" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos para peso ao nascer de 1000 a 1499 g",
        input$faixa_peso == "mort_neonat_1500_2499" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos para peso ao nascer de 1500 a 2499 g",
        input$faixa_peso == "mort_neonat_mais2500" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500 g"
      )
    })

    output$caixa_b7_neonat_i1 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso,
        titulo = titulo_caixa_neonat(),
        tem_meta = TRUE,
        tipo_referencia = "meta ODS",
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso]],
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Taxa de mortalidade neonatal precoce por 1000 nascidos vivos ---------
    titulo_caixa_neonat_precoc <- reactive({
      dplyr::case_when(
        input$faixa_peso_precoc == "mort_neonat_precoc" ~ "Taxa de mortalidade neonatal (0 a 6 dias) por 1000 nascidos vivos",
        input$faixa_peso_precoc == "mort_neonat_precoc_menos1000" ~ "Taxa de mortalidade neonatal (0 a 6 dias) por 1000 nascidos vivos para peso ao nascer menor que 1000 g",
        input$faixa_peso_precoc == "mort_neonat_precoc_1000_1499" ~ "Taxa de mortalidade neonatal (0 a 6 dias) por 1000 nascidos vivos para peso ao nascer de 1000 a 1499 g",
        input$faixa_peso_precoc == "mort_neonat_precoc_1500_2499" ~ "Taxa de mortalidade neonatal (0 a 6 dias) por 1000 nascidos vivos para peso ao nascer de 1500 a 2499 g",
        input$faixa_peso_precoc == "mort_neonat_precoc_mais2500" ~ "Taxa de mortalidade neonatal (0 a 6 dias) por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500 g"
      )
    })

    output$caixa_b7_neonat_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_precoc,
        titulo = titulo_caixa_neonat_precoc(),
        tem_meta = TRUE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_precoc]],
        tipo_referencia = "meta ODS",
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Taxa de mortalidade neonatal tardia por 1000 nascidos vivos ----------
    titulo_caixa_neonat_tardia <- reactive({
      dplyr::case_when(
        input$faixa_peso_tardia == "mort_neonat_tardia" ~ "Taxa de mortalidade neonatal (7 a 27 dias) por 1000 nascidos vivos",
        input$faixa_peso_tardia == "mort_neonat_tardia_menos1000" ~ "Taxa de mortalidade neonatal (7 a 27 dias) por 1000 nascidos vivos para peso ao nascer menor que 1000 g",
        input$faixa_peso_tardia == "mort_neonat_tardia_1000_1499" ~ "Taxa de mortalidade neonatal (7 a 27 dias) por 1000 nascidos vivos para peso ao nascer de 1000 a 1499 g",
        input$faixa_peso_tardia == "mort_neonat_tardia_1500_2499" ~ "Taxa de mortalidade neonatal (7 a 27 dias) por 1000 nascidos vivos para peso ao nascer de 1500 a 2499 g",
        input$faixa_peso_tardia == "mort_neonat_tardia_mais2500" ~ "Taxa de mortalidade neonatal (7 a 27 dias) por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500 g"
      )
    })

    output$caixa_b7_neonat_i3 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_tardia,
        titulo = titulo_caixa_neonat_tardia(),
        tem_meta = TRUE,
        tipo_referencia = "meta ODS",

        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_tardia]],
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Número de óbitos neonatais -------------------------------------------
    titulo_caixa_obitos_neonat <- reactive({
      dplyr::case_when(
        input$obitos_faixa_peso == "obitos_neonat" ~ "Número de óbitos neonatais",
        input$obitos_faixa_peso == "obitos_neonat_menos1000" ~ "Número de óbitos neonatais para peso ao nascer menor que 1000 g",
        input$obitos_faixa_peso == "obitos_neonat_1000_1499" ~ "Número de óbitos neonatais para peso ao nascer de 1000 a 1499 g",
        input$obitos_faixa_peso == "obitos_neonat_1500_2499" ~ "Número de óbitos neonatais para peso ao nascer de 1500 a 2499 g",
        input$obitos_faixa_peso == "obitos_neonat_mais2500" ~ "Número de óbitos neonatais para peso ao nascer maior ou igual a 2500 g"
      )
    })

    output$caixa_b7_neonat_i4 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$obitos_faixa_peso,
        titulo = titulo_caixa_obitos_neonat(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$obitos_faixa_peso]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          nivel_selecionado() == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "330px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$caixa_b7_neonat_i5 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "neonatal momento do obito por peso",
        titulo = "Dentre os óbitos neonatais,",
        tamanho_caixa = "330px"
      )
    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$caixa_b7_neonat_i6 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "neonatal peso por momento do obito",
        titulo = "Dentre os óbitos neonatais,",
        tamanho_caixa = "330px"
      )
    })


    ### Para os indicadores de mortalidade perinatal --------------------------
    #### Número de óbitos perinatais (definição 1) ----------------------------
    titulo_caixa_perinatal_total <- reactive({
      dplyr::case_when(
        input$faixa_peso_perinatal_total == "perinatal_todos_total" ~ "Número de óbitos perinatais",
        input$faixa_peso_perinatal_total == "perinatal_todos_peso_menos_1000" ~ "Número de óbitos perinatais com peso menor que 1000 g",
        input$faixa_peso_perinatal_total == "perinatal_todos_peso_1000_1499" ~ "Número de óbitos perinatais com peso de 1000 a 1499 g",
        input$faixa_peso_perinatal_total == "perinatal_todos_peso_1500_2499" ~ "Número de óbitos perinatais com peso de 1500 a 2499 g",
        input$faixa_peso_perinatal_total == "perinatal_todos_peso_mais_2500" ~ "Número de óbitos perinatais com peso maior ou igual a 2500 g",
      )
    })

    output$caixa_b7_perinatal_i1 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_perinatal_total,
        titulo = titulo_caixa_perinatal_total(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_perinatal_total]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          nivel_selecionado() == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Número de óbitos perinatais (definição 2) ----------------------------
    # titulo_caixa_perinatal_oms <- reactive({
    #   dplyr::case_when(
    #     input$faixa_peso_perinatal_oms == "obitos_perinatal_oms" ~ "Número de óbitos perinatais (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #     input$faixa_peso_perinatal_oms == "perinatal_oms_menos1000" ~ "Número de óbitos perinatais com peso menor que 1000 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #     input$faixa_peso_perinatal_oms == "perinatal_oms_1000_1499" ~ "Número de óbitos perinatais com peso de 1000 a 1499 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #     input$faixa_peso_perinatal_oms == "perinatal_oms_1500_2499" ~ "Número de óbitos perinatais com peso de 1500 a 2499 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #     input$faixa_peso_perinatal_oms == "perinatal_oms_mais2500" ~ "Número de óbitos perinatais com peso maior ou igual a 2500 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #   )
    # })

    output$caixa_b7_perinatal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_perinatal_oms,
        titulo = titulo_caixa_perinatal_oms(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_perinatal_oms]],
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          nivel_selecionado() == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Taxa de mortalidade perinatal (definição 1) --------------------------
    titulo_caixa_taxa_perinatal_total <- reactive({
      dplyr::case_when(
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos",
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_menos1000" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos com peso menor que 1000 g",
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_1000_1499" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos com peso de 1000 a 1499 g",
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_1500_2499" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos com peso de 1500 a 2499 g",
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_mais2500" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos com peso maior ou igual a 2500 g"
      )
    })

    output$caixa_b7_perinatal_i3 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_perinatal_taxa_total,
        titulo = titulo_caixa_taxa_perinatal_total(),
        tem_meta = TRUE,
        tipo_referencia = "meta ODS adaptada para os óbitos perinatais",

        valor_de_referencia = ifelse(
          data7_resumo_referencia()[[input$faixa_peso_perinatal_taxa_total]] > 0,
          data7_resumo_referencia()[[input$faixa_peso_perinatal_taxa_total]],
          NaN
        ),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Taxa de mortalidade perinatal (definição 2) --------------------------
    # titulo_caixa_taxa_perinatal_oms <- reactive({
    #   dplyr::case_when(
    #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_menos1000" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos com peso menor que 1000 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_1000_1499" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos com peso de 1000 a 1499 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_1500_2499" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos com peso de 1500 a 2499 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
    #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_mais2500" ~ "Taxa de mortalidade perinatal por 1000 nascidos vivos com peso maior ou igual a 2500 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)"
    #   )
    # })

    output$caixa_b7_perinatal_i4 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_perinatal_taxa_oms,
        titulo = titulo_caixa_taxa_perinatal_oms(),
        tem_meta = TRUE,
        tipo_referencia = "meta ODS adaptada para os óbitos perinatais",
        valor_de_referencia = ifelse(
            data7_resumo_referencia()[[input$faixa_peso_perinatal_taxa_oms]] > 0,
            data7_resumo_referencia()[[input$faixa_peso_perinatal_taxa_oms]],
            NaN
        ),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$caixa_b7_perinatal_i5 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "perinatal momento do obito por peso",
        titulo = "Dentre os óbitos perinatais,",
        tamanho_caixa = "350px",
      )
    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$caixa_b7_perinatal_i6 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "perinatal peso por momento do obito",
        titulo = "Dentre os óbitos perinatais,",
        tamanho_caixa = "350px",
      )
    })

    ############ Para os de morbidade neonatal

    ### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    output$caixa_b7_morbidade_neonatal_i1 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = "porc_condicoes_ameacadoras",
        titulo = "Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida",
        tem_meta = TRUE,
        valor_de_referencia = data7_resumo_referencia()$porc_condicoes_ameacadoras,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        #fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
        nivel_de_analise = nivel_selecionado()
        # nivel_de_analise = ifelse(
        #   filtros()$comparar == "Não",
        #   filtros()$nivel,
        #   ifelse(
        #     input$localidade_resumo == "escolha1",
        #     filtros()$nivel,
        #     filtros()$nivel2
        #   )
        # )
      )
    })

    ### Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS  -----------
    output$caixa_b7_morbidade_neonatal_i3 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = "porc_internacoes_uti_menores_28_dias_sih_geral",
        titulo = "Porcentagem de internações neonatais (até o 27º dia de vida) em UTI no SUS",
        tem_meta = TRUE,
        valor_de_referencia = data7_resumo_referencia()$porc_internacoes_uti_menores_28_dias_sih_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        #fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
        nivel_de_analise = nivel_selecionado()
        # nivel_de_analise = ifelse(
        #   filtros()$comparar == "Não",
        #   filtros()$nivel,
        #   ifelse(
        #     input$localidade_resumo == "escolha1",
        #     filtros()$nivel,
        #     filtros()$nivel2
        #   )
        # )
      )
    })


    ### Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos com vínculo com o SUS ----
    output$caixa_b7_morbidade_neonatal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = "porc_internacoes_menores_28_dias_sih_geral",
        titulo = "Porcentagem de internações neonatais (até o 27º dia de vida) no SUS",
        tem_meta = TRUE,
        valor_de_referencia = data7_resumo_referencia()$porc_internacoes_menores_28_dias_sih_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        #fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
        nivel_de_analise = nivel_selecionado()
        # nivel_de_analise = ifelse(
        #   filtros()$comparar == "Não",
        #   filtros()$nivel,
        #   ifelse(
        #     input$localidade_resumo == "escolha1",
        #     filtros()$nivel,
        #     filtros()$nivel2
        #   )
        # )
      )
    })

    ### Caixas para obitos potencialmente evitaveis ############################ [zzz]

    ### Aba fetal

    output$caixa_b7_evitaveis_fetal <- renderUI({
      cria_caixa_server(
        dados = bloco7_evitaveis_resumo(),
        indicador = "porc_evitavel_fetal",
        titulo = "Porcentagem de óbitos fetais potencialmente evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = bloco7_evitaveis_resumo_comp()$porc_evitavel_fetal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    ### Aba perinatal

    output$caixa_b7_evitaveis_perinatal <- renderUI({
      cria_caixa_server(
        dados = bloco7_evitaveis_resumo(),
        indicador = "porc_evitavel_perinatal",
        titulo = "Porcentagem de óbitos perinatais potencialmente evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = bloco7_evitaveis_resumo_comp()$porc_evitavel_perinatal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    ### Aba neonatal

    output$caixa_b7_evitaveis_neonatal <- renderUI({
      cria_caixa_server(
        dados = bloco7_evitaveis_resumo(),
        indicador = "porc_evitavel_neonatal",
        titulo = "Porcentagem de óbitos neonatais potencialmente evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = bloco7_evitaveis_resumo_comp()$porc_evitavel_neonatal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    ### Caixas principais causas de obito ######################################

    ### Aba fetal

    output$caixa_b7_principais_fetal <- renderUI({
      cria_caixa_principais_evitaveis_bloco7(
        dados = bloco7_principais_obito_fetal(),
        titulo = "Dentre os óbitos fetais,",
        tamanho_caixa = "350px"
      )
    })

    ### Aba perinatal

    output$caixa_b7_principais_perinatal <- renderUI({
      cria_caixa_principais_evitaveis_bloco7(
        dados = bloco7_principais_obito_perinatal(),
        titulo = "Dentre os óbitos perinatais,",
        tamanho_caixa = "350px"
      )
    })

    ### Aba neonatal

    output$caixa_b7_principais_neonatal <- renderUI({
      cria_caixa_principais_evitaveis_bloco7(
        dados = bloco7_principais_obito_neonatal(),
        titulo = "Dentre os óbitos neonatais,",
        tamanho_caixa = "330px"
      )
    })

    ### Aba morbidade neonatal

    output$caixa_b7_principais_morbidade_neonatal <- renderUI({
      cria_caixa_principais_evitaveis_bloco7(
        dados = bloco7_principais_internacoes_neonatal(),
        titulo = "Dentre as internações neonatais,",
        tamanho_caixa = "320px"
      )
    })

    # Para os gráficos --------------------------------------------------------
    cols <- c("#2c115f", "#b73779", "#fc8961", "#000004FF", "#f1605d")


    ### Para a distribuição de internações por grupos de causas

    data_filtrada_morbidade_aux <- reactive({
      bloco7_dist_morbidade |>
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

    data_filtrada_comp_morbidade_aux <- reactive({
      bloco7_dist_morbidade |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
          if (filtros()$nivel2 == "nacional")
            regiao %in% unique(tabela_aux_municipios$regiao)
          else if (filtros()$nivel2 == "regional")
            regiao == filtros()$regiao2
          else if (filtros()$nivel2 == "estadual")
            uf == filtros()$estado2
          else if (filtros()$nivel2 == "macro")
            macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
          else if(filtros()$nivel2 == "micro")
            r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
          else if(filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano)
    })

    data_plot_grupos_morbidade_neonatal <- reactive({
      data_filtrada_morbidade_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos)))), na.rm=T) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos)),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_morbidade_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_morbidade_neonatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("afeccoes_respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("fatores_maternos", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes_perinatal", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definidas", grupo_cid10) ~ "Mal definidas",
              grepl("ictericia", grupo_cid10) ~ "Icterícia neonatal",
              grepl("endocrinos", grupo_cid10) ~ "Transtornos endócrinos e metabólicos",
              grepl("alimentacao", grupo_cid10) ~ "Problemas de alimentação do recém-nascido",
              grepl("cardiacos_perinatal", grupo_cid10) ~ "Transtornos cardíacos do período perinatal",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_grupos_morbidade_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_morbidade_neonatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "infeccoes",
              grepl("asfixia", grupo_cid10) ~ "asfixia",
              grepl("ma_formacao", grupo_cid10) ~ "ma_formacao",
              grepl("afeccoes_respiratorias", grupo_cid10) ~ "afeccoes_respiratorias",
              grepl("fatores_maternos", grupo_cid10) ~ "fatores_maternos",
              grepl("afeccoes_perinatal", grupo_cid10) ~ "afeccoes_perinatal",
              grepl("mal_definidas", grupo_cid10) ~ "mal_definidas",
              grepl("ictericia", grupo_cid10) ~ "ictericia",
              grepl("endocrinos", grupo_cid10) ~ "endocrinos",
              grepl("alimentacao", grupo_cid10) ~ "alimentacao",
              grepl("cardiacos_perinatal", grupo_cid10) ~ "cardiacos_perinatal",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Prematuridade","Infecções","Asfixia/Hipóxia","Anomalia congênita","Afecções respiratórias do recém-nascido",
                                                                   "Fatores maternos relacionados à gravidez","Afecções originais no período perinatal",
                                                                   "Icterícia neonatal",  "Transtornos endócrinos e metabólicos",
                                                                    "Problemas de alimentação do recém-nascido", "Transtornos cardíacos do período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_morbidade_neonatal_comp <- reactive({
      data_filtrada_comp_morbidade_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos))), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos)),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_morbidade_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_morbidade_neonatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("afeccoes_respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("fatores_maternos", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes_perinatal", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definidas", grupo_cid10) ~ "Mal definidas",
              grepl("ictericia", grupo_cid10) ~ "Icterícia neonatal",
              grepl("endocrinos", grupo_cid10) ~ "Transtornos endócrinos e metabólicos",
              grepl("alimentacao", grupo_cid10) ~ "Problemas de alimentação do recém-nascido",
              grepl("cardiacos_perinatal", grupo_cid10) ~ "Transtornos cardíacos do período perinatal",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_grupos_morbidade_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_morbidade_neonatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "infeccoes",
              grepl("asfixia", grupo_cid10) ~ "asfixia",
              grepl("ma_formacao", grupo_cid10) ~ "ma_formacao",
              grepl("afeccoes_respiratorias", grupo_cid10) ~ "afeccoes_respiratorias",
              grepl("fatores_maternos", grupo_cid10) ~ "fatores_maternos",
              grepl("afeccoes_perinatal", grupo_cid10) ~ "afeccoes_perinatal",
              grepl("mal_definidas", grupo_cid10) ~ "mal_definidas",
              grepl("ictericia", grupo_cid10) ~ "ictericia",
              grepl("endocrinos", grupo_cid10) ~ "endocrinos",
              grepl("alimentacao", grupo_cid10) ~ "alimentacao",
              grepl("cardiacos_perinatal", grupo_cid10) ~ "cardiacos_perinatal",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Prematuridade","Infecções","Asfixia/Hipóxia","Anomalia congênita","Afecções respiratórias do recém-nascido",
                                                                   "Fatores maternos relacionados à gravidez","Afecções originais no período perinatal",
                                                                   "Icterícia neonatal",  "Transtornos endócrinos e metabólicos",
                                                                    "Problemas de alimentação do recém-nascido", "Transtornos cardíacos do período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_morbidade_neonatal_referencia <- reactive({
      bloco7_dist_morbidade |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos))), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos)),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_morbidade_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_morbidade_neonatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("afeccoes_respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("fatores_maternos", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes_perinatal", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definidas", grupo_cid10) ~ "Mal definidas",
              grepl("ictericia", grupo_cid10) ~ "Icterícia neonatal",
              grepl("endocrinos", grupo_cid10) ~ "Transtornos endócrinos e metabólicos",
              grepl("alimentacao", grupo_cid10) ~ "Problemas de alimentação do recém-nascido",
              grepl("cardiacos_perinatal", grupo_cid10) ~ "Transtornos cardíacos do período perinatal",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10) |>
        dplyr::summarise(
          br_porc_obitos = round(sum(br_porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Prematuridade","Infecções","Asfixia/Hipóxia","Anomalia congênita","Afecções respiratórias do recém-nascido",
                                                                   "Fatores maternos relacionados à gravidez","Afecções originais no período perinatal",
                                                                   "Icterícia neonatal",  "Transtornos endócrinos e metabólicos",
                                                                   "Problemas de alimentação do recém-nascido", "Transtornos cardíacos do período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_morbidade_neonatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_grupos_morbidade_neonatal()) != 0,
          "Não existem ocorrências de internações neonatais por grupos de causas para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_grupos_morbidade_neonatal(), data_plot_grupos_morbidade_neonatal_referencia())
    })

    data_plot_grupos_morbidade_neonatal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_grupos_morbidade_neonatal_comp()) != 0,
          "Não existem ocorrências de internações neonatais por grupos de causas para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_grupos_morbidade_neonatal_comp(), data_plot_grupos_morbidade_neonatal_referencia())
    })

    ## Calculando os indicadores para cada ano do período selecionado ---------
    ### Para os gráficos por análise de evitabilidade ----------------------------------
    #### Para a localidade selecionada ----------------------------------------
    data_filtrada_aux <- reactive({
      bloco7_distribuicao_cids |>
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

    # data_plot_evitaveis_fetal <- reactive({
    #   data_filtrada_aux() |>
    #     dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & !dplyr::contains("2") )| "obitos_fetais_totais"), sum) |>
    #     dplyr::rowwise() |>
    #     dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))))) |>
    #     dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
    #     tidyr::pivot_longer(
    #       cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis)),
    #       names_to = "grupo_cid10",
    #       values_to = "porc_obitos"
    #     ) |>
    #     dplyr::mutate(
    #       grupo_cid10 = ifelse(
    #         (grepl(paste(input$cids_evitaveis_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal)),
    #         dplyr::case_when(
    #           grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
    #           grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
    #           grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
    #           grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
    #           grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
    #           grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
    #           grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
    #           grepl("outros", grupo_cid10) ~ "Demais causas",
    #         ),
    #         "Grupos não selecionados"
    #       ),
    #       class = dplyr::case_when(
    #         filtros()$nivel == "nacional" ~ "Brasil",
    #         filtros()$nivel == "regional" ~ filtros()$regiao,
    #         filtros()$nivel == "estadual" ~ filtros()$estado,
    #         filtros()$nivel == "macro" ~ filtros()$macro,
    #         filtros()$nivel == "micro" ~ filtros()$micro,
    #         filtros()$nivel == "municipal" ~ filtros()$municipio
    #       )
    #     ) |>
    #     dplyr::ungroup() |>
    #     dplyr::group_by(ano, grupo_cid10, class) |>
    #     dplyr::summarise(
    #       porc_obitos = round(sum(porc_obitos), 1)
    #     ) |>
    #     dplyr::ungroup() |>
    #     dplyr::mutate(
    #       grupo_cid10 = factor(
    #         grupo_cid10,
    #         levels = c(
    #           "Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
    #           "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
    #           "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas"
    #         )
    #       ),
    #       ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
    #     )
    # })

    data_plot_evitaveis_fetal2 <- reactive({

      if (length(input$momento_obito_fetal_evitaveis2) == 3) {
        data_plot_evitaveis_fetal2_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_fetal") & dplyr::contains("2") & #[eee]
              !dplyr::matches("antes|durante|sem_info_parto") &
              dplyr::matches(paste(input$faixa_peso_fetal_evitaveis2, collapse = "|"))
          )
      } else {
        data_plot_evitaveis_fetal2_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_fetal") & dplyr::contains("2") &
              dplyr::contains(input$momento_obito_fetal_evitaveis2) &
              dplyr::matches(paste(input$faixa_peso_fetal_evitaveis2, collapse = "|"))
          )
      }

      data_plot_evitaveis_fetal2_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_fetal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::across(dplyr::contains("evitaveis_fetal")), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("evitaveis_fetal")), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("evitaveis_fetal"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_fetal2, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal2)),
            dplyr::case_when(
              grepl("imunoprevencao2", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao2", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto2", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("mal_definidas2", grupo_cid10) ~ "Causas mal definidas",
              grepl("nao_aplica2", grupo_cid10) ~ "Causa básica não se aplica a um óbito fetal",
              grepl("outros2", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_evitaveis_fetal2, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal2)),
            dplyr::case_when(
              grepl("imunoprevencao2", grupo_cid10) ~ "imunoprevencao2",
              grepl("mulher_gestacao2", grupo_cid10) ~ "mulher_gestacao2",
              grepl("parto2", grupo_cid10) ~ "parto2",
              grepl("mal_definidas2", grupo_cid10) ~ "mal_definidas2s",
              grepl("nao_aplica2", grupo_cid10) ~ "nao_aplica2",
              grepl("outros2", grupo_cid10) ~ "outros2",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto", "Imunoprevenção", "Grupos não selecionados",
                                                                   "Causas mal definidas", "Causa básica não se aplica a um óbito fetal", "Causas não claramente evitáveis")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_neonatal <- reactive({

      if (length(input$momento_obito_neonatal_evitaveis) == 3) {
        data_plot_evitaveis_neonatal_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_neonatal") &
              !dplyr::matches("0_dias|1_6_dias") &
              dplyr::matches(paste(input$faixa_peso_neonatal_evitaveis, collapse = "|"))
          )
      } else {
        data_plot_evitaveis_neonatal_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_neonatal") &
              dplyr::contains(input$momento_obito_neonatal_evitaveis) &
              dplyr::matches(paste(input$faixa_peso_neonatal_evitaveis, collapse = "|"))
          )
      }

      data_plot_evitaveis_neonatal_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_neonatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_evitaveis_total = sum(dplyr::across(dplyr::contains("evitaveis_neonatal")), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("evitaveis_neonatal")), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("evitaveis_neonatal"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_neonatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
              grepl("outros", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_evitaveis_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_neonatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "imunoprevencao",
              grepl("mulher_gestacao", grupo_cid10) ~ "mulher_gestacao",
              grepl("parto", grupo_cid10) ~ "parto",
              grepl("recem_nascido", grupo_cid10) ~ "recem_nascido",
              grepl("tratamento", grupo_cid10) ~ "tratamento",
              grepl("saude", grupo_cid10) ~ "saude",
              grepl("mal_definidas", grupo_cid10) ~ "mal_definidas",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Causas não claramente evitáveis")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_perinatal <- reactive({

      if (length(input$momento_obito_perinatal) == 5) {
        data_plot_evitaveis_perinatal_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_perinatal") &
              !dplyr::matches("antes|durante|0_dias|1_6_dias|sem_info") &
              dplyr::matches(paste(input$faixa_peso_perinatal_evitaveis, collapse = "|"))
          )
      } else {
        data_plot_evitaveis_perinatal_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_perinatal") &
              dplyr::contains(input$momento_obito_perinatal_evitaveis) &
              dplyr::matches(paste(input$faixa_peso_perinatal_evitaveis, collapse = "|"))
          )
      }

      data_plot_evitaveis_perinatal_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_perinatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_evitaveis_total = sum(dplyr::across(dplyr::contains("evitaveis_perinatal")), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("evitaveis_perinatal")), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("evitaveis_perinatal"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_perinatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_perinatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
              grepl("outros", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_evitaveis_perinatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_perinatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "imunoprevencao",
              grepl("mulher_gestacao", grupo_cid10) ~ "mulher_gestacao",
              grepl("parto", grupo_cid10) ~ "parto",
              grepl("recem_nascido", grupo_cid10) ~ "recem_nascido",
              grepl("tratamento", grupo_cid10) ~ "tratamento",
              grepl("saude", grupo_cid10) ~ "saude",
              grepl("mal_definidas", grupo_cid10) ~ "mal_definidas",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupo_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Causas não claramente evitáveis")))
    })

    #### Para a comparação selecionada ----------------------------------------
    data_filtrada_comp_aux <- reactive({
      bloco7_distribuicao_cids |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
          if (filtros()$nivel2 == "nacional")
            regiao %in% unique(tabela_aux_municipios$regiao)
          else if (filtros()$nivel2 == "regional")
            regiao == filtros()$regiao2
          else if (filtros()$nivel2 == "estadual")
            uf == filtros()$estado2
          else if (filtros()$nivel2 == "macro")
            macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
          else if(filtros()$nivel2 == "micro")
            r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
          else if(filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano)
    })

    # data_plot_evitaveis_fetal_comp <- reactive({
    #   data_filtrada_comp_aux() |>
    #     dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & !dplyr::contains("2"))| "obitos_fetais_totais"), sum) |>
    #     dplyr::rowwise() |>
    #     dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))))) |>
    #     dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
    #     tidyr::pivot_longer(
    #       cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis)),
    #       names_to = "grupo_cid10",
    #       values_to = "porc_obitos"
    #     ) |>
    #     dplyr::mutate(
    #       grupo_cid10 = ifelse(
    #         (grepl(paste(input$cids_evitaveis_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal)),
    #         dplyr::case_when(
    #           grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
    #           grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
    #           grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
    #           grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
    #           grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
    #           grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
    #           grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
    #           grepl("outros", grupo_cid10) ~ "Causas não claramente evitáveis",
    #         ),
    #         "Grupos não selecionados"
    #       ),
    #       class = dplyr::case_when(
    #         filtros()$nivel2 == "nacional" ~ "Brasil",
    #         filtros()$nivel2 == "regional" ~ filtros()$regiao2,
    #         filtros()$nivel2 == "estadual" ~ filtros()$estado2,
    #         filtros()$nivel2 == "macro" ~ filtros()$macro2,
    #         filtros()$nivel2 == "micro" ~ filtros()$micro2,
    #         filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
    #         filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
    #       )
    #     ) |>
    #     dplyr::ungroup() |>
    #     dplyr::group_by(ano, grupo_cid10, class) |>
    #     dplyr::summarise(
    #       porc_obitos = round(sum(porc_obitos), 1)
    #     ) |>
    #     dplyr::ungroup() |>
    #     dplyr::mutate(
    #       grupo_cid10 = factor(
    #         grupo_cid10,
    #         levels = c(
    #           "Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
    #           "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
    #           "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Causas não claramente evitáveis"
    #         )
    #       ),
    #       ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
    #     )
    # })

    data_plot_evitaveis_fetal_comp2 <- reactive({

      if (length(input$momento_obito_fetal_grupos) == 2) {
        data_plot_evitaveis_fetal2_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_fetal") & dplyr::contains("2") &
              !dplyr::matches("antes|durante|sem_info_parto") &
              dplyr::matches(paste(input$faixa_peso_fetal_evitaveis2, collapse = "|"))
          )
      } else {
        data_plot_evitaveis_fetal2_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_fetal") & dplyr::contains("2") &
              dplyr::contains(input$momento_obito_fetal_evitaveis2) &
              dplyr::matches(paste(input$faixa_peso_fetal_evitaveis2, collapse = "|"))
          )
      }

      data_plot_evitaveis_fetal2_comp_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_fetal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::across(dplyr::contains("evitaveis_fetal")), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("evitaveis_fetal")), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("evitaveis_fetal"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_fetal2, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal2)),
            dplyr::case_when(
              grepl("imunoprevencao2", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao2", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto2", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("mal_definidas2", grupo_cid10) ~ "Causas mal definidas",
              grepl("nao_aplica2", grupo_cid10) ~ "Causa básica não se aplica a um óbito fetal",
              grepl("outros2", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_evitaveis_fetal2, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal2)),
            dplyr::case_when(
              grepl("imunoprevencao2", grupo_cid10) ~ "imunoprevencao2",
              grepl("mulher_gestacao2", grupo_cid10) ~ "mulher_gestacao2",
              grepl("parto2", grupo_cid10) ~ "parto2",
              grepl("mal_definidas2", grupo_cid10) ~ "mal_definidas2",
              grepl("nao_aplica2", grupo_cid10) ~ "nao_aplica2",
              grepl("outros2", grupo_cid10) ~ "outros2",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto", "Imunoprevenção", "Grupos não selecionados",
                                                                   "Causas mal definidas", "Causa básica não se aplica a um óbito fetal", "Causas não claramente evitáveis")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_neonatal_comp <- reactive({

      if (length(input$momento_obito_neonatal_evitaveis) == 3) {
        data_plot_evitaveis_neonatal_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_neonatal") &
              !dplyr::matches("0_dias|1_6_dias|7_27_dias") &
              dplyr::matches(paste(input$faixa_peso_neonatal_evitaveis, collapse = "|"))
          )
      } else {
        data_plot_evitaveis_neonatal_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_neonatal") &
              dplyr::contains(input$momento_obito_neonatal_evitaveis) &
              dplyr::matches(paste(input$faixa_peso_neonatal_evitaveis, collapse = "|"))
          )
      }

      data_plot_evitaveis_neonatal_comp_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_neonatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_evitaveis_total = sum(dplyr::across(dplyr::contains("evitaveis_neonatal")), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("evitaveis_neonatal")), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("evitaveis_neonatal"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_neonatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
              grepl("outros", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_evitaveis_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_neonatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "imunoprevencao",
              grepl("mulher_gestacao", grupo_cid10) ~ "mulher_gestacao",
              grepl("parto", grupo_cid10) ~ "parto",
              grepl("recem_nascido", grupo_cid10) ~ "recem_nascido",
              grepl("tratamento", grupo_cid10) ~ "tratamento",
              grepl("saude", grupo_cid10) ~ "saude",
              grepl("mal_definidas", grupo_cid10) ~ "mal_definidas",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Causas não claramente evitáveis")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_perinatal_comp <- reactive({

      if (length(input$momento_obito_perinatal) == 5) {
        data_plot_evitaveis_perinatal_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_perinatal") &
              !dplyr::matches("antes|durante|0_dias|1_6_dias|sem_info") &
              dplyr::matches(paste(input$faixa_peso_perinatal_evitaveis, collapse = "|"))
          )
      } else {
        data_plot_evitaveis_perinatal_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("evitaveis_perinatal") &
              dplyr::contains(input$momento_obito_perinatal_evitaveis) &
              dplyr::matches(paste(input$faixa_peso_perinatal_evitaveis, collapse = "|"))
          )
      }

      data_plot_evitaveis_perinatal_comp_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_perinatal")), sum) |>
        # dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_perinatal") | "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_evitaveis_total = sum(dplyr::across(dplyr::contains("evitaveis_perinatal"))), na.rm=T) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("evitaveis_perinatal")), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("evitaveis_perinatal"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_perinatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_perinatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
              grepl("outros", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_evitaveis_perinatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_perinatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "imunoprevencao",
              grepl("mulher_gestacao", grupo_cid10) ~ "mulher_gestacao",
              grepl("parto", grupo_cid10) ~ "parto",
              grepl("recem_nascido", grupo_cid10) ~ "recem_nascido",
              grepl("tratamento", grupo_cid10) ~ "tratamento",
              grepl("saude", grupo_cid10) ~ "saude",
              grepl("mal_definidas", grupo_cid10) ~ "mal_definidas",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Causas não claramente evitáveis")))
    })

    #### Para a referência selecionada ----------------------------------------
    # data_plot_evitaveis_fetal_referencia <- reactive({
    #   bloco7_distribuicao_cids |>
    #     dplyr::filter(
    #       ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #     ) |>
    #     dplyr::group_by(ano) |>
    #     dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & !dplyr::contains("2"))), sum) |>
    #     dplyr::rowwise() |>
    #     dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))))) |>
    #     dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
    #     tidyr::pivot_longer(
    #       cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis)),
    #       names_to = "grupo_cid10",
    #       values_to = "br_porc_obitos"
    #     ) |>
    #     dplyr::mutate(
    #       grupo_cid10 = ifelse(
    #         (grepl(paste(input$cids_evitaveis_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal)),
    #         dplyr::case_when(
    #           grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
    #           grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
    #           grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
    #           grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
    #           grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
    #           grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
    #           grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
    #           grepl("outros", grupo_cid10) ~ "Demais causas",
    #         ),
    #         "Grupos não selecionados"
    #       )
    #     ) |>
    #     dplyr::ungroup() |>
    #     dplyr::group_by(ano, grupo_cid10) |>
    #     dplyr::summarise(
    #       br_porc_obitos = round(sum(br_porc_obitos), 1)
    #     ) |>
    #     dplyr::ungroup()|>
    #     dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
    #                                                                "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
    #                                                                "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas")),
    #                   ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    #
    # })

    data_plot_evitaveis_fetal_referencia2 <- reactive({

      if (length(input$momento_obito_fetal_evitaveis2) == 3) {
        data_plot_evitaveis_fetal2_referencia_aux <- bloco7_distribuicao_cids |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(ano) |>
          dplyr::select(
            dplyr::contains("evitaveis_fetal") & dplyr::contains("2") &
              !dplyr::matches("antes|durante|sem_info_parto") &
              dplyr::matches(paste(input$faixa_peso_fetal_evitaveis2, collapse = "|"))
          )
      } else {
        data_plot_evitaveis_fetal2_referencia_aux <- bloco7_distribuicao_cids |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(ano) |>
          dplyr::select(
            dplyr::contains("evitaveis_fetal") & dplyr::contains("2") &
              dplyr::contains(input$momento_obito_fetal_evitaveis2) &
              dplyr::matches(paste(input$faixa_peso_fetal_evitaveis2, collapse = "|"))
          )
      }

      data_plot_evitaveis_fetal2_referencia_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_fetal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::across(dplyr::contains("evitaveis_fetal")), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("evitaveis_fetal")), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("evitaveis_fetal"),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_fetal2, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal2)),
            dplyr::case_when(
              grepl("imunoprevencao2", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao2", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto2", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("mal_definidas2", grupo_cid10) ~ "Causas mal definidas",
              grepl("nao_aplica2", grupo_cid10) ~ "Causa básica não se aplica a um óbito fetal",
              grepl("outros2", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10) |>
        dplyr::summarise(
          br_porc_obitos = round(sum(br_porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto", "Imunoprevenção", "Grupos não selecionados",
                                                                   "Causas mal definidas", "Causa básica não se aplica a um óbito fetal", "Causas não claramente evitáveis")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_neonatal_referencia <- reactive({

      if (length(input$momento_obito_neonatal_evitaveis) == 3) {
      data_plot_evitaveis_neonatal_referencia_aux <- bloco7_distribuicao_cids |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::select(
          dplyr::contains("evitaveis_neonatal") &
            !dplyr::matches("0_dias|1_6_dias") &
            dplyr::matches(paste(input$faixa_peso_neonatal_evitaveis, collapse = "|"))
        )
    } else {
      data_plot_evitaveis_neonatal_referencia_aux <- bloco7_distribuicao_cids |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::select(
          dplyr::contains("evitaveis_neonatal") &
            dplyr::contains(input$momento_obito_neonatal_evitaveis) &
            dplyr::matches(paste(input$faixa_peso_neonatal_evitaveis, collapse = "|"))
        )
    }

      data_plot_evitaveis_neonatal_referencia_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_neonatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_evitaveis_total = sum(dplyr::across(dplyr::contains("evitaveis_neonatal")), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("evitaveis_neonatal")), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("evitaveis_neonatal"),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_neonatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
              grepl("outros", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10) |>
        dplyr::summarise(
          br_porc_obitos = round(sum(br_porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Causas não claramente evitáveis")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_perinatal_referencia <- reactive({

      if (length(input$momento_obito_perinatal_evitaveis) == 5) {
        data_plot_evitaveis_perinatal_referencia_aux <- bloco7_distribuicao_cids |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(ano) |>
          dplyr::select(
            dplyr::contains("evitaveis_perinatal") &
              !dplyr::matches("antes|durante|0_dias|1_6_dias|sem_info") &
              dplyr::matches(paste(input$faixa_peso_perinatal_evitaveis, collapse = "|"))
          )
      } else {
        data_plot_evitaveis_perinatal_referencia_aux <- bloco7_distribuicao_cids |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(ano) |>
          dplyr::select(
            dplyr::contains("evitaveis_perinatal") &
              dplyr::contains(input$momento_obito_perinatal_evitaveis) &
              dplyr::matches(paste(input$faixa_peso_perinatal_evitaveis, collapse = "|"))
          )
      }

      data_plot_evitaveis_perinatal_referencia_aux |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_perinatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis))), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis))), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis)),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_perinatal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_perinatal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
              grepl("outros", grupo_cid10) ~ "Causas não claramente evitáveis",
            ),
            "Grupos não selecionados"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10) |>
        dplyr::summarise(
          br_porc_obitos = round(sum(br_porc_obitos, na.rm=T), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Causas não claramente evitáveis")))
    })

    #### Juntando as informações da localidade/comparação com a referência ----
    # data_plot_evitaveis_fetal_completo <- reactive({
    #   validate(
    #     need(
    #       nrow(data_plot_evitaveis_fetal()) != 0,
    #       "Não existem ocorrências de óbitos fetais segundo análise de evitabilidade para a localidade, período e grupos CID-10 selecionados."
    #     )
    #   )
    #   dplyr::full_join(data_plot_evitaveis_fetal(), data_plot_evitaveis_fetal_referencia())
    # })
    #
    # data_plot_evitaveis_fetal_comp_completo <- reactive({
    #   validate(
    #     need(
    #       nrow(data_plot_evitaveis_fetal_comp()) != 0,
    #       "Não existem ocorrências de óbitos fetais segundo análise de evitabilidade para a localidade de comparação, período e grupos CID-10 selecionados."
    #     )
    #   )
    #   dplyr::full_join(data_plot_evitaveis_fetal_comp(), data_plot_evitaveis_fetal_referencia())
    # })


    data_plot_evitaveis_fetal_completo2 <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_fetal2()) != 0,
          "Não existem ocorrências de óbitos fetais segundo análise de evitabilidade para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_fetal2(), data_plot_evitaveis_fetal_referencia2())
    })

    data_plot_evitaveis_fetal_comp_completo2 <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_fetal_comp2()) != 0,
          "Não existem ocorrências de óbitos fetais segundo análise de evitabilidade para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_fetal_comp2(), data_plot_evitaveis_fetal_referencia2())
    })


    data_plot_evitaveis_neonatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_neonatal()) != 0,
          "Não existem ocorrências de óbitos neonatais segundo análise de evitabilidade para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_neonatal(), data_plot_evitaveis_neonatal_referencia())
    })

    data_plot_evitaveis_neonatal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_neonatal_comp()) != 0,
          "Não existem ocorrências de óbitos neonatais segundo análise de evitabilidade para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_neonatal_comp(), data_plot_evitaveis_neonatal_referencia())
    })


    data_plot_evitaveis_perinatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_perinatal()) != 0,
          "Não existem ocorrências de óbitos perinatais segundo análise de evitabilidade para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_perinatal(), data_plot_evitaveis_perinatal_referencia())
    })

    data_plot_evitaveis_perinatal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_perinatal_comp()) != 0,
          "Não existem ocorrências de óbitos perinatais segundo análise de evitabilidade para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_perinatal_comp(), data_plot_evitaveis_perinatal_referencia())
    })

    ### Caixas para obitos potencialmente evitaveis ############################ [zzz]

    ### Dados Fetal, neonatal e perinatal

    data_filtrada_evitaveis_aux <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo_inputs <- ""
      } else {
        if (input$tabset1 == "tabpanel_neonatal") {
          req(input$localidade_resumo_neonat)
          if (input$localidade_resumo_neonat == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else if (input$tabset1 == "tabpanel_fetal") {
          req(input$localidade_resumo_fetal)
          if (input$localidade_resumo_fetal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else if (input$tabset1 == "tabpanel_perinatal"){
          req(input$localidade_resumo_perinatal)
          if (input$localidade_resumo_perinatal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else {
          req(input$localidade_resumo_morbidade_neonatal)
          if (input$localidade_resumo_morbidade_neonatal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        }
      }
      bloco7_distribuicao_cids |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (nivel_selecionado() == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (nivel_selecionado() == "regional")
            regiao == filtros()[[paste0("regiao", sufixo_inputs)]]
          else if (nivel_selecionado() == "estadual")
            uf == filtros()[[paste0("estado", sufixo_inputs)]]
          else if (nivel_selecionado() == "macro")
            macro_r_saude == filtros()[[paste0("macro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_macro", sufixo_inputs)]]
          else if(nivel_selecionado() == "micro")
            r_saude == filtros()[[paste0("micro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_micro", sufixo_inputs)]]
          else if(nivel_selecionado() == "municipal")
            municipio == filtros()[[paste0("municipio", sufixo_inputs)]] & uf == filtros()[[paste0("estado_municipio", sufixo_inputs)]]
          else if (nivel_selecionado() == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        )
    })

    ##### Dados caixas
    bloco7_evitaveis_resumo_aux <- reactive({
      data_filtrada_evitaveis_aux() |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & dplyr::contains("2")) | (dplyr::contains("evitaveis_neonatal")) |
                                          (dplyr::contains("evitaveis_perinatal")) | "obitos_fetais_totais" | "obitos_neonatais_totais" |
                                          "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante", "evitaveis_fetal_sem_info_parto"))))),
          obitos_neonatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias"))))),
          obitos_perinatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias", "evitaveis_perinatal_sem_info")))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante", "evitaveis_fetal_sem_info_parto")))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias", "evitaveis_perinatal_sem_info")))), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        dplyr::select(dplyr::contains(c("outros", "mal_definidas"))) |>
        dplyr::mutate(
          porc_evitavel_fetal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante", "evitaveis_fetal_sem_info_parto")))), na.rm=T),
          porc_evitavel_neonatal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), na.rm=T),
          porc_evitavel_perinatal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias", "evitaveis_perinatal_sem_info")))), na.rm=T)
        )
    })

    bloco7_evitaveis_resumo <- eventReactive(
      c(filtros()$pesquisar, input$tabset1, input$tabset2, input$tabset3, input$tabset4, input$localidade_resumo_fetal,
        input$localidade_resumo_neonat, input$localidade_resumo_perinatal),
      bloco7_evitaveis_resumo_aux(),
      ignoreNULL = FALSE
    )

    ### Dados para a referencia nacional
    bloco7_evitaveis_resumo_comp_aux <- reactive({
      bloco7_distribuicao_cids |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & dplyr::contains("2")) | (dplyr::contains("evitaveis_neonatal")) |
                                          (dplyr::contains("evitaveis_perinatal")) | "obitos_fetais_totais" | "obitos_neonatais_totais" |
                                          "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante", "evitaveis_fetal_sem_info_parto"))))),
          obitos_neonatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias"))))),
          obitos_perinatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias", "evitaveis_perinatal_sem_info")))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante", "evitaveis_fetal_sem_info_parto")))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias", "evitaveis_perinatal_sem_info")))), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        dplyr::select(dplyr::contains(c("outros", "mal_definidas"))) |>
        dplyr::mutate(
          porc_evitavel_fetal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante", "evitaveis_fetal_sem_info_parto")))), na.rm = T),
          porc_evitavel_neonatal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), na.rm = T),
          porc_evitavel_perinatal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias", "evitaveis_perinatal_sem_info")))), na.rm = T)
        )
    })

    bloco7_evitaveis_resumo_comp <- eventReactive(
      c(filtros()$pesquisar, input$momento_obito_fetal_evitaveis2, input$momento_obito_neonatal_evitaveis, input$momento_obito_perinatal_evitaveis),
      bloco7_evitaveis_resumo_comp_aux(),
      ignoreNULL = FALSE
    )

    ### Caixas paras as 3 maiores causas de obitos ############################

    ### Aba fetal

    bloco7_principais_obito_fetal_aux <- reactive({
      data_filtrada_evitaveis_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("fetal_grupos") | "obitos_fetais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = c("fetal_grupos_antes","fetal_grupos_durante")))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = c("fetal_grupos_antes","fetal_grupos_durante")))), ~ (. / obitos_fetais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = c("fetal_grupos_antes","fetal_grupos_durante"))),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, porc_obitos) |>
        dplyr::mutate(
          grupo = dplyr::case_when(
            grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
            grepl("infeccoes", grupo_cid10) ~ "Infecções",
            grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
            grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
            grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
            grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
            grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
            grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
            grepl("outros", grupo_cid10) ~ "Demais causas"
          ),
          porc_obitos = round(porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })

    bloco7_principais_obito_fetal <- eventReactive(
      c(filtros()$pesquisar, input$tabset1, input$tabset2, input$tabset3, input$tabset4, input$localidade_resumo_fetal),
      bloco7_principais_obito_fetal_aux(),
      ignoreNULL = FALSE
    )

    ### Aba perinatal

    bloco7_principais_obito_perinatal_aux <- reactive({
      data_filtrada_evitaveis_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("perinatal_grupos") | "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = c("perinatal_grupos_antes", "perinatal_grupos_durante","perinatal_grupos_0_dias", "perinatal_grupos_1_6_dias")))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = c("perinatal_grupos_antes", "perinatal_grupos_durante","perinatal_grupos_0_dias", "perinatal_grupos_1_6_dias")))), ~ (. / obitos_perinatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input =  c("perinatal_grupos_antes", "perinatal_grupos_durante","perinatal_grupos_0_dias", "perinatal_grupos_1_6_dias"))),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, porc_obitos) |>
        dplyr::mutate(
          grupo = dplyr::case_when(
            grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
            grepl("infeccoes", grupo_cid10) ~ "Infecções",
            grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
            grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
            grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
            grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
            grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
            grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
            grepl("outros", grupo_cid10) ~ "Demais causas"
          ),
          porc_obitos = round(porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })

    bloco7_principais_obito_perinatal <- eventReactive(
      c(filtros()$pesquisar, input$tabset1, input$tabset2, input$tabset3, input$tabset4, input$localidade_resumo_perinatal),
      bloco7_principais_obito_perinatal_aux(),
      ignoreNULL = FALSE
    )

    ### Aba neonatal

    bloco7_principais_obito_neonatal_aux <- reactive({
      data_filtrada_evitaveis_aux() |>
        dplyr::select(
          dplyr::contains("neonatal_grupos") &
            !dplyr::matches("0_dias|1_6_dias|7_27_dias") &
            dplyr::matches(paste(input$faixa_peso_neonatal_grupos, collapse = "|"))
        ) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::across(dplyr::contains("neonatal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("neonatal_grupos")), ~ (. / obitos_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("neonatal_grupos"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, porc_obitos) |>
        dplyr::mutate(
          grupo = dplyr::case_when(
            grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
            grepl("infeccoes", grupo_cid10) ~ "Infecções",
            grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
            grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
            grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
            grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
            grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
            grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
            grepl("outros", grupo_cid10) ~ "Demais causas"
          ),
          porc_obitos = round(porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })

    bloco7_principais_obito_neonatal <- eventReactive(
      c(filtros()$pesquisar, input$tabset1, input$tabset2, input$tabset3, input$tabset4, input$localidade_resumo_neonat),
      bloco7_principais_obito_neonatal_aux(),
      ignoreNULL = FALSE
    )

    ### Aba morbidade neonatal

    data_filtrada_morbidade_aux2 <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo_inputs <- ""
      } else {
        if (input$tabset1 == "tabpanel_neonatal") {
          req(input$localidade_resumo_neonat)
          if (input$localidade_resumo_neonat == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else if (input$tabset1 == "tabpanel_fetal") {
          req(input$localidade_resumo_fetal)
          if (input$localidade_resumo_fetal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else if (input$tabset1 == "tabpanel_perinatal"){
          req(input$localidade_resumo_perinatal)
          if (input$localidade_resumo_perinatal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        } else {
          req(input$localidade_resumo_morbidade_neonatal)
          if (input$localidade_resumo_morbidade_neonatal == "escolha1") {
            sufixo_inputs <- ""
          } else {
            sufixo_inputs <- "2"
          }
        }
      }
      bloco7_dist_morbidade |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (nivel_selecionado() == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (nivel_selecionado() == "regional")
            regiao == filtros()[[paste0("regiao", sufixo_inputs)]]
          else if (nivel_selecionado() == "estadual")
            uf == filtros()[[paste0("estado", sufixo_inputs)]]
          else if (nivel_selecionado() == "macro")
            macro_r_saude == filtros()[[paste0("macro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_macro", sufixo_inputs)]]
          else if(nivel_selecionado() == "micro")
            r_saude == filtros()[[paste0("micro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_micro", sufixo_inputs)]]
          else if(nivel_selecionado() == "municipal")
            municipio == filtros()[[paste0("municipio", sufixo_inputs)]] & uf == filtros()[[paste0("estado_municipio", sufixo_inputs)]]
          else if (nivel_selecionado() == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        )
    })

    bloco7_principais_internacoes_neonatal_aux <- reactive({
      data_filtrada_morbidade_aux2() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = c(
          "morbidade_neonatal_grupos_0_dias",
          "morbidade_neonatal_grupos_1_6_dias",
          "morbidade_neonatal_grupos_7_27_dias"
        )))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_internacoes(input = c(
          "morbidade_neonatal_grupos_0_dias",
          "morbidade_neonatal_grupos_1_6_dias",
          "morbidade_neonatal_grupos_7_27_dias"
        )))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_internacoes(input = c(
            "morbidade_neonatal_grupos_0_dias",
            "morbidade_neonatal_grupos_1_6_dias",
            "morbidade_neonatal_grupos_7_27_dias"
          ))),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, br_porc_obitos) |>
        dplyr::mutate(
          grupo = dplyr::case_when(
            grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
            grepl("infeccoes", grupo_cid10) ~ "Infecções",
            grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
            grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
            grepl("afeccoes_respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
            grepl("fatores_maternos", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
            grepl("afeccoes_perinatal", grupo_cid10) ~ "Afecções originais no período perinatal",
            grepl("mal_definidas", grupo_cid10) ~ "Mal definidas",
            grepl("ictericia", grupo_cid10) ~ "Icterícia neonatal",
            grepl("endocrinos", grupo_cid10) ~ "Transtornos endócrinos e metabólicos",
            grepl("alimentacao", grupo_cid10) ~ "Problemas de alimentação do recém-nascido",
            grepl("cardiacos_perinatal", grupo_cid10) ~ "Transtornos cardíacos do período perinatal",
            grepl("outros", grupo_cid10) ~ "Demais causas"
          ),
          porc_obitos = round(br_porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })

    bloco7_principais_internacoes_neonatal <- eventReactive(
      c(filtros()$pesquisar, input$tabset1, input$tabset2, input$tabset3, input$tabset4, input$localidade_resumo_morbidade_neonatal),
      bloco7_principais_internacoes_neonatal_aux(),
      ignoreNULL = FALSE
    )


    ### Para os gráficos de grupos de causas ----------------------------------
    #### Para a localidade selecionada ----------------------------------------
    data_plot_grupos_fetal <- reactive({

      if (length(input$momento_obito_fetal_grupos) == 3) {
        data_plot_grupos_fetal_aux <- data_filtrada_aux() |>
            dplyr::select(
              dplyr::contains("fetal_grupos") &
                !dplyr::matches("antes|durante|sem_info_parto") &
                dplyr::matches(paste(input$faixa_peso_fetal_grupos, collapse = "|"))
            )
      } else {
        data_plot_grupos_fetal_aux <- data_filtrada_aux() |>
            dplyr::select(
              dplyr::contains("fetal_grupos") &
                dplyr::contains(input$momento_obito_fetal_grupos) &
                dplyr::matches(paste(input$faixa_peso_fetal_grupos, collapse = "|"))
            )
      }

      data_plot_grupos_fetal_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("fetal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_grupos_total = sum(dplyr::across(dplyr::contains("fetal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("fetal_grupos")), ~ (. / obitos_fetais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("fetal_grupos"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "infeccoes",
              grepl("asfixia", grupo_cid10) ~ "asfixia",
              grepl("ma_formacao", grupo_cid10) ~ "ma_formacao",
              grepl("respiratorias", grupo_cid10) ~ "respiratorias",
              grepl("gravidez", grupo_cid10) ~ "gravidez",
              grepl("afeccoes", grupo_cid10) ~ "afeccoes",
              grepl("mal_definida", grupo_cid10) ~ "mal_definida",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Anomalia congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_neonatal <- reactive({

      if (length(input$momento_obito_neonatal_grupos) == 3) {
        data_plot_grupos_neonatal_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("neonatal_grupos") &
              !dplyr::matches("0_dias|1_6_dias|7_27_dias") &
              dplyr::matches(paste(input$faixa_peso_neonatal_grupos, collapse = "|"))
          )
      } else {
        data_plot_grupos_neonatal_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("neonatal_grupos") &
              dplyr::matches(paste(input$momento_obito_neonatal_grupos, collapse = "|")) &
              dplyr::matches(paste(input$faixa_peso_neonatal_grupos, collapse = "|"))
          )
      }

      data_plot_grupos_neonatal_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::across(dplyr::contains("neonatal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("neonatal_grupos")), ~ (. / obitos_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("neonatal_grupos"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_neonatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "infeccoes",
              grepl("asfixia", grupo_cid10) ~ "asfixia",
              grepl("ma_formacao", grupo_cid10) ~ "ma_formacao",
              grepl("respiratorias", grupo_cid10) ~ "respiratorias",
              grepl("gravidez", grupo_cid10) ~ "gravidez",
              grepl("afeccoes", grupo_cid10) ~ "afeccoes",
              grepl("mal_definida", grupo_cid10) ~ "mal_definida",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Anomalia congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_perinatal <- reactive({

      if (length(input$momento_obito_perinatal_grupos) == 5) {
        data_plot_grupos_perinatal_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("perinatal_grupos") &
              !dplyr::matches("antes|durante|0_dias|1_6_dias|sem_info") &
              dplyr::matches(paste(input$faixa_peso_perinatal_grupos, collapse = "|"))
          )
      } else {
        data_plot_grupos_perinatal_aux <- data_filtrada_aux() |>
          dplyr::select(
            dplyr::contains("perinatal_grupos") &
              dplyr::contains(input$momento_obito_perinatal_grupos) &
              dplyr::matches(paste(input$faixa_peso_perinatal_grupos, collapse = "|"))
          )
      }

      data_plot_grupos_perinatal_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("perinatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_grupos_total = sum(dplyr::c_across(dplyr::contains("perinatal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("perinatal_grupos")), ~ (. / obitos_perinatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("perinatal_grupos"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_perinatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_perinatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "infeccoes",
              grepl("asfixia", grupo_cid10) ~ "asfixia",
              grepl("ma_formacao", grupo_cid10) ~ "ma_formacao",
              grepl("respiratorias", grupo_cid10) ~ "respiratorias",
              grepl("gravidez", grupo_cid10) ~ "gravidez",
              grepl("afeccoes", grupo_cid10) ~ "afeccoes",
              grepl("mal_definida", grupo_cid10) ~ "mal_definida",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia", "Anomalia congênita", "Prematuridade",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    #### Para a comparação selecionada ----------------------------------------
    data_plot_grupos_fetal_comp <- reactive({

      if (length(input$momento_obito_fetal_grupos) == 3) {
        data_plot_grupos_fetal_comp_aux <- data_filtrada_comp_aux() |>
            dplyr::select(
              dplyr::contains("fetal_grupos") &
                !dplyr::matches("antes|durante|sem_info_parto") &
                dplyr::matches(paste(input$faixa_peso_fetal_grupos, collapse = "|"))
            )
      } else {
        data_plot_grupos_fetal_comp_aux <- data_filtrada_comp_aux() |>
            dplyr::select(
              dplyr::contains("fetal_grupos") &
                dplyr::contains(input$momento_obito_fetal_grupos) &
                dplyr::matches(paste(input$faixa_peso_fetal_grupos, collapse = "|"))
            )
      }

      data_plot_grupos_fetal_comp_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("fetal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_grupos_total = sum(dplyr::across(dplyr::contains("fetal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("fetal_grupos")), ~ (. / obitos_fetais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("fetal_grupos"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "infeccoes",
              grepl("asfixia", grupo_cid10) ~ "asfixia",
              grepl("ma_formacao", grupo_cid10) ~ "ma_formacao",
              grepl("respiratorias", grupo_cid10) ~ "respiratorias",
              grepl("gravidez", grupo_cid10) ~ "gravidez",
              grepl("afeccoes", grupo_cid10) ~ "afeccoes",
              grepl("mal_definida", grupo_cid10) ~ "mal_definida",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10,
                                           levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Anomalia congênita",
                                                      "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                      "Afecções originais no período perinatal", "Mal definidas",
                                                      "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_neonatal_comp <- reactive({

      if (length(input$momento_obito_neonatal_grupos) == 3) {
        data_plot_grupos_neonatal_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("neonatal_grupos") &
              !dplyr::matches("0_dias|1_6_dias|7_27_dias") &
              dplyr::matches(paste(input$faixa_peso_neonatal_grupos, collapse = "|"))
          )
      } else {
        data_plot_grupos_neonatal_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("neonatal_grupos") &
              dplyr::contains(input$momento_obito_neonatal_grupos) &
              dplyr::matches(paste(input$faixa_peso_neonatal_grupos, collapse = "|"))
          )
      }

      data_plot_grupos_neonatal_comp_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::across(dplyr::contains("neonatal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("neonatal_grupos")), ~ (. / obitos_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("neonatal_grupos"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_neonatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "infeccoes",
              grepl("asfixia", grupo_cid10) ~ "asfixia",
              grepl("ma_formacao", grupo_cid10) ~ "ma_formacao",
              grepl("respiratorias", grupo_cid10) ~ "respiratorias",
              grepl("gravidez", grupo_cid10) ~ "gravidez",
              grepl("afeccoes", grupo_cid10) ~ "afeccoes",
              grepl("mal_definida", grupo_cid10) ~ "mal_definida",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10,
                                           levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Anomalia congênita",
                                                      "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                      "Afecções originais no período perinatal", "Mal definidas",
                                                      "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_perinatal_comp <- reactive({

      if (length(input$momento_obito_perinatal_grupos) == 5) {
        data_plot_grupos_perinatal_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("perinatal_grupos") &
              !dplyr::matches("antes|durante|0_dias|1_6_dias|sem_info") &
              dplyr::matches(paste(input$faixa_peso_perinatal_grupos, collapse = "|"))
          )
      } else {
        data_plot_grupos_perinatal_comp_aux <- data_filtrada_comp_aux() |>
          dplyr::select(
            dplyr::contains("perinatal_grupos") &
              dplyr::contains(input$momento_obito_perinatal_grupos) &
              dplyr::matches(paste(input$faixa_peso_perinatal_grupos, collapse = "|"))
          )
      }

      data_plot_grupos_perinatal_comp_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("perinatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_grupos_total = sum(dplyr::c_across(dplyr::contains("perinatal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("perinatal_grupos")), ~ (. / obitos_perinatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("perinatal_grupos"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_perinatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_perinatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          grupo_cid10_aux = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "infeccoes",
              grepl("asfixia", grupo_cid10) ~ "asfixia",
              grepl("ma_formacao", grupo_cid10) ~ "ma_formacao",
              grepl("respiratorias", grupo_cid10) ~ "respiratorias",
              grepl("gravidez", grupo_cid10) ~ "gravidez",
              grepl("afeccoes", grupo_cid10) ~ "afeccoes",
              grepl("mal_definida", grupo_cid10) ~ "mal_definida",
              grepl("outros", grupo_cid10) ~ "outros",
            ),
            "grupos_nao_selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel2 == "nacional" ~ "Brasil",
            filtros()$nivel2 == "regional" ~ filtros()$regiao2,
            filtros()$nivel2 == "estadual" ~ filtros()$estado2,
            filtros()$nivel2 == "macro" ~ filtros()$macro2,
            filtros()$nivel2 == "micro" ~ filtros()$micro2,
            filtros()$nivel2 == "municipal" ~ filtros()$municipio2,
            filtros()$nivel2 == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, grupo_cid10_aux, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia", "Anomalia congênita", "Prematuridade",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    #### Para a referência ----------------------------------------------------
    data_plot_grupos_fetal_referencia <- reactive({

      if (length(input$momento_obito_fetal_grupos) == 3) {
        data_plot_grupos_fetal_referencia_aux <-
          bloco7_distribuicao_cids |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(ano) |>
            dplyr::select(
              dplyr::contains("fetal_grupos") &
                !dplyr::matches("antes|durante|sem_info_parto") &
                dplyr::matches(paste(input$faixa_peso_fetal_grupos, collapse = "|"))
            )

      } else {
        data_plot_grupos_fetal_referencia_aux <-
          bloco7_distribuicao_cids |>
            dplyr::filter(
              ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
            ) |>
            dplyr::group_by(ano) |>
            dplyr::select(
              dplyr::contains("fetal_grupos") &
                dplyr::contains(input$momento_obito_fetal_grupos) &
                dplyr::matches(paste(input$faixa_peso_fetal_grupos, collapse = "|"))
            )

      }

      data_plot_grupos_fetal_referencia_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("fetal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_grupos_total = sum(dplyr::across(dplyr::contains("fetal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("fetal_grupos")), ~ (. / obitos_fetais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("fetal_grupos"),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_fetal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10) |>
        dplyr::summarise(
          br_porc_obitos = round(sum(br_porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Anomalia congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_neonatal_referencia <- reactive({
      if (length(input$momento_obito_neonatal_grupos) == 3) {
        data_plot_grupos_neonatal_referencia_aux <-
          bloco7_distribuicao_cids |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(ano) |>
          dplyr::select(
            (dplyr::contains("neonatal_grupos") &
               !dplyr::matches("0_dias|1_6_dias|7_27_dias") &
               dplyr::matches(paste(input$faixa_peso_neonatal_grupos, collapse = "|"))),
            obitos_neonatais_totais
          )

      } else {
        data_plot_grupos_neonatal_referencia_aux <-
          bloco7_distribuicao_cids |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(ano) |>
          dplyr::select(
            dplyr::contains("neonatal_grupos") &
              dplyr::contains(input$momento_obito_neonatal_grupos) &
              dplyr::matches(paste(input$faixa_peso_neonatal_grupos, collapse = "|"))
          )

      }

      data_plot_grupos_neonatal_referencia_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::across(dplyr::contains("neonatal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("neonatal_grupos")), ~ (. / obitos_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("neonatal_grupos"),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_neonatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10) |>
        dplyr::summarise(
          br_porc_obitos = round(sum(br_porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Anomalia congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    # data_plot_grupos_neonatal_referencia <- reactive({
    #   bloco7_distribuicao_cids |>
    #     dplyr::filter(
    #       ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #     ) |>
    #     dplyr::group_by(ano) |>
    #     dplyr::summarise_at(dplyr::vars(dplyr::contains("neonat_grupos")), sum) |>
    #     dplyr::rowwise() |>
    #     dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos))))) |>
    #     dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos))), ~ (. / obitos_neonatais_grupos_total * 100)) |>
    #     tidyr::pivot_longer(
    #       cols = dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos)),
    #       names_to = "grupo_cid10",
    #       values_to = "br_porc_obitos"
    #     ) |>
    #     dplyr::mutate(
    #       grupo_cid10 = ifelse(
    #         (grepl(paste(input$cids_grupos_neonatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_neonatal)),
    #         dplyr::case_when(
    #           grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
    #           grepl("infeccoes", grupo_cid10) ~ "Infecções",
    #           grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
    #           grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
    #           grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
    #           grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
    #           grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período neonatal",
    #           grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
    #           grepl("outros", grupo_cid10) ~ "Demais causas",
    #         ),
    #         "Grupos não selecionados"
    #       )
    #     ) |>
    #     dplyr::ungroup() |>
    #     dplyr::group_by(ano, grupo_cid10) |>
    #     dplyr::summarise(
    #       br_porc_obitos = round(sum(br_porc_obitos), 1)
    #     ) |>
    #     dplyr::ungroup() |>
    #     dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Malformação congênita", "Fatores maternos relacionados à gravidez", "Prematuridade",
    #                                                                "Asfixia/Hipóxia", "Infecções", "Afecções respiratórias do recém-nascido",
    #                                                                "Afecções originais no período neonatal", "Mal definidas",
    #                                                                "Grupos não selecionados", "Demais causas")),
    #                   ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    # })

    data_plot_grupos_perinatal_referencia <- reactive({

      if (length(input$momento_obito_perinatal_grupos) == 5) {
        data_plot_grupos_perinatal_referencia_aux <-
          bloco7_distribuicao_cids |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(ano) |>
          dplyr::select(
            dplyr::contains("perinatal_grupos") &
              !dplyr::matches("antes|durante|0_dias|1_6_dias|sem_info") &
              dplyr::matches(paste(input$faixa_peso_perinatal_grupos, collapse = "|"))
          )

      } else {
        data_plot_grupos_perinatal_referencia_aux <-
          bloco7_distribuicao_cids |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          ) |>
          dplyr::group_by(ano) |>
          dplyr::select(
            dplyr::contains("perinatal_grupos") &
              dplyr::contains(input$momento_obito_perinatal_grupos) &
              dplyr::matches(paste(input$faixa_peso_perinatal_grupos, collapse = "|"))
          )

      }

      data_plot_grupos_perinatal_referencia_aux |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("perinatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_grupos_total = sum(dplyr::c_across(dplyr::contains("perinatal_grupos")))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("perinatal_grupos")), ~ (. / obitos_perinatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("perinatal_grupos"),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_grupos_perinatal, collapse="|"), grupo_cid10) & !is.null(input$cids_grupos_perinatal)),
            dplyr::case_when(
              grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
              grepl("infeccoes", grupo_cid10) ~ "Infecções",
              grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
              grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10) |>
        dplyr::summarise(
          br_porc_obitos = round(sum(br_porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia", "Anomalia congênita", "Prematuridade",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    #### Juntando as informações da localidade/comparação com a referência ----
    data_plot_grupos_fetal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_grupos_fetal()) != 0,
          "Não existem ocorrências de óbitos fetais por grupos de causas para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_grupos_fetal(), data_plot_grupos_fetal_referencia())
    })

    data_plot_grupos_fetal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_grupos_fetal_comp()) != 0,
          "Não existem ocorrências de óbitos fetais por grupos de causas para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_grupos_fetal_comp(), data_plot_grupos_fetal_referencia())
    })


    data_plot_grupos_neonatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_grupos_neonatal()) != 0,
          "Não existem ocorrências de óbitos neonatais por grupos de causas para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_grupos_neonatal(), data_plot_grupos_neonatal_referencia())
    })

    data_plot_grupos_neonatal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_grupos_neonatal_comp()) != 0,
          "Não existem ocorrências de óbitos neonatais por grupos de causas para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_grupos_neonatal_comp(), data_plot_grupos_neonatal_referencia())
    })


    data_plot_grupos_perinatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_grupos_perinatal()) != 0,
          "Não existem ocorrências de óbitos perinatais por grupos de causas para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_grupos_perinatal(), data_plot_grupos_perinatal_referencia())
    })

    data_plot_grupos_perinatal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_grupos_perinatal_comp()) != 0,
          "Não existem ocorrências de óbitos perinatais por grupos de causas para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_grupos_perinatal_comp(), data_plot_grupos_perinatal_referencia())
    })

    ### Para o restante dos gráficos ------------------------------------------
    #### Para a localidade selecionada ----------------------------------------
    data7_aux <- reactive({
      bloco7 |>
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
        cria_indicadores(df_calcs = bloco7_calcs(), df_calcs_dist_bloco7 = bloco7_calcs_dist, bloco  = "bloco7", input = input,  filtros = filtros())
    })

    # Não queremos que os gráficos de números de óbitos e taxas se atualizem quando os inputs dos gráficos de distribuição percentual mudarem
    data7 <- eventReactive(filtros()$pesquisar, data7_aux(), ignoreNULL = FALSE)
    data7_internacoes_vinc_sus <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sus, input$idade_dias_sus), data7_aux(), ignoreNULL = FALSE)
    data7_internacoes_publicos_sih <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sih, input$idade_dias_sih), data7_aux(), ignoreNULL = FALSE)
    data7_internacoes_uti_sih <- eventReactive(c(filtros()$pesquisar, input$local_internacao_uti_sih, input$idade_dias_uti_sih), data7_aux(), ignoreNULL = FALSE)

    # Queremos que os gráficos de distribuição percentual do momento do óbito por faixa de peso se atualizem quando os inputs de faixa de peso mudarem
    data7_plot_dist1 <- eventReactive(
      c(filtros()$pesquisar, input$faixa_peso_dist_moment_obit_fetal, input$faixa_peso_dist_moment_obit_perinat, input$faixa_peso_dist_moment_obit_neonat),
      data7_aux(),
      ignoreNULL = FALSE
    )

    # Queremos que os gráficos de distribuição percentual da faixa de peso por momento do óbito se atualizem quando os inputs de momento do óbito mudarem
    data7_plot_dist2 <- eventReactive(
      c(filtros()$pesquisar, input$momento_obito_dist_peso_fetal, input$momento_obito_dist_peso_perinat, input$momento_obito_dist_peso_neonat),
      data7_aux(),
      ignoreNULL = FALSE
    )

    #### Para a comparação selecionada ----------------------------------------
    data7_comp_aux <- reactive({
      bloco7 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel2 == "regional")
            regiao == filtros()$regiao2
          else if (filtros()$nivel2 == "estadual")
            uf == filtros()$estado2
          else if (filtros()$nivel2 == "macro")
            macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
          else if(filtros()$nivel2 == "micro")
            r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
          else if(filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco7_calcs(), df_calcs_dist_bloco7 = bloco7_calcs_dist, bloco  = "bloco7", input = input,  filtros = filtros(), comp = TRUE)
    })

    # Não queremos que os gráficos de números de óbitos e taxas se atualizem quando os inputs dos gráficos de distribuição percentual mudarem
    data7_comp <- eventReactive(filtros()$pesquisar, data7_comp_aux(), ignoreNULL = FALSE)
    data7_internacoes_vinc_sus_comp <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sus, input$idade_dias_sus), data7_comp_aux(), ignoreNULL = FALSE)
    data7_internacoes_publicos_sih_comp <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sih, input$idade_dias_sih), data7_comp_aux(), ignoreNULL = FALSE)
    data7_internacoes_uti_sih_comp <- eventReactive(c(filtros()$pesquisar, input$local_internacao_uti_sih, input$idade_dias_uti_sih), data7_comp_aux(), ignoreNULL = FALSE)

    # Queremos que os gráficos de distribuição percentual do momento do óbito por faixa de peso se atualizem quando os inputs de faixa de peso mudarem
    data7_plot_dist1_comp <- eventReactive(
      c(filtros()$pesquisar, input$faixa_peso_dist_moment_obit_fetal, input$faixa_peso_dist_moment_obit_perinat, input$faixa_peso_dist_moment_obit_neonat),
      data7_comp_aux(),
      ignoreNULL = FALSE
    )

    # Queremos que os gráficos de distribuição percentual da faixa de peso por momento do óbito se atualizem quando os inputs de momento do óbito mudarem
    data7_plot_dist2_comp <- eventReactive(
      c(filtros()$pesquisar, input$momento_obito_dist_peso_fetal, input$momento_obito_dist_peso_perinat, input$momento_obito_dist_peso_neonat),
      data7_comp_aux(),
      ignoreNULL = FALSE
    )

    #### Para a referência ----------------------------------------------------
    data7_referencia_aux <- reactive({
      bloco7 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco7_calcs(), df_calcs_dist_bloco7 = bloco7_calcs_dist, bloco  = "bloco7", input = input,  filtros = filtros(), referencia = TRUE, adicionar_localidade = FALSE) |>
        dplyr::mutate(
          class = "Média nacional",
          localidade_comparacao = "Média nacional"
        ) |>
        dplyr::rename_with(~paste0("br_", .x), dplyr::contains("moment_obito") | dplyr::contains("dist_peso"))
    })

    data7_referencia_perinatal <- reactive({
      bloco7 |>
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
        dplyr::reframe(
          ano = filtros()$ano2[1]:filtros()$ano2[2],
          taxa_perinatal_oms = round(sum(obitos_6dias)/sum(obitos_27dias) * 5, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data7_referencia_perinatal_comp <- reactive({
      bloco7 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (filtros()$nivel2 == "regional")
            regiao == filtros()$regiao2
          else if (filtros()$nivel2 == "estadual")
            uf == filtros()$estado2
          else if (filtros()$nivel2 == "macro")
            macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
          else if(filtros()$nivel2 == "micro")
            r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
          else if(filtros()$nivel2 == "municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "municipios_semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::reframe(
          ano = filtros()$ano2[1]:filtros()$ano2[2],
          taxa_perinatal_oms = round(sum(obitos_6dias)/sum(obitos_27dias) * 5, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    # Não queremos que os gráficos de números de óbitos e taxas se atualizem quando os inputs dos gráficos de distribuição percentual mudarem
    data7_referencia <- eventReactive(filtros()$pesquisar, data7_referencia_aux(), ignoreNULL = FALSE)
    data7_internacoes_vinc_sus_referencia <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sus, input$idade_dias_sus), data7_referencia_aux(), ignoreNULL = FALSE)
    data7_internacoes_publicos_sih_referencia <- eventReactive(c(filtros()$pesquisar, input$local_internacao_sih, input$idade_dias_sih), data7_referencia_aux(), ignoreNULL = FALSE)
    data7_internacoes_uti_sih_referencia <- eventReactive(c(filtros()$pesquisar, input$local_internacao_uti_sih, input$idade_dias_uti_sih), data7_referencia_aux(), ignoreNULL = FALSE)

    # Queremos que os gráficos de distribuição percentual do momento do óbito por faixa de peso se atualizem quando os inputs de faixa de peso mudarem
    data7_plot_dist1_referencia <- eventReactive(
      c(filtros()$pesquisar, input$faixa_peso_dist_moment_obit_fetal, input$faixa_peso_dist_moment_obit_perinat, input$faixa_peso_dist_moment_obit_neonat),
      data7_referencia_aux(),
      ignoreNULL = FALSE
    )

    # Queremos que os gráficos de distribuição percentual da faixa de peso por momento do óbito se atualizem quando os inputs de momento do óbito mudarem
    data7_plot_dist2_referencia <- eventReactive(
      c(filtros()$pesquisar, input$momento_obito_dist_peso_fetal, input$momento_obito_dist_peso_perinat, input$momento_obito_dist_peso_neonat),
      data7_referencia_aux(),
      ignoreNULL = FALSE
    )

    #### Para os gráficos de barras, juntando a localidade selecionada com a referência ----
    data7_juncao_barras_dist1 <- reactive({
      if (filtros()$comparar == "Não") {
        dplyr::full_join(
          data7_plot_dist1(),
          data7_plot_dist1_referencia() |> dplyr::rename(class2 = class),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]),
            class = ifelse(class == "Brasil (valor de referência)", "Brasil", class)
          )
      } else {
        dplyr::full_join(
          data7_plot_dist1(),
          data7_plot_dist1_comp() |>
            dplyr::rename(localidade_comparacao = class) |>
            dplyr::rename_with(~paste0("br_", .x), dplyr::contains("moment_obito") | dplyr::contains("dist_peso")),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]),
            class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
            localidade_comparacao = ifelse(localidade_comparacao == "Brasil (valor de referência)", "Brasil", localidade_comparacao)
          )
      }
    })

    data7_juncao_barras_dist2 <- reactive({
      if (filtros()$comparar == "Não") {
        dplyr::full_join(
          data7_plot_dist2(),
          data7_plot_dist2_referencia() |> dplyr::rename(class2 = class),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]),
            class = ifelse(class == "Brasil (valor de referência)", "Brasil", class)
          )
      } else {
        dplyr::full_join(
          data7_plot_dist2(),
          data7_plot_dist2_comp() |>
            dplyr::rename(localidade_comparacao = class) |>
            dplyr::rename_with(~paste0("br_", .x), dplyr::contains("moment_obito") | dplyr::contains("dist_peso")),
          by = "ano"
        ) |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]),
            class = ifelse(class == "Brasil (valor de referência)", "Brasil", class),
            localidade_comparacao = ifelse(localidade_comparacao == "Brasil (valor de referência)", "Brasil", localidade_comparacao)
          )
      }
    })


    ## Criando os outputs dos gráficos ----------------------------------------
    ### Para os indicadores de mortalidade fetal ------------------------------
    #### Número de óbitos fetais ----------------------------------------------
    output$plot1_fetal <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais()),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais()),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais()),
          class
        )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      }
    })

    #### Taxa de mortalidade fetal  -------------------------------------------
    output$plot2_fetal <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(taxa_mortalidade_fetal()),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(taxa_mortalidade_fetal()),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(taxa_mortalidade_fetal()),
          class
        )

      # data7_plot_percentil5_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil5_taxa_mortalidade_fetal()),
      #     class
      #   )
      #
      # data7_plot_percentil95_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil95_taxa_mortalidade_fetal()),
      #     class
      #   )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(taxa_mortalidade_fetal() != "taxa_mort_fetal", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0)|>
          highcharter::hc_colors(cols)
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )
        if (filtros()$nivel == "nacional" & taxa_mortalidade_fetal() != "taxa_mort_fetal") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(taxa_mortalidade_fetal() != "taxa_mort_fetal", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(taxa_mortalidade_fetal() != "taxa_mort_fetal", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            name = ifelse(taxa_mortalidade_fetal() != "taxa_mort_fetal", data7_plot_comp_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_comp_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
           ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # ) |>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if ((any(c(filtros()$nivel, filtros()$nivel2) == "nacional") & taxa_mortalidade_fetal() != "taxa_mort_fetal") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(taxa_mortalidade_fetal() != "taxa_mort_fetal", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$plot3_fetal <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "Sem informação",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = faltante_dist_moment_obito_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_moment_obito_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Durante o parto",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = durante_dist_moment_obito_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_durante_dist_moment_obito_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Antes do parto",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = antes_dist_moment_obito_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_antes_dist_moment_obito_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(5, direction = -1)[-c(1, 5)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist1()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$plot4_fetal <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "Sem informação",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = faltante_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Maior ou igual a 2500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = mais_2500_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_mais_2500_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500 a 2499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1500_2499_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1500_2499_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1000 a 1499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1000_1499_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1000_1499_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1000g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = menos_1000_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_menos_1000_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist2()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos fetais segundo análise de evitabilidade -------
    output$plot_evitaveis_fetal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_fetal_completo(),
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
            data = data_plot_evitaveis_fetal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_fetal_comp_completo(),
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
          viridis::magma(length(unique(data_plot_evitaveis_fetal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_evitaveis_fetal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_evitaveis_fetal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos fetais"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos fetais segundo análise de evitabilidade segundo o artigo de Vieira et Al ----
    output$plot_evitaveis_fetal2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_fetal_completo2(),
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
            data = data_plot_evitaveis_fetal_completo2(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_fetal_comp_completo2(),
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
          viridis::magma(length(unique(data_plot_evitaveis_fetal2()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_evitaveis_fetal2()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_evitaveis_fetal2()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos fetais"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos fetais por grupos de causas -------
    output$plot_grupos_fetal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_grupos_fetal_completo(),
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
            data = data_plot_grupos_fetal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data_plot_grupos_fetal_comp_completo(),
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
          viridis::magma(length(unique(data_plot_grupos_fetal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_grupos_fetal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_grupos_fetal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos fetais por grupos de causas"), min = 0, max = 100)
    })

    #### Número de óbitos fetais (definição 2) --------------------------------
    output$plot5_fetal <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais_oms()),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais_oms()),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(numero_obitos_fetais_oms()),
          class
        )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      }
    })

    #### Taxa de mortalidade fetal (definição 2) ------------------------------
    output$plot6_fetal <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(taxa_mortalidade_fetal_oms()),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(taxa_mortalidade_fetal_oms()),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(taxa_mortalidade_fetal_oms()),
          class
        )

      # data7_plot_percentil5_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil5_taxa_mortalidade_fetal_oms()),
      #     class
      #   )
      #
      # data7_plot_percentil95_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil95_taxa_mortalidade_fetal_oms()),
      #     class
      #   )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(taxa_mortalidade_fetal_oms() != "taxa_mort_fetal_oms", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
        #   type = "line",
        #   name = "Referência (percentil 95)",
        #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
        #   dashStyle = "ShortDot",
        #   opacity = 0.8
        # )
        if (filtros()$nivel == "nacional" & taxa_mortalidade_fetal_oms() != "taxa_mort_fetal_oms") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(taxa_mortalidade_fetal_oms() != "taxa_mort_fetal_oms", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(taxa_mortalidade_fetal_oms() != "taxa_mort_fetal_oms", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            name = ifelse(taxa_mortalidade_fetal_oms() != "taxa_mort_fetal_oms", data7_plot_comp_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_comp_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # ) |>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
          #   type = "line",
        #   name = "Referência (percentil 95)",
        #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
        #   dashStyle = "ShortDot",
        #   opacity = 0.8
        # ) |>
        highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if ((any(c(filtros()$nivel, filtros()$nivel2) == "nacional") & taxa_mortalidade_fetal_oms() != "taxa_mort_fetal_oms") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(taxa_mortalidade_fetal_oms() != "taxa_mort_fetal_oms", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Para os indicadores de mortalidade neonatal ---------------------------
    #### Número de óbitos neonatais -------------------------------------------
    output$plot1_neonat <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$obitos_faixa_peso),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$obitos_faixa_peso),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$obitos_faixa_peso),
          class
        )


      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      }
    })

    #### Taxa de mortalidade neonatal por 1000 nascidos vivos -----------------
    output$plot2_neonat <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso),
          class
        )

      # percentil5_faixa_peso <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso == "mort_neonat" ~ "percentil5_mort_neonat",
      #     input$faixa_peso == "mort_neonat_menos1000" ~ "percentil5_mort_neonat_menos1000",
      #     input$faixa_peso == "mort_neonat_1000_1499" ~ "percentil5_mort_neonat_1000_1499",
      #     input$faixa_peso == "mort_neonat_1500_2499" ~ "percentil5_mort_neonat_1500_2499",
      #     input$faixa_peso == "mort_neonat_mais2500" ~ "percentil5_mort_neonat_mais2500"
      #   )
      # })
      #
      # percentil95_faixa_peso <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso == "mort_neonat" ~ "percentil95_mort_neonat",
      #     input$faixa_peso == "mort_neonat_menos1000" ~ "percentil95_mort_neonat_menos1000",
      #     input$faixa_peso == "mort_neonat_1000_1499" ~ "percentil95_mort_neonat_1000_1499",
      #     input$faixa_peso == "mort_neonat_1500_2499" ~ "percentil95_mort_neonat_1500_2499",
      #     input$faixa_peso == "mort_neonat_mais2500" ~ "percentil95_mort_neonat_mais2500"
      #   )
      # })

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso),
          class
        )

      # data7_plot_percentil5_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil5_faixa_peso()),
      #     class
      #   )
      #
      # data7_plot_percentil95_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil95_faixa_peso()),
      #     class
      #   )

      if (filtros()$comparar == "Não") {
       grafico_base <- highcharter::highchart() |>
         highcharter::hc_add_dependency("modules/series-label.js") |>
         highcharter::hc_add_series(
           data = data7_plot_aux,
           type = "line",
           name = ifelse(input$faixa_peso != "mort_neonat", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
           #name = ifelse(data7_plot_aux$class == "Brasil (valor de referência)", gsub(" \\(valor de referência\\)", "", data7_plot_aux$class), data7_plot_aux$class),
           highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
       if (filtros()$nivel == "nacional" & input$faixa_peso != "mort_neonat") {
         grafico_base #|>
           # highcharter::hc_add_series(
           #   data = data7_plot_percentil5_aux,
           #   type = "line",
           #   name = "Referência (percentil 5)",
           #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
           #   dashStyle = "ShortDot",
           #   opacity = 0.8
           # )|>
           # highcharter::hc_add_series(
           #   data = data7_plot_percentil95_aux,
           #   type = "line",
           #   name = "Referência (percentil 95)",
           #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
           #   dashStyle = "ShortDot",
           #   opacity = 0.8
           # )
       } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(input$faixa_peso != "mort_neonat", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            ) #|>
           # highcharter::hc_add_series(
           #   data = data7_plot_percentil5_aux,
           #   type = "line",
           #   name = "Referência (percentil 5)",
           #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
           #   dashStyle = "ShortDot",
           #   opacity = 0.8
           # )|>
           # highcharter::hc_add_series(
           #   data = data7_plot_percentil95_aux,
           #   type = "line",
           #   name = "Referência (percentil 95)",
           #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
           #   dashStyle = "ShortDot",
           #   opacity = 0.8
           # )

       }
      } else {
       grafico_base <- highcharter::highchart() |>
         highcharter::hc_add_dependency("modules/series-label.js") |>
         highcharter::hc_add_series(
           data = data7_plot_aux,
           type = "line",
           name = ifelse(input$faixa_peso != "mort_neonat", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
           #name = ifelse(data7_plot_aux$class == "Brasil (valor de referência)", gsub(" \\(valor de referência\\)", "", data7_plot_aux$class), data7_plot_aux$class),
           highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
         ) |>
          highcharter::hc_add_series(
          data = data7_plot_comp_aux,
          type = "line",
          name = ifelse(input$faixa_peso != "mort_neonat", data7_plot_comp_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_comp_aux$class)),
          #name = ifelse(data7_plot_comp_aux$class == "Brasil (valor de referência)", gsub(" \\(valor de referência\\)", "", data7_plot_comp_aux$class), data7_plot_comp_aux$class),
          highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
         ) |>
         highcharter::hc_plotOptions(
           series = list(
             label = list(enabled = TRUE),
             allowPointSelect = TRUE
           )
         ) |>
         # highcharter::hc_add_series(
         #   data = data7_plot_referencia_aux,
         #   type = "line",
         #   name = ifelse(input$faixa_peso != "mort_neonat", "Referência (média nacional)", "Referência (meta ODS)"),
         #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
         #   dashStyle = "ShortDot",
         #   opacity = 0.6
         # )|>
         highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
         highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
         highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
         highcharter::hc_colors(cols)
       if ((any(c(filtros()$nivel, filtros()$nivel2) == "nacional") & input$faixa_peso != "mort_neonat") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
         grafico_base#|>
           # highcharter::hc_add_series(
           #   data = data7_plot_percentil5_aux,
           #   type = "line",
           #   name = "Referência (percentil 5)",
           #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
           #   dashStyle = "ShortDot",
           #   opacity = 0.8
           # )|>
           # highcharter::hc_add_series(
           #   data = data7_plot_percentil95_aux,
           #   type = "line",
           #   name = "Referência (percentil 95)",
           #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
           #   dashStyle = "ShortDot",
           #   opacity = 0.8
           # )
       } else {
         grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(input$faixa_peso != "mort_neonat", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )#|>
           # highcharter::hc_add_series(
           #   data = data7_plot_percentil5_aux,
           #   type = "line",
           #   name = "Referência (percentil 5)",
           #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
           #   dashStyle = "ShortDot",
           #   opacity = 0.8
           # )|>
           # highcharter::hc_add_series(
           #   data = data7_plot_percentil95_aux,
           #   type = "line",
           #   name = "Referência (percentil 95)",
           #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
           #   dashStyle = "ShortDot",
           #   opacity = 0.8
           # )
       }
      }
    })

    #### Taxa de mortalidade neonatal precoce por 1000 nascidos vivos ---------
    output$plot3_neonat <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_precoc),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_precoc),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_precoc),
          class
        )

      # percentil5_faixa_peso_precoc <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso_precoc == "mort_neonat_precoc" ~ "percentil5_mort_neonat_precoc",
      #     input$faixa_peso_precoc == "mort_neonat_precoc_menos1000" ~ "percentil5_mort_neonat_precoc_menos1000",
      #     input$faixa_peso_precoc == "mort_neonat_precoc_1000_1499" ~ "percentil5_mort_neonat_precoc_1000_1499",
      #     input$faixa_peso_precoc == "mort_neonat_precoc_1500_2499" ~ "percentil5_mort_neonat_precoc_1500_2499",
      #     input$faixa_peso_precoc == "mort_neonat_precoc_mais2500" ~ "percentil5_mort_neonat_precoc_mais2500"
      #   )
      # })
      #
      # percentil95_faixa_peso_precoc <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso_precoc == "mort_neonat_precoc" ~ "percentil95_mort_neonat_precoc",
      #     input$faixa_peso_precoc == "mort_neonat_precoc_menos1000" ~ "percentil95_mort_neonat_precoc_menos1000",
      #     input$faixa_peso_precoc == "mort_neonat_precoc_1000_1499" ~ "percentil95_mort_neonat_precoc_1000_1499",
      #     input$faixa_peso_precoc == "mort_neonat_precoc_1500_2499" ~ "percentil95_mort_neonat_precoc_1500_2499",
      #     input$faixa_peso_precoc == "mort_neonat_precoc_mais2500" ~ "percentil95_mort_neonat_precoc_mais2500"
      #   )
      # })

      # data7_plot_percentil5_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil5_faixa_peso_precoc()),
      #     class
      #   )
      #
      # data7_plot_percentil95_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil95_faixa_peso_precoc()),
      #     class
      #   )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(input$faixa_peso_precoc != "mort_neonat_precoc", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
        if (filtros()$nivel == "nacional" & input$faixa_peso_precoc != "mort_neonat_precoc") {
          grafico_base#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(input$faixa_peso_precoc != "mort_neonat_precoc", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(input$faixa_peso_precoc != "mort_neonat_precoc", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            name = ifelse(input$faixa_peso_precoc != "mort_neonat_precoc", data7_plot_comp_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_comp_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
        if ((any(c(filtros()$nivel, filtros()$nivel2) == "nacional") & input$faixa_peso_precoc != "mort_neonat_precoc") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {

          grafico_base#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(input$faixa_peso_precoc != "mort_neonat_precoc", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        }
      }
    })

    #### Taxa de mortalidade neonatal tardia por 1000 nascidos vivos  ---------
    output$plot4_neonat <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_tardia),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_tardia),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_tardia),
          class
        )

      # percentil5_faixa_peso_tardia <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso_tardia == "mort_neonat_tardia" ~ "percentil5_mort_neonat_tardia",
      #     input$faixa_peso_tardia == "mort_neonat_tardia_menos1000" ~ "percentil5_mort_neonat_tardia_menos1000",
      #     input$faixa_peso_tardia == "mort_neonat_tardia_1000_1499" ~ "percentil5_mort_neonat_tardia_1000_1499",
      #     input$faixa_peso_tardia == "mort_neonat_tardia_1500_2499" ~ "percentil5_mort_neonat_tardia_1500_2499",
      #     input$faixa_peso_tardia == "mort_neonat_tardia_mais2500" ~ "percentil5_mort_neonat_tardia_mais2500"
      #   )
      # })
      #
      # percentil95_faixa_peso_tardia <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso_tardia == "mort_neonat_tardia" ~ "percentil95_mort_neonat_tardia",
      #     input$faixa_peso_tardia == "mort_neonat_tardia_menos1000" ~ "percentil95_mort_neonat_tardia_menos1000",
      #     input$faixa_peso_tardia == "mort_neonat_tardia_1000_1499" ~ "percentil95_mort_neonat_tardia_1000_1499",
      #     input$faixa_peso_tardia == "mort_neonat_tardia_1500_2499" ~ "percentil95_mort_neonat_tardia_1500_2499",
      #     input$faixa_peso_tardia == "mort_neonat_tardia_mais2500" ~ "percentil95_mort_neonat_tardia_mais2500"
      #   )
      # })

      # data7_plot_percentil5_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil5_faixa_peso_tardia()),
      #     class
      #   )
      #
      # data7_plot_percentil95_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil95_faixa_peso_tardia()),
      #     class
      #   )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(input$faixa_peso_tardia != "mort_neonat_tardia", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
        if (filtros()$nivel == "nacional" & input$faixa_peso_tardia != "mort_neonat_tardia") {
          grafico_base#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(input$faixa_peso_tardia != "mort_neonat_tardia", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(input$faixa_peso_tardia != "mort_neonat_tardia", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            name = ifelse(input$faixa_peso_tardia != "mort_neonat_tardia", data7_plot_comp_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_comp_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
        if ((any(c(filtros()$nivel, filtros()$nivel2) == "nacional") & input$faixa_peso_tardia != "mort_neonat_tardia") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(input$faixa_peso_tardia != "mort_neonat_tardia", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        }
      }
    })

    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$plot5_neonat <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "Sem informação",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = faltante_moment_obito_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_moment_obito_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "7 a 27 dias de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_7_27dist_moment_obito_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_7_27dist_moment_obito_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "1 a 6 dias de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_1_6dist_moment_obito_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_1_6dist_moment_obito_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Dia 0 de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_0_dist_moment_obito_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_0_dist_moment_obito_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(6, direction = -1)[-c(1, 6)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist1()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$plot6_neonat <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name =  "Sem informação",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = faltante_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name ="Maior ou igual a 2500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = mais_2500_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_mais_2500_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500g a 2499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1500_2499_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1500_2499_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1000g a 1499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1000_1499_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1000_1499_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1000g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = menos_1000_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_menos_1000_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist2()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos neonatais segundo análise de evitabilidade ----
    output$plot_evitaveis_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_neonatal_completo(),
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
            data = data_plot_evitaveis_neonatal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_neonatal_comp_completo(),
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
          viridis::magma(length(unique(data_plot_evitaveis_neonatal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_evitaveis_neonatal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_evitaveis_neonatal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos neonatais"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos neonatais por grupos de causas ----
    output$plot_grupos_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_grupos_neonatal_completo(),
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
            data = data_plot_grupos_neonatal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data_plot_grupos_neonatal_comp_completo(),
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
          viridis::magma(length(unique(data_plot_grupos_neonatal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_grupos_neonatal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_grupos_neonatal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos neonatais por grupos de causas"), min = 0, max = 100)

    })


    ### Para os indicadores de mortalidade perinatal --------------------------
    #### Número de óbitos perinatais (definição 1) ----------------------------
    output$plot1_perinatal <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_total),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_total),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_total),
          class
        )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      }
    })

    #### Número de óbitos perinatais (definição 2) ----------------------------
    output$plot2_perinatal <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_oms),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_oms),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_oms),
          class
        )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
      }
    })

    #### Taxa de mortalidade perinatal (definição 1) ------------------------------
    output$plot3_perinatal <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_taxa_total),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_taxa_total),
          class
        )

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_taxa_total),
          class
        )

      # percentil5_faixa_peso_perinatal_taxa_total <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total" ~ "percentil5_taxa_perinatal_total",
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_menos1000" ~ "percentil5_taxa_perinatal_total_menos1000",
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_1000_1499" ~ "percentil5_taxa_perinatal_total_1000_1499",
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_1500_2499" ~ "percentil5_taxa_perinatal_total_1500_2499",
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_mais2500" ~ "percentil5_taxa_perinatal_total_mais2500"
      #
      #   )
      # })
      #
      # percentil95_faixa_peso_perinatal_taxa_total <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total" ~ "percentil95_taxa_perinatal_total",
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_menos1000" ~ "percentil95_taxa_perinatal_total_menos1000",
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_1000_1499" ~ "percentil95_taxa_perinatal_total_1000_1499",
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_1500_2499" ~ "percentil95_taxa_perinatal_total_1500_2499",
      #     input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_mais2500" ~ "percentil95_taxa_perinatal_total_mais2500"
      #
      #   )
      # })

      # data7_plot_percentil5_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil5_faixa_peso_perinatal_taxa_total()),
      #     class
      #   )
      #
      # data7_plot_percentil95_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil95_faixa_peso_perinatal_taxa_total()),
      #     class
      #   )


      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(input$faixa_peso_perinatal_taxa_total != "taxa_perinatal_total", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
        if (filtros()$nivel == "nacional" & input$faixa_peso_perinatal_taxa_total != "taxa_perinatal_total") {
          grafico_base#|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(input$faixa_peso_perinatal_taxa_total != "taxa_perinatal_total", "Referência (média nacional)", "Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )#|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(input$faixa_peso_perinatal_taxa_total != "taxa_perinatal_total", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            name = ifelse(input$faixa_peso_perinatal_taxa_total != "taxa_perinatal_total", data7_plot_comp_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_comp_aux$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
        if ((any(c(filtros()$nivel, filtros()$nivel2) == "nacional") & input$faixa_peso_perinatal_taxa_total != "taxa_perinatal_total") |
            (filtros()$mostrar_referencia == "nao_mostrar_referencia") ) {
          grafico_base #|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )
        } else if (input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total") {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = glue::glue("Referência (meta ODS)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )#|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil5_aux,
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_plot_percentil95_aux,
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )
        }
      }
    })


    #### Taxa de mortalidade perinatal (definição 2) ------------------------------
    output$plot4_perinatal <- highcharter::renderHighchart({
      data7_plot_aux <- data7() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_taxa_oms),
          class
        )

      data7_plot_comp_aux <- data7_comp() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_taxa_oms),
          class
        )

      if (input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms") {
        data7_plot_referencia_aux <- data7_referencia_perinatal() |>
          dplyr::select(
            ano,
            eixo_y = taxa_perinatal_oms,
            class
          )

        data7_plot_referencia_comp_aux <- data7_referencia_perinatal_comp() |>
          dplyr::select(
            ano,
            eixo_y = taxa_perinatal_oms,
            class
          )
      } else {
        data7_plot_referencia_aux <- data7_referencia() |>
          dplyr::select(
            ano,
            eixo_y = dplyr::all_of(input$faixa_peso_perinatal_taxa_oms),
            class
          )
      }

      # percentil5_faixa_peso_perinatal_taxa_oms <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms" ~ "percentil5_taxa_perinatal_oms",
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_menos1000" ~ "percentil5_taxa_perinatal_oms_menos1000",
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_1000_1499" ~ "percentil5_taxa_perinatal_oms_1000_1499",
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_1500_2499" ~ "percentil5_taxa_perinatal_oms_1500_2499",
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_mais2500" ~ "percentil5_taxa_perinatal_oms_mais2500"
      #
      #   )
      # })
      #
      # percentil95_faixa_peso_perinatal_taxa_oms <- reactive({
      #   dplyr::case_when(
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms" ~ "percentil95_taxa_perinatal_oms",
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_menos1000" ~ "percentil95_taxa_perinatal_oms_menos1000",
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_1000_1499" ~ "percentil95_taxa_perinatal_oms_1000_1499",
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_1500_2499" ~ "percentil95_taxa_perinatal_oms_1500_2499",
      #     input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_mais2500" ~ "percentil95_taxa_perinatal_oms_mais2500"
      #
      #   )
      # })

      # data7_plot_percentil5_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil5_faixa_peso_perinatal_taxa_oms()),
      #     class
      #   )
      #
      # data7_plot_percentil95_aux <- data7_percentil() |>
      #   dplyr::select(
      #     ano,
      #     eixo_y = dplyr::all_of(percentil95_faixa_peso_perinatal_taxa_oms()),
      #     class
      #   )


      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(input$faixa_peso_perinatal_taxa_oms != "taxa_perinatal_oms", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
        if (filtros()$nivel == "nacional" & input$faixa_peso_perinatal_taxa_oms != "taxa_perinatal_oms") {
          grafico_base#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = ifelse(input$faixa_peso_perinatal_taxa_oms != "taxa_perinatal_oms", "Referência (média nacional)", "Referência (meta adaptada para a localidade)"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            name = ifelse(input$faixa_peso_perinatal_taxa_oms != "taxa_perinatal_oms", data7_plot_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_aux$class)),
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_plot_comp_aux,
            name = ifelse(input$faixa_peso_perinatal_taxa_oms != "taxa_perinatal_oms", data7_plot_comp_aux$class, gsub(" \\(valor de referência\\)", "", data7_plot_comp_aux$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
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
        if ((any(c(filtros()$nivel, filtros()$nivel2) == "nacional") & input$faixa_peso_perinatal_taxa_oms != "taxa_perinatal_oms") |
            (filtros()$mostrar_referencia == "nao_mostrar_referencia") ) {
          grafico_base #|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else if (input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms") {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = glue::glue("Referência (meta adaptada para {unique(data7_plot_aux$class)})"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            ) |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_comp_aux,
              type = "line",
              name = glue::glue("Referência (meta adaptada para {unique(data7_plot_comp_aux$class)})"),
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )#|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil5_aux,
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_plot_percentil95_aux,
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        }
      }
    })

    #### Distribuição percentual do momento do óbito por faixa de peso  -------
    output$plot5_perinatal <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "Sem informação",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = faltante_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "1 a 6 dias de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_1_6_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_1_6_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Dia 0 de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_0_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_0_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Durante o parto",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = durante_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_durante_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Antes do parto",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = antes_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_antes_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist1()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$plot6_perinatal <- highcharter::renderHighchart({
      highcharter::highchart()|>
        highcharter::hc_add_series(
          name = "Sem informação",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = faltante_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Maior ou igual a 2500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = mais_2500_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_mais_2500_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500g a 2499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1500_2499_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1500_2499_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1000 a 1499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1000_1499_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1000_1499_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1000g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = menos_1000_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_menos_1000_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist2()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos perinatais segundo análise de evitabilidade ----
    output$plot_evitaveis_perinatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_perinatal_completo(),
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
            data = data_plot_evitaveis_perinatal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_perinatal_comp_completo(),
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
          viridis::magma(length(unique(data_plot_evitaveis_perinatal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_evitaveis_perinatal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_evitaveis_perinatal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos perinatais"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos perinatais por grupos de causas ----
    output$plot_grupos_perinatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_grupos_perinatal_completo(),
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
            data = data_plot_grupos_perinatal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data_plot_grupos_perinatal_comp_completo(),
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
          viridis::magma(length(unique(data_plot_grupos_perinatal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_grupos_perinatal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_grupos_perinatal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos perinatais por grupos de causas"), min = 0, max = 100)

    })



    ### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    output$plot1_morbidade_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base#|>
            # highcharter::hc_add_series(
            #   data = data7_percentil(),
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = percentil5_porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_percentil(),
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = percentil95_porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )#|>
            # highcharter::hc_add_series(
            #   data = data7_percentil(),
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = percentil5_porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_percentil(),
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = percentil95_porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_comp()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = percentil5_porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # ) |>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = percentil95_porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base#|>
            # highcharter::hc_add_series(
            #   data = data7_percentil(),
            #   type = "line",
            #   name = "Referência (percentil 5)",
            #   highcharter::hcaes(x = ano, y = percentil5_porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )|>
            # highcharter::hc_add_series(
            #   data = data7_percentil(),
            #   type = "line",
            #   name = "Referência (percentil 95)",
            #   highcharter::hcaes(x = ano, y = percentil95_porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
            #   dashStyle = "ShortDot",
            #   opacity = 0.8
            # )
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })


    ### Porcentagem de internações até o 27º dia de vida de bebês nascidos em hospitais com vínculo com o SUS

    output$plot2_morbidade_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_internacoes_publicos_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_internacoes_publicos_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = percentil5_porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = percentil95_porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = max(c(
            c(data7_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih),
            c(data7_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih)
          ), na.rm = TRUE) + 1) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_internacoes_publicos_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_internacoes_publicos_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_internacoes_publicos_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_internacoes_publicos_sih_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_internacoes_publicos_sih_comp()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = percentil5_porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = percentil95_porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = max(c(
            c(data7_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih),
            c(data7_internacoes_publicos_sih_comp()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih_comp()$porc_internacoes_menores_28_dias_sih),
            c(data7_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih)
          ), na.rm = TRUE) + 1) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_internacoes_publicos_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })


    ### Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS
    output$plot3_morbidade_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_internacoes_uti_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_internacoes_uti_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = percentil5_porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = percentil95_porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_internacoes_uti_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_internacoes_uti_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_internacoes_uti_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_internacoes_uti_sih_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_internacoes_uti_sih_comp()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 5)",
          #   highcharter::hcaes(x = ano, y = percentil5_porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          # highcharter::hc_add_series(
          #   data = data7_percentil(),
          #   type = "line",
          #   name = "Referência (percentil 95)",
          #   highcharter::hcaes(x = ano, y = percentil95_porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
          #   dashStyle = "ShortDot",
          #   opacity = 0.8
          # )|>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_internacoes_uti_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })


    # Distribuição das internações neonatais por grupos de causas

    output$plot_grupos_morbidade_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_grupos_morbidade_neonatal_completo(),
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
            data = data_plot_grupos_morbidade_neonatal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data_plot_grupos_morbidade_neonatal_comp_completo(),
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
          viridis::magma(length(unique(data_plot_grupos_morbidade_neonatal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_grupos_neonatal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_grupos_morbidade_neonatal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de internações neonatais por grupos de causas"), min = 0, max = 100)

    })


    # cores_aux <- viridis::magma(16, direction = -1)[-c(1, 16)])
    # output$plot_grupos_morbidade_neonatal <- highcharter::renderHighchart({
    #   if (filtros()$comparar == "Não") {
    #     grafico_base <- highcharter::highchart() |>
    #       highcharter::hc_add_series(
    #         data = data_plot_grupos_morbidade_neonatal_completo(),
    #         highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
    #         type = "column",
    #         showInLegend = TRUE,
    #         tooltip = list(
    #           pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
    #         )
    #       )|>
    #       highcharter::hc_colors(
    #         viridis::magma(16, direction = -1)[-c(1, 16)])
    #   } else {
    #     grafico_base <- highcharter::highchart()|>
    #       highcharter::hc_add_series(
    #         data = data_plot_grupos_morbidade_neonatal_completo() |> dplyr::filter(grupo_cid10_aux == "grupos_nao_selecionados"),
    #         highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
    #         type = "column",
    #         showInLegend = TRUE,
    #         color = cores_aux[14],
    #         id = "series1",
    #         tooltip = list(
    #           pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
    #         ),
    #         stack = 0
    #       ) |>
    #       highcharter::hc_add_series(
    #         data = data_plot_grupos_morbidade_neonatal_comp_completo() |> dplyr::filter(grupo_cid10_aux == "grupos_nao_selecionados"),
    #         highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
    #         type = "column",
    #         showInLegend = FALSE,
    #         color = cores_aux[14],
    #         tooltip = list(
    #           pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
    #         ),
    #         stack = 1,
    #         linkedTo = ":previous"
    #       )
    #
    #     for (indicador in input$cids_grupos_morbidade_neonatal) {
    #       grafico_base <- grafico_base |>
    #         highcharter::hc_add_series(
    #           data = data_plot_grupos_morbidade_neonatal_completo()|> dplyr::filter(grepl(indicador, grupo_cid10_aux)),
    #           highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
    #           type = "column",
    #           showInLegend = TRUE,
    #           color = cores_aux[which(input$cids_grupos_morbidade_neonatal == indicador)],
    #           id = paste0("series_", indicador),
    #           tooltip = list(
    #             pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
    #           ),
    #           stack = 0
    #         ) |>
    #         highcharter::hc_add_series(
    #           data = data_plot_grupos_morbidade_neonatal_comp_completo()|> dplyr::filter(grepl(indicador, grupo_cid10_aux)),
    #           highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
    #           type = "column",
    #           showInLegend = FALSE,
    #           color = cores_aux[which(input$cids_grupos_morbidade_neonatal == indicador)],
    #           tooltip = list(
    #             pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
    #           ),
    #           stack = 1,
    #           linkedTo = ":previous"
    #         )
    #     }
    #
    #
    #   }
    #
    #   grafico_base |>
    #     highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
    #     highcharter::hc_legend(reversed = FALSE, title = list(text = "Grupo de causas")) |>
    #     highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_grupos_morbidade_neonatal()$ano), allowDecimals = FALSE) |>
    #     highcharter::hc_yAxis(title = list(text = "% relativo ao total de internações neonatais por grupos de causas"), min = 0, max = 100)
    #
    # })


 })
}

## To be copied in the UI
# mod_bloco_7_ui("bloco_7_1")

## To be copied in the server
# mod_bloco_7_server("bloco_7_1")
