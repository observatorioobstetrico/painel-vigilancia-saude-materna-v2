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
      h2(tags$b(HTML("Mortalidade fetal, perinatal, neonatal e morbidade neonatal: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset1"),
      width = 12,
      collapsible = FALSE,
      tabPanel(
        HTML("<b>Indicadores relacionados à mortalidade fetal </b>"),
        value = "tabpanel_fetal",
        fluidRow(
          column(
            width=12,
            HTML(
              "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
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
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
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
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Número de óbitos fetais &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = ns("parto_fetal"),
                        label = "Momento do óbito",
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
                        label = "Faixa de peso",
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "peso_fetal",
                          "Menor que 1500 g" = "fetal_menos1500",
                          "De 1500 g a 1999 g" = "fetal_1500_1999",
                          "De 2000 g a 2499 g" = "fetal_2000_2499",
                          "Maior ou igual a 2500 g" = "fetal_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_fetal"), height = 450))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Taxa de mortalidade fetal  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      selectizeInput(
                        inputId = ns("parto_fetal2"),
                        label = "Momento do óbito",
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
                        label = "Faixa de peso",
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "peso_fetal",
                          "Menor que 1500 g" = "fetal_menos1500",
                          "De 1500 g a 1999 g" = "fetal_1500_1999",
                          "De 2000 g a 2499 g" = "fetal_2000_2499",
                          "Maior ou igual a 2500 g" = "fetal_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_fetal"), height = 450))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual do momento do óbito por faixa de peso  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      strong(p("Selecione as faixas de peso:", style = "margin-bottom: 0.5rem")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("faixa_peso_dist_moment_obit_fetal"),
                          label    = NULL,
                          choices = c(
                            "Menor que 1500 g" = "dist_moment_obito_fetal_menos1500",
                            "De 2000 a 2499 g" = "dist_moment_obito_fetal_2000_2499",
                            "De 1500 a 1999 g" = "dist_moment_obito_fetal_1500_1999",
                            "Maior ou igual a 2500 g" = "dist_moment_obito_fetal_mais2500"
                          ),
                          selected = c(
                            "dist_moment_obito_fetal_menos1500", "dist_moment_obito_fetal_1500_1999",
                            "dist_moment_obito_fetal_2000_2499", "dist_moment_obito_fetal_mais2500"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_fetal"), height = 460))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual das faixas de peso por momento do óbito  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      strong(p("Selecione os momentos de óbito:", style = "margin-bottom: 0.5rem")),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_fetal"), height = 510))
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
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual dos óbitos fetais por grupos de causas segundo a Rede Interagencial de Informações para Saúde (Fonte: <a href = https://bvsms.saude.gov.br/bvs/publicacoes/demografia_saude_contribuicao_tendencias.pdf , target = _blank>link</a>) &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_grupos_fetal"),
                        label = "Selecione, aqui, os grupos de interesse:",
                        options = list(placeholder = "Selecione, aqui, os grupos de interesse:",
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = "Desselecionar todas",
                                       `select-all-text` = "Selecionar todas",
                                       `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Prematuridade" = "prematuridade",
                          "Infecções" = "infeccoes",
                          "Asfixia/Hipóxia" = "asfixia",
                          "Malformação congênita" = "ma_formacao",
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
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_fetal_grupos"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "fetal_grupos_antes",
                            "Durante o parto" = "fetal_grupos_durante"
                          ),
                          selected = c("fetal_grupos_antes","fetal_grupos_durante"),
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_fetal"), height = 490))
                )
              ),
              # column(
              #   width = 12,
              #   bs4Dash::bs4Card(
              #     width = 12,
              #     status = "primary",
              #     collapsible = FALSE,
              #     headerBorder = FALSE,
              #     style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              #     div(
              #       style = "height: 10%; display: flex; align-items: center;",
              #       HTML("<b style='font-size:19px'> Distribuição percentual dos óbitos fetais por causas evitáveis (Fonte: <a href = 'http://tabnet.datasus.gov.br/cgi/sim/Obitos_Evitaveis_0_a_4_anos.pdf' , target = _blank>link</a>) &nbsp;</b>")
              #     ),
              #     hr(),
              #     fluidRow(
              #       column(
              #         width = 6,
              #         shinyWidgets::pickerInput(
              #           inputId = ns("cids_evitaveis_fetal"),
              #           label = "Selecione, aqui, os grupos de interesse:",
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
              #         strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem")),
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
                   style = "height: 10%; display: flex; align-items: center;",
                   HTML("<b style='font-size:19px'> Distribuição percentual dos óbitos fetais por análise de evitabilidade segundo o <a href = 'https://www.scielo.br/j/ress/a/cF66ngM4VB3YXV7Js8WynXC/?format=pdf&lang=pt' target = _blank>artigo de Vieira et Al (2011)</a> &nbsp;</b>")),
                   hr(),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_evitaveis_fetal2"),
                        label = "Selecione, aqui, os grupos de interesse:",
                        options = list(placeholder = "Selecione, aqui, os grupos de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Imunoprevenção" = "imunoprevencao2",
                          "Mortes reduzíveis por adequada atenção à mulher na gestação" = "mulher_gestacao2",
                          "Mortes reduzíveis por adequada atenção à mulher no parto" = "parto2",
                          "Causas de morte mal-definidas" = "mal_definidas2",
                          "Não se aplicam ao óbito fetal" = "nao_aplica2",
                          "Demais causas (não claramente evitáveis)" = "outros2"
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
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_fetal_evitaveis2"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "evitaveis_fetal_antes",
                            "Durante o parto" = "evitaveis_fetal_durante"#,
                          ),
                          selected = c(
                            "evitaveis_fetal_antes", "evitaveis_fetal_durante"
                          )
                        )
                      )
                    )
                   ),
                   shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_fetal2"), height = 490))
                 )
              )
            )
          )
        )
      ),
      #),

      tabPanel(
        HTML("<b>Indicadores relacionados à mortalidade perinatal </b>"),
        value = "tabpanel_perinatal",
        fluidRow(
          column(
            width=12,
            HTML(
              "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
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
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
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
              # column(
              #   width = 6,
              #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i1")), proxy.height = "300px")
              # ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i2")), proxy.height = "300px")
              ),
              # column(
              #   width = 6,
              #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i3")), proxy.height = "300px")
              # ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i4")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i5")), proxy.height = "300px")
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i6")), proxy.height = "300px")
              )
            )
          ),
          column(
            width = 8,
            fluidRow(
              # column(
              #   width = 6,
              #   bs4Dash::bs4Card(
              #     width = 12,
              #     status = "primary",
              #     collapsible = FALSE,
              #     headerBorder = FALSE,
              #     style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              #     div(
              #       style = "height: 15%; display: flex; align-items: center;",
              #       HTML("<b style='font-size:18px'> Número de óbitos perinatais (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida) &nbsp;</b>")
              #     ),
              #     hr(),
              #     fluidRow(
              #       column(
              #         width = 12,
              #         selectizeInput(
              #           inputId = ns("faixa_peso_perinatal_total"),
              #           label = "Faixa de peso",
              #           options = list(placeholder = "Selecione o intervalo de peso"),
              #           choices = c(
              #             "Geral" = "obitos_perinatal_total",
              #             "Menor que 1500 g" = "perinatal_total_menos1500",
              #             "De 1500 g a 1999 g" = "perinatal_total_1500_1999",
              #             "De 2000 g a 2499 g" = "perinatal_total_2000_2499",
              #             "Maior ou igual a 2500 g" = "perinatal_total_mais2500"
              #           ),
              #           selected = c("obitos_perinatal_total", "perinatal_total_menos1500", "perinatal_total_1500_1999",
              #                        "perinatal_total_2000_2499", "perinatal_total_mais2500"),
              #           width = "100%"
              #         )
              #       )
              #     ),
              #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_perinatal"), height = 410))
              #   )
              # ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:18px'> Número de óbitos perinatais (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_perinatal_oms"),
                        label = "Faixa de peso",
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "obitos_perinatal_oms",
                          "Menor que 1500 g" = "perinatal_oms_menos1500",
                          "De 1500 g a 1999 g" = "perinatal_oms_1500_1999",
                          "De 2000 g a 2499 g" = "perinatal_oms_2000_2499",
                          "Maior ou igual a 2500 g" = "perinatal_oms_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_perinatal"), height = 410))
                )
              ),
              # column(
              #   width = 6,
              #   bs4Dash::bs4Card(
              #     width = 12,
              #     status = "primary",
              #     collapsible = FALSE,
              #     headerBorder = FALSE,
              #     style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              #     div(
              #       style = "height: 15%; display: flex; align-items: center;",
              #       HTML("<b style='font-size:18px'> Taxa de óbitos perinatais (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)  &nbsp;</b>")
              #     ),
              #     hr(),
              #     fluidRow(
              #       column(
              #         width = 12,
              #         selectizeInput(
              #           inputId = ns("faixa_peso_perinatal_taxa_total"),
              #           label = "Faixa de peso",
              #           options = list(placeholder = "Selecione o intervalo de peso"),
              #           choices = c(
              #             "Geral" = "taxa_perinatal_total",
              #             "Menor que 1500 g" = "taxa_perinatal_total_menos1500",
              #             "De 1500 g a 1999 g" = "taxa_perinatal_total_1500_1999",
              #             "De 2000 g a 2499 g" = "taxa_perinatal_total_2000_2499",
              #             "Maior ou igual a 2500 g" = "taxa_perinatal_total_mais2500"
              #           ),
              #           width = "100%"
              #         )
              #       )
              #     ),
              #     shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_perinatal"), height = 410))
              #   )
              # ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:18px'> Taxa de óbitos perinatais (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_perinatal_taxa_oms"),
                        label = "Faixa de peso",
                        options = list(placeholder = "Selecione o intervalo de peso"),
                        choices = c(
                          "Geral" = "taxa_perinatal_oms",
                          "Menor que 1500 g" = "taxa_perinatal_oms_menos1500",
                          "De 1500 g a 1999 g" = "taxa_perinatal_oms_1500_1999",
                          "De 2000 g a 2499 g" = "taxa_perinatal_oms_2000_2499",
                          "Maior ou igual a 2500 g" = "taxa_perinatal_oms_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_perinatal"), height = 410))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual do momento do óbito por faixa de peso  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      strong(p("Selecione as faixas de peso:", style = "margin-bottom: 0.5rem")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("faixa_peso_dist_moment_obit_perinat"),
                          label    = NULL,
                          choices = c(
                            "Menor que 1500 g" = "dist_moment_obito_perinat_menos1500",
                            "De 2000 a 2499 g" = "dist_moment_obito_perinat_2000_2499",
                            "De 1500 a 1999 g" = "dist_moment_obito_perinat_1500_1999",
                            "Maior ou igual a 2500 g" = "dist_moment_obito_perinat_mais2500"
                          ),
                          selected = c(
                            "dist_moment_obito_perinat_menos1500", "dist_moment_obito_perinat_1500_1999",
                            "dist_moment_obito_perinat_2000_2499", "dist_moment_obito_perinat_mais2500"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_perinatal"), height = 460))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual das faixas de peso por momento do óbito  &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      strong(p("Selecione os momentos de óbito:", style = "margin-bottom: 0.5rem")),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_perinatal"), height = 460))
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
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual dos óbitos perinatais por grupos de causas segundo a Rede Interagencial de Informações para Saúde (Fonte: <a href = https://bvsms.saude.gov.br/bvs/publicacoes/demografia_saude_contribuicao_tendencias.pdf , target = _blank>link</a>) &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_grupos_perinatal"),
                        label = "Selecione, aqui, os grupos de interesse:",
                        options = list(placeholder = "Selecione, aqui, os grupos de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Prematuridade" = "prematuridade",
                          "Infecções" = "infeccoes",
                          "Asfixia/Hipóxia" = "asfixia",
                          "Malformação congênita" = "ma_formacao",
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
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_perinatal_grupos"),
                          label    = NULL,
                          choices = c(
                            "Antes do parto" = "perinatal_grupos_antes", # estou aqui
                            "Durante o parto" = "perinatal_grupos_durante",
                            "Dia 0 de vida" = "perinatal_grupos_0_dias",
                            "De 1 a 6 dias de vida" = "perinatal_grupos_1_6_dias"
                          ),
                          selected = c(
                            "perinatal_grupos_antes", "perinatal_grupos_durante",
                            "perinatal_grupos_0_dias", "perinatal_grupos_1_6_dias"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_perinatal"), height = 460))
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
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual dos óbitos perinatais por análise de evitabilidade (Fonte: <a href = http://tabnet.datasus.gov.br/cgi/sim/Obitos_Evitaveis_0_a_4_anos.pdf , target = _blank>link</a>) &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_evitaveis_perinatal"),
                        label = "Selecione, aqui, os grupos de interesse:",
                        options = list(placeholder = "Selecione, aqui, os grupos de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Reduzível pelas ações de imunização" = "imunoprevencao",
                          "Reduzíveis por adequada atenção à mulher na gestação" = "mulher_gestacao",
                          "Reduzíveis por adequada atenção à mulher no parto" = "parto",
                          "Reduzíveis por adequada atenção ao recém-nascido" = "recem_nascido",
                          "Reduzíveis por ações de diagnóstico e tratamento adequado" = "tratamento",
                          "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "saude",
                          "Causas mal definidas" = "mal_definidas",
                          "Demais causas (não claramente evitáveis)" = "outros"
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
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem")),
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
                            "De 1 a 6 dias de vida" = "evitaveis_perinatal_1_6_dias"
                          ),
                          selected = c(
                            "evitaveis_perinatal_antes", "evitaveis_perinatal_durante",
                            "evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_perinatal"), height = 460))
                )
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b>Indicadores relacionados à mortalidade neonatal</b>"),
        value = "tabpanel_neonatal",
        fluidRow(
          column(
            width = 12,
            HTML(
              "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
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
          #     "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
          #       <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos maternos no país, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-grav-puerp/' target = _blank>OOBr Óbitos de Gestantes e Puérperas</a>.
          #       </b> </div>"
          #   ),
          #   hr(),
          #   HTML("<span style='display: block; margin-bottom: 25px;'> </span>"),
          # ),
          column(
            width = 4,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
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
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Número de óbitos neonatais &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("obitos_faixa_peso"),
                        label = "Faixa de peso ao nascer",
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "obitos_neonat",
                          "Menor que 1500 g" = "obitos_neonat_menos1500",
                          "De 1500 g a 1999 g" = "obitos_neonat_1500_1999",
                          "De 2000 g a 2499 g" = "obitos_neonat_2000_2499",
                          "Maior ou igual a 2500 g" = "obitos_neonat_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_neonat"), height = 450))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Taxa de mortalidade neonatal por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso"),
                        label = "Faixa de peso ao nascer",
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat",
                          "Menor que 1500 g" = "mort_neonat_menos1500",
                          "De 1500 g a 1999 g" = "mort_neonat_1500_1999",
                          "De 2000 g a 2499 g" = "mort_neonat_2000_2499",
                          "Maior ou igual a 2500 g" = "mort_neonat_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_neonat"), height = 450))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Taxa de mortalidade neonatal precoce por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_precoc"),
                        label = "Faixa de peso ao nascer",
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat_precoc",
                          "Menor que 1500 g" = "mort_neonat_precoc_menos1500",
                          "De 1500 g a 1999 g" = "mort_neonat_precoc_1500_1999",
                          "De 2000 g a 2499 g" = "mort_neonat_precoc_2000_2499",
                          "Maior ou igual a 2500 g" = "mort_neonat_precoc_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3_neonat"), height = 410))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 650px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Taxa de mortalidade neonatal tardia por 1000 nascidos vivos &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      selectizeInput(
                        inputId = ns("faixa_peso_tardia"),
                        label = "Faixa de peso ao nascer",
                        options = list(placeholder = "Selecione o intervalo de peso ao nascer"),
                        choices = c(
                          "Geral" = "mort_neonat_tardia",
                          "Menor que 1500 g" = "mort_neonat_tardia_menos1500",
                          "De 1500 g a 1999 g" = "mort_neonat_tardia_1500_1999",
                          "De 2000 g a 2499 g" = "mort_neonat_tardia_2000_2499",
                          "Maior ou igual a 2500 g" = "mort_neonat_tardia_mais2500"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4_neonat"), height = 410))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual do momento do óbito por faixa de peso &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      strong(p("Selecione as faixas de peso:", style = "margin-bottom: 0.5rem")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("faixa_peso_dist_moment_obit_neonat"),
                          label    = NULL,
                          choices = c(
                            "Menor que 1500 g" = "dist_moment_obito_neonat_menos1500",
                            "De 2000 g a 2499 g" = "dist_moment_obito_neonat_2000_2499",
                            "De 1500 g a 1999 g" = "dist_moment_obito_neonat_1500_1999",
                            "Maior ou igual a 2500 g" = "dist_moment_obito_neonat_mais2500"
                          ),
                          selected = c(
                            "dist_moment_obito_neonat_menos1500", "dist_moment_obito_neonat_1500_1999",
                            "dist_moment_obito_neonat_2000_2499", "dist_moment_obito_neonat_mais2500"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5_neonat"), height = 460))
                )
              ),
              column(
                width = 6,
                bs4Dash::bs4Card(
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  headerBorder = FALSE,
                  style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual das faixas de peso por momento do óbito &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 12,
                      strong(p("Selecione os momentos de óbito:", style = "margin-bottom: 0.5rem")),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot6_neonat"), height = 460))
                )
              ),
              # column(
              #   width = 12,
              #   bs4Dash::bs4Card(
              #     width = 12,
              #     status = "primary",
              #     collapsible = FALSE,
              #     headerBorder = FALSE,
              #     style = "height: 700px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              #     div(
              #       style = "height: 10%; display: flex; align-items: center;",
              #       HTML("<b style='font-size:19px'> Distribuição percentual dos óbitos neonatais por causas principais definidas pelo DATASUS (Fonte: <a href = http://www2.datasus.gov.br/cid10/V2008/WebHelp/p00_p96.htm , target = _blank>link</a>) &nbsp;</b>")
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
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual dos óbitos neonatais por grupos de causas segundo a Rede Interagencial de Informações para Saúde (Fonte: <a href = https://bvsms.saude.gov.br/bvs/publicacoes/demografia_saude_contribuicao_tendencias.pdf , target = _blank>link</a>) &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    ############################################################
                    ############################################################
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_grupos_neonatal"),
                        label = "Selecione os grupos de causas:",
                        options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Prematuridade" = "prematuridade",
                          "Infecções" = "infeccoes",
                          "Asfixia/Hipóxia" = "asfixia",
                          "Malformação congênita" = "ma_formacao",
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
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem")),
                      tags$div(
                        align = 'left',
                        class = 'multicol',
                        checkboxGroupInput(
                          inputId = ns("momento_obito_neonatal_grupos"),
                          label    = NULL,
                          choices = c(
                            "Dia 0 de vida" = "neonat_grupos_0_dias",
                            "De 1 a 6 dias de vida" = "neonat_grupos_1_6_dias",
                            "De 7 a 27 dias de vida" = "neonat_grupos_7_27_dias"
                          ),
                          selected = c(
                            "neonat_grupos_0_dias",
                            "neonat_grupos_1_6_dias",
                            "neonat_grupos_7_27_dias"
                          )
                        )
                      )
                    )
                  ),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_neonatal"), height = 470))
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
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual dos óbitos neonatais por análise de evitabilidade (Fonte: <a href = http://tabnet.datasus.gov.br/cgi/sim/Obitos_Evitaveis_0_a_4_anos.pdf , target = _blank>link</a>) &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    ############################################################
                    ############################################################
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_evitaveis_neonatal"),
                        label = "Selecione os grupos de causas evitáveis:",
                        options = list(placeholder = "Selecione os grupos de causas evitáveis", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Reduzível pelas ações de imunização" = "imunoprevencao",
                          "Reduzíveis por adequada atenção à mulher na gestação" = "mulher_gestacao",
                          "Reduzíveis por adequada atenção à mulher no parto" = "parto",
                          "Reduzíveis por adequada atenção ao recém-nascido" = "recem_nascido",
                          "Reduzíveis por ações de diagnóstico e tratamento adequado" = "tratamento",
                          "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "saude",
                          "Causas mal definidas" = "mal_definidas",
                          "Demais causas (não claramente evitáveis)" = "outros"
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
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de óbito considerados:", style = "margin-bottom: 0.5rem")),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_neonatal"), height = 470))
                )
              )
            )
          )
        )
      ),


      tabPanel(
        HTML("<b>Indicadores relacionados à morbidade neonatal</b>"),
        value = "tabpanel_morbidade_neonatal",
        fluidRow(
          column(
            width = 12,
            HTML(
              "<div style = 'text-align: center;'> <b style = 'font-size: 19px'>
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
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
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
                  style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                  div(
                    style = "height: 15%; display: flex; align-items: center;",
                    HTML("<b style='font-size:18px'> Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao7"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao7"),
                          icon = icon("triangle-exclamation", style = "color: red"),
                          color = "warning",
                          style = "material-circle",
                          size = "xs"
                        )
                      )
                    )
                  ),
                  hr(),
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1_morbidade_neonatal"), height = 450))
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
                    HTML("<b style='font-size:18px'> Porcentagem de internações neonatais no SUS ocorridas até o 27º dia de vida &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao10"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao10"),
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
                      width = 6,
                      selectizeInput(
                        inputId = ns("local_internacao_sih"),
                        label = "Local da internação",
                        options = list(placeholder = "Selecione o local de internação"),
                        choices = c(
                          "Todos" = "geral",
                          "Dentro da macrorregião de saúde" = "na_macro",
                          "Fora da macrorregião de saúde" = "fora_macro"
                        ),
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Idade, em dias, do bebê", style = "margin-bottom: 0.5rem")),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2_morbidade_neonatal"), height = 350))
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
                    HTML("<b style='font-size:18px'> Porcentagem de internações neonatais em UTI no SUS ocorridas até o 27º dia de vida &nbsp;</b>"),
                    shinyjs::hidden(
                      span(
                        id = ns("mostrar_botao12"),
                        shinyWidgets::actionBttn(
                          inputId = ns("botao12"),
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
                      width = 6,
                      selectizeInput(
                        inputId = ns("local_internacao_uti_sih"),
                        label = "Local da internação",
                        options = list(placeholder = "Selecione o local de internação"),
                        choices = c(
                          "Todos" = "geral",
                          "Dentro da macrorregião de saúde" = "na_macro",
                          "Fora da macrorregião de saúde" = "fora_macro"
                        ),
                        width = "100%"
                      )
                    ),
                    column(
                      width = 6,
                      strong(p("Idade, em dias, do bebê", style = "margin-bottom: 0.5rem")),
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
                    style = "height: 10%; display: flex; align-items: center;",
                    HTML("<b style='font-size:19px'> Distribuição percentual das internações neonatais por grupos de causas segundo a Rede Interagencial de Informações para Saúde (Fonte: <a href = https://bvsms.saude.gov.br/bvs/publicacoes/demografia_saude_contribuicao_tendencias.pdf , target = _blank>link</a>) &nbsp;</b>")
                  ),
                  hr(),
                  fluidRow(
                    column(
                      width = 6,
                      shinyWidgets::pickerInput(
                        inputId = ns("cids_grupos_morbidade_neonatal"),
                        label = "Selecione os grupos de causas:",
                        options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                        choices = c(
                          "Prematuridade" = "prematuridade",
                          "Infecções" = "infeccoes",
                          "Asfixia/Hipóxia" = "asfixia",
                          "Malformação congênita" = "ma_formacao",
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
                    column(
                      width = 6,
                      strong(p("Selecione, aqui, os momentos de internação considerados:", style = "margin-bottom: 0.5rem")),
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
                  shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_morbidade_neonatal"), height = 470))
                )
              )


            )
          )
        )
      ) #fim do tabpanel morbidade neonatal



  ) )
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
        obitos_neonat = rep("sum(obitos_27dias)", 2),
        obitos_neonat_menos1500 = rep("sum(obitos_27dias_menos1500)", 2),
        obitos_neonat_1500_1999 = rep("sum(obitos_27dias_1500_1999)", 2),
        obitos_neonat_2000_2499 = rep("sum(obitos_27dias_2000_2499)", 2),
        obitos_neonat_mais2500 = rep("sum(obitos_27dias_mais2500)", 2),
        mort_neonat = rep("round(sum(obitos_27dias)/sum(nascidos) *1000, 2)", 2),
        mort_neonat_precoc = rep("round(sum(obitos_6dias)/sum(nascidos) *1000, 2)", 2),
        mort_neonat_tardia = rep("round(sum(obitos_7_27dias)/sum(nascidos) *1000, 2)", 2),
        mort_neonat_menos1500 = rep("round(sum(obitos_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2)", 2),
        mort_neonat_precoc_menos1500 = rep("round(sum(obitos_6dias_menos1500)/sum(nascidos_menos1500) *1000, 2)", 2),
        mort_neonat_tardia_menos1500 = rep("round(sum(obitos_7_27dias_menos1500)/sum(nascidos_menos1500) *1000, 2)", 2),
        mort_neonat_1500_1999 = rep("round(sum(obitos_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2)", 2),
        mort_neonat_precoc_1500_1999 = rep("round(sum(obitos_6dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2)", 2),
        mort_neonat_tardia_1500_1999 = rep("round(sum(obitos_7_27dias_1500_1999)/sum(nascidos_1500_1999) *1000, 2)", 2),
        mort_neonat_2000_2499 = rep("round(sum(obitos_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2)", 2),
        mort_neonat_precoc_2000_2499 = rep("round(sum(obitos_6dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2)", 2),
        mort_neonat_tardia_2000_2499 = rep("round(sum(obitos_7_27dias_2000_2499)/sum(nascidos_2000_2499) *1000, 2)", 2),
        mort_neonat_mais2500 = rep("round(sum(obitos_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2)", 2),
        mort_neonat_precoc_mais2500 = rep("round(sum(obitos_6dias_mais2500)/sum(nascidos_mais2500) *1000, 2)", 2),
        mort_neonat_tardia_mais2500 = rep("round(sum(obitos_7_27dias_mais2500)/sum(nascidos_mais2500) *1000, 2)", 2),
        obitos_fetais = rep("sum(obitos_fetais_mais_22sem)", 2),
        fetal_peso_menos_1500 = rep("sum(fetal_peso_menos_1500)", 2),
        fetal_peso_1500_1999 = rep("sum(fetal_peso_1500_1999)", 2),
        fetal_peso_2000_2499 = rep("sum(fetal_peso_2000_2499)", 2),
        fetal_peso_mais_2500 = rep("sum(fetal_peso_mais_2500)", 2),
        fetal_antes = rep("sum(fetal_antes)", 2),
        fetal_durante = rep("sum(fetal_durante)", 2),
        fetal_depois = rep("sum(fetal_depois)", 2),
        fetal_antes_peso_menos_1500 = rep("sum(fetal_antes_peso_menos_1500)", 2),
        fetal_antes_peso_1500_1999 = rep("sum(fetal_antes_peso_1500_1999)", 2),
        fetal_antes_peso_2000_2499 = rep("sum(fetal_antes_peso_2000_2499)", 2),
        fetal_antes_peso_mais_2500 = rep("sum(fetal_antes_peso_mais_2500)", 2),
        fetal_durante_peso_menos_1500 = rep("sum(fetal_durante_peso_menos_1500)", 2),
        fetal_durante_peso_1500_1999 = rep("sum(fetal_durante_peso_1500_1999)", 2),
        fetal_durante_peso_2000_2499 = rep("sum(fetal_durante_peso_2000_2499)", 2),
        fetal_durante_peso_mais_2500 = rep("sum(fetal_durante_peso_mais_2500)", 2),
        fetal_depois_peso_menos_1500 = rep("sum(fetal_depois_peso_menos_1500)", 2),
        fetal_depois_peso_1500_1999 = rep("sum(fetal_depois_peso_1500_1999)", 2),
        fetal_depois_peso_2000_2499 = rep("sum(fetal_depois_peso_2000_2499)", 2),
        fetal_depois_peso_mais_2500 = rep("sum(fetal_depois_peso_mais_2500)", 2),
        taxa_mort_fetal = rep("round(sum(obitos_fetais_mais_22sem)/(sum(nascidos)+sum(obitos_fetais_mais_22sem)) *1000, 2)", 2),
        taxa_mort_fetal_peso_menos_1500 = rep("round(sum(fetal_peso_menos_1500)/(sum(nascidos_menos1500)+sum(fetal_peso_menos_1500)) *1000, 2)", 2),
        taxa_mort_fetal_peso_1500_1999 = rep("round(sum(fetal_peso_1500_1999)/(sum(nascidos_1500_1999)+sum(fetal_peso_1500_1999)) *1000, 2)", 2),
        taxa_mort_fetal_peso_2000_2499 = rep("round(sum(fetal_peso_2000_2499)/(sum(nascidos_2000_2499)+sum(fetal_peso_2000_2499)) *1000, 2)", 2),
        taxa_mort_fetal_peso_mais_2500 = rep("round(sum(fetal_peso_mais_2500)/(sum(nascidos_mais2500)+sum(fetal_peso_mais_2500)) *1000, 2)", 2),
        taxa_mort_fetal_antes = rep("round(sum(fetal_antes)/(sum(nascidos) + sum(fetal_antes)) *1000, 2)", 2),
        taxa_mort_fetal_durante = rep("round(sum(fetal_durante)/(sum(nascidos) + sum(fetal_durante)) *1000, 2)", 2),
        taxa_mort_fetal_depois = rep("round(sum(fetal_depois)/(sum(nascidos) + sum(fetal_depois)) *1000, 2)", 2),
        taxa_mort_fetal_antes_peso_menos_1500 = rep("round(sum(fetal_antes_peso_menos_1500)/(sum(nascidos_menos1500)+sum(fetal_antes_peso_menos_1500)) *1000, 2)", 2),
        taxa_mort_fetal_antes_peso_1500_1999 = rep("round(sum(fetal_antes_peso_1500_1999)/(sum(nascidos_1500_1999)+sum(fetal_antes_peso_1500_1999)) *1000, 2)", 2),
        taxa_mort_fetal_antes_peso_2000_2499 = rep("round(sum(fetal_antes_peso_2000_2499)/(sum(nascidos_2000_2499)+sum(fetal_antes_peso_2000_2499)) *1000, 2)", 2),
        taxa_mort_fetal_antes_peso_mais_2500 = rep("round(sum(fetal_antes_peso_mais_2500)/(sum(nascidos_mais2500)+sum(fetal_antes_peso_mais_2500)) *1000, 2)", 2),
        taxa_mort_fetal_durante_peso_menos_1500 = rep("round(sum(fetal_durante_peso_menos_1500)/(sum(nascidos_menos1500)+sum(fetal_durante_peso_menos_1500)) *1000, 2)", 2),
        taxa_mort_fetal_durante_peso_1500_1999 = rep("round(sum(fetal_durante_peso_1500_1999)/(sum(nascidos_1500_1999)+sum(fetal_durante_peso_1500_1999)) *1000, 2)", 2),
        taxa_mort_fetal_durante_peso_2000_2499 = rep("round(sum(fetal_durante_peso_2000_2499)/(sum(nascidos_2000_2499)+sum(fetal_durante_peso_2000_2499)) *1000, 2)", 2),
        taxa_mort_fetal_durante_peso_mais_2500 = rep("round(sum(fetal_durante_peso_mais_2500)/(sum(nascidos_mais2500)+sum(fetal_durante_peso_mais_2500)) *1000, 2)", 2),
        taxa_mort_fetal_depois_peso_menos_1500 = rep("round(sum(fetal_depois_peso_menos_1500)/(sum(nascidos_menos1500)+sum(fetal_depois_peso_menos_1500)) *1000, 2)", 2),
        taxa_mort_fetal_depois_peso_1500_1999 = rep("round(sum(fetal_depois_peso_1500_1999)/(sum(nascidos_1500_1999)+sum(fetal_depois_peso_1500_1999)) *1000, 2)", 2),
        taxa_mort_fetal_depois_peso_2000_2499 = rep("round(sum(fetal_depois_peso_2000_2499)/(sum(nascidos_2000_2499)+sum(fetal_depois_peso_2000_2499)) *1000, 2)", 2),
        taxa_mort_fetal_depois_peso_mais_2500 = rep("round(sum(fetal_depois_peso_mais_2500)/(sum(nascidos_mais2500)+sum(fetal_depois_peso_mais_2500)) *1000, 2)", 2),


        obitos_perinatal_total = rep("sum(obitos_fetais_mais_22sem) + sum(obitos_6dias)", 2),
         perinatal_total_menos1500 = rep("sum(fetal_peso_menos_1500) + sum(obitos_6dias_menos1500)", 2),
         perinatal_total_1500_1999 = rep("sum(fetal_peso_1500_1999) + sum(obitos_6dias_1500_1999)", 2),
         perinatal_total_2000_2499 = rep("sum(fetal_peso_2000_2499) + sum(obitos_6dias_2000_2499)", 2),
         perinatal_total_mais2500 = rep("sum(fetal_peso_mais_2500) + sum(obitos_6dias_mais2500)", 2),
         obitos_perinatal_oms = rep("sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias)", 2),
         perinatal_oms_menos1500 = rep("sum(peso_menos_1500_mais_28sem, na.rm = T) + sum(obitos_6dias_menos1500)", 2),
         perinatal_oms_1500_1999 = rep("sum(peso_1500_1999_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_1999)", 2),
         perinatal_oms_2000_2499 = rep("sum(peso_2000_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_2000_2499)", 2),
         perinatal_oms_mais2500 = rep("sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500)", 2),
         taxa_perinatal_total = rep("round((sum(obitos_fetais_mais_22sem) + sum(obitos_6dias))/(sum(obitos_fetais_mais_22sem) + sum(nascidos) )*1000, 2)", 2),
         taxa_perinatal_total_menos1500 = rep("round((sum(fetal_peso_menos_1500) + sum(obitos_6dias_menos1500))/(sum(fetal_peso_menos_1500)+ sum(nascidos_menos1500))*1000, 2)", 2),
         taxa_perinatal_total_1500_1999 = rep("round((sum(fetal_peso_1500_1999) + sum(obitos_6dias_1500_1999))/(sum(fetal_peso_1500_1999)+sum(nascidos_1500_1999))*1000, 2)", 2),
         taxa_perinatal_total_2000_2499 = rep("round((sum(fetal_peso_2000_2499)+sum(obitos_6dias_2000_2499))/(sum(fetal_peso_2000_2499)+sum(nascidos_2000_2499))*1000, 2)", 2),
         taxa_perinatal_total_mais2500 = rep("round((sum(fetal_peso_mais_2500)+sum(obitos_6dias_mais2500))/(sum(fetal_peso_mais_2500)+sum(nascidos_mais2500))*1000, 2)", 2),
        taxa_perinatal_oms = rep("round((sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias))/(sum(obitos_fetais_mais_28sem, na.rm=T) + sum(nascidos) )*1000, 2)", 2),
        taxa_perinatal_oms_menos1500 = rep("round((sum(peso_menos_1500_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1500))/(sum(peso_menos_1500_mais_28sem, na.rm=T)+ sum(nascidos_menos1500))*1000, 2)", 2),
        taxa_perinatal_oms_1500_1999 = rep("round((sum(peso_1500_1999_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_1999))/(sum(peso_1500_1999_mais_28sem, na.rm=T)+sum(nascidos_1500_1999))*1000, 2)", 2),
        taxa_perinatal_oms_2000_2499 = rep("round((sum(peso_2000_2499_mais_28sem, na.rm=T)+sum(obitos_6dias_2000_2499))/(sum(peso_2000_2499_mais_28sem, na.rm=T)+sum(nascidos_2000_2499))*1000, 2)", 2),
        taxa_perinatal_oms_mais2500 = rep("round((sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(obitos_6dias_mais2500))/(sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(nascidos_mais2500))*1000, 2)", 2),

        obitos_0dias = rep("sum(obitos_0dias)", 2),
        obitos_0dias_menos1500 = rep("sum(obitos_0dias_menos1500)", 2),
        obitos_0dias_1500_1999 = rep("sum(obitos_0dias_1500_1999)", 2),
        obitos_0dias_2000_2499 = rep("sum(obitos_0dias_2000_2499)", 2),
        obitos_0dias_mais2500 = rep("sum(obitos_0dias_mais2500)", 2),
        obitos_1_6dias = rep("sum(obitos_1_6dias)", 2),
        obitos_1_6dias_menos1500 = rep("sum(obitos_1_6dias_menos1500)", 2),
        obitos_1_6dias_1500_1999 = rep("sum(obitos_1_6dias_1500_1999)", 2),
        obitos_1_6dias_2000_2499 = rep("sum(obitos_1_6dias_2000_2499)", 2),
        obitos_1_6dias_mais2500 = rep("sum(obitos_1_6dias_mais2500)", 2),
        obitos_6dias = rep("sum(obitos_6dias)", 2),
        obitos_6dias_menos1500 = rep("sum(obitos_6dias_menos1500)", 2),
        obitos_6dias_1500_1999 = rep("sum(obitos_6dias_1500_1999)", 2),
        obitos_6dias_2000_2499 = rep("sum(obitos_6dias_2000_2499)", 2),
        obitos_6dias_mais2500 = rep("sum(obitos_6dias_mais2500)", 2),
        obitos_27dias = rep("sum(obitos_27dias)", 2),
        obitos_27dias_menos1500 = rep("sum(obitos_27dias_menos1500)", 2),
        obitos_27dias_1500_1999 = rep("sum(obitos_27dias_1500_1999)", 2),
        obitos_27dias_2000_2499 = rep("sum(obitos_27dias_2000_2499)", 2),
        obitos_27dias_mais2500 = rep("sum(obitos_27dias_mais2500)", 2),
        obitos_7_27dias = rep("sum(obitos_7_27dias)", 2),
        obitos_7_27dias_menos1500 = rep("sum(obitos_7_27dias_menos1500)", 2),
        obitos_7_27dias_1500_1999 = rep("sum(obitos_7_27dias_1500_1999)", 2),
        obitos_7_27dias_2000_2499 = rep("sum(obitos_7_27dias_2000_2499)", 2),
        obitos_7_27dias_mais2500 = rep("sum(obitos_7_27dias_mais2500)", 2),

        antes_dist_moment_obito_fetal = rep("round(
        sum(c(fetal_antes_peso_menos_1500, fetal_antes_peso_1500_1999, fetal_antes_peso_2000_2499, fetal_antes_peso_mais_2500, fetal_antes)[seleciona(aba = 'fetal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_fetal) %in% input$faixa_peso_dist_moment_obit_fetal])/
          sum(c(fetal_peso_menos_1500, fetal_peso_1500_1999, fetal_peso_2000_2499, fetal_peso_mais_2500, obitos_fetais)[seleciona(aba = 'fetal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_fetal) %in% input$faixa_peso_dist_moment_obit_fetal])
        *100, 2)", 2),

        durante_dist_moment_obito_fetal = rep("round(
        sum(c(fetal_durante_peso_menos_1500, fetal_durante_peso_1500_1999, fetal_durante_peso_2000_2499, fetal_durante_peso_mais_2500, fetal_durante)[seleciona(aba = 'fetal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_fetal) %in% input$faixa_peso_dist_moment_obit_fetal])/
          sum(c(fetal_peso_menos_1500, fetal_peso_1500_1999, fetal_peso_2000_2499, fetal_peso_mais_2500, obitos_fetais)[seleciona(aba = 'fetal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_fetal) %in% input$faixa_peso_dist_moment_obit_fetal])
        *100, 2)", 2),

        faltante_dist_moment_obito_fetal = rep("round(100-antes_dist_moment_obito_fetal-durante_dist_moment_obito_fetal, 2)", 2),

        antes_dist_moment_obito_perinat = rep("round(
        sum(c(perinatal_antes_peso_menos_1500, perinatal_antes_peso_1500_1999, perinatal_antes_peso_2000_2499, perinatal_antes_peso_mais_2500, perinatal_antes)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])/
          sum(c(perinatal_oms_menos1500, perinatal_oms_1500_1999, perinatal_oms_2000_2499, perinatal_oms_mais2500, obitos_perinatal_oms)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])
        *100, 2)", 2),

        durante_dist_moment_obito_perinat = rep("round(
        sum(c(perinatal_durante_peso_menos_1500, perinatal_durante_peso_1500_1999, perinatal_durante_peso_2000_2499, perinatal_durante_peso_mais_2500, perinatal_durante)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])/
          sum(c(perinatal_oms_menos1500, perinatal_oms_1500_1999, perinatal_oms_2000_2499, perinatal_oms_mais2500, obitos_perinatal_oms)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])
        *100, 2)", 2),

        dia_0_dist_moment_obito_perinat = rep("round(
        sum(c(obitos_0dias_menos1500, obitos_0dias_1500_1999, obitos_0dias_2000_2499, obitos_0dias_mais2500, obitos_0dias)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])/
          sum(c(perinatal_oms_menos1500, perinatal_oms_1500_1999, perinatal_oms_2000_2499, perinatal_oms_mais2500, obitos_perinatal_oms)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])
        *100, 2)", 2),

        dia_1_6_dist_moment_obito_perinat = rep("round(
        sum(c(obitos_1_6dias_menos1500, obitos_0dias_1500_1999, obitos_1_6dias_2000_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])/
          sum(c(perinatal_oms_menos1500, perinatal_oms_1500_1999, perinatal_oms_2000_2499, perinatal_oms_mais2500, obitos_perinatal_oms)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])
        *100, 2)", 2),

        faltante_dist_moment_obito_perinat = rep("round(100 -antes_dist_moment_obito_perinat -durante_dist_moment_obito_perinat -dia_0_dist_moment_obito_perinat -dia_1_6_dist_moment_obito_perinat, 2)", 2),

        dia_0_dist_moment_obito_neonat = rep("round(
        sum(c(obitos_0dias_menos1500, obitos_0dias_1500_1999, obitos_0dias_2000_2499, obitos_0dias_mais2500, obitos_0dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat])/
          sum(c(obitos_27dias_menos1500, obitos_27dias_1500_1999, obitos_27dias_2000_2499, obitos_27dias_mais2500, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat])
        *100, 2)", 2),

        dia_1_6dist_moment_obito_neonat = rep("round(
        sum(c(obitos_1_6dias_menos1500, obitos_1_6dias_1500_1999, obitos_1_6dias_2000_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat])/
          sum(c(obitos_27dias_menos1500, obitos_27dias_1500_1999, obitos_27dias_2000_2499, obitos_27dias_mais2500, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat])
        *100, 2)", 2),

        dia_7_27dist_moment_obito_neonat = rep("round(
        sum(c(obitos_7_27dias_menos1500, obitos_7_27dias_1500_1999, obitos_7_27dias_2000_2499, obitos_7_27dias_mais2500, obitos_7_27dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat])/
          sum(c(obitos_27dias_menos1500, obitos_27dias_1500_1999, obitos_27dias_2000_2499, obitos_27dias_mais2500, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_neonat) %in% input$faixa_peso_dist_moment_obit_neonat])
        *100, 2)", 2),

        faltante_moment_obito_neonat = rep("round(100 -dia_0_dist_moment_obito_neonat -dia_1_6dist_moment_obito_neonat -dia_7_27dist_moment_obito_neonat, 2)", 2),

        menos_1500_dist_peso_fetal = rep("round(
        sum(c(fetal_antes_peso_menos_1500, fetal_durante_peso_menos_1500, fetal_peso_menos_1500)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal])/
          sum(c(fetal_antes, fetal_durante, obitos_fetais)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal])
        *100, 2)", 2),

        de_1500_1999_dist_peso_fetal = rep("round(
        sum(c(fetal_antes_peso_1500_1999, fetal_durante_peso_1500_1999, fetal_peso_1500_1999)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal])/
          sum(c(fetal_antes, fetal_durante, obitos_fetais)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal])
        *100, 2)", 2),

        de_2000_2499_dist_peso_fetal = rep("round(
        sum(c(fetal_antes_peso_2000_2499, fetal_durante_peso_2000_2499, fetal_peso_2000_2499)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal])/
          sum(c(fetal_antes, fetal_durante, obitos_fetais)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal])
        *100, 2)", 2),

        mais_2500_dist_peso_fetal = rep("round(
        sum(c(fetal_antes_peso_mais_2500, fetal_durante_peso_mais_2500, fetal_peso_mais_2500)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal])/
          sum(c(fetal_antes, fetal_durante, obitos_fetais)[seleciona(aba = 'fetal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_fetal) %in% input$momento_obito_dist_peso_fetal])
        *100, 2)", 2),

        faltante_dist_peso_fetal = rep("round(100 -menos_1500_dist_peso_fetal-de_1500_1999_dist_peso_fetal-de_2000_2499_dist_peso_fetal -mais_2500_dist_peso_fetal, 2)", 2),

        menos_1500_dist_peso_perinat = rep("round(
        sum(c(perinatal_antes_peso_menos_1500, perinatal_durante_peso_menos_1500, obitos_0dias_menos1500, obitos_1_6dias_menos1500, perinatal_oms_menos1500)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])/
          sum(c(perinatal_antes, perinatal_durante, obitos_0dias, obitos_1_6dias, obitos_perinatal_oms)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])
        *100, 2)", 2),

        de_1500_1999_dist_peso_perinat = rep("round(
        sum(c(perinatal_antes_peso_1500_1999, perinatal_durante_peso_1500_1999, obitos_0dias_1500_1999, obitos_1_6dias_1500_1999, perinatal_oms_1500_1999)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])/
          sum(c(perinatal_antes, perinatal_durante, obitos_0dias, obitos_1_6dias, obitos_perinatal_oms)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])
        *100, 2)", 2),


        de_2000_2499_dist_peso_perinat = rep("round(
        sum(c(perinatal_antes_peso_2000_2499, perinatal_durante_peso_2000_2499, obitos_0dias_2000_2499, obitos_1_6dias_2000_2499, perinatal_oms_2000_2499)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])/
          sum(c(perinatal_antes, perinatal_durante, obitos_0dias, obitos_1_6dias, obitos_perinatal_oms)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])
        *100, 2)", 2),

        mais_2500_dist_peso_perinat = rep("round(
        sum(c(perinatal_antes_peso_mais_2500 , perinatal_durante_peso_mais_2500, obitos_0dias_mais2500, obitos_1_6dias_mais2500, perinatal_oms_mais2500)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])/
          sum(c(perinatal_antes, perinatal_durante, obitos_0dias, obitos_1_6dias, obitos_perinatal_oms)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])
        *100, 2)", 2),

        faltante_dist_peso_perinat = rep("round(100 -menos_1500_dist_peso_perinat -de_1500_1999_dist_peso_perinat -de_2000_2499_dist_peso_perinat -mais_2500_dist_peso_perinat, 2)", 2),


        menos_1500_dist_peso_neonat = rep("round(
        sum(c(obitos_0dias_menos1500, obitos_1_6dias_menos1500, obitos_7_27dias_menos1500, obitos_27dias_menos1500)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat])/
          sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat])
        *100, 2)", 2),

        de_1500_1999_dist_peso_neonat = rep("round(
        sum(c(obitos_0dias_1500_1999, obitos_1_6dias_1500_1999, obitos_7_27dias_1500_1999, obitos_27dias_1500_1999)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat])/
          sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat])
        *100, 2)", 2),

        de_2000_2499_dist_peso_neonat = rep("round(
        sum(c(obitos_0dias_2000_2499, obitos_1_6dias_2000_2499, obitos_7_27dias_2000_2499, obitos_27dias_2000_2499)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat])/
          sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat])
        *100, 2)", 2),

        mais_2500_dist_peso_neonat = rep("round(
        sum(c(obitos_0dias_mais2500, obitos_1_6dias_mais2500, obitos_7_27dias_mais2500, obitos_27dias_mais2500)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat])/
          sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[seleciona(aba = 'neonatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_neonat) %in% input$momento_obito_dist_peso_neonat])
        *100, 2)", 2),

        faltante_dist_peso_neonat = rep("round(100 -menos_1500_dist_peso_neonat -de_1500_1999_dist_peso_neonat -de_2000_2499_dist_peso_neonat -mais_2500_dist_peso_neonat, 2)", 2),


        porc_condicoes_ameacadoras = rep("round(sum(nascidos_condicoes_ameacadoras) / sum(nascidos) * 100, 1)", 2),
        #porc_internacoes_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)", 2),
        porc_internacoes_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)", 2),
        porc_internacoes_uti_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral_internado_uti) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)", 2)
      )


       if (is.null(input$idade_dias_sih[1])) {
         df_calcs_aux3 <- data.frame(
           tipo = c("local", "referencia"),
           porc_internacoes_menores_28_dias_sih = "NA"
         )
       } else {
         df_calcs_aux3 <- data.frame(
           tipo = c("local", "referencia"),
           porc_internacoes_menores_28_dias_sih = rep(glue::glue(
           #   " if(length(input$idade_dias_sih) == 1){
           #   round(sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_sih[1]}[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)
           #   }
           #   else if(length(input$idade_dias_sih) == 2){
           #   round((sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_sih[1]}[ano <= 2022]) + sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_sih[2]}[ano <= 2022])) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)
           #   } else{
           #   round(sum(internacoes_{input$local_internacao_sih}_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1),
           # } "

             "ifelse(
               length(input$idade_dias_sih) == 3,
               round(sum(internacoes_{input$local_internacao_sih}_geral) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
               ifelse(
               length(input$idade_dias_sih == 2),
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
           porc_internacoes_uti_menores_28_dias_sih = "NA"
         )
       } else {
         df_calcs_aux4 <- data.frame(
           tipo = c("local", "referencia"),
           porc_internacoes_uti_menores_28_dias_sih = rep(glue::glue(

             # "if(length(input$idade_dias_sih) == 1){
             # round(sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_uti_sih[1]}_internado_uti[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)
             # } else if(length(input$idade_dias_sih) == 2){
             # round((sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_uti_sih[1]}_internado_uti[ano <= 2022]) + sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_uti_sih[2]}_internado_uti[ano <= 2022])) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)
             # } else{
             # round(sum(internacoes_{input$local_internacao_sih}_geral_internado_uti[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1),
             #
             # }
             # "
              "ifelse(
                length(input$idade_dias_sih) == 3,
                round(sum(internacoes_{input$local_internacao_sih}_geral_internado_uti) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
                ifelse(
                length(input$idade_dias_sih) == 2,
                round((sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_uti_sih[1]}_internado_uti) + sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_uti_sih[2]}_internado_uti)) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1),
                round(sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_uti_sih[1]}_internado_uti) / sum(nascidos_estabelecimentos_publicos_sih) * 100, 1)
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


    ## Criando os outputs que receberão os nomes dos locais selecionados quando há comparação --------
    output$input_localidade_resumo_fetal <- renderUI({
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

    ## Para os botões de alerta quanto à incompletude e cobertura --------------

    ### Calculando os indicadores de incompletude ------------------------------
    data_incompletude_aux <- reactive({
      base_incompletude |>
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
        dplyr::group_by(ano) |>
        dplyr::summarise(
          peso = round(sum(peso_incompletos, na.rm = TRUE) / sum(peso_totais, na.rm = TRUE) * 100, 2),
          gestacao = round(sum(gestacao_incompletos, na.rm = TRUE) / sum(gestacao_totais, na.rm = TRUE) * 100, 2),
          semagestac = round(sum(semagestac_incompletos, na.rm = TRUE) / sum(semagestac_totais, na.rm = TRUE) * 100, 2),
          idanomal = round(sum(idanomal_incompletos, na.rm = TRUE) / sum(idanomal_totais,na.rm = TRUE) *100, 2),
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

    ### Calculando os indicadores de cobertura --------------------------------
    data_cobertura <- reactive({
      if (filtros()$nivel == "Municipal") {
        sub_registro_sinasc_muni_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "Estadual") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$estado
          )
      } else if (filtros()$nivel == "Regional") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$regiao
          )
      } else if (filtros()$nivel == "Nacional") {
        sub_registro_sinasc_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == "Brasil"
          )
      } else {
        data.frame(
          ano = filtros()$ano2[1]:filtros()$ano2[2],
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

    ### Juntando os dados de incompletude e cobertura -------------------------
    data_incompletude <- reactive({dplyr::full_join(data_incompletude_aux(), data_cobertura(), by = c("ano", "localidade"))})


    #### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao7", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$gestacao > 5, na.rm = TRUE) |
            any(data_incompletude()$peso > 5, na.rm = TRUE) |
            any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao7", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao7, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$peso,
        variavel_incompletude1 = "PESO",
        descricao_incompletude1 = "em branco ou ignorados",
        incompletude2 = data_incompletude()$gestacao,
        variavel_incompletude2 = "GESTACAO",
        descricao_incompletude2 = "em branco ou ignorados",
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
          if (nivel_selecionado() == "Nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
          else if (nivel_selecionado() == "Regional")
            regiao == filtros()[[paste0("regiao", sufixo_inputs)]]
          else if (nivel_selecionado() == "Estadual")
            uf == filtros()[[paste0("estado", sufixo_inputs)]]
          else if (nivel_selecionado() == "Macrorregião de saúde")
            macro_r_saude == filtros()[[paste0("macro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_macro", sufixo_inputs)]]
          else if(nivel_selecionado() == "Microrregião de saúde")
            r_saude == filtros()[[paste0("micro", sufixo_inputs)]] & uf == filtros()[[paste0("estado_micro", sufixo_inputs)]]
          else if(nivel_selecionado() == "Municipal")
            municipio == filtros()[[paste0("municipio", sufixo_inputs)]] & uf == filtros()[[paste0("estado_municipio", sufixo_inputs)]]
          else if (nivel_selecionado() == "Municípios semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        cria_indicadores(df_calcs = bloco7_calcs(), bloco  = "bloco7", input = input, filtros = filtros(), adicionar_localidade = FALSE) |>
        dplyr::mutate(
          localidade = dplyr::case_when(
            nivel_selecionado() == "Nacional" ~ "Brasil",
            nivel_selecionado() == "Regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
            nivel_selecionado() == "Estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
            nivel_selecionado() == "Macrorregião de saúde" ~ filtros()[[paste0("macro", sufixo_inputs)]],
            nivel_selecionado() == "Microrregião de saúde" ~ filtros()[[paste0("micro", sufixo_inputs)]],
            nivel_selecionado() == "Municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]],
            nivel_selecionado() == "Municípios semelhantes" ~ "Média dos municípios semelhantes"
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
        cria_indicadores(df_calcs = bloco7_calcs(), bloco  = "bloco7", input = input,  filtros = filtros(), referencia = TRUE)
    })

    # Não queremos que as caixinhas se atualizem quando os inputs dos gráficos de distribuição percentual mudarem
    data7_resumo_referencia <- eventReactive(
      c(filtros()$pesquisar, input$tabset1, input$tabset2, input$tabset3, input$tabset4, input$localidade_resumo_fetal, input$localidade_resumo_neonat, input$localidade_resumo_perinatal, input$localidade_resumo_morbidade_neonatal),
      data7_resumo_referencia_aux(),
      ignoreNULL = FALSE
    )

    ## Criando os outputs das caixinhas ---------------------------------------
    ### Para os indicadores de mortalidade fetal ------------------------------
    #### Número de óbitos fetais ----------------------------------------------
    numero_obitos_fetais <- reactive({
      dplyr::case_when(
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "peso_fetal") ~ "obitos_fetais",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_menos1500") ~ "fetal_peso_menos_1500",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "fetal_peso_1500_1999",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "fetal_peso_2000_2499",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_peso_mais_2500",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_antes",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_menos1500") ~ "fetal_antes_peso_menos_1500",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "fetal_antes_peso_1500_1999",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "fetal_antes_peso_2000_2499",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_antes_peso_mais_2500",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_durante",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_menos1500") ~ "fetal_durante_peso_menos_1500",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "fetal_durante_peso_1500_1999",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "fetal_durante_peso_2000_2499",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_durante_peso_mais_2500",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "peso_fetal") ~ "fetal_depois",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_menos1500") ~ "fetal_depois_peso_menos_1500",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "fetal_depois_peso_1500_1999",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "fetal_depois_peso_2000_2499",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_mais2500") ~ "fetal_depois_peso_mais_2500",
      )
    })

    titulo_obitos_fetais <- reactive({
      dplyr::case_when(
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais (geral)",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_menos1500") ~ "Número de óbitos fetais com peso menor que 1500 g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "Número de óbitos fetais com peso de 1500 a 1999 g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "Número de óbitos fetais com peso de 2000 a 2499 g",
        (input$parto_fetal == "fetal_parto_geral" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais com peso maior ou igual a 2500 g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais antes do trabalho de parto",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_menos1500") ~ "Número de óbitos fetais antes do trabalho de parto com peso menor que 1500 g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "Número de óbitos fetais antes do trabalho de parto com peso de 1500 a 1999 g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "Número de óbitos fetais antes do trabalho de parto com peso de 2000 a 2499 g",
        (input$parto_fetal == "antes" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais antes do trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais durante o trabalho de parto",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_menos1500") ~ "Número de óbitos fetais durante o trabalho de parto com peso menor que 1500 g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "Número de óbitos fetais durante o trabalho de parto com peso de 1500 a 1999 g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "Número de óbitos fetais durante o trabalho de parto com peso de 2000 a 2499 g",
        (input$parto_fetal == "durante" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais durante o trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "peso_fetal") ~ "Número de óbitos fetais depois do trabalho de parto",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_menos1500") ~ "Número de óbitos fetais depois do trabalho de parto com peso menor que 1500 g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_1500_1999") ~ "Número de óbitos fetais depois do trabalho de parto com peso de 1500 a 1999 g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_2000_2499") ~ "Número de óbitos fetais depois do trabalho de parto com peso de 2000 a 2499 g",
        (input$parto_fetal == "depois" & input$faixa_peso_fetal == "fetal_mais2500") ~ "Número de óbitos fetais depois do trabalho de parto com peso maior ou igual a 2500 g",
      )
    })

    output$caixa_b7_fetal_i1 <- renderUI({
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
          nivel_selecionado() == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "330px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })


    #### Taxa de mortalidade fetal --------------------------------------------
    taxa_mortalidade_fetal <- reactive({
      dplyr::case_when(
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "peso_fetal") ~ "taxa_mort_fetal",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_menos1500") ~ "taxa_mort_fetal_peso_menos_1500",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1500_1999") ~ "taxa_mort_fetal_peso_1500_1999",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_2000_2499") ~ "taxa_mort_fetal_peso_2000_2499",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "taxa_mort_fetal_peso_mais_2500",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "peso_fetal") ~ "taxa_mort_fetal_antes",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_menos1500") ~ "taxa_mort_fetal_antes_peso_menos_1500",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1500_1999") ~ "taxa_mort_fetal_antes_peso_1500_1999",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_2000_2499") ~ "taxa_mort_fetal_antes_peso_2000_2499",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "taxa_mort_fetal_antes_peso_mais_2500",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "peso_fetal") ~ "taxa_mort_fetal_durante",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_menos1500") ~ "taxa_mort_fetal_durante_peso_menos_1500",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1500_1999") ~ "taxa_mort_fetal_durante_peso_1500_1999",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_2000_2499") ~ "taxa_mort_fetal_durante_peso_2000_2499",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "taxa_mort_fetal_durante_peso_mais_2500",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "peso_fetal") ~ "taxa_mort_fetal_depois",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_menos1500") ~ "taxa_mort_fetal_depois_peso_menos_1500",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1500_1999") ~ "taxa_mort_fetal_depois_peso_1500_1999",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_2000_2499") ~ "taxa_mort_fetal_depois_peso_2000_2499",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "taxa_mort_fetal_depois_peso_mais_2500",
      )
    })

    titulo_taxa_mortalidade_fetal <- reactive({
      dplyr::case_when(
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "peso_fetal") ~ "Taxa de mortalidade fetal (geral)",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_menos1500") ~ "Taxa de mortalidade fetal com peso menor que 1500 g",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_1500_1999") ~ "Taxa de mortalidade fetal com peso de 1500 a 1999 g",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_2000_2499") ~ "Taxa de mortalidade fetal com peso de 2000 a 2499 g",
        (input$parto_fetal2 == "fetal_parto_geral" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "Taxa de mortalidade fetal com peso maior ou igual a 2500 g",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "peso_fetal") ~ "Taxa de mortalidade fetal antes do trabalho de parto",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_menos1500") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso menor que 1500 g",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_1500_1999") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso de 1500 a 1999 g",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_2000_2499") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso de 2000 a 2499 g",
        (input$parto_fetal2 == "antes" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "Taxa de mortalidade fetal antes do trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "peso_fetal") ~ "Taxa de mortalidade fetal durante o trabalho de parto",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_menos1500") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso menor que 1500 g",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_1500_1999") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso de 1500 a 1999 g",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_2000_2499") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso de 2000 a 2499 g",
        (input$parto_fetal2 == "durante" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "Taxa de mortalidade fetal durante o trabalho de parto com peso maior ou igual a 2500 g",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "peso_fetal") ~ "Taxa de mortalidade fetal depois do trabalho de parto",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_menos1500") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso menor que 1500 g",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_1500_1999") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso de 1500 a 1999 g",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_2000_2499") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso de 2000 a 2499 g",
        (input$parto_fetal2 == "depois" & input$faixa_peso_fetal2 == "fetal_mais2500") ~ "Taxa de mortalidade fetal depois do trabalho de parto com peso maior ou igual a 2500 g",
      )
    })

    output$caixa_b7_fetal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = taxa_mortalidade_fetal(),
        titulo = titulo_taxa_mortalidade_fetal(),
        tem_meta = FALSE,
        valor_de_referencia = dplyr::if_else(data7_resumo_referencia()[[taxa_mortalidade_fetal()]] >0 ,
                                             data7_resumo_referencia()[[taxa_mortalidade_fetal()]], NaN),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$caixa_b7_fetal_i3 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "fetal peso por idade gestacional",
        titulo = "Dentre os óbitos fetais (geral),",
        tamanho_caixa = "330px"
      )
    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$caixa_b7_fetal_i4 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "fetal momento do obito por peso",
        titulo = "Dentre os óbitos fetais (geral),",
        tamanho_caixa = "330px"
      )
    })


    ### Para os indicadores de mortalidade neonatal ---------------------------
    #### Taxa de mortalidade neonatal por 1000 nascidos vivos -----------------
    titulo_caixa_neonat <- reactive({
      dplyr::case_when(
        input$faixa_peso == "mort_neonat" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos",
        input$faixa_peso == "mort_neonat_menos1500" ~ "Mortalidade neonatatl por 1000 nascidos vivos para peso ao nascer menor que 1500 g",
        input$faixa_peso == "mort_neonat_1500_1999" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos para peso ao nascer de 1500 a 1999 g",
        input$faixa_peso == "mort_neonat_2000_2499" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos para peso ao nascer de 2000 a 2499 g",
        input$faixa_peso == "mort_neonat_mais2500" ~ "Taxa de mortalidade neonatal por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500 g"
      )
    })

    output$caixa_b7_neonat_i1 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso,
        titulo = titulo_caixa_neonat(),
        tem_meta = FALSE,
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
        input$faixa_peso_precoc == "mort_neonat_precoc" ~ "Taxa de mortalidade neonatal precoce por 1000 nascidos vivos",
        input$faixa_peso_precoc == "mort_neonat_precoc_menos1500" ~ "Taxa de mortalidade neonatal precoce por 1000 nascidos vivos para peso ao nascer menor que 1500 g",
        input$faixa_peso_precoc == "mort_neonat_precoc_1500_1999" ~ "Taxa de mortalidade neonatal precoce por 1000 nascidos vivos para peso ao nascer de 1500 a 1999 g",
        input$faixa_peso_precoc == "mort_neonat_precoc_2000_2499" ~ "Taxa de mortalidade neonatal precoce por 1000 nascidos vivos para peso ao nascer de 2000 a 2499 g",
        input$faixa_peso_precoc == "mort_neonat_precoc_mais2500" ~ "Taxa de mortalidade neonatal precoce por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500 g"
      )
    })

    output$caixa_b7_neonat_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_precoc,
        titulo = titulo_caixa_neonat_precoc(),
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia()[[input$faixa_peso_precoc]],
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
        input$faixa_peso_tardia == "mort_neonat_tardia" ~ "Taxa de mortalidade neonatal tardia por 1000 nascidos vivos",
        input$faixa_peso_tardia == "mort_neonat_tardia_menos1500" ~ "Taxa de mortalidade neonatal tardia por 1000 nascidos vivos para peso ao nascer menor que 1500 g",
        input$faixa_peso_tardia == "mort_neonat_tardia_1500_1999" ~ "Taxa de mortalidade neonatal tardia por 1000 nascidos vivos para peso ao nascer de 1500 a 1999 g",
        input$faixa_peso_tardia == "mort_neonat_tardia_2000_2499" ~ "Taxa de mortalidade neonatal tardia por 1000 nascidos vivos para peso ao nascer de 2000 a 2499 g",
        input$faixa_peso_tardia == "mort_neonat_tardia_mais2500" ~ "Taxa de mortalidade neonatal tardia por 1000 nascidos vivos para peso ao nascer maior ou igual a 2500 g"
      )
    })

    output$caixa_b7_neonat_i3 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_tardia,
        titulo = titulo_caixa_neonat_tardia(),
        tem_meta = FALSE,
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
        input$obitos_faixa_peso == "obitos_neonat_menos1500" ~ "Número de óbitos neonatais para peso ao nascer menor que 1500 g",
        input$obitos_faixa_peso == "obitos_neonat_1500_1999" ~ "Número de óbitos neonatais para peso ao nascer de 1500 a 1999 g",
        input$obitos_faixa_peso == "obitos_neonat_2000_2499" ~ "Número de óbitos neonatais para peso ao nascer de 2000 a 2499 g",
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
          nivel_selecionado() == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
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
        titulo = "Dentre os óbitos neonatais (geral),",
        tamanho_caixa = "330px"
      )
    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$caixa_b7_neonat_i6 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7_resumo(),
        indicador = "neonatal peso por momento do obito",
        titulo = "Dentre os óbitos neonatais (geral),",
        tamanho_caixa = "330px"
      )
    })


    ### Para os indicadores de mortalidade perinatal --------------------------
    #### Número de óbitos perinatais (definição 1) ----------------------------
    titulo_caixa_perinatal_total <- reactive({
      dplyr::case_when(
        input$faixa_peso_perinatal_total == "obitos_perinatal_total" ~ "Número de óbitos perinatais (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_total == "perinatal_total_menos1500" ~ "Número de óbitos perinatais com peso menor que 1500 g (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_total == "perinatal_total_1500_1999" ~ "Número de óbitos perinatais com peso de 1500 a 1999 g (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_total == "perinatal_total_2000_2499" ~ "Número de óbitos perinatais com peso de 2000 a 2499 g (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_total == "perinatal_total_mais2500" ~ "Número de óbitos perinatais com peso maior ou igual a 2500 g (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
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
          nivel_selecionado() == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Número de óbitos perinatais (definição 2) ----------------------------
    titulo_caixa_perinatal_oms <- reactive({
      dplyr::case_when(
        input$faixa_peso_perinatal_oms == "obitos_perinatal_oms" ~ "Número de óbitos perinatais (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_oms == "perinatal_oms_menos1500" ~ "Número de óbitos perinatais com peso menor que 1500 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_oms == "perinatal_oms_1500_1999" ~ "Número de óbitos perinatais com peso de 1500 a 1999 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_oms == "perinatal_oms_2000_2499" ~ "Número de óbitos perinatais com peso de 2000 a 2499 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_oms == "perinatal_oms_mais2500" ~ "Número de óbitos perinatais com peso maior ou igual a 2500 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
      )
    })

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
          nivel_selecionado() == "Nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 2), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Taxa de mortalidade perinatal (definição 1) --------------------------
    titulo_caixa_taxa_perinatal_total <- reactive({
      dplyr::case_when(
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total" ~ "Taxa de mortalidade perinatal (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_menos1500" ~ "Taxa de mortalidade perinatal com peso menor que 1500 g (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_1500_1999" ~ "Taxa de mortalidade perinatal com peso de 1500 a 1999 g (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_2000_2499" ~ "Taxa de mortalidade perinatal com peso de 2000 a 2499 g (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_taxa_total == "taxa_perinatal_total_mais2500" ~ "Taxa de mortalidade perinatal com peso maior ou igual a 2500 g (feto com idade gestacional maior ou igual a 22 semanas ou peso maior ou igual a 500g ou neonatal com até 6 dias de vida)",
      )
    })

    output$caixa_b7_perinatal_i3 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_perinatal_taxa_total,
        titulo = titulo_caixa_taxa_perinatal_total(),
        tem_meta = FALSE,
        valor_de_referencia = dplyr::if_else(data7_resumo_referencia()[[input$faixa_peso_perinatal_taxa_total]] >0 ,
                                             data7_resumo_referencia()[[input$faixa_peso_perinatal_taxa_total]], NaN),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "350px",
        pagina = "bloco_7",
        nivel_de_analise = nivel_selecionado()
      )
    })

    #### Taxa de mortalidade perinatal (definição 2) --------------------------
    titulo_caixa_taxa_perinatal_oms <- reactive({
      dplyr::case_when(
        input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms" ~ "Taxa de mortalidade perinatal (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_menos1500" ~ "Taxa de mortalidade perinatal com peso menor que 1500 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_1500_1999" ~ "Taxa de mortalidade perinatal com peso de 1500 a 1999 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_2000_2499" ~ "Taxa de mortalidade perinatal com peso de 2000 a 2499 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
        input$faixa_peso_perinatal_taxa_oms == "taxa_perinatal_oms_mais2500" ~ "Taxa de mortalidade perinatal com peso maior ou igual a 2500 g (feto com idade gestacional maior ou igual a 28 semanas ou peso maior ou igual a 1000g ou neonatal com até 6 dias de vida)",
      )
    })

    output$caixa_b7_perinatal_i4 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = input$faixa_peso_perinatal_taxa_oms,
        titulo = titulo_caixa_taxa_perinatal_oms(),
        tem_meta = FALSE,
        valor_de_referencia = dplyr::if_else(data7_resumo_referencia()[[input$faixa_peso_perinatal_taxa_oms]] >0 ,
                                             data7_resumo_referencia()[[input$faixa_peso_perinatal_taxa_oms]], NaN),
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
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    ### Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS  -----------
    output$caixa_b7_morbidade_neonatal_i3 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = "porc_internacoes_uti_menores_28_dias_sih_geral",
        titulo = "Porcentagem de internações neonatais em UTI no SUS ocorridas até o 27º dia de vida (geral)",
        tem_meta = TRUE,
        valor_de_referencia = data7_resumo_referencia()$porc_internacoes_uti_menores_28_dias_sih_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })


    ### Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos com vínculo com o SUS ----
    output$caixa_b7_morbidade_neonatal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7_resumo(),
        indicador = "porc_internacoes_menores_28_dias_sih_geral",
        titulo = "Porcentagem de internações neonatais no SUS ocorridas até o 27º dia de vida (geral)",
        tem_meta = TRUE,
        valor_de_referencia = data7_resumo_referencia()$porc_internacoes_menores_28_dias_sih_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        fonte_titulo = "15px",
        pagina = "bloco_5",
        tipo_referencia = "média nacional",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    # Para os gráficos --------------------------------------------------------
    cols <- c("#2c115f", "#b73779", "#fc8961")

    ### Para a distribuição de internações por grupos de causas

    data_filtrada_morbidade_aux <- reactive({
      bloco7_dist_morbidade |>
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

    data_filtrada_comp_morbidade_aux <- reactive({
      bloco7_dist_morbidade |>
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

    data_plot_grupos_morbidade_neonatal <- reactive({
      data_filtrada_morbidade_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos))))) |>
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período neonatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Infecções", "Prematuridade", "Afecções respiratórias do recém-nascido",
                                                                   "Fatores maternos relacionados à gravidez", "Afecções originais no período neonatal",
                                                                   "Malformação congênita","Asfixia/Hipóxia","Mal definidas", "Grupos não selecionados","Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_morbidade_neonatal_comp <- reactive({
      data_filtrada_comp_morbidade_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(input = input$momento_internacao_neonatal_grupos))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período neonatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
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
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Infecções", "Prematuridade", "Afecções respiratórias do recém-nascido",
                                                                   "Fatores maternos relacionados à gravidez", "Afecções originais no período neonatal",
                                                                   "Malformação congênita","Asfixia/Hipóxia","Mal definidas", "Grupos não selecionados", "Demais causas")),
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
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$momento_internacao_neonatal_grupos))))) |>
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período neonatal",
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
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Infecções", "Prematuridade", "Afecções respiratórias do recém-nascido",
                                                                   "Fatores maternos relacionados à gravidez", "Afecções originais no período neonatal",
                                                                   "Malformação congênita","Asfixia/Hipóxia","Mal definidas", "Grupos não selecionados", "Demais causas")),
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
    ### Para os gráficos de causas evitáveis ----------------------------------
    #### Para a localidade selecionada ----------------------------------------
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

    data_plot_evitaveis_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & !dplyr::contains("2") )| "obitos_fetais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis)),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          grupo_cid10 = factor(
            grupo_cid10,
            levels = c(
              "Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
              "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
              "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas"
            )
          ),
          ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
        )
    })

    data_plot_evitaveis_fetal2 <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & dplyr::contains("2")) | "obitos_fetais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2)),
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
              grepl("outros2", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Adequada atenção à mulher na gestação","Causa básica não se aplica a um óbito fetal", "Adequada atenção à mulher no parto",
                                                                   "Causas mal definidas", "Grupos não selecionados","Imunoprevenção","Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_neonatal") | "obitos_neonatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis))), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis)),
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
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_perinatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_perinatal") | "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis))), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis)),
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
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas")))
    })

    #### Para a comparação selecionada ----------------------------------------
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

    data_plot_evitaveis_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & !dplyr::contains("2"))| "obitos_fetais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis)),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
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
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          grupo_cid10 = factor(
            grupo_cid10,
            levels = c(
              "Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
              "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
              "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas"
            )
          ),
          ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
        )
    })

    data_plot_evitaveis_fetal_comp2 <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & dplyr::contains("2")) | "obitos_fetais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2)),
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
              grepl("nao_aplica2", grupo_cid10) ~ "Não se aplicam ao óbito fetal",
              grepl("outros2", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
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
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Causas mal definidas","Não se aplicam ao óbito fetal", "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_neonatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_neonatal") | "obitos_neonatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis))), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis)),
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
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
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
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_perinatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_perinatal") | "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis))), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis)),
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
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
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
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas")))
    })

    #### Para a referência selecionada ----------------------------------------
    data_plot_evitaveis_fetal_referencia <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & !dplyr::contains("2"))), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis", input = input$momento_obito_fetal_evitaveis)),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$cids_evitaveis_fetal, collapse="|"), grupo_cid10) & !is.null(input$cids_evitaveis_fetal)),
            dplyr::case_when(
              grepl("imunoprevencao", grupo_cid10) ~ "Imunoprevenção",
              grepl("mulher_gestacao", grupo_cid10) ~ "Adequada atenção à mulher na gestação",
              grepl("parto", grupo_cid10) ~ "Adequada atenção à mulher no parto",
              grepl("recem_nascido", grupo_cid10) ~ "Adequada atenção ao recém nascido",
              grepl("tratamento", grupo_cid10) ~ "Ações de diagnóstico e tratamento adequado",
              grepl("saude", grupo_cid10) ~ "Ações de promoção à saúde vinculadas a ações de atenção",
              grepl("mal_definidas", grupo_cid10) ~ "Causas mal definidas",
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
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))

    })

    data_plot_evitaveis_fetal_referencia2 <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_fetal") & dplyr::contains("2")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = input$momento_obito_fetal_evitaveis2)),
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
              grepl("outros2", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10) |>
        dplyr::summarise(
          br_porc_obitos = round(sum(br_porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Adequada atenção à mulher na gestação","Causa básica não se aplica a um óbito fetal", "Adequada atenção à mulher no parto",
                                                                   "Causas mal definidas", "Grupos não selecionados","Imunoprevenção","Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_neonatal_referencia <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_neonatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis))), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = input$momento_obito_neonatal_evitaveis)),
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
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_evitaveis_perinatal_referencia <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("evitaveis_perinatal")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = input$momento_obito_perinatal_evitaveis))))) |>
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
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto",
                                                                   "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado",
                                                                   "Ações de promoção à saúde vinculadas a ações de atenção", "Causas mal definidas","Grupos não selecionados", "Demais causas")))
    })

    #### Juntando as informações da localidade/comparação com a referência ----
    data_plot_evitaveis_fetal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_fetal()) != 0,
          "Não existem ocorrências de óbitos fetais por causas evitáveis para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_fetal(), data_plot_evitaveis_fetal_referencia())
    })

    data_plot_evitaveis_fetal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_fetal_comp()) != 0,
          "Não existem ocorrências de óbitos fetais por causas evitáveis para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_fetal_comp(), data_plot_evitaveis_fetal_referencia())
    })


    data_plot_evitaveis_fetal_completo2 <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_fetal2()) != 0,
          "Não existem ocorrências de óbitos fetais por causas evitáveis para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_fetal2(), data_plot_evitaveis_fetal_referencia2())
    })

    data_plot_evitaveis_fetal_comp_completo2 <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_fetal_comp2()) != 0,
          "Não existem ocorrências de óbitos fetais por causas evitáveis para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_fetal_comp2(), data_plot_evitaveis_fetal_referencia2())
    })


    data_plot_evitaveis_neonatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_neonatal()) != 0,
          "Não existem ocorrências de óbitos neonatais por causas evitáveis para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_neonatal(), data_plot_evitaveis_neonatal_referencia())
    })

    data_plot_evitaveis_neonatal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_neonatal_comp()) != 0,
          "Não existem ocorrências de óbitos neonatais por causas evitáveis para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_neonatal_comp(), data_plot_evitaveis_neonatal_referencia())
    })


    data_plot_evitaveis_perinatal_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_perinatal()) != 0,
          "Não existem ocorrências de óbitos perinatais por causas evitáveis para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_perinatal(), data_plot_evitaveis_perinatal_referencia())
    })

    data_plot_evitaveis_perinatal_comp_completo <- reactive({
      validate(
        need(
          nrow(data_plot_evitaveis_perinatal_comp()) != 0,
          "Não existem ocorrências de óbitos perinatais por causas evitáveis para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data_plot_evitaveis_perinatal_comp(), data_plot_evitaveis_perinatal_referencia())
    })


    ### Para os gráficos de grupos de causas ----------------------------------
    #### Para a localidade selecionada ----------------------------------------
    data_plot_grupos_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("fetal_grupos") | "obitos_fetais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos))), ~ (. / obitos_fetais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período fetal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("neonat_grupos") | "obitos_neonatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos))), ~ (. / obitos_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período neonatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período neonatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_perinatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("perinatal_grupos") | "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos))), ~ (. / obitos_perinatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ "Brasil",
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    #### Para a comparação selecionada ----------------------------------------
    data_plot_grupos_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("fetal_grupos") | "obitos_fetais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos))), ~ (. / obitos_fetais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
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
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10,
                                           levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                      "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                      "Afecções originais no período fetal", "Mal definidas",
                                                      "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_neonatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("neonat_grupos") | "obitos_neonatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos))), ~ (. / obitos_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período neonatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
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
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período neonatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_perinatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("perinatal_grupos") | "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos))), ~ (. / obitos_perinatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
              grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
              grepl("outros", grupo_cid10) ~ "Demais causas",
            ),
            "Grupos não selecionados"
          ),
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
        dplyr::ungroup() |>
        dplyr::group_by(ano, grupo_cid10, class) |>
        dplyr::summarise(
          porc_obitos = round(sum(porc_obitos), 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período perinatal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    #### Para a referência ----------------------------------------------------
    data_plot_grupos_fetal_referencia <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("fetal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos))), ~ (. / obitos_fetais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = input$momento_obito_fetal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
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
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                   "Afecções originais no período fetal", "Mal definidas",
                                                                   "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_neonatal_referencia <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("neonat_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos))), ~ (. / obitos_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="neonatal", grafico = "grupos", input = input$momento_obito_neonatal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
              grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
              grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
              grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período neonatal",
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
        dplyr::ungroup() |>
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                                  "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
                                                                  "Afecções originais no período neonatal", "Mal definidas",
                                                                  "Grupos não selecionados", "Demais causas")),
                      ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1]))
    })

    data_plot_grupos_perinatal_referencia <- reactive({
      bloco8_graficos |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("perinatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos))), ~ (. / obitos_perinatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = input$momento_obito_perinatal_grupos)),
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
              grepl("ma_formacao", grupo_cid10) ~ "Malformação congênita",
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
        dplyr::mutate(grupo_cid10 = factor(grupo_cid10, levels = c("Fatores maternos relacionados à gravidez","Asfixia/Hipóxia",  "Malformação congênita",
                                                                   "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade",
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
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco7_calcs(), bloco  = "bloco7", input = input,  filtros = filtros())
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
          if (filtros()$nivel2 == "Nacional")
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
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
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco7_calcs(), bloco  = "bloco7", input = input,  filtros = filtros(), comp = TRUE)
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
        cria_indicadores(df_calcs = bloco7_calcs(), bloco  = "bloco7", input = input,  filtros = filtros(), referencia = TRUE, adicionar_localidade = FALSE) |>
        dplyr::mutate(
          class = "Média nacional",
          localidade_comparacao = "Média nacional"
        ) |>
        dplyr::rename_with(~paste0("br_", .x), dplyr::contains("moment_obito") | dplyr::contains("dist_peso"))
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
      if (filtros()$comparar == FALSE) {
        dplyr::full_join(data7_plot_dist1(), data7_plot_dist1_referencia(), by = "ano") |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
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
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
          )
      }
    })

    data7_juncao_barras_dist2 <- reactive({
      if (filtros()$comparar == FALSE) {
        dplyr::full_join(data7_plot_dist2(), data7_plot_dist2_referencia(), by = "ano") |>
          dplyr::arrange(dplyr::desc(ano)) |>
          dplyr::mutate(
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
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
            ano = factor(ano, levels = filtros()$ano2[2]:filtros()$ano2[1])
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
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
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

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
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
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
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
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_moment_obito_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Durante o parto",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = durante_dist_moment_obito_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_durante_dist_moment_obito_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Antes do parto",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = antes_dist_moment_obito_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_antes_dist_moment_obito_fetal:,f}% </b>"
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
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Maior ou igual a 2500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = mais_2500_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_mais_2500_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 2000 a 2499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_2000_2499_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_2000_2499_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500 a 1999g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1500_1999_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1500_1999_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = menos_1500_dist_peso_fetal),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_menos_1500_dist_peso_fetal:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist2()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos fetais por causas evitáveis -------
    output$plot_evitaveis_fetal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_fetal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
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
        highcharter::hc_legend(reversed = FALSE, title = list(text = "Grupo de causas evitáveis")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_evitaveis_fetal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_evitaveis_fetal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_evitaveis_fetal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos fetais por causas evitáveis"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos fetais por causas evitáveis segundo o artigo de Vieira et Al ----
    output$plot_evitaveis_fetal2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_fetal_completo2(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
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
        highcharter::hc_legend(reversed = FALSE, title = list(text = "Grupo de causas evitáveis")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_evitaveis_fetal2()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_evitaveis_fetal2()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_evitaveis_fetal2()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos fetais por causas evitáveis"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos fetais por grupos de causas ----
    output$plot_grupos_fetal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_grupos_fetal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
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
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
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

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso),
          class
        )

      if (filtros()$comparar == "Não") {
       grafico_base <- highcharter::highchart() |>
         highcharter::hc_add_series(
           data = data7_plot_aux,
           type = "line",
           highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
         ) |>
         highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
         highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
         highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
         highcharter::hc_colors(cols)
       if (filtros()$nivel == "Nacional") {
         grafico_base
       } else {
         grafico_base |>
           highcharter::hc_add_series(
             data = data7_plot_referencia_aux,
             type = "line",
             name = "Referência (média nacional)",
             highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
            dashStyle = "ShortDot",
             opacity = 0.8
           )
       }
      } else {
       grafico_base <- highcharter::highchart() |>
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
         highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
         highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
         highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
         highcharter::hc_colors(cols)
       if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
         grafico_base
       } else {
         grafico_base |>
           highcharter::hc_add_series(
             data = data7_plot_referencia_aux,
             type = "line",
             name = "Referência (média nacional)",
             highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
             dashStyle = "ShortDot",
             opacity = 0.7
           )
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

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
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
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
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

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
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
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
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
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_moment_obito_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "7 a 27 dias de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_7_27dist_moment_obito_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_7_27dist_moment_obito_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "1 a 6 dias de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_1_6dist_moment_obito_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_1_6dist_moment_obito_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Dia 0 de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_0_dist_moment_obito_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_0_dist_moment_obito_neonat:,f}% </b>"
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
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name ="Maior ou igual a 2500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = mais_2500_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_mais_2500_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 2000g a 2499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_2000_2499_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_2000_2499_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500g a 1999g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1500_1999_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1500_1999_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = menos_1500_dist_peso_neonat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_menos_1500_dist_peso_neonat:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist2()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos neonatais por causas evitáveis ----
    output$plot_evitaveis_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_neonatal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
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
        highcharter::hc_legend(reversed = FALSE, title = list(text = "Grupo de causas evitáveis")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_evitaveis_neonatal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_evitaveis_neonatal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_evitaveis_neonatal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos neonatais por causas evitáveis"), min = 0, max = 100)

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
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
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
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
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
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
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
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
      }
    })

    #### Taxa de óbitos perinatais (definição 1) ------------------------------
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

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
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
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #### Taxa de óbitos perinatais (definição 2) ------------------------------
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

      data7_plot_referencia_aux <- data7_referencia() |>
        dplyr::select(
          ano,
          eixo_y = dplyr::all_of(input$faixa_peso_perinatal_taxa_oms),
          class
        )

      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_plot_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
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
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = ""), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_plot_referencia_aux,
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = eixo_y, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
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
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "1 a 6 dias de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_1_6_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_1_6_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Dia 0 de vida",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = dia_0_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_dia_0_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Durante o parto",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = durante_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_durante_dist_moment_obito_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Antes do parto",
          data =  data7_juncao_barras_dist1(),
          highcharter::hcaes(x = ano, y = antes_dist_moment_obito_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_antes_dist_moment_obito_perinat:,f}% </b>"
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
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_faltante_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Maior ou igual a 1500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = mais_2500_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_mais_2500_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 2000g a 2499g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_2000_2499_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_2000_2499_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "De 1500 a 1999g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = de_1500_1999_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_de_1500_1999_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Menor que 1500g",
          data =  data7_juncao_barras_dist2(),
          highcharter::hcaes(x = ano, y = menos_1500_dist_peso_perinat),
          type = "bar",
          showInLegend = TRUE,
          tooltip = list(
            pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> {point.localidade_comparacao}: <b> {point.br_menos_1500_dist_peso_perinat:,f}% </b>"
          )
        ) |>
        highcharter::hc_legend(reversed = TRUE) |>
        highcharter::hc_plotOptions(series = list(stacking = "percent")) |>
        highcharter::hc_colors(viridis::magma(7, direction = -1)[-c(1, 7)]) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_juncao_barras_dist2()$ano), allowDecimals = FALSE, reversed = TRUE, tickInterval = 1) |>
        highcharter::hc_yAxis(title = list(text = "% óbitos"), min = 0, max = 100)

    })

    #### Distribuição percentual dos óbitos perinatais por causas evitáveis ----
    output$plot_evitaveis_perinatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data_plot_evitaveis_perinatal_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
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
        highcharter::hc_legend(reversed = FALSE, title = list(text = "Grupo de causas evitáveis")) |>
        highcharter::hc_colors(
          viridis::magma(length(unique(data_plot_evitaveis_perinatal()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data_plot_evitaveis_perinatal()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data_plot_evitaveis_perinatal()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de óbitos perinatais por causas evitáveis"), min = 0, max = 100)

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
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
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
          highcharter::hc_add_series(
            data = data7(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data7()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data7()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_comp()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data5_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    ### Porcentagem de internações até o 27º dia de vida de bebês nascidos em hospitais com vínculo com o SUS

    output$plot2_morbidade_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_internacoes_publicos_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data7_internacoes_publicos_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = max(c(
            c(data7_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih),
            c(data7_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih)
          ), na.rm = TRUE) + 1) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
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
          highcharter::hc_add_series(
            data = data7_internacoes_publicos_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data7_internacoes_publicos_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_internacoes_publicos_sih_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_internacoes_publicos_sih_comp()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = max(c(
            c(data7_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih()$porc_internacoes_menores_28_dias_sih),
            c(data7_internacoes_publicos_sih_comp()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih_comp()$porc_internacoes_menores_28_dias_sih),
            c(data7_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih, data7_internacoes_publicos_sih_referencia()$porc_internacoes_menores_28_dias_sih)
          ), na.rm = TRUE) + 1) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_internacoes_publicos_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    ### Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS
    output$plot3_morbidade_neonatal <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_internacoes_uti_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data7_internacoes_uti_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
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
          highcharter::hc_add_series(
            data = data7_internacoes_uti_sih(),
            type = "line",
            name = ifelse(filtros()$nivel == "Nacional", "Brasil (valor de referência)", data7_internacoes_uti_sih()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_internacoes_uti_sih_comp(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_internacoes_uti_sih_comp()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_internacoes_uti_sih_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = localidade_comparacao, colour = localidade_comparacao),
              dashStyle = "ShortDot",
              opacity = 0.7
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
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name}: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
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


 })
}

## To be copied in the UI
# mod_bloco_7_ui("bloco_7_1")

## To be copied in the server
# mod_bloco_7_server("bloco_7_1")
