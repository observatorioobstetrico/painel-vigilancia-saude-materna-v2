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
      h2(tags$b(HTML("Garbage codes, causas principais e causas evitáveis"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    bs4Dash::bs4TabCard(
      id = ns("tabset"),
      width = 12,
      collapsible = FALSE,
      tabPanel(
        HTML("<b> Garbage codes </b>"),
        value = "garbage",
        fluidRow(
          column(
            width = 2,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
            hr(),
            fluidRow(
              column(
                width = 12,
                HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                uiOutput(ns("input_localidade_resumo_garbage")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_garbage_i1")), proxy.height = "300px")
              ),
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_garbage_i2")), proxy.height = "300px")
              ),
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_garbage_i3")), proxy.height = "300px")
              )
            )
          ),
          column(
            width = 10,
            id = ns("sem_comparacao_garbage"),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências de garbage codes para óbitos maternos &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_garbage_materno")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos maternos preenchidos com garbage codes </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_garbage_materno"),
                      label = "Garbage codes",
                      options = list(placeholder = "Selecione os garbage codes", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("garbage_materno", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("garbage_materno", names(bloco8_graficos))]),
                          function (cid) {
                            nome_cid <- df_cid10 |>
                              dplyr::filter(causabas == toupper(substr(cid, nchar("garbage_materno_") + 1, nchar(cid)))) |>
                              dplyr::pull(causabas_subcategoria)
                          }
                        ) |> unlist()
                        x[sort(names(x))]
                      },
                      selected = names(bloco8_graficos)[grepl("garbage_materno", names(bloco8_graficos))],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_materno"), height = "450px"))
              )
            ),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências dos garbage codes para óbitos fetais &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_garbage_fetal")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos fetais preenchidos com garbage codes </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_garbage_fetal"),
                      label = "Garbage codes",
                      options = list(placeholder = "Selecione os garbage codes", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("garbage_fetal", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("garbage_fetal", names(bloco8_graficos))]),
                          function (cid) {
                            nome_cid <- df_cid10 |>
                              dplyr::filter(causabas == toupper(substr(cid, nchar("garbage_fetal_") + 1, nchar(cid)))) |>
                              dplyr::pull(causabas_subcategoria)
                          }
                        ) |> unlist()
                        x[sort(names(x))]
                      },
                      selected = names(bloco8_graficos)[grepl("garbage_fetal", names(bloco8_graficos))],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_fetal"), height = "450px"))
              )
            ),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências dos garbage codes para óbitos neonatais &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_garbage_neonatal")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos neonatais preenchidos com garbage codes </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_garbage_neonatal"),
                      label = "Capítulos da CID-10",
                      options = list(placeholder = "Selecione os capítulos da CID-10", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("garbage_neonatal", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("garbage_neonatal", names(bloco8_graficos))]),
                          function (cid) {
                            nome_cid <- df_cid10 |>
                              dplyr::select(capitulo_cid10) |>
                              unique() |>
                              dplyr::filter(janitor::make_clean_names(capitulo_cid10) == substr(cid, nchar("garbage_neonatal_") + 1, nchar(cid))) |>
                              dplyr::pull(capitulo_cid10)
                          }
                        ) |> unlist()
                        x[DescTools::SortMixed(names(x), numeric.type = "roman", roman.case = "upper")]
                      },
                      selected = names(bloco8_graficos)[grepl("garbage_neonatal", names(bloco8_graficos))],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_neonatal"), height = "450px"))
              )
            )
          ),
          shinyjs::hidden(
            column(
              width = 10,
              id = ns("com_comparacao_garbage"),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos maternos preenchidos com garbage codes &nbsp;</b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_garbage_materno_comp"),
                      label = "Garbage codes",
                      options = list(placeholder = "Selecione os garbage codes", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("garbage_materno", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("garbage_materno", names(bloco8_graficos))]),
                          function (cid) {
                            nome_cid <- df_cid10 |>
                              dplyr::filter(causabas == toupper(substr(cid, nchar("garbage_materno_") + 1, nchar(cid)))) |>
                              dplyr::pull(causabas_subcategoria)
                          }
                        ) |> unlist()
                        x[sort(names(x))]
                      },
                      selected = names(bloco8_graficos)[grepl("garbage_materno", names(bloco8_graficos))],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_materno_comp"), height = 360))
              ),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos fetais preenchidos com garbage codes &nbsp;</b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_garbage_fetal_comp"),
                      label = "Garbage codes",
                      options = list(placeholder = "Selecione os garbage codes", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("garbage_fetal", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("garbage_fetal", names(bloco8_graficos))]),
                          function (cid) {
                            nome_cid <- df_cid10 |>
                              dplyr::filter(causabas == toupper(substr(cid, nchar("garbage_fetal_") + 1, nchar(cid)))) |>
                              dplyr::pull(causabas_subcategoria)
                          }
                        ) |> unlist()
                        x[sort(names(x))]
                      },
                      selected = names(bloco8_graficos)[grepl("garbage_fetal", names(bloco8_graficos))],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_fetal_comp"), height = 360))
              ),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos neonatais preenchidos com garbage codes &nbsp;</b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_garbage_neonatal_comp"),
                      label = "Capítulos da CID-10",
                      options = list(placeholder = "Selecione os capítulos da CID-10", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("garbage_neonatal", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("garbage_neonatal", names(bloco8_graficos))]),
                          function (cid) {
                            nome_cid <- df_cid10 |>
                              dplyr::select(capitulo_cid10) |>
                              unique() |>
                              dplyr::filter(janitor::make_clean_names(capitulo_cid10) == substr(cid, nchar("garbage_neonatal_") + 1, nchar(cid))) |>
                              dplyr::pull(capitulo_cid10)
                          }
                        ) |> unlist()
                        x[DescTools::SortMixed(names(x), numeric.type = "roman", roman.case = "upper")]
                      },
                      selected = names(bloco8_graficos)[grepl("garbage_neonatal", names(bloco8_graficos))],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_neonatal_comp"), height = 360))
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b> Causas principais </b>"),
        value = "principais",
        fluidRow(
          column(
            width = 2,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
            hr(),
            fluidRow(
              column(
                width = 12,
                HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                uiOutput(ns("input_localidade_resumo_principais")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_principais_i1")), proxy.height = "300px")
              ),
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_principais_i2")), proxy.height = "300px")
              )
            )
          ),
          column(
            width = 10,
            id = ns("sem_comparacao_principais"),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências das causas principais para óbitos fetais &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_principais_fetal")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos fetais ocorridos por causas principais </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_principais_fetal"),
                      label = "Grupos de causas principais",
                      options = list(placeholder = "Selecione os grupos de causas principais", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("principais_fetal", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("principais_fetal", names(bloco8_graficos))]),
                          function (cid) {
                            if (cid == "principais_fetal_a00_b99") {
                              "(A00-B99) Infecciosas"
                            } else if (cid == "principais_fetal_q00_q99") {
                              "(Q00-Q99) Anomalias congênitas"
                            } else {
                              nome_cid <- df_cid10 |>
                                dplyr::filter(substr(causabas, 1, 3) == toupper(substr(cid, nchar(cid) - 2, nchar(cid)))) |>
                                dplyr::pull(grupo_cid10) |>
                                unique()
                            }
                          }
                        ) |> unlist()
                        x[sort(names(x))]
                      },
                      selected = names(bloco8_graficos)[grepl("principais_fetal", names(bloco8_graficos))],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_principais_fetal"), height = "450px"))
              )
            ),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências das causas principais para óbitos neonatais &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_principais_neonatal")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos neonatais ocorridos por causas principais </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_principais_neonatal"),
                      label = "Grupos de causas principais",
                      options = list(placeholder = "Selecione os grupos de causas principais", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("principais_neonatal", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("principais_neonatal", names(bloco8_graficos))]),
                          function (cid) {
                            if (cid == "principais_neonatal_a00_b99") {
                              "(A00-B99) Infecciosas"
                            } else if (cid == "principais_neonatal_q00_q99") {
                              "(Q00-Q99) Anomalias congênitas"
                            } else if (cid == "principais_neonatal_outros") {
                              "Outros"
                            } else if (cid == "principais_neonatal_j00_j99") {
                              "(J00-J99) Respiratórias"
                            } else {
                              nome_cid <- df_cid10 |>
                                dplyr::filter(substr(causabas, 1, 3) == toupper(substr(cid, nchar(cid) - 2, nchar(cid)))) |>
                                dplyr::pull(grupo_cid10) |>
                                unique()
                            }
                          }
                        ) |> unlist()
                        x[sort(names(x))]
                      },
                      selected = names(bloco8_graficos)[grepl("principais_neonatal", names(bloco8_graficos))][-1],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_principais_neonatal"), height = "450px"))
              )
            )
          ),
          shinyjs::hidden(
            column(
              width = 10,
              id = ns("com_comparacao_principais"),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos fetais ocorridos por causas principais &nbsp;</b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_principais_fetal_comp"),
                      label = "Grupos de causas principais",
                      options = list(placeholder = "Selecione os grupos de causas principais", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("principais_fetal", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("principais_fetal", names(bloco8_graficos))]),
                          function (cid) {
                            if (cid == "principais_fetal_a00_b99") {
                              "(A00-B99) Infecciosas"
                            } else if (cid == "principais_fetal_q00_q99") {
                              "(Q00-Q99) Anomalias congênitas"
                            } else {
                              nome_cid <- df_cid10 |>
                                dplyr::filter(substr(causabas, 1, 3) == toupper(substr(cid, nchar(cid) - 2, nchar(cid)))) |>
                                dplyr::pull(grupo_cid10) |>
                                unique()
                            }
                          }
                        ) |> unlist()
                        x[sort(names(x))]
                      },
                      selected = names(bloco8_graficos)[grepl("principais_fetal", names(bloco8_graficos))],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_principais_fetal_comp"), height = 360))
              ),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos neonatais ocorridos por causas principais </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_principais_neonatal_comp"),
                      label = "Grupos de causas principais",
                      options = list(placeholder = "Selecione os grupos de causas principais", `actions-box` = TRUE),
                      choices = {
                        x <- sort(names(bloco8_graficos)[grepl("principais_neonatal", names(bloco8_graficos))])
                        names(x) <- lapply(
                          sort(names(bloco8_graficos)[grepl("principais_neonatal", names(bloco8_graficos))]),
                          function (cid) {
                            if (cid == "principais_neonatal_a00_b99") {
                              "(A00-B99) Infecciosas"
                            } else if (cid == "principais_neonatal_q00_q99") {
                              "(Q00-Q99) Anomalias congênitas"
                            } else if (cid == "principais_neonatal_outros") {
                              "Outros"
                            } else if (cid == "principais_neonatal_j00_j99") {
                              "(J00-J99) Respiratórias"
                            } else {
                              nome_cid <- df_cid10 |>
                                dplyr::filter(substr(causabas, 1, 3) == toupper(substr(cid, nchar(cid) - 2, nchar(cid)))) |>
                                dplyr::pull(grupo_cid10) |>
                                unique()
                            }
                          }
                        ) |> unlist()
                        x[sort(names(x))]
                      },
                      selected = names(bloco8_graficos)[grepl("principais_neonatal", names(bloco8_graficos))][-1],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_principais_neonatal_comp"), height = 360))
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b> Causas evitáveis </b>"),
        value = "evitaveis",
        fluidRow(
          column(
            width = 2,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
            hr(),
            fluidRow(
              column(
                width = 12,
                HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                uiOutput(ns("input_localidade_resumo_evitaveis")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_evitaveis_i1")), proxy.height = "300px")
              ),
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_evitaveis_i2")), proxy.height = "300px")
              )
            )
          ),
          column(
            width = 10,
            id = ns("sem_comparacao_evitaveis"),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências das causas evitáveis para óbitos fetais &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_evitaveis_fetal")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos fetais ocorridos por causas evitáveis </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_evitaveis_fetal"),
                      label = "Grupos de causas evitáveis",
                      options = list(placeholder = "Selecione os grupos de causas evitáveis", `actions-box` = TRUE),
                      choices = c(
                        "Reduzível pelas ações de imunização" = "evitaveis_fetal_imunoprevencao",
                        "Reduzíveis por adequada atenção à mulher na gestação" = "evitaveis_fetal_mulher_gestacao",
                        "Reduzíveis por adequada atenção à mulher no parto" = "evitaveis_fetal_parto",
                        "Reduzíveis por adequada atenção ao recém-nascido" = "evitaveis_fetal_recem_nascido",
                        #"Reduzíveis por ações de diagnóstico e tratamento adequado" = "evitaveis_fetal_tratamento",
                        #"Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "evitaveis_fetal_saude",
                        "Causas mal definidas" = "evitaveis_fetal_mal_definidas",
                        "Demais causas (não claramente evitáveis)" = "evitaveis_fetal_outros"
                      ),
                      selected = names(bloco8_graficos)[grepl("evitaveis_fetal", names(bloco8_graficos))][-2],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_fetal"), height = "450px"))
              )
            ),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências das causas evitáveis para óbitos neonatais &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_evitaveis_neonatal")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos neonatais ocorridos por causas evitáveis </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_evitaveis_neonatal"),
                      label = "Grupos de causas evitáveis",
                      options = list(placeholder = "Selecione os grupos de causas evitáveis", `actions-box` = TRUE),
                      choices = c(
                        "Reduzível pelas ações de imunização" = "evitaveis_neonatal_imunoprevencao",
                        "Reduzíveis por adequada atenção à mulher na gestação" = "evitaveis_neonatal_mulher_gestacao",
                        "Reduzíveis por adequada atenção à mulher no parto" = "evitaveis_neonatal_parto",
                        "Reduzíveis por adequada atenção ao recém-nascido" = "evitaveis_neonatal_recem_nascido",
                        "Reduzíveis por ações de diagnóstico e tratamento adequado" = "evitaveis_neonatal_tratamento",
                        "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "evitaveis_neonatal_saude",
                        "Causas mal definidas" = "evitaveis_neonatal_mal_definidas",
                        "Demais causas (não claramente evitáveis)" = "evitaveis_neonatal_outros"
                      ),
                      selected = names(bloco8_grafico_evitaveis_neonatal)[grepl("evitaveis_neonatal", names(bloco8_grafico_evitaveis_neonatal))][-1],
                      multiple = TRUE,
                      width = "100%"
                    )
                  ),
                  column(
                    width = 6,
                    selectizeInput(
                      inputId = ns("peso_evitaveis_neonatal"),
                      label = "Faixa de peso",
                      options = list(placeholder = "Selecione a faixa de peso"),
                      choices = c(
                        "Todas",
                        "> 1000 g",
                        "> 1500 g"
                      ),
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_neonatal"), height = "450px"))
              )
            )
          ),
          shinyjs::hidden(
            column(
              width = 10,
              id = ns("com_comparacao_evitaveis"),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos fetais ocorridos por causas evitáveis &nbsp;</b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_evitaveis_fetal_comp"),
                      label = "Grupos de causas evitáveis",
                      options = list(placeholder = "Selecione os grupos de causas evitáveis", `actions-box` = TRUE),
                      choices = c(
                        "Reduzível pelas ações de imunização" = "evitaveis_fetal_imunoprevencao",
                        "Reduzíveis por adequada atenção à mulher na gestação" = "evitaveis_fetal_mulher_gestacao",
                        "Reduzíveis por adequada atenção à mulher no parto" = "evitaveis_fetal_parto",
                        "Reduzíveis por adequada atenção ao recém-nascido" = "evitaveis_fetal_recem_nascido",
                        #"Reduzíveis por ações de diagnóstico e tratamento adequado" = "evitaveis_fetal_tratamento",
                        #"Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "evitaveis_fetal_saude",
                        "Causas mal definidas" = "evitaveis_fetal_mal_definidas",
                        "Demais causas (não claramente evitáveis)" = "evitaveis_fetal_outros"
                      ),
                      selected = names(bloco8_graficos)[grepl("evitaveis_fetal", names(bloco8_graficos))][-2],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_fetal_comp"), height = 360))
              ),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos neonatais ocorridos por causas evitáveis </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 6,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_evitaveis_neonatal_comp"),
                      label = "Grupos de causas evitáveis",
                      options = list(placeholder = "Selecione os grupos de causas evitáveis", `actions-box` = TRUE),
                      choices = c(
                        "Reduzível pelas ações de imunização" = "evitaveis_neonatal_imunoprevencao",
                        "Reduzíveis por adequada atenção à mulher na gestação" = "evitaveis_neonatal_mulher_gestacao",
                        "Reduzíveis por adequada atenção à mulher no parto" = "evitaveis_neonatal_parto",
                        "Reduzíveis por adequada atenção ao recém-nascido" = "evitaveis_neonatal_recem_nascido",
                        "Reduzíveis por ações de diagnóstico e tratamento adequado" = "evitaveis_neonatal_tratamento",
                        "Reduzíveis por ações promoção à saúde vinculadas a ações de atenção " = "evitaveis_neonatal_saude",
                        "Causas mal definidas" = "evitaveis_neonatal_mal_definidas",
                        "Demais causas (não claramente evitáveis)" = "evitaveis_neonatal_outros"
                      ),
                      selected = names(bloco8_grafico_evitaveis_neonatal)[grepl("evitaveis_neonatal", names(bloco8_grafico_evitaveis_neonatal))][-1],
                      multiple = TRUE,
                      width = "100%"
                    )
                  ),
                  column(
                    width = 6,
                    selectizeInput(
                      inputId = ns("peso_evitaveis_neonatal_comp"),
                      label = "Faixa de peso",
                      options = list(placeholder = "Selecione a faixa de peso"),
                      choices = c(
                        "Todas",
                        "> 1000 g",
                        "> 1500 g"
                      ),
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_evitaveis_neonatal_comp"), height = 360))
              )
            )
          )
        )
      ),
      tabPanel(
        HTML("<b> Grupos de causas </b>"),
        value = "grupos",
        fluidRow(
          column(
            width = 2,
            HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
            HTML("<b style='font-size:19px'> Resumo do período </b>"),
            hr(),
            fluidRow(
              column(
                width = 12,
                HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
                uiOutput(ns("input_localidade_resumo_grupos")),
                align = "center"
              )
            ),
            fluidRow(
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_grupos_i1")), proxy.height = "300px")
              ),
              column(
                width = 12,
                shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_grupos_i2")), proxy.height = "300px")
              )
            )
          ),
          column(
            width = 10,
            id = ns("sem_comparacao_grupos"),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências dos grupos de causas para óbitos fetais &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_grupos_fetal")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos fetais ocorridos pelos grupos de causas </b>")
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
                        #"Infecções" = "fetal_grupos_infeccoes",
                        "Asfixia/Hipóxia" = "fetal_grupos_asfixia",
                        "Má formação congênita" = "fetal_grupos_ma_formacao",
                        #"Afecções respiratórias dos recém nascidos" = "fetal_grupos_respiratorias",
                        "Fatores maternos relacionados à gravidez " = "fetal_grupos_gravidez",
                        #"Transtornos cardiorrespiratórios originados do período perinatal" = "fetal_grupos_cardiorrespiratorias",
                        "Afecções originais no período perinatal" = "fetal_grupos_afeccoes_perinatal",
                        #"Mal definidas" = "fetal_grupos_mal_definida",
                        "Demais causas" = "fetal_grupos_outros"
                      ),
                      selected = names(bloco8_graficos)[grepl("fetal_grupos", names(bloco8_graficos))][-1],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_fetal"), height = "450px"))
              )
            ),
            bs4Dash::bs4TabCard(
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              tabPanel(
                HTML("<b> Tabela </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Tabela de frequências dos grupos de causas para óbitos neonatais &nbsp;</b>")
                ),
                hr(),
                shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_grupos_neonatal")))
              ),
              tabPanel(
                HTML("<b> Gráfico </b>"),
                div(
                  style = "height: 10%; display: flex; align-items: center;",
                  HTML("<b style='font-size:18px'> Porcentagem de óbitos neonatais ocorridos pelos grupos de causas </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_grupos_neonatal"),
                      label = "Grupos de causas",
                      options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE),
                      choices = c(
                        "Prematuridade" = "neonat_grupos_prematuridade",
                        "Infecções" = "neonat_grupos_infeccoes",
                        "Asfixia/Hipóxia" = "neonat_grupos_asfixia",
                        "Má formação congênita" = "neonat_grupos_ma_formacao",
                        "Afecções respiratórias dos recém nascidos" = "neonat_grupos_respiratorias",
                        "Fatores maternos relacionados à gravidez " = "neonat_grupos_gravidez",
                        #"Transtornos cardiorrespiratórios originados do período perinatal" = "neonat_grupos_cardiorrespiratorias",
                        "Afecções originais no período perinatal" = "neonat_grupos_afeccoes_perinatal",
                        "Mal definidas" = "neonat_grupos_mal_definida",
                        "Demais causas" = "neonat_grupos_outros"
                      ),
                      selected = names(bloco8_graficos)[grepl("neonat_grupos", names(bloco8_graficos))][-2],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_neonatal"), height = "450px"))
              )
            )
          ),
          shinyjs::hidden(
            column(
              width = 10,
              id = ns("com_comparacao_grupos"),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos fetais ocorridos por causas evitáveis &nbsp;</b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_grupos_fetal_comp"),
                      label = "Grupos de causas",
                      options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE),
                      choices = c(
                        "Prematuridade" = "fetal_grupos_prematuridade",
                        #"Infecções" = "fetal_grupos_infeccoes",
                        "Asfixia/Hipóxia" = "fetal_grupos_asfixia",
                        "Má formação congênita" = "fetal_grupos_ma_formacao",
                        #"Afecções respiratórias dos recém nascidos" = "fetal_grupos_respiratorias",
                        "Fatores maternos relacionados à gravidez " = "fetal_grupos_gravidez",
                        #"Transtornos cardiorrespiratórios originados do período perinatal" = "fetal_grupos_cardiorrespiratorias",
                        "Afecções originais no período perinatal" = "fetal_grupos_afeccoes_perinatal",
                        #"Mal definidas" = "fetal_grupos_mal_definida",
                        "Demais causas" = "fetal_grupos_outros"
                      ),
                      selected = names(bloco8_graficos)[grepl("fetal_grupos", names(bloco8_graficos))][-1],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_fetal_comp"), height = 360))
              ),
              bs4Dash::bs4Card(
                width = 12,
                status = "primary",
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
                div(
                  style = "height: 15%; display: flex; align-items: center;",
                  HTML("<b style='font-size:19px'> Porcentagem de óbitos neonatais ocorridos por causas evitáveis </b>")
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::pickerInput(
                      inputId = ns("cids_grupos_neonatal_comp"),
                      label = "Grupos de causas",
                      options = list(placeholder = "Selecione os grupos de causas", `actions-box` = TRUE),
                      choices = c(
                        "Prematuridade" = "neonat_grupos_prematuridade",
                        "Infecções" = "neonat_grupos_infeccoes",
                        "Asfixia/Hipóxia" = "neonat_grupos_asfixia",
                        "Má formação congênita" = "neonat_grupos_ma_formacao",
                        "Afecções respiratórias dos recém nascidos" = "neonat_grupos_respiratorias",
                        "Fatores maternos relacionados à gravidez " = "neonat_grupos_gravidez",
                        #"Transtornos cardiorrespiratórios originados do período perinatal" = "neonat_grupos_cardiorrespiratorias",
                        "Afecções originais no período perinatal" = "neonat_grupos_afeccoes_perinatal",
                        "Mal definidas" = "neonat_grupos_mal_definida",
                        "Demais causas" = "neonat_grupos_outros"
                      ),
                      selected = names(bloco8_graficos)[grepl("neonat_grupos", names(bloco8_graficos))][-2],
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_grupos_neonatal_comp"), height = 360))
              )
            )
          )
        )
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
        shinyjs::hide(id = "com_comparacao_garbage", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "com_comparacao_principais", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "com_comparacao_evitaveis", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "com_comparacao_grupos", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "sem_comparacao_garbage", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "sem_comparacao_principais", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "sem_comparacao_evitaveis", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "sem_comparacao_grupos", anim = TRUE, animType = "slide", time = 0.8)
      } else {
        shinyjs::hide(id = "sem_comparacao_garbage", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "sem_comparacao_principais", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "sem_comparacao_evitaveis", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::hide(id = "sem_comparacao_grupos", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "com_comparacao_garbage", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "com_comparacao_principais", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "com_comparacao_evitaveis", anim = TRUE, animType = "slide", time = 0.8)
        shinyjs::show(id = "com_comparacao_grupos", anim = TRUE, animType = "slide", time = 0.8)
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

      tags$b(texto, style = "font-size: 33px")
    })

    ##### Dados para o resumo do perído para a localidade escolhida #####
    nivel_selecionado <- reactive({
      if (filtros()$comparar == "Não") {
        filtros()$nivel
      } else {
        req(input[[glue::glue("localidade_resumo_{input$tabset}")]])
        if (input[[glue::glue("localidade_resumo_{input$tabset}")]] == "escolha1") {
          filtros()$nivel
        } else {
          filtros()$nivel2
        }
      }
    })

    input_cids_garbage_materno <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_garbage_materno
      } else {
        input$cids_garbage_materno_comp
      }
    })

    input_cids_garbage_fetal <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_garbage_fetal
      } else {
        input$cids_garbage_fetal_comp
      }
    })

    input_cids_garbage_neonatal <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_garbage_neonatal
      } else {
        input$cids_garbage_neonatal_comp
      }
    })

    input_cids_principais_fetal <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_principais_fetal
      } else {
        input$cids_principais_fetal_comp
      }
    })

    input_cids_principais_neonatal <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_principais_neonatal
      } else {
        input$cids_principais_neonatal_comp
      }
    })

    input_cids_evitaveis_fetal <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_evitaveis_fetal
      } else {
        input$cids_evitaveis_fetal_comp
      }
    })

    input_cids_evitaveis_neonatal <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_evitaveis_neonatal
      } else {
        input$cids_evitaveis_neonatal_comp
      }
    })

    input_peso_evitaveis_neonatal <- reactive({
      if (filtros()$comparar == "Não") {
        input$peso_evitaveis_neonatal
      } else {
        input$peso_evitaveis_neonatal_comp
      }
    })

    input_cids_grupos_fetal <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_grupos_fetal
      } else {
        input$cids_grupos_fetal_comp
      }
    })

    input_cids_grupos_neonatal <- reactive({
      if (filtros()$comparar == "Não") {
        input$cids_grupos_neonatal
      } else {
        input$cids_grupos_neonatal_comp
      }
    })

    output$input_localidade_resumo_garbage <- renderUI({
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
          inputId = ns("localidade_resumo_garbage"),
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

    output$input_localidade_resumo_principais <- renderUI({
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
          inputId = ns("localidade_resumo_principais"),
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

    output$input_localidade_resumo_evitaveis <- renderUI({
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
          inputId = ns("localidade_resumo_evitaveis"),
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

    output$input_localidade_resumo_grupos <- renderUI({
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
          inputId = ns("localidade_resumo_grupos"),
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


    data8_resumo <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo_inputs <- ""
      } else {
        req(input[[glue::glue("localidade_resumo_{input$tabset}")]])
        if (input[[glue::glue("localidade_resumo_{input$tabset}")]] == "escolha1") {
          sufixo_inputs <- ""
        } else {
          sufixo_inputs <- "2"
        }
      }
      df1 <- bloco8_graficos |>
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
        dplyr::summarise(
          prop_garbage_code_materno = round(sum(dplyr::across(dplyr::all_of(input_cids_garbage_materno()))) / sum(obitos_maternos_totais) * 100, 1),
          prop_garbage_code_fetal = round(sum(dplyr::across(dplyr::all_of(input_cids_garbage_fetal()))) / sum(obitos_fetais_totais) * 100, 1),
          prop_garbage_code_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_garbage_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
          prop_principais_fetal = round(sum(dplyr::across(dplyr::all_of(input_cids_principais_fetal()))) / sum(obitos_fetais_totais) * 100, 1),
          prop_principais_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_principais_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
          prop_evitaveis_fetal = round(sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_fetal()))) / sum(obitos_fetais_totais) * 100, 1),
          #prop_evitaveis_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
          prop_grupos_fetal = round(sum(dplyr::across(dplyr::all_of(input_cids_grupos_fetal()))) / sum(obitos_fetais_totais) * 100, 1),
          prop_grupos_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_grupos_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
          localidade = dplyr::case_when(
            nivel_selecionado() == "nacional" ~ "Brasil",
            nivel_selecionado() == "regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
            nivel_selecionado() == "estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
            nivel_selecionado() == "macro" ~ filtros()[[paste0("macro", sufixo_inputs)]],
            nivel_selecionado() == "micro" ~ filtros()[[paste0("micro", sufixo_inputs)]],
            nivel_selecionado() == "municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]]
          )
        ) |>
        dplyr::ungroup()

      req(input$tabset)
      if (input$tabset == "evitaveis") {
        df2 <- bloco8_grafico_evitaveis_neonatal |>
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
              grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)],
            if (input_peso_evitaveis_neonatal() == "> 1000 g")
              faixa_de_peso == "> 1000 g" | faixa_de_peso == "> 1500 g"
            else if (input_peso_evitaveis_neonatal() == "> 1500 g")
              faixa_de_peso == "> 1500 g"
            else
              faixa_de_peso %in% unique(bloco8_grafico_evitaveis_neonatal$faixa_de_peso)
          ) |>
          dplyr::summarise(
            prop_evitaveis_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
            localidade = dplyr::case_when(
              nivel_selecionado() == "nacional" ~ "Brasil",
              nivel_selecionado() == "regional" ~ filtros()[[paste0("regiao", sufixo_inputs)]],
              nivel_selecionado() == "estadual" ~ filtros()[[paste0("estado", sufixo_inputs)]],
              nivel_selecionado() == "macro" ~ filtros()[[paste0("macro", sufixo_inputs)]],
              nivel_selecionado() == "micro" ~ filtros()[[paste0("micro", sufixo_inputs)]],
              nivel_selecionado() == "municipal" ~ filtros()[[paste0("municipio", sufixo_inputs)]]
            )
          ) |>
          dplyr::ungroup()

        dplyr::full_join(df1, df2)
      } else {
        df1
      }

    })

    data8_resumo_referencia <- reactive({
      df1 <- bloco8_graficos |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::summarise(
          prop_garbage_code_materno = round(sum(dplyr::across(dplyr::all_of(input_cids_garbage_materno()))) / sum(obitos_maternos_totais) * 100, 1),
          prop_garbage_code_fetal = round(sum(dplyr::across(dplyr::all_of(input_cids_garbage_fetal()))) / sum(obitos_fetais_totais) * 100, 1),
          prop_garbage_code_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_garbage_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
          prop_principais_fetal = round(sum(dplyr::across(dplyr::all_of(input_cids_principais_fetal()))) / sum(obitos_fetais_totais) * 100, 1),
          prop_principais_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_principais_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
          prop_evitaveis_fetal = round(sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_fetal()))) / sum(obitos_fetais_totais) * 100, 1),
          #prop_evitaveis_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
          prop_grupos_fetal = round(sum(dplyr::across(dplyr::all_of(input_cids_grupos_fetal()))) / sum(obitos_fetais_totais) * 100, 1),
          prop_grupos_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_grupos_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
          localidade = "Referência"
        ) |>
        dplyr::ungroup()

      req(input$tabset)
      if (input$tabset == "evitaveis") {
        df2 <- bloco8_grafico_evitaveis_neonatal |>
          dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
          dplyr::summarise(
            prop_evitaveis_neonatal = round(sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_neonatal()))) / sum(obitos_neonatais_totais) * 100, 1),
            localidade = "Referência"
          ) |>
          dplyr::ungroup()

        dplyr::full_join(df1, df2)
      } else {
        df1
      }

    })


    output$caixa_b8_garbage_i1 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_garbage_code_materno",
        titulo = "Porcentagem de óbitos maternos preenchidos com garbage codes",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_garbage_code_materno,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b8_garbage_i2 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_garbage_code_fetal",
        titulo = "Porcentagem de óbitos fetais preenchidos com garbage codes",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_garbage_code_fetal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b8_garbage_i3 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_garbage_code_neonatal",
        titulo = "Porcentagem de óbitos neonatais preenchidos com garbage codes",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_garbage_code_neonatal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b8_principais_i1 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_principais_fetal",
        titulo = "Porcentagem de óbitos fetais ocorridos por causas principais",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_principais_fetal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b8_principais_i2 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_principais_neonatal",
        titulo = "Porcentagem de óbitos neonatais ocorridos por causas principais",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_principais_neonatal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b8_evitaveis_i1 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_evitaveis_fetal",
        titulo = "Porcentagem de óbitos fetais ocorridos por causas evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_evitaveis_fetal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b8_evitaveis_i2 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_evitaveis_neonatal",
        titulo = "Porcentagem de óbitos neonatais ocorridos por causas evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_evitaveis_neonatal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b8_grupos_i1 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_grupos_fetal",
        titulo = "Porcentagem de óbitos fetais ocorridos pelos grupos de causas",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_grupos_fetal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })

    output$caixa_b8_grupos_i2 <- renderUI({
      cria_caixa_server(
        dados = data8_resumo(),
        indicador = "prop_grupos_neonatal",
        titulo = "Porcentagem de óbitos neonatais ocorridos pelos grupos de causas",
        tem_meta = FALSE,
        valor_de_referencia = data8_resumo_referencia()$prop_grupos_neonatal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "303px",
        pagina = "bloco_8",
        nivel_de_analise = nivel_selecionado()
      )
    })


    ##### Definindo as cores para os gráficos #####
    cols <- c("#2c115f", "#b73779", "#fc8961")



    # tabela garbage codes obitos maternos
    data8_garbage_materno <- reactive({
      bloco8_garbage_materno |>
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
        dplyr::group_by(capitulo_cid10, grupo_cid10, causabas_subcategoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_garbage_materno <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_garbage_materno()) != 0,
          "Não existem ocorrências de óbitos maternos preenchidos com garbage codes para a localidade e períodos selecionados."
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

      data8_garbage_materno() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "grupo_cid10", "causabas_subcategoria"),
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
              align = "left",
              footer = "Total"
            ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_subcategoria = reactable::colDef(
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
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
    data8_garbage_fetal <- reactive({
      bloco8_garbage_fetal |>
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
        dplyr::group_by(capitulo_cid10, grupo_cid10, causabas_subcategoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_garbage_fetal <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_garbage_fetal()) != 0,
          "Não existem ocorrências de óbitos fetais preenchidos com garbage codes para a localidade e períodos selecionados."
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

      data8_garbage_fetal() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "grupo_cid10", "causabas_subcategoria"),
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
              align = "left",
              footer = "Total"
            ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_subcategoria = reactable::colDef(
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
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
    data8_garbage_neonatal <- reactive({
      bloco8_garbage_neonatal |>
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
        dplyr::group_by(capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_idade, faixa_de_peso, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_garbage_neonatal <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_garbage_neonatal()) != 0,
          "Não existem ocorrências de óbitos neonatais preenchidos com garbage codes para a localidade e períodos selecionados."
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

      data8_garbage_neonatal() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "grupo_cid10", "causabas_subcategoria", "faixa_de_idade", "faixa_de_peso"),
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
              align = "left",
              footer = "Total"
            ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_subcategoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            faixa_de_idade = reactable::colDef(
              name = "Faixa de idade",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todas"))
            ),
            faixa_de_peso = reactable::colDef(
              name = "Faixa de peso",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todas"))
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
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
    data8_principais_fetal <- reactive({
      bloco8_principais_fetal |>
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
        dplyr::group_by(capitulo_cid10, grupo_cid10, causabas_subcategoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_principais_fetal <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_principais_fetal()) != 0,
          "Não existem ocorrências de óbitos fetais por causas principais para a localidade e períodos selecionados."
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

      data8_principais_fetal() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "grupo_cid10", "causabas_subcategoria"),
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
              align = "left",
              footer = "Total"
            ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_subcategoria = reactable::colDef(
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
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
    data8_principais_neonatal <- reactive({
      bloco8_principais_neonatal |>
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
        dplyr::group_by(capitulo_cid10, grupo_cid10, causabas_subcategoria, faixa_de_idade, faixa_de_peso, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_principais_neonatal <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_principais_neonatal()) != 0,
          "Não existem ocorrências de óbitos neonatais por causas principais para a localidade e períodos selecionados."
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

      data8_principais_neonatal() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "grupo_cid10", "causabas_subcategoria", "faixa_de_idade", "faixa_de_peso"),
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
              align = "left",
              footer = "Total"
            ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_subcategoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            faixa_de_idade = reactable::colDef(
              name = "Faixa de idade",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todas"))
            ),
            faixa_de_peso = reactable::colDef(
              name = "Faixa de peso",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todas"))
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
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
    data8_evitaveis_fetal <- reactive({
      bloco8_evitaveis_fetal |>
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
        dplyr::group_by(capitulo_cid10, grupo_cid, causabas_subcategoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_evitaveis_fetal <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_evitaveis_fetal()) != 0,
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

      data8_evitaveis_fetal() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "grupo_cid", "causabas_subcategoria"),
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
              align = "left",
              footer = "Total"
            ),
            grupo_cid = reactable::colDef(
              name = "Grupo de causas evitáveis",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_subcategoria = reactable::colDef(
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
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
    data8_evitaveis_neonatal <- reactive({
      bloco8_evitaveis_neonatal |>
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
        dplyr::group_by(capitulo_cid10, grupo_cid, causabas_subcategoria, faixa_de_idade, faixa_de_peso, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_evitaveis_neonatal <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_evitaveis_neonatal()) != 0,
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

      data8_evitaveis_neonatal() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "grupo_cid", "causabas_subcategoria", "faixa_de_idade", "faixa_de_peso"),
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
              align = "left",
              footer = "Total"
            ),
            grupo_cid = reactable::colDef(
              name = "Grupo de causas evitáveis",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_subcategoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            faixa_de_idade = reactable::colDef(
              name = "Faixa de idade",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos"))
            ),
            faixa_de_peso = reactable::colDef(
              name = "Faixa de peso",
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
          rowStyle = htmlwidgets::JS(
            "function(rowInfo) {
                if (rowInfo.aggregated === true) {
                 return { fontWeight: 700 }
                }
              }"
          )
        )

    })

    # tabela grupos de causa fetal

    data8_grupos_fetal <- reactive({
      bloco8_fetal_grupos |>
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
        dplyr::group_by(#capitulo_cid10,
          grupo_cid10, causabas_subcategoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_grupos_fetal <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_grupos_fetal()) != 0,
          "Não existem ocorrências de óbitos fetais por grupos de causa para a localidade e períodos selecionados."
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

      data8_grupos_fetal() |>
        reactable::reactable(
          groupBy = c(#"capitulo_cid10",
            "grupo_cid10", "causabas_subcategoria"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            # capitulo_cid10 = reactable::colDef(
            #   name = "Capítulo CID-10",
            #   minWidth = 60,
            #   aggregate = htmlwidgets::JS("function() { return ''}"),
            #   format = list(aggregated = reactable::colFormat(prefix = "Todos")),
            #   align = "left",
            #   footer = "Total"
            # ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left",
              footer = "Total"
            ),
            causabas_subcategoria = reactable::colDef(
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
          rowStyle = htmlwidgets::JS(
            "function(rowInfo) {
                if (rowInfo.aggregated === true) {
                 return { fontWeight: 700 }
                }
              }"
          )
        )

    })


    # tabela grupos de causas obitos neonatais
    data8_grupos_neonatal <- reactive({
      bloco8_grupos_neonatal |>
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
        dplyr::group_by(#capitulo_cid10,
          grupo_cid10, causabas_subcategoria, faixa_de_idade, faixa_de_peso, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
    })

    output$tabela_grupos_neonatal <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_principais_neonatal()) != 0,
          "Não existem ocorrências de óbitos neonatais por grupos de causas para a localidade e períodos selecionados."
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

      data8_grupos_neonatal() |>
        reactable::reactable(
          groupBy = c(#"capitulo_cid10",
            "grupo_cid10", "causabas_subcategoria", "faixa_de_idade", "faixa_de_peso"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            # capitulo_cid10 = reactable::colDef(
            #   name = "Capítulo CID-10",
            #   minWidth = 60,
            #   aggregate = htmlwidgets::JS("function() { return ''}"),
            #   format = list(aggregated = reactable::colFormat(prefix = "Todos")),
            #   align = "left",
            #   footer = "Total"
            # ),
            grupo_cid10 = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left",
              footer = "Total"
            ),
            causabas_subcategoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            faixa_de_idade = reactable::colDef(
              name = "Faixa de idade",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todas"))
            ),
            faixa_de_peso = reactable::colDef(
              name = "Faixa de peso",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todas"))
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
              aggregate = "sum",
              footer = htmlwidgets::JS(
                "function(colInfo) {
                let obitosTotais = 0
                colInfo.data.forEach(function(row) {
                  obitosTotais += row['frequencia']
                })
                return obitosTotais
              }"
              )
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          height = 450,
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

    data_filtrada_comp_aux <- reactive({
      bloco8_graficos |>
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


    # Criando o gráfico da porcentagem de garbage codes p/ óbitos maternos --------
    data_plot_garbage_materno <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input_cids_garbage_materno()))),
          obitos_maternos_totais = sum(obitos_maternos_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_maternos_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_garbage_materno_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input_cids_garbage_materno()))),
          obitos_maternos_totais = sum(obitos_maternos_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_maternos_totais * 100, 1),
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
        dplyr::ungroup()
    })

    output$plot_garbage_materno <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_materno(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos preenchidos com garbage codes"), min = 0, max = 100)

    })

    output$plot_garbage_materno_comp <- highcharter::renderHighchart({

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
    data_plot_garbage_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input_cids_garbage_fetal()))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_fetais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_garbage_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input_cids_garbage_fetal()))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_fetais_totais * 100, 1),
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
        dplyr::ungroup()
    })

    output$plot_garbage_fetal <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_fetal(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos preenchidos com garbage codes"), min = 0, max = 100)

    })

    output$plot_garbage_fetal_comp <- highcharter::renderHighchart({

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

    # Criando o gráfico da porcentagem de garbage codes p/ óbitos neonatais --------
    data_plot_garbage_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input_cids_garbage_neonatal()))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_neonatais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_garbage_neonatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input_cids_garbage_neonatal()))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_garbage_code = round(obitos_garbage_code / obitos_neonatais_totais * 100, 1),
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
        dplyr::ungroup()
    })

    output$plot_garbage_neonatal <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_neonatal(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos preenchidos com garbage codes"), min = 0, max = 100)

    })

    output$plot_garbage_neonatal_comp <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_garbage_neonatal(),
          highcharter::hcaes(x = ano, y = prop_garbage_code, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_garbage_neonatal_comp(),
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
    data_plot_principais_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_principais_fetal = sum(dplyr::across(dplyr::all_of(input_cids_principais_fetal()))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_principais_fetal = round(obitos_principais_fetal / obitos_fetais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_principais_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_principais_fetal = sum(dplyr::across(dplyr::all_of(input_cids_principais_fetal()))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_principais_fetal = round(obitos_principais_fetal / obitos_fetais_totais * 100, 1),
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
        dplyr::ungroup()
    })

    output$plot_principais_fetal <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_principais_fetal(),
          highcharter::hcaes(x = ano, y = prop_principais_fetal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos por causas principais"), min = 0, max = 100)

    })

    output$plot_principais_fetal_comp <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_principais_fetal(),
          highcharter::hcaes(x = ano, y = prop_principais_fetal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_principais_fetal_comp(),
          highcharter::hcaes(x = ano, y = prop_principais_fetal, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos por causas principais"), min = 0, max = 100)

    })


    # Criando o gráfico da porcentagem de principais causas p/ óbitos neonatais --------
    data_plot_principais_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_principais_neonatal = sum(dplyr::across(dplyr::all_of(input_cids_principais_neonatal()))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_principais_neonatal = round(obitos_principais_neonatal / obitos_neonatais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_principais_neonatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_principais_neonatal = sum(dplyr::across(dplyr::all_of(input_cids_principais_neonatal()))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_principais_neonatal = round(obitos_principais_neonatal / obitos_neonatais_totais * 100, 1),
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
        dplyr::ungroup()
    })

    output$plot_principais_neonatal <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_principais_neonatal(),
          highcharter::hcaes(x = ano, y = prop_principais_neonatal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos por causas principais"), min = 0, max = 100)
    })

    output$plot_principais_neonatal_comp <- highcharter::renderHighchart({
      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_principais_neonatal(),
          highcharter::hcaes(x = ano, y = prop_principais_neonatal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_add_series(
          data = data_plot_principais_neonatal_comp(),
          highcharter::hcaes(x = ano, y = prop_principais_neonatal, group = class),
          type = "column",
          color = "#b73779",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos por causas principais"), min = 0, max = 100)
    })


    # Criando o gráfico da porcentagem de causas evitáveis p/ óbitos fetais --------
    data_plot_evitaveis_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_evitaveis_fetal = sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_fetal()))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_evitaveis_fetal = round(obitos_evitaveis_fetal / obitos_fetais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_evitaveis_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_evitaveis_fetal = sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_fetal()))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_evitaveis_fetal = round(obitos_evitaveis_fetal / obitos_fetais_totais * 100, 1),
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
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos por causas evitáveis"), min = 0, max = 100)

    })

    output$plot_evitaveis_fetal_comp <- highcharter::renderHighchart({

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
        highcharter::hc_yAxis(title = list(text = "% de óbitos por causas evitáveis"), min = 0, max = 100)

    })

    # Criando o gráfico da porcentagem de causas evitáveis p/ óbitos neonatais --------
    data_plot_evitaveis_neonatal <- reactive({
      bloco8_grafico_evitaveis_neonatal |>
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
            municipio == filtros()$municipio & uf == filtros()$estado_municipio,
          if (input_peso_evitaveis_neonatal() == "> 1000 g")
            faixa_de_peso == "> 1000 g" | faixa_de_peso == "> 1500 g"
          else if (input_peso_evitaveis_neonatal() == "> 1500 g")
            faixa_de_peso == "> 1500 g"
          else
            faixa_de_peso %in% unique(bloco8_grafico_evitaveis_neonatal$faixa_de_peso)
        ) |>
        dplyr::group_by(ano)|>
        dplyr::summarise(
          obitos_evitaveis_neonatal = sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_neonatal()))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_evitaveis_neonatal = round(obitos_evitaveis_neonatal / obitos_neonatais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_evitaveis_neonatal_comp <- reactive({
      bloco8_grafico_evitaveis_neonatal |>
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
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)],
          if (input_peso_evitaveis_neonatal() == "> 1000 g")
            faixa_de_peso == "> 1000 g" | faixa_de_peso == "> 1500 g"
          else if (input_peso_evitaveis_neonatal() == "> 1500 g")
            faixa_de_peso == "> 1500 g"
          else
            faixa_de_peso %in% unique(bloco8_grafico_evitaveis_neonatal$faixa_de_peso)
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          obitos_evitaveis_neonatal = sum(dplyr::across(dplyr::all_of(input_cids_evitaveis_neonatal()))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_evitaveis_neonatal = round(obitos_evitaveis_neonatal / obitos_neonatais_totais * 100, 1),
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
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos por causas evitáveis"), min = 0, max = 100)

    })

    output$plot_evitaveis_neonatal_comp <- highcharter::renderHighchart({

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
        highcharter::hc_yAxis(title = list(text = "% de óbitos por causas evitáveis"), min = 0, max = 100)

    })


    # Criando o gráfico da porcentagem de grupos de causas p/ óbitos fetais --------
    data_plot_grupos_fetal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_grupos_fetal = sum(dplyr::across(dplyr::all_of(input_cids_grupos_fetal()))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_grupos_fetal = round(obitos_grupos_fetal / obitos_fetais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_grupos_fetal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_grupos_fetal = sum(dplyr::across(dplyr::all_of(input_cids_grupos_fetal()))),
          obitos_fetais_totais = sum(obitos_fetais_totais),
          prop_grupos_fetal = round(obitos_grupos_fetal / obitos_fetais_totais * 100, 1),
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
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos pelos grupos de causas"), min = 0, max = 100)

    })

    output$plot_grupos_fetal_comp <- highcharter::renderHighchart({
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
        highcharter::hc_yAxis(title = list(text = "% de óbitos pelos grupos de causas"), min = 0, max = 100)

    })

    # Criando o gráfico da porcentagem de grupos de causas p/ óbitos neonatais --------
    data_plot_grupos_neonatal <- reactive({
      data_filtrada_aux() |>
        dplyr::summarise(
          obitos_grupos_neonatal = sum(dplyr::across(dplyr::all_of(input_cids_grupos_neonatal()))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_grupos_neonatal = round(obitos_grupos_neonatal / obitos_neonatais_totais * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
    })

    data_plot_grupos_neonatal_comp <- reactive({
      data_filtrada_comp_aux() |>
        dplyr::summarise(
          obitos_grupos_neonatal = sum(dplyr::across(dplyr::all_of(input_cids_grupos_neonatal()))),
          obitos_neonatais_totais = sum(obitos_neonatais_totais),
          prop_grupos_neonatal = round(obitos_grupos_neonatal / obitos_neonatais_totais * 100, 1),
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
        dplyr::ungroup()
    })

    output$plot_grupos_neonatal <- highcharter::renderHighchart({

      highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data_plot_grupos_neonatal(),
          highcharter::hcaes(x = ano, y = prop_grupos_neonatal, group = class),
          type = "column",
          color = "#2c115f",
          showInLegend = TRUE
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de óbitos pelos grupos de causas"), min = 0, max = 100)

    })

    output$plot_grupos_neonatal_comp <- highcharter::renderHighchart({

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
        highcharter::hc_yAxis(title = list(text = "% de óbitos pelos grupos de causas"), min = 0, max = 100)

    })

  })
}


## To be copied in the UI
# mod_bloco_8_ui("bloco_8_1")

## To be copied in the server
# mod_bloco_8_server("bloco_8_1")
