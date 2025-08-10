#' bloco_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_bloco_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyalert::useShinyalert(force = TRUE),  # Set up shinyalert
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(class = "fonte-titulos-pagina", tags$b(HTML("Condições socioeconômicas e de acesso ao serviço de saúde: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    div(style = "top: 56px; position: fixed"),
    fluidRow(
      column(
        width = 4,
        HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
        div(
          HTML("<b class = 'fonte-muito-grande'> Resumo do período &nbsp;</b>"),
          shinyWidgets::actionBttn(
            inputId = ns('botao_resumo'),
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
            uiOutput(ns("input_localidade_resumo")),
            align = "center"
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i2")), proxy.height = "293px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i1")), proxy.height = "293px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b1_i4"),
              style = "height: 303px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 31%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Porcentagem de nascidos vivos de mães nas faixas etárias selecionadas&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_idademae"),
                      actionButton(
                        inputId = ns("info_btn_idademae"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i4")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b1_i5"),
              style = "height: 303px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 31%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Porcentagem de nascidos vivos de mães das raças/cores selecionadas&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_racacormae"),
                      actionButton(
                        inputId = ns("info_btn_racacormae"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i5")), proxy.height = "276px")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b1_i6"),
              style = "height: 303px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 31%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Porcentagem de nascidos vivos de mães com as escolaridades selecionadas&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_escmae"),
                      actionButton(
                        inputId = ns("info_btn_escmae"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i6")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i7")), proxy.height = "325px")
          )
        ),
        fluidRow(
          column(
            offset = 3,
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b1_i8")), proxy.height = "325px")
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
                HTML("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos por faixa etária da mãe &nbsp;</b>"),
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
                    inputId = ns("input_idademae"),
                    label = span(class = "fonte-grande", "Faixa etária da mãe"),
                    options = list(placeholder = "Selecione, aqui, as faixas etárias de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "10 a 14 anos" = "nvm_10_a_14_anos",
                      "15 a 19 anos" = "nvm_15_a_19_anos",
                      "20 a 34 anos" = "nvm_entre_20_e_34_anos",
                      "Maior que 34 anos" = "nvm_maior_que_34_anos"
                    ),
                    selected = "nvm_10_a_14_anos",
                    multiple = TRUE,
                    width = "99%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1"), height = 380))
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
                HTML("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos por raça/cor da mãe &nbsp;</b>"),
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
                    inputId = ns("input_racacormae"),
                    label = span(class = "fonte-grande", "Raça/cor da mãe"),
                    options = list(placeholder = "Selecione, aqui, as raças/cores de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "Amarela" = "nvm_com_cor_da_pele_amarela",
                      "Branca" = "nvm_com_cor_da_pele_branca",
                      "Indígena" = "nvm_indigenas",
                      "Parda" = "nvm_com_cor_da_pele_parda",
                      "Preta" = "nvm_com_cor_da_pele_preta"
                    ),
                    selected = "nvm_com_cor_da_pele_amarela",
                    multiple = TRUE,
                    width = "99%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2"), height = 380))
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
                HTML("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos por escolaridade da mãe &nbsp;</b>"),
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
                    inputId = ns("input_escmae"),
                    label = span(class = "fonte-grande", "Escolaridade da mãe"),
                    options = list(placeholder = "Selecione, aqui, as escolaridades de interesse", `actions-box` = TRUE, `deselect-all-text` = "Desselecionar todas", `select-all-text` = "Selecionar todas", `none-selected-text` = "Nenhuma opção selecionada"),
                    choices = c(
                      "Até 3 anos de estudo" = "nvm_com_escolaridade_ate_3",
                      "De 4 a 7 anos de estudo" = "nvm_com_escolaridade_de_4_a_7",
                      "De 8 a 11 anos de estudo" = "nvm_com_escolaridade_de_8_a_11",
                      "Mais de 11 anos de estudo" = "nvm_com_escolaridade_acima_de_11"
                    ),
                    selected = "nvm_com_escolaridade_ate_3",
                    multiple = TRUE,
                    width = "99%"
                  )
                )
              ),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3"), height = 380))
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
                HTML("<b class = 'fonte-muito-grande'> Porcentagem de mulheres de 10 a 49 anos usuárias exclusivas do SUS &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao4"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao4"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4"), height = 460))
            )
          ),
          column(
            offset = 3,
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 570px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 10%; display: flex; align-items: center;",
                HTML("<b class = 'fonte-muito-grande'> Cobertura populacional da Atenção Básica &nbsp;</b>"),
                shinyjs::hidden(
                  span(
                    id = ns("mostrar_botao5"),
                    shinyWidgets::actionBttn(
                      inputId = ns("botao5"),
                      icon = icon("triangle-exclamation", style = "color: red"),
                      color = "warning",
                      style = "material-circle",
                      size = "xs"
                    )
                  )
                )
              ),
              hr(),
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5"), height = 460))
            )
          )
        )
      )
    )
  )
}

#' bloco_1 Server Functions
#'
#' @noRd
mod_bloco_1_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco1_calcs <- data.frame(
      tipo = c("local", "referencia"),
      sum_total_de_nascidos_vivos = rep("sum(total_de_nascidos_vivos)", 2),
      porc_dependentes_sus = rep("round((sum(populacao_feminina_10_a_49) - sum(pop_fem_10_49_com_plano_saude))/sum(populacao_feminina_10_a_49) * 100, 1)", 2),
      porc_cobertura_esf = c("round(sum(media_cobertura_esf)/sum(populacao_total) * 100, 1)", "dplyr::first(95)"),
      porc_nvm_idademae = rep("round(sum(dplyr::across(dplyr::all_of(input$input_idademae))) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_racacormae = rep("round(sum(dplyr::across(dplyr::all_of(input$input_racacormae))) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_escmae = rep("round(sum(dplyr::across(dplyr::all_of(input$input_escmae))) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_escolaridade_ate_3 = rep("round(sum(nvm_com_escolaridade_ate_3)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_cor_da_pele_preta = rep("round(sum(nvm_com_cor_da_pele_preta)/sum(total_de_nascidos_vivos) * 100, 1)", 2)
    )

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

    ## Criando o output que receberá os nomes dos locais selecionados quando há comparação --------
    output$input_localidade_resumo <- renderUI({
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


    ## Criando o pop-up com a informação sobre o resumo do período -------------
    observeEvent(input$botao_resumo, {
      shinyalert::shinyalert(
        html = TRUE,
        title = '<div class = "fonte-titulos-modal" style = "color: #656565"> Sobre o "Resumo do período" </div>',
        text = '
          <div style = "text-align: justify; text-justify: inter-word;">
            Todas as caixinhas que estão sob o "Resumo do período", na esquerda da página, referem-se aos valores dos indicadores calculados considerando todo o período selecionado.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando alguma comparação é feita, o usuário pode selecionar para qual localidade o resumo do período será calculado clicando em um dos botões que irão aparecer em cima das caixinhas.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Para este bloco, as caixinhas relacionadas à raça/cor e à escolaridade da mãe mudam de acordo com a raça/cor e escolaridade selecionadas nos gráficos.
          </div>',
        size = "s",
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
          idademae = round(sum(idademae_incompletos, na.rm = TRUE)/sum(idademae_totais, na.rm = TRUE) * 100, 1),
          racacormae = round(sum(racacormae_incompletos, na.rm = TRUE)/sum(racacormae_totais, na.rm = TRUE) * 100, 1),
          escmae = round(sum(escmae_incompletos, na.rm = TRUE)/sum(escmae_totais, na.rm = TRUE) * 100, 1),
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
            filtros()$nivel == "nacional" ~ "Brasil (valor de referência)",
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
    #### Porcentagem de nascidos vivos por faixa etária da mãe ----------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$idademae > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    }, ignoreNULL = FALSE)

    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$idademae,
        variavel_incompletude1 = "IDADEMAE (idade da mãe)",
        descricao_incompletude1 = "ignorados, em branco ou maiores que 55",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de nascidos vivos por raça/cor da mãe --------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$racacormae > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    }, ignoreNULL = FALSE)

    observeEvent(input$botao2, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$racacormae,
        variavel_incompletude1 = "RACACORMAE (raça/cor da mãe)",
        descricao_incompletude1 = "ignorados ou em branco",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de nascidos vivos por escolaridade da mãe ----------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$escmae > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    }, ignoreNULL = FALSE)

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$escmae,
        variavel_incompletude1 = "ESCMAE (escolaridade da mãe)",
        descricao_incompletude1 = "ignorados ou em branco",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })


    # Para o resumo do período ------------------------------------------------
    ## Calculando uma média dos indicadores para o período selecionado --------
    ### Para a localidade selecionada -----------------------------------------
    data1_resumo_aux <- reactive({
      bloco1 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$comparar == "Não") {
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
          } else {
            req(input$localidade_resumo)
            if (input$localidade_resumo == "escolha1") {
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
            } else {
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
            }
          }
        ) |>
        cria_indicadores(df_calcs = bloco1_calcs, input = input, filtros = filtros(), localidade_resumo = input$localidade_resumo)
    })

    data1_resumo_idademae <- eventReactive(c(filtros()$pesquisar, input$localidade_resumo, input$input_idademae), {
      data1_resumo_aux()
    }, ignoreNULL = FALSE)

    data1_resumo_escmae <- eventReactive(c(filtros()$pesquisar, input$localidade_resumo, input$input_escmae), {
      data1_resumo_aux()
    }, ignoreNULL = FALSE)

    data1_resumo_racacormae <- eventReactive(c(filtros()$pesquisar, input$localidade_resumo, input$input_racacormae), {
      data1_resumo_aux()
    }, ignoreNULL = FALSE)

    data1_resumo_outros <- eventReactive(c(filtros()$pesquisar, input$localidade_resumo), {
      data1_resumo_aux()
    }, ignoreNULL = FALSE)


    ### Para a referência -----------------------------------------------------
    data1_resumo_referencia_aux <- reactive({
      bloco1 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco1_calcs, input = input, filtros = filtros(), referencia = TRUE, adicionar_localidade = FALSE)
    })

    data1_resumo_referencia_idademae <- eventReactive(c(filtros()$pesquisar, input$input_idademae), {
      data1_resumo_referencia_aux()
    }, ignoreNULL = FALSE)

    data1_resumo_referencia_escmae <- eventReactive(c(filtros()$pesquisar, input$input_escmae), {
      data1_resumo_referencia_aux()
    }, ignoreNULL = FALSE)

    data1_resumo_referencia_racacormae <- eventReactive(c(filtros()$pesquisar, input$input_racacormae), {
      data1_resumo_referencia_aux()
    }, ignoreNULL = FALSE)

    data1_resumo_referencia_outros <- eventReactive(c(filtros()$pesquisar), {
      data1_resumo_referencia_aux()
    }, ignoreNULL = FALSE)


    ## Criando os outputs das caixinhas ---------------------------------------
    ### IDHM ------------------------------------------------------------------
    output$caixa_b1_i1 <- renderUI({
      if (filtros()$comparar == "Não") {
        if (filtros()$nivel == "municipal") {
          idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
          posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        } else if (filtros()$nivel == "estadual") {
          idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)]))
          posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)])
        } else if (filtros()$nivel == "nacional") {
          idhm <- 0.727
        } else {
          idhm <- NaN
        }

        if (is.na(idhm)) {
          texto_comp <- "Classificação não aplicável"
        } else {
          texto_posicao <- dplyr::case_when(
            filtros()$nivel == "nacional" ~ "",
            filtros()$nivel == "estadual" ~ "({posicao_idhm}º lugar entre os 27 estados brasileiros)",
            filtros()$nivel == "municipal" ~ "({posicao_idhm}º lugar entre os 5.570 municípios brasileiros)",
          )
          texto_comp <- dplyr::case_when(
            as.numeric(idhm) <= 0.499 ~ "Muito baixo {glue::glue(texto_posicao)}",
            as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo {glue::glue(texto_posicao)}",
            as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio {glue::glue(texto_posicao)}",
            as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto {glue::glue(texto_posicao)}",
            as.numeric(idhm) > 0.8 ~ "Muito alto {glue::glue(texto_posicao)}"
          )
        }
      } else {
        req(input$localidade_resumo)
        if (input$localidade_resumo == "escolha1") {
          if (filtros()$nivel == "municipal") {
            idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
            posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
          } else if (filtros()$nivel == "estadual") {
            idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)]))
            posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)])
          } else if (filtros()$nivel == "nacional") {
            idhm <- 0.727
          } else {
            idhm <- NaN
          }

          if (is.na(idhm)) {
            texto_comp <- "Classificação não aplicável"
          } else {
            texto_posicao <- dplyr::case_when(
              filtros()$nivel == "nacional" ~ "",
              filtros()$nivel == "estadual" ~ "({posicao_idhm}º lugar entre os 27 estados brasileiros)",
              filtros()$nivel == "municipal" ~ "({posicao_idhm}º lugar entre os 5.570 municípios brasileiros)",
            )
            texto_comp <- dplyr::case_when(
              as.numeric(idhm) <= 0.499 ~ "Muito baixo {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto {glue::glue(texto_posicao)}",
              as.numeric(idhm) > 0.8 ~ "Muito alto {glue::glue(texto_posicao)}"
            )
          }
        } else {
          if (filtros()$nivel2 == "municipal") {
            idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio2 & tabela_aux_municipios$uf == filtros()$estado_municipio2)])
            posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio2 & tabela_aux_municipios$uf == filtros()$estado_municipio2)]
          } else if (filtros()$nivel2 == "estadual") {
            idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado2)]))
            posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado2)])
          } else if (filtros()$nivel2 == "nacional") {
            idhm <- 0.727
          } else {
            idhm <- NaN
          }

          if (is.na(idhm)) {
            texto_comp <- "Classificação não aplicável"
          } else {
            texto_posicao <- dplyr::case_when(
              filtros()$nivel2 == "nacional" ~ "",
              filtros()$nivel2 == "estadual" ~ "({posicao_idhm}º lugar entre os 27 estados brasileiros)",
              filtros()$nivel2 == "municipal" ~ "({posicao_idhm}º lugar entre os 5.570 municípios brasileiros)",
            )
            texto_comp <- dplyr::case_when(
              as.numeric(idhm) <= 0.499 ~ "Muito baixo {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio {glue::glue(texto_posicao)}",
              as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto {glue::glue(texto_posicao)}",
              as.numeric(idhm) > 0.8 ~ "Muito alto {glue::glue(texto_posicao)}"
            )
          }
        }
      }

      cor_comp <- dplyr::case_when(
        is.na(idhm) ~ "lightgrey",
        as.numeric(idhm) <= 0.499 ~ "#d998a0",  #vermelho
        as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "#d8b382",  #laranja
        as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "#f1eb99",  #amarelo
        as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "#a2e4b8",  #verde
        as.numeric(idhm) > 0.8 ~ "#cbd6ff"  #azul
      )

      cria_caixa_server(
        dados = NULL,
        indicador = NULL,
        titulo = dplyr::if_else(filtros()$nivel == "nacional", true = "IDH", false = "IDHM"),
        tem_meta = FALSE,
        valor_de_referencia = 0.727,
        valor_indicador = idhm,
        tipo = "taxa",
        texto_footer = glue::glue(texto_comp),
        cor = cor_comp,
        invertido = TRUE,
        tamanho_caixa = 303,
        pagina = "bloco_1",
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

    ### Total de nascidos vivos -----------------------------------------------
    output$caixa_b1_i2 <- renderUI({
      cria_caixa_server(
        dados = data1_resumo_outros(),
        indicador = "sum_total_de_nascidos_vivos",
        titulo = "Total de nascidos vivos",
        tem_meta = FALSE,
        valor_de_referencia = data1_resumo_referencia_outros()$sum_total_de_nascidos_vivos,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (o total nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} nascidos vivos"
        ),
        tamanho_caixa = 303,
        pagina = "bloco_1",
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

    ### Porcentagem de nascidos vivos por faixa etária da mãe -----------------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_idademae",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Porcentagem de nascidos vivos faixa etária da mãe". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_idademae <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_idademae), {
      output_pronto_idademae(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_idademae", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b1_i4 <- renderUI({
      output_pronto_idademae(TRUE)
      cria_caixa_server(
        dados = data1_resumo_idademae(),
        indicador = "porc_nvm_idademae",
        titulo = "Porcentagem de nascidos vivos de mães nas faixas etárias selecionadas",
        tem_meta = FALSE,
        valor_de_referencia = data1_resumo_referencia_idademae()$porc_nvm_idademae,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        ),
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_idademae(), {
      if (output_pronto_idademae()) {
        shinyjs::show(id = "mostrar_botao_idademae", anim = TRUE, animType = "fade", time = 0.8)
      }
    })

    ### Porcentagem de nascidos vivos por raça/cor da mãe ---------------------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_racacormae",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Porcentagem de nascidos vivos por raça/cor da mãe". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_racacormae <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_racacormae), {
      output_pronto_racacormae(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_racacormae", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b1_i5 <- renderUI({
      output_pronto_racacormae(TRUE)
      cria_caixa_server(
        dados = data1_resumo_racacormae(),
        indicador = "porc_nvm_racacormae",
        titulo = "Porcentagem de nascidos vivos de mães das raças/cores selecionadas",
        tem_meta = FALSE,
        valor_de_referencia = data1_resumo_referencia_racacormae()$porc_nvm_racacormae,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = 303,
        pagina = "bloco_1",
        tipo_referencia = "média nacional",
        cor = "lightgrey",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        ),
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_racacormae(), {
      if (output_pronto_racacormae()) {
        shinyjs::show(id = "mostrar_botao_racacormae", anim = TRUE, animType = "fade", time = 0.8)
      }
    })

    ### Porcentagem de nascidos vivos por escolaridade da mãe -----------------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_escmae",
      options = list(
        title = '<span class = "fonte-media">Esta caixinha depende das escolhas feitas no gráfico "Porcentagem de nascidos vivos por escolaridade da mãe". </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_escmae <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_escmae), {
      output_pronto_escmae(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_escmae", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b1_i6 <- renderUI({
      output_pronto_escmae(TRUE)
      cria_caixa_server(
        dados = data1_resumo_escmae(),
        indicador = "porc_nvm_escmae",
        titulo = "Porcentagem de nascidos vivos de mães com as escolaridades selecionadas",
        tem_meta = FALSE,
        valor_de_referencia = data1_resumo_referencia_escmae()$porc_nvm_escmae,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        ),
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_escmae(), {
      if (output_pronto_escmae()) {
        shinyjs::show(id = "mostrar_botao_escmae", anim = TRUE, animType = "fade", time = 0.8)
      }
    })

    ### Porcentagem de mulheres de 10 a 49 anos usuárias exclusivas do SUS ----
    output$caixa_b1_i7 <- renderUI({
      cria_caixa_server(
        dados = data1_resumo_outros(),
        indicador = "porc_dependentes_sus",
        titulo = "Porcentagem de mulheres de 10 a 49 anos usuárias exclusivas do SUS",
        tem_meta = FALSE,
        valor_de_referencia = data1_resumo_referencia_outros()$porc_dependentes_sus,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = 303,
        pagina = "bloco_1",
        cor = "lightgrey",
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

    ### Cobertura populacional da Atenção Básica ------------------------------
    output$caixa_b1_i8 <- renderUI({
      cria_caixa_server(
        dados = data1_resumo_outros(),
        indicador = "porc_cobertura_esf",
        titulo = "Cobertura populacional da Atenção Básica",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = 303,
        pagina = "bloco_1",
        tipo_referencia = "meta ODS",
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
    cols <- c("#2c115f", "#b73779", "#fc8961", "#000004FF", "#f1605d")

    ## Calculando os indicadores para cada ano do período selecionado ---------
    ### Para a localidade selecionada -----------------------------------------
    data1_aux <- reactive({
      bloco1 |>
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
        cria_indicadores(df_calcs = bloco1_calcs, input = input, filtros = filtros())
    })

    data1_idademae <- eventReactive(c(filtros()$pesquisar, input$input_idademae), {
      data1_aux()
    }, ignoreNULL = FALSE)

    data1_escmae <- eventReactive(c(filtros()$pesquisar, input$input_escmae), {
      data1_aux()
    }, ignoreNULL = FALSE)

    data1_racacormae <- eventReactive(c(filtros()$pesquisar, input$input_racacormae), {
      data1_aux()
    }, ignoreNULL = FALSE)

    data1_outros <- eventReactive(c(filtros()$pesquisar), {
      data1_aux()
    }, ignoreNULL = FALSE)

    ### Para a comparação selecionada -----------------------------------------
    data1_comp_aux <- reactive({
      bloco1 |>
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
        cria_indicadores(df_calcs = bloco1_calcs, input = input, filtros = filtros(), comp = TRUE)
    })

    data1_comp_idademae <- eventReactive(c(filtros()$pesquisar, input$input_idademae), {
      data1_comp_aux()
    }, ignoreNULL = FALSE)

    data1_comp_escmae <- eventReactive(c(filtros()$pesquisar, input$input_escmae), {
      data1_comp_aux()
    }, ignoreNULL = FALSE)

    data1_comp_racacormae <- eventReactive(c(filtros()$pesquisar, input$input_racacormae), {
      data1_comp_aux()
    }, ignoreNULL = FALSE)

    data1_comp_outros <- eventReactive(c(filtros()$pesquisar), {
      data1_comp_aux()
    }, ignoreNULL = FALSE)

    ### Para a referência -----------------------------------------------------
    data1_referencia_aux <- reactive({
      bloco1 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco1_calcs, input = input, filtros = filtros(), referencia = TRUE)
    })

    data1_referencia_idademae <- eventReactive(c(filtros()$pesquisar, input$input_idademae), {
      data1_referencia_aux()
    }, ignoreNULL = FALSE)

    data1_referencia_escmae <- eventReactive(c(filtros()$pesquisar, input$input_escmae), {
      data1_referencia_aux()
    }, ignoreNULL = FALSE)

    data1_referencia_racacormae <- eventReactive(c(filtros()$pesquisar, input$input_racacormae), {
      data1_referencia_aux()
    }, ignoreNULL = FALSE)

    data1_referencia_outros <- eventReactive(c(filtros()$pesquisar), {
      data1_referencia_aux()
    }, ignoreNULL = FALSE)


    ## Criando os outputs dos gráficos ----------------------------------------
    ### Porcentagem de nascidos vivos por faixa etária da mãe -----------------
    output$plot1 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_idademae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_idademae, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data1_referencia_idademae(),
              name = "Referência (média nacional)",
              type = "line",
              highcharter::hcaes(x = ano, y = porc_nvm_idademae, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_idademae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_idademae, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_comp_idademae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_idademae, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia"))  {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data1_referencia_idademae(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_nvm_idademae, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })


    ### Porcentagem de nascidos vivos por raça/cor da mãe --------------------
    output$plot2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_racacormae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_racacormae, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data1_referencia_racacormae(),
            type = "line",
            name = "Referência (média nacional)",
            highcharter::hcaes(x = ano, y = porc_nvm_racacormae, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_racacormae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_racacormae, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_comp_racacormae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_racacormae, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data1_referencia_racacormae(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_nvm_racacormae, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })


    ### Porcentagem de nascidos vivos por escolaridade da mãe -----------------
    output$plot3 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_escmae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_escmae, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data1_referencia_escmae(),
            type = "line",
            name = "Referência (média nacional)",
            highcharter::hcaes(x = ano, y = porc_nvm_escmae, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_escmae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_escmae, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_comp_escmae(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nvm_escmae, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, ceiling = 100) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data1_referencia_escmae(),
            type = "line",
            name = "Referência (média nacional)",
            highcharter::hcaes(x = ano, y = porc_nvm_escmae, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.6
          )
        }
      }
    })


    ### Porcentagem de mulheres de 10 a 49 anos usuárias exclusivas do SUS-----
    output$plot4 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_outros(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data1_referencia_outros(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_outros(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_comp_outros(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data1_referencia_outros(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_dependentes_sus, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }

      }
    })


    ### Cobertura populacional da Atenção Básica ------------------------------
    output$plot5 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_outros(),
            name = dplyr::if_else(filtros()$nivel == "nacional", "Brasil", unique(data1_outros()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_referencia_outros(),
            type = "line",
            name = "Referência (meta ODS)",
            highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data1_outros(),
            name = dplyr::if_else(filtros()$nivel == "nacional", "Brasil", unique(data1_outros()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data1_comp_outros(),
            name = dplyr::if_else(filtros()$nivel2 == "nacional", "Brasil", unique(data1_comp_outros()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class)
          ) |>
          highcharter::hc_plotOptions(
            series = list(
              label = list(enabled = TRUE),
              allowPointSelect = TRUE
            )
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data1_referencia_outros(),
              type = "line",
              name = "Referência (meta ODS)",
              highcharter::hcaes(x = ano, y = porc_cobertura_esf, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }

      }
    })

  })
}

## To be copied in the UI
# mod_bloco_1_ui("bloco_1_1")

## To be copied in the server
# mod_bloco_1_server("bloco_1_1")
