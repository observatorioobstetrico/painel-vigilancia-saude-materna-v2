#' bloco_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Assistência pré-natal: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
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
            uiOutput(ns("input_localidade_resumo")),
            align = "center"
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i1")), proxy.height = "300px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i2")), proxy.height = "300px")
          ),
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i3")), proxy.height = "325px")
          ),
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i5")), proxy.height = "325px")
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b3_i4")), proxy.height = "325px")
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
              style = "height: 530px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:19px'> Cobertura de assistência pré-natal &nbsp;</b>"),
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
            shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1"), height = 410)))
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 530px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Porcentagem de mulheres com inicio precoce do pré-natal &nbsp;</b>"
                ),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot2"), height = 410))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 530px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Porcentagem de mulheres com número adequado de consultas de pré-natal &nbsp;</b>"
                ),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot3"), height = 410))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 530px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Porcentagem de mulheres com mais de sete consultas de pré-natal &nbsp;</b>"
                ),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot5"), height = 410))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 530px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML(
                  "<b style='font-size:19px'> Incidência de sífilis congênita por mil nascidos vivos &nbsp;</b>"
                ),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot4"), height = 410))
            )
          )
        )
      )
    )
  )
}

#' bloco_3 Server Functions
#'
#' @noRd

mod_bloco_3_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco3_calcs <- data.frame(
      tipo = c("local", "referencia"),
      porc_inicio_prec = c("round(sum(mulheres_com_inicio_precoce_do_prenatal) / sum(total_de_nascidos_vivos) * 100, 1)", "95"),
      porc_sc = c("round(sum(casos_sc) / sum(total_de_nascidos_vivos) * 1000, 1)", "0.5"),
      porc_1con = c("round(sum(mulheres_com_pelo_menos_uma_consulta_prenatal[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 1)", "dplyr::first(95[ano >= 2014])"),
      porc_7 = c("round(sum(mulheres_com_mais_de_sete_consultas_prenatal[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 1)", "dplyr::first(95[ano >= 2014])"),
      porc_consultas_adequadas = rep("round(sum(mulheres_com_consultas_prenatal_adequadas[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 2)", 2)
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

    ## Criando o output que receberá os nomes dos locais selecionados quando há comparação --------
    output$input_localidade_resumo <- renderUI({
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
          consprenat = round(sum(consprenat_incompletos, na.rm = TRUE)/sum(consprenat_totais, na.rm = TRUE) * 100, 2),
          mesprenat = round(sum(mesprenat_incompletos, na.rm = TRUE)/sum(mesprenat_totais, na.rm = TRUE) * 100, 2),
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

    ### Ativando os botões de alerta quando necessário ------------------------
    #### Cobertura de assistência pré-natal -----------------------------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)] > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)],
        variavel_incompletude1 = "CONSPRENAT",
        descricao_incompletude1 = "em branco",
        df = data_incompletude() |> dplyr::filter(ano >= 2014),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de mulheres com inicio precoce do pré-natal --------------
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$mesprenat > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao2, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$mesprenat,
        variavel_incompletude1 = "MESPRENAT",
        descricao_incompletude1 = "em branco",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    #### Porcentagem de mulheres com número adequado de consultas de pré-natal ----
    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)] > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$consprenat[which(data_incompletude()$ano >= 2014)],
        variavel_incompletude1 = "CONSPRENAT",
        descricao_incompletude1 = "em branco",
        df = data_incompletude() |> dplyr::filter(ano >= 2014),
        cobertura = data_incompletude()$cobertura
      )
    })


    # Para o resumo do período ------------------------------------------------
    ## Calculando uma média dos indicadores para o período selecionado --------
    ### Para a localidade selecionada -----------------------------------------
    data3_resumo <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::filter(
          if (filtros()$comparar == "Não") {
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
          } else {
            req(input$localidade_resumo)
            if (input$localidade_resumo == "escolha1") {
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
            } else {
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
            }
          }
        ) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros())
    })

    ### Para a referência -----------------------------------------------------
    data_resumo_referencia <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros(), referencia = TRUE)
    })


    ## Criando os outputs das caixinhas ---------------------------------------
    ### Cobertura de assistência pré-natal ------------------------------------
    output$caixa_b3_i1 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_1con",
        titulo = "Cobertura de assistência pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "recomendações OMS",
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

    ### Porcentagem de mulheres com inicio precoce do pré-natal -----------------
    output$caixa_b3_i2 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_inicio_prec",
        titulo = "Porcentagem de mulheres com início precoce do pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        fonte_titulo = "15px",
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "recomendações OMS",
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

    ### Porcentagem de mulheres com número adequado de consultas de pré-natal -----------------
    output$caixa_b3_i3 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_consultas_adequadas",
        titulo = "Porcentagem de mulheres com número adequado de consultas de pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = data_resumo_referencia()$porc_consultas_adequadas,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
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

    ### Incidência de sífilis congênita por mil nascidos vivos -----------------
    output$caixa_b3_i4 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_sc",
        titulo = "Incidência de sífilis congênita por mil nascidos vivos",
        tem_meta = TRUE,
        valor_de_referencia = 0.5,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "meta OMS",
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

    output$caixa_b3_i5 <- renderUI({
      cria_caixa_server(
        dados = data3_resumo(),
        indicador = "porc_7",
        titulo = "Porcentagem de mulheres com mais de sete consultas de pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = "300px",
        pagina = "bloco_3",
        tipo_referencia = "recomendações OMS",
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

    ## Calculando os indicadores para cada ano do período selecionado ---------
    ### Para a localidade selecionada -----------------------------------------
    data3 <- reactive({
      bloco3 |>
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
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros())
    })

    ### Para a comparação selecionada -----------------------------------------
    data3_comp <- reactive({
      bloco3 |>
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
          else if (filtros()$nivel2 == "Microrregião de saúde")
            r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
          else if (filtros()$nivel2 == "Municipal")
            municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
          else if (filtros()$nivel2 == "Municípios semelhantes")
            grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
        ) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros(), comp = TRUE)
    })

    ### Para a referência -----------------------------------------------------
    data3_referencia <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros(), referencia = TRUE)
    })


    ## Criando os outputs dos gráficos ----------------------------------------
    ### Cobertura de assistência pré-natal ------------------------------------
    output$plot1 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2014,
          "Este indicador só está disponível a partir de 2014."
        )
      )
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_1con, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia() |> dplyr::filter(ano >= 2014),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_1con, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = max(2014, filtros()$ano2[1]):filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_1con, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_1con, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = max(2014, filtros()$ano2[1]):filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
       if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
         grafico_base
       } else {
         grafico_base |>
           highcharter::hc_add_series(
             data = data3_referencia() |> dplyr::filter(ano >= 2014),
             type = "line",
             name = "Referência (recomendações OMS)",
             highcharter::hcaes(x = ano, y = porc_1con, group = class, colour = class),
             dashStyle = "ShortDot",
             opacity = 0.7
           )
       }
      }
    })

    ### Porcentagem de mulheres com inicio precoce do pré-natal -----------------
    output$plot2 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia(),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data3_referencia(),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_inicio_prec, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }

    })

    ### Porcentagem de mulheres com número adequado de consultas de pré-natal -----------------
    output$plot3 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2014,
          "Este indicador só está disponível a partir de 2014."
        )
      )
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            name = dplyr::if_else(filtros()$nivel == "Nacional", "Brasil (valor de referência)", unique(data3()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = max(2014, filtros()$ano2[1]):filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$nivel == "Nacional") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data3_referencia() |> dplyr::filter(ano >= 2014),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            name = dplyr::if_else(filtros()$nivel == "Nacional", "Brasil (valor de referência)", unique(data3()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp() |> dplyr::filter(ano >= 2014),
            name = dplyr::if_else(filtros()$nivel2 == "Nacional", "Brasil (valor de referência)", unique(data3_comp()$class)),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = max(2014, filtros()$ano2[1]):filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data3_referencia() |> dplyr::filter(ano >= 2014),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_consultas_adequadas, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    ### Incidência de sífilis congênita por mil nascidos vivos -----------------
    output$plot4 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia(),
            type = "line",
            name = "Referência (meta OMS)",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "Taxa"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data3_referencia(),
            type = "line",
            name = "Referência (meta OMS)",
            highcharter::hcaes(x = ano, y = porc_sc, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
    })



    #gráficos porcentagem mais de 7 consultas
    output$plot5 <- highcharter::renderHighchart({
      validate(
        need(
          filtros()$ano2[2] >= 2014,
          "Este indicador só está disponível a partir de 2014."
        )
      )
      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_referencia() |> dplyr::filter(ano >= 2014),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data3() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data3_comp() |> dplyr::filter(ano >= 2014),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0, max = 100) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data3_referencia() |> dplyr::filter(ano >= 2014),
            type = "line",
            name = "Referência (recomendações OMS)",
            highcharter::hcaes(x = ano, y = porc_7, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
    })



  })
}


## To be copied in the UI
# mod_bloco_3_ui("bloco_3_1")

## To be copied in the server
# mod_bloco_3_server("bloco_3_1")
