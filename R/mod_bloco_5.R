#' bloco_5 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_5_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Condições de nascimento: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;"),
      style = "position: fixed; top: 56px; width: 93.75%; background-color: white; z-index: 100;"
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
        fluidRow(column(
              width = 6,
              shinycssloaders::withSpinner(uiOutput(ns("b5_i1")), proxy.height = "300px")
            ),
            column(
              width = 6,
              shinycssloaders::withSpinner(uiOutput(ns("b5_i2")), proxy.height = "300px")
            )
          ),
          fluidRow(column(
            width = 6,
            offset = 3,
            shinycssloaders::withSpinner(uiOutput(ns("b5_i3")), proxy.height = "325px")
          ))
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
                HTML(
                  "<b style='font-size:19px'> Porcentagem de baixo peso ao nascer &nbsp;</b>"
                ),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1"), height = 410))
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
                  "<b style='font-size:19px'> Porcentagem de nascimentos prematuros &nbsp;</b>"
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
            offset = 3,
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
                  "<b style='font-size:19px'> Porcentagem de nascimentos termo precoce &nbsp;</b>"
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
          )
        )
      )#,
      # fluidRow(
      #   column(
      #     width = 6,
      #     bs4Dash::bs4Card(
      #       width = 12,
      #       status = "primary",
      #       height = "150px",
      #       headerBorder = FALSE,
      #       collapsible = FALSE,
      #       HTML(
      #         "As condições de nascimento do recém-nato também dependem da saúde materna.
      #       <br>
      #       <br>
      #       Embora sejam indicadores relacionados ao bebê, devem ser monitorados por também refletirem a qualidade dos cuidados recebidos pela gestante durante a assistência pré-natal e ao parto."
      #       )
      #     )
      #   ),
      #   column(
      #     width = 6,
      #     bs4Dash::bs4Card(
      #       width = 12,
      #       status = "primary",
      #       headerBorder = FALSE,
      #       collapsible = FALSE,
      #       height = "150px",
      #       HTML(
      #         "Neste bloco apresentamos a porcentagem de nascimentos prematuros (com menos de 37 semanas gestacionais) e com baixo peso ao nascer (<2500g), principais determinantes da mortalidade infantil.
      #       Apresentamos também os nascimentos termo precoce (bebês nascidos com 37 e 38 semanas gestacionais), que apresentam maior risco de complicações e que são mais frequentes em locais com taxa elevada de cesariana."
      #       )
      #     )
      #   )
      # )
    )
  )
}

#' bloco_5 Server Functions
#'
#' @noRd
mod_bloco_5_server <- function(id, filtros){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##### Criando o output que recebe a localidade e o ano escolhidos ####
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

    #cores pros graficos
    cols <- c("#2c115f", "#b73779", "#fc8961")

    #dados com filtros e calculos porcentagens
    data5 <- reactive({
      bloco5 |>
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
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1),
          class = dplyr::case_when(
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

    data_resumo <- reactive({
      bloco5 |>
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1)
        ) |>
        dplyr::ungroup()
    })

    data_resumo_referencia_baixo_peso <- reactive({
      base_referencia_baixo_peso |>
        dplyr::filter(
          if (filtros()$comparar == "Não") {
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
          } else {
            req(input$localidade_resumo)
            if (input$localidade_resumo == "escolha1") {
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
            } else {
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
            }
          }
        ) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(nascidos, na.rm = TRUE),
          porc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1)
        )

    })


    ##### Dados de incompletude e cobertura para os indicadores do quinto bloco #####
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
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
        sub_registro_sinasc_muni_2015_2020 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "Estadual") {
        sub_registro_sinasc_uf_regioes_2015_2020 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$estado
          )
      } else if (filtros()$nivel == "Regional") {
        sub_registro_sinasc_uf_regioes_2015_2020 |>
          dplyr::filter(
            ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2],
            localidade == filtros()$regiao
          )
      } else if (filtros()$nivel == "Nacional") {
        sub_registro_sinasc_uf_regioes_2015_2020 |>
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

    data_incompletude <- reactive({dplyr::full_join(data_incompletude_aux(), data_cobertura(), by = c("ano", "localidade"))})


    observeEvent(input$botao1, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$peso,
        variavel_incompletude1 = "PESO",
        descricao_incompletude1 = "em branco ou preenchida com 9999",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$peso > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao2, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$gestacao,
        variavel_incompletude1 = "GESTACAO",
        descricao_incompletude1 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$gestacao > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        incompletude1 = data_incompletude()$semagestac,
        variavel_incompletude1 = "SEMAGESTAC",
        descricao_incompletude1 = "em branco ou ignorados",
        df = data_incompletude(),
        cobertura = data_incompletude()$cobertura
      )
    })

    observeEvent(filtros()$pesquisar, {
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
      req(any(data_incompletude()$semagestac > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
      shinyjs::show(id = "mostrar_botao3", anim = TRUE, animType = "fade", time = 0.8)
    },
    ignoreNULL = FALSE
    )

    #dados comparação 2 com filtros e calculos porcentagens (segunda seleção)
    data5_comp <- reactive({
      bloco5 |>
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos * 100, 1),
          porc_premat = round(sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos * 100, 1),
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
        dplyr::ungroup()
    })

    data_referencia <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos, na.rm = TRUE),
          porc_premat = 10,
          porc_termo_precoce = 20,
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_referencia_baixo_peso_aux <- reactive({
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
          porc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_referencia_baixo_peso <- reactive({
      data.frame(
        ano = filtros()$ano2[1]:filtros()$ano2[2],
        porc_baixo_peso = data_referencia_baixo_peso_aux()$porc_baixo_peso,
        class = "Referência"
      )
    })

    data_referencia_baixo_peso_comp_aux <- reactive({
      base_referencia_baixo_peso |>
        dplyr::filter(
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
        dplyr::summarise(
          total_de_nascidos_vivos = sum(nascidos, na.rm = TRUE),
          porc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100 * 0.7, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })

    data_referencia_baixo_peso_comp <- reactive({
      data.frame(
        ano = filtros()$ano2[1]:filtros()$ano2[2],
        porc_baixo_peso = data_referencia_baixo_peso_comp_aux()$porc_baixo_peso,
        class = "Referência"
      )
    })


    #nome da localidade para os gráficos sem comparações
    local <- reactive({
      dplyr::case_when(
        filtros()$nivel == "Nacional" ~ "Brasil",
        filtros()$nivel == "Regional" ~ filtros()$regiao,
        filtros()$nivel == "Estadual" ~ filtros()$estado,
        filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
        filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
        filtros()$nivel == "Municipal" ~ filtros()$municipio
      )
    })

    output$b5_i1 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_baixo_peso",
        titulo = "Porcentagem de baixo peso ao nascer",
        tem_meta = TRUE,
        valor_de_referencia = data_resumo_referencia_baixo_peso()$porc_baixo_peso,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
        pagina = "bloco_5",
        tipo_referencia = "meta de redução global",
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

    output$b5_i2 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_premat",
        titulo = "Porcentagem de nascimentos prematuros",
        tem_meta = TRUE,
        valor_de_referencia = 10,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
        pagina = "bloco_5",
        tipo_referencia = "países desenvolvidos",
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

    output$b5_i3 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_termo_precoce",
        titulo = "Porcentagem de nascimentos termo precoce",
        tem_meta = TRUE,
        valor_de_referencia = 20,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
        pagina = "bloco_5",
        tipo_referencia = "países desenvolvidos",
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


    #gráfico porcentagem baixo peso
    output$plot1 <- highcharter::renderHighchart({

      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_baixo_peso, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data_referencia_baixo_peso(),
            type = "line",
            name = "Referência para a localidade (meta de redução global)",
            highcharter::hcaes(x = ano, y = porc_baixo_peso, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_baixo_peso, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_baixo_peso, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data_referencia_baixo_peso(),
              type = "line",
              name = glue::glue("Referência para {unique(data5()$class)} (meta de redução global)"),
              highcharter::hcaes(x = ano, y = porc_baixo_peso, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })

    #gráfico porcentagem prematuros
    output$plot2 <- highcharter::renderHighchart({

      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_premat, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_premat, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_premat, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_premat, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_premat, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
    })

    #gráficos porcentagem termo precoce
    output$plot3 <- highcharter::renderHighchart({

      if (filtros()$comparar == "Não") {
        highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.8
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data5(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data5_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class)
          ) |>
          highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
          highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
          highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
          highcharter::hc_colors(cols)

        if (filtros()$mostrar_referencia == "nao_mostrar_referencia") {
          grafico_base
        } else {
          grafico_base |> highcharter::hc_add_series(
            data = data_referencia(),
            type = "line",
            name = "Referência (países desenvolvidos)",
            highcharter::hcaes(x = ano, y = porc_termo_precoce, group = class, colour = class),
            dashStyle = "ShortDot",
            opacity = 0.7
          )
        }
      }
    })



  })
}



## To be copied in the UI
# mod_bloco_5_ui("bloco_5_1")

## To be copied in the server
# mod_bloco_5_server("bloco_5_1")
