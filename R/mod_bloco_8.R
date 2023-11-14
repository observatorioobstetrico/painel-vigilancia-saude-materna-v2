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
      h2(tags$b(HTML("Asfixia e malformações: série histórica"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    fluidRow(
      column(
        width = 4,
        HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
        HTML("<b style='font-size:18px'> Resumo do período </b>"),
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
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b8_i1")), proxy.height = "300px")
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
              style = "height: 520px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Porcentagem de nascidos vivos com asfixia (pela primeira filtragem) &nbsp;</b>"),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot1"), height = 400))
            )
          )
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 630px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 15%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequência dos grupos de malformações prioritárias para vigilância &nbsp;</b>"),
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
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_malformacoes")))
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

    ##### Criando o output que recebe a localidade e o ano escolhidos #####
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

    ##### Dados para o resumo do perído para a localidade escolhida #####
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
      asfixia |>
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
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1)) |>
        dplyr::ungroup()
    })


    data_resumo2 <- reactive({
      asfixia |>
        dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1)) |>
        dplyr::ungroup()
    })


    ##### Dados para o resumo do perído para a comparação com o valor nacional/de referência #####
    data_resumo_brasil <- reactive({
      asfixia |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1)) |>
        dplyr::ungroup()
    })


    ##### Dados do segundo bloco de indicadores para a localidade escolhida #####
    data8 <- reactive({
      asfixia |>
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
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "Nacional" ~ dplyr::if_else(
              filtros()$comparar == "Não",
              "Brasil (valor de referência)",
              dplyr::if_else(
                filtros()$mostrar_referencia == "nao_mostrar_referencia",
                "Brasil",
                "Brasil (valor de referência)"
              )
            ),
            filtros()$nivel == "Regional" ~ filtros()$regiao,
            filtros()$nivel == "Estadual" ~ filtros()$estado,
            filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
            filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
            filtros()$nivel == "Municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })


    ##### Dados do segundo bloco de indicadores para quando há comparação #####
    data8_comp <- reactive({
      asfixia |>
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
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1),
          class = dplyr::case_when(
            filtros()$nivel2 == "Nacional" ~ dplyr::if_else(
              filtros()$comparar == "Não",
              "Brasil (valor de referência)",
              dplyr::if_else(
                filtros()$mostrar_referencia == "nao_mostrar_referencia",
                "Brasil",
                "Brasil (valor de referência)"
              )
            ),
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


    ##### Valores nacionais/de referência para os gráficos #####
    data8_referencia <- reactive({
      asfixia |>
        dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1)/total_de_nascidos_vivos*100, 1),
          class = "Referência"
        ) |>
        dplyr::ungroup()
    })


    ##### Dados de incompletude e cobertura para os indicadores do segundo bloco #####
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
          idademae = round(sum(idademae_incompletos, na.rm = TRUE) / sum(idademae_totais, na.rm = TRUE) * 100, 2),
          qtdpartces = round(sum(qtdpartces_incompletos, na.rm = TRUE) / sum(qtdpartces_totais, na.rm = TRUE) * 100, 2),
          qtdpartnor = round(sum(qtdpartnor_incompletos, na.rm = TRUE) / sum(qtdpartnor_totais, na.rm = TRUE) * 100, 2),
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


    ##### Criando as caixinhas para os indicadores do segundo bloco #####
    output$caixa_b8_i1 <- renderUI({
      cria_caixa_server(
        dados = data_resumo(),
        indicador = "porc_nascidos_vivos_asfixia1",
        titulo = "Porcentagem de nascidos vivos com asfixia (pela primeira filtragem)",
        tem_meta = FALSE,
        valor_de_referencia = data_resumo_brasil()$porc_nascidos_vivos_asfixia1,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "300px",
        fonte_titulo = "16px",
        pagina = "bloco_8",
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




    ##### Criando os modais para quando há incompletude #####
    # observeEvent(input$botao1, {
    #   cria_modal_incompletude(
    #     incompletude1 = data_incompletude()$idademae,
    #     variavel_incompletude1 = "IDADEMAE",
    #     descricao_incompletude1 = "ignorados, em branco ou maiores que 55",
    #     df = data_incompletude(),
    #     cobertura = data_incompletude()$cobertura
    #   )
    # })
    #
    # observeEvent(filtros()$pesquisar, {
    #   shinyjs::hide(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    #   req(any(data_incompletude()$idademae > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
    #   shinyjs::show(id = "mostrar_botao1", anim = TRUE, animType = "fade", time = 0.8)
    # })
    #
    # observeEvent(input$botao2, {
    #   cria_modal_incompletude(
    #     incompletude1 = data_incompletude()$qtdpartces,
    #     variavel_incompletude1 = "QTDPARTCES",
    #     descricao_incompletude1 = "em branco ou preenchidos com 99",
    #     incompletude2 = data_incompletude()$qtdpartnor,
    #     variavel_incompletude2 = "QTDPARTNOR",
    #     descricao_incompletude2 = "em branco ou preenchidos com 99",
    #     df = data_incompletude(),
    #     cobertura = data_incompletude()$cobertura
    #   )
    # })
    #
    # observeEvent(filtros()$pesquisar, {
    #   shinyjs::hide(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    #   req(any(data_incompletude()$qtdpartces > 5, na.rm = TRUE) | any(data_incompletude()$qtdpartnor > 5, na.rm = TRUE) | any(data_incompletude()$cobertura < 90, na.rm = TRUE))
    #   shinyjs::show(id = "mostrar_botao2", anim = TRUE, animType = "fade", time = 0.8)
    # },
    # ignoreNULL = FALSE
    # )


    ##### Definindo as cores para os gráficos #####
    cols <- c("#2c115f", "#b73779", "#fc8961")


    ##### Criando o gráfico de asfixia #####
    output$plot1 <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data8(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class)
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
              data = data8_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data8(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data8_comp(),
            type = "line",
            highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class)
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
              data = data8_referencia(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia1, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.7
            )
        }
      }
    })


    # ##### Criando o gráfico da porcentagem de mulheres com mais de 3 partos anteriores #####
    # output$plot2 <- highcharter::renderHighchart({
    #   if (filtros()$comparar == "Não") {
    #     grafico_base <- highcharter::highchart() |>
    #       highcharter::hc_add_series(
    #         data = data8(),
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia2, group = class, colour = class)
    #       ) |>
    #       highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
    #       highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
    #       highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
    #       highcharter::hc_colors(cols)
    #     if (filtros()$nivel == "Nacional") {
    #       grafico_base
    #     } else {
    #       grafico_base |>
    #         highcharter::hc_add_series(
    #           data = data8_referencia(),
    #           type = "line",
    #           name = "Referência (média nacional)",
    #           highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia2, group = class, colour = class),
    #           dashStyle = "ShortDot",
    #           opacity = 0.8
    #         )
    #     }
    #   } else {
    #     grafico_base <- highcharter::highchart() |>
    #       highcharter::hc_add_series(
    #         data = data8(),
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia2, group = class, colour = class)
    #       ) |>
    #       highcharter::hc_add_series(
    #         data = data8_comp(),
    #         type = "line",
    #         highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia2, group = class, colour = class)
    #       ) |>
    #       highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE) |>
    #       highcharter::hc_xAxis(title = list(text = ""), categories = filtros()$ano2[1]:filtros()$ano2[2], allowDecimals = FALSE) |>
    #       highcharter::hc_yAxis(title = list(text = "%"), min = 0) |>
    #       highcharter::hc_colors(cols)
    #     if (any(c(filtros()$nivel, filtros()$nivel2) == "Nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
    #       grafico_base
    #     } else {
    #       grafico_base |>
    #         highcharter::hc_add_series(
    #           data = data8_referencia(),
    #           type = "line",
    #           name = "Referência (média nacional)",
    #           highcharter::hcaes(x = ano, y = porc_nascidos_vivos_asfixia2, group = class, colour = class),
    #           dashStyle = "ShortDot",
    #           opacity = 0.7
    #         )
    #     }
    #   }
    # })


    ##### Criando tabela #####
    data8_nascidos_vivos <- reactive({
      asfixia |>
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
        dplyr::summarize(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos)
        ) |>
        dplyr::ungroup()
    })

    data8_malformacao <- reactive({
      malformacao |>
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
        dplyr::group_by(grupo_de_anomalias_congenitas, codigo_cid, descricao, ano) |>
        dplyr::summarize(
          frequencia = sum(nascidos_vivos_anomalia)
        ) |>
        dplyr::ungroup() |>
        dplyr::right_join(data8_nascidos_vivos()) |>
        dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
        dplyr::mutate(anomalia_descricao = paste(codigo_cid, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_malformacoes <- reactable::renderReactable({
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

      data8_malformacao() |>
          reactable::reactable(
            groupBy = c("grupo_de_anomalias_congenitas", "anomalia_descricao"),
            defaultColDef = reactable::colDef(
              footerStyle = list(fontWeight = "bold"),
              align = "center"
            ),
            columns = list(
              grupo_de_anomalias_congenitas = reactable::colDef(
                name = "Grupo de anomalias congênitas",
                minWidth = 60,
                aggregate = "unique",
                align = "left"
              ),
              anomalia_descricao = reactable::colDef(
                name = "Código CID-10",
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
                aggregate = "sum"
              ),
              total_de_nascidos_vivos = reactable::colDef(show = FALSE),
              prevalencia = reactable::colDef(
                name = "Prevalência",
                minWidth = 60,
                aggregate = proporcao_geral("frequencia", "total_de_nascidos_vivos", 10000),
                format = reactable::colFormat(
                  digits = 2
                )
              )
            ),
            sortable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            striped = TRUE,
            borderless = TRUE,
            pagination = FALSE,
            height = 500,
            rowStyle = htmlwidgets::JS(
              "function(rowInfo) {
                if (rowInfo.aggregated === true) {
                 return { fontWeight: 700 }
                }
              }"
            )
          )

    })



  })
}


## To be copied in the UI
# mod_bloco_8_ui("bloco_8_1")

## To be copied in the server
# mod_bloco_8_server("bloco_8_1")
