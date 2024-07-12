#' bloco_9 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_9_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(tags$b(HTML("Resumo dos indicadores: gráfico de radar"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    fluidRow(
      # column(
      #   width = 4,
      #   HTML("<span style='display: block; margin-bottom: 27px;'> </span>"),
      #   HTML("<b style='font-size:18px'> Resumo do período </b>"),
      #   hr(),
      #   fluidRow(
      #     column(
      #       width = 12,
      #       HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      #       uiOutput(ns("input_localidade_resumo")),
      #       align = "center"
      #     )
      #   )
      #   fluidRow(
      #     column(
      #       width = 6,
      #       shinycssloaders::withSpinner(uiOutput(ns("caixa_b2_i2")), proxy.height = "300px")
      #     )
      #   )
      # ),
      column(
        width = 12,
        fluidRow(
          column(
            width = 12,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height: 620px; padding-top: 0; padding-bottom: 0; overflow-y: auto",  # Aumentei a altura do card
              div(
                style = "height: 15%; display: flex; align-items: center;",
                HTML("<b style='font-size:18px'> Resumo dos indicadores &nbsp;</b>")
              ),
              hr(),

              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart"), height = 400))
            )
          )
        )
      )
    )
  )
}

#' bloco_9 Server Functions
#'
#' @noRd

mod_bloco_9_server <- function(id, filtros) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    bloco9_calcs <- data.frame(
      tipo = c("local", "referencia"),
      mort_neonat = rep("round(sum(obitos_27dias)/sum(total_de_nascidos_vivos) *1000, 2)", 2),
      porc_condicoes_ameacadoras = rep("round(sum(nascidos_condicoes_ameacadoras) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
      prop_1500_sem_uti= rep("round((sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_sem_uti)) / (sum(partos_na_macro_com_uti) + sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_com_uti) + sum(partos_fora_macro_sem_uti)) * 100, 1)", 2),
      porc_consultas_inadequadas = rep("100 - round(sum(mulheres_com_consultas_prenatal_adequadas[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 2)", 2),
      porc_mais_3pt = rep("round(sum(mulheres_com_mais_de_tres_partos_anteriores) / sum(total_de_nascidos_vivos) * 100, 1)", 2)

    )



    data9 <- reactive({
      bloco2 |>
        dplyr::left_join(bloco3) |>
        dplyr::left_join(bloco4_deslocamento_macrorregiao) |>
        dplyr::left_join(bloco5) |>
        dplyr::left_join(bloco7) |>
        dplyr::select(ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
                      mulheres_com_mais_de_tres_partos_anteriores,
                      mulheres_com_consultas_prenatal_adequadas,
                      partos_na_macro_sem_uti, partos_fora_macro_sem_uti, partos_na_macro_com_uti, partos_fora_macro_com_uti,
                      nascidos_condicoes_ameacadoras,
                      obitos_27dias,
                      total_de_nascidos_vivos) |>
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
        dplyr::mutate(obitos_27dias = ifelse(ano == 2012 & is.na(obitos_27dias), 0, obitos_27dias)) |>
        cria_indicadores(df_calcs = bloco9_calcs, filtros = filtros())
    })

    ### Para a comparação selecionada -----------------------------------------
    data9_comp <- reactive({
      bloco2 |>
        dplyr::left_join(bloco3) |>
        dplyr::left_join(bloco4_deslocamento_macrorregiao) |>
        dplyr::left_join(bloco5) |>
        dplyr::left_join(bloco7) |>
        dplyr::select(ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
                      mulheres_com_mais_de_tres_partos_anteriores,
                      mulheres_com_consultas_prenatal_adequadas,
                      partos_na_macro_sem_uti, partos_fora_macro_sem_uti, partos_na_macro_com_uti, partos_fora_macro_com_uti,
                      nascidos_condicoes_ameacadoras,
                      obitos_27dias,
                      total_de_nascidos_vivos) |>
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
        dplyr::mutate(obitos_27dias = ifelse(ano == 2012 & is.na(obitos_27dias), 0, obitos_27dias)) |>
        cria_indicadores(df_calcs = bloco9_calcs, filtros = filtros(), comp = TRUE)
    })





    # teste <- bloco2 |>
    #   dplyr::left_join(bloco3) |>
    #   dplyr::left_join(bloco4_deslocamento_macrorregiao) |>
    #   dplyr::left_join(bloco5) |>
    #   dplyr::left_join(bloco7) |>
    #   dplyr::mutate(max_mort_neonat = (obitos_27dias / total_de_nascidos_vivos * 1000),
    #                 max_porc_condicoes_ameacadoras = (nascidos_condicoes_ameacadoras / total_de_nascidos_vivos * 100),
    #                 max_prop_1500_sem_uti = ((partos_na_macro_sem_uti + partos_fora_macro_sem_uti) /
    #                                               (partos_na_macro_com_uti + partos_na_macro_sem_uti + partos_fora_macro_com_uti + partos_fora_macro_sem_uti) * 100),
    #                 max_porc_mais_3pt = (mulheres_com_mais_de_tres_partos_anteriores / total_de_nascidos_vivos * 100),
    #                 porc_consultas_inadequadas = (((mulheres_com_consultas_prenatal_adequadas)) / (total_de_nascidos_vivos) * 100)
    #
    #                 ) |>
    #   dplyr::select(ano, uf, municipio, max_mort_neonat, max_porc_condicoes_ameacadoras, max_prop_1500_sem_uti, max_porc_mais_3pt, porc_consultas_inadequadas)

      # dplyr::summarise(
      #   max_mort_neonat = max(obitos_27dias / total_de_nascidos_vivos * 1000, na.rm = TRUE),
      #   max_porc_condicoes_ameacadoras = max(nascidos_condicoes_ameacadoras / total_de_nascidos_vivos * 100, na.rm = TRUE),
      #   max_prop_1500_sem_uti = max((partos_na_macro_sem_uti + partos_fora_macro_sem_uti) /
      #                                 (partos_na_macro_com_uti + partos_na_macro_sem_uti + partos_fora_macro_com_uti + partos_fora_macro_sem_uti) * 100, na.rm = TRUE),
      #   max_porc_consultas_inadequadas = 100,  # Valor máximo possível
      #   max_porc_mais_3pt = max(mulheres_com_mais_de_tres_partos_anteriores / total_de_nascidos_vivos * 100, na.rm = TRUE)
      # ) |> tidyr::replace_na(list(max_mort_neonat = 0, max_porc_condicoes_ameacadoras = 0, max_prop_1500_sem_uti = 0, max_porc_mais_3pt = 0))
      #


    max_values <- bloco2 |>
      dplyr::left_join(bloco3) |>
      dplyr::left_join(bloco4_deslocamento_macrorregiao) |>
      dplyr::left_join(bloco5) |>
      dplyr::left_join(bloco7) |>
      dplyr::summarise(
        max_mort_neonat = ifelse(sum(total_de_nascidos_vivos, na.rm = TRUE) > 0,
                                 max(obitos_27dias / total_de_nascidos_vivos * 1000, na.rm = TRUE),
                                 NA),
        max_porc_condicoes_ameacadoras = ifelse(sum(total_de_nascidos_vivos, na.rm = TRUE) > 0,
                                                max(nascidos_condicoes_ameacadoras / total_de_nascidos_vivos * 100, na.rm = TRUE),
                                                NA),
        max_prop_1500_sem_uti = ifelse((sum(partos_na_macro_com_uti, na.rm = TRUE) + sum(partos_na_macro_sem_uti, na.rm = TRUE) + sum(partos_fora_macro_com_uti, na.rm = TRUE) + sum(partos_fora_macro_sem_uti, na.rm = TRUE)) > 0,
                                       max((partos_na_macro_sem_uti + partos_fora_macro_sem_uti) / (partos_na_macro_com_uti + partos_na_macro_sem_uti + partos_fora_macro_com_uti + partos_fora_macro_sem_uti) * 100, na.rm = TRUE),
                                       NA),
        max_porc_consultas_inadequadas = 100,  # Valor máximo possível
        max_porc_mais_3pt = ifelse(sum(total_de_nascidos_vivos, na.rm = TRUE) > 0,
                                   max(mulheres_com_mais_de_tres_partos_anteriores / total_de_nascidos_vivos * 100, na.rm = TRUE),
                                   NA)
      )



    normalize_indicators <- function(df, max_values) {
      df <- df |>
        dplyr::mutate(
          mort_neonat = ifelse(!is.na(max_values$max_mort_neonat), mort_neonat / max_values$max_mort_neonat, NA),
          porc_condicoes_ameacadoras = ifelse(!is.na(max_values$max_porc_condicoes_ameacadoras), porc_condicoes_ameacadoras / max_values$max_porc_condicoes_ameacadoras, NA),
          prop_1500_sem_uti = ifelse(!is.na(max_values$max_prop_1500_sem_uti), prop_1500_sem_uti / max_values$max_prop_1500_sem_uti, NA),
          porc_consultas_inadequadas = ifelse(!is.na(max_values$max_porc_consultas_inadequadas), porc_consultas_inadequadas / max_values$max_porc_consultas_inadequadas, NA),
          porc_mais_3pt = ifelse(!is.na(max_values$max_porc_mais_3pt), porc_mais_3pt / max_values$max_porc_mais_3pt, NA)
        )
      return(df)
    }


    data9_normalized <- reactive({
      df <- data9()
      normalize_indicators(df, max_values)
    })

    data9_comp_normalized <- reactive({
      df2 <- data9_comp()
      normalize_indicators(df2, max_values)
    })


    output$spider_chart <- highcharter::renderHighchart({
      df <- data9_normalized()
      df2 <- data9_comp_normalized()

      # Limpar a coluna 'class' para remover "(valor de referência)" apenas para Brasil
      df$class <- ifelse(grepl("Brasil \\(valor de referência\\)", df$class), "Brasil", df$class)
      df2$class <- ifelse(grepl("Brasil \\(valor de referência\\)", df2$class), "Brasil", df2$class)


      print(df)
      print(df2)
      categories <- c('Taxa de mortalidade neonatal por 1000 nascidos vivos',
                      'Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida',
                      'Porcentagem de nascidos vivos com peso < 1500 g nascidos em serviço sem UTI neonatal',
                      'Porcentagem de mulheres com número inadequado de consultas de pré-natal',
                      'Porcentagem de mulheres com mais de 3 partos anteriores')


      if (filtros()$comparar == "Não") {


        values <- round(as.numeric(df[1, c(1:5)]), 3)
        highcharter::highchart() |>
          highcharter::hc_chart(polar = TRUE, type = "line", backgroundColor = "transparent") |>
          highcharter::hc_pane(size = '80%') |>
          highcharter::hc_xAxis(categories = categories, tickmarkPlacement = 'on', lineWidth = 0, labels = list(style = list(fontWeight = 'bold'))) |>
          highcharter::hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = 1) |>
          highcharter::hc_add_series(name = df$class, data = values, color = "#1f77b4", lineWidth = 2, marker = list(enabled = TRUE, symbol = "circle", radius = 4)) |>
          highcharter::hc_tooltip(shared = TRUE, pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b><br/>')


      } else {


        values1 <- round(as.numeric(df[1, c(1:5)]), 3)
        values2 <- round(as.numeric(df2[1, c(1:5)]), 3)
        highcharter::highchart() |>
          highcharter::hc_chart(polar = TRUE, type = "line", backgroundColor = "transparent") |>
          highcharter::hc_pane(size = '80%') |>
          highcharter::hc_xAxis(categories = categories, tickmarkPlacement = 'on', lineWidth = 0, labels = list(style = list(fontWeight = 'bold'))) |>
          highcharter::hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = 1) |>
          highcharter::hc_add_series(name = df$class, data = values1, color = "#1f77b4", lineWidth = 2, marker = list(enabled = TRUE, symbol = "circle", radius = 4)) |>
          highcharter::hc_add_series(name = df2$class, data = values2, color = "#ff7f0e", lineWidth = 2, marker = list(enabled = TRUE, symbol = "diamond", radius = 4)) |>
          highcharter::hc_tooltip(shared = TRUE, useHTML = TRUE, headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>', pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b><br/>')


      }
    })





# bloco 7 Taxa de mortalidade neonatal por 1000 nascidos vivos,
# mort_neonat = rep("round(sum(obitos_27dias)/sum(nascidos) *1000, 2)", 2)

# bloco 5 Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida,
# porc_condicoes_ameacadoras = rep("round(sum(nascidos_condicoes_ameacadoras) / sum(total_de_nascidos_vivos) * 100, 1)", 2)

# bloco 4 Porcentagem de nascidos vivos com peso < 1500 g nascidos em serviço sem UTI neonatal,
# bloco4_deslocamento_macrorregiao
# prop_1500_sem_uti= rep("round((sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_sem_uti)) / (sum(partos_na_macro_com_uti) + sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_com_uti) + sum(partos_fora_macro_sem_uti)) * 100, 1)", 2)

# bloco 3 Porcentagem de mulheres com número inadequado (complementar do adequado) de consultas de pré-natal  e
# porc_consultas_adequadas = rep("round(sum(mulheres_com_consultas_prenatal_adequadas[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 2)", 2)

# bloco 2 Porcentagem de mulheres com mais de 3 partos anteriores .
# porc_mais_3pt = rep("round(sum(mulheres_com_mais_de_tres_partos_anteriores) / sum(total_de_nascidos_vivos) * 100, 1)", 2)

    # mort_neonat
    # porc_condicoes_ameacadoras
    # prop_1500_sem_uti
    # porc_consultas_adequadas
    # porc_mais_3pt



    # data9_resumo <- reactive({
    #   bloco2 |>
    #     dplyr::left_join(bloco3) |>
    #     dplyr::left_join(bloco4_deslocamento_macrorregiao) |>
    #     dplyr::left_join(bloco5) |>
    #     dplyr::left_join(bloco7) |>
    #     dplyr::select(ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
    #                   mulheres_com_mais_de_tres_partos_anteriores,
    #                   mulheres_com_consultas_prenatal_adequadas,
    #                   partos_na_macro_sem_uti, partos_fora_macro_sem_uti, partos_na_macro_com_uti, partos_fora_macro_com_uti,
    #                   nascidos_condicoes_ameacadoras,
    #                   obitos_27dias,
    #                   total_de_nascidos_vivos) |>
    #     dplyr::filter(ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]) |>
    #     dplyr::filter(
    #       if (filtros()$comparar == "Não") {
    #         if (filtros()$nivel == "Nacional")
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         else if (filtros()$nivel == "Regional")
    #           regiao == filtros()$regiao
    #         else if (filtros()$nivel == "Estadual")
    #           uf == filtros()$estado
    #         else if (filtros()$nivel == "Macrorregião de saúde")
    #           macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
    #         else if(filtros()$nivel == "Microrregião de saúde")
    #           r_saude == filtros()$micro & uf == filtros()$estado_micro
    #         else if(filtros()$nivel == "Municipal")
    #           municipio == filtros()$municipio & uf == filtros()$estado_municipio
    #       } else {
    #         req(input$localidade_resumo)
    #         if (input$localidade_resumo == "escolha1") {
    #           if (filtros()$nivel == "Nacional")
    #             ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #           else if (filtros()$nivel == "Regional")
    #             regiao == filtros()$regiao
    #           else if (filtros()$nivel == "Estadual")
    #             uf == filtros()$estado
    #           else if (filtros()$nivel == "Macrorregião de saúde")
    #             macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
    #           else if(filtros()$nivel == "Microrregião de saúde")
    #             r_saude == filtros()$micro & uf == filtros()$estado_micro
    #           else if(filtros()$nivel == "Municipal")
    #             municipio == filtros()$municipio & uf == filtros()$estado_municipio
    #         } else {
    #           if (filtros()$nivel2 == "Nacional")
    #             ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #           else if (filtros()$nivel2 == "Regional")
    #             regiao == filtros()$regiao2
    #           else if (filtros()$nivel2 == "Estadual")
    #             uf == filtros()$estado2
    #           else if (filtros()$nivel2 == "Macrorregião de saúde")
    #             macro_r_saude == filtros()$macro2 & uf == filtros()$estado_macro2
    #           else if(filtros()$nivel2 == "Microrregião de saúde")
    #             r_saude == filtros()$micro2 & uf == filtros()$estado_micro2
    #           else if(filtros()$nivel2 == "Municipal")
    #             municipio == filtros()$municipio2 & uf == filtros()$estado_municipio2
    #           else if (filtros()$nivel2 == "Municípios semelhantes")
    #             grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
    #         }
    #       }
    #     ) |>
    #     cria_indicadores(df_calcs = bloco9_calcs, filtros = filtros())
    # })


  })
}





## To be copied in the UI
# mod_bloco_9_ui("bloco_9_1")

## To be copied in the server
# mod_bloco_9_server("bloco_9_1")
