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
      h2(tags$b(HTML("Garbage codes, causas principais e causas evitáveis: tabelas"), htmlOutput(ns("titulo_localidade"), inline = TRUE)), style = "padding-left: 0.4em"),
      hr(style = "margin-bottom: 0px;")
    ),
    fluidRow(
      id = ns("sem_comparacao"),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências dos garbage codes para óbito materno &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_materno_garbage")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências dos garbage codes para óbito fetal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_fetal_garbage")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências dos garbage codes para óbito neonatal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_neonat_garbage")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências das causas principais para óbito fetal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_fetal_causas")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências das causas principais para óbito neonatal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_neonat_causas")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências das causas evitáveis para óbito fetal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_fetal_evitaveis")))
        )
      ),
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          collapsible = FALSE,
          headerBorder = FALSE,
          style = "height: 600px; padding-top: 0; padding-bottom: 0; overflow-y: auto",
          div(
            style = "height: 10%; display: flex; align-items: center;",
            HTML("<b style='font-size:18px'> Tabela de frequências das causas evitáveis para óbito neonatal &nbsp;</b>")
          ),
          hr(),
          shinycssloaders::withSpinner(reactable::reactableOutput(ns("tabela_neonat_evitaveis")))
        )
      )
    ),
    shinyjs::hidden(
     fluidRow(
       id = ns("com_comparacao"),
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
             HTML("<b style='font-size:19px'> Porcentagem de óbitos maternos preenchidos com garbage codes &nbsp;</b>"),
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
                 inputId = ns("cids_garbage_materno"),
                 label = "Garbage codes",
                 options = list(placeholder = "Selecione os garbage codes", `actions-box` = TRUE),
                 choices = c(
                   "D39 Neoplasia de comportamento incerto ou desconhecido dos órgãos genitais femininos" = "garbage_materno_d39",
                   "F53 Transtornos mentais e comportamentais associados ao puerpério, não classificados em outra parte" = "garbage_materno_f53",
                   "O94 Sequelas de complicações da gravidez, parto e puerpério" = "garbage_materno_o94",
                   "095 Morte obstétrica de causa não especificada" = "garbage_materno_o95"
                 ),
                 selected = names(bloco8_graficos)[grepl("garbage_materno", names(bloco8_graficos))],
                 multiple = TRUE,
                 width = "100%"
              )
             )
           ),
           shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_materno"), height = 360))
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
             HTML("<b style='font-size:19px'> Porcentagem de óbitos fetais preenchidos com garbage codes &nbsp;</b>"),
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
                 inputId = ns("cids_garbage_fetal"),
                 label = "Grupos de garbage codes",
                 options = list(placeholder = "Selecione os grupos de garbage codes", `actions-box` = TRUE),
                 choices = c(
                   "(P90-P96) Outros transtornos originados no período perinatal" = "garbage_fetal_p90_p96",
                   "(Q10-Q18) Malformações congênitas do olho, do ouvido, da face e do pescoço" = "garbage_fetal_q10_q18",
                   "(Q35-Q37) Fenda labial e fenda palatina" = "garbage_fetal_q35_q37",
                   "(Q80-Q89) Outras malformações congênitas" = "garbage_fetal_q80_q89"
                 ),
                 selected = names(bloco8_graficos)[grepl("garbage_fetal", names(bloco8_graficos))],
                 multiple = TRUE,
                 width = "100%"
               )
             )
           ),
           shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_garbage_fetal"), height = 360))
         )
       ),
       # Adicionar outras columns com width 6 a partir daqui (ele já vai entender que vão ficar dois gráficos por linha)
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
        shinyjs::hide(id = "com_comparacao", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "sem_comparacao", anim = TRUE, animType = "slide", time = 0.8)
      } else {
        shinyjs::hide(id = "sem_comparacao", anim = TRUE, animType = "slide", time = 0.001)
        shinyjs::show(id = "com_comparacao", anim = TRUE, animType = "slide", time = 0.8)
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



    ##### Definindo as cores para os gráficos #####
    cols <- c("#2c115f", "#b73779", "#fc8961")



    # tabela garbage codes obitos maternos
    data8_materno_garbage <- reactive({
      bloco8_materno_garbage |>
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
        dplyr::group_by(capitulo_cid10, causabas_categoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_materno_garbage <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_materno_garbage()) != 0,
          "Não existem ocorrências de garbage codes para a localidade e períodos selecionados."
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

      data8_materno_garbage() |>
        reactable::reactable(
          groupBy = c("capitulo_cid10", "causabas_categoria"),
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
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
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
              aggregate = "sum"
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


    # tabela garbage codes obitos fetais
    bloco8_fetal_garbage$causabas_grupo <- factor(bloco8_fetal_garbage$causabas_grupo,
                                                  levels = c("Outros transtornos originados no período perinatal",
                                                             "Malformações congênitas do olho, do ouvido, da face e do pescoço",
                                                             "Fenda labial e fenda palatina",
                                                             "Outras malformações congênitas",
                                                             "Anomalias cromossômicas não classificadas em outra parte",
                                                             "Transtornos respiratórios e cardiovasculares específicos do período perinatal"))

    data8_fetal_garbage <- reactive({
      bloco8_fetal_garbage |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_fetal_garbage <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_fetal_garbage()) != 0,
          "Não existem ocorrências de garbage codes para a localidade e períodos selecionados."
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

      data8_fetal_garbage() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
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
              aggregate = "sum"
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

    # tabela garbage codes obitos neonatais
    data8_neonat_garbage <- reactive({
      bloco8_neonat_garbage |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano, idade_dias) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_neonat_garbage <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_neonat_garbage()) != 0,
          "Não existem ocorrências de garbage codes para a localidade e períodos selecionados."
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

      data8_neonat_garbage() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria", "idade_dias"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            idade_dias = reactable::colDef(
              name = "Grupo de idade",
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
              aggregate = "sum"
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


    # tabela principais causas obitos fetais
    data8_fetal_causas <- reactive({
      bloco8_fetal_causas |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_fetal_causas <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_fetal_causas()) != 0,
          "Não existem ocorrências de óbitos fetais pelas causas mais comuns para a localidade e períodos selecionados."
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

      data8_fetal_causas() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
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
              aggregate = "sum"
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

    # tabela principais causas obitos neonatais
    data8_neonat_causas <- reactive({
      bloco8_neonat_causas |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano, idade_dias) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_neonat_causas <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_neonat_causas()) != 0,
          "Não existem ocorrências de óbitos neonatais pelas causas mais comuns para a localidade e períodos selecionados."
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

      data8_neonat_causas() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria", "idade_dias"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            idade_dias = reactable::colDef(
              name = "Grupo de idade",
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
              aggregate = "sum"
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

    # tabela evitaveis obitos fetais
    data8_fetal_evitaveis <- reactive({
      bloco8_fetal_evitaveis |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, grupo, ano) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_fetal_evitaveis <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_fetal_evitaveis()) != 0,
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

      data8_fetal_evitaveis() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria", "grupo"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            grupo = reactable::colDef(
              name = "Grupo de causas",
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

    # tabela evitaveis obitos neonatais
    data8_neonat_evitaveis <- reactive({
      bloco8_neonat_evitaveis |>
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
        dplyr::group_by(causabas_grupo, capitulo_cid10, causabas_categoria, ano, idade_dias, grupo) |>
        dplyr::summarize(
          frequencia = sum(obitos)
        ) |>
        dplyr::ungroup()
      #dplyr::right_join(data5_nascidos_vivos()) |>
      #dplyr::mutate(prevalencia = round(frequencia/total_de_nascidos_vivos * 10000, 2)) |>
      #dplyr::mutate(anomalia_descricao = paste(anomalia, descricao, sep = " - "), .keep = "unused", .after = grupo_de_anomalias_congenitas)
    })

    output$tabela_neonat_evitaveis <- reactable::renderReactable({
      validate(
        need(
          nrow(data8_neonat_evitaveis()) != 0,
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

      data8_neonat_evitaveis() |>
        reactable::reactable(
          groupBy = c("causabas_grupo", "capitulo_cid10", "causabas_categoria", "grupo", "idade_dias"),
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            causabas_grupo = reactable::colDef(
              name = "Grupo CID-10",
              minWidth = 60,
              aggregate = "unique",
              align = "left"
            ),
            capitulo_cid10 = reactable::colDef(
              name = "Capítulo CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            causabas_categoria = reactable::colDef(
              name = "Categoria CID-10",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos")),
              align = "left"
            ),
            grupo = reactable::colDef(
              name = "Grupo de causas",
              minWidth = 60,
              aggregate = htmlwidgets::JS("function() { return ''}"),
              format = list(aggregated = reactable::colFormat(prefix = "Todos"))
            ),
            idade_dias = reactable::colDef(
              name = "Grupo de idade",
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
              aggregate = "sum"
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


    # Criando o gráfico da porcentagem de garbage codes p/ óbitos maternos --------
    output$plot_garbage_materno <- highcharter::renderHighchart({

      data_plot_garbage_materno <- reactive({
        data_filtrada_aux() |>
          dplyr::summarise(
            obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input$cids_garbage_materno))),
            obitos_maternos_totais = sum(obitos_maternos_totais),
            prop_garbage_code = round(obitos_garbage_code / obitos_maternos_totais * 100, 1),
            class = dplyr::case_when(
              filtros()$nivel == "Nacional" ~ "Brasil",
              filtros()$nivel == "Regional" ~ filtros()$regiao,
              filtros()$nivel == "Estadual" ~ filtros()$estado,
              filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
              filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
              filtros()$nivel == "Municipal" ~ filtros()$municipio
            )
          )
      })

      data_plot_garbage_materno_comp <- reactive({
        data_filtrada_comp_aux() |>
          dplyr::summarise(
            obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input$cids_garbage_materno))),
            obitos_maternos_totais = sum(obitos_maternos_totais),
            prop_garbage_code = round(obitos_garbage_code / obitos_maternos_totais * 100, 1),
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
    output$plot_garbage_fetal <- highcharter::renderHighchart({

      data_plot_garbage_fetal <- reactive({
        data_filtrada_aux() |>
          dplyr::summarise(
            obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input$cids_garbage_fetal))),
            obitos_fetais_totais = sum(obitos_fetais_totais),
            prop_garbage_code = round(obitos_garbage_code / obitos_fetais_totais * 100, 1),
            class = dplyr::case_when(
              filtros()$nivel == "Nacional" ~ "Brasil",
              filtros()$nivel == "Regional" ~ filtros()$regiao,
              filtros()$nivel == "Estadual" ~ filtros()$estado,
              filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
              filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
              filtros()$nivel == "Municipal" ~ filtros()$municipio
            )
          )
      })

      observe(print(data_plot_garbage_fetal()))

      data_plot_garbage_fetal_comp <- reactive({
        data_filtrada_comp_aux() |>
          dplyr::summarise(
            obitos_garbage_code = sum(dplyr::across(dplyr::all_of(input$cids_garbage_fetal))),
            obitos_fetais_totais = sum(obitos_fetais_totais),
            prop_garbage_code = round(obitos_garbage_code / obitos_fetais_totais * 100, 1),
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


  })
}


## To be copied in the UI
# mod_bloco_8_ui("bloco_8_1")

## To be copied in the server
# mod_bloco_8_server("bloco_8_1")
