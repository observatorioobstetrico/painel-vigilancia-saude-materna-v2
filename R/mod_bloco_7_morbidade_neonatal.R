#' bloco_7_morbidade_neonatal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_7_morbidade_neonatal_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
            uiOutput(ns("input_localidade_resumo")),
            align = "center"
          )
        ),
        fluidRow(
          column(
            width = 6,
            shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_morbidade_neonatal_condicoes_ameacadoras")), proxy.height = "300px")
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_morbidade_neonatal_internacoes"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 31%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Porcentagem de internações neonatais (até o 27º dia de vida) em relação ao total de partos no SUS&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_internacoes"),
                      actionButton(
                        inputId = ns("info_btn_internacoes"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_morbidade_neonatal_internacoes")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_morbidade_neonatal_internacoes_uti"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 31%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Porcentagem de internações neonatais (até o 27º dia de vida) em UTI em relação ao total de partos no SUS&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_internacoes_uti"),
                      actionButton(
                        inputId = ns("info_btn_internacoes_uti"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_morbidade_neonatal_internacoes_uti")), proxy.height = "276px")
            )
          ),
          column(
            width = 6,
            bs4Dash::box(
              id = ns("caixa_b7_morbidade_neonatal_principais"),
              style = "height: 330px; overflow: visible; padding: 0;",
              width = 12,
              collapsible = FALSE,
              headerBorder = FALSE,
              div(
                class = "fonte-grande",
                style = glue::glue("height: 15%; padding: 0 10px; position: absolute; z-index: 2; color: transparent;"),
                htmltools::tagList(
                  HTML("<b> Dentre as internações neonatais selecionadas,&nbsp;</b>"),
                  shinyjs::hidden(
                    span(
                      id = ns("mostrar_botao_principais"),
                      actionButton(
                        inputId = ns("info_btn_principais"),
                        label = NULL,
                        icon = icon("info-circle", class = "info-icon"),
                        class = "btn btn-sm btn-no-style info-btn fonte-media"
                      )
                    )
                  )
                )
              ),
              shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_morbidade_neonatal_principais")), proxy.height = "276px")
            )
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
                HTML("<b class = 'fonte-muito-grande'> Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida &nbsp;</b>"),
                actionButton(
                  inputId = ns("botao_explicacao_indicador"),
                  label = NULL,
                  icon = icon("info-circle", class = "info-icon"),
                  class = "btn btn-sm btn-no-style info-btn fonte-media"
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_condicoes_ameacadoras"), height = 460))
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
                    inputId = ns("input_internacoes_local"),
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
                      inputId = ns("input_internacoes_idade"),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_internacoes"), height = 360))
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
                    inputId = ns("input_internacoes_uti_local"),
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
                      inputId = ns("input_internacoes_uti_idade"),
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_internacoes_uti"), height = 360))
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
                    inputId = ns("input_principais_grupos"),
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
                      inputId = ns("input_principais_momento"),
                      label = NULL,
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
              shinycssloaders::withSpinner(highcharter::highchartOutput(ns("plot_causas_principais"), height = 480))
            )
          )
        )
      )
    )
  )
}

#' bloco_7_morbidade_neonatal Server Functions
#'
#' @noRd
mod_bloco_7_morbidade_neonatal_server <- function(id, filtros){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Criando alguns outputs para a UI ----------------------------------------
    ## Criando os outputs que receberão os nomes dos locais selecionados quando há comparação --------
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


    ## Criando o output que receberá o nível selecionado ----------------------
    nivel_selecionado <- reactive({
      if (filtros()$comparar == "Não") {
        filtros()$nivel
      } else {
        req(input$localidade_resumo)
        if (input$localidade_resumo == "escolha1") {
          filtros()$nivel
        } else {
          filtros()$nivel2
        }
      }
    })


    ## Para os botões de informação em alguns indicadores ----------------------
    ### Criando o pop-up com a informação sobre o cálculo do indicador de condições ameaçadoras
    bs4Dash::addTooltip(
      session = session,
      id = "botao_explicacao_indicador",
      options = list(
        title = '<span class = "fonte-media">Foram considerados nascidos com condições potencialmente ameaçadoras à vida aqueles com peso < 1500 g, com idade gestacional < 32 semanas ou com Apgar de quinto minuto < 7. </span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )


    ## Criando o pop-up com a informação sobre o resumo do período -------------
    observeEvent(c(input$botao_resumo), {
      shinyalert::shinyalert(
        html = TRUE,
        title = '<div class = "fonte-titulos-modal" style = "color: #656565"> Sobre o "Resumo do período" </div>',
        text = '
          <div style = "text-align: justify; text-justify: inter-word;">
            Todos os resultados apresentados no "Resumo do período", na esquerda da página, referem-se aos valores dos indicadores calculados considerando todo o período selecionado.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando alguma comparação é feita, o usuário pode selecionar para qual localidade o resumo do período será calculado clicando em um dos botões com o nome das localidades que estão sendo comparadas.
            <span style="display: block; margin-bottom: 14px;"> </span>
            Quando o indicador permite a seleção de uma ou mais categorias, o resumo do período refere-se apenas à(s) categoria(s) selecionada(s).
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


    # Calculando os indicadores para cada ano do período selecionado ----------
    ## Criando funções auxiliares com as colunas e os cálculos dos indicadores
    ### Para a porcentagem de nascidos vivos com condições ameaçadoras à vida
    cols_ameacadoras <- function(input) {
      list(
        num_porc = "nascidos_condicoes_ameacadoras",
        den_porc = "total_de_nascidos_vivos"
      )
    }

    indicadores_ameacadoras <- function(s) {
      porc <- round(s$num_porc / s$den_porc * 100, 1)
      data.table::as.data.table(list(
        porc_condicoes_ameacadoras = porc
      ))
    }

    ### Para a porcentagem de internações neonatais
    cols_internacoes <- function(input) {
      list(
        num_porc = sprintf("internacoes_%s_%s", input$input_internacoes_local, input$input_internacoes_idade),
        den_porc = "nascidos_estabelecimentos_publicos_sih"
      )
    }

    indicadores_internacoes <- function(s) {
      porc <- round(s$num_porc / s$den_porc * 100, 1)
      data.table::as.data.table(list(
        porc_internacoes_menores_28_dias_sih = porc
      ))
    }

    ### Para a porcentagem de internações neonatais
    cols_internacoes_uti <- function(input) {
      list(
        num_porc = sprintf("internacoes_%s_%s_internado_uti", input$input_internacoes_uti_local, input$input_internacoes_uti_idade),
        den_porc = "nascidos_estabelecimentos_publicos_sih"
      )
    }

    indicadores_internacoes_uti <- function(s) {
      porc <- round(s$num_porc / s$den_porc * 100, 1)
      data.table::as.data.table(list(
        porc_internacoes_uti_menores_28_dias_sih = porc
      ))
    }


    ## Pré-filtrando os dados da localidade, comparação e refeência ------------
    bloco7_morbidade_neonatal_localidade <- reactive(filtra_localidade(bloco7_morbidade_neonatal, filtros()))
    bloco7_morbidade_neonatal_comp <- reactive(filtra_localidade(bloco7_morbidade_neonatal, filtros(), comp = TRUE))
    bloco7_morbidade_neonatal_referencia <- reactive(filtra_localidade(bloco7_morbidade_neonatal, filtros(), referencia = TRUE))

    data_filtrada_morbidade_graficos <- reactive({
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

    data_filtrada_comp_morbidade_graficos <- reactive({
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

    data_filtrada_morbidade_resumo <- reactive({
      if (filtros()$comparar == "Não") {
        sufixo_inputs <- ""
      } else {
        req(input$localidade_resumo)
        if (input$localidade_resumo == "escolha1") {
          sufixo_inputs <- ""
        } else {
          sufixo_inputs <- "2"
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


    ## Calculando os indicadores para a localidade escolhida ------------------
    ### A função processa_bloco7 retorna dataframes do resumo e dos gráficos
    dfs_localidade_ameacadoras <- reactive(processa_bloco7(bloco7_morbidade_neonatal_localidade(), input, filtros(), cols_ameacadoras, indicadores_ameacadoras))
    dfs_localidade_internacoes <- reactive(processa_bloco7(bloco7_morbidade_neonatal_localidade(), input, filtros(), cols_internacoes, indicadores_internacoes))
    dfs_localidade_internacoes_uti <- reactive(processa_bloco7(bloco7_morbidade_neonatal_localidade(), input, filtros(), cols_internacoes_uti, indicadores_internacoes_uti))

    ### Criando dataframes separados para o resumo
    data7_resumo_localidade_ameacadoras <- eventReactive(c(filtros()$pesquisar), {
      dfs_localidade_ameacadoras()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_localidade_internacoes <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_local, input$input_internacoes_idade), {
      dfs_localidade_internacoes()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_localidade_internacoes_uti <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_uti_local, input$input_internacoes_uti_idade), {
      dfs_localidade_internacoes_uti()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_principais <- reactive({
      data_filtrada_morbidade_resumo() |>
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
          values_to = "porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, porc_obitos) |>
        dplyr::mutate(
          grupo_cid10 = dplyr::case_when(
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
          porc_obitos = round(porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo_cid10, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })


    ### Criando dataframes separados para os gráficos
    data7_localidade_ameacadoras <- eventReactive(c(filtros()$pesquisar), {
      dfs_localidade_ameacadoras()$graficos
    }, ignoreNULL = FALSE)

    data7_localidade_internacoes <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_local, input$input_internacoes_idade), {
      dfs_localidade_internacoes()$graficos
    }, ignoreNULL = FALSE)

    data7_localidade_internacoes_uti <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_uti_local, input$input_internacoes_uti_idade), {
      dfs_localidade_internacoes_uti()$graficos
    }, ignoreNULL = FALSE)

    data7_localidade_principais <- reactive({
      data_filtrada_morbidade_graficos() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$input_principais_momento)))), na.rm=T) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_internacoes(input = input$input_principais_momento))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_internacoes(input = input$input_principais_momento)),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$input_principais_grupos, collapse="|"), grupo_cid10) & !is.null(input$input_principais_grupos)),
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
            (grepl(paste(input$input_principais_grupos, collapse="|"), grupo_cid10) & !is.null(input$input_principais_grupos)),
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


    ## Para a comparação selecionada ------------------------------------------
    dfs_comp_ameacadoras <- reactive(processa_bloco7(bloco7_morbidade_neonatal_comp(), input, filtros(), cols_ameacadoras, indicadores_ameacadoras, comp = TRUE))
    dfs_comp_internacoes <- reactive(processa_bloco7(bloco7_morbidade_neonatal_comp(), input, filtros(), cols_internacoes, indicadores_internacoes, comp = TRUE))
    dfs_comp_internacoes_uti <- reactive(processa_bloco7(bloco7_morbidade_neonatal_comp(), input, filtros(), cols_internacoes_uti, indicadores_internacoes_uti, comp = TRUE))

    ### Criando dataframes separados para o resumo
    data7_resumo_comp_ameacadoras <- eventReactive(c(filtros()$pesquisar), {
      dfs_comp_ameacadoras()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_comp_internacoes <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_local, input$input_internacoes_idade), {
      dfs_comp_internacoes()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_comp_internacoes_uti <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_uti_local, input$input_internacoes_uti_idade), {
      dfs_comp_internacoes_uti()$resumo
    }, ignoreNULL = FALSE)

    ### Criando dataframes separados para os gráficos
    data7_comp_ameacadoras <- eventReactive(c(filtros()$pesquisar), {
      dfs_comp_ameacadoras()$graficos
    }, ignoreNULL = FALSE)

    data7_comp_internacoes <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_local, input$input_internacoes_idade), {
      dfs_comp_internacoes()$graficos
    }, ignoreNULL = FALSE)

    data7_comp_internacoes_uti <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_uti_local, input$input_internacoes_uti_idade), {
      dfs_comp_internacoes_uti()$graficos
    }, ignoreNULL = FALSE)

    data7_comp_principais <- reactive({
      data_filtrada_comp_morbidade_graficos() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$input_principais_momento))), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_internacoes(input = input$input_principais_momento))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_internacoes(input = input$input_principais_momento)),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$input_principais_grupos, collapse="|"), grupo_cid10) & !is.null(input$input_principais_grupos)),
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
            (grepl(paste(input$input_principais_grupos, collapse="|"), grupo_cid10) & !is.null(input$input_principais_grupos)),
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


    ## Para a referência ------------------------------------------------------
    ### A função processa_bloco7 retorna dataframes do resumo e dos gráficos
    dfs_referencia_ameacadoras <- reactive(processa_bloco7(bloco7_morbidade_neonatal_referencia(), input, filtros(), cols_ameacadoras, indicadores_ameacadoras, referencia = TRUE))
    dfs_referencia_internacoes <- reactive(processa_bloco7(bloco7_morbidade_neonatal_referencia(), input, filtros(), cols_internacoes, indicadores_internacoes, referencia = TRUE))
    dfs_referencia_internacoes_uti <- reactive(processa_bloco7(bloco7_morbidade_neonatal_referencia(), input, filtros(), cols_internacoes_uti, indicadores_internacoes_uti, referencia = TRUE))

    ### Criando dataframes separados para o resumo
    data7_resumo_referencia_ameacadoras <- eventReactive(c(filtros()$pesquisar), {
      dfs_referencia_ameacadoras()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_referencia_internacoes <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_local, input$input_internacoes_idade), {
      dfs_referencia_internacoes()$resumo
    }, ignoreNULL = FALSE)

    data7_resumo_referencia_internacoes_uti <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_uti_local, input$input_internacoes_uti_idade), {
      dfs_referencia_internacoes_uti()$resumo
    }, ignoreNULL = FALSE)

    ### Criando dataframes separados para os gráficos
    data7_referencia_ameacadoras <- eventReactive(c(filtros()$pesquisar), {
      dfs_referencia_ameacadoras()$graficos
    }, ignoreNULL = FALSE)

    data7_referencia_internacoes <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_local, input$input_internacoes_idade), {
      dfs_referencia_internacoes()$graficos
    }, ignoreNULL = FALSE)

    data7_referencia_internacoes_uti <- eventReactive(c(filtros()$pesquisar, input$input_internacoes_uti_local, input$input_internacoes_uti_idade), {
      dfs_referencia_internacoes_uti()$graficos
    }, ignoreNULL = FALSE)

    data7_referencia_principais <- reactive({
      bloco7_dist_morbidade |>
        dplyr::filter(
          ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = input$input_principais_momento))), na.rm=T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_internacoes(input = input$input_principais_momento))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_internacoes(input = input$input_principais_momento)),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::mutate(
          grupo_cid10 = ifelse(
            (grepl(paste(input$input_principais_grupos, collapse="|"), grupo_cid10) & !is.null(input$input_principais_grupos)),
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

    data7_principais_completo <- reactive({
      validate(
        need(
          nrow(data7_localidade_principais()) != 0,
          "Não existem ocorrências de internações neonatais por grupos de causas para a localidade, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data7_localidade_principais(), data7_referencia_principais())
    })

    data7_comp_principais_completo <- reactive({
      validate(
        need(
          nrow(data7_comp_principais()) != 0,
          "Não existem ocorrências de internações neonatais por grupos de causas para a localidade de comparação, período e grupos CID-10 selecionados."
        )
      )
      dplyr::full_join(data7_comp_principais(), data7_referencia_principais())
    })


    ## Decidindo qual resumo mostrar ------------------------------------------
    escolhe_resumo <- function(comparar, localidade_resumo, df_localidade, df_comparacao) {
      if (comparar == "Não") {
        df_localidade
      } else {
        req(localidade_resumo)
        if (localidade_resumo == "escolha1") {
          df_localidade
        } else {
          df_comparacao
        }
      }
    }

    data7_resumo_ameacadoras <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_ameacadoras(), data7_resumo_comp_ameacadoras()))
    data7_resumo_internacoes <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_internacoes(), data7_resumo_comp_internacoes()))
    data7_resumo_internacoes_uti <- reactive(escolhe_resumo(filtros()$comparar, input$localidade_resumo, data7_resumo_localidade_internacoes_uti(), data7_resumo_comp_internacoes_uti()))


    ## Criando os outputs das caixinhas ---------------------------------------
    ### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida
    output$caixa_b7_morbidade_neonatal_condicoes_ameacadoras <- renderUI({
      cria_caixa_server(
        dados = data7_resumo_ameacadoras(),
        indicador = "porc_condicoes_ameacadoras",
        titulo = "Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida",
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia_ameacadoras()$porc_condicoes_ameacadoras,
        invertido = FALSE,
        pagina = "bloco_7",
        tamanho_caixa = 330,
        tipo = "porcentagem",
        nivel_de_analise = nivel_selecionado()
      )
    })


    ### Porcentagem de internações neonatais (até o 27º dia de vida) em relação ao total de partos no SUS ----
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_internacoes",
      options = list(
        title = '<span class = "fonte-media">A média apresentada para o período refere-se à(s) categoria(s) selecionadas no respectivo gráfico à direita.</span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_internacoes <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_internacoes_local, input$input_internacoes_idade), {
      output_pronto_internacoes(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_internacoes", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_morbidade_neonatal_internacoes <- renderUI({
      output_pronto_internacoes(TRUE)
      cria_caixa_server(
        dados = data7_resumo_internacoes(),
        indicador = "porc_internacoes_menores_28_dias_sih",
        titulo = "Porcentagem de internações neonatais (até o 27º dia de vida) em relação ao total de partos no SUS",
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia_internacoes()$porc_internacoes_menores_28_dias_sih,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "bloco_7",
        tamanho_caixa = 330,
        nivel_de_analise = nivel_selecionado(),
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_internacoes(), {
      if (output_pronto_internacoes()) {
        shinyjs::show(id = "mostrar_botao_internacoes", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    ### Porcentagem de internações neonatais (até o 27º dia de vida) em UTI em relação ao total de partos no SUS ----
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_internacoes_uti",
      options = list(
        title = '<span class = "fonte-media">A média apresentada para o período refere-se à(s) categoria(s) selecionadas no respectivo gráfico à direita.</span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_internacoes_uti <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_internacoes_uti_local, input$input_internacoes_uti_idade), {
      output_pronto_internacoes_uti(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_internacoes_uti", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_morbidade_neonatal_internacoes_uti <- renderUI({
      output_pronto_internacoes_uti(TRUE)
      cria_caixa_server(
        dados = data7_resumo_internacoes_uti(),
        indicador = "porc_internacoes_uti_menores_28_dias_sih",
        titulo = "Porcentagem de internações neonatais (até o 27º dia de vida) em UTI em relação ao total de partos no SUS",
        tem_meta = FALSE,
        valor_de_referencia = data7_resumo_referencia_internacoes_uti()$porc_internacoes_uti_menores_28_dias_sih,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "bloco_7",
        tamanho_caixa = 330,
        nivel_de_analise = nivel_selecionado(),
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_internacoes_uti(), {
      if (output_pronto_internacoes_uti()) {
        shinyjs::show(id = "mostrar_botao_internacoes_uti", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    #### Distribuição percentual do óbito por grupo de causas -----------------
    #### Criando a tooltip para essa caxinha
    bs4Dash::addTooltip(
      session = session,
      id = "info_btn_principais",
      options = list(
        title = '<span class = "fonte-media">A média apresentada para o período refere-se à(s) categoria(s) selecionadas no respectivo gráfico à direita.</span>',
        placement = "top",
        animation = TRUE,
        html = TRUE,
        delay = list(show = 75, hide = 75) # delay em ms
      )
    )

    output_pronto_principais <- reactiveVal(FALSE)

    #### Escondendo o botão da tooltip antes de começar a renderizar de novo
    observeEvent(c(filtros()$pesquisar, input$localidade_resumo, input$input_principais_momento, input$input_principais_grupos), {
      output_pronto_principais(FALSE)  # Reseta
      shinyjs::hide(id = "mostrar_botao_principais", anim = FALSE)
    }, priority = 1)

    #### Criando a caixinha
    output$caixa_b7_morbidade_neonatal_principais <- renderUI({
      output_pronto_principais(TRUE)
      cria_caixa_principais_evitaveis_bloco7(
        dados = data7_resumo_principais(),
        titulo = "Dentre as internações neonatais selecionadas,",
        tamanho_caixa = 330,
        retornar_caixa_completa = FALSE
      )
    })

    #### Mostra o botão da tooltip quando a caixa estiver pronta
    observeEvent(output_pronto_principais(), {
      if (output_pronto_principais()) {
        shinyjs::show(id = "mostrar_botao_principais", anim = TRUE, animType = "fade", time = 0.8)
      }
    })


    # Criando os outputs dos gráficos -----------------------------------------
    ## Definindo as cores usadas nos gráficos ---------------------------------
    cols <- c("#51127CFF", "#b73779", "#fc8961", "#000004FF", "#f1605d")

    ## Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    output$plot_condicoes_ameacadoras <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_localidade_ameacadoras(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_localidade_ameacadoras()$class),
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
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_ameacadoras(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_localidade_ameacadoras(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_localidade_ameacadoras()$class),
            highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_ameacadoras(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_comp_ameacadoras()$class),
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
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_ameacadoras(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_condicoes_ameacadoras, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })


    ## Porcentagem de internações neonatais (até o 27º dia de vida) em relação ao total de partos no SUS ----
    output$plot_internacoes <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_localidade_internacoes(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_localidade_internacoes()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
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
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_internacoes(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_localidade_internacoes(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_localidade_internacoes()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_internacoes(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_comp_internacoes()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class)
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
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_internacoes(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_menores_28_dias_sih, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })


    ## Porcentagem de internações neonatais (até o 27º dia de vida) em UTI em relação ao total de partos no SUS ----
    output$plot_internacoes_uti <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_localidade_internacoes_uti(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_localidade_internacoes_uti()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
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
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_internacoes_uti(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.8
            )
        }
      } else {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_dependency("modules/series-label.js") |>
          highcharter::hc_add_series(
            data = data7_localidade_internacoes_uti(),
            type = "line",
            name = ifelse(filtros()$nivel == "nacional", "Brasil (valor de referência)", data7_localidade_internacoes_uti()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_internacoes_uti(),
            type = "line",
            name = ifelse(filtros()$nivel2 == "Brasil", "Brasil (valor de referência)", data7_comp_internacoes_uti()$class),
            highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class)
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
        if (any(c(filtros()$nivel, filtros()$nivel2) == "nacional") | (filtros()$mostrar_referencia == "nao_mostrar_referencia")) {
          grafico_base
        } else {
          grafico_base |>
            highcharter::hc_add_series(
              data = data7_referencia_internacoes_uti(),
              type = "line",
              name = "Referência (média nacional)",
              highcharter::hcaes(x = ano, y = porc_internacoes_uti_menores_28_dias_sih, group = class, colour = class),
              dashStyle = "ShortDot",
              opacity = 0.6
            )
        }
      }
    })

    ## Distribuição percentual das internações neonatais por grupos de causas ---------
    output$plot_causas_principais <- highcharter::renderHighchart({
      if (filtros()$comparar == "Não") {
        grafico_base <- highcharter::highchart() |>
          highcharter::hc_add_series(
            data = data7_principais_completo(),
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
            data = data7_principais_completo(),
            highcharter::hcaes(x = ano, y = porc_obitos, group = grupo_cid10),
            type = "column",
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = "<span style = 'color: {series.color}'> &#9679 </span> {series.name} <b>({point.class})</b>: <b> {point.y}% </b> <br> Média nacional: <b> {point.br_porc_obitos:,f}% </b>"
            ),
            stack = 0
          ) |>
          highcharter::hc_add_series(
            data = data7_comp_principais_completo(),
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
          viridis::magma(length(unique(data7_principais_completo()$grupo_cid10)) + 2, direction = 1)[-c(1, length(unique(data7_principais_completo()$grupo_cid10)) + 2)]
        ) |>
        highcharter::hc_xAxis(title = list(text = ""), categories = unique(data7_principais_completo()$ano), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% relativo ao total de internações neonatais"), min = 0, max = 100)
    })


  })
}

## To be copied in the UI
# mod_bloco_7_morbidade_neonatal_ui("bloco_7_morbidade_neonatal_1")

## To be copied in the server
# mod_bloco_7_morbidade_neonatal_server("bloco_7_morbidade_neonatal_1")
