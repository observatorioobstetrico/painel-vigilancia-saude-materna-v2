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
              div(
                HTML("<b style='font-size:18px'> Resumo dos indicadores &nbsp;</b>")
              ),
              hr(),
              div(
                shinyWidgets::pickerInput(ns("selected_indicators"), "Escolha até 5 indicadores:",
                                          choices = NULL,  # Inicialmente vazio, será atualizado no server
                                          options = list(
                                            `max-options` = 5,
                                            `max-options-text` = "Você só pode selecionar até 5 opções!",
                                            `none-selected-text` = "Nenhuma opção selecionada",
                                            `select-all-text` = "Selecionar Todos",
                                            `deselect-all-text` = "Desmarcar Todos"
                                          ),
                                          multiple = TRUE,
                                          selected = c('porc_nvm_menor_que_20_anos',
                                                       'porc_nvm_entre_20_e_34_anos',
                                                       'porc_nvm_maior_que_34_anos'
                                                       # 'porc_nvm_com_cor_da_pele_branca',
                                                       # 'porc_nvm_com_cor_da_pele_preta'
                                                       )
               )
              ),
                shinycssloaders::withSpinner(highcharter::highchartOutput(ns("spider_chart"), height = 800))
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


# verificar rmm bloco 6-----------------------------------------------------------


# tabela escolha de indicadores -------------------------------------------

# para atualizar a tabela de indicadores
# recomendo baixar a tabela em formato xlsx, atualizar e salvar em csv novamente
# tabela_radar <- tabela_radar
# xlsx::write.xlsx(tabela_radar, 'local/tabela_radar.xlsx')


tabela_radar <- tabela_radar |>
  dplyr::mutate(bloco = paste0(toupper(substring(bloco, 1, 1)), substr(bloco, 2, 5), " ", substr(bloco, 6, 6)),
                indicador = paste(bloco, "-", indicador),
                nome_grafico = substring(indicador, 11)) |>
  dplyr::filter(entra == "sim")


# Atualizando as opções do pickerInput
shinyWidgets::updatePickerInput(session, "selected_indicators",
                  choices = setNames(tabela_radar$nome_abreviado, tabela_radar$indicador),
                  selected = c('porc_nvm_menor_que_20_anos',
                               'porc_nvm_entre_20_e_34_anos',
                               'porc_nvm_maior_que_34_anos'
                               # 'porc_nvm_com_cor_da_pele_branca',
                               # 'porc_nvm_com_cor_da_pele_preta'
                               ))


user_interacted <- reactiveVal(FALSE)

observeEvent(input$selected_indicators, {
  if (user_interacted()) {
    if (length(input$selected_indicators) != 5) {
      showNotification("Por favor, selecione exatamente 5 indicadores.", type = "error")
    }
  } else {
    user_interacted(TRUE)
  }
})

# mensagem input -------------------------------------------
    # observe({
    #   if (length(input$selected_indicators) != 5) {
    #     showNotification("Por favor, selecione exatamente 5 indicadores.", type = "error")
    #   }
    # })



# bloco 1 -----------------------------------------------------------------

    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco1_calcs <- data.frame(
      tipo = c("local", "referencia"),
      sum_total_de_nascidos_vivos = rep("sum(total_de_nascidos_vivos)", 2),
      porc_dependentes_sus = rep("round((sum(populacao_feminina_10_a_49[ano <= 2021]) - sum(pop_fem_10_49_com_plano_saude[ano <= 2021]))/sum(populacao_feminina_10_a_49[ano <= 2021]) * 100, 1)", 2),
      porc_cobertura_esf = c("round(sum(media_cobertura_esf[ano <= 2022])/sum(populacao_total[ano <= 2022]) * 100, 1)", "dplyr::first(95[ano <= 2022])"),
      porc_nvm_menor_que_20_anos = rep("round(sum(nvm_menor_que_20_anos)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_entre_20_e_34_anos = rep("round(sum(nvm_entre_20_e_34_anos)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_maior_que_34_anos = rep("round(sum(nvm_maior_que_34_anos)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_escolaridade_ate_3 = rep("round(sum(nvm_com_escolaridade_ate_3)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_escolaridade_de_4_a_7 = rep("round(sum(nvm_com_escolaridade_de_4_a_7)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_escolaridade_de_8_a_11 = rep("round(sum(nvm_com_escolaridade_de_8_a_11)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_escolaridade_acima_de_11 = rep("round(sum(nvm_com_escolaridade_acima_de_11)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_cor_da_pele_branca = rep("round(sum(nvm_com_cor_da_pele_branca)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_cor_da_pele_preta = rep("round(sum(nvm_com_cor_da_pele_preta)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_cor_da_pele_amarela = rep("round(sum(nvm_com_cor_da_pele_amarela)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_com_cor_da_pele_parda = rep("round(sum(nvm_com_cor_da_pele_parda)/sum(total_de_nascidos_vivos) * 100, 1)", 2),
      porc_nvm_indigenas = rep("round(sum(nvm_indigenas)/sum(total_de_nascidos_vivos) * 100, 1)", 2)
    )


    ### Para a localidade selecionada -----------------------------------------
    data1 <- reactive({
      bloco1 |>
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
        cria_indicadores(df_calcs = bloco1_calcs, filtros = filtros())
    })

    ### Para a comparação selecionada -----------------------------------------
    data1_comp <- reactive({
      bloco1 |>
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
        cria_indicadores(df_calcs = bloco1_calcs, filtros = filtros(), comp = TRUE)
    })


# bloco 2 -----------------------------------------------------------------

    ## Criando um data.frame com os cálculos dos indicadores -------------------
    bloco2_calcs <- data.frame(
      tipo = c("local", "referencia"),

      porc_menor20 = c("round(sum(nvm_menor_que_20[ano <= 2021]) / sum(pop_feminina_10_a_19[ano <= 2021]) * 1000, 1)", "dplyr::first(20[ano <= 2021])"),
      porc_mais_3pt = rep("round(sum(mulheres_com_mais_de_tres_partos_anteriores) / sum(total_de_nascidos_vivos) * 100, 1)", 2),

      geral_tx_abortos_mil_mulheres_lim_inf = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 3) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 5)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),
      geral_tx_abortos_mil_mulheres_valor_medio = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 4) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 6)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),
      geral_tx_abortos_mil_mulheres_lim_sup = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 5) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 7)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),

      sus_tx_abortos_mil_mulheres_lim_inf = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 3) / sum(pop_fem_sus_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),
      sus_tx_abortos_mil_mulheres_valor_medio = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 4) / sum(pop_fem_sus_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),
      sus_tx_abortos_mil_mulheres_lim_sup = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 5) / sum(pop_fem_sus_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),

      ans_tx_abortos_mil_mulheres_lim_inf = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 5) / sum(pop_fem_ans_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),
      ans_tx_abortos_mil_mulheres_valor_medio = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 6) / sum(pop_fem_ans_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),
      ans_tx_abortos_mil_mulheres_lim_sup = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 7) / sum(pop_fem_ans_10_49[ano >= 2015 & ano <= 2021]) * 1000, 1)", 2),

      geral_tx_abortos_cem_nascidos_vivos_lim_inf = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015]) * 0.75)) * 3) + (((sum(abortos_ans_menor_30[ano >= 2015]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015]) * 0.75)) * 5)) / sum(total_de_nascidos_vivos[ano >= 2015]) * 100, 1)", 2),
      geral_tx_abortos_cem_nascidos_vivos_valor_medio = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015]) * 0.75)) * 4) + (((sum(abortos_ans_menor_30[ano >= 2015]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015]) * 0.75)) * 6)) / sum(total_de_nascidos_vivos[ano >= 2015]) * 100, 1)", 2),
      geral_tx_abortos_cem_nascidos_vivos_lim_sup = rep("round(((((sum(abortos_sus_menor_30[ano >= 2015]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015]) * 0.75)) * 5) + (((sum(abortos_ans_menor_30[ano >= 2015]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015]) * 0.75)) * 7)) / sum(total_de_nascidos_vivos[ano >= 2015]) * 100, 1)", 2),

      sus_tx_abortos_cem_nascidos_vivos_lim_inf = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 3) / sum(total_de_nascidos_vivos_sus[ano >= 2015 & ano <= 2021]) * 100, 1)", 2),
      sus_tx_abortos_cem_nascidos_vivos_valor_medio = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 4) / sum(total_de_nascidos_vivos_sus[ano >= 2015 & ano <= 2021]) * 100, 1)", 2),
      sus_tx_abortos_cem_nascidos_vivos_lim_sup = rep("round((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 5) / sum(total_de_nascidos_vivos_sus[ano >= 2015 & ano <= 2021]) * 100, 1)", 2),

      ans_tx_abortos_cem_nascidos_vivos_lim_inf = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 5) / sum(total_de_nascidos_vivos_ans[ano >= 2015 & ano <= 2021]) * 100, 1)", 2),
      ans_tx_abortos_cem_nascidos_vivos_valor_medio = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 6) / sum(total_de_nascidos_vivos_ans[ano >= 2015 & ano <= 2021]) * 100, 1)", 2),
      ans_tx_abortos_cem_nascidos_vivos_lim_sup = rep("round((((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2021]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2021]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2021]) * 0.75)) * 7) / sum(total_de_nascidos_vivos_ans[ano >= 2015 & ano <= 2021]) * 100, 1)", 2)
    )


    ### Para a localidade selecionada -----------------------------------------
    data2 <- reactive({
      bloco2 |>
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
        cria_indicadores(df_calcs = bloco2_calcs, filtros = filtros())
    })

    ### Para a comparação selecionada -----------------------------------------
    data2_comp <- reactive({
      bloco2 |>
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
        cria_indicadores(df_calcs = bloco2_calcs, filtros = filtros(), comp = TRUE)
    })



    # bloco 3 -----------------------------------------------------------------

    ## Criando um data.frame com os cálculos dos indicadores -------------------
    bloco3_calcs <- data.frame(
      tipo = c("local", "referencia"),
      porc_inicio_prec = c("round(sum(mulheres_com_inicio_precoce_do_prenatal) / sum(total_de_nascidos_vivos) * 100, 1)", "95"),
      porc_sc = c("round(sum(casos_sc) / sum(total_de_nascidos_vivos) * 1000, 1)", "0.5"),
      porc_1con = c("round(sum(mulheres_com_pelo_menos_uma_consulta_prenatal[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 1)", "dplyr::first(95[ano >= 2014])"),
      porc_7 = c("round(sum(mulheres_com_mais_de_sete_consultas_prenatal[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 1)", "dplyr::first(95[ano >= 2014])"),
      porc_consultas_adequadas = rep("round(sum(mulheres_com_consultas_prenatal_adequadas[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 2)", 2)
    )


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
        cria_indicadores(df_calcs = bloco3_calcs, filtros = filtros(), comp = TRUE)
    })



    # bloco 4 -----------------------------------------------------------------

# Criando data.frames com os cálculos dos indicadores ---------------------
bloco4_calcs <- data.frame(
  tipo = c("local", "referencia"),
  prop_nasc_robson1 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_1) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
  prop_nasc_robson2 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_2) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
  prop_nasc_robson3 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_3) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
  prop_nasc_robson4 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_4) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
  prop_nasc_robson5 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_5) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
  prop_nasc_robson6_a_9 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
  prop_nasc_robson10 = rep("round((sum(mulheres_dentro_do_grupo_de_robson_10) / sum(total_de_nascidos_vivos)) * 100, 1)", 2),
  prop_nasc_robson_faltante = rep('round((sum(total_de_nascidos_vivos) - sum(dplyr::across(dplyr::starts_with("mulheres_dentro")))) / sum(total_de_nascidos_vivos) * 100, 1)', 2),
  prop_tx_cesariana_geral = c("round(sum(mulheres_com_parto_cesariana)/sum(total_de_nascidos_vivos) * 100, 1)", "25"),
  prop_robson1_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_1) / sum(mulheres_dentro_do_grupo_de_robson_1)) * 100, 1)", "10"),
  prop_robson2_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_2) / sum(mulheres_dentro_do_grupo_de_robson_2)) * 100, 1)", "35"),
  prop_robson3_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_3) / sum(mulheres_dentro_do_grupo_de_robson_3)) * 100, 1)", "3"),
  prop_robson4_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_4) / sum(mulheres_dentro_do_grupo_de_robson_4)) * 100, 1)", "15"),
  prop_robson5_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_5) / sum(mulheres_dentro_do_grupo_de_robson_5)) * 100, 1)", "60"),
  prop_robson6_a_9_tx_cesariana = rep("round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1)", 2),
  prop_robson10_tx_cesariana = c("round((sum(total_cesariana_grupo_robson_10) / sum(mulheres_dentro_do_grupo_de_robson_10)) * 100, 1)", "30"),
  contrib_robson1_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_1) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
  contrib_robson2_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_2) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
  contrib_robson3_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_3) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
  contrib_robson4_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_4) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
  contrib_robson5_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_5) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
  contrib_robson6_a_9_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
  contrib_robson10_tx_cesariana = rep("round(sum(total_cesariana_grupo_robson_10) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2),
  contrib_robson_faltante_tx_cesariana = rep("round((sum(mulheres_com_parto_cesariana) - sum(dplyr::across(dplyr::starts_with('total_cesariana')))) / sum(mulheres_com_parto_cesariana) * 100, 1)", 2)
)

bloco4_deslocamento_calcs <- data.frame(
  tipo = c("local", "referencia"),
  prop_partos_municipio_res = rep("round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
  prop_partos_fora_municipio_res = rep("round(sum(nao_local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
  prop_partos_rsaude_res = rep("round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
  prop_partos_macro_rsaude_res = rep("round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
  prop_partos_fora_macro_rsaude_res = rep("round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2),
  prop_partos_fora_uf_res = rep("round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)", 2)
)

bloco4_deslocamento_macro_calcs <- data.frame(  # Esse dataframe vai juntar com o de cima quando as informações estiverem na mesma base
  tipo = c("local", "referencia"),
  prop_partos_na_macro_com_uti = rep("round(sum(partos_na_macro_com_uti)/sum(nascimentos) * 100, 1)", 2),
  prop_partos_na_macro_sem_uti = rep("round(sum(partos_na_macro_sem_uti)/sum(nascimentos) * 100, 1)", 2),
  prop_partos_fora_macro_com_uti = rep("round(sum(partos_fora_macro_com_uti)/sum(nascimentos) * 100, 1)", 2),
  prop_partos_fora_macro_sem_uti = rep("round(sum(partos_fora_macro_sem_uti)/sum(nascimentos) * 100, 1)", 2),
  prop_partos_na_macro_sem_inf = rep("round(sum(partos_na_macro_sem_inf)/sum(nascimentos) * 100, 1)", 2),
      prop_partos_fora_macro_sem_inf = rep("round(sum(partos_fora_macro_sem_inf)/sum(nascimentos) * 100, 1)", 2),
      prop_partos_sem_uti = rep("round((sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_sem_uti)) / (sum(partos_na_macro_com_uti) + sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_com_uti) + sum(partos_fora_macro_sem_uti)) * 100, 1)", 2)

    )

### Para a localidade selecionada -----------------------------------------


data4 <- reactive({
  bloco4 |>
    dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
    cria_indicadores(df_calcs = bloco4_calcs, filtros = filtros())
})

data4_deslocamento <- reactive({
  bloco4_deslocamento_muni |>
    dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
    cria_indicadores(df_calcs = bloco4_deslocamento_calcs, filtros = filtros())
})

data4_macrorregiao <- reactive({  # Esse dataframe vai sumir quando juntarem as bases de deslocamento
  bloco4_deslocamento_macrorregiao |>
    dplyr::ungroup() |>
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
    cria_indicadores(df_calcs = bloco4_deslocamento_macro_calcs, filtros = filtros())
})

### Para a comparação selecionada -----------------------------------------

data4_comp <- reactive({
  bloco4 |>
    dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
    cria_indicadores(df_calcs = bloco4_calcs, filtros = filtros(), comp = TRUE)
})

data4_deslocamento_comp <- reactive({
  bloco4_deslocamento_muni |>
    dplyr::filter(ano >= max(2014, filtros()$ano2[1]) & ano <= filtros()$ano2[2]) |>
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
    cria_indicadores(df_calcs = bloco4_deslocamento_calcs, filtros = filtros(), comp = TRUE)
})

data4_macrorregiao_comp <- reactive({  # Esse dataframe vai sumir quando juntarem as bases de deslocamento
  bloco4_deslocamento_macrorregiao |>
    dplyr::ungroup() |>
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
    cria_indicadores(df_calcs = bloco4_deslocamento_macro_calcs, filtros = filtros(), comp = TRUE)
})


    # bloco 5 -----------------------------------------------------------------


    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco5_calcs <- reactive({
      df_calcs_aux1 <- data.frame(
        tipo = c("local", "referencia"),
        porc_nasc_baixo_peso = rep("round(sum(nascidos_vivos_com_baixo_peso) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_peso_menor_1500 = rep("round(sum(nascidos_vivos_peso_menor_1500) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_peso_1500_a_1999 = rep("round(sum(nascidos_vivos_peso_1500_a_1999) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_peso_2000_a_2499 = rep("round(sum(nascidos_vivos_peso_2000_a_2499) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_baixo_peso_menor_1500 = rep("round(sum(nascidos_vivos_peso_menor_1500) / sum(nascidos_vivos_com_baixo_peso) * 100, 1)", 2),
        porc_baixo_peso_1500_a_1999 = rep("round(sum(nascidos_vivos_peso_1500_a_1999) / sum(nascidos_vivos_com_baixo_peso) * 100, 1)", 2),
        porc_baixo_peso_2000_a_2499 = rep("round(sum(nascidos_vivos_peso_2000_a_2499) / sum(nascidos_vivos_com_baixo_peso) * 100, 1)", 2),
        porc_nasc_premat = c("round(sum(nascidos_vivos_prematuros) / sum(total_de_nascidos_vivos) * 100, 1)", "10"),
        porc_nasc_menos_de_28_semanas = rep("round(sum(nascidos_vivos_menos_de_28_semanas) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_28_a_32_semanas = rep("round(sum(nascidos_vivos_28_a_32_semanas) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_33_a_34_semanas = rep("round(sum(nascidos_vivos_33_a_34_semanas) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nasc_35_a_36_semanas = rep("round(sum(nascidos_vivos_35_a_36_semanas) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_premat_menos_de_28_semanas = rep("round(sum(nascidos_vivos_menos_de_28_semanas) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_premat_28_a_32_semanas = rep("round(sum(nascidos_vivos_28_a_32_semanas) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_premat_33_a_34_semanas = rep("round(sum(nascidos_vivos_33_a_34_semanas) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_premat_35_a_36_semanas = rep("round(sum(nascidos_vivos_35_a_36_semanas) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_premat_faltantes = rep("round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                           nascidos_vivos_28_a_32_semanas,
                                                                                           nascidos_vivos_33_a_34_semanas,
                                                                                           nascidos_vivos_35_a_36_semanas)))) / sum(nascidos_vivos_prematuros) * 100, 1)", 2),
        porc_termo_precoce = c("round(sum(nascidos_vivos_termo_precoce) / sum(total_de_nascidos_vivos) * 100, 1)", "20"),
        porc_condicoes_ameacadoras = rep("round(sum(nascidos_condicoes_ameacadoras) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_nascidos_vivos_asfixia1 = rep("round(sum(nascidos_vivos_asfixia1) / sum(total_nascidos) * 100, 1)", 2),
        porc_malformacao_geral = rep("round(sum(total_de_nascidos_malformacao) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_malformacao_vigilancia = rep("round(sum(nascidos_vivos_anomalia) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
        porc_internacoes_menores_28_dias_vinc_sus_geral = rep("round(sum(internacoes_geral_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_sus[ano <= 2022]) * 100, 1)", 2),
        porc_internacoes_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)", 2),
        porc_internacoes_uti_menores_28_dias_sih_geral = rep("round(sum(internacoes_geral_geral_internado_uti[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)", 2)
      )

      if (is.null(input$idade_dias_sus[1])) {
        df_calcs_aux2 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_menores_28_dias_vinc_sus = "NA"
        )
      } else {
        df_calcs_aux2 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_menores_28_dias_vinc_sus = rep(glue::glue(
            "ifelse(
              length(input$idade_dias_sus) == 2,
              round(sum(internacoes_{input$local_internacao_sus}_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_sus[ano <= 2022]) * 100, 1),
              round(sum(internacoes_{input$local_internacao_sus}_{input$idade_dias_sus[1]}[ano <= 2022]) / sum(nascidos_estabelecimentos_sus[ano <= 2022]) * 100, 1)
            )"
          ), 2)
        )
      }

      if (is.null(input$idade_dias_sih[1])) {
        df_calcs_aux3 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_menores_28_dias_sih = "NA"
        )
      } else {
        df_calcs_aux3 <- data.frame(
          tipo = c("local", "referencia"),
          porc_internacoes_menores_28_dias_sih = rep(glue::glue(
            "ifelse(
              length(input$idade_dias_sih) == 2,
              round(sum(internacoes_{input$local_internacao_sih}_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1),
              round(sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_sih[1]}[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)
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
            "ifelse(
              length(input$idade_dias_sih) == 2,
              round(sum(internacoes_{input$local_internacao_sih}_geral_internado_uti[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1),
              round(sum(internacoes_{input$local_internacao_sih}_{input$idade_dias_uti_sih[1]}_internado_uti[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1)
            )"
          ), 2)
        )
      }

      dplyr::full_join(
        df_calcs_aux1,
        dplyr::full_join(df_calcs_aux2, dplyr::full_join(df_calcs_aux3, df_calcs_aux4))
      )
    })


    ### Para a localidade selecionada -----------------------------------------
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
        cria_indicadores(df_calcs = bloco5_calcs(), input = input, filtros = filtros())
    })

    ### Para a comparação selecionada -----------------------------------------
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

        cria_indicadores(df_calcs = bloco5_calcs(), input = input, filtros = filtros(), comp = TRUE)
    })


    # bloco 6 -----------------------------------------------------------------

    # Criando um data.frame com os cálculos dos indicadores -------------------
    bloco6_calcs <- data.frame(
      tipo = c("local", "referencia"),
      soma_obitos_mat_totais = rep("sum(obitos_mat_totais)", 2),
      rmm = c("round(sum(obitos_mat_totais) / sum(nascidos) * 100000, 1)", "30"),
      prop_obitos_diretos = rep("round(sum(obitos_mat_diretos) / sum(obitos_mat_totais) * 100, 1)", 2),
      prop_obitos_aborto = rep("round(sum(obitos_mat_aborto) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_hipertens = rep("round(sum(obitos_mat_hipertensao) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_hemo = rep("round(sum(obitos_mat_hemorragia) / sum(obitos_mat_diretos) * 100, 1)", 2),
      prop_obitos_infec = rep("round(sum(obitos_mat_infec_puerperal) / sum(obitos_mat_diretos) * 100, 1)", 2),
      soma_casos_mmg = rep("sum(casos_mmg)", 2),
      prop_mmg_int_publicas = rep("round(sum(casos_mmg) / sum(total_internacoes) * 100, 1)", 2),
      prop_mmg_hipertensao = rep("round(sum(casos_mmg_hipertensao) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_hemorragia = rep("round(sum(casos_mmg_hemorragia) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_infeccao = rep("round(sum(casos_mmg_infeccoes) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_uti = rep("round(sum(casos_mmg_uti) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_tmp = rep("round(sum(casos_mmg_tmp) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_transfusao = rep("round(sum(casos_mmg_transfusao) / sum(casos_mmg) * 100, 1)", 2),
      prop_mmg_cirurgia = rep("round(sum(casos_mmg_cirurgia) / sum(casos_mmg) * 100, 1)", 2)
    )


    ### Para a localidade selecionada -----------------------------------------
    data6 <- reactive({
      bloco6 |>
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
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros())
    })


    # data6_correcao_rmm <- reactive({
    #   if(filtros()$nivel %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
    #     if(filtros()$nivel == "Estadual"){
    #       rmm_corrigida |>
    #         dplyr::filter(
    #           localidade == filtros()$estado,
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     } else if(filtros()$nivel == "Regional"){
    #       rmm_corrigida |>
    #         dplyr::filter(
    #           localidade == filtros()$regiao,
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     } else if(filtros()$nivel=="Nacional"){
    #       rmm_corrigida |>
    #         dplyr::filter(
    #           localidade == "Brasil",
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     }
    #   }
    #   else{
    #     data.frame(ano = filtros()$ano2[1]:filtros()$ano2[2])
    #   }
    # })
    #
    # data6_rmm_corrigida <- reactive({
    #   if(filtros()$nivel %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
    #     dplyr::full_join(data6(), data6_correcao_rmm(), by= "ano") |>
    #       dplyr::mutate(
    #         rmm_c=RMM
    #       )
    #   } else{
    #     dplyr::full_join(data6(), data6_correcao_rmm(), by= "ano") |>
    #       dplyr::mutate(
    #         rmm_c=rmm
    #       )
    #   }
    # })

    ### Para a comparação selecionada -----------------------------------------
    data6_comp <- reactive({
      bloco6 |>
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

        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros(), comp = TRUE)
    })

    # data6_comp_correcao_rmm <- reactive({
    #   if(filtros()$nivel2 %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
    #     if(filtros()$nivel2 == "Estadual"){
    #       rmm_corrigida |>
    #         dplyr::filter(
    #           localidade == filtros()$estado,
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     } else if(filtros()$nivel2 == "Regional"){
    #       rmm_corrigida |>
    #         dplyr::filter(
    #           localidade == filtros()$regiao,
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     } else if(filtros()$nivel2=="Nacional"){
    #       rmm_corrigida |>
    #         dplyr::filter(
    #           localidade == "Brasil",
    #           ano >= filtros()$ano2[1] & ano <= filtros()$ano2[2]
    #         )
    #     }
    #   }
    #   else{
    #     data.frame(ano = filtros()$ano2[1]:filtros()$ano2[2])
    #   }
    # })
    #
    # data6_comp_rmm_corrigida <- reactive({
    #   if(filtros()$nivel2 %in% c("Estadual", "Regional", "Nacional") & filtros()$ano2[2] < 2023){
    #     dplyr::full_join(data6_comp(), data6_comp_correcao_rmm(), by= "ano") |>
    #       dplyr::mutate(
    #         rmm_c = RMM
    #       )
    #   } else{
    #     dplyr::full_join(data6_comp(), data6_comp_correcao_rmm(), by= "ano") |>
    #       dplyr::mutate(
    #         rmm_c = rmm
    #       )
    #   }
    # })



    # bloco 7 -----------------------------------------------------------------

    bloco7_calcs <- reactive({
      data.frame(
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
        perinatal_oms_menos1500 = rep("sum(peso_menos_1500_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1500)", 2),
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
        sum(c(fetal_antes_peso_menos_1500, fetal_antes_peso_1500_1999, fetal_antes_peso_2000_2499, fetal_antes_peso_mais_2500, fetal_antes)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])/
          sum(c(perinatal_total_menos1500, perinatal_total_1500_1999, perinatal_total_2000_2499, perinatal_total_mais2500, obitos_perinatal_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])
        *100, 2)", 2),

        durante_dist_moment_obito_perinat = rep("round(
        sum(c(fetal_durante_peso_menos_1500, fetal_durante_peso_1500_1999, fetal_durante_peso_2000_2499, fetal_durante_peso_mais_2500, fetal_durante)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])/
          sum(c(perinatal_total_menos1500, perinatal_total_1500_1999, perinatal_total_2000_2499, perinatal_total_mais2500, obitos_perinatal_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])
        *100, 2)", 2),

        dia_0_dist_moment_obito_perinat = rep("round(
        sum(c(obitos_0dias_menos1500, obitos_0dias_1500_1999, obitos_0dias_2000_2499, obitos_0dias_mais2500, obitos_0dias)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])/
          sum(c(perinatal_total_menos1500, perinatal_total_1500_1999, perinatal_total_2000_2499, perinatal_total_mais2500, obitos_perinatal_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])
        *100, 2)", 2),

        dia_1_6_dist_moment_obito_perinat = rep("round(
        sum(c(obitos_1_6dias_menos1500, obitos_0dias_1500_1999, obitos_1_6dias_2000_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])/
          sum(c(perinatal_total_menos1500, perinatal_total_1500_1999, perinatal_total_2000_2499, perinatal_total_mais2500, obitos_perinatal_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat])
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
        sum(c(fetal_antes_peso_menos_1500, fetal_durante_peso_menos_1500, obitos_0dias_menos1500, obitos_1_6dias_menos1500, perinatal_total_menos1500)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])/
          sum(c(fetal_antes, fetal_durante, obitos_0dias, obitos_1_6dias, obitos_perinatal_total)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])
        *100, 2)", 2),

        de_1500_1999_dist_peso_perinat = rep("round(
        sum(c(fetal_antes_peso_1500_1999, fetal_durante_peso_1500_1999, obitos_0dias_1500_1999, obitos_1_6dias_1500_1999, perinatal_total_1500_1999)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])/
          sum(c(fetal_antes, fetal_durante, obitos_0dias, obitos_1_6dias, obitos_perinatal_total)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])
        *100, 2)", 2),


        de_2000_2499_dist_peso_perinat = rep("round(
        sum(c(fetal_antes_peso_2000_2499, fetal_durante_peso_2000_2499, obitos_0dias_2000_2499, obitos_1_6dias_2000_2499, perinatal_total_2000_2499)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])/
          sum(c(fetal_antes, fetal_durante, obitos_0dias, obitos_1_6dias, obitos_perinatal_total)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])
        *100, 2)", 2),

        mais_2500_dist_peso_perinat = rep("round(
        sum(c(fetal_antes_peso_mais_2500 , fetal_durante_peso_mais_2500, obitos_0dias_mais2500, obitos_1_6dias_mais2500, perinatal_total_mais2500)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])/
          sum(c(fetal_antes, fetal_durante, obitos_0dias, obitos_1_6dias, obitos_perinatal_total)[seleciona(aba = 'perinatal', indicador ='peso por momento do obito', input$momento_obito_dist_peso_perinat) %in% input$momento_obito_dist_peso_perinat])
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

        faltante_dist_peso_neonat = rep("round(100 -menos_1500_dist_peso_neonat -de_1500_1999_dist_peso_neonat -de_2000_2499_dist_peso_neonat -mais_2500_dist_peso_neonat, 2)", 2)
      )
    })

  ### Para a localidade selecionada -----------------------------------------

data7 <- reactive({
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
    )|>
    cria_indicadores(df_calcs = bloco7_calcs(), bloco  = "bloco7", input = input, filtros = filtros())
})

    ### Para a comparação selecionada -----------------------------------------

data7_comp <- reactive({
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
    cria_indicadores(df_calcs = bloco7_calcs(), bloco  = "bloco7", input = input, filtros = filtros(), comp = TRUE)
})


output$spider_chart <- highcharter::renderHighchart({
  req(input$selected_indicators)

  # Carregar dados
  df <- cbind(data1(), data2(), data3(), data4(), data4_deslocamento(), data4_macrorregiao(), data5(), data6(), data7())
  df2 <- cbind(data1_comp(), data2_comp(), data3_comp(), data4_comp(), data4_deslocamento_comp(), data4_macrorregiao_comp(),
               data5_comp(), data6_comp(), data7_comp())

  selected_indicators <- input$selected_indicators

  # Selecionar colunas relevantes
  df <- df[, c('class', selected_indicators)]
  df2 <- df2[, c('class', selected_indicators)]

  # Ajustar o nome da classe
  df$class <- ifelse(grepl("Brasil \\(valor de referência\\)", df$class), "Brasil", df$class)
  df2$class <- ifelse(grepl("Brasil \\(valor de referência\\)", df2$class), "Brasil", df2$class)

  # Categorias para o eixo x
  categories <- tabela_radar$nome_grafico[tabela_radar$nome_abreviado %in% selected_indicators]

  if (filtros()$comparar == "Não") {
    # Obter valores para o gráfico
    values <- round(as.numeric(df[1, selected_indicators]), 3)

    # Encontrar o valor máximo dos dados
    max_value <- max(values, na.rm = TRUE)

    # Definir o valor máximo do eixo y como o próximo múltiplo de 100 maior que o valor máximo
    yAxis_max <- ceiling(max_value / 100) * 100

    # Criar gráfico
    highcharter::highchart() |>
      highcharter::hc_chart(polar = TRUE, type = "line", backgroundColor = "transparent", marginBottom = 80) |>
      highcharter::hc_pane(size = '80%') |>
      highcharter::hc_xAxis(categories = categories, tickmarkPlacement = 'on', lineWidth = 0, labels = list(style = list(fontWeight = 'bold', fontSize = '13px'))) |>
      highcharter::hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = yAxis_max) |>
      highcharter::hc_add_series(name = df$class[1], data = values, color = "#2c115f", lineWidth = 2, marker = list(enabled = TRUE, symbol = "circle", radius = 4)) |>
      highcharter::hc_legend(align = 'right', verticalAlign = 'middle', layout = 'vertical', itemStyle = list(fontWeight = 'bold', fontSize = '25px')) |>
      highcharter::hc_legend(itemMarginTop = 10)  # Ajustar a margem entre itens da legenda
  } else {
    # Obter valores para o gráfico
    values1 <- round(as.numeric(df[1, selected_indicators]), 3)
    values2 <- round(as.numeric(df2[1, selected_indicators]), 3)

    # Encontrar o valor máximo dos dados
    max_value1 <- max(values1, na.rm = TRUE)
    max_value2 <- max(values2, na.rm = TRUE)

    # Definir o valor máximo do eixo y como o próximo múltiplo de 100 maior que o maior valor encontrado
    yAxis_max <- ceiling(max(max_value1, max_value2) / 100) * 100

    # Criar gráfico
    highcharter::highchart() |>
      highcharter::hc_chart(polar = TRUE, type = "line", backgroundColor = "transparent", marginBottom = 0) |>
      highcharter::hc_pane(size = '80%') |>
      highcharter::hc_xAxis(categories = categories, tickmarkPlacement = 'on', lineWidth = 0, labels = list(style = list(fontWeight = 'bold', fontSize = '13px'))) |>
      highcharter::hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = yAxis_max) |>
      highcharter::hc_add_series(name = df$class[1], data = values1, color = "#2c115f", lineWidth = 2, marker = list(enabled = TRUE, symbol = "circle", radius = 4)) |>
      highcharter::hc_add_series(name = df2$class[1], data = values2, color = "#b73779", lineWidth = 2, marker = list(enabled = TRUE, symbol = "diamond", radius = 4)) |>
      highcharter::hc_legend(align = 'right', verticalAlign = 'middle', layout = 'vertical', itemStyle = list(fontWeight = 'bold', fontSize = '25px')) |>
      highcharter::hc_legend(itemMarginTop = 10)  # Ajustar a margem entre itens da legenda
  }


})



# output$spider_chart <- highcharter::renderHighchart({
#   req(input$selected_indicators)
#
#   df <- cbind(data1(), data2(), data3(), data4(), data4_deslocamento(), data4_macrorregiao(), data5(), data6(), data7())
#   df2 <- cbind(data1_comp(), data2_comp(), data3_comp(), data4_comp(), data4_deslocamento_comp(), data4_macrorregiao_comp(),
#                data5_comp(), data6_comp(), data7_comp())
#
#   selected_indicators <- input$selected_indicators
#
#   df <- df[, c('class', selected_indicators)]
#   df2 <- df2[, c('class', selected_indicators)]
#
#   df$class <- ifelse(grepl("Brasil \\(valor de referência\\)", df$class), "Brasil", df$class)
#   df2$class <- ifelse(grepl("Brasil \\(valor de referência\\)", df2$class), "Brasil", df2$class)
#
#   print(df)
#   print(df2)
#
#   categories <- tabela_radar$nome_grafico[tabela_radar$nome_abreviado %in% selected_indicators]
#
#   if (filtros()$comparar == "Não") {
#     values <- round(as.numeric(df[1, selected_indicators]), 3)
#     highcharter::highchart() |>
#       highcharter::hc_chart(polar = TRUE, type = "line", backgroundColor = "transparent", marginBottom = 80) |>
#       highcharter::hc_pane(size = '80%') |>
#       highcharter::hc_xAxis(categories = categories, tickmarkPlacement = 'on', lineWidth = 0, labels = list(style = list(fontWeight = 'bold', fontSize = '10px'))) |>
#       highcharter::hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = 100) |>
#       highcharter::hc_add_series(name = df$class[1], data = values, color = "#1f77b4", lineWidth = 2, marker = list(enabled = TRUE, symbol = "circle", radius = 4)) |>
#       highcharter::hc_legend(align = 'right', verticalAlign = 'middle', layout = 'vertical', itemStyle = list(fontWeight = 'bold', fontSize = '17px'))
#   } else {
#     values1 <- round(as.numeric(df[1, selected_indicators]), 3)
#     values2 <- round(as.numeric(df2[1, selected_indicators]), 3)
#     highcharter::highchart() |>
#       highcharter::hc_chart(polar = TRUE, type = "line", backgroundColor = "transparent", marginBottom = 0) |>
#       highcharter::hc_pane(size = '80%') |>
#       highcharter::hc_xAxis(categories = categories, tickmarkPlacement = 'on', lineWidth = 0, labels = list(style = list(fontWeight = 'bold', fontSize = '10px'))) |>
#       highcharter::hc_yAxis(gridLineInterpolation = 'polygon', lineWidth = 0, min = 0, max = 100) |>
#       highcharter::hc_add_series(name = df$class[1], data = values1, color = "#1f77b4", lineWidth = 2, marker = list(enabled = TRUE, symbol = "circle", radius = 4)) |>
#       highcharter::hc_add_series(name = df2$class[1], data = values2, color = "#ff7f0e", lineWidth = 2, marker = list(enabled = TRUE, symbol = "diamond", radius = 4)) |>
#       highcharter::hc_legend(align = 'right', verticalAlign = 'middle', layout = 'vertical', itemStyle = list(fontWeight = 'bold', fontSize = '17px'))
#   }
#
#
# })












# fim ---------------------------------------------------------------------









  })
}





## To be copied in the UI
# mod_bloco_9_ui("bloco_9_1")

## To be copied in the server
# mod_bloco_9_server("bloco_9_1")
