infos_indicador <- tabela_indicadores |>
  dplyr::filter(indicador == "Proporção de mulheres com 4 a 7 anos de estudo")

data <- bloco1 |>
  dplyr::filter(
    ano >= 2012 & ano <= 2020
  ) |>
  dplyr::group_by(regiao) |>
  dplyr::summarise(
    numerador := sum(.data[[infos_indicador$numerador]]),
    denominador := sum(.data[[infos_indicador$denominador]]),
    proporcao = round(sum(.data[[infos_indicador$numerador]])/sum(.data[[infos_indicador$denominador]]) * {infos_indicador$fator}, 2)
  ) |>
  dplyr::ungroup()


data_grafico1 <- bloco1 |>
  dplyr::filter(
    ano >= 2012 & ano <= 2020
  ) |>
  dplyr::group_by(regiao) |>
  dplyr::summarise(
    "{infos_indicador$numerador}" := sum(.data[[infos_indicador$numerador]]),
    "{infos_indicador$denominador}" := sum(.data[[infos_indicador$denominador]]),
    proporcao = round(sum(.data[[infos_indicador$numerador]])/sum(.data[[infos_indicador$denominador]]) * {infos_indicador$fator}, 2)
  ) |>
  dplyr::ungroup()

highcharter::hchart(
  data_grafico1,
  type = "column",
  highcharter::hcaes(
    x = regiao,
    y = proporcao
  ) |>
    highcharter::hc_colors(c("#4285F4", "#DB4437", "#F4B400", "#0F9D58", "#990099"))

)


output$plot1 <- highcharter::renderHighchart({

  total <- c(
    data1_graph()$total_nvm_menor_que_20_anos,
    data1_graph()$total_nvm_entre_20_e_34_anos,
    data1_graph()$total_nvm_maior_que_34_anos
  )

  categorias <- c("< 20 anos", "20 a 34 anos", ">= 35 anos")

  if (length(total) == length(categorias)) {
    df2 <- data.frame(total, categorias)
    df2$categorias <- factor(df2$categorias,
                             levels = c("< 20 anos", "20 a 34 anos", ">= 35 anos"))
    highcharter::hchart(
      df2,
      type = "column",
      highcharter::hcaes(
        x = categorias,
        y = total,
        color = c("#4285F4", "#DB4437", "#F4B400")
      )
    ) |>
      highcharter::hc_title(text = HTML("<b style='font-size:16px'> Nascidos vivos por faixa etária da mãe </b>")) |>
      highcharter::hc_xAxis(title = list(text = "")) |>
      highcharter::hc_yAxis(title = list(text = "Nascidos vivos")) |>
      highcharter::hc_add_theme(highcharter::hc_theme_elementary())
  }

})

infos_indicador$fator <- as.character(infos_indicador$fator)

















# data4 <- bloco4 |>
#     dplyr::filter(ano >= 2012 & ano <= 2020) |>
#     dplyr::filter(
#         uf == "SP"
#     ) |>
#     dplyr::select(1, 10:25) |>
#     dplyr::group_by(ano) |>
#     dplyr::summarise(
#       total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
#       mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
#       prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 2),
#       prop_contribuicao_robson1_cesariana = round((sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson2_cesariana = round((sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson3_cesariana = round((sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson4_cesariana = round((sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson5_cesariana = round((sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson6_a_9_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson10_cesariana = round((sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana) * 100, 2)
#     ) |>
#     dplyr::ungroup() |>
#     tibble::rownames_to_column() |>
#     tidyr::pivot_longer(!rowname, names_to = "col1", values_to = "col2") |>
#     tidyr::pivot_wider(names_from = "rowname", values_from = "col2") |>
#     dplyr::slice(-c(1:3)) |>
#     setNames(c("col1", as.character(2012:2020))) |>
#     dplyr::mutate(
#       localidade = "SP",
#       .after = col1
#     )
#
# data4_comp <- bloco4 |>
#     dplyr::filter(ano >= 2012 & ano <= 2020) |>
#     dplyr::filter(
#       municipio == "São Paulo"
#     ) |>
#     dplyr::group_by(ano) |>
#     dplyr::select(1, 10:25) |>
#     dplyr::group_by(ano) |>
#     dplyr::summarise(
#       total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
#       mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
#       prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 2),
#       prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 2),
#       prop_contribuicao_robson1_cesariana = round((sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson2_cesariana = round((sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson3_cesariana = round((sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson4_cesariana = round((sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson5_cesariana = round((sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson6_a_9_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana) * 100, 2),
#       prop_contribuicao_robson10_cesariana = round((sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana) * 100, 2)
#     ) |>
#     dplyr::ungroup() |>
#     tibble::rownames_to_column() |>
#     tidyr::pivot_longer(!rowname, names_to = "col1", values_to = "col2") |>
#     tidyr::pivot_wider(names_from = "rowname", values_from = "col2") |>
#     dplyr::slice(-c(1:3)) |>
#     setNames(c("col1", as.character(2012:2020))) |>
#     dplyr::mutate(
#       localidade = "São Paulo",
#       .after = col1
#     )
#
# data_juncao <- rbind(data4, data4_comp)
#
#
# data4[0,] |>
#   dplyr::slice(1:7) |>
#   dplyr::mutate(
#     grupo_robson = c(as.character(1:5), "6 a 9", "10"),
#     .before = localidade
#   ) |>
#   dplyr::select(-c(col1, localidade)) |>
#   reactable::reactable(
#     defaultColDef = reactable::colDef(
#       minWidth = 60,
#       footerStyle = list(fontWeight = "bold")
#     ),
#     columns = list(
#       grupo_robson = reactable::colDef(
#         name = "Grupo de Robson",
#         minWidth = 90
#       )
#     ),
#     sortable = TRUE,
#     resizable = TRUE,
#     highlight = TRUE,
#     striped = TRUE,
#     bordered = FALSE,
#     pagination = FALSE,
#     height = 350
#   )
#
# teste <- data4[0,]

# px_localidade <- max(strwidth(unique(data_juncao()$localidade), font = 12, units = 'in')) * 75
# data_juncao() |>
#   dplyr::slice(1:7, 15:21) |>
#   dplyr::mutate(
#     grupo_robson = rep(c(as.character(1:5), "6 a 9", "10"), times = 2),
#     ordem = rep(1:7, times = 2),
#     .after = col1
#   ) |>
#   data.table::setorder(ordem) |>
#   dplyr::select(-c(col1, ordem)) |>
#   reactable::reactable(
#     defaultColDef = reactable::colDef(
#       minWidth = 60,
#       footerStyle = list(fontWeight = "bold")
#     ),
#     columns = list(
#       grupo_robson = reactable::colDef(
#         name = "Grupo de Robson",
#         minWidth = 90
#       ),
#       localidade = reactable::colDef(
#         name = "Localidade",
#         minWidth = px_localidade
#       )
#     ),
#     sortable = TRUE,
#     resizable = TRUE,
#     highlight = TRUE,
#     striped = TRUE,
#     bordered = FALSE,
#     pagination = FALSE,
#     height = 350
#   )
#
#
