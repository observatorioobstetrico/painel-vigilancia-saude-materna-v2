#
#
# # Criando um data.frame com os cálculos dos indicadores -------------------
# bloco9_calcs <- data.frame(
#   tipo = c("local", "referencia"),
#   mort_neonat = rep("round(sum(obitos_27dias)/sum(total_de_nascidos_vivos) *1000, 2)", 2),
#   porc_condicoes_ameacadoras = rep("round(sum(nascidos_condicoes_ameacadoras) / sum(total_de_nascidos_vivos) * 100, 1)", 2),
#   prop_1500_sem_uti= rep("round((sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_sem_uti)) / (sum(partos_na_macro_com_uti) + sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_com_uti) + sum(partos_fora_macro_sem_uti)) * 100, 1)", 2),
#   porc_consultas_adequadas = rep("round(sum(mulheres_com_consultas_prenatal_adequadas[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 2)", 2),
#   porc_mais_3pt = rep("round(sum(mulheres_com_mais_de_tres_partos_anteriores) / sum(total_de_nascidos_vivos) * 100, 1)", 2)
#
# )
#
# data_resumo <-
#   bloco2 |>
#   dplyr::left_join(bloco3) |>
#   dplyr::left_join(bloco4_deslocamento_macrorregiao) |>
#   dplyr::left_join(bloco5) |>
#   dplyr::left_join(bloco7) |>
#   dplyr::select(ano, codmunres, municipio, grupo_kmeans, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude,
#                 mulheres_com_mais_de_tres_partos_anteriores,
#                 mulheres_com_consultas_prenatal_adequadas,
#                 partos_na_macro_sem_uti, partos_fora_macro_sem_uti, partos_na_macro_com_uti, partos_fora_macro_com_uti,
#                 nascidos_condicoes_ameacadoras,
#                 obitos_27dias,
#                 total_de_nascidos_vivos,
#                 nascidos)
#
# data_resumo2 <- data_resumo |>
#   dplyr::group_by(ano) |>
#   dplyr::summarise(mort_neonat = round(sum(obitos_27dias)/sum(total_de_nascidos_vivos) *1000, 2),
#                    porc_condicoes_ameacadoras = round(sum(nascidos_condicoes_ameacadoras) / sum(total_de_nascidos_vivos) * 100, 1),
#                    porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores) / sum(total_de_nascidos_vivos) * 100, 1),
#                    porc_consultas_adequadas = 100 -round(sum(mulheres_com_consultas_prenatal_adequadas[ano >= 2014]) / sum(total_de_nascidos_vivos[ano >= 2014]) * 100, 2),
#                    prop_1500_sem_uti= round((sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_sem_uti)) / (sum(partos_na_macro_com_uti) + sum(partos_na_macro_sem_uti) + sum(partos_fora_macro_com_uti) + sum(partos_fora_macro_sem_uti)) * 100, 1)
#                    )

