suppressMessages({
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
})
ns <- asNamespace("painel.indicadores")
for (f in c(
  "filtra_localidade", "cria_indicadores", "processa_bloco7", "processa_causas",
  "cache_interno_guardar", "cache_interno_buscar", "chave_cache_interno",
  "identidade_base", "rotulo_filtros"
)) assign(f, get(f, envir = ns), envir = globalenv())
for (b in c("bloco7_fetal", "bloco7_evitaveis_fetal", "bloco7_principais_fetal",
            "bloco4", "base_incompletude", "bloco7")) assign(b, get(b, envir = ns), envir = globalenv())
cache_env <- get(".cache_interno", envir = ns)
source("dev/perf/bench_helpers.R")

filtros <- filtros_padrao(nivel = "estadual", estado = "São Paulo", bloco = "bloco7",
                          tipo_do_indicador_blocos4_6_7 = "fetal",
                          indicador_blocos4_6_7 = "Número de óbitos fetais")

input <- list(
  input_num_obitos_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais", "sem_info"),
  input_num_obitos_momento = "todos",
  input_taxa_de_mortalidade_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais", "sem_info"),
  input_taxa_de_mortalidade_momento = "todos",
  input_distribuicao_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais"),
  input_distribuicao_momento = c("antes", "durante"),
  input_evitaveis_peso = c("menos_1000", "1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
  input_evitaveis_momento = c("antes", "durante", "sem_info_parto"),
  input_evitaveis_grupos = c("imunoprevencao", "mulher_gestacao", "parto", "nao_aplica", "mal_definidas", "outros"),
  input_principais_peso = c("menos_1000", "1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao"),
  input_principais_momento = c("antes", "sem_info_parto", "durante"),
  input_principais_grupos = c("prematuridade", "infeccoes", "asfixia", "ma_formacao", "gravidez", "afeccoes", "mal_definidas", "outros")
)

cols_num_obitos_taxa <- extrai_calcs("R/mod_bloco_7_fetal.R", "cols_num_obitos_taxa")
ind_num_obitos_taxa <- extrai_calcs("R/mod_bloco_7_fetal.R", "indicadores_num_obitos_taxa")
cols_distribuicao_peso <- extrai_calcs("R/mod_bloco_7_fetal.R", "cols_distribuicao_peso")
ind_distribuicao_peso <- extrai_calcs("R/mod_bloco_7_fetal.R", "indicadores_distribuicao_peso")
cols_distribuicao_momento <- extrai_calcs("R/mod_bloco_7_fetal.R", "cols_distribuicao_momento")
ind_distribuicao_momento <- extrai_calcs("R/mod_bloco_7_fetal.R", "indicadores_distribuicao_momento")

cat("== Bloco 7 fetal, estadual SP (como no app; 12 chamadas de processa_bloco7 por busca) ==\n")
df_loc <- filtra_localidade(bloco7_fetal, filtros)
cat("base filtrada:", nrow(df_loc), "linhas\n\n")

# cache frio/quente de filtra_localidade
rm(list = ls(envir = cache_env), envir = cache_env)
tempo("filtra_localidade COLD (1a chamada)", filtra_localidade(bloco7_fetal, filtros), rep = 1)
tempo("filtra_localidade HOT (cache)", filtra_localidade(bloco7_fetal, filtros), rep = 5)
cat("\n")

tempo("processa_bloco7 num_obitos",  processa_bloco7(df_loc, input, filtros, cols_num_obitos_taxa, ind_num_obitos_taxa), rep = 5)
tempo("processa_bloco7 distribuicao_peso",  processa_bloco7(df_loc, input, filtros, cols_distribuicao_peso, ind_distribuicao_peso), rep = 5)
tempo("processa_bloco7 distribuicao_momento",  processa_bloco7(df_loc, input, filtros, cols_distribuicao_momento, ind_distribuicao_momento), rep = 5)

cat("\n-- escopos comp/referencia (base completa Brasil) --\n")
df_ref <- filtra_localidade(bloco7_fetal, filtros, referencia = TRUE)
cat("referencia:", nrow(df_ref), "linhas\n")
tempo("processa_bloco7 num_obitos (referencia)", processa_bloco7(df_ref, input, filtros, cols_num_obitos_taxa, ind_num_obitos_taxa, referencia = TRUE), rep = 5)

cat("\n== processa_causas (evitaveis/principais) ====\n")
rm(list = ls(envir = cache_env), envir = cache_env)
tempo("processa_causas evitaveis COLD (SP)", processa_causas(bloco7_evitaveis_fetal, filtros,
  pesos = input$input_evitaveis_peso, momentos = input$input_evitaveis_momento,
  grupos = input$input_evitaveis_grupos, prefixo_coluna = "evitaveis_fetal"), rep = 3)
tempo("processa_causas evitaveis HOT (SP)", processa_causas(bloco7_evitaveis_fetal, filtros,
  pesos = input$input_evitaveis_peso, momentos = input$input_evitaveis_momento,
  grupos = input$input_evitaveis_grupos, prefixo_coluna = "evitaveis_fetal"), rep = 5)
tempo("processa_causas principais COLD (SP)", processa_causas(bloco7_principais_fetal, filtros,
  pesos = input$input_principais_peso, momentos = input$input_principais_momento,
  grupos = input$input_principais_grupos, prefixo_coluna = "principais_fetal"), rep = 3)
tempo("processa_causas evitaveis COLD (referencia/Brasil)", processa_causas(bloco7_evitaveis_fetal, filtros,
  referencia = TRUE,
  pesos = input$input_evitaveis_peso, momentos = input$input_evitaveis_momento,
  grupos = input$input_evitaveis_grupos, prefixo_coluna = "evitaveis_fetal"), rep = 3)
tempo("processa_causas principais COLD (referencia/Brasil)", processa_causas(bloco7_principais_fetal, filtros,
  referencia = TRUE,
  pesos = input$input_principais_peso, momentos = input$input_principais_momento,
  grupos = input$input_principais_grupos, prefixo_coluna = "principais_fetal"), rep = 3)

cat("\n== ops de base_incompletude (como data_incompletude_aux dos modulos bloco 7) ==\n")
tempo("filter+group_by+summarise incompletude (SP)",
  base_incompletude |>
    dplyr::filter(ano >= 2012 & ano <= 2024) |>
    dplyr::filter(uf == "São Paulo") |>
    dplyr::group_by(ano) |>
    dplyr::summarise(peso = round(sum(peso_incompletos, na.rm = TRUE) / sum(peso_totais, na.rm = TRUE) * 100, 1)) |>
    dplyr::ungroup(),
  rep = 5)
