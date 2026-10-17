# Microbenchmark: cria_indicadores (atual vs vetorizada)
suppressMessages({
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
})
ns <- asNamespace("painel.indicadores")
for (f in c("filtra_localidade", "cria_indicadores", "cache_interno_guardar", "cache_interno_buscar", "chave_cache_interno")) {
  assign(f, get(f, envir = ns), envir = globalenv())
}
bloco4 <- get("bloco4", envir = ns)
cache_env <- get(".cache_interno", envir = ns)
source("dev/perf/bench_helpers.R")

filtros <- filtros_padrao()
bloco4_calcs <- extrai_calcs("R/mod_bloco_4.R", "bloco4_calcs")

# versão vetorizada: uma única passada de summarise
cria_indicadores_vec <- function(df_localidade, df_calcs, filtros,
                                 referencia = FALSE, comp = FALSE,
                                 adicionar_localidade = TRUE,
                                 localidade_resumo = "escolha1") {
  if (referencia) {
    df_calcs <- dplyr::filter(df_calcs, tipo == "referencia")
  } else {
    df_calcs <- dplyr::filter(df_calcs, tipo == "local")
  }
  df_calcs <- dplyr::select(df_calcs, !tipo)

  colunas <- names(df_calcs)
  exprs <- lapply(df_calcs, rlang::parse_expr)
  names(exprs) <- colunas

  agrupado <- dplyr::group_vars(df_localidade)
  if (length(agrupado) == 0) {
    df_aux <- dplyr::summarise(df_localidade, !!!exprs)
  } else {
    df_aux <- dplyr::summarise(df_localidade, !!!exprs, .groups = "keep")
  }

  if (adicionar_localidade) {
    sufixo <- ifelse(comp == TRUE | localidade_resumo == "escolha2", "2", "")
    df_aux |>
      dplyr::mutate(
        class = dplyr::case_when(
          filtros[[paste0("nivel", sufixo)]] == "nacional" | referencia ~ dplyr::if_else(
            filtros$comparar == "Não",
            "Brasil (valor de referência)",
            dplyr::if_else(
              filtros$mostrar_referencia == "nao_mostrar_referencia",
              "Brasil",
              "Brasil (valor de referência)"
            )
          ),
          filtros[[paste0("nivel", sufixo)]] == "regional" ~ filtros[[paste0("regiao", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "estadual" ~ filtros[[paste0("estado", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "macro" ~ filtros[[paste0("macro", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "micro" ~ filtros[[paste0("micro", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "municipal" ~ filtros[[paste0("municipio", sufixo)]],
          filtros[[paste0("nivel", sufixo)]] == "municipios_semelhantes" ~ "Média dos municípios semelhantes"
        )
      ) |>
      dplyr::ungroup()
  } else {
    dplyr::ungroup(df_aux)
  }
}

# dados como os módulos usam
df_loc <- filtra_localidade(bloco4, filtros)

# limpa cache para medir a frio e o quente
rm(list = ls(envir = cache_env), envir = cache_env)

cat("== filtrados (estadual SP, 2012-2024):", nrow(df_loc), "linhas x", ncol(df_loc), "colunas ==\n\n")

cat("-- cria_indicadores ATUAL --\n")
r_old <- tempo("cria_indicadores atual (estadual SP)", cria_indicadores(df_loc, bloco4_calcs, filtros = filtros), rep = 5)

cat("\n-- cria_indicadores VETORIZADA --\n")
r_new <- tempo("cria_indicadores vetorizada (estadual SP)", cria_indicadores_vec(df_loc, bloco4_calcs, filtros = filtros), rep = 5)

cat("\n-- equivalência --\n")
r_old_df <- as.data.frame(r_old); r_new_df <- as.data.frame(r_new)
r_new_df <- r_new_df[, names(r_old_df), drop = FALSE]
compara <- mapply(function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE)), r_old_df, r_new_df)
cat("colunas idênticas:", sum(compara), "de", length(compara), "\n")
if (!all(compara)) print(names(compara)[!compara])

cat("\n-- com grupos (por ano) --\n")
df_loc_ano <- dplyr::group_by(df_loc, ano)
r_old_g <- tempo("atual (group_by ano)", cria_indicadores(df_loc_ano, bloco4_calcs, filtros = filtros, adicionar_localidade = FALSE), rep = 3)
r_new_g <- tempo("vetorizada (group_by ano)", cria_indicadores_vec(df_loc_ano, bloco4_calcs, filtros = filtros, adicionar_localidade = FALSE), rep = 3)
r_old_g_df <- as.data.frame(r_old_g); r_new_g_df <- as.data.frame(r_new_g)
r_new_g_df <- r_new_g_df[, names(r_old_g_df), drop = FALSE]
compara_g <- mapply(function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE)), r_old_g_df, r_new_g_df)
cat("colunas idênticas (grupo):", sum(compara_g), "de", length(compara_g), "\n")
if (!all(compara_g)) print(names(compara_g)[!compara_g])
