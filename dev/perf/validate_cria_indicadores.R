# Valida a nova cria_indicadores contra a implementacao antiga, em todos os
# conjuntos *_calcs dos modulos e multiplos cenarios de filtro/escopo.
suppressMessages(pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE))
ns <- asNamespace("painel.indicadores")
source("dev/perf/bench_helpers.R")

extrai_calcs <- function(arquivo, nome) {
  expr <- parse(arquivo)
  achado <- NULL
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(nome))) { achado <<- e[[3]]; return(invisible(TRUE)) }
      for (i in seq_along(e)) { if (!is.null(achado)) return(invisible(TRUE)); walk(e[[i]]) }
    }
    invisible(FALSE)
  }
  for (e in expr) { walk(e); if (!is.null(achado)) break }
  if (is.null(achado)) stop("nao achei ", nome)
  eval(achado)
}

extrai_expr <- function(arquivo, nome) {
  expr <- parse(arquivo)
  achado <- NULL
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(nome))) { achado <<- e[[3]]; return(invisible(TRUE)) }
      for (i in seq_along(e)) { if (!is.null(achado)) return(invisible(TRUE)); walk(e[[i]]) }
    }
    invisible(FALSE)
  }
  for (e in expr) { walk(e); if (!is.null(achado)) break }
  if (is.null(achado)) stop("nao achei ", nome)
  achado
}

# bloco5_calcs e montado a partir de um data.frame + 2 eventReactive
constroi_bloco5_calcs <- function() {
  aux1 <- extrai_calcs("R/mod_bloco_5.R", "df_calcs_aux1")
  expr2 <- extrai_expr("R/mod_bloco_5.R", "df_calcs_aux2")
  expr3 <- extrai_expr("R/mod_bloco_5.R", "df_calcs_aux3")
  env <- new.env(parent = globalenv())
  env$input <- list(
    baixo_peso = c("nascidos_vivos_peso_menor_1000", "nascidos_vivos_peso_1000_a_1499", "nascidos_vivos_peso_1500_a_2499"),
    faixa_prematuridade = c("nascidos_vivos_menos_de_28_semanas", "nascidos_vivos_28_a_32_semanas", "nascidos_vivos_33_a_34_semanas", "nascidos_vivos_35_a_36_semanas")
  )
  aux2 <- eval(expr2[[3]], envir = env)
  aux3 <- eval(expr3[[3]], envir = env)
  dplyr::full_join(aux1, dplyr::full_join(aux2, aux3))
}

# bloco6_calcs_resumo deriva de bloco6_calcs
constroi_bloco6_calcs_resumo <- function() {
  env <- new.env(parent = globalenv())
  env$bloco6_calcs <- extrai_calcs("R/mod_bloco_6.R", "bloco6_calcs")
  eval(extrai_expr("R/mod_bloco_6.R", "bloco6_calcs_resumo"), envir = env)
}

# --- copia da implementacao ANTIGA ----------------------------------------
cria_indicadores_old <- function(df_localidade, df_calcs, df_calcs_dist_bloco7 = NULL, filtros, referencia = FALSE, comp = FALSE, adicionar_localidade = TRUE, input = NULL, bloco = "outros", localidade_resumo = "escolha1") {
  if (referencia == FALSE) {
    df_calcs <- df_calcs |> dplyr::filter(tipo == "local") |> dplyr::select(!tipo)
    if (grepl("bloco7", bloco) & !grepl("morbidade", bloco)) {
      df_calcs_dist_bloco7 <- df_calcs_dist_bloco7 |> dplyr::filter(tipo == "local") |> dplyr::select(!tipo)
    }
  } else {
    df_calcs <- df_calcs |> dplyr::filter(tipo == "referencia") |> dplyr::select(!tipo)
    if (grepl("bloco7", bloco) & !grepl("morbidade", bloco)) {
      df_calcs_dist_bloco7 <- df_calcs_dist_bloco7 |> dplyr::filter(tipo == "referencia") |> dplyr::select(!tipo)
    }
  }
  colunas_summarise <- names(df_calcs)
  df_localidade_aux <- df_localidade |> dplyr::summarise() |> dplyr::ungroup()
  if (ncol(df_localidade_aux) == 0) {
    for (coluna in colunas_summarise) {
      df_localidade_aux <- cbind(
        df_localidade_aux,
        dplyr::summarise(df_localidade, !!coluna := !!rlang::parse_expr(df_calcs[[coluna]]))
      )
    }
  } else {
    for (coluna in colunas_summarise) {
      df_localidade_aux <- dplyr::full_join(
        df_localidade_aux,
        dplyr::summarise(df_localidade, !!coluna := !!rlang::parse_expr(df_calcs[[coluna]])),
        by = dplyr::join_by(ano)
      )
    }
  }
  if (grepl("bloco7", bloco) & !grepl("morbidade", bloco)) {
    colunas_summarise_dist_bloco7 <- names(df_calcs_dist_bloco7)
    if (nrow(df_localidade_aux) == 1) {
      for (coluna in colunas_summarise_dist_bloco7) {
        df_localidade_aux <- cbind(
          df_localidade_aux,
          dplyr::summarise(df_localidade_aux, !!coluna := !!rlang::parse_expr(df_calcs_dist_bloco7[[coluna]]))
        )
      }
    } else {
      for (coluna in colunas_summarise_dist_bloco7) {
        df_localidade_aux <- dplyr::full_join(
          df_localidade_aux,
          dplyr::summarise(df_localidade_aux |> dplyr::group_by(ano), !!coluna := !!rlang::parse_expr(df_calcs_dist_bloco7[[coluna]])),
          by = dplyr::join_by(ano)
        )
      }
    }
  }
  if (adicionar_localidade == TRUE) {
    sufixo <- ifelse(comp == TRUE | localidade_resumo == "escolha2", "2", "")
    df_localidade_aux |>
      dplyr::mutate(
        class = dplyr::case_when(
          filtros[[paste0("nivel", sufixo)]] == "nacional" | referencia == TRUE ~ dplyr::if_else(
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
    df_localidade_aux |> dplyr::ungroup()
  }
}

cria_indicadores_new <- get("cria_indicadores", envir = ns)
filtra_localidade <- get("filtra_localidade", envir = ns)
cache_env <- get(".cache_interno", envir = ns)

bases <- list(
  bloco1 = get("bloco1", envir = ns),
  bloco2 = get("bloco2", envir = ns),
  bloco3 = get("bloco3", envir = ns),
  bloco4 = get("bloco4", envir = ns),
  bloco4_deslocamento_muni = get("bloco4_deslocamento_muni", envir = ns),
  bloco4_profissional = get("bloco4_profissional", envir = ns),
  bloco5 = get("bloco5", envir = ns),
  bloco6 = get("bloco6", envir = ns)
)

calcs <- list(
  bloco1_calcs = list(file = "R/mod_bloco_1.R", nome = "bloco1_calcs", bases = c("bloco1")),
  bloco2_calcs = list(file = "R/mod_bloco_2.R", nome = "bloco2_calcs", bases = c("bloco2")),
  bloco3_calcs = list(file = "R/mod_bloco_3.R", nome = "bloco3_calcs", bases = c("bloco3")),
  bloco4_calcs = list(file = "R/mod_bloco_4.R", nome = "bloco4_calcs", bases = c("bloco4")),
  bloco4_deslocamento_calcs = list(file = "R/mod_bloco_4.R", nome = "bloco4_deslocamento_calcs", bases = c("bloco4_deslocamento_muni", "bloco4_deslocamento_uf")),
  bloco4_profissional_calcs = list(file = "R/mod_bloco_4.R", nome = "bloco4_profissional_calcs", bases = c("bloco4", "bloco4_profissional")),
  bloco4_profissional_calcs2 = list(file = "R/mod_bloco_4.R", nome = "bloco4_profissional_calcs2", bases = c("bloco4_profissional")),
  bloco4_profissional_calcs4 = list(file = "R/mod_bloco_4.R", nome = "bloco4_profissional_calcs4", bases = c("bloco4_profissional")),
  bloco5_calcs = list(file = NULL, nome = "bloco5_calcs", bases = c("bloco5"), construtor = constroi_bloco5_calcs),
  bloco6_calcs = list(file = "R/mod_bloco_6.R", nome = "bloco6_calcs", bases = c("bloco6")),
  bloco6_calcs_resumo = list(file = NULL, nome = "bloco6_calcs_resumo", bases = c("bloco6"), construtor = constroi_bloco6_calcs_resumo)
)

# bases extra sob demanda
for (b in c("bloco4_deslocamento_uf", "bloco7_fetal", "bloco7_evitaveis_fetal")) {
  bases[[b]] <- get(b, envir = ns)
}

filtros_estadual <- filtros_padrao(nivel = "estadual", estado = "São Paulo")
filtros_nacional <- filtros_padrao(nivel = "nacional", estado = "")
filtros_municipal <- filtros_padrao(nivel = "municipal", estado = "Rio de Janeiro", municipio = "Angra dos Reis", estado_municipio = "Rio de Janeiro")
filtros_comp <- filtros_padrao(nivel = "estadual", estado = "São Paulo", comparar = "Sim",
                               nivel2 = "estadual", estado2 = "Minas Gerais")

resultados <- list()
falhas <- 0
checks <- 0

compara <- function(rotulo, old, new) {
  checks <<- checks + 1
  ok <- tryCatch(
    {
      o <- old()
      n <- new()
      isTRUE(all.equal(as.data.frame(o), as.data.frame(n), check.attributes = TRUE)) &&
        identical(class(o), class(n)) &&
        identical(dplyr::group_vars(o), dplyr::group_vars(n)) &&
        identical(names(o), names(n))
    },
    error = function(e) {
      # se a antiga falha, a nova tambem deve falhar do mesmo jeito
      eo <- tryCatch({ old(); NA_character_ }, error = function(e1) conditionMessage(e1))
      en <- tryCatch({ new(); NA_character_ }, error = function(e1) conditionMessage(e1))
      !is.na(eo) && !is.na(en)
    }
  )
  if (!isTRUE(ok)) {
    falhas <<- falhas + 1
    cat("  FALHOU:", rotulo, "\n")
  }
  invisivel <- ok
}

cat("== crias_indicadores: antiga vs nova ==\n")
for (nm in names(calcs)) {
  cf <- calcs[[nm]]
  if (!is.null(cf$construtor)) {
    df_calcs <- tryCatch(cf$construtor(), error = function(e) { cat("  [", nm, "] construtor falhou:", conditionMessage(e), "\n"); NULL })
    if (is.null(df_calcs)) next
  } else {
    df_calcs <- extrai_calcs(cf$file, cf$nome)
  }

  for (b in cf$bases) {
    if (is.null(bases[[b]])) next
    dfb <- bases[[b]]

    for (fsc in c("estadual", "nacional", "municipal")) {
      f <- switch(fsc,
        estadual = filtros_estadual,
        nacional = filtros_nacional,
        municipal = filtros_municipal
      )
      df_loc <- tryCatch({ rm(list = ls(envir = cache_env), envir = cache_env); filtra_localidade(dfb, f) }, error = function(e) NULL)
      if (is.null(df_loc)) next

      rot <- paste0(nm, " [", b, "/", fsc, "]")
      compara(paste(rot, "local"), function() cria_indicadores_old(df_loc, df_calcs, filtros = f), function() cria_indicadores_new(df_loc, df_calcs, filtros = f))
      compara(paste(rot, "referencia"), function() cria_indicadores_old(df_loc, df_calcs, filtros = f, referencia = TRUE), function() cria_indicadores_new(df_loc, df_calcs, filtros = f, referencia = TRUE))
      compara(paste(rot, "sem localidade"), function() cria_indicadores_old(df_loc, df_calcs, filtros = f, adicionar_localidade = FALSE), function() cria_indicadores_new(df_loc, df_calcs, filtros = f, adicionar_localidade = FALSE))
      compara(paste(rot, "comp"), function() cria_indicadores_old(df_loc, df_calcs, filtros = filtros_comp, comp = TRUE), function() cria_indicadores_new(df_loc, df_calcs, filtros = filtros_comp, comp = TRUE))
      compara(paste(rot, "escolha2"), function() cria_indicadores_old(df_loc, df_calcs, filtros = filtros_comp, localidade_resumo = "escolha2"), function() cria_indicadores_new(df_loc, df_calcs, filtros = filtros_comp, localidade_resumo = "escolha2"))
    }
  }
}

cat("\n== ramo agrupado por ano (branch do full_join) ==\n")
df_calcs <- extrai_calcs("R/mod_bloco_1.R", "bloco1_calcs")
df_loc_g <- dplyr::group_by(filtra_localidade(bases$bloco1, filtros_estadual), ano)
compara("bloco1_calcs agrupado por ano", function() cria_indicadores_old(df_loc_g, df_calcs, filtros = filtros_estadual, adicionar_localidade = FALSE), function() cria_indicadores_new(df_loc_g, df_calcs, filtros = filtros_estadual, adicionar_localidade = FALSE))

df_calcs4 <- extrai_calcs("R/mod_bloco_4.R", "bloco4_calcs")
df_loc_g4 <- dplyr::group_by(filtra_localidade(bases$bloco4, filtros_estadual), ano)
compara("bloco4_calcs agrupado por ano", function() cria_indicadores_old(df_loc_g4, df_calcs4, filtros = filtros_estadual, adicionar_localidade = FALSE), function() cria_indicadores_new(df_loc_g4, df_calcs4, filtros = filtros_estadual, adicionar_localidade = FALSE))

cat("\nchecks:", checks, "| falhas:", falhas, "\n")
if (falhas == 0) cat("OK: nova cria_indicadores identica a antiga em todos os casos.\n")
