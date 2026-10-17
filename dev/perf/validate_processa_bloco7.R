# Valida processa_bloco7 (com cache) contra a versao antiga (sem cache)
suppressMessages(pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE))
ns <- asNamespace("painel.indicadores")
source("dev/perf/bench_helpers.R")
cache_env <- get(".cache_interno", envir = ns)

processa_bloco7_new <- get("processa_bloco7", envir = ns)
filtra_localidade <- get("filtra_localidade", envir = ns)

# copia da versao antiga (sem cache)
processa_bloco7_old <- function(data, input, filtros, cols_fn, indicadores_fn, localidade_resumo = "escolha1", comp = FALSE, referencia = FALSE) {
  dt <- data |> data.table::as.data.table()
  cols <- cols_fn(input)
  colunas <- unique(unlist(cols, use.names = FALSE))
  agrega <- function(grp) {
    dt[, lapply(.SD, \(x) sum(as.numeric(x), na.rm = TRUE)), by = grp, .SDcols = colunas]
  }
  monta <- function(agg, grp) {
    if (nrow(agg) == 0) {
      s <- lapply(cols, function(v) 0)
      names(s) <- names(cols)
      return(cbind(agg[, grp, with = FALSE], data.table::rbindlist(list(indicadores_fn(s)), fill = TRUE)[0]))
    }
    linhas <- lapply(seq_len(nrow(agg)), function(i) {
      valores <- agg[i, colunas, with = FALSE]
      s <- lapply(cols, function(v) {
        sum(vapply(v, function(cn) as.numeric(valores[[cn]][1]), numeric(1)))
      })
      names(s) <- names(cols)
      data.table::rbindlist(list(indicadores_fn(s)), fill = TRUE)
    })
    cbind(agg[, grp, with = FALSE], data.table::rbindlist(linhas, fill = TRUE))
  }
  agg_graficos <- agrega(c("ano", "class"))
  agg_resumo <- agg_graficos[, lapply(.SD, sum), .SDcols = colunas, by = "class"]
  list(graficos = monta(agg_graficos, c("ano", "class")), resumo = monta(agg_resumo, "class"))
}

extrai_calcs <- function(arquivo, nome, env = parent.frame()) {
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
  eval(achado, envir = env)
}

input_fetal <- list(
  input_num_obitos_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais", "sem_info"),
  input_num_obitos_momento = "todos",
  input_taxa_de_mortalidade_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais", "sem_info"),
  input_taxa_de_mortalidade_momento = "todos",
  input_distribuicao_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais"),
  input_distribuicao_momento = c("antes", "durante")
)
input_fetal_b <- modifyList(input_fetal, list(
  input_num_obitos_momento = "antes",
  input_distribuicao_momento = c("durante")
))

input_neonatal <- list(
  input_num_obitos_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais", "sem_info"),
  input_taxa_de_mortalidade_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais", "sem_info"),
  input_taxa_de_mortalidade_precoce_peso = c("menos_1000"),
  input_taxa_de_mortalidade_tardia_peso = c("2500_mais"),
  input_distribuicao_peso = c("menos_1000", "1000_1499", "1500_2499", "2500_mais"),
  input_distribuicao_momento = c("0_dias", "1_a_6_dias", "7_a_27_dias")
)
input_neonatal_b <- modifyList(input_neonatal, list(
  input_num_obitos_peso = c("menos_1000"),
  input_distribuicao_momento = c("0_dias")
))

input_morbidade <- list(
  input_internacoes_local = "na_macro",
  input_internacoes_idade = "geral",
  input_internacoes_uti_local = "na_macro",
  input_internacoes_uti_idade = "geral"
)

cenarios <- list(
  list(nome = "fetal num_obitos", base = "bloco7_fetal", cols = "cols_num_obitos_taxa", ind = "indicadores_num_obitos_taxa", input = quote(input_fetal), filtros = "filtros_estadual"),
  list(nome = "fetal distrib_peso", base = "bloco7_fetal", cols = "cols_distribuicao_peso", ind = "indicadores_distribuicao_peso", input = quote(input_fetal), filtros = "filtros_estadual"),
  list(nome = "fetal distrib_momento", base = "bloco7_fetal", cols = "cols_distribuicao_momento", ind = "indicadores_distribuicao_momento", input = quote(input_fetal), filtros = "filtros_estadual"),
  list(nome = "neonatal num_obitos", base = "bloco7_neonatal", cols = "cols_num_obitos_taxa", ind = "indicadores_num_obitos_taxa", input = quote(input_neonatal), filtros = "filtros_nacional"),
  list(nome = "neonatal distrib_peso", base = "bloco7_neonatal", cols = "cols_distribuicao_peso", ind = "indicadores_distribuicao_peso", input = quote(input_neonatal), filtros = "filtros_nacional"),
  list(nome = "neonatal distrib_momento", base = "bloco7_neonatal", cols = "cols_distribuicao_momento", ind = "indicadores_distribuicao_momento", input = quote(input_neonatal), filtros = "filtros_nacional"),
  list(nome = "morbidade ameacadoras", base = "bloco7_morbidade_neonatal", cols = "cols_ameacadoras", ind = "indicadores_ameacadoras", input = quote(list()), filtros = "filtros_estadual"),
  list(nome = "morbidade internacoes", base = "bloco7_morbidade_neonatal", cols = "cols_internacoes", ind = "indicadores_internacoes", input = quote(input_morbidade), filtros = "filtros_estadual"),
  list(nome = "morbidade internacoes_uti", base = "bloco7_morbidade_neonatal", cols = "cols_internacoes_uti", ind = "indicadores_internacoes_uti", input = quote(input_morbidade), filtros = "filtros_estadual")
)

filtros_estadual <- filtros_padrao(nivel = "estadual", estado = "São Paulo", bloco = "bloco7")
filtros_nacional <- filtros_padrao(nivel = "nacional", estado = "", bloco = "bloco7")
filtros_estadual_comp <- filtros_padrao(nivel = "estadual", estado = "São Paulo", bloco = "bloco7",
                                        comparar = "Sim", nivel2 = "estadual", estado2 = "Minas Gerais")
filtros_nacional_comp <- filtros_padrao(nivel = "nacional", estado = "", bloco = "bloco7",
                                        comparar = "Sim", nivel2 = "estadual", estado2 = "Minas Gerais")
filtros_vazio <- filtros_padrao(nivel = "estadual", estado = "ZZ", bloco = "bloco7")

modulo_do_scenario <- function(nome) {
  if (grepl("^fetal", nome)) return("R/mod_bloco_7_fetal.R")
  if (grepl("^neonatal", nome)) return("R/mod_bloco_7_neonatal.R")
  if (grepl("^morbidade", nome)) return("R/mod_bloco_7_morbidade_neonatal.R")
  stop("?")
}

falhas <- 0
checks <- 0

compara_tab <- function(a, b) {
  isTRUE(all.equal(as.data.frame(a), as.data.frame(b), check.attributes = TRUE)) &&
    identical(names(a), names(b)) && identical(class(a), class(b))
}

for (cs in cenarios) {
  arq <- modulo_do_scenario(cs$nome)
  cols_fn <- extrai_calcs(arq, cs$cols)
  ind_fn <- extrai_calcs(arq, cs$ind)
  input <- eval(cs$input)
  base <- get(cs$base, envir = ns)
  fr <- get(cs$filtros)

  for (escopo in c("local", "referencia", "comp")) {
    ref <- escopo == "referencia"
    cmp <- escopo == "comp"
    fr <- if (cmp) get(paste0(cs$filtros, "_comp")) else get(cs$filtros)
    pesq <- list(
      filtros = fr, referencia = ref, comp = cmp,
      input = input
    )
    compara_escopo <- function(tag, input_uso) {
      checks <<- checks + 1
      rm(list = ls(envir = cache_env), envir = cache_env)
      df <- filtra_localidade(base, fr, comp = cmp, referencia = ref)
      old <- processa_bloco7_old(df, input_uso, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)
      new1 <- processa_bloco7_new(df, input_uso, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)
      new2 <- processa_bloco7_new(df, input_uso, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)
      ok <- compara_tab(old$graficos, new1$graficos) && compara_tab(old$resumo, new1$resumo) &&
        identical(new1, new2)
      if (!ok) {
        falhas <<- falhas + 1
        cat("  FALHOU:", cs$nome, escopo, tag, "\n")
      }
    }
    compara_escopo("base", input)

    # segundo input (chave de cache diferente, resultado diferente)
    if (cs$nome == "fetal num_obitos") {
      df <- filtra_localidade(base, fr, comp = cmp, referencia = ref)
      old_b <- processa_bloco7_old(df, input_fetal_b, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)
      checks <- checks + 1
      new_b <- processa_bloco7_new(df, input_fetal_b, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)
      new_a_again <- processa_bloco7_new(df, input, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)
      ok <- compara_tab(old_b$graficos, new_b$graficos) && compara_tab(old_b$resumo, new_b$resumo) &&
        compara_tab(new_a_again$graficos, processa_bloco7_old(df, input, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)$graficos)
      if (!ok) { falhas <- falhas + 1; cat("  FALHOU:", cs$nome, escopo, "troca de input\n") }
    }
    if (cs$nome == "neonatal num_obitos") {
      df <- filtra_localidade(base, fr, comp = cmp, referencia = ref)
      old_b <- processa_bloco7_old(df, input_neonatal_b, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)
      checks <- checks + 1
      new_b <- processa_bloco7_new(df, input_neonatal_b, fr, cols_fn, ind_fn, comp = cmp, referencia = ref)
      ok <- compara_tab(old_b$graficos, new_b$graficos) && compara_tab(old_b$resumo, new_b$resumo)
      if (!ok) {
        falhas <- falhas + 1
        cat("  FALHOU:", cs$nome, escopo, "troca de input\n")
        cat("   graficos old:", paste(dim(old_b$graficos), collapse = "x"), "| new:", paste(dim(new_b$graficos), collapse = "x"), "\n")
        print(all.equal(as.data.frame(old_b$graficos), as.data.frame(new_b$graficos), check.attributes = TRUE))
        cat("   resumo old:", paste(dim(old_b$resumo), collapse = "x"), "| new:", paste(dim(new_b$resumo), collapse = "x"), "\n")
        print(all.equal(as.data.frame(old_b$resumo), as.data.frame(new_b$resumo), check.attributes = TRUE))
      }
    }
  }
}

# caso vazio
cat("== caso vazio (Acre 2025-2026) ==\n")
for (cs in cenarios[1:2]) {
  arq <- modulo_do_scenario(cs$nome)
  cols_fn <- extrai_calcs(arq, cs$cols)
  ind_fn <- extrai_calcs(arq, cs$ind)
  input <- eval(cs$input)
  base <- get(cs$base, envir = ns)
  rm(list = ls(envir = cache_env), envir = cache_env)
  df <- filtra_localidade(base, filtros_vazio)
  checks <- checks + 1
  old <- processa_bloco7_old(df, input, filtros_vazio, cols_fn, ind_fn)
  new <- processa_bloco7_new(df, input, filtros_vazio, cols_fn, ind_fn)
  ok <- compara_tab(old$graficos, new$graficos) && compara_tab(old$resumo, new$resumo) && nrow(new$graficos) == 0
  if (!ok) { falhas <- falhas + 1; cat("  FALHOU: vazio", cs$nome, "\n") }
}

cat("\nchecks:", checks, "| falhas:", falhas, "\n")
if (falhas == 0) cat("OK: processa_bloco7 com cache identico ao antigo.\n")
