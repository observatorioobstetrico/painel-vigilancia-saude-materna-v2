suppressMessages(pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE))
ns <- asNamespace("painel.indicadores")
source("dev/perf/bench_helpers.R")

bloco1 <- get("bloco1", envir = ns)
bloco2 <- get("bloco2", envir = ns)
bloco3 <- get("bloco3", envir = ns)

tabela_indicadores <- get("tabela_indicadores", envir = ns)
cat("valores de tipo_do_indicador:\n")
print(table(tabela_indicadores$tipo_do_indicador))

base_agregada_old <- function(base, infos, anos) {
  base |>
    dplyr::filter(ano %in% anos) |>
    dplyr::group_by(uf, macro_r_saude, municipio) |>
    dplyr::summarise(
      numerador = !!rlang::parse_expr(infos$numerador),
      denominador = !!rlang::parse_expr(infos$denominador),
      indicador := dplyr::if_else(
        condition = infos$tipo_do_indicador == "porcentagem",
        true = numerador / denominador,
        false = round(numerador / denominador * infos$fator, 1)
      )
    )
}

base_agregada_new <- function(base, infos, anos) {
  dados <- base |>
    dplyr::filter(ano %in% anos) |>
    dplyr::group_by(uf, macro_r_saude, municipio) |>
    dplyr::summarise(
      numerador = !!rlang::parse_expr(infos$numerador),
      denominador = !!rlang::parse_expr(infos$denominador)
    )
  dados$indicador <- if (infos$tipo_do_indicador == "porcentagem") {
    dados$numerador / dados$denominador
  } else {
    round(dados$numerador / dados$denominador * infos$fator, 1)
  }
  dados
}

indicadores_teste <- c(
  "Porcentagem de nascidos vivos de mães com menos de 4 anos de estudo",
  "Porcentagem de nascidos vivos com baixo peso ao nascer",
  "Número de nascidos vivos"
)

falhas <- 0
for (ind in indicadores_teste) {
  infos <- tabela_indicadores[tabela_indicadores$indicador == ind, , drop = FALSE]
  if (nrow(infos) == 0) { cat("indicador nao encontrado:", ind, "\n"); next }
  infos <- infos[1, ]
  base <- switch(infos$bloco,
    bloco1 = bloco1,
    bloco2 = bloco2,
    bloco3 = bloco3,
    NULL
  )
  if (is.null(base)) { cat("base nao mapeada para", ind, "(bloco", infos$bloco, ") - pulando\n"); next }

  old <- base_agregada_old(base, infos, 2012:2024)
  new <- base_agregada_new(base, infos, 2012:2024)
  ok <- isTRUE(all.equal(as.data.frame(old), as.data.frame(new), check.attributes = TRUE)) &&
    identical(class(old), class(new)) &&
    identical(dplyr::group_vars(old), dplyr::group_vars(new)) &&
    identical(names(old), names(new))
  cat(sprintf("%-70s %s (tipo=%s, fator=%s)\n", substr(ind, 1, 68), ifelse(ok, "OK", "FALHOU"), infos$tipo_do_indicador, infos$fator))
  if (!ok) {
    falhas <- falhas + 1
    print(all.equal(as.data.frame(old), as.data.frame(new), check.attributes = TRUE))
  }
}
cat("\nfalhas:", falhas, "\n")

# tempo old vs new no cenario nacional real
infos <- tabela_indicadores[tabela_indicadores$indicador == indicadores_teste[1], , drop = FALSE][1, ]
source("dev/perf/bench_helpers.R")
tempo("data_tabela1 OLD (if_else por grupo)", base_agregada_old(bloco1, infos, 2012:2024), rep = 3)
tempo("data_tabela1 NEW (fora do summarise)", base_agregada_new(bloco1, infos, 2012:2024), rep = 3)

# caso tipo != porcentagem (ramo else), com tabela do bloco 3 e bloco 7
extras <- list(
  list(base = "bloco3", ind = tabela_indicadores[tabela_indicadores$bloco == "bloco3" & tabela_indicadores$tipo_do_indicador != "porcentagem", , drop = FALSE]),
  list(base = "bloco7", ind = tabela_indicadores[grepl("Taxa de mortalidade fetal", tabela_indicadores$indicador), , drop = FALSE])
)
for (ex in extras) {
  if (nrow(ex$ind) == 0) { cat("sem indicador em", ex$base, "\n"); next }
  for (k in seq_len(min(3, nrow(ex$ind)))) {
    infos <- ex$ind[k, ]
    base <- get(ex$base, envir = ns)
    old <- tryCatch(base_agregada_old(base, infos, 2012:2024), error = function(e) NULL)
    new <- tryCatch(base_agregada_new(base, infos, 2012:2024), error = function(e) NULL)
    if (is.null(old) && is.null(new)) { cat(ex$base, infos$indicador, ": ambos falham igual (ok p/ espelho)\n"); next }
    ok <- isTRUE(all.equal(as.data.frame(old), as.data.frame(new), check.attributes = TRUE)) &&
      identical(class(old), class(new)) && identical(names(old), names(new))
    cat(sprintf("%-58s [%s] %s\n", substr(infos$indicador, 1, 56), ex$base, ifelse(ok, "OK", "FALHOU")))
    if (!ok) print(all.equal(as.data.frame(old), as.data.frame(new), check.attributes = TRUE))
  }
}
