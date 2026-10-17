# Valida cria_caixa_conjunta_bloco5 (novo: HTML string) contra HEAD.
suppressMessages(pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE))
ns <- asNamespace("painel.indicadores")
new <- get("cria_caixa_conjunta_bloco5", envir = ns)
arq_old <- file.path(tempdir(), "funcoes_globais_head.R")
system2("git", c("show", "HEAD:R/funcoes_globais.R"), stdout = arq_old)
env_old <- new.env(parent = ns)
sys.source(arq_old, envir = env_old)
old <- env_old$cria_caixa_conjunta_bloco5

norm <- function(x) {
  s <- as.character(x)
  gsub(">\\s+<", "><", trimws(gsub("\\s+", " ", gsub(">\\s*\\n\\s*<", "><", s))))
}

dados_bp <- data.frame(porc_baixo_peso_menor_1000 = 25.5,
                       porc_baixo_peso_1000_a_1499 = 10,
                       porc_baixo_peso_1500_a_2499 = 30.25)
dados_bp_nan <- data.frame(porc_baixo_peso_menor_1000 = NaN,
                           porc_baixo_peso_1000_a_1499 = 12.5,
                           porc_baixo_peso_1500_a_2499 = NaN)
dados_pre <- data.frame(porc_premat_menos_de_28_semanas = 5.5,
                        porc_premat_28_a_32_semanas = 10,
                        porc_premat_33_a_34_semanas = 15.25,
                        porc_premat_35_a_36_semanas = 64,
                        porc_premat_faltantes = 5.25)
dados_pre_nan <- data.frame(porc_premat_menos_de_28_semanas = NaN,
                            porc_premat_28_a_32_semanas = 10,
                            porc_premat_33_a_34_semanas = NaN,
                            porc_premat_35_a_36_semanas = 64,
                            porc_premat_faltantes = NaN)

casos <- list(
  list(nome = "baixo peso normal", args = list(dados = dados_bp, indicador = "baixo peso",
       titulo = "Dentre os nascidos vivos com baixo peso,", tamanho_caixa = 320)),
  list(nome = "baixo peso NaN", args = list(dados = dados_bp_nan, indicador = "baixo peso",
       titulo = "Dentre os nascidos vivos com baixo peso,", tamanho_caixa = 320)),
  list(nome = "prematuridade normal", args = list(dados = dados_pre, indicador = "prematuridade",
       titulo = "Dentre os nascimentos prematuros,", tamanho_caixa = 320)),
  list(nome = "prematuridade NaN", args = list(dados = dados_pre_nan, indicador = "prematuridade",
       titulo = "Dentre os nascimentos prematuros,", tamanho_caixa = 320)),
  list(nome = "prematuridade default 300", args = list(dados = dados_pre, indicador = "prematuridade",
       titulo = "Dentre os nascimentos prematuros,")),
  list(nome = "fonte custom", args = list(dados = dados_bp, indicador = "baixo peso",
       titulo = "Título <i>x</i>", fonte_titulo = "fonte-media", width_caixa = 6)),
  list(nome = "indicador invalido", args = list(dados = dados_bp, indicador = "outro", titulo = "x"))
)

chama <- function(fn, args) tryCatch(list(ok = TRUE, valor = do.call(fn, args)),
                                     error = function(e) list(ok = FALSE, valor = conditionMessage(e)))

falhas <- 0
checks <- 0
for (cs in casos) {
  checks <- checks + 1
  a <- chama(old, cs$args); b <- chama(new, cs$args)
  if (!a$ok || !b$ok) {
    ok <- identical(a$ok, b$ok) && identical(a$valor, b$valor)
  } else if (is.null(a$valor) || is.null(b$valor)) {
    ok <- identical(a$valor, b$valor)
  } else {
    ok <- isTRUE(identical(norm(a$valor), norm(b$valor)))
  }
  if (!ok) {
    falhas <- falhas + 1
    cat("FALHOU:", cs$nome, "\n")
    if (a$ok && b$ok && !is.null(a$valor) && !is.null(b$valor)) {
      na <- norm(a$valor); nb <- norm(b$valor)
      n <- min(nchar(na), nchar(nb))
      i <- which(strsplit(na, "")[[1]][1:n] != strsplit(nb, "")[[1]][1:n])[1]
      if (!is.na(i)) cat("  pos", i, "\n  OLD:", substr(na, max(1, i - 80), i + 100), "\n  NEW:", substr(nb, max(1, i - 80), i + 100), "\n")
    } else {
      cat("  old:", substr(paste(a$valor, collapse = " "), 1, 200), "\n  new:", substr(paste(b$valor, collapse = " "), 1, 200), "\n")
    }
  }
}
cat("\nchecks:", checks, "| falhas:", falhas, "\n")
if (falhas == 0) cat("OK: cria_caixa_conjunta_bloco5 produz HTML identico ao antigo.\n")
