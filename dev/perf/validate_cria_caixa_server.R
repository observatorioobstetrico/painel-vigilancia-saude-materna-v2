# Valida cria_caixa_server (novo: string HTML unica) contra a versao do HEAD.
suppressMessages(pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE))
ns <- asNamespace("painel.indicadores")
cria_caixa_server_new <- get("cria_caixa_server", envir = ns)

arq_old <- file.path(tempdir(), "funcoes_globais_head.R")
system2("git", c("show", "HEAD:R/funcoes_globais.R"), stdout = arq_old)
env_old <- new.env(parent = ns)
sys.source(arq_old, envir = env_old)
cria_caixa_server_old <- env_old$cria_caixa_server

norm <- function(x) {
  s <- as.character(x)
  gsub(">\\s+<", "><", trimws(gsub("\\s+", " ", gsub(">\\s*\\n\\s*<", "><", s))))
}

base_args <- list(
  dados = data.frame(indicador_x = 4321, taxa_x = 9.4321, porc_x = 15.75),
  indicador = "indicador_x",
  titulo = "Número de óbitos perinatais",
  tem_meta = FALSE,
  nivel_de_analise = "estadual",
  tipo_referencia = "",
  valor_de_referencia = 100000,
  tipo = "número",
  invertido = FALSE,
  pagina = "bloco_7",
  tamanho_caixa = 330
)

combos <- list()
add <- function(nome, ...) combos[[length(combos) + 1]] <<- modifyList(base_args, list(...)) |> append(list(nome = nome))
add("numero simples", nome = "numero simples")
add("porcentagem", indicador = "porc_x", tipo = "porcentagem", valor_de_referencia = 20)
add("taxa", indicador = "taxa_x", tipo = "taxa", valor_de_referencia = 8.7, tem_meta = TRUE, tipo_referencia = "meta ODS")
add("km", indicador = "taxa_x", tipo = "km", valor_de_referencia = 12)
add("invertido", indicador = "porc_x", tipo = "porcentagem", valor_de_referencia = 10, invertido = TRUE)
add("cor explicita", cor = "#f1605d")
add("referencia vetorial", valor_de_referencia = c(6.6, 12.5), tipo = "porcentagem", indicador = "porc_x")
add("referencia vetorial acima", valor_de_referencia = c(6.6, 12.5), valor_indicador = 30, tipo = "porcentagem")
add("referencia vetorial abaixo", valor_de_referencia = c(6.6, 12.5), valor_indicador = 2, tipo = "porcentagem")
add("razao >= 2", valor_indicador = 500000)
add("valor zero", valor_indicador = 0)
add("valor NA (NaN)", valor_indicador = NA_real_)
add("referencia zero", valor_de_referencia = 0, valor_indicador = 0)
add("referencia NA", valor_de_referencia = NA_real_)
add("texto_footer custom", texto_footer = "Qualquer texto <b>com HTML</b>")
add("texto_footer glue", texto_footer = "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional")
add("texto_caixa", texto_caixa = "12,3 un")
add("nacional sem footer", nivel_de_analise = "nacional")
add("nacional com footer", nivel_de_analise = "nacional", texto_footer = "x")
add("caixa completa", retornar_caixa_completa = TRUE)
add("caixa completa nacional", retornar_caixa_completa = TRUE, nivel_de_analise = "nacional")
add("caixa completa taxa", retornar_caixa_completa = TRUE, indicador = "taxa_x", tipo = "taxa", valor_de_referencia = 8.7)
add("fonte custom", fonte_titulo = "fonte-media", fonte_comparacao = "fonte-grande")
add("tamanho 300", tamanho_caixa = 300)
add("width 6", width_caixa = 6)
add("titulo com html", titulo = "Taxa&nbsp;de mortalidade <i>x</i>")
add("valor negativo", valor_indicador = -5, valor_de_referencia = 10)
add("dados string", dados = data.frame(indicador_x = "12345"), valor_indicador = NULL)
add("milhar grande", valor_indicador = 1234567)
add("decimal longo", valor_indicador = 0.123456)

falhas <- 0
checks <- 0
chama <- function(fn, args) tryCatch(list(ok = TRUE, valor = do.call(fn, args)), error = function(e) list(ok = FALSE, valor = conditionMessage(e)))
# testa os dois caminhos (com e sem bs4Dash::box) para cada combinacao
for (cs in combos) {
  for (completa in c(FALSE, TRUE)) {
    nome <- cs$nome
    cs$nome <- NULL
    cs$retornar_caixa_completa <- completa
    checks <- checks + 1
    a <- chama(cria_caixa_server_old, cs)
    b <- chama(cria_caixa_server_new, cs)
    if (!a$ok || !b$ok) {
      ok <- identical(a$ok, b$ok) && identical(a$valor, b$valor)
    } else {
      ok <- isTRUE(identical(norm(a$valor), norm(b$valor)))
    }
    if (!ok) {
      falhas <- falhas + 1
      cat("FALHOU:", nome, "| completa:", completa, "\n")
      if (a$ok && b$ok) {
        cat(" OLD:", substr(norm(a$valor), 1, 300), "\n NEW:", substr(norm(b$valor), 1, 300), "\n")
      } else {
        cat(" old:", substr(a$valor, 1, 200), "\n new:", substr(b$valor, 1, 200), "\n")
      }
    }
  }
}
cat("\nchecks:", checks, "| falhas:", falhas, "\n")
if (falhas == 0) cat("OK: cria_caixa_server produz HTML identico ao antigo (TRUE e FALSE).\n")
