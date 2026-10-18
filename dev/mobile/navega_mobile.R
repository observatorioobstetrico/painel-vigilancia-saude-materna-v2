# Navega por todas as abas em viewport mobile, salvando screenshots do viewport
# (390x844) para inspeção visual.
# Usage: Rscript dev/mobile/navega_mobile.R <outdir>
args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "dev/mobile/out_nav"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stopnav")
unlink(stopfile)
log <- file.path(repo, outdir, "app.log")

porta_ocupada <- tryCatch(
  { con <- suppressWarnings(socketConnection("127.0.0.1", 8123, open = "r", timeout = 1)); close(con); TRUE },
  error = function(e) FALSE
)
if (porta_ocupada) stop("porta 8123 ocupada")

px <- processx::process$new(
  "Rscript", c("dev/mobile/serve_app.R", stopfile),
  wd = repo, stdout = log, stderr = log, cleanup = TRUE
)
cleanup <- function() {
  try(px$kill_tree(), silent = TRUE)
  try(unlink(stopfile), silent = TRUE)
  invisible(NULL)
}
on.exit(cleanup())

t0 <- Sys.time()
repeat {
  if (!px$is_alive()) stop("app process died; see ", log)
  con <- try(suppressWarnings(socketConnection("127.0.0.1", 8123, open = "r", timeout = 1)), silent = TRUE)
  if (!inherits(con, "try-error")) { close(con); break }
  if (as.numeric(Sys.time() - t0, units = "secs") > 90) stop("app did not start")
  Sys.sleep(0.3)
}
cat("SERVER_UP\n")

library(chromote)
b <- ChromoteSession$new()
on.exit({ try(b$close(), silent = TRUE); cleanup() }, add = TRUE)
try(b$Network$enable(), silent = TRUE)
try(b$Network$setCacheDisabled(cacheDisabled = TRUE), silent = TRUE)

eval_js <- function(js) {
  r <- b$Runtime$evaluate(sprintf("(function(){ %s })()", js))
  r$result$value
}
wait_js <- function(expr, timeout = 60, poll = 0.15) {
  t0 <- Sys.time()
  repeat {
    r <- b$Runtime$evaluate(expr)
    if (isTRUE(r$result$value)) return(TRUE)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) stop("timeout: ", expr)
    Sys.sleep(poll)
  }
}
shot_view <- function(name) {
  p <- file.path(repo, outdir, paste0(name, ".png"))
  r <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(r$data), p)
  cat("SHOT", p, "\n")
}
navega <- function(tab) {
  eval_js(sprintf("$('.main-sidebar a[data-value=\"%s\"]').click(); true", tab))
  Sys.sleep(1.2)
}
busca <- function(espera = 5) {
  eval_js("Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); true")
  Sys.sleep(espera)
}

b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
wait_js("!!window.Shiny", 60)
Sys.sleep(3)

cat("innerWidth:", eval_js("return window.innerWidth"), " innerHeight:", eval_js("return window.innerHeight"), "\n")

# SOBRE
shot_view("v01_sobre_topo")
eval_js("window.scrollTo(0, 700); true")
Sys.sleep(0.5)
shot_view("v02_sobre_700")
eval_js("window.scrollTo(0, 0); true")

# NIVEL 1
navega("nivel_1")
shot_view("v03_nivel1_topo")
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('ano',2024); true")
Sys.sleep(0.4)
busca(6)
shot_view("v04_nivel1_resultado_topo")
eval_js("window.scrollTo(0, 1500); true")
Sys.sleep(0.5)
shot_view("v05_nivel1_resultado_1500")

# BLOCO 1
navega("bloco_1")
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('bloco','bloco1'); true")
Sys.sleep(0.5)
busca(6)
shot_view("v06_bloco1_topo")
eval_js("window.scrollTo(0, 1200); true")
Sys.sleep(0.5)
shot_view("v07_bloco1_1200")

# BLOCO 4
navega("bloco_4")
eval_js("Shiny.setInputValue('nivel','estadual'); true")
Sys.sleep(0.4)
eval_js("Shiny.setInputValue('estado','São Paulo'); true")
Sys.sleep(0.4)
eval_js("Shiny.setInputValue('bloco','bloco4'); true")
Sys.sleep(0.4)
busca(6)
shot_view("v08_bloco4_topo")
eval_js("window.scrollTo(0, 1000); true")
Sys.sleep(0.5)
shot_view("v09_bloco4_1000")

# BLOCO 7
navega("bloco_7")
eval_js("Shiny.setInputValue('nivel','estadual'); Shiny.setInputValue('bloco','bloco7'); true")
Sys.sleep(0.5)
busca(7)
shot_view("v10_bloco7_topo")
eval_js("window.scrollTo(0, 1400); true")
Sys.sleep(0.5)
shot_view("v11_bloco7_1400")

# NIVEL 3
navega("nivel_3")
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('bloco','bloco1'); true")
Sys.sleep(0.5)
eval_js("Shiny.setInputValue('indicador', 'Porcentagem de nascidos vivos de mães com menos de 4 anos de estudo'); true")
Sys.sleep(0.4)
busca(7)
shot_view("v12_nivel3_topo")
eval_js("window.scrollTo(0, 1200); true")
Sys.sleep(0.5)
shot_view("v13_nivel3_1200")

# DOCUMENTACAO
navega("documentacao")
shot_view("v14_documentacao")
eval_js("window.scrollTo(0, 600); true")
Sys.sleep(0.5)
shot_view("v15_documentacao_600")

# APARECIDA
navega("aparecida")
shot_view("v16_aparecida")

cat("erros de output global:", eval_js("return document.querySelectorAll('.shiny-output-error').length"), "\n")

writeLines("done", file.path(outdir, "PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("NAVEGACAO DONE\n")
