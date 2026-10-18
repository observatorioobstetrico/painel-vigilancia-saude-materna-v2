# Validates the new .div-titulo bar on desktop and mobile.
# Usage: Rscript dev/mobile/validate_layout.R <outdir>
args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "dev/mobile/out6"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop6")
unlink(stopfile)
log <- file.path(repo, outdir, "app.log")

porta_ocupada <- tryCatch(
  { con <- suppressWarnings(socketConnection("127.0.0.1", 8123, open = "r", timeout = 1)); close(con); TRUE },
  error = function(e) FALSE
)
if (porta_ocupada) stop("porta 8123 ja esta ocupada por outro processo — mate-o antes de rodar o probe")

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
shot <- function(name) {
  p <- file.path(repo, outdir, paste0(name, ".png"))
  b$screenshot(filename = p, selector = "html", delay = 0.25)
  cat("SHOT", p, "\n")
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

# ---------- DESKTOP ----------
b$Emulation$setDeviceMetricsOverride(width = 1400, height = 900, deviceScaleFactor = 1, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
wait_js("!!window.Shiny", 60)
Sys.sleep(3)

cat("\n== DESKTOP 1400x900 ==\n")
cat("navbar h:", eval_js("return Math.round(document.querySelector('.wrapper .main-header').getBoundingClientRect().height)"), "\n")
cat("altura-navbar var:", eval_js("return getComputedStyle(document.documentElement).getPropertyValue('--altura-navbar')"), "\n")
cat("barra (sobre):", eval_js("var t=document.querySelector('.div-titulo'); var r=t.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),l:Math.round(r.left),w:Math.round(r.width),h:Math.round(r.height),pos:getComputedStyle(t).position});"), "\n")
cat("conteudo sobre comeca em:", eval_js("var el=document.querySelector('.tab-pane.active'); var r=el.getBoundingClientRect(); return Math.round(r.top);"), "\n")
shot("d01_sobre")

cat("\n-- navega para bloco_1 (desktop) --\n")
eval_js("$('.sidebar a[data-value=\"bloco_1\"]').click(); true")
Sys.sleep(2)
cat("barra (bloco_1):", eval_js("var t=document.querySelector('.div-titulo'); var r=t.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),l:Math.round(r.left),w:Math.round(r.width),h:Math.round(r.height)});"), "\n")
cat("filtros top:", eval_js("var c=document.querySelector('.card'); var r=c.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),l:Math.round(r.left)});"), "\n")
cat("titulo e output:", eval_js("var o=document.getElementById('bloco_1_1-titulo_localidade'); return o ? o.innerText : 'AUSENTE';"), "\n")
shot("d02_bloco_1")

cat("\n-- scroll 600px: barra deve ficar sticky no topo --\n")
eval_js("window.scrollTo(0, 600); true")
Sys.sleep(0.6)
cat("scrollY:", eval_js("return window.scrollY"), "\n")
cat("barra pos scroll:", eval_js("var t=document.querySelector('.div-titulo'); var r=t.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),h:Math.round(r.height)});"), "\n")
shot("d03_scroll600")

cat("\n-- rola de volta e busca nacional --\n")
eval_js("window.scrollTo(0, 0); true")
Sys.sleep(0.4)
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); true")
Sys.sleep(6)
cat("erros:", eval_js("return document.querySelectorAll('.shiny-output-error').length"), "\n")
cat("titulo pos-busca:", eval_js("var o=document.getElementById('bloco_1_1-titulo_localidade'); return o ? o.innerText : 'AUSENTE';"), "\n")
shot("d04_pos_busca")

# ---------- MOBILE ----------
cat("\n\n== MOBILE 390x844 ==\n")
b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = TRUE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
wait_js("!!window.Shiny", 60)
Sys.sleep(3)

cat("innerWidth:", eval_js("return window.innerWidth"), "\n")
cat("altura-navbar var:", eval_js("return getComputedStyle(document.documentElement).getPropertyValue('--altura-navbar')"), "\n")
cat("barra pos:", eval_js("var t=document.querySelector('.div-titulo'); var r=t.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),l:Math.round(r.left),w:Math.round(r.width),h:Math.round(r.height),pos:getComputedStyle(t).position});"), "\n")
cat("spacer h:", eval_js("return document.getElementById('div-titulo-spacer').style.height"), "\n")
shot("m01_sobre")

cat("\n-- abre sidebar --\n")
eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(1)
cat("sidebar w:", eval_js("return Math.round(document.querySelector('.main-sidebar').getBoundingClientRect().width)"), "\n")
cat("overlay:", eval_js("var o=document.getElementById('sidebar-overlay'); return o ? getComputedStyle(o).display : 'n/a';"), "\n")
cat("elementFromPoint hamburger:", eval_js("var h=document.querySelector('[data-widget=\"pushmenu\"]'); var r=h.getBoundingClientRect(); var el=document.elementFromPoint(r.left+r.width/2, r.top+r.height/2); return el ? el.tagName + '.' + (el.className||'').toString().slice(0,50) : 'null';"), "\n")
shot("m02_sidebar")

cat("\n-- clica em Nivel 1 no menu --\n")
eval_js("$('.main-sidebar a[data-value=\"nivel_1\"]').click(); true")
Sys.sleep(1.5)
cat("aba:", eval_js("return Shiny.shinyapp.$inputValues.abas"), "\n")
cat("body:", eval_js("return document.body.className"), "\n")
shot("m03_apos_nivel1")

cat("\n-- filtra nacional + busca --\n")
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('ano', 2024); true")
Sys.sleep(0.4)
eval_js("Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); true")
Sys.sleep(6)
cat("erros:", eval_js("return document.querySelectorAll('.shiny-output-error').length"), "\n")
cat("graficos:", eval_js("return document.querySelectorAll('.highcharts-container').length"), "\n")
shot("m04_nivel1_resultado")

cat("\n-- rola para baixo: barra deve esconder --\n")
eval_js("window.scrollTo(0, 700); true")
Sys.sleep(0.8)
cat("barra hidden class:", eval_js("return document.querySelector('.div-titulo').classList.contains('nav-scroll-hidden')"), "\n")
cat("barra top:", eval_js("var r=document.querySelector('.div-titulo').getBoundingClientRect(); return Math.round(r.top);"), "\n")
shot("m05_scroll_down")

cat("\n-- rola para cima: barra volta --\n")
eval_js("window.scrollTo(0, 400); true")
Sys.sleep(0.8)
cat("barra hidden class:", eval_js("return document.querySelector('.div-titulo').classList.contains('nav-scroll-hidden')"), "\n")
cat("barra top:", eval_js("var r=document.querySelector('.div-titulo').getBoundingClientRect(); return Math.round(r.top);"), "\n")
shot("m06_scroll_up")

cat("\n-- navega para bloco_5 e confere titulo/barra --\n")
eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(0.8)
eval_js("$('.main-sidebar a[data-value=\"bloco_5\"]').click(); true")
Sys.sleep(1.5)
cat("aba:", eval_js("return Shiny.shinyapp.$inputValues.abas"), "\n")
cat("sidebar fechada:", eval_js("return !document.body.classList.contains('sidebar-open')"), "\n")
cat("barra pos:", eval_js("var t=document.querySelector('.div-titulo'); var r=t.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),h:Math.round(r.height)});"), "\n")
cat("spacer:", eval_js("return document.getElementById('div-titulo-spacer').style.height"), "\n")
shot("m07_bloco5")

writeLines("done", file.path(outdir, "PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("VALIDATION DONE\n")
