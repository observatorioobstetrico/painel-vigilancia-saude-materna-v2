# Mobile probe: starts the app, emulates a phone viewport and dumps the DOM
# state + screenshots for a set of interactions.
# Usage: Rscript dev/mobile/probe_mobile.R <outdir> [scenario]
args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "dev/mobile/out"
scenario <- if (length(args) >= 2) args[[2]] else "all"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop")
unlink(stopfile)
log <- file.path(repo, outdir, "app.log")
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
cat("SERVER_UP after", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")

library(chromote)
b <- ChromoteSession$new()
on.exit({ try(b$close(), silent = TRUE); cleanup() }, add = TRUE)

b$set_viewport_size(width = 390, height = 844, mobile = TRUE)
try(
  b$Emulation$setUserAgentOverride(
    userAgent = paste0(
      "Mozilla/5.0 (iPhone; CPU iPhone OS 16_0 like Mac OS X) ",
      "AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Mobile/15E148 Safari/604.1"
    )
  ),
  silent = TRUE
)

b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(1)

wait_js <- function(expr, timeout = 60, poll = 0.15) {
  t0 <- Sys.time()
  repeat {
    r <- invisible(b$Runtime$evaluate(expr))
    if (isTRUE(r$result$value)) return(TRUE)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) stop("timeout waiting: ", expr)
    Sys.sleep(poll)
  }
}

wait_js("!!window.Shiny", 60)
wait_js("!window.Shiny.shinyapp.$socket.readyState || window.Shiny.shinyapp.$socket.readyState === 1", 60)
Sys.sleep(2)

shot <- function(name) {
  p <- file.path(repo, outdir, paste0(name, ".png"))
  b$screenshot(filename = p, selector = "html", delay = 0.2)
  cat("SHOT", p, "\n")
}

eval_js <- function(js) {
  r <- b$Runtime$evaluate(js)
  r$result$value
}

cat("== estado inicial ==\n")
cat("viewport:", eval_js("window.innerWidth + 'x' + window.innerHeight"), "\n")
cat("body classes:", eval_js("document.body.className"), "\n")
cat("navbar h:", eval_js("(document.querySelector('.wrapper .main-header')||{getBoundingClientRect:function(){return{height:0}}}).getBoundingClientRect().height"), "\n")
cat("sidebar display:", eval_js("getComputedStyle(document.querySelector('.main-sidebar')).transform + ' | left:' + getComputedStyle(document.querySelector('.main-sidebar')).left"), "\n")
cat("titulo count/size:", eval_js("(function(){var t=document.querySelectorAll('.div-titulo'); if(!t.length) return 'none'; var r=t[0].getBoundingClientRect(); return t.length + ' | rect=' + JSON.stringify({t:r.top,l:r.left,w:r.width,h:r.height}) + ' | pos=' + getComputedStyle(t[0]).position;})()"), "\n")
cat("hamburger visivel:", eval_js("(function(){var h=document.querySelector('[data-widget=\"pushmenu\"]'); if(!h) return 'none'; var r=h.getBoundingClientRect(); return 'rect=' + JSON.stringify({t:r.top,l:r.left,w:r.width,h:r.height}) + ' vis=' + getComputedStyle(h).visibility;})()"), "\n")
shot("01_inicial")

cat("== clicando hamburger ==\n")
eval_js("document.querySelector('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(1)
cat("body classes:", eval_js("document.body.className"), "\n")
cat("sidebar transform:", eval_js("getComputedStyle(document.querySelector('.main-sidebar')).transform"), "\n")
cat("sidebar z-index:", eval_js("getComputedStyle(document.querySelector('.main-sidebar')).zIndex"), "\n")
shot("02_sidebar_aberta")

cat("== clicando item nivel_1 da sidebar ==\n")
eval_js("(function(){var a=document.querySelector('.sidebar a[data-value=\"nivel_1\"]'); if(!a) return 'nao achou'; a.click(); return 'clicou';})()")
Sys.sleep(2)
cat("aba ativa (shiny):", eval_js("window.Shiny ? window.Shiny.shinyapp.$inputValues.abas : 'n/a'"), "\n")
cat("link ativo:", eval_js("(document.querySelector('.sidebar a.active')||{getAttribute:function(){return null}}).getAttribute('data-value')"), "\n")
cat("tab-pane ativa:", eval_js("(document.querySelector('.tab-pane.active')||{id:''}).id"), "\n")
cat("body classes:", eval_js("document.body.className"), "\n")
shot("03_apos_clique_nivel_1")

cat("== clicando item bloco_1 da sidebar ==\n")
eval_js("(function(){var a=document.querySelector('.sidebar a[data-value=\"bloco_1\"]'); if(!a) return 'nao achou'; a.click(); return 'clicou';})()")
Sys.sleep(2)
cat("tab-pane ativa:", eval_js("(document.querySelector('.tab-pane.active')||{id:''}).id"), "\n")
shot("04_apos_clique_bloco_1")

cat("== tentando interagir com o select de nivel (mobile) ==\n")
cat("select visivel:", eval_js("(function(){var e=document.getElementById('nivel'); if(!e) return 'nao achou'; var r=e.getBoundingClientRect(); return JSON.stringify({t:r.top,l:r.left,w:r.width,h:r.height, vis:getComputedStyle(e).visibility});})()"), "\n")
cat("div titulo cobre select? titulo bottom vs select top:", eval_js("(function(){var t=document.querySelector('.tab-pane.active .div-titulo'); var e=document.getElementById('nivel'); if(!t||!e) return 'n/a'; var tr=t.getBoundingClientRect(); var er=e.getBoundingClientRect(); return 'titulo.bottom=' + tr.bottom + ' select.top=' + er.top + ' overlap=' + (tr.bottom > er.top);})()"), "\n")
shot("05_bloco_1_estado")

if (scenario == "all") {
  cat("== screenshot full page bloco 1 ==\n")
  eval_js("window.scrollTo(0,0); true")
  Sys.sleep(0.5)
  shot("06_bloco_1_top")

  cat("== busca nivel 1 ==")
  eval_js("$('.sidebar a[data-value=\"nivel_1\"]').click(); true")
  Sys.sleep(1.5)
  eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
  Sys.sleep(0.6)
  eval_js("if (window.Shiny) { Shiny.setInputValue('nivel', 'nacional'); Shiny.setInputValue('ano', 2024); } true")
  Sys.sleep(0.4)
  eval_js("Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); true")
  Sys.sleep(5)
  shot("07_nivel_1_resultado")
  cat("erros de output:", eval_js("document.querySelectorAll('.shiny-output-error').length"), "\n")
  cat("graficos:", eval_js("document.querySelectorAll('.highcharts-container').length"), "\n")
}

writeLines("done", file.path(outdir, "PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
