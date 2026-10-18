# Focused tap probe: uses real input events (mouse/touch) on the mobile viewport.
# Usage: Rscript dev/mobile/probe_taps.R <outdir>
args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "dev/mobile/out2"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop2")
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

b$Emulation$setDeviceMetricsOverride(
  width = 390, height = 844, deviceScaleFactor = 3, mobile = TRUE
)
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
Sys.sleep(2)

eval_js <- function(js) {
  r <- b$Runtime$evaluate(sprintf("(function(){ %s })()", js))
  r$result$value
}

shot <- function(name) {
  p <- file.path(repo, outdir, paste0(name, ".png"))
  b$screenshot(filename = p, selector = "html", delay = 0.2)
  cat("SHOT", p, "\n")
}

tap_at <- function(x, y, label = "") {
  hit <- eval_js(sprintf("var el=document.elementFromPoint(%f,%f); return el ? (el.tagName + '|' + (el.className||'').toString().slice(0,60) + '|' + (el.innerText||'').toString().slice(0,40)) : 'none';", x, y))
  b$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = "left", clickCount = 1L)
  Sys.sleep(0.06)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button = "left", clickCount = 1L)
  Sys.sleep(0.5)
  cat(sprintf("TAP (%s) at %.0f,%.0f -> %s\n", label, x, y, hit))
}

cat("meta viewport:", eval_js("var m=document.querySelector('meta[name=viewport]'); return m? m.content : 'NENHUM'"), "\n")
cat("innerWidth x innerHeight:", eval_js("return window.innerWidth + 'x' + window.innerHeight"), "\n")
cat("visual scale:", eval_js("return window.visualViewport ? window.visualViewport.scale : 'n/a'"), "\n")
cat("devicePixelRatio:", eval_js("return window.devicePixelRatio"), "\n")
cat("screen:", eval_js("return screen.width + 'x' + screen.height"), "\n")
cat("outer:", eval_js("return window.outerWidth + 'x' + window.outerHeight"), "\n")
shot("t01_inicial")

cat("\n== rect do hamburger ==\n")
cat(eval_js("var h=document.querySelector('[data-widget=\"pushmenu\"]'); var r=h.getBoundingClientRect(); return JSON.stringify({t:r.top,l:r.left,w:r.width,h:r.height,cx:r.left+r.width/2,cy:r.top+r.height/2})"), "\n")
cat("== rect do item nivel_1 (sidebar fechada) ==\n")
cat(eval_js("var a=document.querySelector('.sidebar a[data-value=\"nivel_1\"]'); var r=a.getBoundingClientRect(); return JSON.stringify({t:r.top,l:r.left,w:r.width,h:r.height})"), "\n")

cat("\n== TAP hamburger ==\n")
hc <- jsonlite::fromJSON(eval_js("var h=document.querySelector('[data-widget=\"pushmenu\"]'); var r=h.getBoundingClientRect(); return JSON.stringify({cx:r.left+r.width/2,cy:r.top+r.height/2})"))
tap_at(hc$cx, hc$cy, "hamburger")
cat("body classes:", eval_js("return document.body.className"), "\n")
cat("sidebar rect:", eval_js("var s=document.querySelector('.main-sidebar'); var r=s.getBoundingClientRect(); return JSON.stringify({t:r.top,l:r.left,w:r.width,h:r.height,r:r.right})"), "\n")
shot("t02_sidebar_aberta")

cat("\n== elementFromPoint nos itens do menu (sidebar aberta) ==\n")
items <- eval_js("
  var names = ['nivel_1','bloco_1','bloco_2','nivel_3'];
  var out = [];
  names.forEach(function(n){
    var a = document.querySelector('.sidebar a[data-value=\"' + n + '\"]');
    if (!a) { out.push(n + ': nao achou'); return; }
    var r = a.getBoundingClientRect();
    var el = document.elementFromPoint(r.left + r.width/2, r.top + r.height/2);
    var hit = (el === a || a.contains(el)) ? 'ALVO' : (el ? el.tagName + '|' + (el.className||'').toString().slice(0,50) : 'null');
    out.push(n + ': rect=' + JSON.stringify({x:Math.round(r.left), y:Math.round(r.top), w:Math.round(r.width), h:Math.round(r.height)}) + ' hit=' + hit);
  });
  return out.join(' || ');
")
cat(strsplit(items, " \\|\\| ")[[1]], sep = "\n")

cat("\n== TAP item nivel_1 ==\n")
r1 <- jsonlite::fromJSON(eval_js("var a=document.querySelector('.sidebar a[data-value=\"nivel_1\"]'); var r=a.getBoundingClientRect(); return JSON.stringify({cx:r.left+r.width/2,cy:r.top+r.height/2})"))
tap_at(r1$cx, r1$cy, "nivel_1")
cat("aba (shiny):", eval_js("return window.Shiny ? window.Shiny.shinyapp.$inputValues.abas : 'n/a'"), "\n")
cat("body classes:", eval_js("return document.body.className"), "\n")
shot("t03_apos_tap_nivel_1")

cat("\n== fecha sidebar (tap hamburger) ==\n")
tap_at(hc$cx, hc$cy, "hamburger")
cat("body classes:", eval_js("return document.body.className"), "\n")
Sys.sleep(1)
cat("aba (shiny):", eval_js("return window.Shiny ? window.Shiny.shinyapp.$inputValues.abas : 'n/a'"), "\n")
shot("t04_nivel_1_fechado")

cat("\n== TAP item bloco_1 (reabrindo) ==\n")
tap_at(hc$cx, hc$cy, "hamburger")
Sys.sleep(0.5)
r2 <- jsonlite::fromJSON(eval_js("var a=document.querySelector('.sidebar a[data-value=\"bloco_1\"]'); var r=a.getBoundingClientRect(); return JSON.stringify({cx:r.left+r.width/2,cy:r.top+r.height/2})"))
tap_at(r2$cx, r2$cy, "bloco_1")
cat("aba (shiny):", eval_js("return window.Shiny ? window.Shiny.shinyapp.$inputValues.abas : 'n/a'"), "\n")
cat("tab-pane ativa:", eval_js("return (document.querySelector('.tab-pane.active')||{id:''}).id"), "\n")
cat("body classes:", eval_js("return document.body.className"), "\n")
shot("t05_apos_tap_bloco_1")

cat("\n== estado do .div-titulo e filtros apos nav ==\n")
cat(eval_js("
  var t = document.querySelector('.tab-pane.active .div-titulo') || document.querySelector('.div-titulo');
  var tr = t ? t.getBoundingClientRect() : null;
  var card = document.querySelector('.tab-pane.active .card') || document.querySelector('.card');
  var cr = card ? card.getBoundingClientRect() : null;
  return JSON.stringify({
    titulo: tr ? {t:Math.round(tr.top),l:Math.round(tr.left),w:Math.round(tr.width),h:Math.round(tr.height), pos:getComputedStyle(t).position} : 'nenhum',
    card: cr ? {t:Math.round(cr.top),l:Math.round(cr.left),w:Math.round(cr.width)} : 'nenhum',
    sidebarAberta: document.body.classList.contains('sidebar-open'),
    contentMarginLeft: getComputedStyle(document.querySelector('.content-wrapper')).marginLeft,
    contentW: document.querySelector('.content-wrapper').getBoundingClientRect().width
  });
"), "\n")
shot("t06_estado")

# touch events
cat("\n== TOUCH no item nivel_1 ==\n")
eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(0.6)
rt <- jsonlite::fromJSON(eval_js("var a=document.querySelector('.sidebar a[data-value=\"nivel_1\"]'); var r=a.getBoundingClientRect(); return JSON.stringify({cx:r.left+r.width/2,cy:r.top+r.height/2})"))
b$Input$dispatchTouchEvent(type = "touchStart", touchPoints = list(list(x = rt$cx, y = rt$cy)))
Sys.sleep(0.05)
b$Input$dispatchTouchEvent(type = "touchEnd", touchPoints = list())
Sys.sleep(0.8)
cat("aba (shiny) apos touch:", eval_js("return window.Shiny ? window.Shiny.shinyapp.$inputValues.abas : 'n/a'"), "\n")
cat("body classes apos touch:", eval_js("return document.body.className"), "\n")

writeLines("done", file.path(outdir, "PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
