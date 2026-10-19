# Validação visual das correções: sobre, sidebar, nivel 1 (layout), caixas e desktop.
args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "dev/mobile/out_fix"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop_fix")
unlink(stopfile)

px <- processx::process$new(
  "Rscript", c("dev/mobile/serve_app.R", stopfile),
  wd = repo, stdout = file.path(outdir, "app.log"), stderr = file.path(outdir, "app.log"), cleanup = TRUE
)
cleanup <- function() {
  try(px$kill_tree(), silent = TRUE)
  try(unlink(stopfile), silent = TRUE)
  invisible(NULL)
}
on.exit(cleanup())

t0 <- Sys.time()
repeat {
  if (!px$is_alive()) stop("app died")
  con <- try(suppressWarnings(socketConnection("127.0.0.1", 8123, open = "r", timeout = 1)), silent = TRUE)
  if (!inherits(con, "try-error")) { close(con); break }
  if (as.numeric(Sys.time() - t0, units = "secs") > 120) stop("sem app")
  Sys.sleep(0.3)
}

library(chromote)
b <- ChromoteSession$new()
on.exit({ try(b$close(), silent = TRUE); cleanup() }, add = TRUE)

eval_js <- function(js) {
  r <- b$Runtime$evaluate(sprintf("(function(){ %s })()", js))
  if (!is.null(r$exceptionDetails)) return(paste0("JS_ERROR: ", r$exceptionDetails$text))
  r$result$value
}

wait_js <- function(expr, timeout = 90, poll = 0.2) {
  t0 <- Sys.time()
  repeat {
    r <- tryCatch(b$Runtime$evaluate(expr), error = function(e) NULL)
    if (!is.null(r) && isTRUE(r$result$value)) return(TRUE)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) return(FALSE)
    Sys.sleep(poll)
  }
}

shot <- function(name) {
  p <- file.path(repo, outdir, paste0(name, ".png"))
  b$screenshot(filename = p, selector = "html", delay = 0.25)
  cat("SHOT", basename(p), "\n")
}

abrir <- function(tab) {
  eval_js("if (!document.body.classList.contains('sidebar-open')) { $('[data-widget=\"pushmenu\"]').trigger('click'); } return true;")
  Sys.sleep(0.5)
  eval_js(sprintf("var a=document.querySelector('.main-sidebar a[data-value=\"%s\"]'); if(a){a.click(); return true;} return false;", tab))
  Sys.sleep(1.6)
}

buscar <- function() {
  eval_js("Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); return true;")
  Sys.sleep(2.5)
  wait_js("Array.from(document.querySelectorAll('.content .load-container')).every(function(el){return el.classList.contains('shiny-spinner-hidden')||getComputedStyle(el).display==='none';})", 90)
  Sys.sleep(2.5)
}

largura <- function() {
  cat("innerWidth:", eval_js("return window.innerWidth"),
      "| docEl.scrollWidth:", eval_js("return document.documentElement.scrollWidth"),
      "| scale:", eval_js("return window.visualViewport ? window.visualViewport.scale : 'n/a'"), "\n")
}

# ---------------- MOBILE ----------------
b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = TRUE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
wait_js("!!window.Shiny")
Sys.sleep(3)

cat("=== SOBRE (logos) ===\n")
largura()
shot("m_sobre")
cat("altura das imagens:", eval_js("
  var imgs = [].slice.call(document.querySelectorAll('.sobre-page .inner img'));
  return imgs.map(function(i){ return Math.round(i.getBoundingClientRect().height) + 'x' + Math.round(i.getBoundingClientRect().width); }).join(' | ');
"), "\n")
cat("h3 font-size:", eval_js("var h=document.querySelector('.sobre-page h3'); return h ? getComputedStyle(h).fontSize : 'n/a';"), "\n")

cat("\n=== SIDEBAR ABERTA ===\n")
eval_js("$('[data-widget=\"pushmenu\"]').trigger('click'); return true;")
Sys.sleep(0.8)
shot("m_sidebar")
cat("itens (icon x / texto x):", eval_js("
  var itens = [].slice.call(document.querySelectorAll('.main-sidebar .nav-link'));
  return itens.slice(0, 6).map(function(a){
    var ic = a.querySelector('.nav-icon, i');
    var p = a.querySelector('p');
    if (!ic || !p) return 'sem icone/p';
    var ri = ic.getBoundingClientRect(), rp = p.getBoundingClientRect();
    return (Math.round(ri.width) + 'x' + Math.round(ri.height)) + ' / ' + (Math.round(rp.width) + 'x' + Math.round(rp.height)) + ' mesmoTopo=' + (Math.abs(ri.top - rp.top) < 8);
  }).join(' | ');
"), "\n")
cat("ativo (visual):", eval_js("
  var a = document.querySelector('.main-sidebar .nav-link.active');
  if (!a) return 'nenhum';
  var st = getComputedStyle(a);
  return JSON.stringify({cor: st.color, fundo: st.backgroundColor, peso: st.fontWeight, sombra: st.boxShadow, raio: st.borderRadius});
"), "\n")
eval_js("$('[data-widget=\"pushmenu\"]').trigger('click'); return true;")
Sys.sleep(0.6)

cat("\n=== NIVEL 1: largura do layout apos busca ===\n")
abrir("nivel_1")
largura()
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('ano', 2024); return true;")
buscar()
largura()
cat("tabs:", eval_js("
  ['nivel_1_1-tabset1','nivel_1_1-tabset2','nivel_1_1-tabset3'].map(function(id){
    var el = document.getElementById(id);
    if (!el) return id + ': n/a';
    return id + ' w=' + Math.round(el.getBoundingClientRect().width) + ' sw=' + el.scrollWidth + ' ovx=' + getComputedStyle(el).overflowX;
  }).join(' | ');
"), "\n")
shot("m_nivel1")
b$screenshot(filename = file.path(repo, outdir, "m_nivel1_viewport.png"), delay = 0.2)

cat("\n=== BLOCO 1: caixas ===\n")
abrir("bloco_1")
eval_js("Shiny.setInputValue('nivel','nacional'); return true;")
buscar()
largura()
cat("caixas:", eval_js("
  var k = document.querySelectorAll('.caixa-indicador');
  if (!k.length) return 'nenhuma';
  var a = k[0];
  var secs = [].slice.call(a.children).map(function(c){ return c.className.split(' ')[0] + ':' + Math.round(c.getBoundingClientRect().height); });
  return 'n=' + k.length + ' altura=' + Math.round(a.getBoundingClientRect().height) + ' secoes=' + secs.join(',');
"), "\n")
shot("m_bloco1")

cat("\n=== BLOCO 7 (fetal): caixas ===\n")
abrir("bloco_7")
buscar()
largura()
cat("caixas:", eval_js("
  var k = document.querySelectorAll('.caixa-indicador');
  if (!k.length) return 'nenhuma';
  var a = k[0];
  var secs = [].slice.call(a.children).map(function(c){ return c.className.split(' ')[0] + ':' + Math.round(c.getBoundingClientRect().height); });
  return 'n=' + k.length + ' altura=' + Math.round(a.getBoundingClientRect().height) + ' secoes=' + secs.join(',');
"), "\n")
shot("m_bloco7")

# ---------------- DESKTOP ----------------
b$Emulation$setDeviceMetricsOverride(width = 1400, height = 900, deviceScaleFactor = 1, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
wait_js("!!window.Shiny")
Sys.sleep(3)

cat("\n=== DESKTOP ===\n")
cat("reduced-motion ativo:", eval_js("return window.matchMedia('(prefers-reduced-motion: reduce)').matches"), "\n")
cat("transicao sidebar:", eval_js("var s=document.querySelector('.main-sidebar'); return s ? getComputedStyle(s).transition : 'n/a';"), "\n")
cat("transicao brand-link:", eval_js("var s=document.querySelector('.brand-link'); return s ? getComputedStyle(s).transition : 'n/a';"), "\n")

abrir("bloco_1")
eval_js("Shiny.setInputValue('nivel','nacional'); return true;")
buscar()
shot("d_bloco1")
cat("caixa desktop:", eval_js("
  var a = document.querySelector('.caixa-indicador');
  if (!a) return 'nenhuma';
  var secs = [].slice.call(a.children).map(function(c){ return c.className.split(' ')[0] + ':' + Math.round(c.getBoundingClientRect().height); });
  return 'altura=' + Math.round(a.getBoundingClientRect().height) + ' secoes=' + secs.join(',');
"), "\n")

abrir("sobre")
shot("d_sobre")

cleanup()
cat("PROBE FIX DONE\n")
