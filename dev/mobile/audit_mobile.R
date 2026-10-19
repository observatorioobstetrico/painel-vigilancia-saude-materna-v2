# Auditoria mobile: sobe o app, emula um celular e mede/compara TODAS as telas.
# Uso: Rscript dev/mobile/audit_mobile.R <outdir>
args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "dev/mobile/out_audit"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop_audit")
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
  if (as.numeric(Sys.time() - t0, units = "secs") > 120) stop("app did not start")
  Sys.sleep(0.3)
}
cat("SERVER_UP after", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")

library(chromote)
b <- ChromoteSession$new()
on.exit({ try(b$close(), silent = TRUE); cleanup() }, add = TRUE)

try(b$Network$enable(), silent = TRUE)
try(b$Network$setCacheDisabled(cacheDisabled = TRUE), silent = TRUE)

eval_js <- function(js) {
  r <- b$Runtime$evaluate(sprintf("(function(){ %s })()", js))
  if (!is.null(r$exceptionDetails)) {
    return(paste0("JS_ERROR: ", r$exceptionDetails$text))
  }
  r$result$value
}

shot <- function(name) {
  p <- file.path(repo, outdir, paste0(name, ".png"))
  b$screenshot(filename = p, selector = "html", delay = 0.25)
  cat("SHOT", basename(p), "\n")
}

wait_js <- function(expr, timeout = 60, poll = 0.2) {
  t0 <- Sys.time()
  repeat {
    r <- tryCatch(b$Runtime$evaluate(expr), error = function(e) NULL)
    if (!is.null(r) && isTRUE(r$result$value)) return(TRUE)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout) return(FALSE)
    Sys.sleep(poll)
  }
}

metrics <- function() {
  eval_js("
    var vw = window.innerWidth;
    var overflow = document.documentElement.scrollWidth - vw;
    var offenders = [];
    document.querySelectorAll('.content *').forEach(function(el){
      var r = el.getBoundingClientRect();
      if (r.width > vw + 4 && r.height > 0 && getComputedStyle(el).position !== 'fixed') {
        var tag = el.tagName.toLowerCase();
        var id = el.id ? ('#' + el.id) : '';
        var cls = (el.className && typeof el.className === 'string') ? ('.' + el.className.split(' ').slice(0,2).join('.')) : '';
        var txt = (el.textContent||'').trim().slice(0,40);
        offenders.push(tag + id + cls + ' w=' + Math.round(r.width) + ' :: ' + txt);
      }
    });
    // scrolls aninhados verticais (elementos roláveis fora html/body)
    var nested = [];
    document.querySelectorAll('.content *').forEach(function(el){
      if (el.clientHeight > 80 && el.scrollHeight > el.clientHeight + 12) {
        var tag = el.tagName.toLowerCase();
        var id = el.id ? ('#' + el.id) : '';
        var cls = (el.className && typeof el.className === 'string') ? ('.' + el.className.split(' ').slice(0,2).join('.')) : '';
        nested.push(tag + id + cls + ' ' + el.clientHeight + '<' + el.scrollHeight);
      }
    });
    // alvos de toque pequenos (visíveis); para checkbox/radio considera o label
    var small = [];
    document.querySelectorAll('.content button, .content .btn, .content .nav-link, .content a.action-button, .content input[type=checkbox], .content input[type=radio], .content .selectize-input').forEach(function(el){
      var alvo = el;
      if (el.matches('input[type=checkbox], input[type=radio]')) {
        var lab = el.closest('label');
        if (lab) alvo = lab;
      }
      var r = alvo.getBoundingClientRect();
      var st = getComputedStyle(alvo);
      if (st.display === 'none' || st.visibility === 'hidden' || r.height === 0) return;
      if (r.height < 40 || r.width < 40) {
        var id = alvo.id ? ('#' + alvo.id) : '';
        var cls = (alvo.className && typeof alvo.className === 'string') ? ('.' + alvo.className.split(' ').slice(0,2).join('.')) : '';
        small.push(alvo.tagName.toLowerCase() + id + cls + ' ' + Math.round(r.width) + 'x' + Math.round(r.height));
      }
    });
    var fab = document.getElementById('btn-filtros-fab');
    var fabVisible = false;
    if (fab) {
      var fr = fab.getBoundingClientRect();
      fabVisible = fr.height > 0 && fr.width > 0 && getComputedStyle(fab).visibility !== 'hidden';
    }
    return JSON.stringify({
      overflow: overflow,
      offenders: offenders.slice(0, 8),
      nested: nested.length,
      nestedSamples: nested.slice(0, 6),
      small: small.length,
      smallSamples: small.slice(0, 8),
      fab: fabVisible,
      errors: document.querySelectorAll('.shiny-output-error').length
    });
  ")
}

report <- function(nome) {
  m <- metrics()
  cat("\n== TELA", nome, "==\n")
  cat(m, "\n")
  shot(nome)
}

aba <- function(tab) {
  eval_js(sprintf("
    if (!document.body.classList.contains('sidebar-open')) {
      $('[data-widget=\"pushmenu\"]').trigger('click');
    }
    true
  "))
  Sys.sleep(0.6)
  eval_js(sprintf("
    var a = document.querySelector('.main-sidebar a[data-value=\"%s\"]');
    if (a) { a.click(); return true; }
    return false;
  ", tab))
  wait_js(sprintf(
    "document.querySelector('.tab-pane.active') && document.querySelector('.tab-pane.active').id.indexOf('%s') >= 0",
    tab
  ), timeout = 15)
  Sys.sleep(1.2)
}

buscar <- function(espera = 25) {
  eval_js("Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); return true;")
  Sys.sleep(2)
  wait_js("
    Array.from(document.querySelectorAll('.content .load-container')).every(function(el){
      return el.classList.contains('shiny-spinner-hidden') || getComputedStyle(el).display === 'none';
    })
  ", timeout = espera)
  Sys.sleep(1.5)
}

# ---------- MOBILE ----------
b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = TRUE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
wait_js("!!window.Shiny", 90)
Sys.sleep(3)
cat("innerWidth:", eval_js("return window.innerWidth"), "\n")

telas <- c("sobre", "nivel_1", "bloco_1", "bloco_2", "bloco_3", "bloco_4",
           "bloco_5", "bloco_6", "bloco_7", "nivel_3", "documentacao", "aparecida")

for (tab in telas) {
  aba(tab)
  report(paste0("m_", tab))
}

# ---------- BUSCAS (telas representativas) ----------
cat("\n\n===== BUSCAS =====\n")
aba("nivel_1")
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('ano', 2024); true")
buscar(60)
report("m_nivel_1_resultado")

aba("bloco_1")
eval_js("Shiny.setInputValue('nivel','nacional'); true")
buscar(60)
report("m_bloco_1_resultado")

aba("bloco_7")
buscar(60)
report("m_bloco_7_resultado")

aba("nivel_3")
eval_js("Shiny.setInputValue('bloco','bloco1'); true")
Sys.sleep(1)
eval_js("Shiny.setInputValue('indicador','Porcentagem de nascidos vivos de mães com idade de 35 anos ou mais'); true")
buscar(60)
Sys.sleep(6)
report("m_nivel_3_resultado")

# ---------- PAINEL DE FILTROS ----------
cat("\n\n===== PAINEL DE FILTROS =====\n")
aba("bloco_1")
eval_js("document.getElementById('btn-filtros-fab').click(); return true;")
Sys.sleep(0.8)
cat("painel aberto:", eval_js("return document.body.classList.contains('filtro-mobile-aberto');"), "\n")
cat("botao sticky visivel:", eval_js("
  var btn = document.getElementById('pesquisar');
  if (!btn) return 'sem botao';
  var row = btn.closest('.row');
  var r = btn.getBoundingClientRect();
  var st = row ? getComputedStyle(row) : null;
  return JSON.stringify({w: Math.round(r.width), h: Math.round(r.height), rowPosition: st ? st.position : 'n/a', rowBottom: row ? Math.round(row.getBoundingClientRect().bottom) : -1});
"), "\n")
m <- metrics()
cat("metricas painel:", m, "\n")
shot("m_filtros_painel")

# ---------- RESUMO ----------
cat("\n\n===== FIM =====\n")
writeLines("done", file.path(outdir, "PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("AUDIT DONE\n")
