# Drives a scenario against the app at 127.0.0.1:8123 via chromote.
# Usage: Rscript dev/perf/drive.R <scenario> <profile_out> <stopfile>
args <- commandArgs(trailingOnly = TRUE)
scenario <- args[[1]]
out <- args[[2]]
stopfile <- args[[3]]

repo <- normalizePath(".")
abspath <- function(p) if (grepl("^[A-Za-z]:|^/", p)) p else file.path(repo, p)
stopfile <- abspath(stopfile)
out <- abspath(out)
startfile <- paste0(out, ".start")

log <- paste0(out, ".app.log")
unlink(c(stopfile, startfile))
appscript <- Sys.getenv("PERF_APP_SCRIPT", "dev/perf/profile_app.R")
px <- processx::process$new(
  "Rscript", c(appscript, out, startfile, stopfile),
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
  if (as.numeric(Sys.time() - t0, units = "secs") > 60) stop("app did not start")
  Sys.sleep(0.3)
}
cat("SERVER_UP after", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")

library(chromote)
b <- ChromoteSession$new()
on.exit({ try(b$close(), silent = TRUE); cleanup() }, add = TRUE)

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

b$Runtime$evaluate(paste(
  "window.__prof = {busy: false, last_busy: 0, last_idle: 0};",
  "$(document).on('shiny:busy', function(){window.__prof.busy=true; window.__prof.last_busy=performance.now();});",
  "$(document).on('shiny:idle', function(){window.__prof.busy=false; window.__prof.last_idle=performance.now();});",
  "window.__set = function(id, val){ var el = document.getElementById(id); if (el && el.selectize) { el.selectize.setValue(val); } else if (window.Shiny) { Shiny.setInputValue(id, val, {priority:'event'}); } };",
  "window.__search = function(){ window.__prof.last_idle = 0; Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); };",
  "window.__subtab = function(txt){ $('.card-header .nav-link').each(function(){ if(this.innerText.toLowerCase().indexOf(txt) > -1) this.click(); }); };",
  "true"
))

wait_js("!!window.Shiny", 60)
wait_js("window.__prof.last_idle > 0 && !window.__prof.busy", 60)
cat(sprintf("INITIAL_IDLE after %.2f s\n", as.numeric(Sys.time() - t0, units = "secs")))

Sys.sleep(1)
file.create(startfile)
Sys.sleep(0.2)

click_search <- function(label = "") {
  b$Runtime$evaluate("window.__search();")
  t0 <- Sys.time()
  repeat {
    r <- b$Runtime$evaluate("window.__prof.last_idle > 0 && !window.__prof.busy")
    if (isTRUE(r$result$value)) break
    if (as.numeric(Sys.time() - t0, units = "secs") > 30) {
      cat("TIMEOUT", label, "\n")
      st <- b$Runtime$evaluate("(function(){ try { return JSON.stringify(window.__prof); } catch(e) { return 'ERR ' + e.message; } })()")
      cat("STATE:", st$result$value, "\n")
      break
    }
    Sys.sleep(0.1)
  }
  el <- as.numeric(Sys.time() - t0, units = "secs")
  cat(sprintf("WALL %-44s %6.2f s\n", label, el))
  invisible(el)
}

switch_tab <- function(tab) {
  b$Runtime$evaluate(sprintf("$('.sidebar a[data-value=\"%s\"]').click();", tab))
  Sys.sleep(1.0)
}

set_sel <- function(id, val) {
  b$Runtime$evaluate(sprintf("window.__set('%s', %s);", id, jsonlite::toJSON(val, auto_unbox = TRUE)))
  Sys.sleep(0.15)
}

sub_tab <- function(txt) {
  b$Runtime$evaluate(sprintf("window.__subtab(%s);", jsonlite::toJSON(txt, auto_unbox = TRUE)))
  Sys.sleep(1.0)
}

# ---- scenarios --------------------------------------------------------------
if (scenario == "boot") {
  Sys.sleep(2)
  st <- b$Runtime$evaluate("JSON.stringify({ttfb: performance.timing.responseStart - performance.timing.navigationStart, dcl: performance.timing.domContentLoadedEventEnd - performance.timing.navigationStart, load: performance.timing.loadEventEnd - performance.timing.navigationStart, nodes: document.getElementsByTagName('*').length, html_kb: Math.round(document.documentElement.outerHTML.length/1024), selects: document.querySelectorAll('select').length, selectized: document.querySelectorAll('.selectized').length, inputs: document.querySelectorAll('.shiny-input-container').length, hc: document.querySelectorAll('.highcharts-container').length})")
  cat("TIMINGS:", st$result$value, "\n")
}

if (scenario == "boot2") {
  # segunda carga de pagina no mesmo processo (UI cacheada no servidor)
  b$Page$navigate("http://127.0.0.1:8123")
  invisible(b$Page$loadEventFired())
  Sys.sleep(3)
  st <- b$Runtime$evaluate("JSON.stringify({ttfb: performance.timing.responseStart - performance.timing.navigationStart, dcl: performance.timing.domContentLoadedEventEnd - performance.timing.navigationStart, load: performance.timing.loadEventEnd - performance.timing.navigationStart})")
  cat("TIMINGS_2A_CARGA:", st$result$value, "\n")
}

if (scenario == "hidden") {
  set_sel("nivel", "nacional")
  set_sel("ano", 2024)
  click_search("search while all module tabs hidden")
}

if (scenario == "nivel1_probe") {
  log_app <- paste0(out, ".app.log")
  if (identical(Sys.getenv("PANEL_PREAQUECE_NIVEL1", "1"), "1")) {
    t_pre <- Sys.time()
    repeat {
      pronto <- file.exists(log_app) && any(grepl(
        "PREAQUECE_NIVEL1 concluido",
        suppressWarnings(readLines(log_app, warn = FALSE))
      ))
      if (isTRUE(pronto)) break
      if (as.numeric(Sys.time() - t_pre, units = "secs") > 60) {
        cat("PREWARM_TIMEOUT\n")
        break
      }
      Sys.sleep(0.2)
    }
    cat(sprintf("PREWARM_WAIT %6.2f s\n", as.numeric(Sys.time() - t_pre, units = "secs")))
  }

  t0 <- Sys.time()
  b$Runtime$evaluate("$('.sidebar a[data-value=\"nivel_1\"]').click();")
  for (i in 1:40) {
    Sys.sleep(0.25)
    st <- b$Runtime$evaluate("JSON.stringify({ativos: document.querySelectorAll('.tab-pane.active .caixa-indicador').length, total: document.querySelectorAll('.caixa-indicador').length, hc: document.querySelectorAll('.highcharts-container').length, busy: window.__prof.busy})")
    cat(sprintf("%6.2f %s\n", as.numeric(Sys.time() - t0, units = "secs"), st$result$value))
  }
  caixas_txt <- b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.tab-pane.active .caixa-indicador')).map(e => e.innerText))")
  writeLines(caixas_txt$result$value, paste0(out, ".caixas.json"))
}

if (scenario == "nivel1_first") {
  log_app <- paste0(out, ".app.log")

  # Quando o pre-aquecimento esta ligado, espera o marcador no log para medir
  # apenas a primeira renderizacao (nao a leitura dos parquets em paralelo).
  if (identical(Sys.getenv("PANEL_PREAQUECE_NIVEL1", "1"), "1")) {
    t_pre <- Sys.time()
    repeat {
      pronto <- file.exists(log_app) && any(grepl(
        "PREAQUECE_NIVEL1 concluido",
        suppressWarnings(readLines(log_app, warn = FALSE))
      ))
      if (isTRUE(pronto)) break
      if (as.numeric(Sys.time() - t_pre, units = "secs") > 60) {
        cat("PREWARM_TIMEOUT\n")
        break
      }
      Sys.sleep(0.2)
    }
    cat(sprintf("PREWARM_WAIT %6.2f s\n", as.numeric(Sys.time() - t_pre, units = "secs")))
  }

  b$Runtime$evaluate("window.__prof.last_idle = 0; window.__prof.busy = false; true")
  t0 <- Sys.time()
  b$Runtime$evaluate("$('.sidebar a[data-value=\"nivel_1\"]').click();")

  repeat {
    st <- b$Runtime$evaluate("JSON.stringify({busy: window.__prof.busy, idle: window.__prof.last_idle, caixas: document.querySelectorAll('.tab-pane.active .caixa-indicador').length})")
    info <- jsonlite::fromJSON(st$result$value)
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")
    if (!isTRUE(info$busy) && (info$idle > 0 || (elapsed > 2 && info$caixas > 0))) break
    if (elapsed > 60) {
      cat("TIMEOUT nivel1_first\n")
      break
    }
    Sys.sleep(0.1)
  }
  cat(sprintf("WALL %-44s %6.2f s\n", "primeira renderizacao nivel1", as.numeric(Sys.time() - t0, units = "secs")))
  Sys.sleep(1)
  st <- b$Runtime$evaluate("JSON.stringify({hc: document.querySelectorAll('.highcharts-container').length, erros: document.querySelectorAll('.shiny-output-error').length, caixas: document.querySelectorAll('.tab-pane.active .caixa-indicador').length})")
  cat("CHECK_NIVEL1_FIRST:", st$result$value, "\n")

  click_search("nivel1 busca repetida")
  Sys.sleep(1)
  st2 <- b$Runtime$evaluate("JSON.stringify({hc: document.querySelectorAll('.highcharts-container').length, erros: document.querySelectorAll('.shiny-output-error').length, caixas: document.querySelectorAll('.tab-pane.active .caixa-indicador').length})")
  cat("CHECK_NIVEL1_SECOND:", st2$result$value, "\n")
}

if (scenario == "nivel1") {
  switch_tab("nivel_1")
  set_sel("nivel", "nacional")
  set_sel("ano", 2024)
  Sys.sleep(0.5)
  click_search("nivel1 nacional 2024")
  Sys.sleep(2)
  st <- b$Runtime$evaluate("JSON.stringify({hc: document.querySelectorAll('.highcharts-container').length, erros: document.querySelectorAll('.shiny-output-error').length})")
  cat("CHECK:", st$result$value, "\n")
}

if (scenario == "nivel1_estadual") {
  switch_tab("nivel_1")
  set_sel("nivel", "estadual")
  Sys.sleep(0.3)
  set_sel("estado", "São Paulo")
  Sys.sleep(0.3)
  click_search("nivel1 estadual SP")
  Sys.sleep(2)
  st <- b$Runtime$evaluate("JSON.stringify({hc: document.querySelectorAll('.highcharts-container').length, erros: document.querySelectorAll('.shiny-output-error').length, caixas: document.querySelectorAll('.tab-pane.active .caixa-indicador').length})")
  cat("CHECK_NIVEL1_ESTADUAL:", st$result$value, "\n")
}

if (scenario == "bloco1") {
  switch_tab("bloco_1")
  set_sel("nivel", "nacional")
  set_sel("bloco", "bloco1")
  Sys.sleep(0.5)
  click_search("bloco1 nacional")
}

if (scenario == "bloco4") {
  switch_tab("bloco_4")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco4")
  set_sel("tipo_do_indicador_blocos4_6_7", "robson")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador_blocos4_6_7', 'Porcentagem de cesarianas');")
  Sys.sleep(0.3)
  click_search("bloco4 robson estadual SP")
}

if (scenario == "bloco7") {
  switch_tab("bloco_7")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco7")
  set_sel("tipo_do_indicador_blocos4_6_7", "neonatal")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador_blocos4_6_7', 'Número de óbitos neonatais');")
  Sys.sleep(0.3)
  sub_tab("neonatal")
  click_search("bloco7 neonatal search")
  click_search("bloco7 neonatal repeat (2nd)")
}

if (scenario == "bloco7_fetal") {
  switch_tab("bloco_7")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco7")
  set_sel("tipo_do_indicador_blocos4_6_7", "fetal")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador_blocos4_6_7', 'Número de óbitos fetais');")
  Sys.sleep(0.3)
  sub_tab("fetal")
  click_search("bloco7 fetal search")
}

if (scenario == "nivel3") {
  switch_tab("nivel_3")
  set_sel("nivel", "nacional")
  set_sel("bloco", "bloco1")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador', 'Porcentagem de nascidos vivos de mães com menos de 4 anos de estudo');")
  Sys.sleep(0.3)
  click_search("nivel3 bloco1 escolaridade nacional")
}

if (scenario == "nivel3_b3") {
  switch_tab("nivel_3")
  set_sel("nivel", "nacional")
  set_sel("bloco", "bloco3")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador', 'Porcentagem de mulheres com oito ou mais consultas de pré-natal');")
  Sys.sleep(0.3)
  click_search("nivel3 bloco3 pre-natal nacional")
}

if (scenario == "bloco2") {
  switch_tab("bloco_2")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco2")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador', 'Porcentagem de mulheres com mais de 3 partos anteriores');")
  Sys.sleep(0.3)
  click_search("bloco2 estadual SP")
}

if (scenario == "bloco3") {
  switch_tab("bloco_3")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco3")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador', 'Cobertura de assistência pré-natal');")
  Sys.sleep(0.3)
  click_search("bloco3 estadual SP")
}

if (scenario == "bloco5") {
  switch_tab("bloco_5")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco5")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador', 'Porcentagem de nascidos vivos a termo precoce');")
  Sys.sleep(0.3)
  click_search("bloco5 estadual SP")
}

if (scenario == "bloco6") {
  switch_tab("bloco_6")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco6")
  set_sel("tipo_do_indicador_blocos4_6_7", "mortalidade")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador_blocos4_6_7', 'Razão de mortalidade materna');")
  Sys.sleep(0.3)
  click_search("bloco6 estadual SP")
}

if (scenario == "bloco4_desloc") {
  switch_tab("bloco_4")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco4")
  set_sel("tipo_do_indicador_blocos4_6_7", "deslocamento")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador_blocos4_6_7', 'Porcentagem de nascidos vivos segundo local de ocorrência do parto');")
  Sys.sleep(0.3)
  click_search("bloco4 deslocamento estadual SP")
}

if (scenario == "bloco4_prof") {
  switch_tab("bloco_4")
  set_sel("nivel", "estadual")
  set_sel("estado", "São Paulo")
  set_sel("bloco", "bloco4")
  set_sel("tipo_do_indicador_blocos4_6_7", "profissional")
  Sys.sleep(0.5)
  b$Runtime$evaluate("window.__set('indicador_blocos4_6_7', 'Porcentagem de nascidos vivos de partos ocorridos em hospital');")
  Sys.sleep(0.3)
  click_search("bloco4 profissional estadual SP")
}

if (scenario == "duas_sessoes") {
  b2 <- ChromoteSession$new()
  on.exit(try(b2$close(), silent = TRUE), add = TRUE)
  b2$Page$navigate("http://127.0.0.1:8123")
  invisible(b2$Page$loadEventFired())
  Sys.sleep(2)

  eval2 <- function(js) invisible(b2$Runtime$evaluate(js))
  eval2("window.__busy = false; $(document).on('shiny:busy', function(){window.__busy=true}); $(document).on('shiny:idle', function(){window.__busy=false; window.__idle=(window.__idle||0)+1}); window.__set = function(id, val){ var el = document.getElementById(id); if (el && el.selectize) { el.selectize.setValue(val); } else { Shiny.setInputValue(id, val, {priority:'event'}); } }; true")

  # sessao 1: nivel 1
  switch_tab("nivel_1")
  Sys.sleep(1)
  dbg1 <- b$Runtime$evaluate("JSON.stringify({aba_ativa: $('.sidebar a.active').attr('data-value'), abas: Shiny.shinyapp.$inputValues.abas, selects_visiveis: $('#ano:visible').length})")
  cat("DEBUG_S1:", dbg1$result$value, "\n")
  set_sel("nivel", "nacional")
  set_sel("ano", 2024)
  click_search("sessao1 nivel1")
  Sys.sleep(4)
  chk1 <- b$Runtime$evaluate("JSON.stringify({hc: document.querySelectorAll('.highcharts-container').length, erros: document.querySelectorAll('.shiny-output-error').length, tem_caixa: document.body.innerHTML.indexOf('caixa_b1') > -1, tem_titulo1: document.body.innerHTML.indexOf('nivel_1_1-titulo') > -1, abas_bodies: document.querySelectorAll('.tab-pane').length, active_pane: (document.querySelector('.tab-pane.active')||{id:''}).id})")

  # sessao 2: bloco 5
  eval2("$('.sidebar a[data-value=\"bloco_5\"]').click();")
  Sys.sleep(1)
  eval2("window.__set('nivel', 'estadual'); window.__set('estado', 'São Paulo'); window.__set('bloco', 'bloco5');")
  Sys.sleep(0.5)
  eval2("window.__set('indicador', 'Porcentagem de nascidos vivos a termo precoce');")
  Sys.sleep(0.3)
  eval2("Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'});")
  Sys.sleep(6)
  chk2 <- b2$Runtime$evaluate("JSON.stringify({hc: document.querySelectorAll('.highcharts-container').length, erros: document.querySelectorAll('.shiny-output-error').length})")

  # sessao 1 continua funcionando apos a 2 (reativa a aba antes de medir)
  try(b$parent$Target$activateTarget(targetId = b$get_target_id()), silent = TRUE)
  Sys.sleep(2)
  click_search("sessao1 nivel1 de novo")
  Sys.sleep(3)
  chk1b <- b$Runtime$evaluate("JSON.stringify({hc: document.querySelectorAll('.highcharts-container').length, erros: document.querySelectorAll('.shiny-output-error').length, visivel: document.visibilityState, titulo: (document.getElementById('nivel_1_1-titulo_localidade')||{innerText:''}).innerText})")

  cat("CHECK_S1:", chk1$result$value, "\n")
  cat("CHECK_S2:", chk2$result$value, "\n")
  cat("CHECK_S1b:", chk1b$result$value, "\n")
}

Sys.sleep(2)
st <- b$Runtime$evaluate("JSON.stringify({hc: document.querySelectorAll('.highcharts-container').length, erros: document.querySelectorAll('.shiny-output-error').length, validacoes: document.querySelectorAll('.shiny-output-error-validation').length})")
cat("CHECK_FINAL:", st$result$value, "\n")

writeLines("done", stopfile)
px$wait(timeout = 60000)
Sys.sleep(0.5)
cat("PROFILE_FILE", out, "\n")
