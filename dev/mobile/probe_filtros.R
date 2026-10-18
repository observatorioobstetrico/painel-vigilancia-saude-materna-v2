# Tests the mobile filter drawer and checks desktop did not regress.
repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop9")
unlink(stopfile)
log <- file.path(repo, "dev/mobile/out9/app.log")
dir.create(file.path(repo, "dev/mobile/out9"), showWarnings = FALSE, recursive = TRUE)

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
shot_view <- function(name) {
  p <- file.path(repo, "dev/mobile/out9", paste0(name, ".png"))
  r <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(r$data), p)
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
b$Emulation$setDeviceMetricsOverride(width = 1366, height = 900, deviceScaleFactor = 1, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
wait_js("!!window.Shiny", 60)
Sys.sleep(3)

cat("\n== DESKTOP ==\n")
eval_js("$('.main-sidebar a[data-value=\"bloco_1\"]').click(); true")
Sys.sleep(1.5)
cat("card filtros visivel (display do wrapper):", eval_js("var w=document.querySelector('.filtros-global-wrapper'); return getComputedStyle(w).display + ' transform=' + getComputedStyle(w).transform; "), "\n")
cat("card posicao:", eval_js("var c=document.querySelector('.filtros-global-wrapper .card'); var r=c.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),l:Math.round(r.left),w:Math.round(r.width),h:Math.round(r.height)});"), "\n")
cat("FAB display desktop:", eval_js("var f=document.getElementById('btn-filtros-fab'); return f ? getComputedStyle(f).display : 'ausente';"), "\n")
cat("close-header display desktop:", eval_js("var f=document.querySelector('.filtros-mobile-close-header'); return f ? getComputedStyle(f).display : 'ausente';"), "\n")
shot_view("d1_desktop_filtros")

# ---------- MOBILE ----------
cat("\n== MOBILE ==\n")
b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
wait_js("!!window.Shiny", 60)
Sys.sleep(3)

eval_js("$('.main-sidebar a[data-value=\"bloco_1\"]').click(); true")
Sys.sleep(1.5)
shot_view("m1_filtros_card_no_fluxo_ou_fora")

cat("painel escondido (transform):", eval_js("var w=document.querySelector('.filtros-global-wrapper'); return getComputedStyle(w).transform;"), "\n")
cat("FAB display mobile:", eval_js("var f=document.getElementById('btn-filtros-fab'); return f ? getComputedStyle(f).display : 'ausente';"), "\n")

cat("\n-- toca o FAB --\n")
fab <- jsonlite::fromJSON(eval_js("var f=document.getElementById('btn-filtros-fab'); var r=f.getBoundingClientRect(); return JSON.stringify({cx:r.left+r.width/2, cy:r.top+r.height/2});"))
b$Input$dispatchMouseEvent(type = "mousePressed", x = fab$cx, y = fab$cy, button = "left", clickCount = 1L)
b$Input$dispatchMouseEvent(type = "mouseReleased", x = fab$cx, y = fab$cy, button = "left", clickCount = 1L)
Sys.sleep(0.8)
cat("body filtro-mobile-aberto:", eval_js("return document.body.classList.contains('filtro-mobile-aberto')"), "\n")
cat("painel transform:", eval_js("var w=document.querySelector('.filtros-global-wrapper'); return getComputedStyle(w).transform;"), "\n")
shot_view("m2_painel_aberto")

cat("\n-- clica em 'Atualizar resultados' dentro do painel --\n")
eval_js("Shiny.setInputValue('nivel','nacional'); true")
Sys.sleep(0.5)
b$Runtime$evaluate("document.getElementById('pesquisar').click(); true")
Sys.sleep(1.2)
cat("body ainda aberto?", eval_js("return document.body.classList.contains('filtro-mobile-aberto')"), "\n")
shot_view("m3_apos_buscar")

cat("\n-- reabre e fecha pelo backdrop --\n")
b$Runtime$evaluate("document.getElementById('btn-filtros-fab').click(); true")
Sys.sleep(0.6)
cat("aberto:", eval_js("return document.body.classList.contains('filtro-mobile-aberto')"), "\n")
b$Input$dispatchMouseEvent(type = "mousePressed", x = 20, y = 400, button = "left", clickCount = 1L)
b$Input$dispatchMouseEvent(type = "mouseReleased", x = 20, y = 400, button = "left", clickCount = 1L)
Sys.sleep(0.6)
cat("fechou pelo backdrop:", eval_js("return !document.body.classList.contains('filtro-mobile-aberto')"), "\n")
shot_view("m4_apos_backdrop")

cat("\n-- FAB ausente no 'sobre' --\n")
eval_js("$('.main-sidebar a[data-value=\"sobre\"]').click(); true")
Sys.sleep(1.2)
cat("FAB visivel agora:", eval_js("var f=document.getElementById('btn-filtros-fab'); if(!f) return 'ausente'; var r=f.getBoundingClientRect(); return r.width > 0 && r.height > 0;"), "\n")

cat("\n-- nivel 3: FAB presente e painel abre --\n")
eval_js("$('.main-sidebar a[data-value=\"nivel_3\"]').click(); true")
Sys.sleep(1.5)
cat("FAB presente na nivel_3:", eval_js("var f=document.getElementById('btn-filtros-fab'); if(!f) return 'ausente'; var r=f.getBoundingClientRect(); return r.width > 0;"), "\n")
b$Runtime$evaluate("document.getElementById('btn-filtros-fab').click(); true")
Sys.sleep(0.8)
shot_view("m5_nivel3_painel")

writeLines("done", file.path(repo, "dev/mobile/out9/PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
