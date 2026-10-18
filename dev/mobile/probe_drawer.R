# Redesigned mobile drawer validation.
repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop13")
unlink(stopfile)
log <- file.path(repo, "dev/mobile/out13/app.log")
dir.create(file.path(repo, "dev/mobile/out13"), showWarnings = FALSE, recursive = TRUE)
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
  p <- file.path(repo, "dev/mobile/out13", paste0(name, ".png"))
  r <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(r$data), p)
  cat("SHOT", p, "\n")
}
b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(3)

cat("barra fundo:", eval_js("return getComputedStyle(document.querySelector('.div-titulo')).backgroundColor"), "| borda:", eval_js("return getComputedStyle(document.querySelector('.div-titulo')).borderBottomColor"), "\n")
shot_view("r1_inicial")

cat("\n-- abre drawer --\n")
eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(1.2)
cat("botao fechar existe:", eval_js("return !!document.querySelector('.btn-sidebar-fechar')"), "\n")
cat("brand visivel:", eval_js("var bl=document.querySelector('.main-sidebar .brand-link'); var r=bl.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),w:Math.round(r.width),h:Math.round(r.height),display:getComputedStyle(bl).display});"), "\n")
cat("badges:", eval_js("
  var out = [];
  document.querySelectorAll('.main-sidebar .menu-numero').forEach(function(e){ out.push(e.textContent); });
  return out.join(',');
"), "\n")
cat("sidebar scroll propria:", eval_js("var s=document.querySelector('.main-sidebar .sidebar'); return JSON.stringify({h:Math.round(s.getBoundingClientRect().height), overflow:getComputedStyle(s).overflowY, scrollH:s.scrollHeight});"), "\n")
shot_view("r2_drawer_aberto")

cat("\n-- fecha pelo botao X --\n")
b$Runtime$evaluate("document.querySelector('.btn-sidebar-fechar').click(); true")
Sys.sleep(1)
cat("fechado:", eval_js("return !document.body.classList.contains('sidebar-open')"), "\n")

cat("\n-- abre de novo e escolhe bloco_4 --\n")
eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(1)
b$Runtime$evaluate("document.querySelector('.main-sidebar a[data-value=\"bloco_4\"]').click(); true")
Sys.sleep(1.5)
cat("aba:", eval_js("return Shiny.shinyapp.$inputValues.abas"), "| fechou:", eval_js("return !document.body.classList.contains('sidebar-open')"), "\n")
shot_view("r3_bloco4")

cat("\n-- abre com item ativo: rola ate ele? --\n")
eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(1.5)
shot_view("r4_drawer_ativo")

cat("\n-- desktop check --\n")
b$Emulation$setDeviceMetricsOverride(width = 1366, height = 900, deviceScaleFactor = 1, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(3)
cat("barra fundo desktop:", eval_js("return getComputedStyle(document.querySelector('.div-titulo')).backgroundColor"), "\n")
cat("botao X display desktop:", eval_js("var b=document.querySelector('.btn-sidebar-fechar'); return b ? getComputedStyle(b).display : 'ausente';"), "\n")
eval_js("$('.main-sidebar a[data-value=\"bloco_1\"]').click(); true")
Sys.sleep(1.5)
shot_view("r5_desktop")

writeLines("done", file.path(repo, "dev/mobile/out13/PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
