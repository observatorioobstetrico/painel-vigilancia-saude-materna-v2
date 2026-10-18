# Inspects the reactable container chain widths on mobile.
repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop11")
unlink(stopfile)
log <- file.path(repo, "dev/mobile/out11/app.log")
dir.create(file.path(repo, "dev/mobile/out11"), showWarnings = FALSE, recursive = TRUE)
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
b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(3)
eval_js("$('.main-sidebar a[data-value=\"nivel_3\"]').click(); true")
Sys.sleep(1.5)
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('bloco','bloco1'); true")
Sys.sleep(0.5)
eval_js("Shiny.setInputValue('indicador', 'Porcentagem de nascidos vivos de mães com menos de 4 anos de estudo'); true")
Sys.sleep(0.4)
eval_js("Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); true")
Sys.sleep(7)
cat(eval_js("
  var el = document.querySelector('.ReactTable');
  var out = [];
  while (el && el !== document.body) {
    var r = el.getBoundingClientRect();
    var cs = getComputedStyle(el);
    out.push(el.tagName + '#' + (el.id||'') + '.' + (el.className||'').toString().slice(0, 60) +
      ' | w=' + Math.round(r.width) + ' l=' + Math.round(r.left) +
      ' | display=' + cs.display + ' overflowX=' + cs.overflowX + ' width=' + cs.width + ' maxW=' + cs.maxWidth + ' minW=' + cs.minWidth);
    el = el.parentElement;
  }
  return out.join('\\n');
"), "\n")
writeLines("done", file.path(repo, "dev/mobile/out11/PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
