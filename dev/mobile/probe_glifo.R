# Checks how the numbered icons render (::before content).
repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop14")
unlink(stopfile)
log <- file.path(repo, "dev/mobile/out14/app.log")
dir.create(file.path(repo, "dev/mobile/out14"), showWarnings = FALSE, recursive = TRUE)
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
b$Emulation$setDeviceMetricsOverride(width = 1366, height = 900, deviceScaleFactor = 1, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(3)

cat("antes do JS? content do ::before do fa-1:",
  eval_js("
    var el = document.querySelector('.main-sidebar .nav-treeview i.fas');
    if (!el) return 'nao achou';
    var cs = getComputedStyle(el, '::before');
    return JSON.stringify({content: cs.content, fontFamily: cs.fontFamily, className: el.className, textContent: el.textContent});
  "), "\n")

writeLines("done", file.path(repo, "dev/mobile/out14/PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
