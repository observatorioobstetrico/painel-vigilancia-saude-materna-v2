# Prints which CSS rules match .div-titulo
repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop7")
unlink(stopfile)
log <- file.path(repo, "dev/mobile/out7/app.log")
dir.create(file.path(repo, "dev/mobile/out7"), showWarnings = FALSE, recursive = TRUE)
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

b$Emulation$setDeviceMetricsOverride(width = 1400, height = 900, deviceScaleFactor = 1, mobile = FALSE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(3)

cat("innerWidth:", eval_js("return window.innerWidth"), "\n")
cat("folhas de estilo:", eval_js("return document.styleSheets.length"), "\n")
rules <- eval_js("
  var out = [];
  for (var i = 0; i < document.styleSheets.length; i++) {
    var sheet = document.styleSheets[i];
    var rules;
    try { rules = sheet.cssRules; } catch (e) { out.push('sheet ' + i + ' inacessivel'); continue; }
    if (!rules) { out.push('sheet ' + i + ' sem rules'); continue; }
    for (var j = 0; j < rules.length; j++) {
      var r = rules[j];
      if (r.cssText && r.cssText.indexOf('.div-titulo') > -1) {
        out.push('sheet[' + i + '] (' + (sheet.href || 'inline') + '): ' + r.cssText.slice(0, 200));
      }
    }
  }
  return out.join('\\n');
")
cat(rules, "\n")
cat("computed .div-titulo:", eval_js("return getComputedStyle(document.querySelector('.div-titulo')).position"), "\n")
cat("inline style tag contem sticky?:", eval_js("return document.documentElement.outerHTML.indexOf('position: sticky') > -1"), "\n")
cat("inline style tag contem 'position: fixed' perto de div-titulo?:", eval_js("var h=document.documentElement.outerHTML; var i=h.indexOf('.div-titulo'); return i < 0 ? 'nao achou div-titulo' : h.slice(i-100, i+200);"), "\n")

writeLines("done", file.path(repo, "dev/mobile/out7/PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
