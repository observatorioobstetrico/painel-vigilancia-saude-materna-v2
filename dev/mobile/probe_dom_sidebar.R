# Dumps the real sidebar/menu DOM structure for the mobile redesign.
repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stopdom")
unlink(stopfile)
log <- file.path(repo, "dev/mobile/out_dom/app.log")
dir.create(file.path(repo, "dev/mobile/out_dom"), showWarnings = FALSE, recursive = TRUE)

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

cat("== brand-link outerHTML ==\n")
cat(substr(eval_js("return document.querySelector('.main-sidebar .brand-link').outerHTML;"), 1, 600), "\n\n")

cat("== sidebar nav outerHTML (primeiros 3 itens) ==\n")
cat(substr(eval_js("
  var ul = document.querySelector('.main-sidebar .nav-sidebar');
  var out = '';
  for (var i = 0; i < Math.min(3, ul.children.length); i++) {
    out += ul.children[i].outerHTML + '\\n---\\n';
  }
  return out;
"), 1, 2600), "\n\n")

cat("== subitens do nivel 2 (primeiros 2) ==\n")
cat(substr(eval_js("
  var tree = document.querySelector('.main-sidebar .nav-treeview');
  if (!tree) return 'sem treeview';
  var out = '';
  for (var i = 0; i < Math.min(2, tree.children.length); i++) {
    out += tree.children[i].outerHTML + '\\n---\\n';
  }
  return out;
"), 1, 1600), "\n\n")

cat("== classes/estilo do item ativo ==\n")
cat(eval_js("
  var a = document.querySelector('.main-sidebar .nav-link.active');
  if (!a) return 'nenhum ativo';
  var cs = getComputedStyle(a);
  return a.className + ' | bg=' + cs.backgroundColor + ' color=' + cs.color;
"), "\n\n")

cat("== tamanhos de toque ==\n")
cat(eval_js("
  var links = document.querySelectorAll('.main-sidebar .nav-link');
  var el = links[0].getBoundingClientRect();
  return 'primeiro link: h=' + Math.round(el.height) + ' w=' + Math.round(el.width);
"), "\n")

writeLines("done", file.path(repo, "dev/mobile/out_dom/PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
