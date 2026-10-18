# Lists elements that overflow horizontally on mobile.
repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop8")
unlink(stopfile)
log <- file.path(repo, "dev/mobile/out8/app.log")
dir.create(file.path(repo, "dev/mobile/out8"), showWarnings = FALSE, recursive = TRUE)
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

cat("document.scrollWidth:", eval_js("return document.documentElement.scrollWidth"), " body.scrollWidth:", eval_js("return document.body.scrollWidth"), " innerWidth:", eval_js("return window.innerWidth"), "\n")

cat("\n== elementos com right > 391 (sobre) ==\n")
lista <- eval_js("
  var out = [];
  var els = document.querySelectorAll('body *');
  for (var i = 0; i < els.length; i++) {
    var el = els[i];
    var r = el.getBoundingClientRect();
    if (r.width > 0 && r.right > 391 && r.height > 0) {
      var cs = getComputedStyle(el);
      if (cs.position === 'fixed' && r.left >= 390) continue;
      out.push(el.tagName + '.' + (el.className||'').toString().slice(0,60) + ' | w=' + Math.round(r.width) + ' right=' + Math.round(r.right) + ' | ' + (el.innerText||'').toString().slice(0,30).replace(/\\n/g,' '));
    }
  }
  return out.slice(0, 30).join('\\n');
")
cat(lista, "\n")

cat("\n== navega bloco_1 e recheca ==\n")
eval_js("$('.main-sidebar a[data-value=\"bloco_1\"]').click(); true")
Sys.sleep(1.5)
cat("document.scrollWidth:", eval_js("return document.documentElement.scrollWidth"), "\n")
lista2 <- eval_js("
  var out = [];
  var els = document.querySelectorAll('.tab-pane.active *');
  for (var i = 0; i < els.length; i++) {
    var el = els[i];
    var r = el.getBoundingClientRect();
    if (r.width > 0 && r.right > 391 && r.height > 0) {
      var cs = getComputedStyle(el);
      if (cs.position === 'fixed' && r.left >= 390) continue;
      out.push(el.tagName + '.' + (el.className||'').toString().slice(0,60) + ' | w=' + Math.round(r.width) + ' right=' + Math.round(r.right));
    }
  }
  return out.slice(0, 30).join('\\n');
")
cat(lista2, "\n")

cat("\n== doc: iframe ==\n")
eval_js("$('.main-sidebar a[data-value=\"documentacao\"]').click(); true")
Sys.sleep(1.5)
cat(eval_js("
  var f = document.querySelector('.tab-pane.active iframe');
  if (!f) return 'sem iframe';
  var r = f.getBoundingClientRect();
  return 'iframe w=' + Math.round(r.width) + ' h=' + Math.round(r.height) + ' src=' + (f.getAttribute('src')||'').slice(0,60);
"), "\n")
cat(eval_js("
  var els = document.querySelectorAll('.tab-pane.active > *');
  var out = [];
  for (var i = 0; i < els.length; i++) { out.push(els[i].tagName + '.' + (els[i].className||'').toString().slice(0,40)); }
  return out.join(' | ');
"), "\n")
cat("scrollWidth doc:", eval_js("return document.documentElement.scrollWidth"), "\n")

cat("\n== aparecida: card da imagem ==\n")
eval_js("$('.main-sidebar a[data-value=\"aparecida\"]').click(); true")
Sys.sleep(1.2)
cat(eval_js("
  var c = document.querySelector('.tab-pane.active .card2');
  if (!c) return 'sem card2';
  var r = c.getBoundingClientRect();
  var img = c.querySelector('img, image');
  var ri = img ? img.getBoundingClientRect() : null;
  return 'card w=' + Math.round(r.width) + ' h=' + Math.round(r.height) + ' | img w=' + (ri?Math.round(ri.width):'n/a') + ' h=' + (ri?Math.round(ri.height):'n/a');
"), "\n")

writeLines("done", file.path(repo, "dev/mobile/out8/PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
