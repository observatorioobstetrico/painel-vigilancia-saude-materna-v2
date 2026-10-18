# Checks AdminLTE sidebar overlay presence + brand structure.
# Usage: Rscript dev/mobile/probe_overlay.R
repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop5")
unlink(stopfile)
log <- file.path(repo, "dev/mobile/out5/app.log")
dir.create(file.path(repo, "dev/mobile/out5"), showWarnings = FALSE, recursive = TRUE)
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
eval_js <- function(js) {
  r <- b$Runtime$evaluate(sprintf("(function(){ %s })()", js))
  r$result$value
}
b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = TRUE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(3)

cat("overlay #sidebar-overlay existe:", eval_js("return !!document.getElementById('sidebar-overlay')"), "\n")
cat("overlay display (fechada):", eval_js("var o=document.getElementById('sidebar-overlay'); return o ? getComputedStyle(o).display : 'n/a';\n"), "\n")
cat("header outerHTML (500 chars):\n", substr(eval_js("return document.querySelector('.main-header').outerHTML;"), 1, 500), "\n")
cat("brand-link do header existe:", eval_js("return !!document.querySelector('.main-header .brand-link')"), "\n")
cat("brand-link do sidebar existe:", eval_js("return !!document.querySelector('.main-sidebar .brand-link')"), "\n")

cat("\n-- abrindo sidebar --\n")
eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(1)
cat("overlay display (aberta):", eval_js("var o=document.getElementById('sidebar-overlay'); return o ? getComputedStyle(o).display : 'n/a';\n"), "\n")
cat("overlay rect:", eval_js("var o=document.getElementById('sidebar-overlay'); if(!o) return 'n/a'; var r=o.getBoundingClientRect(); return JSON.stringify({t:r.top,l:r.left,w:r.width,h:r.height,z:getComputedStyle(o).zIndex});"), "\n")
cat("sidebar rect:", eval_js("var s=document.querySelector('.main-sidebar'); var r=s.getBoundingClientRect(); return JSON.stringify({l:Math.round(r.left),w:Math.round(r.width),z:getComputedStyle(s).zIndex});"), "\n")

cat("\n-- clicando no overlay em x=380 (fora da sidebar de 500? nao existe) --\n")
cat("nota: viewport 512, sidebar 500 -> 'fora' = 6px\n")
b$Input$dispatchMouseEvent(type = "mousePressed", x = 508, y = 400, button = "left", clickCount = 1L)
b$Input$dispatchMouseEvent(type = "mouseReleased", x = 508, y = 400, button = "left", clickCount = 1L)
Sys.sleep(0.8)
cat("body apos clique overlay:", eval_js("return document.body.className"), "\n")

cat("\n-- teste com viewport 360 (sidebar 500 > tela) --\n")
b$Emulation$setDeviceMetricsOverride(width = 360, height = 780, deviceScaleFactor = 3, mobile = TRUE)
Sys.sleep(1)
cat("innerWidth:", eval_js("return window.innerWidth"), "\n")
eval_js("if(!document.body.classList.contains('sidebar-open')) $('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(0.8)
cat("sidebar w:", eval_js("return Math.round(document.querySelector('.main-sidebar').getBoundingClientRect().width)"), " body:", eval_js("return document.body.className"), "\n")
cat("hamburger pontos:", eval_js("var h=document.querySelector('[data-widget=\"pushmenu\"]'); var r=h.getBoundingClientRect(); return JSON.stringify({cx:r.left+r.width/2,cy:r.top+r.height/2});"), "\n")
cat("elementFromPoint no hamburger:", eval_js("var h=document.querySelector('[data-widget=\"pushmenu\"]'); var r=h.getBoundingClientRect(); var el=document.elementFromPoint(r.left+r.width/2, r.top+r.height/2); return el ? el.tagName + '.' + (el.className||'').toString().slice(0,50) : 'null';"), "\n")
cat("brand-link visivel cobre hamburger?:", eval_js("var bl=document.querySelector('.main-sidebar .brand-link'); if(!bl) return 'sem brand'; var r=bl.getBoundingClientRect(); return JSON.stringify({l:r.left,w:r.width,z:getComputedStyle(bl).zIndex, pos:getComputedStyle(bl).position});"), "\n")

writeLines("done", file.path(repo, "dev/mobile/out5/PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
