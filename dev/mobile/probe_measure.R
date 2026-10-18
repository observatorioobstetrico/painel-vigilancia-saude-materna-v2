# Measures the layout structure (scroll container, paddings) on desktop and mobile.
# Usage: Rscript dev/mobile/probe_measure.R <outdir>
args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "dev/mobile/out3"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop3")
unlink(stopfile)
log <- file.path(repo, outdir, "app.log")
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
shot <- function(name) {
  p <- file.path(repo, outdir, paste0(name, ".png"))
  b$screenshot(filename = p, selector = "html", delay = 0.2)
  cat("SHOT", p, "\n")
}

b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(3)

cat("== DESKTOP (992x1323 default) ==\n")
cat("innerWidth:", eval_js("return window.innerWidth"), "\n")
cat(eval_js("
  var sel = ['.content-wrapper', '.content-wrapper > .content', '.tab-content', '.tab-pane.active', '.div-titulo'];
  var out = [];
  sel.forEach(function(s){
    var e = document.querySelector(s);
    if (!e) { out.push(s + ': ausente'); return; }
    var cs = getComputedStyle(e);
    out.push(s + ': rect=' + JSON.stringify({t:Math.round(e.getBoundingClientRect().top),l:Math.round(e.getBoundingClientRect().left),w:Math.round(e.getBoundingClientRect().width)}) +
      ' pad=' + cs.padding + ' marg=' + cs.margin + ' overflow=' + cs.overflow + ' pos=' + cs.position + ' z=' + cs.zIndex);
  });
  return out.join('\\n');
"), "\n")

eval_js("$('.sidebar a[data-value=\"bloco_1\"]').click(); true")
Sys.sleep(2)
eval_js("Shiny.setInputValue('nivel','nacional'); Shiny.setInputValue('bloco','bloco1'); Shiny.setInputValue('pesquisar', Date.now(), {priority:'event'}); true")
Sys.sleep(4)
shot("desktop_bloco1")

# sticky experiment: inject style and check if it sticks
eval_js("
  var st = document.createElement('style');
  st.textContent = '.div-titulo { position: sticky !important; top: 57px; z-index: 101; background: #EAF5FF; }';
  document.head.appendChild(st);
  true
")
Sys.sleep(0.5)
eval_js("window.scrollTo(0, 900); true")
Sys.sleep(0.5)
cat("sticky test scrollY:", eval_js("return window.scrollY"), "\n")
cat("titulo rect pos scroll:", eval_js("var t=document.querySelector('.tab-pane.active .div-titulo'); var r=t.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),h:Math.round(r.height)});"), "\n")
shot("desktop_sticky_teste")
eval_js("window.scrollTo(0, 0); true")

writeLines("done", file.path(outdir, "PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
