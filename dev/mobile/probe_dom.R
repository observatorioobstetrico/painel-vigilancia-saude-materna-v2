# Dumps DOM details of navbar/sidebar/brand for mobile layout design.
# Usage: Rscript dev/mobile/probe_dom.R <outdir>
args <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(args) >= 1) args[[1]] else "dev/mobile/out4"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

repo <- normalizePath(".")
stopfile <- file.path(repo, "dev/mobile/.stop4")
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

b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 3, mobile = TRUE)
b$Page$navigate("http://127.0.0.1:8123")
invisible(b$Page$loadEventFired())
Sys.sleep(3)

cat("== estrutura do wrapper ==\n")
cat(eval_js("
  function resumo(el) {
    if (!el) return 'ausente';
    var cs = getComputedStyle(el);
    var r = el.getBoundingClientRect();
    return el.tagName + '.' + (el.className||'').toString().slice(0,70) +
      ' t=' + Math.round(r.top) + ' l=' + Math.round(r.left) + ' w=' + Math.round(r.width) + ' h=' + Math.round(r.height) +
      ' z=' + cs.zIndex + ' pos=' + cs.position + ' disp=' + cs.display;
  }
  var out = [];
  out.push('body>div.wrapper: ' + resumo(document.querySelector('body > div.wrapper')));
  var wrapper = document.querySelector('body > div.wrapper');
  for (var i = 0; i < wrapper.children.length; i++) {
    out.push('  wrapper.child[' + i + ']: ' + resumo(wrapper.children[i]));
  }
  var header = document.querySelector('.main-header');
  out.push('main-header children:');
  for (var i = 0; i < header.children.length; i++) {
    out.push('  header.child[' + i + ']: ' + resumo(header.children[i]));
  }
  var sidebar = document.querySelector('.main-sidebar');
  out.push('sidebar children:');
  for (var i = 0; i < sidebar.children.length; i++) {
    out.push('  sidebar.child[' + i + ']: ' + resumo(sidebar.children[i]));
  }
  var brand = document.querySelector('.main-header .brand-link') || document.querySelector('.brand-link');
  out.push('primeiro .brand-link: ' + resumo(brand));
  out.push('margens sidebar fechada: ' + getComputedStyle(sidebar).marginLeft + ' / transition: ' + getComputedStyle(sidebar).transition.slice(0,60));
  out.push('body.scrollWidth=' + document.body.scrollWidth + ' innerWidth=' + window.innerWidth);
  var push = document.querySelector('[data-widget=\"pushmenu\"]');
  out.push('pushmenu parent: ' + resumo(push.parentElement) + ' | html: ' + push.outerHTML.slice(0, 200));
  return out.join('\\n');
"), "\n")

cat("\n== hamburger z-index chain ==\n")
cat(eval_js("
  var el = document.querySelector('[data-widget=\"pushmenu\"]');
  var out = [];
  while (el) {
    var cs = getComputedStyle(el);
    out.push(el.tagName + '.' + (el.className||'').toString().slice(0,50) + ' z=' + cs.zIndex + ' pos=' + cs.position);
    el = el.parentElement;
  }
  return out.join(' -> ');
"), "\n")

cat("\n== abrindo sidebar ==\n")
eval_js("$('[data-widget=\"pushmenu\"]').click(); true")
Sys.sleep(1)
cat("body:", eval_js("return document.body.className"), "\n")
cat("sidebar rect:", eval_js("var s=document.querySelector('.main-sidebar'); var r=s.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),l:Math.round(r.left),w:Math.round(r.width),h:Math.round(r.height),r:Math.round(r.right)});"), "\n")
cat("brand-link rect:", eval_js("var s=document.querySelector('.brand-link'); var r=s.getBoundingClientRect(); return JSON.stringify({t:Math.round(r.top),l:Math.round(r.left),w:Math.round(r.width),h:Math.round(r.height)});"), "\n")
cat("overlay/backdrop existente:", eval_js("return document.querySelectorAll('.sidebar-overlay, .overlay, [class*=backdrop]').length"), "\n")
cat(eval_js("
  var overlay = document.createElement('div');
  overlay.id = '__probe_overlay';
  overlay.style.cssText = 'position:fixed;top:300px;left:0;width:100%;height:200px;background:rgba(255,0,0,.15);z-index:99999;';
  document.body.appendChild(overlay);
  true
"), "\n")

cat("\n== elementos no canto superior esquerdo (onde fica o hamburger) ==\n")
cat(eval_js("
  var els = document.elementsFromPoint(31, 28);
  return els.map(function(e){ return e.tagName + '.' + (e.className||'').toString().slice(0,50); }).join(' > ');
"), "\n")

cat("\n== tentando fechar sidebar clicando fora (fora da sidebar) ==\n")
b$Input$dispatchMouseEvent(type = "mousePressed", x = 495, y = 600, button = "left", clickCount = 1L)
Sys.sleep(0.05)
b$Input$dispatchMouseEvent(type = "mouseReleased", x = 495, y = 600, button = "left", clickCount = 1L)
Sys.sleep(0.8)
cat("body apos clique fora:", eval_js("return document.body.className"), "\n")

cat("\n== link do brand aponta para: ==\n")
cat(eval_js("var a=document.querySelector('.main-header .brand-link') || document.querySelector('.brand-link'); return a ? a.outerHTML.slice(0,300) : 'ausente';"), "\n")

writeLines("done", file.path(outdir, "PROBE_OK"))
cleanup()
Sys.sleep(0.5)
cat("PROBE DONE\n")
