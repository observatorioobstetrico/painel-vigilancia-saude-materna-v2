# Igual ao profile_app.R, mas com a UI pre-renderizada/cacheada.
args <- commandArgs(trailingOnly = TRUE)
out <- args[[1]]
startfile <- args[[2]]
stopfile <- args[[3]]

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
options(golem.app.prod = TRUE)

app_ui_orig <- painel.indicadores:::app_ui

ui_cached <- local({
  cache <- new.env(parent = emptyenv())
  function(request) {
    if (!exists("r", cache, inherits = FALSE)) {
      t0 <- Sys.time()
      assign("r", htmltools::renderTags(app_ui_orig(request)), envir = cache)
      cat("UI_CACHE_BUILD", round(as.numeric(Sys.time() - t0, units = "secs"), 3), "s\n")
    }
    r <- get("r", cache, inherits = FALSE)
    htmltools::tagList(htmltools::HTML(r$html), r$dependencies)
  }
})

profiling <- FALSE
poll_stop <- function() {
  if (file.exists(stopfile)) {
    if (profiling) Rprof(NULL)
    shiny::stopApp("stopfile")
  } else {
    if (!profiling && file.exists(startfile)) {
      Rprof(out, interval = 0.01, line.profiling = TRUE)
      profiling <<- TRUE
    }
    later::later(poll_stop, 0.1)
  }
}

app <- shiny::shinyApp(
  ui = ui_cached,
  server = painel.indicadores:::app_server
)

later::later(poll_stop, 0.1)
shiny::runApp(app, port = 8123, host = "127.0.0.1", launch.browser = FALSE, quiet = TRUE)
if (profiling) Rprof(NULL)

s <- summaryRprof(out, lines = "both")
saveRDS(s, paste0(out, ".rds"))
cat("PROFILING DONE\n")
