# Starts the app on 127.0.0.1:8123 for interactive/mobile probing.
# Usage: Rscript dev/mobile/serve_app.R <stopfile>
args <- commandArgs(trailingOnly = TRUE)
stopfile <- if (length(args) >= 1) args[[1]] else "dev/mobile/.stop"

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
options(golem.app.prod = TRUE)

poll_stop <- function() {
  if (file.exists(stopfile)) {
    shiny::stopApp("stopfile")
  } else {
    later::later(poll_stop, 0.1)
  }
}
later::later(poll_stop, 0.1)

app <- shiny::shinyApp(
  ui = painel.indicadores:::app_ui,
  server = painel.indicadores:::app_server
)

shiny::runApp(app, port = 8123, host = "127.0.0.1", launch.browser = FALSE, quiet = TRUE)
