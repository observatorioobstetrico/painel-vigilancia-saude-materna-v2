# Starts the app; profiles between startfile appearance and stopfile appearance.
# Usage: Rscript dev/perf/profile_app.R <out> <startfile> <stopfile>
args <- commandArgs(trailingOnly = TRUE)
out <- args[[1]]
startfile <- args[[2]]
stopfile <- args[[3]]

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
options(golem.app.prod = TRUE)

if (nzchar(Sys.getenv("PERF_TRACE_CRIA"))) {
  contador <- new.env(parent = emptyenv())
  contador$n <- 0L
  contador$t <- 0
  contador$t0 <- 0
  trace(painel.indicadores:::cria_indicadores,
    tracer = function() {
      contador$n <- contador$n + 1L
      assign("t0", proc.time()[["elapsed"]], envir = contador)
    },
    exit = function() {
      contador$t <- contador$t + proc.time()[["elapsed"]] - contador$t0
    },
    print = FALSE,
    where = asNamespace("painel.indicadores")
  )
  imprime_trace <- function() cat("TRACE_CRIA n=", contador$n, " t=", round(contador$t, 3), "s\n")
} else {
  imprime_trace <- function() invisible(NULL)
}

if (nzchar(Sys.getenv("PERF_TRACE_COMP"))) {
  trace(
    "processa_bloco7",
    tracer = quote(if (isTRUE(comp)) cat("COMP_BLOCO7\n")),
    print = FALSE,
    where = asNamespace("painel.indicadores")
  )
  trace(
    "processa_causas",
    tracer = quote(if (isTRUE(comp)) cat("COMP_CAUSAS\n")),
    print = FALSE,
    where = asNamespace("painel.indicadores")
  )
}

profiling <- FALSE
poll_stop <- function() {
  if (file.exists(stopfile)) {
    if (profiling) Rprof(NULL)
    imprime_trace()
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
  ui = painel.indicadores:::app_ui,
  server = painel.indicadores:::app_server
)

later::later(poll_stop, 0.1)
shiny::runApp(app, port = 8123, host = "127.0.0.1", launch.browser = FALSE, quiet = TRUE)
if (profiling) Rprof(NULL)

s <- summaryRprof(out, lines = "both")
saveRDS(s, paste0(out, ".rds"))
cat("PROFILING DONE\n")
