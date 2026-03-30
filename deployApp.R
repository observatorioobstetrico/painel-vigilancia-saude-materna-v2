# Deploy the ShinyApp

## Se no console aparecer "The following required packages are not installed: ..."
## Digite 1 e aperte ENTER

rsconnect::deployApp(
  appDir = ".",
  appName = "painel-vigilancia-saude-materna-v2",
  account = "observatorioobstetrico",
  server = "shinyapps.io",
  appFiles = c(
    "app.R",
    "report.Rmd",
    "DESCRIPTION",
    "NAMESPACE",
    "README.md",
    "dev/",
    "man/",
    "inst/",
    "R/",
    "data/"
  ),
  # lint = FALSE,
  forceUpdate = TRUE
  )

