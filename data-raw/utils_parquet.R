# Utilitários para gravar bases de dados --------------------------------
#
# As bases volumosas do painel (>= ~0.3 MB) ficam em inst/app/dados/*.parquet
# e são carregadas sob demanda pelo app (ver R/acesso_dados.R). Ao atualizar
# os dados no pipeline, use usar_dados_parquet(objeto) no lugar de
# usethis::use_data(objeto) para essas bases grandes. As bases pequenas
# (choices, tabelas auxiliares) continuam em data/ via usethis::use_data().

usar_dados_parquet <- function(x, overwrite = TRUE) {
  nome <- deparse(substitute(x))
  destino <- file.path("inst", "app", "dados")
  dir.create(destino, showWarnings = FALSE, recursive = TRUE)
  arquivo <- file.path(destino, paste0(nome, ".parquet"))
  if (!overwrite && file.exists(arquivo)) {
    stop("Arquivo já existe: ", arquivo, call. = FALSE)
  }
  arrow::write_parquet(
    as.data.frame(x),
    arquivo,
    compression = "zstd",
    compression_level = 5
  )
  rda <- file.path("data", paste0(nome, ".rda"))
  if (file.exists(rda)) {
    file.remove(rda)
  }
  invisible(arquivo)
}

converter_rda_em_parquet <- function(nomes) {
  destino <- file.path("inst", "app", "dados")
  dir.create(destino, showWarnings = FALSE, recursive = TRUE)
  for (nome in nomes) {
    rda <- file.path("data", paste0(nome, ".rda"))
    if (!file.exists(rda)) {
      warning("Não encontrei ", rda, " - pulando.", call. = FALSE)
      next
    }
    env <- new.env(parent = emptyenv())
    load(rda, envir = env)
    arrow::write_parquet(
      as.data.frame(get(nome, envir = env)),
      file.path(destino, paste0(nome, ".parquet")),
      compression = "zstd",
      compression_level = 5
    )
    file.remove(rda)
  }
  invisible(NULL)
}
