# Extrai a expressão `nome <- <expr>` do arquivo (procura recursivamente),
# avalia no env dado e devolve o valor.
extrai_calcs <- function(arquivo, nome, env = parent.frame()) {
  expr <- parse(arquivo)
  achado <- NULL
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(nome))) {
        achado <<- e[[3]]
        return(invisible(TRUE))
      }
      for (i in seq_along(e)) if (!is.null(achado)) return(invisible(TRUE)) else walk(e[[i]])
    } else if (is.pairlist(e) || is.expression(e)) {
      for (i in seq_along(e)) if (!is.null(achado)) return(invisible(TRUE)) else walk(e[[i]])
    }
    invisible(FALSE)
  }
  for (e in expr) { walk(e); if (!is.null(achado)) break }
  if (is.null(achado)) stop("não encontrei ", nome, " em ", arquivo)
  eval(achado, envir = env)
}

filtros_padrao <- function(nivel = "estadual", estado = "São Paulo", ...) {
  base <- list(
    ano = 2024,
    ano2 = c(2012L, 2024L),
    nivel = nivel,
    regiao = "", estado = estado, macro = "", micro = "", municipio = "",
    estado_macro = "", estado_micro = "", estado_municipio = "",
    comparar = "Não",
    nivel2 = "", regiao2 = "", estado2 = "", macro2 = "", micro2 = "",
    municipio2 = "", estado_macro2 = "", estado_micro2 = "", estado_municipio2 = "",
    bloco = "bloco4", indicador = "", pesquisar = 1,
    mostrar_referencia = "nao_mostrar_referencia",
    tipo_do_indicador_blocos4_6_7 = "robson",
    indicador_blocos4_6_7 = "Porcentagem de cesarianas",
    indicador_uma_caixinha_adicional_bloco5 = "",
    indicador_uma_caixinha_adicional_bloco7 = "",
    indicador_duas_caixinhas_adicionais1 = "",
    indicador_duas_caixinhas_adicionais2 = ""
  )
  mods <- list(...)
  base[names(mods)] <- mods
  base
}

tempo <- function(rotulo, expr, rep = 3L) {
  expr <- substitute(expr)
  env <- parent.frame()
  ts <- numeric(rep)
  valor <- NULL
  for (i in seq_len(rep)) {
    t0 <- proc.time()[["elapsed"]]
    valor <- eval(expr, env)
    ts[i] <- proc.time()[["elapsed"]] - t0
  }
  cat(sprintf("%-52s min=%7.3f  med=%7.3f  (n=%d)\n", rotulo, min(ts), stats::median(ts), rep))
  invisible(valor)
}
