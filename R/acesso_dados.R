# Acesso aos dados do painel ------------------------------------------------
#
# As bases volumosas (>= ~0.3 MB, ver inst/app/dados) sao armazenadas em
# parquet e carregadas sob demanda: o binding ativo criado em .onLoad()
# le o arquivo apenas na primeira vez que o objeto e acessado e guarda o
# resultado em cache na propria sessao R. Ja as bases pequenas continuam
# em data/ (data.frame disponibilizado pelo mecanismo de lazy-data).
#
# Para regerar os arquivos: ver data-raw/utils_parquet.R

.dados_parquet_cache <- new.env(parent = emptyenv())

#' Caminho de um arquivo parquet de dados
#' @noRd
caminho_dados_parquet <- function(nome) {
  app_sys("app", "dados", paste0(nome, ".parquet"))
}

#' Carrega (com cache) uma base armazenada em parquet
#' @noRd
carrega_dados_parquet <- function(nome) {
  if (!exists(nome, envir = .dados_parquet_cache, inherits = FALSE)) {
    caminho <- caminho_dados_parquet(nome)
    if (!nzchar(caminho) || !file.exists(caminho)) {
      stop(
        "Base de dados '", nome, ".parquet' não encontrada em inst/app/dados. ",
        "Rode o pipeline de atualização (data-raw).",
        call. = FALSE
      )
    }
    assign(
      nome,
      as.data.frame(arrow::read_parquet(caminho)),
      envir = .dados_parquet_cache
    )
  }
  get(nome, envir = .dados_parquet_cache, inherits = FALSE)
}

#' Lista as bases disponiveis em inst/app/dados
#' @noRd
dados_parquet_disponiveis <- function() {
  diretorio <- app_sys("app", "dados")
  if (!dir.exists(diretorio)) {
    return(character())
  }
  sub("\\.parquet$", "", list.files(diretorio, pattern = "\\.parquet$"))
}

.dados_projetados_cache <- new.env(parent = emptyenv())

#' Carrega apenas as colunas necessarias de uma base parquet (com cache)
#'
#' Usado para bases muito largas (ex.: bloco7_distribuicao_cids, 1095 colunas),
#' onde ler a tabela inteira seria custoso. Colunas inexistentes sao ignoradas.
#' @noRd
carrega_dados_projetado <- function(nome, colunas) {
  caminho <- caminho_dados_parquet(nome)
  if (!nzchar(caminho) || !file.exists(caminho)) {
    stop(
      "Base de dados '", nome, ".parquet' não encontrada em inst/app/dados. ",
      "Rode o pipeline de atualização (data-raw).",
      call. = FALSE
    )
  }
  ds <- arrow::open_dataset(caminho)
  colunas <- intersect(colunas, ds$schema$names)
  chave <- paste(nome, paste(sort(colunas), collapse = "\u001f"), sep = "\u001e")
  if (!exists(chave, envir = .dados_projetados_cache, inherits = FALSE)) {
    x <- ds |>
      dplyr::select(dplyr::all_of(colunas)) |>
      dplyr::collect()
    assign(chave, as.data.frame(x), envir = .dados_projetados_cache)
  }
  get(chave, envir = .dados_projetados_cache, inherits = FALSE)
}

#' Cria bindings ativos para cada base parquet no namespace do pacote
#'
#' Assim todo o codigo existente que referencia as bases pelo nome
#' (ex.: `bloco1`) continua funcionando, porem o carregamento passa a
#' ser preguicoso e em cache.
#' @noRd
registra_bindings_dados <- function(pkg) {
  ns <- asNamespace(pkg)
  for (nome in dados_parquet_disponiveis()) {
    local({
      nm <- nome
      if (!exists(nm, envir = ns, inherits = FALSE)) {
        makeActiveBinding(nm, function() carrega_dados_parquet(nm), ns)
      }
    })
  }
  invisible(NULL)
}

# Bases lidas pelo Nivel 1 (home). O primeiro acesso a cada uma delas paga a
# leitura do parquet; como o cache e por processo, pre-aquecemos em segundo
# plano para que o primeiro clique no Nivel 1 encontre tudo em memoria.
.parquets_nivel_1 <- c(
  "bloco1",
  "bloco2",
  "bloco3",
  "bloco4",
  "bloco4_deslocamento_muni",
  "bloco4_deslocamento_macrorregiao",
  "bloco4_deslocamento_uf",
  "bloco4_profissional",
  "bloco5",
  "bloco6",
  "bloco7",
  "bloco7_fetal",
  "bloco7_perinatal",
  "bloco7_neonatal",
  "bloco7_morbidade_neonatal",
  "bloco7_dist_morbidade",
  "bloco7_evitaveis_fetal",
  "bloco7_evitaveis_perinatal",
  "bloco7_evitaveis_neonatal",
  "bloco7_principais_fetal",
  "bloco7_principais_perinatal",
  "bloco7_principais_neonatal",
  "base_incompletude",
  "base_cobertura_muni_2015_2021",
  "base_cobertura_uf_regioes_2015_2021",
  "base_referencia_baixo_peso",
  "malformacao",
  "rmm_corrigida"
)

.dados_preaquecimento <- new.env(parent = emptyenv())

#' Pre-aquece em segundo plano os parquets usados pelo Nivel 1
#'
#' Carrega um arquivo por callback do `later`, cedendo o event loop entre eles
#' para nao bloquear as sessoes que ja estao interagindo com o app.
#'
#' @noRd
preaquece_dados_nivel_1 <- function(nomes = .parquets_nivel_1, pausa = 0.02) {
  inicio <- proc.time()[["elapsed"]]
  i <- 0L

  proximo <- function() {
    i <<- i + 1L
    if (i > length(nomes)) {
      if (identical(Sys.getenv("PANEL_PREAQUECE_NIVEL1_DEBUG"), "1")) {
        cat(sprintf(
          "PREAQUECE_NIVEL1 concluido: %d arquivos em %.2f s\n",
          length(nomes),
          proc.time()[["elapsed"]] - inicio
        ))
      }
      return(invisible(NULL))
    }

    try(carrega_dados_parquet(nomes[[i]]), silent = TRUE)
    later::later(proximo, pausa)
  }

  proximo()
  invisible(NULL)
}

#' Agenda o pre-aquecimento uma unica vez por processo
#'
#' @noRd
agenda_preaquecimento_nivel_1 <- function(atraso = 0.2) {
  if (!identical(Sys.getenv("PANEL_PREAQUECE_NIVEL1", "1"), "1")) {
    return(invisible(NULL))
  }
  if (isTRUE(.dados_preaquecimento$agendado)) {
    return(invisible(NULL))
  }

  .dados_preaquecimento$agendado <- TRUE
  later::later(function() preaquece_dados_nivel_1(), atraso)
  invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
  registra_bindings_dados(pkgname)
  invisible()
}
