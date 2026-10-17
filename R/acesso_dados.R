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

.onLoad <- function(libname, pkgname) {
  registra_bindings_dados(pkgname)
  invisible()
}
