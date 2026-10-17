# Cache interno em memoria para resultados intermediarios caros -------------
#
# Usado para evitar refazer filtros/agregacoes quando a mesma base e os mesmos
# filtros passam por aqui repetidas vezes (troca de abas, buscas repetidas,
# modulos diferentes). A chave e montada pelos chamadores com o endereco de
# memoria da base + os campos de filtro relevantes (ver funcoes_globais.R).
#
# A capacidade e limitada por numero de itens e por uma estimativa de bytes
# (nao usamos utils::object.size, que e lento para tabelas grandes).

.cache_interno <- new.env(parent = emptyenv())
.cache_interno_controle <- new.env(parent = emptyenv())
.cache_interno_controle$seq <- 0L
.cache_interno_max_itens <- 48L
.cache_interno_max_bytes <- 350 * 1024^2

# A chave e um hash (curto, de tamanho fixo) dos componentes. Nunca usamos a
# descricao completa como nome de variavel: o R limita nomes a 10000 bytes e
# tabelas largas (ex.: 206 colunas) estouravam esse limite.
chave_cache_interno <- function(...) {
  rlang::hash(list(...))
}

estima_tamanho <- function(x) {
  if (is.data.frame(x)) {
    return(as.numeric(nrow(x)) * as.numeric(ncol(x)) * 8)
  }
  if (is.list(x)) {
    return(sum(vapply(x, estima_tamanho, numeric(1))))
  }
  1024
}

cache_interno_buscar <- function(chave) {
  if (exists(chave, envir = .cache_interno, inherits = FALSE)) {
    get(chave, envir = .cache_interno, inherits = FALSE)$valor
  } else {
    NULL
  }
}

cache_interno_guardar <- function(chave, valor) {
  if (exists(chave, envir = .cache_interno, inherits = FALSE)) {
    return(invisible(valor))
  }
  tamanho <- estima_tamanho(valor)
  # Objetos muito grandes nao compensam: ocupariam o cache inteiro e seriam
  # despejados logo em seguida (caso da tabela de 1095 colunas filtrada).
  if (tamanho > .cache_interno_max_bytes / 2) {
    return(invisible(valor))
  }

  .cache_interno_controle$seq <- .cache_interno_controle$seq + 1L
  assign(
    chave,
    list(valor = valor, tamanho = tamanho, ordem = .cache_interno_controle$seq),
    envir = .cache_interno
  )

  entradas <- ls(.cache_interno)
  total <- sum(vapply(entradas, function(k) {
    get(k, envir = .cache_interno, inherits = FALSE)$tamanho
  }, numeric(1)))

  while (length(entradas) > .cache_interno_max_itens || total > .cache_interno_max_bytes) {
    ordens <- vapply(entradas, function(k) {
      get(k, envir = .cache_interno, inherits = FALSE)$ordem
    }, numeric(1))
    mais_antiga <- entradas[which.min(ordens)]
    total <- total - get(mais_antiga, envir = .cache_interno, inherits = FALSE)$tamanho
    rm(list = mais_antiga, envir = .cache_interno)
    entradas <- setdiff(entradas, mais_antiga)
  }
  invisible(valor)
}

# Identidade da base para a chave: endereco de memoria + dimensoes + nomes.
# Os chamadores passam apenas objetos estaveis (bases vindas dos parquets);
# os guardas evitam colisoes por reuso de endereco apos o GC.
identidade_base <- function(data) {
  chave_cache_interno(
    rlang::obj_address(data),
    nrow(data),
    ncol(data),
    names(data)
  )
}

# Campos de `filtros` que afetam os filtros de localidade/periodo
rotulo_filtros <- function(filtros) {
  chave_cache_interno(
    filtros$nivel, filtros$regiao, filtros$estado, filtros$macro, filtros$micro, filtros$municipio,
    filtros$estado_macro, filtros$estado_micro, filtros$estado_municipio,
    filtros$comparar, filtros$mostrar_referencia,
    filtros$nivel2, filtros$regiao2, filtros$estado2, filtros$macro2, filtros$micro2, filtros$municipio2,
    filtros$estado_macro2, filtros$estado_micro2, filtros$estado_municipio2,
    filtros$ano, filtros$ano2
  )
}

# Versao de `filtros` sem o campo volatil `pesquisar` (timestamp do clique),
# para uso como chave de shiny::bindCache.
filtros_sem_pesquisar <- function(filtros) {
  filtros$pesquisar <- NULL
  filtros
}
