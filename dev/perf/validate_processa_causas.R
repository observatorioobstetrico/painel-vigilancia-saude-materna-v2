# Valida processa_causas (novo: soma por matriz/rowsum) contra a versao antiga
# (data.table agrupado). Roda para as 3 bases, escopos local/referencia/comp.
suppressMessages(pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE))
ns <- asNamespace("painel.indicadores")
source("dev/perf/bench_helpers.R")
cache_env <- get(".cache_interno", envir = ns)

# ------- copia da versao antiga (data.table agrupado) -------------------------
filtra_localidade_dt_old <- function(dt, filtros, comp = FALSE, referencia = FALSE, localidade_resumo = "escolha1", add_class = TRUE, nivel_atual = "nivel_2") {
  sufixo <- ifelse(comp == TRUE | localidade_resumo == "escolha2", "2", "")
  filtro_de <- function(var) filtros[[paste0(var, sufixo)]]
  if (nivel_atual == "nivel_1") {
    dt <- dt[ano == filtros$ano]
  } else if (nivel_atual == "nivel_2") {
    dt <- dt[ano >= filtros$ano2[1] & ano <= filtros$ano2[2]]
  } else {
    dt <- dt[TRUE]
  }
  if (referencia) {
    dt <- data.table::copy(dt)
    dt[, class := "Brasil (valor de referência)"]
    return(dt)
  }
  nivel_selecionado <- filtro_de("nivel")
  dt <- switch(
    nivel_selecionado,
    "nacional" = dt,
    "regional" = dt[regiao == filtro_de("regiao")],
    "estadual" = dt[uf == filtro_de("estado")],
    "macro" = dt[macro_r_saude == filtro_de("macro") & uf == filtro_de("estado_macro")],
    "micro" = dt[r_saude == filtro_de("micro") & uf == filtro_de("estado_micro")],
    "municipal" = dt[municipio == filtro_de("municipio") & uf == filtro_de("estado_municipio")],
    "municipios_semelhantes" = dt[grupo_kmeans == tabela_aux_municipios$grupo_kmeans[which(tabela_aux_municipios$municipio == filtro_de("municipio") & tabela_aux_municipios$uf == filtro_de("estado_municipio"))]]
  )
  if (add_class) {
    dt[, class := data.table::fcase(
      nivel_selecionado == "nacional", valor_ou_na(if (filtros$comparar == "Não") "Brasil (valor de referência)" else if (filtros$mostrar_referencia == "nao_mostrar_referencia") "Brasil" else "Brasil (valor de referência)"),
      nivel_selecionado == "regional", valor_ou_na(filtro_de("regiao")),
      nivel_selecionado == "estadual", valor_ou_na(filtro_de("estado")),
      nivel_selecionado == "macro", valor_ou_na(filtro_de("macro")),
      nivel_selecionado == "micro", valor_ou_na(filtro_de("micro")),
      nivel_selecionado == "municipal", valor_ou_na(filtro_de("municipio")),
      nivel_selecionado == "municipios_semelhantes", "Média dos municípios semelhantes",
      default = NA_character_
    )]
  }
  dt
}
valor_ou_na_old <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]
valor_ou_na <- valor_ou_na_old

agrega_colunas_evitaveis_old <- function(data, filtros, pesos, momentos, comp = FALSE, referencia = FALSE, localidade_resumo = "escolha1", prefixo_coluna = "evitaveis_fetal", nivel_atual = "nivel_2") {
  dt <- data.table::as.data.table(data)
  todas_fases <- length(momentos) == 3
  pesos_pat   <- if (!is.null(pesos)) paste(pesos, collapse = "|") else FALSE
  momentos_pat <- if (!is.null(momentos)) paste(momentos, collapse = "|") else FALSE
  alvo_cols <- names(dt)[
    grepl(prefixo_coluna, names(dt)) &
      grepl(pesos_pat,    names(dt)) &
      (todas_fases | grepl(momentos_pat, names(dt)))
  ]
  dt <- filtra_localidade_dt_old(dt, filtros, comp, referencia, localidade_resumo, nivel_atual = nivel_atual)
  if (any(dt[["class"]] == "Brasil (valor de referência)")) {
    dt <- data.table::copy(dt)
    dt[class == "Brasil (valor de referência)", class := "Brasil"]
  }
  soma <- function(x, by) {
    x[, lapply(.SD, sum, na.rm = TRUE), by = by, .SDcols = alvo_cols]
  }
  agg_graficos <- soma(dt, c("ano", "class"))
  agg_resumo <- soma(agg_graficos, "class")
  para_long <- function(agg, agrupadores) {
    agg[, total := rowSums(.SD), .SDcols = alvo_cols]
    agg[, (alvo_cols) := lapply(.SD, \(x) x / total * 100), .SDcols = alvo_cols]
    data.table::melt(agg, id.vars = agrupadores, measure.vars = alvo_cols,
                     variable.name = "grupo_cid10", value.name = "porc_obitos")
  }
  list(
    graficos = para_long(agg_graficos, c("ano", "class")),
    resumo = para_long(agg_resumo, "class")
  )
}

# ------- versao antiga de processa_causas_sem_cache ---------------------------
processa_causas_old <- function(data, pesos, momentos, filtros, comp = FALSE, referencia = FALSE, grupos = NULL, localidade_resumo = "escolha1", prefixo_coluna = "evitaveis_fetal", nivel_atual = "nivel_2") {
  agregado <- agrega_colunas_evitaveis_old(data, filtros, pesos, momentos, comp, referencia, localidade_resumo, prefixo_coluna, nivel_atual)
  dt_graficos <- data.table::copy(data.table::as.data.table(agregado$graficos))
  dt_resumo <- data.table::copy(data.table::as.data.table(agregado$resumo))
  padrao <- if (!is.null(grupos)) paste(grupos, collapse = "|") else FALSE
  dt_graficos[, grupo_cid10 := as.character(grupo_cid10)]
  dt_graficos[, grupo_cid10 := data.table::fifelse(grepl(padrao, grupo_cid10), grupo_cid10, "Total de causas não selecionadas")]
  dt_resumo[, grupo_cid10 := as.character(grupo_cid10)]
  dt_resumo[, grupo_cid10 := data.table::fifelse(grepl(padrao, grupo_cid10), grupo_cid10, "Total de causas não selecionadas")]
  renomeia_grupos <- function(dt, prefixo_coluna) {
    dt[, grupo_cid10 := as.character(grupo_cid10)]
    dt[is.na(grupo_cid10), grupo_cid10 := ""]
    if (startsWith(prefixo_coluna, "evitaveis")) {
      dt[, grupo_cid10 := data.table::fcase(
        grepl("imunoprevencao", grupo_cid10), "Imunoprevenção",
        grepl("mulher_gestacao", grupo_cid10), "Adequada atenção à mulher na gestação",
        grepl("parto", grupo_cid10), "Adequada atenção à mulher no parto",
        grepl("recem_nascido", grupo_cid10), "Adequada atenção ao recém nascido",
        grepl("tratamento", grupo_cid10), "Ações de diagnóstico e tratamento adequado",
        grepl("saude", grupo_cid10), "Ações de promoção à saúde vinculadas a ações de atenção",
        grepl("mal_definidas", grupo_cid10), "Causas de morte mal-definidas",
        grepl("outros", grupo_cid10), "Demais causas (não claramente evitáveis)",
        default = "Total de causas não selecionadas"
      )]
    } else {
      dt[, grupo_cid10 := data.table::fcase(
        grepl("prematuridade", grupo_cid10), "Prematuridade",
        grepl("infeccoes", grupo_cid10), "Infecções",
        grepl("asfixia", grupo_cid10), "Asfixia/Hipóxia",
        grepl("ma_formacao", grupo_cid10), "Anomalia congênita",
        grepl("respiratorias", grupo_cid10), "Afecções respiratórias do recém-nascido",
        grepl("gravidez", grupo_cid10), "Fatores maternos relacionados à gravidez",
        grepl("afeccoes", grupo_cid10), "Afecções originais no período perinatal",
        grepl("mal_definida", grupo_cid10), "Mal-definidas",
        grepl("outros", grupo_cid10), "Demais causas",
        default = "Total de causas não selecionadas"
      )]
    }
    dt
  }
  dt_graficos <- renomeia_grupos(dt_graficos, prefixo_coluna)
  dt_resumo <- renomeia_grupos(dt_resumo, prefixo_coluna)
  niveis_evitaveis_fetal <- c("Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto", "Imunoprevenção", "Causas de morte mal-definidas", "Demais causas (não claramente evitáveis)", "Total de causas não selecionadas")
  niveis_evitaveis_geral <- c("Imunoprevenção", "Adequada atenção à mulher na gestação", "Adequada atenção à mulher no parto", "Adequada atenção ao recém nascido", "Ações de diagnóstico e tratamento adequado", "Ações de promoção à saúde vinculadas a ações de atenção", "Causas de morte mal-definidas", "Demais causas (não claramente evitáveis)", "Total de causas não selecionadas")
  niveis_principais <- c("Fatores maternos relacionados à gravidez", "Asfixia/Hipóxia", "Anomalia congênita", "Infecções", "Afecções respiratórias do recém-nascido", "Prematuridade", "Afecções originais no período perinatal", "Mal-definidas", "Demais causas", "Total de causas não selecionadas")
  if (startsWith(prefixo_coluna, "evitaveis")) {
    niveis <- if (prefixo_coluna == "evitaveis_fetal") niveis_evitaveis_fetal else niveis_evitaveis_geral
  } else if (startsWith(prefixo_coluna, "principais")) {
    niveis <- niveis_principais
  } else stop("Prefixo de coluna não reconhecido.")
  dt_graficos[, grupo_cid10 := factor(grupo_cid10, levels = niveis)]
  dt_resumo[, grupo_cid10 := factor(grupo_cid10, levels = niveis)]
  dt_graficos <- dt_graficos[, .(porc_obitos = round(sum(porc_obitos, na.rm = TRUE), 1)), by = .(ano, grupo_cid10, class)]
  dt_resumo <- dt_resumo[, .(porc_obitos = round(sum(porc_obitos, na.rm = TRUE), 1)), by = .(grupo_cid10)]
  dt_resumo <- dt_resumo[order(-porc_obitos)]
  if (startsWith(prefixo_coluna, "evitaveis")) {
    list(
      graficos = as.data.frame(dt_graficos),
      resumo = as.data.frame(dt_resumo) |>
        dplyr::filter(!(grupo_cid10 %in% c("Causas de morte mal-definidas", "Demais causas (não claramente evitáveis)"))) |>
        dplyr::summarise(porc_obitos_evitaveis = sum(porc_obitos))
    )
  } else {
    list(
      graficos = as.data.frame(dt_graficos),
      resumo = as.data.frame(dt_resumo) |> dplyr::arrange(desc(porc_obitos))
    )
  }
}

# ------- cenarios -------------------------------------------------------------
processa_causas_new <- get("processa_causas", envir = ns)

pesos5_picker  <- c("menos_1000", "1000_a_1499", "1500_a_2499", "2500_mais", "sem_informacao")

cenarios <- list(
  list(nome = "fetal/evitaveis", base = "bloco7_evitaveis_fetal", prefixo = "evitaveis_fetal",
       pesos = pesos5_picker, momentos = c("antes", "durante", "sem_info_parto"),
       grupos = c("imunoprevencao", "mulher_gestacao", "parto", "nao_aplica", "mal_definidas", "outros")),
  list(nome = "fetal/principais", base = "bloco7_principais_fetal", prefixo = "principais_fetal",
       pesos = pesos5_picker, momentos = c("antes", "sem_info_parto", "durante"),
       grupos = c("prematuridade", "infeccoes", "asfixia", "ma_formacao", "gravidez", "afeccoes", "mal_definida", "outros")),
  list(nome = "perinatal/evitaveis", base = "bloco7_evitaveis_perinatal", prefixo = "evitaveis_perinatal",
       pesos = pesos5_picker, momentos = c("antes", "durante", "sem_info_parto", "0_dias", "1_a_6_dias"),
       grupos = c("imunoprevencao", "mulher_gestacao", "parto", "recem_nascido", "tratamento", "saude", "mal_definidas", "outros")),
  list(nome = "perinatal/principais", base = "bloco7_principais_perinatal", prefixo = "principais_perinatal",
       pesos = pesos5_picker, momentos = c("antes", "sem_info_parto", "durante", "0_dias", "1_a_6_dias"),
       grupos = c("prematuridade", "infeccoes", "asfixia", "ma_formacao", "respiratorias", "gravidez", "afeccoes", "mal_definida", "outros")),
  list(nome = "neonatal/evitaveis", base = "bloco7_evitaveis_neonatal", prefixo = "evitaveis_neonatal",
       pesos = pesos5_picker, momentos = c("0_dias", "1_a_6_dias", "7_a_27_dias"),
       grupos = c("imunoprevencao", "mulher_gestacao", "parto", "recem_nascido", "tratamento", "saude", "mal_definidas", "outros")),
  list(nome = "neonatal/principais", base = "bloco7_principais_neonatal", prefixo = "principais_neonatal",
       pesos = pesos5_picker, momentos = c("0_dias", "1_a_6_dias", "7_a_27_dias"),
       grupos = c("prematuridade", "infeccoes", "asfixia", "ma_formacao", "respiratorias", "gravidez", "afeccoes", "mal_definida", "outros"))
)

filtros_estadual <- filtros_padrao(nivel = "estadual", estado = "São Paulo", bloco = "bloco7")
filtros_nacional <- filtros_padrao(nivel = "nacional", estado = "", bloco = "bloco7")
filtros_municipal <- filtros_padrao(nivel = "municipal", estado = "", bloco = "bloco7",
  municipio = "São Paulo", estado_municipio = "São Paulo")
filtros_regional <- filtros_padrao(nivel = "regional", regiao = "Sudeste", bloco = "bloco7")
filtros_macro <- filtros_padrao(nivel = "macro", macro = "Macrorregião 1", estado = "São Paulo",
  estado_macro = "São Paulo", bloco = "bloco7")
filtros_micro <- filtros_padrao(nivel = "micro", micro = "Região de Saúde 1", estado = "São Paulo",
  estado_micro = "São Paulo", bloco = "bloco7")
filtros_semelhantes <- filtros_padrao(nivel = "municipios_semelhantes", municipio = "São Paulo",
  estado_municipio = "São Paulo", bloco = "bloco7")
filtros_comp <- filtros_padrao(nivel = "estadual", estado = "São Paulo", bloco = "bloco7",
  comparar = "Sim", nivel2 = "estadual", estado2 = "Minas Gerais")
filtros_ref_junto <- filtros_padrao(nivel = "estadual", estado = "São Paulo", bloco = "bloco7",
  comparar = "Sim", mostrar_referencia = "mostrar_referencia",
  nivel2 = "estadual", estado2 = "Minas Gerais")

falhas <- 0
checks <- 0
compara_tab <- function(a, b) {
  isTRUE(all.equal(as.data.frame(a), as.data.frame(b), check.attributes = TRUE)) &&
    identical(names(a), names(b)) && identical(class(a), class(b))
}

for (cs in cenarios) {
  base <- get(cs$base, envir = ns)
  for (caso in list(
    list(tag = "estadual", fr = filtros_estadual, comp = FALSE, referencia = FALSE),
    list(tag = "nacional", fr = filtros_nacional, comp = FALSE, referencia = FALSE),
    list(tag = "municipal", fr = filtros_municipal, comp = FALSE, referencia = FALSE),
    list(tag = "regional", fr = filtros_regional, comp = FALSE, referencia = FALSE),
    list(tag = "macro", fr = filtros_macro, comp = FALSE, referencia = FALSE),
    list(tag = "micro", fr = filtros_micro, comp = FALSE, referencia = FALSE),
    list(tag = "semelhantes", fr = filtros_semelhantes, comp = FALSE, referencia = FALSE),
    list(tag = "estadual/referencia", fr = filtros_estadual, comp = FALSE, referencia = TRUE),
    list(tag = "nacional/referencia", fr = filtros_nacional, comp = FALSE, referencia = TRUE),
    list(tag = "comp", fr = filtros_comp, comp = TRUE, referencia = FALSE),
    list(tag = "comp+referencia", fr = filtros_ref_junto, comp = FALSE, referencia = FALSE)
  )) {
    checks <- checks + 1
    rm(list = ls(envir = cache_env), envir = cache_env)
    old <- processa_causas_old(base, cs$pesos, cs$momentos, caso$fr, comp = caso$comp,
                               referencia = caso$referencia, grupos = cs$grupos, prefixo_coluna = cs$prefixo)
    rm(list = ls(envir = cache_env), envir = cache_env)
    new <- processa_causas_new(base, cs$pesos, cs$momentos, caso$fr, comp = caso$comp,
                               referencia = caso$referencia, grupos = cs$grupos, prefixo_coluna = cs$prefixo)
    ok <- compara_tab(old$graficos, new$graficos) && compara_tab(old$resumo, new$resumo)
    if (!ok) {
      falhas <- falhas + 1
      cat("  FALHOU:", cs$nome, caso$tag, "\n")
      bad <- which(old$graficos$ano != new$graficos$ano | old$graficos$porc_obitos != new$graficos$porc_obitos)
      cat("   linhas divergentes:", length(bad), "de", nrow(old$graficos), "\n")
      if (length(bad) > 0) {
        i <- bad[1]
        cat("   old[", i, "]:", paste(unlist(old$graficos[i, ]), collapse = " | "), "\n")
        cat("   new[", i, "]:", paste(unlist(new$graficos[i, ]), collapse = " | "), "\n")
        cat("   old$ano esperado nos primeiros:", paste(head(old$graficos$ano, 12), collapse = ","), "\n")
        cat("   new$ano                    :", paste(head(new$graficos$ano, 12), collapse = ","), "\n")
      }
    }
  }
  # subconjunto de pesos/momentos (inputs alternativos)
  checks <- checks + 1
  pesos_b <- c("menos_1000", "2500_mais")
  momentos_b <- cs$momentos[1]
  rm(list = ls(envir = cache_env), envir = cache_env)
  old <- processa_causas_old(base, pesos_b, momentos_b, filtros_estadual, grupos = cs$grupos[1:3], prefixo_coluna = cs$prefixo)
  rm(list = ls(envir = cache_env), envir = cache_env)
  new <- processa_causas_new(base, pesos_b, momentos_b, filtros_estadual, grupos = cs$grupos[1:3], prefixo_coluna = cs$prefixo)
  ok <- compara_tab(old$graficos, new$graficos) && compara_tab(old$resumo, new$resumo)
  if (!ok) { falhas <- falhas + 1; cat("  FALHOU: subconjunto", cs$nome, "\n") }
}

# caso vazio (estado inexistente)
checks <- checks + 1
fr_vazio <- filtros_padrao(nivel = "estadual", estado = "ZZ", bloco = "bloco7")
base <- get("bloco7_evitaveis_fetal", envir = ns)
old <- processa_causas_old(base, pesos5_picker, c("antes", "durante", "sem_info_parto"), fr_vazio,
                           grupos = c("imunoprevencao"), prefixo_coluna = "evitaveis_fetal")
rm(list = ls(envir = cache_env), envir = cache_env)
new <- processa_causas_new(base, pesos5_picker, c("antes", "durante", "sem_info_parto"), fr_vazio,
                           grupos = c("imunoprevencao"), prefixo_coluna = "evitaveis_fetal")
ok <- compara_tab(old$graficos, new$graficos) && compara_tab(old$resumo, new$resumo)
if (!ok) { falhas <- falhas + 1; cat("  FALHOU: caso vazio\n") }

cat("\nchecks:", checks, "| falhas:", falhas, "\n")
if (falhas == 0) cat("OK: processa_causas identico ao antigo.\n")
