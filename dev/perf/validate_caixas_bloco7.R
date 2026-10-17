# Valida cria_caixa_conjunta_bloco7 e cria_caixa_principais_evitaveis_bloco7
# (novo: HTML como string unica) contra a versao do HEAD.
suppressMessages(pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE))
ns <- asNamespace("painel.indicadores")
new_conjunta <- get("cria_caixa_conjunta_bloco7", envir = ns)
new_principais <- get("cria_caixa_principais_evitaveis_bloco7", envir = ns)

arq_old <- file.path(tempdir(), "funcoes_globais_head.R")
system2("git", c("show", "HEAD:R/funcoes_globais.R"), stdout = arq_old)
env_old <- new.env(parent = ns)
sys.source(arq_old, envir = env_old)
old_conjunta <- env_old$cria_caixa_conjunta_bloco7
old_principais <- env_old$cria_caixa_principais_evitaveis_bloco7

norm <- function(x) {
  s <- as.character(x)
  gsub(">\\s+<", "><", trimws(gsub("\\s+", " ", gsub(">\\s*\\n\\s*<", "><", s))))
}

dados_conjunta <- data.frame(
  porc_obito_fetal_menos_1000 = 25.5,
  porc_obito_fetal_1000_1499 = 10,
  porc_obito_fetal_1500_2499 = 30.25,
  porc_obito_fetal_2500_mais = 30,
  porc_obito_fetal_peso_faltante = 4.25,
  porc_obito_fetal_antes = 90,
  porc_obito_fetal_durante = 5,
  porc_obito_fetal_momento_faltante = 5,
  porc_obito_perinatal_antes = 40.5,
  porc_obito_perinatal_durante = 10,
  porc_obito_perinatal_0_dias = 30,
  porc_obito_perinatal_1_a_6_dias = 15,
  porc_obito_perinatal_momento_faltante = 4.5,
  porc_obito_perinatal_menos_1000 = 30,
  porc_obito_perinatal_1000_1499 = 20,
  porc_obito_perinatal_1500_2499 = 25,
  porc_obito_perinatal_2500_mais = 20,
  porc_obito_perinatal_peso_faltante = 5,
  porc_obito_neonatal_0_dias = 35,
  porc_obito_neonatal_1_a_6_dias = 40,
  porc_obito_neonatal_7_a_27_dias = 25,
  porc_obito_neonatal_menos_1000 = 40,
  porc_obito_neonatal_1000_1499 = 15,
  porc_obito_neonatal_1500_2499 = 25,
  porc_obito_neonatal_2500_mais = 15,
  porc_obito_neonatal_peso_faltante = 5
)
dados_conjunta_nan <- dados_conjunta
dados_conjunta_nan[, ] <- NaN

indicadores <- c("fetal peso por idade gestacional", "fetal momento do obito por peso",
  "perinatal momento do obito por peso", "perinatal peso por momento do obito",
  "neonatal momento do obito por peso", "neonatal peso por momento do obito")

dados_princ <- data.frame(
  grupo_cid10 = factor(c("Prematuridade", "Asfixia/Hipóxia", "Adequada atenção ao recém nascido",
                         "Demais causas (não claramente evitáveis)", "Causas de morte mal-definidas",
                         "Imunoprevenção", "Ações de diagnóstico e tratamento adequado"),
                       levels = c("Imunoprevenção", "Adequada atenção à mulher na gestação",
                                  "Adequada atenção à mulher no parto", "Adequada atenção ao recém nascido",
                                  "Ações de diagnóstico e tratamento adequado",
                                  "Ações de promoção à saúde vinculadas a ações de atenção",
                                  "Causas de morte mal-definidas", "Demais causas (não claramente evitáveis)",
                                  "Total de causas não selecionadas")),
  porc_obitos = c(30.1, 20, 15.55, 10.1, 8, 6.5, 0.05)
)
dados_princ_grupo_char <- dados_princ
dados_princ_grupo_char$grupo_cid10 <- as.character(dados_princ$grupo_cid10)
dados_princ_nan <- data.frame(grupo_cid10 = dados_princ$grupo_cid10[1:3], porc_obitos = c(NaN, NaN, NaN))
dados_princ_vazio <- data.frame(grupo_cid10 = factor(character(0)), porc_obitos = numeric(0))

falhas <- 0
checks <- 0
chama <- function(fn) tryCatch(list(ok = TRUE, valor = fn()), error = function(e) list(ok = FALSE, valor = conditionMessage(e)))
compara <- function(nome, fn_old, fn_new) {
  checks <<- checks + 1
  a <- chama(fn_old); b <- chama(fn_new)
  if (!a$ok || !b$ok) {
    ok <- identical(a$ok, b$ok) && identical(a$valor, b$valor)
  } else {
    ok <- isTRUE(identical(norm(a$valor), norm(b$valor)))
  }
  if (!ok) {
    falhas <<- falhas + 1
    cat("FALHOU:", nome, "\n")
    if (a$ok && b$ok) {
      na <- norm(a$valor); nb <- norm(b$valor)
      n <- min(nchar(na), nchar(nb))
      i <- which(strsplit(na, "")[[1]][1:n] != strsplit(nb, "")[[1]][1:n])[1]
      if (!is.na(i)) cat("  pos", i, "\n  OLD:", substr(na, max(1, i - 80), i + 100), "\n  NEW:", substr(nb, max(1, i - 80), i + 100), "\n")
    } else {
      cat("  old:", substr(a$valor, 1, 200), "\n  new:", substr(b$valor, 1, 200), "\n")
    }
  }
}

for (ind in indicadores) {
  for (d in list(normal = dados_conjunta, nan = dados_conjunta_nan)) {
    for (completa in c(TRUE, FALSE)) {
      args <- list(dados = d, indicador = ind, titulo = "Dentre os óbitos selecionados,",
                   tamanho_caixa = 330, retornar_caixa_completa = completa)
      compara(paste("conjunta", ind, "| nan?", is.nan(d$porc_obito_fetal_menos_1000[1]), "| completa", completa),
              function() do.call(old_conjunta, args), function() do.call(new_conjunta, args))
    }
  }
}
args_p <- function(d, completa) list(dados = d, titulo = "Dentre os óbitos perinatais selecionados,",
                                    tamanho_caixa = 330, retornar_caixa_completa = completa)
for (d in list(fator = dados_princ, char = dados_princ_grupo_char, nan = dados_princ_nan, vazio = dados_princ_vazio)) {
  for (completa in c(TRUE, FALSE)) {
    args <- args_p(d, completa)
    compara(paste("principais", class(d$grupo_cid10)[1], "n=", nrow(d), "completa", completa),
            function() do.call(old_principais, args), function() do.call(new_principais, args))
  }
}

cat("\nchecks:", checks, "| falhas:", falhas, "\n")
if (falhas == 0) cat("OK: caixas do Bloco 7 produzem HTML identico ao antigo.\n")
