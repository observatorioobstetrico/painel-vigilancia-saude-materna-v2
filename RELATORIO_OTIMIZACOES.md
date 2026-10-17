# Relatório da sessão — migração para Parquet e otimizações de desempenho

> Projeto: `painel-vigilancia-saude-materna-v2` (Shiny + Golem)
> Sessões: 16–17/10/2026 (parte 1: migração Parquet e otimizações de cálculo; parte 2: otimizações de UI/Nível 3/`cria_indicadores`/cache do Bloco 7; parte 3: agregações largas com `rowsum` e caixas em string HTML)
> Este documento resume **tudo o que foi feito, medido e descoberto**, incluindo bugs encontrados no caminho e o que ficou pendente. As partes 1–2 foram commitadas ao final da sessão anterior; a parte 3 foi commitada junto com esta atualização.

---

## 1. Resumo executivo

| Métrica | Antes | Depois |
|---|---|---|
| Boot do app (`pkgload::load_all`) | **~25,3 s / 1.614 MB RSS** | **~2,9 s / ~205 MB** |
| Dados em `data/` carregados no boot | 60 arquivos `.rda` (36,7 MB) | 8 arquivos auxiliares (342 KB) |
| Bases em Parquet | — | 45 arquivos (49,5 MB) em `inst/app/dados/` |
| `processa_bloco7` (12 chamadas) | 2.427 ms | **190 ms (~13×)**; com cache (parte 2) ≈ 0 em buscas repetidas |
| `processa_causas` (1 chamada) | 473 ms | **177 ms (~2,7×)**; cache = 0 ms |
| Tabela de CIDs no nível 3 | 305 MB de RAM | **5,9 MB** (projeção de 12–13 de 1.095 colunas) |
| Busca do nível 3 (tabela de CIDs) | ~2–3 s de computação | ~0,4–0,5 s + `bindCache` em buscas repetidas |
| Download da tabela do nível 3 | calculado em toda busca (~177 ms) | calculado só quando o usuário baixa |
| **Montagem/serialização da UI por requisição** (parte 2) | **2,7–3,6 s** (1,39 s tags + 1,32 s serialização) | **0,08–0,14 s** (1ª req.: 3,4 s; pré-renderizada por processo) |
| **2ª carga da página no mesmo processo** (parte 2) | TTFB ~3,9–4,1 s | **TTFB ~0,13 s** |
| **Busca do nível 3, nacional** (parte 2) | **~2,7 s** | **~1,7 s** (condição fora do `summarise`) |
| **`cria_indicadores`** (bloco4, 25 colunas) (parte 2) | **30–50 ms/chamada** | **10–20 ms** (passada única) |
| **`data_tabela1` no nacional** (parte 2) | **0,52–0,59 s** | **0,02–0,06 s (~10×)** |
| **`processa_causas`** (parte 3, soma por matriz/`rowsum`) | **104–534 ms/chamada** | **66–303 ms (1,5–3,3×)** |
| **Pipeline server completo de uma aba do Bloco 7** (parte 3) | **0,76–1,76 s** | **0,52–1,21 s (1,3–1,6×)** |
| **Troca para as sub-abas perinatal/neonatal do Bloco 7** (navegador, parte 3) | **4,07 s / 3,55 s** | **3,10 s / 2,80 s** |
| **Caixas (construção + serialização)** (parte 3) | **2,6–8,5 ms/caixa** | **1,1–3,3 ms (1,4–5,2×)** |

---

## 2. Camada de dados: migração RDA → Parquet

### 2.1 Diagnóstico

- O app usa `pkgload::load_all()` no `app.R` (inclusive em produção, via `deployApp.R`), e `pkgload:::load_data()` executa `load()` em **todos** os `.rda` de `data/` de forma *eager*. O `LazyData: true` do DESCRIPTION não vale nesse caminho.
- Medição: 60 arquivos, 36,7 MB em disco → **23,8 s de leitura e 1.445 MB de RAM** (RSS de ~1,6 GB no boot).
- As maiores tabelas eram desproporcionais: `bloco7_distribuicao_cids` tem 72.410 × **1.095 colunas** (305 MB de RAM, **95,4% das células são zero**), e há variantes perinatal/neonatal/fetal (~620 MB somadas).

### 2.2 O que foi feito

1. **45 bases convertidas para Parquet** (`zstd`, nível 5) em `inst/app/dados/*.parquet` e removidas de `data/`:
   - Todas as `bloco1`…`bloco7*`, `bloco8_*`, `base_incompletude`, `base_garbage_code`, `malformacao`, etc.
   - Na segunda rodada (uniformidade): `bloco4_deslocamento_macrorregiao`, `bloco4_deslocamento_uf`, `base_cobertura_muni_2015_2021`, `base_cobertura_uf_regioes_2015_2021`, `base_referencia_baixo_peso`, `rmm_corrigida` e os 4 `sub_registro_{sim|sinasc}_{muni|uf_regioes}_2015_2021`.
2. **Carregamento preguiçoso com cache** (`R/acesso_dados.R`):
   - `.onLoad()` cria *active bindings* para cada parquet no namespace; o código existente continua usando `bloco1`, `get(filtros()$bloco)`, etc. sem alterações.
   - A base só é lida na primeira utilização e fica em cache na sessão.
   - `carrega_dados_projetado(nome, colunas)` lê apenas colunas necessárias via Arrow (usado no nível 3).
3. **8 arquivos auxiliares mantidos em `data/`** (pequenos, necessários no boot): `municipios_choices`, `estados_choices`, `micro_r_saude_choices`, `macro_r_saude_choices`, `tabela_indicadores`, `tabela_aux_municipios`, `df_cid10`, `tabela_radar`.
4. **Arquivos sem uso removidos** (auditados por busca exata + `get()` dinâmico):
   - `base_incompletude_bloco7`, `bloco8_garbage_fetal`, `bloco8_garbage_materno`, `bloco8_garbage_neonatal`, `bloco8_grafico_evitaveis_neonatal_precoce`, `rmm_corrigida_2012_2021`, `rmm_fator_de_correcao`.
   - `rmm_fator_de_correcao` teve também a computação e o `use_data()` removidos do pipeline. `base_incompletude_bloco7` continua sendo calculado (alimenta `base_incompletude` no `blocos.R`), mas não é mais salvo.
5. **Pipeline atualizado**:
   - Novo `data-raw/utils_parquet.R` com `usar_dados_parquet(objeto)` e `converter_rda_em_parquet(nomes)`.
   - `data-raw/csv/blocos.R` e `data-raw/extracao-dos-dados/cobertura/cobertura.R` agora usam `usar_dados_parquet()` para as bases grandes.
6. **DESCRIPTION**: `arrow` e `rlang` adicionados a `Imports`. O `deployApp.R` já incluía `inst/` e `data/` — não precisou mudar.

### 2.3 Validação da migração

- **45/45 bases Parquet idênticas aos `.rda` originais** (`all.equal`), fatores e UTF-8 preservados.
- Funções-chave (`cria_indicadores`, `filtra_localidade`, `processa_causas`, `processa_bloco7`, `get()` dinâmico) produzem resultados idênticos com Parquet.
- Boot medido após a migração: **2,86 s / 205 MB**.
- Trade-off: bundle de deploy cresce (49,5 MB Parquet vs 34,7 MB `.rda`); leitura e memória caem drasticamente.

### 2.4 Como atualizar os dados a partir de agora

```r
# no console, na raiz do projeto
source("data-raw/csv/blocos.R")                 # gera as bases e grava parquet
source("data-raw/extracao-dos-dados/cobertura/cobertura.R")  # cobertura/sub-registro
```

Regra: bases ≥ ~0,3 MB → `usar_dados_parquet(objeto)`; bases pequenas (choices/tabelas auxiliares) → `usethis::use_data()`.

---

## 3. Otimizações de cálculo

### 3.1 `processa_bloco7` — agregar primeiro (maior ganho do Bloco 7)

**Antes:** para cada grupo, `unlist(.SD[, v, with = FALSE])` coluna a coluna e `indicadores_fn` dentro do `j`; o group-by era refeito duas vezes (por `ano+class` e por `class`).

**Depois** (`R/funcoes_globais.R`):
- Uma única agregação vetorizada em C: `dt[, lapply(.SD, sum), by = c("ano","class"), .SDcols = colunas]`;
- Resumo derivado do agregado anual (`rollup` por `class`), sem reler a base;
- `indicadores_fn` aplicada só nas poucas linhas agregadas.

**Medição:** 12 chamadas: **2.427 ms → 190 ms (~13×)**. Resultados idênticos ao antigo (incluindo caso vazio, multi-class e colunas multi-nome).

### 3.2 `processa_causas` / `filtra_colunas_evitaveis_principais` — uma passada

- Nova `agrega_colunas_evitaveis()` faz filtro + agregação uma única vez e devolve os dois formatos longos (gráficos por ano×class e resumo por class, derivado do agregado anual).
- `processa_causas` deixou de chamar `filtra_colunas` duas vezes (que refiltrava e remeltava a base inteira).
- **Medição:** 473 ms → **177 ms (~2,7×)**; repetição com cache = ~0 ms.

### 3.3 Cache interno de resultados (`R/cache_interno.R`)

- Cache em memória com:
  - chave = **`rlang::hash()`** (32 bytes, tamanho fixo);
  - identidade da base = endereço de memória + dimensões + nomes;
  - limite de 48 itens / 350 MB, evicção do mais antigo;
  - objetos > 175 MB não são cacheados (caso da tabela de 1.095 colunas filtrada).
- Aplicado **dentro de** `filtra_localidade()` e `processa_causas()` — vale para todos os módulos/bases sem mudar chamadas.
- Ganho: trocar de sub-aba do Bloco 7 ou repetir busca com os mesmos filtros deixa de refiltrar/recalcular (0 ms).

### 3.4 Filtros largos em `data.table` (P3.8)

- Nova `filtra_localidade_dt()` replica a semântica de `filtra_localidade()` em data.table e é usada no caminho das tabelas largas (`agrega_colunas_evitaveis`), eliminando `as.data.frame()` + cópias do dplyr.
- `filtra_localidade()` continua com a mesma API (agora com cache) para os demais chamadores.

### 3.5 Nível 3 — projeção de colunas via Arrow (P2.5)

- `base_bloco_selecionado()` agora projeta apenas as colunas citadas nas expressões do indicador (`calculo`, `numerador`, `denominador`) + chaves de localidade/ano, via `carrega_dados_projetado()`.
- **Medição:** 12–13 colunas em vez de 1.095; **5,9 MB em vez de 305 MB** de RAM; carga fria 0,38 s.
- **Validação:** 36 indicadores roteados para a tabela de CIDs produzem dados e cálculos **idênticos** à tabela completa.

### 3.6 Outros ganhos

- **`data_tabela_download` sob demanda**: cálculo movido para dentro do `downloadHandler` (economiza ~177 ms em toda busca do nível 3, para quem não baixa).
- **`bindCache` no nível 3**: `data_grafico_regioes`, `data_grafico_serie` e `data_tabela1` cacheados com chave = filtros sem o campo volátil `pesquisar` (+ `input$opcoes_tab1` no caso da tabela). Busca repetida fica instantânea.
- **`highcharter`**: a versão instalada (**0.12.6.1**, GitHub `main`) já é a mais recente; CRAN está em 0.9.5. Nada a atualizar.

### 3.7 UI pré-renderizada e cacheada por processo (parte 2)

- **Diagnóstico (medido):** montar as tags (`app_ui()`) custava **1,39 s** e serializá-las (`as.character`/`renderTags`) mais **1,32 s** → **~2,7–3,6 s por requisição**, todos os usuários pagando de novo (625 KB de HTML; TTFB medido ~3,9–4,1 s no boot).
- **Solução (`R/app_ui.R`):** o corpo original virou `constroi_app_ui()`; `app_ui()` chama `htmltools::renderTags()` **uma única vez por processo** e devolve `tagList(tags$head(HTML(head)), HTML(html), dependencies)` nas requisições seguintes. `htmltools` entrou em `Imports` no DESCRIPTION.
- **Medição:** 1ª requisição no processo = 3,42 s; seguintes = **0,08–0,14 s** (~30×). Na validação e2e, a 2ª carga de página no mesmo processo caiu para **TTFB = 133 ms** (vs ~4,1 s).
- **Validação:** página renderizada idêntica (head normalizado igual; body igual exceto indentação; dependências e recursos presentes no navegador); app rodou normalmente em todos os cenários.
- Observação: o registro de *singletons* do head é recriado vazio na página cacheada; não há dependência dinâmica que reaproveite os singletons do head inicial, e o teste e2e (gráficos/tabelas/tooltips) não acusou efeito.

### 3.8 Nível 3 — condição por grupo fora do `summarise` (parte 2)

- **Diagnóstico (medido):** `data_tabela1` chamava `dplyr::if_else(condition = infos_indicador()$tipo_do_indicador == …, …)` **dentro** do `summarise`; com `group_by(uf, macro_r_saude, municipio)` isso reavalia a condição (e o reativo `infos_indicador()`) **5.570 vezes** no nacional → **0,52–0,59 s** só nessa etapa (perfil: `dplyr::summarise` 1,55–1,71 s por busca).
- **Solução (`R/mod_nivel_3.R`):** `summarise()` só de `numerador`/`denominador`; o `indicador` passa a ser calculado **depois**, vetorizado (`if (tipo == "porcentagem") … else round(… * fator, 1)`). O `downloadHandler` também teve `periodo`/`fator`/expressões hoisted.
- **Medição:** pipeline nacional **0,59 s → 0,06 s (~10×)**; busca do Nível 3 no app: bloco1 **2,72 s → 1,65 s**, bloco3 **2,62 s → 1,83 s**.
- **Validação:** 9/9 indicadores idênticos (porcentagem, taxa ×1000 e incidência), incluindo classe/`group_vars`/ordem de colunas.

### 3.9 `cria_indicadores` em passada única (parte 2)

- **Diagnóstico:** a função fazia **uma `summarise` + um `full_join` por coluna** de cálculo (25 no bloco4) → 30–50 ms por chamada (bloco4, estadual); no perfil, 0,52 s (bloco1) e 1,67 s (bloco4) por busca.
- **Solução (`R/funcoes_globais.R`):** todas as expressões viram uma única lista de quosures (`lapply(df_calcs, rlang::parse_expr)` + `!!!exprs` numa só `summarise`; `.groups = "keep"` no ramo agrupado), preservando exatamente o comportamento dos dois ramos (0-col e agrupado por `ano`, inclusive o ramo `df_calcs_dist_bloco7`).
- **Medição:** **30–50 ms → 10–20 ms** por chamada (~2,5–5×); busca do bloco4: 2,45 s → 2,16 s (8 chamadas/busca, medido com `trace`).
- **Validação:** **192/192** comparações idênticas à versão antiga (11 conjuntos de cálcs — blocos 1–6, `bloco4_deslocamento`, `bloco4_profissional` × níveis nacional/estadual/municipal × escopos local/referência/comp/sem localidade/`escolha2` + ramo agrupado por ano + bloco5 montado dos `eventReactive`).

### 3.10 `processa_bloco7` com cache interno (parte 2)

- **Diagnóstico:** ao contrário de `processa_causas`, o `processa_bloco7` não tinha cache: as 12 chamadas por busca (~10 ms cada, mais cargas de parquet na 1ª vez) eram refeitas em toda busca.
- **Solução (`R/funcoes_globais.R`):** wrapper com `chave_cache_interno` = identidade da base + escopo (`comp`/`referencia`/`localidade_resumo`) + **`cols` completo** (agrupamento de colunas por indicador) + código do `indicadores_fn` (`codigo_funcao()`) + filtros. O corpo virou `processa_bloco7_sem_cache()`.
- **Bug encontrado na validação:** a 1ª versão da chave usava a **união** das colunas — como `tot` e `num_taxa_total` compartilham nomes de coluna, inputs diferentes geravam a mesma chave e o resultado cacheado era devolvido indevidamente. Corrigido para incluir o `cols` completo.
- **Medição:** cache hit ≈ **0 ms** (12 chamadas deixam de custar ~0,1–0,2 s por busca repetida).
- **Validação:** **35/35** comparações idênticas à versão antiga (8 cenários dos módulos fetal/neonatal/morbidade × 3 escopos + troca de faixa de peso/momento + caso com zero linhas).

### 3.11 Não implementado

- **`highchartProxy`** (atualização in-place das séries): a API existe (`highchartProxy`, `hcpxy_update_series`), mas exige, por gráfico, separar render inicial (isolado dos filtros) de um `observeEvent` que empurra só as séries — senão o render completo e o proxy competem. São ~50 gráficos; o risco de regressão é alto e, após os ganhos acima, o custo restante é majoritariamente renderização no navegador. Recomendação: fazer incremental (Nível 3 e Bloco 7 primeiro) com validação visual.

---

## 3b. Parte 3 — agregações largas (`rowsum`) e caixas em string HTML

### 3.12 Diagnóstico: por que perinatal/neonatal do Bloco 7 demoravam mais que fetal

Medições no navegador (chromote, busca feita na aba fetal e depois troca de sub-aba):

| Cenário | Antes | Depois |
|---|---|---|
| Trocar para **perinatal** (1ª visita, fria) | 4,07 s | 3,10 s |
| Trocar para **neonatal** (1ª visita, fria) | 3,55 s | 2,80 s |
| Trocar de volta para **fetal** | 0,02 s | 0,02 s |
| Trocar para morbidade | ~1,2 s | ~1,2 s |

Motivos:
1. **Fetal é a sub-aba padrão**: o Shiny suspende outputs de abas ocultas; o fetal é renderizado quando o Bloco 7 abre (com os filtros vigentes), então o clique nele é instantâneo. Perinatal/neonatal pagam o pipeline frio inteiro no 1º clique (e de novo a cada busca feita em outra aba).
2. **As tabelas de causas são muito mais largas**: `evitaveis_perinatal` 182 colunas, `principais_perinatal` 206 (fetal: 73/116; neonatal: 130/145). No app instrumentado, `processa_causas` custava ~446 ms (perinatal) e ~355 ms (neonatal) por chamada contra ~260 ms (fetal).
3. Neonatal ainda tem 8 gráficos/caixas (fetal/perinatal têm 6).

### 3.13 Agregação por matriz + `rowsum` (`processa_causas` e `processa_bloco7`)

- **Causa (medida):** `dt[, lapply(.SD, sum), by = "ano", .SDcols = ...]` materializa as colunas de `.SD` **por grupo**: ~186–208 ms nas tabelas largas (172–196 colunas), contra ~11 ms da mesma soma sem `by`. `as.matrix` + `rowsum` (C) faz o mesmo em ~48–54 ms.
- **Mudanças (`R/funcoes_globais.R`):**
  - novos helpers `soma_colunas_por_ano()` (matriz + `rowsum`, preservando a ordem de 1ª aparição dos anos, igual ao `by`) e `agrega_por_ano_rapido()` (com fallback para o caminho antigo: vazio, colunas não numéricas, `class` não constante);
  - `agrega_colunas_evitaveis`: estreita a tabela para chaves + colunas alvo **antes** de filtrar/copiar e renomeia `class` in place (remove uma cópia integral redundante);
  - `processa_bloco7_sem_cache`: usa o caminho rápido para o agregado anual.
- **Antes/depois por chamada** (mediana, cache frio; SP / Brasil):

| Base | local antes → depois | referência antes → depois |
|---|---|---|
| evitáveis fetal | 104 → 66 ms | 318 → **98 ms (3,3×)** |
| evitáveis perinatal | 239 → 129 ms | 494 → 223 ms (2,2×) |
| principais perinatal | 278 → 158 ms | 534 → 303 ms (1,8×) |
| evitáveis neonatal | 179 → 93 ms | 218 → 143 ms (1,5×) |
| principais neonatal | 185 → 104 ms | 254 → 161 ms (1,6×) |

- **Pipeline server completo de uma aba** (filtros + 8 `processa_bloco7` + 4 `processa_causas`, mediana de 5): fetal nacional 0,88→0,65 s / estadual 0,76→0,52 s; perinatal 1,76→1,21 / 1,40→0,87; neonatal 1,19→0,81 / 1,10→0,72 (1,3–1,6×).
- **Validação:** **73/73** casos de `processa_causas` idênticos (6 bases × 11 escopos — nacional/estadual/regional/macro/micro/municipal/semelhantes/referência/comp — + subconjuntos de pesos/momentos + caso vazio) e **35/35** de `processa_bloco7`.

### 3.14 Caixas como string HTML única

- **Diagnóstico (medido):** cada caixa custava 1,6–5,6 ms de construção **+ ~1,7 ms (p50) de serialização** `tag → HTML` que o Shiny paga a cada render. O volume é grande: **nível 1 tem 93 caixas** (76 renderizadas por sessão de busca), bloco 4 tem 32 e bloco 6 tem 15. (A atribuição inicial de "0,3–0,45 s por render" nos traces era inclusiva dos resumos reativos — `processa_bloco7`/`processa_causas` —, não do HTML em si.)
- **Mudança:** `cria_caixa_server`, `cria_caixa_conjunta_bloco5`, `cria_caixa_conjunta_bloco7` e `cria_caixa_principais_evitaveis_bloco7` montam o conteúdo como **uma string HTML** (helper compartilhado `attr_class_html()` reproduz o tratamento de `class` do htmltools: `NA` → atributo sem valor, `NULL` → omite; descrições escapadas com `htmltools::htmlEscape()`). O `bs4Dash::box` (caminho "caixa completa") foi mantido como wrapper.
- **Antes/depois (construção + serialização, mediana):**

| Função | Caminho | Antes | Depois | Ganho |
|---|---|---|---|---|
| `cria_caixa_server` | inline (`FALSE`) | 2,63 ms | **1,08 ms** | 2,4× |
| `cria_caixa_server` | `bs4Dash::box` | 4,50 ms | **3,22 ms** | 1,4× |
| `cria_caixa_conjunta_bloco5` | `box` | 5,22–6,49 ms | **2,75–2,92 ms** | 1,9–2,2× |
| `cria_caixa_conjunta_bloco7` | inline | 5,05 ms | **1,11 ms** | 4,5× |
| `cria_caixa_conjunta_bloco7` | `box` | 6,46 ms | **3,01 ms** | 2,1× |
| `cria_caixa_principais_evitaveis_bloco7` | inline | 7,14 ms | **1,37 ms** | 5,2× |
| `cria_caixa_principais_evitaveis_bloco7` | `box` | 8,52 ms | **3,27 ms** | 2,6× |

- **Validação:** `cria_caixa_server` **60/60** (grade de argumentos × os dois caminhos), `cria_caixa_conjunta_bloco5` **7/7**, `cria_caixa_conjunta_bloco7` **32/32**, `cria_caixa_principais_evitaveis_bloco7` **32/32** — HTML idêntico ao antigo (normalizado), incluindo casos de borda (NaN, vetor de referência, NA, fator, vazio).
- **Bug de teste encontrado:** o validador inicial do `cria_caixa_server` só exercitava o caminho `TRUE` (default) e tratava string HTML como erro — corrigido para cobrir os dois caminhos com comparador robusto.

### 3.15 Observações de medição (parte 3)

- O harness `dev/perf/drive.R` tem corrida no `shiny:idle` quando a busca não produz recomputação (timeout de 30 s mesmo com o código antigo — confirmado via `git stash`); os cenários customizados com espera por gráficos renderizados são mais confiáveis.
- `Rprof` com `line.profiling` distorce todos os tempos (~2×); para comparar antes/depois usar `trace()`/medição direta e o mesmo cenário.

---

## 4. Bugs encontrados e corrigidos durante a sessão

### 4.1 Filtros do app não apareciam (`app_ui.R`)

- **Causa:** cinco `conditionalPanel()` tinham `style = "display: none;"` inline. O Shiny atual controla a visibilidade por **classe CSS** (`shiny-conditional--shown`; regra `[data-display-if]:not(.shiny-conditional--shown){display:none}`). Ao satisfazer a condição, a classe é adicionada, mas o `display:none` inline continuava valendo → card de filtros e painéis do nível 3 ficavam ocultos para sempre.
- **Correção:** removido o `style = "display: none;"` dos cinco painéis (filtros, Ano, Período e os dois painéis do nível 3).
- **Bônus:** o `src` do `funcoes_javascript.js` estava errado (`/funcoes_javascript.js`); corrigido para `www/global/funcoes_javascript.js`.

### 4.2 Sessão quebrava ao iniciar (`bindCache` + `eventReactive`)

- `eventReactive()` já aplica `bindEvent()` internamente; `bindCache()` depois dele lança erro e **aborta a inicialização do `app_server`** (cascata de `object 'filtros' not found` em todos os módulos).
- **Correção:** `reactive({...}) |> bindCache(...) |> bindEvent(...)`.

### 4.3 Erro do Bloco 7 nível 2: `variable names are limited to 10000 bytes`

- A chave do cache era a concatenação dos nomes das colunas; tabelas largas (ex.: `bloco7_principais_perinatal`, 206 colunas) ultrapassavam o limite de 10.000 bytes para nomes de variáveis no R. Afetava as abas perinatal, neonatal e morbidade.
- **Correção:** chave = `rlang::hash()` (32 bytes). Além disso, a estrutura de controle do cache foi reescrita (a evicção quebrava com objetos gigantes) e objetos > 175 MB não são cacheados.

### 4.4 Harness de testes que "prendia" o comando

- **Causa 1:** `on.exit()` no top-level de um Rscript **não executa** (só funciona dentro de função). O app iniciado pelo harness nunca era encerrado e, no Windows, herdava os handles do pipe do terminal — o comando só "voltava" no timeout da ferramenta.
- **Causa 2:** `processo$kill(tree = TRUE)` — argumento inexistente no processx.
- **Correção:** fluxo do harness em `tryCatch(..., finally = limpar())`, com `processo$kill_tree()`, fechamento do Chrome do chromote e fallback via `ps` para Chrome headless órfão. **Prova:** execução completa em **27,9 s**, com porta 8123 liberada automaticamente.
- Também foram limpos processos órfãos deixados pelos testes anteriores (instâncias do app e Chrome headless).

### 4.5 Warnings pré-existentes encontrados na parte 2 (não corrigidos)

- Clicar em "Atualizar resultados" sem indicador do Nível 3 selecionado dispara `Error in if: argument is of length zero` em dois observadores do `R/mod_nivel_3.R` (linhas ~1037 e ~1348: `infos_indicador()$num_indicadores_incompletude` / `$nome_abreviado` com 0 linhas). **Não é regressão da parte 2** (presente em logs gravados antes das mudanças); o Shiny apenas registra o warning e a sessão segue normal. Correção simples: `req(nrow(...))`/checagem de `length()` antes dos `if`.

---

## 5. Validação

### 5.1 Comparações de equivalência (old vs new)

- `processa_bloco7`: idêntico em múltiplos conjuntos de colunas, multi-classe e caso vazio.
- `processa_causas`: idêntico nos escopos local/comparação/referência, subconjuntos de pesos/momentos/grupos e níveis nacional/estadual.
- `filtra_localidade` (com e sem cache) e `filtra_localidade_dt`: idênticos em níveis nacional, estadual, municipal, macro e referência, em bases de Blocos 1 e 7.
- Projeção do nível 3: 36 indicadores com dados e cálculos idênticos à tabela completa.
- Parquet: 45/45 bases idênticas aos `.rda` originais.

### 5.2 Testes end-to-end (Chrome headless via chromote)

- **Nível 1 (home):** 3 gráficos, 0 erros de output.
- **Bloco 7 (nível 2)**, navegando pelas 4 sub-abas com busca em cada:

| Sub-aba | Gráficos | Erros de output |
|---|---|---|
| Mortalidade fetal | 9 | 0 |
| Mortalidade perinatal | 15 | 0 |
| Mortalidade neonatal | 23 | 0 |
| Morbidade neonatal | 27 | 0 |

- **Nível 3** (indicador de óbitos neonatais por causas evitáveis, tabela de CIDs): página renderizada completa, **0 erros de código**; a única mensagem exibida é um `validate(need(...))` esperado ("incompletude não disponível").
- Log do servidor sem erros em todas as execuções finais.

### 5.3 Validações da parte 2 (após as mudanças de UI/Nível 3/`cria_indicadores`/cache)

- **Equivalência ponta a ponta por script** (fora do navegador, comparação `all.equal` + classe/`group_vars`/nomes):
  - `cria_indicadores` antiga vs nova: **192/192 casos idênticos**;
  - `processa_bloco7` antigo vs novo (com cache): **35/35 idênticos**, incluindo troca de input e caso vazio;
  - `data_tabela1` antigo vs novo: **9/9 indicadores idênticos** (porcentagem/taxa/incidência).
- **Testes e2e (Chrome headless), código final:** boot, 2ª carga de página no mesmo processo, Nível 1, Blocos 1–6 (incluindo as 3 sub-abas do Bloco 4), Bloco 7 (fetal e neonatal), Nível 3 (blocos 1 e 3) e **duas sessões simultâneas** — todos com **0 `shiny-output-error`** e gráficos renderizando (contagem de `.highcharts-container` no DOM: 2–10 por tela, conforme o bloco).
- **Duas sessões simultâneas:** sessão 1 (Nível 1) e sessão 2 (Bloco 5) operando juntas; ao reativar a aba, a sessão 1 recomputa/rende normalmente (3 gráficos; a suspensão de outputs em aba de fundo é comportamento normal do Shiny, não interferência entre sessões).
- **Trace no app:** `cria_indicadores` é chamada 8× por busca no Bloco 4 (tempo agregado 2,05 s incluindo os reativos encadeados).
- Logs do servidor sem erros novos; as **únicas ocorrências** são 2 warnings pré-existentes do `mod_nivel_3` (ver 4.5).

### 5.4 Validações da parte 3 (agregações e caixas)

- **Equivalência por script** (fora do navegador, `all.equal` + nomes/classes):
  - `processa_causas` (novo: matriz/`rowsum`) vs antigo: **73/73 idênticos**;
  - `processa_bloco7` (novo) vs antigo: **35/35 idênticos**;
  - `cria_caixa_server` (novo: string HTML) vs antigo: **60/60 idênticos** (27 combinações de argumentos × 2 caminhos + bordas);
  - `cria_caixa_conjunta_bloco5`: **7/7**; `cria_caixa_conjunta_bloco7`: **32/32**; `cria_caixa_principais_evitaveis_bloco7`: **32/32**.
- **Testes e2e (Chrome headless) com o código final:** nível 1 (0,34–0,55 s, 3 gráficos), blocos 1, 4, 5 (2,24 s, 8 gráficos), 6 e 7 (fetal e neonatal) — todos com **0 `shiny-output-error`**.
- Observação: no harness `drive.R`, alguns cenários (bloco 4, bloco 7) intermitentemente estouram o timeout de idle de 30 s por corrida do próprio harness (reproduzido também com o código antigo via `git stash`); a verificação `CHECK_FINAL` confirma renderização e 0 erros.

---

## 6. Inventário final dos dados

**`data/` (8 arquivos, 342 KB)** — usados no boot:

| Base | Uso |
|---|---|
| `municipios_choices` | popular selectize de município (localidade/comparação) |
| `micro_r_saude_choices` | popular selectize de região de saúde |
| `macro_r_saude_choices` | popular selectize de macrorregião |
| `estados_choices` | choices de estado na UI |
| `tabela_indicadores` | catálogo mestre (selects do nível 3, cálculos, referências) |
| `tabela_aux_municipios` | IDHM, `grupo_kmeans` (municípios semelhantes), regiões/RS/macro |
| `df_cid10` | mapa CID-10 → subcategoria (garbage codes e causas no nível 3) |
| `tabela_radar` | rótulos dos gráficos de radar (blocos 2–5) |

**`inst/app/dados/` (45 parquets, 49,5 MB)** — carregados sob demanda, incluindo as 4 tabelas `bloco7_distribuicao_cids*` (a maior com 1.095 colunas) e todas as bases dos blocos.

---

## 7. Arquivos criados/alterados

| Arquivo | O que mudou |
|---|---|
| `R/acesso_dados.R` (novo) | bindings preguiçosos `.onLoad`, cache de parquet, `carrega_dados_projetado()` |
| `R/cache_interno.R` (novo) | cache em memória com chave hash, limites e evicção |
| `R/funcoes_globais.R` | `processa_bloco7` reescrito; `agrega_colunas_evitaveis`; `filtra_localidade_dt`; wrappers de cache; `processa_causas` em passada única; **(parte 2)** `cria_indicadores` em passada única, cache do `processa_bloco7` (`processa_bloco7_sem_cache` + `codigo_funcao()`), `processa_causas` em passada única; **(parte 3)** `soma_colunas_por_ano()` + `agrega_por_ano_rapido()` (matriz/`rowsum`), estreitamento de colunas e renomeação in place em `agrega_colunas_evitaveis`, caminho rápido no `processa_bloco7_sem_cache`; `attr_class_html()` + as 4 funções de caixa montando HTML como string única |
| `R/mod_nivel_3.R` | projeção Arrow no `base_bloco_selecionado`; download sob demanda; `bindCache` em 3 reativos; **(parte 2)** `data_tabela1`/download com condição fora do `summarise` |
| `R/app_ui.R` | remoção do `display:none` inline dos 5 `conditionalPanel`; correção do `src` do JS; **(parte 2)** UI pré-renderizada/cacheada (`constroi_app_ui()` + `.ui_renderizada`) |
| `DESCRIPTION` | `arrow` e `rlang` em Imports; **(parte 2)** `htmltools` em Imports |
| `data-raw/utils_parquet.R` (novo) | `usar_dados_parquet()` / `converter_rda_em_parquet()` |
| `data-raw/csv/blocos.R` | gravação em parquet das bases grandes; remoção de saídas mortas |
| `data-raw/extracao-dos-dados/cobertura/cobertura.R` | gravação em parquet das bases de cobertura/sub-registro |
| `data/*.rda` | 52 removidos (35 + 10 convertidos, 7 sem uso) |
| `inst/app/dados/*.parquet` (novo) | 45 bases |
| `dev/perf/` (novo, parte 2) | harness de medição: `drive.R` (cenários via chromote), `profile_app.R` (Rprof; `profile_app2.R` é o protótipo da UI cacheada), `bench_helpers.R`, `bench_*.R` e `validate_*.R` (equivalência de `cria_indicadores`/`processa_bloco7`/`data_tabela1`) |
| `dev/perf/validate_processa_causas.R` (novo, parte 3) | 73 comparações de `processa_causas` (bases × escopos × inputs × vazio) |
| `dev/perf/validate_cria_caixa_server.R` (novo, parte 3) | 60 comparações da caixa (argumentos × caminhos `TRUE`/`FALSE`, com correção do validador anterior) |
| `dev/perf/validate_caixas_bloco7.R` (novo, parte 3) | 32 comparações das caixas conjunta/principais do Bloco 7 |
| `dev/perf/validate_caixa_bloco5.R` (novo, parte 3) | 7 comparações da caixa conjunta do Bloco 5 |

---

## 8. Pendências e próximos passos sugeridos

1. **`highchartProxy`** nos gráficos do Nível 3 e Bloco 7 (incremental, com validação visual) — item do plano original não implementado.
2. **Formato longo esparso** para as 4 tabelas `bloco7_distribuicao_cids*` (95,4% zeros): reduz RAM e elimina o `melt` de ~19,5 M células por consulta.
3. **Paginação na reactable do nível 3** (`groupBy` renderiza milhares de municípios no navegador; o payload da tabela nacional é ~361 KB por busca e não muda com a paginação — o ganho é só de renderização no cliente).
4. **Corrigir os 2 observadores do `mod_nivel_3`** que emitem warning sem indicador selecionado (ver 4.5).
5. **Renderização de abas sob demanda**: o HTML inicial (625 KB, ~5,9 mil nós, 61 selects) carrega as 9 abas de uma vez; o gap medido entre `load` (~5 s) e `shiny:idle` (~13 s) aponta para inicialização de JS no cliente — prototipar UI preguiçosa por aba (fora do escopo desta rodada).
6. Rodar `R CMD check`/`devtools::check()` (as mudanças das partes 1–3 já foram commitadas).
7. **(parte 3) Montagem dos gráficos `highcharter`**: ~0,5–0,75 s por render de aba (medido nos traces); candidatos: `highchartProxy` (item 1) e evitar reprocessar a mesma data frame adicionada como 4–5 séries nos gráficos de distribuição.
8. **(parte 3) Pré-aquecer as sub-abas irmãs do Bloco 7 após uma busca** (via `later`): a troca de sub-aba cairia de ~3 s para ~1–1,5 s (o resto é render no navegador), mas custa ~1,5–2 s de CPU em background por busca — decisão de produto (multi-usuário no shinyapps.io).
9. **(parte 3) Caixas — ganhos remanescentes pequenos**: o wrapper `bs4Dash::box` (caminho "caixa completa") ainda responde por ~1,4 ms de build + ~1,4 ms de conversão por caixa; gerar o card como string eliminaria isso, mas acopla ao HTML interno do bs4Dash (não recomendado). Os observadores hide/show por caixa (~2 mensagens shinyjs cada) poderiam virar CSS, com risco visual médio.

---

## 9. Comandos úteis

```r
# rodar o app em desenvolvimento
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
painel.indicadores::run_app()

# regenerar as bases Parquet a partir do pipeline
source("data-raw/csv/blocos.R")

# validacoes de equivalencia (parte 2)
source("dev/perf/validate_cria_indicadores.R")   # 192 comparacoes
source("dev/perf/validate_processa_bloco7.R")    # 35 comparacoes + cache/vazio
source("dev/perf/validate_data_tabela1.R")       # 9 comparacoes (Nivel 3)

# validacoes de equivalencia (parte 3)
source("dev/perf/validate_processa_causas.R")    # 73 comparacoes (6 bases x 11 escopos + bordas)
source("dev/perf/validate_cria_caixa_server.R")  # 60 comparacoes (argumentos x TRUE/FALSE)
source("dev/perf/validate_caixas_bloco7.R")      # 32 comparacoes (conjunta + principais)
source("dev/perf/validate_caixa_bloco5.R")       # 7 comparacoes (baixo peso/prematuridade)
```

```powershell
# harness e2e (sobe o app, dirige o Chrome headless e para via stopfile)
# <cenario>: boot | boot2 | nivel1 | bloco1..bloco7 | bloco7_fetal | nivel3 | nivel3_b3 |
#            bloco4 | bloco4_desloc | bloco4_prof | duas_sessoes
Rscript dev/perf/drive.R bloco7 <saida.out> <saida.stop>
```

> Observação de deploy: `arrow` precisa estar disponível no ambiente do shinyapps.io (está em `Imports`; o rsconnect instala as dependências). O `deployApp.R` já envia `inst/` (com os parquets) e `data/` (auxiliares).
