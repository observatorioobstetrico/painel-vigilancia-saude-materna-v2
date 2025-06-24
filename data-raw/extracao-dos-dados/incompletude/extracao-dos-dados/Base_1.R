## pacotes
library(readr)
library(janitor)
library(dplyr)
library(lubridate)
library(forcats)
library(pander)
library(skimr)
require(survival)
require(truncnorm)
require(LaplacesDemon)
require(TeachingDemos)
require(coda)
library(foreign)
library("reshape2")
library(tidyr)
# setwd('extracao_dados_Samuel')

##### Incompletude SINASC raca

# CRIACAO TABELA BASE MUNICIPIOS COMPARACAO -------------------------------

dados_nasc <- read_delim("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/total_nascidos_CODMUNRES.csv",
                         delim = ",", escape_double = FALSE, trim_ws = TRUE) |>
  filter(ano >= 2012 & ano <= 2023)

dados_nasc_agr <- dados_nasc %>%
  rename(nasc = total_de_nascidos_vivos)

janitor::get_dupes(dados_nasc_agr, codmunres)

dados <- dplyr::distinct(dados_nasc_agr, ano, codmunres, .keep_all = TRUE)

#COMPARANDO COM BASE ANTIGA

# dados_nasc_antigo <- read_delim("bases/total_nascidos_CODMUNRES.csv",
#                                 delim = ",", escape_double = FALSE, trim_ws = TRUE)
# dados_nasc_agr_antigo <- dados_nasc_antigo %>% #select(-1) |>
#   rename(nasc = total_de_nascidos_vivos)
# sum(dados_nasc_agr |> filter(ano <= 2021 ) |> pull(NASC)) - sum(dados_nasc_agr_antigo$nasc)
#DEU DIFERENCA, TESTANDO AGORA PEGAR SOMENTE OS MUNICIPIOS UTILIZADO NO PAINEL DE VIGILANCIA

codigos_municipios <- read.csv("data-raw/csv/tabela_aux_municipios.csv") |>
  pull(codmunres)
#Criando um data.frame auxiliar que possui uma linha para cada combinação de município e ano
df_aux_municipios <- data.frame(codmunres = rep(codigos_municipios,
                                                each = length(2012:2023)),
                                ano = 2012:2023)

df_verificacao <- left_join(df_aux_municipios, dados_nasc_agr)
df_verificacao$nasc[is.na(df_verificacao$nasc)] <- 0

# sum(df_verificacao |> filter(ano < 2021 ) |> pull(NASC)) - sum(dados_nasc_agr_antigo$NASC)


# dados_nasc_agr_antigo$ano |> unique()

#AGORA FOI, USAR ESSE. E COMO SABEMOS QUE OS NUMERO DE NASC CONFERE,
#PODEMOS UTILIZAR ESSE BANCO DE DADOS PARA VERIFICACAO AO INVES DA BASE ANTIGA DE
#CADA VARIAVEL
dados_nasc_agr <- df_verificacao #%>%
  #rename(CODMUNRES = codmunres,
  #       ANO = ano,
  #       NASC = nasc)
rm(df_verificacao,codigos_municipios #,dados_nasc_antigo,dados_nasc_agr_antigo
   )

# RACACORMAE -----------------------------------------------------------------

raca_muni <- read.csv("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/RACACORMAE_muni.csv")

#esse passo é importante pq na API da PCDaS corremos pela UF de residência,
#mas usamos o municipio de ocorrência (CODMUNRES)
raca_muni_agr <- raca_muni %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_raca = sum(NASC)) |> clean_names()

## total de nascimentos das duas bases (de nascimentos e raca) coincidem
dados <- left_join(dados_nasc_agr, raca_muni_agr, by = c("ano", "codmunres"))

dados[is.na(dados$nasc_raca),"nasc_raca"] <- 0

# sum(dados$nasc-dados$nasc_raca)

# total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos
# guardar esse resultado em um objeto diferente para usar de comparacao para as outras variaveis

dados2_cor <- dados %>%
  group_by(ano) %>%
  summarise(n = sum(nasc))

# agora vamos só trabalhar com os dados faltantes
IGNORADOS <- raca_muni %>%
  filter(RACACORMAE  == 9)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES , ANO) %>%
  summarise(IGNORADOS = sum(NASC))

NULOS <- raca_muni %>%
  filter( RACACORMAE  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES , ANO) %>%
  summarise(NULOS = sum(NASC))

names(dados_nasc_agr)<- dados_nasc_agr |> names() |> toupper()

dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))

dados_RACACORMAE <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_RACACORMAE$variavel <- 'RACACORMAE'

#IMPORTACAO DOS BANCOS ------------------------------------------------------------------
variaveis <- c( "IDADEMAE" , "ESCMAE" ,"QTDPARTNOR",  "QTDPARTCES",
                "CONSPRENAT" ,"MESPRENAT" ,"PARTO" ,"TPROBSON" , "PESO" ,
                "GESTACAO" , "SEMAGESTAC", "IDANOMAL")

IDADEMAE   <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/IDADEMAE_muni.csv"),
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)
ESCMAE <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/ESCMAE_muni.csv"),
                     delim = ",", escape_double = FALSE, trim_ws = TRUE)
QTDPARTNOR <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/QTDPARTNOR_muni.csv"),
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)
QTDPARTCES <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/QTDPARTCES_muni.csv"),
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)
CONSPRENAT <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/CONSPRENAT_muni.csv"),
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)
MESPRENAT <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/MESPRENAT_muni.csv"),
                        delim = ",", escape_double = FALSE, trim_ws = TRUE)
PARTO <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/PARTO_muni.csv"),
                    delim = ",", escape_double = FALSE, trim_ws = TRUE)
TPROBSON <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/TPROBSON_muni.csv"),
                       delim = ",", escape_double = FALSE, trim_ws = TRUE)
PESO <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/PESO_muni.csv"),
                   delim = ",", escape_double = FALSE, trim_ws = TRUE)
GESTACAO <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/GESTACAO_muni.csv"),
                       delim = ",", escape_double = FALSE, trim_ws = TRUE)
SEMAGESTAC <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/SEMAGESTAC_muni.csv"),
                         delim = ",", escape_double = FALSE, trim_ws = TRUE)
IDANOMAL <- read_delim(("data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/IDANOMAL_muni.csv"),
                       delim = ",", escape_double = FALSE, trim_ws = TRUE)

# CONSPRENAT -----------------------------------------------

# AGREGANDO POR MUNICIPIO DA OCORRENCIA

# CONSPRENAT_agr_antigo <-  read_delim(("bases_antigas/CONSPRENAT_muni.csv"),
#                                      delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
#   group_by(UF,Municipio, Ano) %>%
#   summarise(nasc_CONSPRENAT = sum(Nascidos)) %>%
#   rename(CODMUNRES = Municipio,
#          ANO = Ano)
CONSPRENAT_agr <- CONSPRENAT %>%
  group_by(CODMUNRES , ANO) %>%
  summarise(nasc_CONSPRENAT = sum(NASC))


## CORRIGINDO O ERRO DE 2013

# CONSPRENAT_antigo <-  read_delim(("bases_antigas/CONSPRENAT_muni.csv"),
#                                  delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
#   rename(CODMUNRES = Municipio,
#          ANO = Ano,
#          NASC = Nascidos)
# CONSPRENAT <- CONSPRENAT |>
#   rbind(CONSPRENAT_antigo[CONSPRENAT_antigo$ANO == 2013,c('CODMUNRES', 'ANO', 'NASC','CONSPRENAT')])


### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, CONSPRENAT_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_CONSPRENAT %>% is.na(),"nasc_CONSPRENAT"] <- 0
(dados$NASC-dados$nasc_CONSPRENAT) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
(dados2$n - dados2_cor$n) %>% sum()

# agora vamos só trabalhar com os dados
IGNORADOS <- CONSPRENAT %>%
  filter(CONSPRENAT  == 'IGNORADO')

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))

NULOS <- CONSPRENAT %>%
  filter( CONSPRENAT  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))

dados_CONSPRENAT <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)

dados_CONSPRENAT$variavel <- 'CONSPRENAT'
# PARTO -------------------------------------------------------------------

# AGREGANDO POR MUNICIPIO DA OCORRENCIA
PARTO_agr <- PARTO %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_PARTO = sum(NASC))

### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, PARTO_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_PARTO %>% is.na(),"nasc_PARTO"] <- 0
(dados$NASC -dados$nasc_PARTO) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
(dados2$n - dados2_cor$n) %>% sum()

#CALCULANDO INCOMPLETUDE
IGNORADOS <- PARTO %>%
  filter(PARTO  == 9)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))

NULOS <- PARTO %>%
  filter( PARTO  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))

dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))

dados_PARTO <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_PARTO$variavel <- 'PARTO'

# TPROBSON ----------------------------------------------------------------
## CORRIGINDO O ERRO DE 2013
# TPROBSON_antigo <-  read_delim(("bases_antigas/TPROBSON_muni.csv"),
#                                delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
#   rename(CODMUNRES = Municipio,
#          ANO = Ano,
#          NASC = Nascidos)
# TPROBSON <- TPROBSON |>
#   rbind(TPROBSON_antigo[TPROBSON_antigo$ANO == 2013,c('CODMUNRES', 'ANO', 'NASC','TPROBSON')])
# AGREGANDO POR MUNICIPIO DA OCORRENCIA
TPROBSON_agr <- TPROBSON %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_TPROBSON = sum(NASC))

### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, TPROBSON_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_TPROBSON %>% is.na(),"nasc_TPROBSON"] <- 0
(dados$NASC-dados$nasc_TPROBSON) %>% sum()


#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
(dados2$n - dados2_cor$n) %>% sum()


#CALCULANDO INCOMPLETUDE.
IGNORADOS <- TPROBSON %>%
  filter(TPROBSON  == 11 | TPROBSON == 12)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))

NULOS <- TPROBSON %>%
  filter( TPROBSON  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_TPROBSON <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_TPROBSON$variavel <- 'TPROBSON'

# ESCMAE ------------------------------------------------------------------

# AGREGANDO POR MUNICIPIO DA OCORRENCIA
ESCMAE_agr <- ESCMAE %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_ESCMAE = sum(NASC))

### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, ESCMAE_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_ESCMAE %>% is.na(),"nasc_ESCMAE"] <- 0
(dados$NASC-dados$nasc_ESCMAE) %>% unique()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- ESCMAE %>%
  filter(ESCMAE  == 9)

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))


NULOS <- ESCMAE %>%
  filter( ESCMAE  %>% is.na() )

NULOS <-NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_ESCMAE <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_ESCMAE$variavel <- 'ESCMAE'



# IDADEMAE ----------------------------------------------------------------
# AGREGANDO POR MUNICIPIO DA OCORRENCIA
IDADEMAE_agr <- IDADEMAE %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_IDADEMAE = sum(NASC))


### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, IDADEMAE_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_IDADEMAE %>% is.na(),"nasc_IDADEMAE"] <- 0
(dados$NASC-dados$nasc_IDADEMAE) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- IDADEMAE %>%
  filter(IDADEMAE  == 'IGNORADO')

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))

NULOS <- IDADEMAE %>%
  filter( IDADEMAE  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))



dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_IDADEMAE <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_IDADEMAE$variavel <- 'IDADEMAE'

# PESO --------------------------------------------------------------------
# AGREGANDO POR MUNICIPIO DA OCORRENCIA
PESO_agr <- PESO %>%
  group_by(CODMUNRES, ANO)  %>%
  summarise(nasc_PESO = sum(NASC))


### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, PESO_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_PESO %>% is.na(),"nasc_PESO"] <- 0
(dados$NASC-dados$nasc_PESO) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- PESO %>%
  filter(PESO  == 'IGNORADO')

IGNORADOS <- IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))


NULOS <- PESO %>%
  filter( PESO  %>% is.na() )

NULOS <- NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_PESO <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_PESO$variavel <- 'PESO'

# GESTACAO ----------------------------------------------------------------
# AGREGANDO POR MUNICIPIO DA OCORRENCIA
GESTACAO_agr <- GESTACAO %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_GESTACAO = sum(NASC))

### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, GESTACAO_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_GESTACAO %>% is.na(),"nasc_GESTACAO"] <- 0
(dados$NASC-dados$nasc_GESTACAO) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- GESTACAO %>%
  filter(GESTACAO  == 9)

IGNORADOS <-  IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))

NULOS <- GESTACAO %>%
  filter( GESTACAO  %>% is.na() )

NULOS <-  NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_GESTACAO <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_GESTACAO$variavel <- 'GESTACAO'

# MESPRENAT ---------------------------------------------------------------
# AGREGANDO POR MUNICIPIO DA OCORRENCIA
MESPRENAT_agr <- MESPRENAT %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_MESPRENAT = sum(NASC))


### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, MESPRENAT_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_MESPRENAT %>% is.na(),"nasc_MESPRENAT"] <- 0
(dados$NASC-dados$nasc_MESPRENAT) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- MESPRENAT %>%
  filter(MESPRENAT  == 'IGNORADO')

IGNORADOS <-  IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))


NULOS <- MESPRENAT %>%
  filter( MESPRENAT  %>% is.na() )

NULOS <-  NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_MESPRENAT <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_MESPRENAT$variavel <- 'MESPRENAT'

# QTDPARTCES --------------------------------------------------------------

# AGREGANDO POR MUNICIPIO DA OCORRENCIA
QTDPARTCES_agr <- QTDPARTCES %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_QTDPARTCES = sum(NASC))

### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, QTDPARTCES_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_QTDPARTCES %>% is.na(),"nasc_QTDPARTCES"] <- 0
(dados$NASC-dados$nasc_QTDPARTCES) %>% unique()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- QTDPARTCES %>%
  filter(QTDPARTCES  == 99)

IGNORADOS <-  IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))


NULOS <- QTDPARTCES %>%
  filter( QTDPARTCES  %>% is.na() )

NULOS <-NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_QTDPARTCES <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_QTDPARTCES$variavel <- 'QTDPARTCES'
# QTDPARTNOR --------------------------------------------------------------

# AGREGANDO POR MUNICIPIO DA OCORRENCIA
QTDPARTNOR_agr <- QTDPARTNOR %>%  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_QTDPARTNOR = sum(NASC))


### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, QTDPARTNOR_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_QTDPARTNOR %>% is.na(),"nasc_QTDPARTNOR"] <- 0
(dados$NASC-dados$nasc_QTDPARTNOR) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- QTDPARTNOR %>%
  filter(QTDPARTNOR  == 99)

IGNORADOS <-  IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))

NULOS <- QTDPARTNOR %>%
  filter( QTDPARTNOR  %>% is.na() )


NULOS <-NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_QTDPARTNOR <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_QTDPARTNOR$variavel <- 'QTDPARTNOR'
# SEMAGESTAC --------------------------------------------------------------

# AGREGANDO POR MUNICIPIO DA OCORRENCIA
SEMAGESTAC_agr <- SEMAGESTAC %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_SEMAGESTAC = sum(NASC))

### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, SEMAGESTAC_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_SEMAGESTAC %>% is.na(),"nasc_SEMAGESTAC"] <- 0
(dados$NASC-dados$nasc_SEMAGESTAC) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- SEMAGESTAC %>%
  filter(SEMAGESTAC  == 'IGNORADO')

IGNORADOS <-  IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))


NULOS <- SEMAGESTAC %>%
  filter( SEMAGESTAC  %>% is.na() )

NULOS <-NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_SEMAGESTAC <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_SEMAGESTAC$variavel <- 'SEMAGESTAC'

# IDANOMAL -----------------------------------------------------------------

# AGREGANDO POR MUNICIPIO DA OCORRENCIA
IDANOMAL_agr <- IDANOMAL %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(nasc_IDANOMAL = sum(NASC))

### número total de nascimentos das  duas bases coincidem
dados <- left_join(dados_nasc_agr, IDANOMAL_agr, by = c("ANO", "CODMUNRES"))
dados[ dados$nasc_IDANOMAL %>% is.na(),"nasc_IDANOMAL"] <- 0
(dados$NASC-dados$nasc_IDANOMAL) %>% sum()

#total de nascimentos coincide com o total de NASC vivos do painel de Indicadores Obstétricos:
dados2 <- dados %>%
  group_by(ANO) %>%
  summarise(n = sum(NASC))
sum(dados2$n - dados2_cor$n)

#CALCULANDO INCOMPLETUDE.
IGNORADOS <- IDANOMAL %>%
  filter(IDANOMAL  == 'IGNORADO')

IGNORADOS <-  IGNORADOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(IGNORADOS = sum(NASC))


NULOS <- IDANOMAL %>%
  filter( IDANOMAL  %>% is.na() )

NULOS <-NULOS %>%
  group_by(CODMUNRES, ANO) %>%
  summarise(NULOS = sum(NASC))


dados <- full_join(NULOS, IGNORADOS, by = c("CODMUNRES","ANO")) %>%
  right_join(dados_nasc_agr, by = c("ANO", "CODMUNRES"))
dados_IDANOMAL <- dados %>%
  mutate(NULOS =  ifelse(is.na(NULOS), 0, NULOS),
         IGNORADOS =  ifelse(is.na(IGNORADOS), 0, IGNORADOS)) %>% rename(TOTAIS = NASC)
dados_IDANOMAL$variavel <- 'IDANOMAL'

# JUNCAO ------------------------------------------------------------------


dados_final <- bind_rows(dados_CONSPRENAT,dados_PARTO ,dados_TPROBSON,
                         dados_RACACORMAE,dados_GESTACAO,dados_MESPRENAT,dados_QTDPARTCES,
                         dados_QTDPARTNOR,dados_SEMAGESTAC,
                         dados_ESCMAE ,dados_IDADEMAE ,dados_PESO, dados_IDANOMAL)
dados_final$aux <- dados_final$CODMUNRES %>% substr(start = 1,stop =2)
UFS <- data.frame(
  COD = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
  UF = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA',
         'MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF'))
dados_final$CODMUNRES <- dados_final$CODMUNRES %>% as.character()
dados_final <-merge(dados_final, UFS, by.x = "aux", by.y = "COD", all.x = TRUE)
dados_final_2 <- dados_final %>% select(-c(aux))

#VERIFICANDO COM BASE ANTIGA
# base_1_antiga <- read.csv("Base_1_2012-2022.csv")
# sum(dados_final_2 |> filter(ANO < 2021 ) |> pull(NULOS)) - sum(base_1_antiga$NULOS)
#
# sum(dados_final_2 |> filter(ANO < 2021 ) |> pull(IGNORADOS)) - sum(base_1_antiga$IGNORADOS)
#
# sum(dados_final_2 |> filter(ANO < 2021 ) |> pull(TOTAIS)) - sum(base_1_antiga$TOTAIS)
#
# teste <- full_join(dados_final_2 |>
#                      mutate(CODMUNRES = as.integer(CODMUNRES)) |>
#                      filter(ANO < 2021),
#                    base_1_antiga |>
#                      select(- X), by = c("CODMUNRES","ANO","variavel","TOTAIS", "UF") )
#
# diferentes <- teste[teste$NULOS.x - teste$NULOS.y  + teste$IGNORADOS.x - teste$IGNORADOS.y !=0,]
# diferentes$ANO |> unique()
# diferentes$variavel |> unique()


write.csv(dados_final_2,"data-raw/extracao-dos-dados/incompletude/extracao-dos-dados/bases_novas/Base_1_2012-2023_v2.csv")
# dados_final_2[dados_final_2$TOTAIS %>% is.na()]
