# Painel de Vigilância da Saúde Materna

![Badge em Desenvolvimento](http://img.shields.io/static/v1?label=STATUS&message=EM%20DESENVOLVIMENTO&color=GREEN&style=for-the-badge)

## Índice

- [Descrição](#descrição)
- [Instalação](#instalação)
- [Como Usar](#como-usar)
- [Estrutura do Projeto](#estrutura-do-projeto)
    - [Pastas data-raw e data](##pastas-data-raw-e-data)

## Descrição

Este repositório contém todos os códigos utilizados para a extração dos dados e para a construção do Painel de Vigilância da Saúde Materna, um painel de visualização em Shiny construído a partir do framework `{golem}`, no R. O painel é formado por um conjunto de indicadores que expressam a vulnerabilidade das mulheres ao óbito materno, seja por suas condições socioeconômicas, seja pela atenção à saúde recebida ao longo da sua vida reprodutiva. O objetivo do projeto é criar uma interface amigável para a visualização desses indicadores, de modo que eles possam ser explorados pelos gestores de saúde para identificar áreas prioritárias para investimento em seu município/estado. 

## Instalação

Antes de começar a utilizar o painel localmente, instale os pré-requisitos necessários e siga os passos abaixo para configurar o projeto em seu computador.

### Pré-requisitos

- [R](https://www.r-project.org/)
- [RStudio](https://www.rstudio.com/)

### Passos para a instalação

1. Utilizando o shell de sua preferência, clone o repositório do projeto:
    ```bash
    git clone https://github.com/seu-usuario/seu-repositorio.git
    cd seu-repositorio
    ```

2. Abra o arquivo `painel.indicadores.Rproj` e instale, no R, as dependências do projeto:
    ```r
    install.packages("devtools")
    devtools::install_deps()
    ```

## Como usar o aplicativo

Para abrir o aplicativo, seja localmente ou virtualmente, siga as instruções abaixo.

### Executando o aplicativo localmente

1. Abra o projeto do R `painel.indicadores.Rproj` no RStudio.
2. Aperte simultaneamente as teclas `ctrl + shift + L`, ou execute o seguinte comando no console do R, para carregar o conteúdo do aplicativo:
    ```r
    devtools::load_all(".")
    ```
3. Rode o aplicativo Shiny executando o seguinte comando no console do R:
    ```r
    run_app()
    ```

### Acessando o aplicativo via shinyapps

Para acessar a versão mais recente do aplicativo virtualmente, acesse o [link do shinyapps.io](https://observatorioobstetrico.shinyapps.io/painel-vigilancia-saude-materna-v2/) no qual o painel está hospedado.

## Estrutura do projeto

Por ser um aplicativo criado dentro do framework golem, o diretório principal do projeto se assemelha ao que se encontraria em um pacote qualquer do R. Abaixo, pode ser vista uma breve descrição da estrutura de diretórios e dos arquivos principais utilizados pelo aplicativo. É importante citar que, para rodar qualquer um dos scripts citados, é necessário estar dentro do projeto do R `painel.indicadores.Rproj`.

### Pastas data-raw e data

A pasta `data-raw` contém os códigos utilizados para se obter todas as bases de dados que alimentam o painel, bem como os arquivos `.csv` gerados ao final de cada processo. Essa pasta é composta por duas subpastas, denominadas `extracao-dos-dados` - na qual estão os códigos utilizados para a geração de cada base de dados - e `csv` - na qual estão os arquivos `.csv` gerados. Dentro de `extracao-dos-dados`, se encontram:

- Uma pasta denominada `blocos`, composta pelos scripts em R que baixam, tratam e salvam as bases utilizadas para o cálculo dos indicadores de cada um dos sete blocos do painel. Cada base contém uma linha para cada combinação de município e ano considerados dentro do painel;

- Uma pasta denomidada `cobertura`, composta pelo script `cobertura.R`, responsável por ler e tratar as planilhas `.ods` que contêm os dados anuais de sub-registro do SIM e do SINASC. Como resultado, o script gera duas bases: uma com os dados de sub-registro dos municípios e outra com os dados de estados, regiões e Brasil. Assim como as bases dos blocos, cada uma dessas duas bases contêm uma linha para cada combinação de município (ou estado/região/Brasil) e ano considerados dentro do painel;

- E uma pasta denominada `incompletude`, composta pelos scripts em R que geram uma base contendo dados referentes à incompletude de todas as variáveis do SINASC utilizadas para o cálculo de algum dos indicadores do painel. Assim como as bases anteriores, a base de incompletude contém uma linha para cada combinação de município e ano considerados dentro do painel.

Todas as bases geradas pelos scripts citados acima são guardadas em formato `.csv` dentro da pasta `csv`, que contém, também, um script em R denominado `blocos.R`. Esse script é responsável por ler os arquivos `.csv`, realizar quaisquer tratamentos adicionais necessários (como incluir colunas contendo informações adicionais sobre cada município) e gerar os arquivos `.rda` que se encontram dentro da pasta `data`, no diretório principal do projeto. Quando o usuário carrega o conteúdo do aplicativo (através do atalho `ctrl + shift + L`), os arquivos `.rda` podem ser acessados normalmente dentro do R, como se fossem data frames salvos no ambiente.

### Pasta R

A pasta `R` contém os scripts que compõem, de fato, o aplicativo Shiny. Como o projeto utiliza o framework `{golem}`, cada "página" do aplicativo é separada em um arquivo diferente, formando os diversos módulos do Shiny que existem dentro da pasta. Os arquivos `app_ui.R` e `app_server.R` recebem, respectivamente, os elementos que formam a UI e o servidor compartilhados entre todas as páginas do aplicativo, como a estrutura base do dashboard - criada a partir do pacote {bs4Dash} - e as opções de filtro que existem em cada página. Por outro lado, os arquivos que começam com "mod" contêm as UIs e os servidores individuais para cada página do aplicativo. Mais especificamente,

- o arquivo `mod_nivel_1.R` contém a UI e o servidor individuais do nível 1 do painel ("Resumo dos blocos de indicadores");

- os arquivos que começam com `mod_bloco` contêm as UIs e os servidores individuais de cada um dos blocos do nível 2 do painel ("Séries históricas");

- e o arquivo `mod_nivel_3.R` contém a UI e o servidor individuais do nível 3 do painel ("Visão detalhada dos indicadores").

Por fim, a pasta R contém, também, os arquivos `mod_sobre.R` e `mod_documentacao.R`, que recebem as UIs e os servidores individuais utilizados para a criação do conteúdo das abas "Sobre o painel" e "Documentação dos indicadores", respectivamente; e o arquivo `funcoes_globais.R`, na qual são criadas algumas funções do R utilizadas ao longo de todo o painel, como a função que cria as caixinhas do "Resumo do período", a função que cria os modais de incompletude e a função que cria o relatório dos indicadores (disponível no nível 1 do painel).

### Pasta inst

A pasta `inst` contém, em geral, arquivos relacionados ao visual do painel, como imagens e scripts `.css` e `.html`. No caminho `inst/app/www`, se encontram:

- A pasta `global`, que contém um arquivo `.css` que define alguns elementos visuais do painel e um arquivo `.js` com algumas funções utilizadas em todo o aplicativo;

- A pasta `html`, que contém os arquivos `.html` utilizados para a criação das caixinhas existentes na aba "Documentação do indicador", encontrada no nível 3 do painel;

- A pasta `logos`, que contém as logos utilizadas na aba "Sobre o painel";

- E a pasta `report`, que contém os arquivos `.css`, `.html` e as imagens envolvidos na geração do relatório dos indicadores, disponível no nível 1 do painel.

### Outros arquivos e pastas

Além das pastas e arquivos citados acima, o diretório principal do painel contém, também, o arquivo `report.Rmd`, utilizado para a criação do relatório interativo que contém o resumo dos indicadores para o ano selecionado (disponível no nível 1 do painel). Outras pastas e arquivos não mencionados são referentes a documentos que o próprio pacote `{golem}` cria durante o processo inicial da geração do projeto. 
