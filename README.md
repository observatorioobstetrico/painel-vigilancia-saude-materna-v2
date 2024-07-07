# Painel de Vigilância da Saúde Materna

![Badge em Desenvolvimento](http://img.shields.io/static/v1?label=STATUS&message=EM%20DESENVOLVIMENTO&color=GREEN&style=for-the-badge)

## Índice

- [Descrição](#descrição)
- [Instalação](#instalação)
- [Como Usar](#como-usar)
- [Estrutura do Projeto](#estrutura-do-projeto)

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

A pasta data-raw contém os códigos utilizados para se obter todas as bases de dados que alimentam o painel, bem como os arquivos `.csv` gerados ao final de cada processo. Essa pasta é composta por duas subpastas, denominadas `extracao-dos-dados` - na qual estão os códigos utilizados para a geração de cada base de dados - e `csv` - na qual estão os arquivos `.csv` gerados. Dentro de `extracao-dos-dados`, se encontram:
    - Uma pasta denominada `blocos`, composta pelos scripts em R que baixam, tratam e salvam as bases utilizadas para o cálculo dos indicadores de cada bloco do painel. Essas bases contêm uma linha para cada combinação de município e ano considerados dentro do painel;
    - Uma pasta denomidada `cobertura`, composta pelo script `cobertura.R`, responsável por ler e tratar as planilhas `.ods` que contêm os dados anuais de sub-registro do SIM e do SINASC. Como resultado, o script gera duas bases: uma com os dados de sub-registro dos municípios e outra com os dados de estados, regiões e Brasil. Cada base contém uma linha para cada combinação de município (ou estado/região/Brasil) e ano considerados dentro do painel;
    - E uma pasta denominada `incompletude`, composta pelos scripts em R que geram uma base contendo dados referentes à incompletude de todas as variáveis do SINASC utilizadas para o cálculo de algum dos indicadores do painel. Assim como as bases anteriores, a base de incompletude contém uma linha para cada combinação de município e ano considerados dentro do painel.


