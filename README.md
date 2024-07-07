# Painel de Vigilância da Saúde Materna

![Badge em Desenvolvimento](http://img.shields.io/static/v1?label=STATUS&message=EM%20DESENVOLVIMENTO&color=GREEN&style=for-the-badge)

## Índice

- [Descrição](#descrição)
- [Instalação](#instalação)
- [Como Usar](#como-usar)
- [Estrutura do Projeto](#estrutura-do-projeto)

## Descrição

Este repositório contém todos os códigos utilizados para a extração dos dados e para a construção do Painel de Vigilância da Saúde Materna, um painel de visualização em Shiny construído a partir do framework golem, no R. O painel é formado por um conjunto de indicadores que expressam a vulnerabilidade das mulheres ao óbito materno, seja por suas condições socioeconômicas, seja pela atenção à saúde recebida ao longo da sua vida reprodutiva. O objetivo do projeto é criar uma interface amigável para a visualização desses indicadores, de modo que eles possam ser explorados pelos gestores de saúde para identificar áreas prioritárias para investimento em seu município/estado. 

## Instalação

Instruções para instalar configurar o ambiente do projeto.

### Pré-requisitos

- [R](https://www.r-project.org/)
- [RStudio](https://www.rstudio.com/)

### Passos para Instalação

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

## Como Usar

Instruções para rodar o aplicativo localmente.

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

