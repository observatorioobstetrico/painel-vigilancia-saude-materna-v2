#' sobre UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_sobre_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML("<h2><b>Painel de Vigilância da Saúde Materna</b></h2>"),
    fluidRow(
      HTML(
        "
          <p align='justify'; style='font-size:18px'>
          A morte materna – morte de uma mulher durante a gestação, parto ou até 42 dias após o
          término da gestação – não é um evento isolado. Ela é o resultado de uma série de
          determinantes sociais em saúde que atuam durante a vida de uma mulher.

          <span style='display: block; margin-bottom: 12px;'> </span>

          Neste painel, você encontrará um conjunto de indicadores que expressam a vulnerabilidade das
          mulheres ao óbito materno, seja por suas condições socioeconômicas, seja pela atenção à saúde
          recebida ao longo da sua vida reprodutiva. Os indicadores estão disponíveis para municípios, estados, regiões de saúde e país e foram
          calculados a partir de vários sistemas de informação brasileiros (SINASC, SIM, SINAN, SIH, SIAB,
          ANS e estimativas populacionais). O painel contém indicadores para o período 2012 a 2020 e
          será atualizado anualmente.

          <span style='display: block; margin-bottom: 12px;'> </span>

          Os indicadores têm fácil visualização e podem ser explorados pelos gestores de saúde para
          identificar áreas prioritárias para investimento em seu município/estado. Para auxiliar na interpretação dos indicadores, são apresentados valores de referência, que
          podem ser metas nacionais ou internacionais, recomendações da Organização Mundial da Saúde, valores observados em países desenvolvidos ou a média nacional.

          <span style='display: block; margin-bottom: 12px;'> </span>

          Em seu menu lateral, o painel está organizado em três níveis:
          </p>
          <ol align='justify'; style='font-size:18px; font-weight: bold;'>
            <li> Resumo dos blocos de indicadores: <span style = 'font-weight: normal;'>nesse nível, são mostrados todos os indicadores, de todos os blocos, em uma única tela, para o ano e região geográfica selecionados.
                 Um relatório com o resumo dos indicadores está disponível para impressão;</span></li>
            <li> Séries históricas: <span style = 'font-weight: normal;'>nesse nível, é mostrada a evolução dos indicadores de cada bloco para a localidade e período de tempo selecionados, sendo possível a comparação entre
                 localidades, situando o município/estado/região no cenário nacional;</span></li>
            <li> Visão detalhada dos indicadores: <span style = 'font-weight: normal;'>nesse nível, cada indicador pode ser visualizado individualmente e comparado a outras localidades.
                 Deve-se primeiro selecionar o bloco ao qual o indicador pertence e depois o indicador escolhido. Além das visualizações, a documentação de cada indicador é apresentada de forma resumida: sua definição, a fonte dos dados, o método de cálculo e informações sobre a qualidade da informação são apresentados,</span></li>
          </ol>

          <p align='justify'; style='font-size:18px'>

          Além disso, ainda no menu lateral, também estão disponíveis:
          </p>

          <ol start='4', align='justify'; style='font-size:18px; font-weight: bold;'>
            <li> Documentação dos indicadores: <span style = 'font-weight: normal;'> nesse munu, disponibilizamos um documento para impressão, contendo a documentação completa de todos os indicadores do painel;</span></li>
            <li> A história da Aparecida: <span style = 'font-weight: normal;'>nesse menu, disponibilizamos um link para a \"História de Aparecida\", onde mostramos os indicadores apresentados neste painel em diferentes contextos e em como eles refletem as situações de vulnerabilidade da mulher ao óbito materno, permitindo maior conhecimento sobre a morte materna e seus determinantes.</span></li>
          </ol>


          <p align='justify'; style='font-size:18px'>
          Para análises adicionais não disponíveis neste painel, acesse o site do Tabnet/DATASUS, disponível em <a href = https://datasus.saude.gov.br/informacoes-de-saude-tabnet/, target = _blank>https://datasus.saude.gov.br/informacoes-de-saude-tabnet/</a>.
          </p>

          <span style='display: block; margin-bottom: 12px;'> </span>
          "
      )
    ),
    fluidRow(
      column(
        width = 12,
        HTML(
          "
          <h3> <b> Realização </b> </h2>

          <div class = 'outer'>
            <div class = 'inner'>
              <img src = 'www/logos/realizacao/logo_oobr.png' width = '170px' style = 'margin-right:20px'>
              <img src = 'www/logos/realizacao/logo_fiocruz.png' width = '190px'>
              <img src = 'www/logos/realizacao/logo_ufes.png' width = '160px'>
              <img src = 'www/logos/realizacao/logo_ufrj.png' width = '180px'>
              <img src = 'www/logos/realizacao/logo_medicina_usp.png' width = '170px'>
            </div>
          </div>

          "
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        HTML(
          "
          <h3> <b> Financiadores  </b> </h2>

          <div class = 'outer'>
            <div class = 'inner'>
              <img src = 'www/logos/financiadores/logo_bill_melinda.png' width = '190px' style = 'margin-right:20px'>
              <img src = 'www/logos/financiadores/logo_cnpq.png' width = '160px'>
              <img src = 'www/logos/financiadores/logo_ms.png' width = '450px'>
              <img src = 'www/logos/financiadores/logo_fapes.png' width = '480px'>
            </div>
          </div>

          "
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        HTML(
          "
          <h3> <b> Apoio </b> </h2>

          <div class = 'outer'>
            <div class = 'inner'>
              <img src = 'www/logos/apoio/logo_pcdas.png' width = '150px' style = 'margin-right:20px'>
              <img src = 'www/logos/apoio/logo_odd.png' width = '130px'>
            </div>
          </div>

          "
        )
      )
    )
  )

}


#' sobre Server Functions
#'
#' @noRd
mod_sobre_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sobre_ui("sobre_1")

## To be copied in the server
# mod_sobre_server("sobre_1")
