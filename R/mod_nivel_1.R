#' nivel_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @import bs4Dash
#'
#' @import highcharter
mod_nivel_1_ui <- function(id) {
  ns <- NS(id)
  options(
    spinner.color = "grey",
    spinner.color.background = "#ffffff",
    spinner.size = 0.5,
    spinner.type = 6
  )
  tagList(
    div(
      class = "div-titulo",
      HTML("<span style='display: block; margin-bottom: 15px;'> </span>"),
      h2(
        tags$b(
          HTML("Resumo dos blocos de indicadores"),
          htmlOutput(ns("titulo_localidade"), inline = TRUE)
        ),
        class = "fonte-titulos-pagina",
        style = "padding-left: 0.4em"
      ),
      hr(style = "margin-bottom: 0px;")
    ),
    HTML("<span style='display: block; margin-bottom: 20px;'> </span>"),
    fluidRow(
      column(
        width = 12,
        shinyWidgets::downloadBttn(
          outputId = ns("report"),
          color = "primary",
          label = HTML(
            "<span class = 'fonte-muito-grande'> &nbsp; Fazer download do relatório contendo o resumo dos indicadores para a localidade e ano escolhidos </span>"
          ),
          style = "unite",
          size = "sm"
        ),
        align = "center"
      )
    ),
    HTML("<span style='display: block; margin-bottom: 20px;'> </span>"),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(
            HTML(
              "1 - Condições socioeconômicas e de acesso ao serviço de saúde &nbsp;"
            ),
            class = 'fonte-titulos-modal',
            a(
              icon("chart-line", color = "#007bff"),
              onclick = "openTab('bloco_1')",
              href = "#"
            )
          ),
          #icon = icon("1"),
          status = "primary",
          collapsible = FALSE,
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' class = 'fonte-muito-grande'>
                As condições socioeconômicas e de acesso a serviços de saúde são os determinantes mais distais
                do óbito materno, por interferirem no futuro da saúde reprodutiva.
                <span style='display: block; margin-bottom: 14px;'> </span>
                A garantia de acesso a serviços de saúde de qualidade e políticas integradas visando a promoção
                da saúde podem diminuir o risco de morte materna.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Neste bloco, o gestor pode acompanhar as condições socioeconômicas e de acesso a serviços de
                saúde do seu município através dos indicadores apresentados.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Ver série histórica dos indicadores </span>"
                  ),
                  onclick = "openTab('bloco_1')",
                  href = "#"
                )
              ),
              HTML(
                "<span style='display: block; margin-bottom: 1em;'> </span>"
              ),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b1"),
                  label = HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Como interpretar os indicadores desse bloco? </span>"
                  ),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b1_i1")),
                    proxy.height = "270px"
                  )
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b1_i3")),
                    proxy.height = "270px"
                  )
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b1_i4")),
                    proxy.height = "270px"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b1_i2")),
                    proxy.height = "360px"
                  )
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b1_i5")),
                    proxy.height = "360px"
                  )
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b1_i6")),
                    proxy.height = "360px"
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              shinycssloaders::withSpinner(
                highcharter::highchartOutput(ns("plot3"), height = "280px"),
                proxy.height = "340px"
              )
            ),
            column(
              width = 8,
              fluidRow(
                column(
                  width = 6,
                  shinycssloaders::withSpinner(
                    highcharter::highchartOutput(ns("plot1"), height = "280px"),
                    proxy.height = "340px"
                  )
                ),
                column(
                  width = 6,
                  shinycssloaders::withSpinner(
                    highcharter::highchartOutput(ns("plot2"), height = "280px"),
                    proxy.height = "340px"
                  )
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(
            HTML("2 - Planejamento reprodutivo &nbsp;"),
            class = 'fonte-titulos-modal',
            a(
              icon("chart-line", color = "#007bff"),
              onclick = "openTab('bloco_2')",
              href = "#"
            )
          ),
          #icon = icon("2"),
          status = "primary",
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' class = 'fonte-muito-grande'>
                O acesso ao planejamento reprodutivo e a prevenção de gestações indesejadas ou de alto risco
                é fundamental para que as mulheres tenham uma gestação segura.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Dados sobre o acesso a métodos contraceptivos não estão disponíveis nos sistemas de informação
                brasileiros de uso rotineiro.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Os indicadores deste bloco apresentam as necessidades não atendidas de contracepção de forma indireta,
                por meio da taxa de fecundidade em adolescentes, da porcentagem de mulheres com número elevado de partos,
                e das taxas de aborto inseguro.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Ver série histórica dos indicadores </span>"
                  ),
                  onclick = "openTab('bloco_2')",
                  href = "#"
                )
              ),
              HTML(
                "<span style='display: block; margin-bottom: 1em;'> </span>"
              ),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b2"),
                  label = HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Como interpretar os indicadores desse bloco? </span>"
                  ),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              fluidRow(
                column(
                  offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b2_i1")),
                    proxy.height = "270px"
                  )
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b2_i2")),
                    proxy.height = "270px"
                  )
                ),
                column(
                  offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b2_i3")),
                    proxy.height = "270px"
                  )
                ),
                column(
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b2_i4")),
                    proxy.height = "280px"
                  )
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          title = tags$b(
            HTML("3 - Assistência pré-natal &nbsp;"),
            class = 'fonte-titulos-modal',
            a(
              icon("chart-line", color = "#007bff"),
              onclick = "openTab('bloco_3')",
              href = "#"
            )
          ),
          #icon = icon("3"),
          status = "primary",
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' class = 'fonte-muito-grande'>
                A assistência pré-natal é uma ação de saúde efetiva para a redução da mortalidade materna ao permitir:
                <ul align='justify' class = 'fonte-muito-grande'>
                  <li> O diagnóstico e o tratamento precoce de doenças pré-existentes e de complicações na gravidez (tais como
                  hipertensão arterial, diabetes, sífilis e outras doenças infecciosas); </li>
                  <li> A adoção de medidas preventivas, como vacinas e suplementos alimentares; </li>
                  <li> O fornecimento de orientações e preparação para o parto e o aleitamento
                  materno, bem como para redução/cessação do fumo, do uso do álcool e de outras drogas.</li>
                </ul>
                </p>

                <p align='justify' class = 'fonte-muito-grande'>
                Neste bloco, o gestor pode acompanhar a porcentagem de mulheres que receberam assistência pré-natal, a porcentagem
                de mulheres com início do acompanhamento pré-natal até 12 semanas gestacionais, a porcentagem de mulheres que recebeu o número mínimo
                de 8 consultas recomendado pela Organização Mundial de Saúde, e a porcentagem de mulheres que recebeu o número
                adequado de consultas, considerando a idade gestacional no parto. Pode também avaliar a incidência de sífilis congênita,
                que é considerado um evento sentinela da qualidade da assistência pré-natal, por ser um desfecho negativo evitável
                com ações realizadas exclusivamente durante essa assistência.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Ver série histórica dos indicadores </span>"
                  ),
                  onclick = "openTab('bloco_3')",
                  href = "#"
                )
              ),
              HTML(
                "<span style='display: block; margin-bottom: 1em;'> </span>"
              ),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b3"),
                  label = HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Como interpretar os indicadores desse bloco? </span>"
                  ),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              fluidRow(
                column( #Cobertura de assistência pré-natal
                  #offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b3_i1")),
                    proxy.height = "270px"
                  )
                ),
                column( #Porcentagem de mulheres com início do pré-natal até 12 semanas de gestação
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b3_i2")),
                    proxy.height = "270px"
                  )
                ),

                column( #Porcentagem de mulheres com oito ou mais consultas de pré-natal
                  # offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b3_i5")),
                    proxy.height = "280px"
                  )
                )
              ),
              fluidRow(
                column( #Porcentagem de mulheres com número adequado de consultas de pré-natal para a idade gestacional no parto
                  offset = 2,
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b3_i3")),
                    proxy.height = "280px"
                  )
                ),
                column( #Incidência de sífilis congênita por mil nascidos vivos
                  width = 4,
                  shinycssloaders::withSpinner(
                    uiOutput(ns("caixa_b3_i4")),
                    proxy.height = "280px"
                  )
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width = 12,
          style = "padding-bottom: 100px",
          title = tags$b(
            HTML("4 - Assistência ao parto &nbsp;"),
            class = 'fonte-titulos-modal',
            a(
              icon("chart-line", color = "#007bff"),
              onclick = "openTab('bloco_4')",
              href = "#"
            )
          ),
          #icon = icon("4"),
          status = "primary",
          fluidRow(
            column(
              width = 5,
              HTML(
                "
                <p align='justify' class = 'fonte-muito-grande'>
                A adequada assistência ao parto, com recursos disponíveis e atendimento oportuno, é essencial para
                o manejo de complicações e redução da mortalidade materna, fetal e neonatal. O uso apropriado de tecnologias de assistência
                de saúde e o cuidado centrado nas necessidades da mulher e da sua família estão entre as recomendações mais
                recentes da Organização Mundial de Saúde para a assistência ao parto.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Os indicadores de assistência ao parto são apresentados em três abas:

                <span style='display: block; margin-bottom: 14px;'> </span>
                Deslocamento para parto: nessa aba, apresentamos
                a porcentagem de mulheres que precisam se deslocar de seu município de residência para ter
                assistência ao parto e a distância percorrida para chegar nos serviços de saúde de alta e
                baixa complexidade. Para o nível de análise municipal, apresentamos duas informações adicionais:
                município com primeira, segunda e terceira maior proporção de partos fora do município de residência da mulher
                e hospital com maior número de partos ocorridos fora do município de residência da mulher. Também é apresentada
                a porcentagem de nascidos vivos com peso ao nascer < 1500g segundo local de ocorrência do parto e disponibilidade
                de leitos de UTI neonatal. Essas informações podem auxiliar os gestores municipais e estaduais a verificar se
                a regionalização da atenção ao parto está ocorrendo conforme o planejado, ou seja, se os partos estão ocorrendo
                em serviços e municípios previstos na regionalização e atendendo à orientação de que RN com peso ao nascer < 1500 g
                devem nascer em local com disponibilidade de leitos de UTI neonatal.

                <span style='display: block; margin-bottom: 14px;'> </span>
                Profissional e local do parto: nessa aba apresentamos a porcentagem de nascimentos segundo local de ocorrência (hospital, outro
                estabelecimento de saúde, domicílio, aldeia indígena, sem informação, outros) e a porcentagem de partos vaginais hospitalares
                segundo tipo de profissional.
                A assistência ao parto por profissionais capacitados é uma das principais estratégias para a redução da mortalidade materna e perinatal.
                Partos hospitalares ocorrem em um ambiente onde existem profissionais capacitados e, em teoria, são assistidos por esses profissionais.
                Partos não hospitalares podem ser planejados e serem assistidos por profissionais capacitados, mas também podem ocorrer sem assistência.
                Uma porcentagem elevada de partos não hospitalares deverá ser investigada localmente para avaliar se foram partos sem assistência.
                Existem evidências científicas de que a assistência ao parto vaginal por enfermeiras e obstetrizes está associada a melhores resultados
                maternos e perinatais, ao menor uso de intervenções no trabalho de parto e parto, além da maior satisfação com a experiência de parto.
                Sendo assim, em um contexto de reforma do modelo de atenção obstétrica, é recomendável o aumento da atuação de enfermeiras e obstetrizes
                na assistência ao parto vaginal, conforme as revisões sistemáticas e os posicionamento da Organização Mundial da Saúde Como valor de
                referência, utilizamos a porcentagem de partos por enfermeiras observada em países com elevada participação da enfermagem no modelo de
                atenção ao parto,  taxas adequadas de cesariana e baixas taxas de mortalidade materna e perinatal.

                <span style='display: block; margin-bottom: 14px;'> </span>
                Grupo de Robson: nessa aba, apresentamos a porcentagem total de nascimentos por cesarianas, bem como a
                distribuição das mulheres segundo grupos de Robson, a taxa de cesariana em cada grupo de
                Robson e a contribuição de cada grupo para a taxa global de cesariana.
                </p>
                "
              ),
              fluidRow(
                a(
                  style = "width: 100%;",
                  class = "btn action-button btn-outline-primary btn-flat",
                  icon("chart-line"),
                  HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Ver série histórica dos indicadores </span>"
                  ),
                  onclick = "openTab('bloco_4')",
                  href = "#"
                )
              ),
              HTML(
                "<span style='display: block; margin-bottom: 1em;'> </span>"
              ),
              fluidRow(
                bs4Dash::actionButton(
                  inputId = ns("popup_b4"),
                  label = HTML(
                    "<span class = 'fonte-muito-grande'> &nbsp; Como interpretar os indicadores desse bloco? </span>"
                  ),
                  icon = icon("circle-question"),
                  status = "primary",
                  flat = TRUE,
                  outline = TRUE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 7,
              bs4Dash::bs4TabCard(
                id = ns("tabset1"),
                width = 12,
                collapsible = FALSE,
                # tabPanel(
                #   HTML("<b>Grupo de Robson</b>"),
                #   fluidRow(
                #     column(
                #       width = 12,
                #       status = "primary",
                #       collapsible = FALSE,
                #       #style = "height: 360px; overflow-y: auto",
                #       shinycssloaders::withSpinner(
                #         reactable::reactableOutput(ns("table4")),
                #         proxy.height = "320px"
                #       ),
                #     ),
                #   ),
                # ),
                tabPanel(
                  HTML("<b>Deslocamento para parto</b>"),
                  fluidRow(
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(
                        uiOutput(ns("caixa_b4_i1_deslocamento_muni")),
                        proxy.height = "280px"
                      )
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(
                        uiOutput(ns("caixa_b4_i2_deslocamento_muni")),
                        proxy.height = "280px"
                      )
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(
                        uiOutput(ns("caixa_b4_i3_deslocamento_muni")),
                        proxy.height = "280px"
                      )
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(
                        uiOutput(ns("caixa_b4_i4_deslocamento_muni")),
                        proxy.height = "280px"
                      )
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(
                        uiOutput(ns("caixa_b4_i5_deslocamento_muni")),
                        proxy.height = "280px"
                      )
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(
                        uiOutput(ns("caixa_b4_i9_deslocamento_muni")),
                        proxy.height = "280px"
                      ) #[fff]
                    ),
                  )
                ),
                ##TAGGG
                tabPanel(
                  HTML("<b>Profissional e local do parto</b>"),
                  fluidRow(
                    column(
                      offset = 2,
                      width = 4,
                      shinycssloaders::withSpinner(
                        uiOutput(ns("caixa_b4_i1_profissional")),
                        proxy.height = "280px"
                      )
                    ),
                    column(
                      width = 4,
                      shinycssloaders::withSpinner(
                        uiOutput(ns("caixa_b4_i2_profissional")),
                        proxy.height = "280px"
                      )
                    )
                  )
                ),
                tabPanel(
                  HTML("<b>Grupo de Robson</b>"),
                  fluidRow(
                    column(
                      width = 12,
                      status = "primary",
                      collapsible = FALSE,
                      #style = "height: 360px; overflow-y: auto",
                      shinycssloaders::withSpinner(
                        reactable::reactableOutput(ns("table4")),
                        proxy.height = "320px"
                      ),
                    ),
                  ),
                )
                ####
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          bs4Dash::bs4Card(
            width = 12,
            title = tags$b(
              HTML("5 - Condições de nascimento &nbsp;"),
              class = 'fonte-titulos-modal',
              a(
                icon("chart-line", color = "#007bff"),
                onclick = "openTab('bloco_5')",
                href = "#"
              )
            ),
            #icon = icon("5"),
            status = "primary",
            fluidRow(
              column(
                width = 5,
                HTML(
                  "
                <p align='justify' class = 'fonte-muito-grande'>
                As condições de nascimento do recém-nato dependem da saúde materna e devem ser monitoradas porque refletem a
                qualidade dos cuidados recebidos pela gestante durante a assistência pré-natal e ao parto. Além disso, seu
                monitoramento é importante para o planejamento da atenção à saúde do recém-nascido.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Neste bloco apresentamos a porcentagem de nascimentos prematuros (com menos de 37 semanas gestacionais) e com baixo
                peso ao nascer (<2500g), principais determinantes da mortalidade infantil. Apresentamos também os nascimentos a termo
                precoce (bebês nascidos com 37 e 38 semanas gestacionais), que têm maior risco de complicações e ocorrem com maior frequência em locais com taxa elevada de cesariana;
                a porcentagem de nascidos com anomalias congênitas (total e as
                definidas pelo Ministério da Saúde como de relevância para a vigilância em saúde); e a porcentagem de recém-nascidos com peso ≥2500g
                e sem anomalias congênitas que nasceram com asfixia no quinto minuto de vida (Apgar <7).


                </p>
                "
                ),
                fluidRow(
                  a(
                    style = "width: 100%;",
                    class = "btn action-button btn-outline-primary btn-flat",
                    icon("chart-line"),
                    HTML(
                      "<span class = 'fonte-muito-grande'> &nbsp; Ver série histórica dos indicadores </span>"
                    ),
                    onclick = "openTab('bloco_5')",
                    href = "#"
                  )
                ),
                HTML(
                  "<span style='display: block; margin-bottom: 1em;'> </span>"
                ),
                fluidRow(
                  bs4Dash::actionButton(
                    inputId = ns("popup_b5"),
                    label = HTML(
                      "<span class = 'fonte-muito-grande'> &nbsp; Como interpretar os indicadores desse bloco? </span>"
                    ),
                    icon = icon("circle-question"),
                    status = "primary",
                    flat = TRUE,
                    outline = TRUE,
                    width = "100%"
                  )
                )
              ),
              column(
                width = 7,
                fluidRow(
                  column(
                    width = 4,
                    shinycssloaders::withSpinner(
                      uiOutput(ns("caixa_b5_i1")),
                      proxy.height = "270px"
                    )
                  ),
                  column(
                    width = 4,
                    shinycssloaders::withSpinner(
                      uiOutput(ns("caixa_b5_i2")),
                      proxy.height = "270px"
                    )
                  ),
                  column(
                    width = 4,
                    shinycssloaders::withSpinner(
                      uiOutput(ns("caixa_b5_i3")),
                      proxy.height = "270px"
                    )
                  ),
                  column(
                    width = 4,
                    shinycssloaders::withSpinner(
                      uiOutput(ns("caixa_b5_i4")),
                      proxy.height = "270px"
                    )
                  ),
                  column(
                    width = 4,
                    shinycssloaders::withSpinner(
                      uiOutput(ns("caixa_b5_i5")),
                      proxy.height = "270px"
                    )
                  ),
                  column(
                    width = 4,
                    shinycssloaders::withSpinner(
                      uiOutput(ns("caixa_b5_i7")),
                      proxy.height = "270px"
                    )
                  ),
                  column(
                    offset = 2,
                    width = 4,
                    shinycssloaders::withSpinner(
                      uiOutput(ns("caixa_b5_i9")),
                      proxy.height = "270px"
                    )
                  ),
                  column(
                    width = 4,
                    shinycssloaders::withSpinner(
                      uiOutput(ns("caixa_b5_i8")),
                      proxy.height = "270px"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          bs4Dash::bs4Card(
            width = 12,
            title = tags$b(
              HTML("6 - Mortalidade e morbidade materna &nbsp;"),
              class = 'fonte-titulos-modal',
              a(
                icon("chart-line", color = "#007bff"),
                onclick = "openTab('bloco_6')",
                href = "#"
              )
            ),
            #icon = icon("6"),
            status = "primary",
            fluidRow(
              column(
                width = 12,
                HTML(
                  "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos maternos no país, incluindo desagregação de raça/cor, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-grav-puerp/' target = _blank>OOBr Óbitos de Gestantes e Puérperas</a>.
                </b> </div>"
                ),
                hr(),
                HTML(
                  "<span style='display: block; margin-bottom: 27px;'> </span>"
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                HTML(
                  "
                <p align='justify' class = 'fonte-muito-grande'>
                A morte materna e a morbidade materna grave são os desfechos adversos de curto prazo para a saúde da mulher e são
                resultantes da cadeia de determinantes apresentados nos blocos anteriores.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Neste bloco, apresentamos o número absoluto de óbitos maternos e a Razão de Mortalidade Materna. Apresentamos também
                a porcentagem de óbitos maternos por causas obstétricas diretas, que são as que podem ser evitadas por uma adequada
                assistência pré-natal e ao parto, e as principais causas desses óbitos.
                <span style='display: block; margin-bottom: 14px;'> </span>
                A morbidade materna grave, ou seja, mulher que apresentou uma complicação grave durante a gestação, parto ou pós parto,
                também é apresentada, sendo um indicador importante em locais com número reduzido de nascimentos, onde óbitos maternos
                são pouco frequentes. Além da porcentagem de internações obstétricas com morbidade materna grave, apresentamos as principais
                causas dessa morbidade e os principais indicadores de manejo de gravidade.
                </p>
                "
                ),
                fluidRow(
                  a(
                    style = "width: 100%;",
                    class = "btn action-button btn-outline-primary btn-flat",
                    icon("chart-line"),
                    HTML(
                      "<span class = 'fonte-muito-grande'> &nbsp; Ver série histórica dos indicadores </span>"
                    ),
                    onclick = "openTab('bloco_6')",
                    href = "#"
                  )
                ),
                HTML(
                  "<span style='display: block; margin-bottom: 1em;'> </span>"
                ),
                fluidRow(
                  bs4Dash::actionButton(
                    inputId = ns("popup_b6"),
                    label = HTML(
                      "<span class = 'fonte-muito-grande'> &nbsp; Como interpretar os indicadores desse bloco? </span>"
                    ),
                    icon = icon("circle-question"),
                    status = "primary",
                    flat = TRUE,
                    outline = TRUE,
                    width = "100%"
                  )
                )
              ),
              column(
                width = 7,
                bs4Dash::bs4TabCard(
                  id = ns("tabset1"),
                  width = 12,
                  collapsible = FALSE,
                  tabPanel(
                    HTML("<b>Mortalidade materna</b>"),
                    fluidRow(
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_mort_i1")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_mort_i2")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_mort_i3")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_mort_i4")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_mort_i5")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_mort_i6")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        offset = 4,
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_mort_i7")),
                          proxy.height = "290px"
                        )
                      )
                    )
                  ),
                  tabPanel(
                    HTML("<b>Morbidade materna grave</b>"),
                    fluidRow(
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_morb_i1")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_morb_i2")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_morb_i3")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_morb_i4")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_morb_i5")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_morb_i6")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        offset = 2,
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_morb_i7")),
                          proxy.height = "290px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b6_morb_i8")),
                          proxy.height = "290px"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),

      fluidRow(
        column(
          width = 12,
          bs4Dash::bs4Card(
            width = 12,
            title = tags$b(
              HTML(
                "7 - Mortalidade fetal, perinatal, neonatal e morbidade neonatal &nbsp;"
              ),
              class = 'fonte-titulos-modal',
              a(
                icon("chart-line", color = "#007bff"),
                onclick = "openTab('bloco_7')",
                href = "#"
              )
            ),
            #icon = icon("6"),
            status = "primary",
            fluidRow(
              column(
                width = 12,
                HTML(
                  "<div style = 'text-align: center;'> <b class = 'fonte-muito-grande'>
                <i class='fa-solid fa-circle-info'></i> &nbsp; Para mais detalhes a respeito dos óbitos fetais e neonatais no país, incluindo desagregação de raça/cor, acesse o painel <a href = 'https://observatorioobstetrico.shinyapps.io/obitos-fetais-neonatais/' target = _blank>OOBr Óbitos Fetais e Neonatais</a>.
                </b> </div>"
                ),
                hr(),
                HTML(
                  "<span style='display: block; margin-bottom: 27px;'> </span>"
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                HTML(
                  "
                <p align='justify' class = 'fonte-muito-grande'>
                A mortalidade fetal, neonatal e perinatal são os desfechos adversos mais graves
                da atenção à gestação, parto e nascimento, enquanto a morbidade neonatal é um
                indicador que reflete a carga de doença em recém-nascidos e suas necessidades
                assistenciais.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Neste bloco, apresentamos o número absoluto de óbitos e a taxa
                de mortalidade fetal, neonatal e perinatal; a distribuição desses óbitos segundo
                o peso ao nascer e o momento de ocorrência do óbito; e as principais causas dos
                óbitos e sua evitabilidade. Para a avaliação da evitabilidade, ou seja, da possibilidade do óbito ser evitado, utilizamos a Lista Brasileira de Causas de Mortes Evitáveis por intervenção do Sistema Único de Saúde.
                <span style='display: block; margin-bottom: 14px;'> </span>
                Para a morbidade neonatal, são apresentadas a porcentagem de nascidos com condições potencialmente ameaçadoras à vida (peso < 1500g ou idade gestacional < 32 semanas ou asfixia ao nascer); o número de internações neonatais e de internações em unidade de terapia intensiva neonatal até o 27º dia de vida em três faixas etárias (menos de um dia de vida, 1 a 6 dias de vida, 7 a 27 dias de vida) e segundo local de ocorrência (na macrorregião de saúde de residência ou fora da macrorregião de saúde); e as principais causas de morbidade. A análise da morbidade é particularmente importante para locais com baixo número de nascimentos, onde a frequência de óbitos neonatais pode ser muito baixa ou inexistente, ampliando a capacidade de análise do cuidado e dos riscos da população.

                </p>
                "
                ),
                fluidRow(
                  a(
                    style = "width: 100%;",
                    class = "btn action-button btn-outline-primary btn-flat",
                    icon("chart-line"),
                    HTML(
                      "<span class = 'fonte-muito-grande'> &nbsp; Ver série histórica dos indicadores </span>"
                    ),
                    onclick = "openTab('bloco_7')",
                    href = "#"
                  )
                ),
                HTML(
                  "<span style='display: block; margin-bottom: 1em;'> </span>"
                ),
                fluidRow(
                  bs4Dash::actionButton(
                    inputId = ns("popup_b7"),
                    label = HTML(
                      "<span class = 'fonte-muito-grande'> &nbsp; Como interpretar os indicadores desse bloco? </span>"
                    ),
                    icon = icon("circle-question"),
                    status = "primary",
                    flat = TRUE,
                    outline = TRUE,
                    width = "100%"
                  )
                )
              ),
              column(
                width = 7,
                bs4Dash::bs4TabCard(
                  id = ns("tabset1"),
                  width = 12,
                  collapsible = FALSE,
                  tabPanel(
                    HTML("<b>Mortalidade fetal</b>"),
                    fluidRow(
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_fetal_i1")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_fetal_i2")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_fetal_i3")),
                          proxy.height = "270px"
                        )
                      ),
                      # column(
                      #   width = 4,
                      #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_fetal_i7")), proxy.height = "270px")
                      # ),
                      # column(
                      #   width = 4,
                      #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_fetal_i8")), proxy.height = "270px")
                      # ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_fetal_i4")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_fetal_i6")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_fetal_i5")),
                          proxy.height = "280px"
                        )
                      )
                    )
                  ),
                  tabPanel(
                    HTML("<b>Mortalidade perinatal</b>"),
                    fluidRow(
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_perinatal_i7")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_perinatal_i8")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_perinatal_i3")),
                          proxy.height = "270px"
                        )
                      ),
                      # column(
                      #   width = 4,
                      #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i7")), proxy.height = "270px")
                      # ),
                      # column(
                      #   width = 4,
                      #   shinycssloaders::withSpinner(uiOutput(ns("caixa_b7_perinatal_i8")), proxy.height = "270px")
                      # ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_perinatal_i4")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_perinatal_i6")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_perinatal_i5")),
                          proxy.height = "280px"
                        )
                      )
                    )
                  ),

                  tabPanel(
                    HTML("<b>Mortalidade neonatal</b>"),
                    fluidRow(
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_neonatal_i4")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_neonatal_i1")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_neonatal_i2")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_neonatal_i3")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_neonatal_i5")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_neonatal_i6")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        offset = 2,
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_neonatal_i8")),
                          proxy.height = "280px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_neonatal_i7")),
                          proxy.height = "280px"
                        )
                      )
                    )
                  ),

                  tabPanel(
                    HTML("<b>Morbidade neonatal</b>"),
                    fluidRow(
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_morbidade_i1")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_morbidade_i2")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns("caixa_b7_morbidade_i3")),
                          proxy.height = "270px"
                        )
                      ),
                      column(
                        offset = 4,
                        width = 4,
                        shinycssloaders::withSpinner(
                          uiOutput(ns(
                            "caixa_b7_principais_morbidade_neonatal"
                          )),
                          proxy.height = "270px"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_nivel_1_server <- function(id, filtros) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ##### Criando o output que recebe a localidade e o ano escolhidos ####
    output$titulo_localidade <- renderUI({
      ano <- filtros()$ano

      texto <- dplyr::case_when(
        filtros()$nivel == "municipal" ~
          glue::glue("({filtros()$municipio}, {ano})"),
        filtros()$nivel == "estadual" ~
          glue::glue("({filtros()$estado}, {filtros()$ano})"),
        filtros()$nivel == "macro" ~
          glue::glue("({filtros()$macro}, {ano})"),
        filtros()$nivel == "micro" ~
          glue::glue("({filtros()$micro}, {ano})"),
        filtros()$nivel == "regional" ~
          glue::glue("({filtros()$regiao}, {ano})"),
        filtros()$nivel == "nacional" ~ glue::glue("(Brasil, {ano})")
      )

      tags$b(texto, class = "fonte-titulos-pagina")
    })

    ##### Criando os modais com as informações sobre os indicadores de cada bloco ####
    observeEvent(input$popup_b1, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal'> Interpretação dos indicadores do Bloco 1: Condições socioeconômicas e de acesso ao serviço de saúde </div>",
        includeHTML("inst/app/www/html/texto_popup_b1.html"),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE
      )
    })

    observeEvent(input$popup_b2, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal'> Interpretação dos indicadores do Bloco 2: Planejamento reprodutivo </div>",
        includeHTML("inst/app/www/html/texto_popup_b2.html"),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE,
        immediate = TRUE
      )
    })

    observeEvent(input$popup_b3, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal'> Interpretação dos indicadores do Bloco 3: Assistência pré-natal </div>",
        includeHTML("inst/app/www/html/texto_popup_b3.html"),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE,
        immediate = TRUE
      )
    })

    observeEvent(input$popup_b4, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal'> Interpretação dos indicadores do Bloco 4: Assistência ao parto </div>",
        includeHTML("inst/app/www/html/texto_popup_b4.html"),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE,
        immediate = TRUE
      )
    })

    observeEvent(input$popup_b5, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal'> Interpretação dos indicadores do Bloco 5: Condições de nascimento </div>",
        includeHTML("inst/app/www/html/texto_popup_b5.html"),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE,
        immediate = TRUE
      )
    })

    observeEvent(input$popup_b6, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal'> Interpretação dos indicadores do Bloco 6: Mortalidade e morbidade materna </div>",
        includeHTML("inst/app/www/html/texto_popup_b6.html"),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE,
        immediate = TRUE
      )
    })

    observeEvent(input$popup_b7, {
      shinyalert::shinyalert(
        html = TRUE,
        title = "<div class = 'fonte-titulos-modal'> Interpretação dos indicadores do Bloco 7: Mortalidade fetal, perinatal, neonatal e morbidade neonatal </div>",
        includeHTML("inst/app/www/html/texto_popup_b7.html"),
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#007bff",
        animation = TRUE,
        immediate = TRUE
      )
    })

    localidade_relatorio <- eventReactive(
      filtros()$pesquisar,
      {
        dplyr::case_when(
          filtros()$nivel == "nacional" ~ "brasil",
          filtros()$nivel == "regional" ~
            janitor::make_clean_names(filtros()$regiao),
          filtros()$nivel == "estadual" ~
            janitor::make_clean_names(filtros()$estado),
          filtros()$nivel == "macro" ~
            janitor::make_clean_names(filtros()$macro),
          filtros()$nivel == "micro" ~
            janitor::make_clean_names(filtros()$micro),
          filtros()$nivel == "municipal" ~
            janitor::make_clean_names(filtros()$municipio)
        )
      },
      ignoreNULL = FALSE
    )

    #### Criando o output que recebe o arquivo para impressão #####
    output$report <- downloadHandler(
      filename = reactive(paste0("relatorio_indicadores_", localidade_relatorio(), "_", filtros()$ano, ".pdf")),
      content = function(file) {

        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        arquivo_html <- tempfile(
          fileext = ".html"
        )

        if (filtros()$nivel == "nacional") {

          localidade <- "Brasil"
          idhm <- 0.727
          posicao_idhm <- ""
          comparacao_idhm <- "---"
          classificacao_idhm <- "Alto"

        } else if (filtros()$nivel == "regional") {

          localidade <- filtros()$regiao
          idhm <- "---"
          posicao_idhm <- ""
          comparacao_idhm <- "---"
          classificacao_idhm <- "Classificação não aplicável"

        } else if (filtros()$nivel == "estadual") {

          localidade <- filtros()$estado
          idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)]))
          posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)])
          comparacao_idhm <- glue::glue("{posicao_idhm}º lugar entre os 27 estados brasileiros")
          classificacao_idhm <- dplyr::case_when(
            as.numeric(idhm) <= 0.499 ~ "Muito baixo",
            as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo",
            as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio",
            as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto",
            as.numeric(idhm) > 0.8 ~ "Muito alto"
          )

        } else if (filtros()$nivel == "macro") {

          localidade <- glue::glue("Macrorregião de saúde estadual {filtros()$macro} ({filtros()$estado_macro})")
          idhm <- "---"
          posicao_idhm <- ""
          comparacao_idhm <- "---"
          classificacao_idhm <- "Classificação não aplicável"

        } else if (filtros()$nivel == "micro") {

          localidade <- glue::glue("Região de saúde estadual {filtros()$micro} ({filtros()$estado_micro})")
          idhm <- "---"
          posicao_idhm <- ""
          comparacao_idhm <- "---"
          classificacao_idhm <- "Classificação não aplicável"

        } else if (filtros()$nivel == "municipal") {

          localidade <- glue::glue("Município de {filtros()$municipio} ({filtros()$estado_municipio})")
          idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
          posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
          comparacao_idhm <- glue::glue("{posicao_idhm}º lugar entre os 5.570 municípios brasileiros")
          classificacao_idhm <- dplyr::case_when(
            as.numeric(idhm) <= 0.499 ~ "Muito baixo",
            as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo",
            as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio",
            as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto",
            as.numeric(idhm) > 0.8 ~ "Muito alto"
          )
        }

        withProgress(message = "Renderizando o HTML...", {

          incProgress(0.2)

          rmarkdown::render(
            input = "report.Rmd",
            output_file = arquivo_html,
            params = list(
              localidade = localidade,
              nivel = filtros()$nivel,
              ano = filtros()$ano,
              data1 = data1(),
              idhm = idhm,
              classificacao_idhm = classificacao_idhm,
              comparacao_idhm = comparacao_idhm,
              posicao_idhm = posicao_idhm,
              data1_comp = data1_comp(),
              data2 = data2(),
              data2_comp = data2_comp(),
              data3 = data3(),
              data3_comp = data3_comp(),
              data4 = data4(),
              data4_comp = data4_comp,
              data4_deslocamento = data4_deslocamento(),
              data4_comp_deslocamento = data4_comp_deslocamento(),
              data5 = data5(),
              data5_comp = data5_comp(),
              data5_comp_baixo_peso = data5_comp_baixo_peso(),
              data6 = data6(),
              data6_rmm_corrigida = data6_rmm_corrigida(),
              data6_comp = data6_comp(),
              data_incompletude = data_incompletude(),
              data7 = data7(),
              data7_comp = data7_comp(),
              bloco7_evitaveis_resumo = bloco7_evitaveis_resumo(),
              bloco7_evitaveis_resumo_comp = bloco7_evitaveis_resumo_comp()

            )
          )

          incProgress(0.5, message = "Renderizando o PDF...")

          pagedown::chrome_print(
            input = arquivo_html,
            output = file,
            extra_args = c("--no-sandbox")
          )

          incProgress(0.3)

        })



      }
    )


    ##### Dados de incompletude e cobertura para a localidade escolhida #####
    data_incompletude_aux <- reactive({
      base_incompletude |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          idademae = round(sum(idademae_incompletos, na.rm = TRUE)/sum(idademae_totais, na.rm = TRUE) * 100, 1),
          escmae = round(sum(escmae_incompletos, na.rm = TRUE)/sum(escmae_totais, na.rm = TRUE) * 100, 1),
          racacor = round(sum(racacor_incompletos, na.rm = TRUE)/sum(racacor_totais, na.rm = TRUE) * 100, 1),
          qtdpartnor = round(sum(qtdpartnor_incompletos, na.rm = TRUE)/sum(qtdpartnor_totais, na.rm = TRUE) * 100, 1),
          qtdpartces = round(sum(qtdpartces_incompletos, na.rm = TRUE)/sum(qtdpartces_totais, na.rm = TRUE) * 100, 1),
          consprenat = round(sum(consprenat_incompletos, na.rm = TRUE)/sum(consprenat_totais, na.rm = TRUE) * 100, 1),
          mesprenat = round(sum(mesprenat_incompletos, na.rm = TRUE)/sum(mesprenat_totais, na.rm = TRUE) * 100, 1),
          parto = round(sum(parto_incompletos, na.rm = TRUE)/sum(parto_totais, na.rm = TRUE) * 100, 1),
          tprobson = round(sum(tprobson_incompletos, na.rm = TRUE)/sum(tprobson_totais, na.rm = TRUE) * 100, 1),
          peso = round(sum(peso_incompletos, na.rm = TRUE)/sum(peso_totais, na.rm = TRUE) * 100, 1),
          gestacao = round(sum(gestacao_incompletos, na.rm = TRUE)/sum(gestacao_totais, na.rm = TRUE) * 100, 1),
          semagestac = round(sum(semagestac_incompletos, na.rm = TRUE)/sum(semagestac_totais, na.rm = TRUE) * 100, 1),
          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })

    data_cobertura <- reactive({
      if (filtros()$nivel == "municipal") {
        base_cobertura_muni_2015_2021 |>
          dplyr::filter(
            ano == ifelse(
              filtros()$ano %in% c(2012, 2013, 2014),
              2015,
              filtros()$ano
            ),
            municipio == filtros()$municipio,
            uf == filtros()$estado_municipio
          ) |>
          dplyr::mutate(
            ano = filtros()$ano
          ) |>
          dplyr::rename(
            localidade = municipio
          )
      } else if (filtros()$nivel == "estadual") {
        base_cobertura_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano == ifelse(
              filtros()$ano %in% c(2012, 2013, 2014),
              2015,
              filtros()$ano
            ),
            localidade == filtros()$estado
          ) |>
          dplyr::mutate(
            ano = filtros()$ano
          )
      } else if (filtros()$nivel == "regional") {
        base_cobertura_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano == ifelse(
              filtros()$ano %in% c(2012, 2013, 2014),
              2015,
              filtros()$ano
            ),
            localidade == filtros()$regiao
          ) |>
          dplyr::mutate(
            ano = filtros()$ano
          )
      } else if (filtros()$nivel == "nacional") {
        base_cobertura_uf_regioes_2015_2021 |>
          dplyr::filter(
            ano == ifelse(
              filtros()$ano %in% c(2012, 2013, 2014),
              2015,
              filtros()$ano
            ),
            localidade == "Brasil"
          ) |>
          dplyr::mutate(
            ano = filtros()$ano
          )
      } else {
        data.frame(
          ano = filtros()$ano,
          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          ),
          cobertura = 100
        )
      }
    })

    data_incompletude <- reactive({
      dplyr::full_join(data_incompletude_aux(), data_cobertura(), by = c("ano", "localidade"))
    })

    ##### Dados do primeiro bloco de indicadores para a localidade escolhida #####
    data1 <- reactive({
      bloco1 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          populacao_feminina_10_a_49 = sum(populacao_feminina_10_a_49),
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = round(sum(media_cobertura_esf)/sum(populacao_total) * 100, 1),
          porc_nvm_10_a_14_anos = round(sum(nvm_10_a_14_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_15_a_19_anos = round(sum(nvm_15_a_19_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_7 = round((sum(nvm_com_escolaridade_ate_3) + sum(nvm_com_escolaridade_de_4_a_7))/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_negra = round((sum(nvm_com_cor_da_pele_parda) + sum(nvm_com_cor_da_pele_preta))/sum(total_de_nascidos_vivos) * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1),
          class = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil (valor de referência)",
            filtros()$nivel == "regional" ~ paste("Região ",filtros()$regiao),
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })

    ##### Dados do primeiro bloco de indicadores para a comparação com o Brasil #####
    data1_comp <- reactive({
      bloco1 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          populacao_feminina_10_a_49 = sum(populacao_feminina_10_a_49),
          porc_dependentes_sus = round((populacao_feminina_10_a_49 - sum(pop_fem_10_49_com_plano_saude, na.rm = TRUE))/populacao_feminina_10_a_49 * 100, 1),
          porc_cobertura_esf = 95,
          porc_nvm_10_a_14_anos = round(sum(nvm_10_a_14_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_15_a_19_anos = round(sum(nvm_15_a_19_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_entre_20_e_34_anos = round(sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_maior_que_34_anos = round(sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_3 = round(sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_4_a_7 = round(sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_de_8_a_11 = round(sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_acima_de_11 = round(sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_escolaridade_ate_7 = round((sum(nvm_com_escolaridade_ate_3) + sum(nvm_com_escolaridade_de_4_a_7))/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_branca = round(sum(nvm_com_cor_da_pele_branca)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_preta = round(sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_amarela = round(sum(nvm_com_cor_da_pele_amarela)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_parda = round(sum(nvm_com_cor_da_pele_parda)/total_de_nascidos_vivos * 100, 1),
          porc_nvm_com_cor_da_pele_negra = round((sum(nvm_com_cor_da_pele_parda) + sum(nvm_com_cor_da_pele_preta))/sum(total_de_nascidos_vivos) * 100, 1),
          porc_nvm_indigenas = round(sum(nvm_indigenas)/total_de_nascidos_vivos * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas para os indicadores do primeiro bloco #####
    output$caixa_b1_i1 <- renderUI({

      if (filtros()$nivel == "municipal") {
        uf <- unique(tabela_aux_municipios$uf[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        micro <- unique(tabela_aux_municipios$r_saude[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        macro <- unique(tabela_aux_municipios$macro_r_saude[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        regiao <- unique(tabela_aux_municipios$regiao[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        texto <-
          "
      <span class = 'fonte-destaque-caixas1' style = 'font-weight: 600'>{filtros()$municipio}</span>
      <div class = 'fonte-grande'>
        <b> Região do país: </b> {regiao}
        <br>
        <b> UF: </b> {uf}
        <br>
        <b> Macrorregião de saúde estadual: </b> {macro}
        <br>
        <b> Região de saúde estadual: </b> {micro}
      </div>
      "
      } else if (filtros()$nivel == "micro") {
        uf <- unique(tabela_aux_municipios$uf[which(tabela_aux_municipios$r_saude == filtros()$micro & tabela_aux_municipios$uf == filtros()$estado_micro)])
        macro <- unique(tabela_aux_municipios$macro_r_saude[which(tabela_aux_municipios$r_saude == filtros()$micro & tabela_aux_municipios$uf == filtros()$estado_micro)])
        regiao <- unique(tabela_aux_municipios$regiao[which(tabela_aux_municipios$r_saude == filtros()$micro & tabela_aux_municipios$uf == filtros()$estado_micro)])
        texto <-
          "
      <b class = 'fonte-destaque-caixas1'> {filtros()$micro} </b>
      <div class = 'fonte-grande'>
        <b> Região do país: </b> {regiao}
        <br>
        <b> UF: </b> {uf}
        <br>
        <b> Macrorregião de saúde estadual: </b> {macro}
      </div>
      "
      } else if (filtros()$nivel == "macro") {
        uf <- unique(tabela_aux_municipios$uf[which(tabela_aux_municipios$macro_r_saude == filtros()$macro & tabela_aux_municipios$uf == filtros()$estado_macro)])
        regiao <- unique(tabela_aux_municipios$regiao[which(tabela_aux_municipios$macro_r_saude == filtros()$macro & tabela_aux_municipios$uf == filtros()$estado_macro)])
        texto <-
          "
      <b class = 'fonte-destaque-caixas1'> {filtros()$macro} </b>
      <div class = 'fonte-grande'>
        <b> Região do país: </b> {regiao}
        <br>
        <b> UF: </b> {uf}
      </div>
      "
      } else if (filtros()$nivel == "estadual") {
        regiao <- unique(tabela_aux_municipios$regiao[which(tabela_aux_municipios$uf == filtros()$estado)])
        texto <-
          "
      <b class = 'fonte-destaque-caixas1'> {filtros()$estado} </b>
      <div class = 'fonte-grande'>
        <b> Região do país: </b> {regiao}
      </div>
      "
      } else if (filtros()$nivel == "regional") {
        texto <- "<b class = 'fonte-destaque-caixas1'> {filtros()$regiao} </b>"
      } else if (filtros()$nivel == "nacional") {
        texto <- "<b class = 'fonte-destaque-caixas1'> Brasil </b>"
      }

      bs4Dash::box(
        style = "height: 300px; overflow-y: auto; padding-top:0",
        width = 12,
        collapsible = FALSE,
        headerBorder = FALSE,
        HTML(glue::glue(paste(texto)))
      )
    })

    output$caixa_b1_i2 <- renderUI({

      if (filtros()$nivel == "municipal") {
        idhm <- as.numeric(tabela_aux_municipios$idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)])
        posicao_idhm <- tabela_aux_municipios$posicao_idhm[which(tabela_aux_municipios$municipio == filtros()$municipio & tabela_aux_municipios$uf == filtros()$estado_municipio)]
      } else if (filtros()$nivel == "estadual") {
        idhm <- as.numeric(unique(tabela_aux_municipios$idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)]))
        posicao_idhm <- unique(tabela_aux_municipios$posicao_idh_uf[which(tabela_aux_municipios$uf == filtros()$estado)])
      } else if (filtros()$nivel == "nacional") {
        idhm <- 0.727
      } else {
        idhm <- NaN
      }

      cor_comp <- dplyr::case_when(
        is.na(idhm) ~ "lightgrey",
        as.numeric(idhm) <= 0.499 ~ "#d998a0",  #vermelho
        as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "#d8b382",  #laranja
        as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "#f1eb99",  #amarelo
        as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "#a2e4b8",  #verde
        as.numeric(idhm) > 0.8 ~ "#cbd6ff"  #azul
      )

      if (is.na(idhm)) {
        texto_comp <- "Classificação não aplicável"
      } else {
        texto_posicao <- dplyr::case_when(
          filtros()$nivel == "nacional" ~ "",
          filtros()$nivel == "estadual" ~ "({posicao_idhm}º lugar entre os 27 estados brasileiros)",
          filtros()$nivel == "municipal" ~ "({posicao_idhm}º lugar entre os 5.570 municípios brasileiros)",
        )
        texto_comp <- dplyr::case_when(
          as.numeric(idhm) <= 0.499 ~ "Muito baixo {glue::glue(texto_posicao)}",
          as.numeric(idhm) >= 0.500 & as.numeric(idhm) <= 0.599 ~ "Baixo {glue::glue(texto_posicao)}",
          as.numeric(idhm) >= 0.6 & as.numeric(idhm) <= 0.699 ~ "Médio {glue::glue(texto_posicao)}",
          as.numeric(idhm) >= 0.7 & as.numeric(idhm) <= 0.799 ~ "Alto {glue::glue(texto_posicao)}",
          as.numeric(idhm) > 0.8 ~ "Muito alto {glue::glue(texto_posicao)}"
        )
      }

      cria_caixa_server(
        dados = NULL,
        indicador = NULL,
        titulo = dplyr::if_else(filtros()$nivel == "nacional", true = "IDH", false = "IDHM"),
        tem_meta = FALSE,
        valor_de_referencia = 0.727,
        valor_indicador = idhm,
        tipo = "taxa",
        texto_footer = glue::glue(texto_comp),
        cor = cor_comp,
        invertido = TRUE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b1_i3 <- renderUI({
      cria_caixa_server(
        dados = data1(),
        indicador = "populacao_feminina_10_a_49",
        titulo = "População feminina entre 10 e 49 anos",
        tem_meta = FALSE,
        valor_de_referencia = data1_comp()$populacao_feminina_10_a_49,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        #cor = dplyr::if_else(filtros()$nivel == "nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (o número nacional é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} mulheres"
        ),
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b1_i4 <- renderUI({
      cria_caixa_server(
        dados = data1(),
        indicador = "total_de_nascidos_vivos",
        titulo = "Nascidos vivos",
        tem_meta = FALSE,
        valor_de_referencia = data1_comp()$total_de_nascidos_vivos,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        #cor = dplyr::if_else(filtros()$nivel == "nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} nascidos vivos"
        ),
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b1_i5 <- renderUI({
      cria_caixa_server(
        dados = data1(),
        indicador = "porc_dependentes_sus",
        titulo = "Porcentagem de mulheres de 10 a 49 anos usuárias exclusivas do SUS",
        tem_meta = FALSE,
        valor_de_referencia = data1_comp()$porc_dependentes_sus,
        tipo = "porcentagem",
        invertido = FALSE,
        cor = "lightgrey",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b1_i6 <- renderUI({
      cria_caixa_server(
        dados = data1(),
        indicador = "porc_cobertura_esf",
        titulo = "Cobertura populacional da Atenção Básica",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "meta ODS",
        nivel_de_analise = filtros()$nivel
      )
    })


    ##### Definindo as cores para os gráficos de barra #####
    cols <- c("#2c115f", "#b73779", "#fc8961")


    ##### Criando os gráficos de barras para os indicadores de porcentagem de nascidos vivos #####
    output$plot1 <- highcharter::renderHighchart({

      df1 <- reactive({
        data.frame(
          "total" = c(
            data1()$porc_nvm_com_escolaridade_ate_3,
            data1()$porc_nvm_com_escolaridade_de_4_a_7,
            data1()$porc_nvm_com_escolaridade_de_8_a_11,
            data1()$porc_nvm_com_escolaridade_acima_de_11,
            data1_comp()$porc_nvm_com_escolaridade_ate_3,
            data1_comp()$porc_nvm_com_escolaridade_de_4_a_7,
            data1_comp()$porc_nvm_com_escolaridade_de_8_a_11,
            data1_comp()$porc_nvm_com_escolaridade_acima_de_11
          ),
          "categorias" = factor(rep(c("0 a 3 anos", "4 a 7 anos", "8 a 11 anos", "> 11 anos"), times = 2)),
          "local" = rep(c(data1()$class, "Referência (média nacional)"), each = 4)
        )
      })

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_xAxis(categories = df1()$categorias) |>
        highcharter::hc_add_series(
          data = df1() |> dplyr::filter(local == data1()$class),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%") |>
        highcharter::hc_title(text = HTML("<b class = 'fonte-grande'> Porcentagem de nascidos vivos por escolaridade da mãe </b>")) |>
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos")) |>
        highcharter::hc_colors(cols)

      if (filtros()$nivel == "nacional") {
        grafico_base
      } else {
        grafico_base |> highcharter::hc_add_series(
          data = df1() |> dplyr::filter(local == "Referência (média nacional)"),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        )
      }
    })

    output$plot2 <- highcharter::renderHighchart({

      df2 <- reactive({
        data.frame(
          "total" = c(
            data1()$porc_nvm_10_a_14_anos,
            data1()$porc_nvm_15_a_19_anos,
            data1()$porc_nvm_entre_20_e_34_anos,
            data1()$porc_nvm_maior_que_34_anos,
            data1_comp()$porc_nvm_10_a_14_anos,
            data1_comp()$porc_nvm_15_a_19_anos,
            data1_comp()$porc_nvm_entre_20_e_34_anos,
            data1_comp()$porc_nvm_maior_que_34_anos
          ),
          "categorias" = rep(c("10 a 14 anos", "15 a 19 anos", "20 a 34 anos", "> 34 anos"), times = 2),
          "local" = rep(c(data1()$class, "Referência (média nacional)"), each = 4)
        )
      })

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_xAxis(categories = df2()$categorias) |>
        highcharter::hc_add_series(
          data = df2() |> dplyr::filter(local == data1()$class),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%") |>
        highcharter::hc_title(text = HTML("<b class = 'fonte-grande'> Porcentagem de nascidos vivos por faixa etária da mãe </b>")) |>
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos")) |>
        highcharter::hc_colors(cols)

      if (filtros()$nivel == "nacional") {
        grafico_base
      } else {
        grafico_base |> highcharter::hc_add_series(
          data = df2() |> dplyr::filter(local == "Referência (média nacional)"),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        )
      }

    })

    output$plot3 <- highcharter::renderHighchart({

      df3 <- reactive({
        data.frame(
          "total" = c(
            data1()$porc_nvm_com_cor_da_pele_amarela,
            data1()$porc_nvm_com_cor_da_pele_branca,
            data1()$porc_nvm_indigenas,
            data1()$porc_nvm_com_cor_da_pele_parda,
            data1()$porc_nvm_com_cor_da_pele_preta,
            data1()$porc_nvm_com_cor_da_pele_negra,
            data1_comp()$porc_nvm_com_cor_da_pele_amarela,
            data1_comp()$porc_nvm_com_cor_da_pele_branca,
            data1_comp()$porc_nvm_indigenas,
            data1_comp()$porc_nvm_com_cor_da_pele_parda,
            data1_comp()$porc_nvm_com_cor_da_pele_preta,
            data1_comp()$porc_nvm_com_cor_da_pele_negra
          ),
          "categorias" = rep(c("Amarela", "Branca",  "Indígena", "Parda", "Preta", "Negra (pardas e pretas)"), times = 2),
          "local" = rep(c(data1()$class, "Referência (média nacional)"), each = 6)
        )
      })

      grafico_base <- highcharter::highchart() |>
        highcharter::hc_xAxis(categories = df3()$categorias) |>
        highcharter::hc_add_series(
          data = df3() |> dplyr::filter(local == data1()$class),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        ) |>
        highcharter::hc_tooltip(valueSuffix = "%") |>
        highcharter::hc_title(text = HTML("<b class = 'fonte-grande'> Porcentagem de nascidos vivos por raça/cor da mãe</b>")) |>
        highcharter::hc_xAxis(title = list(text = ""), allowDecimals = FALSE) |>
        highcharter::hc_yAxis(title = list(text = "% de nascidos vivos")) |>
        highcharter::hc_colors(cols)


      if (filtros()$nivel == "nacional") {
        grafico_base
      } else {
        grafico_base |> highcharter::hc_add_series(
          data = df3() |> dplyr::filter(local == "Referência (média nacional)"),
          type = "column",
          highcharter::hcaes(x = categorias, y = total, group = local)
        )
      }

    })

    ##### Dados do segundo bloco de indicadores para a localidade escolhida #####
    data2 <- reactive({
      bloco2 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_menor20 = round(sum(nvm_menor_que_20)/sum(populacao_feminina_10_a_14 + populacao_feminina_15_a_19)*1000, 1),
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos*100, 1),
          tx_abortos_mil_mulheres_lim_inf = round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 3) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1),
          tx_abortos_mil_mulheres_lim_sup = round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 6)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_lim_inf = round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 3) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4)) / sum(total_de_nascidos_vivos_10_a_49[ano >= 2015 & ano <= 2023]) * 100, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49[ano >= 2015 & ano <= 2023]) * 100, 1),
          tx_abortos_cem_nascidos_vivos_lim_sup = round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 6)) / sum(total_de_nascidos_vivos_10_a_49[ano >= 2015 & ano <= 2023]) * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Dados do segundo bloco de indicadores para a comparação com o Brasil #####
    data2_comp <- reactive({
      bloco2 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_menor20 = 30,
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos*100, 1),
          tx_abortos_mil_mulheres_valor_medio = round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5)) / sum(pop_fem_10_49[ano >= 2015 & ano <= 2023]) * 1000, 1),
          tx_abortos_cem_nascidos_vivos_valor_medio = round(((((sum(abortos_sus_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_sus_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_sus_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 4) + (((sum(abortos_ans_menor_30[ano >= 2015 & ano <= 2023]) * 0.9) + (sum(abortos_ans_30_a_39[ano >= 2015 & ano <= 2023]) * 0.85) + (sum(abortos_ans_40_a_49[ano >= 2015 & ano <= 2023]) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49[ano >= 2015 & ano <= 2023]) * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas para os indicadores do segundo bloco #####
    output$caixa_b2_i1 <- renderUI({
      cria_caixa_server(
        dados = data2(),
        indicador = "porc_menor20",
        titulo = "Taxa específica de fecundidade de mulheres com menos de 20 anos de idade (por mil)",
        tem_meta = TRUE,
        valor_de_referencia = 30,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "países desenvolvidos",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b2_i2 <- renderUI({
      cria_caixa_server(
        dados = data2(),
        indicador = "porc_mais_3pt",
        titulo = "Porcentagem de nascidos vivos de mulheres com mais de 3 partos anteriores",
        tem_meta = FALSE,
        valor_de_referencia = data2_comp()$porc_mais_3pt,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b2_i3 <- renderUI({
      cria_caixa_server(
        dados = data2() |> dplyr::filter(ano >= 2015),
        indicador = "tx_abortos_mil_mulheres_valor_medio",
        titulo = "Valor médio da taxa de abortos inseguros por mil MIF",
        tem_meta = FALSE,
        valor_de_referencia = data2_comp()$tx_abortos_mil_mulheres_valor_medio,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b2_i4 <- renderUI({
      cria_caixa_server(
        dados = data2() |> dplyr::filter(ano >= 2015),
        indicador = "tx_abortos_cem_nascidos_vivos_valor_medio",
        titulo = "Valor médio da razão de abortos inseguros por 100 nascidos vivos",
        tem_meta = FALSE,
        valor_de_referencia = data2_comp()$tx_abortos_cem_nascidos_vivos_valor_medio,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })


    ##### Dados do terceiro bloco de indicadores para a localidade escolhida #####
    data3 <- reactive({
      bloco3 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_inicio_prec = round(sum(mulheres_com_inicio_precoce_do_prenatal)/total_de_nascidos_vivos * 100, 1),
          cobertura_pre_natal = round(sum(mulheres_com_pelo_menos_uma_consulta_prenatal)/total_de_nascidos_vivos * 100, 1),
          porc_7 = round(sum(mulheres_com_mais_de_sete_consultas_prenatal)/total_de_nascidos_vivos * 100, 1),
          porc_consultas_adequadas = round(sum(mulheres_com_consultas_prenatal_adequadas)/total_de_nascidos_vivos*100, 1),
          porc_sc = round(sum(casos_sc)/total_de_nascidos_vivos * 1000, 1)
        ) |>
        dplyr::ungroup()
    })

    ##### Dados do terceiro bloco de indicadores para a comparação com o Brasil #####
    data3_comp <- reactive({
      bloco3 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          cobertura_pre_natal = 95,
          porc_inicio_prec = 95,
          porc_7 = 95,
          porc_consultas_adequadas = round(sum(mulheres_com_consultas_prenatal_adequadas)/total_de_nascidos_vivos*100, 1),
          porc_sc = 0.5
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas para os indicadores do terceiro bloco #####
    output$caixa_b3_i1 <- renderUI({
      cria_caixa_server(
        dados = data3() |> dplyr::filter(ano >= 2014),
        indicador = "cobertura_pre_natal",
        titulo = "Cobertura de assistência pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b3_i2 <- renderUI({
      cria_caixa_server(
        dados = data3(),
        indicador = "porc_inicio_prec",
        titulo = "Porcentagem de mulheres com início do pré-natal até 12 semanas de gestação",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = filtros()$nivel
      )
    })

    # output$caixa_b3_i3 <- renderUI({
    #   cria_caixa_server(
    #     dados = data3() |> dplyr::filter(ano >= 2014),
    #     indicador = "porc_7",
    #     titulo = "Porcentagem de mulheres com mais de sete consultas de pré-natal",
    #     tem_meta = TRUE,
    #     valor_de_referencia = 95,
    #     tipo = "porcentagem",
    #     invertido = TRUE,
    #     pagina = "nivel_1",
    #     tipo_referencia = "recomendações OMS",
    #     nivel_de_analise = filtros()$nivel
    #   )
    # })

    output$caixa_b3_i3 <- renderUI({
      cria_caixa_server(
        dados = data3() |> dplyr::filter(ano >= 2014),
        indicador = "porc_consultas_adequadas",
        titulo = "Porcentagem de mulheres com número adequado de consultas de pré-natal para a idade gestacional no parto",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b3_i4 <- renderUI({
      cria_caixa_server(
        dados = data3(),
        indicador = "porc_sc",
        titulo = "Incidência de sífilis congênita por mil nascidos vivos",
        tem_meta = TRUE,
        valor_de_referencia = 0.5,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "meta OMS",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b3_i5 <- renderUI({
      cria_caixa_server(
        dados = data3(),
        indicador = "porc_7",
        titulo = "Porcentagem de mulheres com oito ou mais consultas de pré-natal",
        tem_meta = TRUE,
        valor_de_referencia = 95,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        tipo_referencia = "recomendações OMS",
        nivel_de_analise = filtros()$nivel
      )
    })



    ##### Dados do quarto bloco de indicadores para a localidade escolhida #####
    data4 <- reactive({
      bloco4 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 1),
          prop_nasc_robson_faltante = round((total_de_nascidos_vivos - sum(dplyr::across(dplyr::starts_with("mulheres_dentro")))) / total_de_nascidos_vivos * 100, 1),
          prop_tx_cesariana_geral = round(mulheres_com_parto_cesariana/total_de_nascidos_vivos * 100, 1),
          prop_robson1_tx_cesariana = round((sum(total_cesariana_grupo_robson_1) / sum(mulheres_dentro_do_grupo_de_robson_1)) * 100, 1),
          prop_robson2_tx_cesariana = round((sum(total_cesariana_grupo_robson_2) / sum(mulheres_dentro_do_grupo_de_robson_2)) * 100, 1),
          prop_robson3_tx_cesariana = round((sum(total_cesariana_grupo_robson_3) / sum(mulheres_dentro_do_grupo_de_robson_3)) * 100, 1),
          prop_robson4_tx_cesariana = round((sum(total_cesariana_grupo_robson_4) / sum(mulheres_dentro_do_grupo_de_robson_4)) * 100, 1),
          prop_robson5_tx_cesariana = round((sum(total_cesariana_grupo_robson_5) / sum(mulheres_dentro_do_grupo_de_robson_5)) * 100, 1),
          prop_robson6_a_9_tx_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)) * 100, 1),
          prop_robson10_tx_cesariana = round((sum(total_cesariana_grupo_robson_10) / sum(mulheres_dentro_do_grupo_de_robson_10)) * 100, 1),
          contrib_robson1_tx_cesariana = round(sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson2_tx_cesariana = round(sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson3_tx_cesariana = round(sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson4_tx_cesariana = round(sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson5_tx_cesariana = round(sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson6_a_9_tx_cesariana = round(sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson10_tx_cesariana = round(sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana * 100, 1),
          contrib_robson_faltante_tx_cesariana = round((mulheres_com_parto_cesariana - sum(dplyr::across(dplyr::starts_with("total_cesariana")))) / mulheres_com_parto_cesariana * 100, 1),
          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })

    data4_deslocamento <- reactive({
      if (filtros()$nivel != "estadual" & filtros()$nivel != "municipal") {
        bloco4_deslocamento_muni |>
          dplyr::filter(
            ano == filtros()$ano
          ) |>
          dplyr::filter(
            if (filtros()$nivel == "nacional")
              ano == filtros()$ano
            else if (filtros()$nivel == "regional")
              regiao == filtros()$regiao
            else if (filtros()$nivel == "macro")
              macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
            else if(filtros()$nivel == "micro")
              r_saude == filtros()$micro & uf == filtros()$estado_micro
          ) |>
          dplyr::group_by(ano) |>
          dplyr::summarise(
            prop_partos_municipio_res = round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_mun_res = round(sum(nao_local, na.rm=TRUE)/sum(destino_total, na.rm=TRUE)*100, 1),
            prop_partos_rsaude_res = round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_macro_rsaude_res = round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_uf_res = round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            localidade = dplyr::case_when(
              filtros()$nivel == "nacional" ~ "Brasil",
              filtros()$nivel == "regional" ~ filtros()$regiao,
              filtros()$nivel == "macro" ~ filtros()$macro,
              filtros()$nivel == "micro" ~ filtros()$micro
            )
          ) |>
          dplyr::ungroup()
      } else if (filtros()$nivel == "municipal") {
        bloco4_deslocamento_muni |>
          dplyr::filter(
            ano == filtros()$ano,
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
          ) |>
          dplyr::group_by(ano) |>
          dplyr::mutate(
            prop_partos_municipio_res = round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_mun_res = round(sum(nao_local, na.rm=TRUE)/sum(destino_total, na.rm=TRUE)*100, 1),
            prop_partos_rsaude_res = round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_macro_rsaude_res = round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_uf_res = round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            localidade = filtros()$municipio,
            .keep = "unused"
          ) |>
          dplyr::ungroup()
      } else if (filtros()$nivel == "estadual") {
        bloco4_deslocamento_uf |>
          dplyr::filter(
            ano == filtros()$ano,
            uf == filtros()$estado
          ) |>
          dplyr::group_by(ano) |>
          dplyr::mutate(
            prop_partos_municipio_res = round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_mun_res = round(sum(nao_local, na.rm=TRUE)/sum(destino_total, na.rm=TRUE)*100, 1),
            prop_partos_rsaude_res = round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_macro_rsaude_res = round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            prop_partos_fora_uf_res = round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
            localidade = filtros()$estado,
            .keep = "unused"
          ) |>
          dplyr::ungroup()

      }

    })

    ##TAGGG
    data4_profissional <- reactive({
      bloco4_profissional |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          prop_nasc_local_hospital = round(sum(nasc_local_hospital, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_outros_est_saude = round(sum(nasc_local_outros_est_saude, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_domicilio = round(sum(nasc_local_domicilio, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_outros = round(sum(nasc_local_outros, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_aldeia = round(sum(nasc_local_aldeia, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_sem_inf = round(sum(nasc_local_sem_inf, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_fora_hospital = round(sum(nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros,  nasc_local_aldeia, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_assistido_enf_obs = round(sum(nasc_assistido_enf_obs, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100,1),

          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        ) |>
        dplyr::ungroup()
    })

    data4_deslocamento_macrorregiao <- reactive({
      # if (filtros()$nivel != "estadual" & filtros()$nivel != "municipal") {
      bloco4_deslocamento_macrorregiao |>
        dplyr::filter(
          ano == filtros()$ano
        ) |>
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          prop_partos_com_4mais_uti = round((sum(partos_na_macro_com_4mais_uti) + sum(partos_fora_macro_com_4mais_uti)) / (sum(partos_na_macro_com_4mais_uti) + sum(partos_na_macro_sem_4mais_uti) + sum(partos_fora_macro_com_4mais_uti) + sum(partos_fora_macro_sem_4mais_uti)) * 100, 1),
          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro
          )
        ) |>
        dplyr::ungroup()
    })

    ##### Dados do quarto bloco de indicadores para a comparação com o Brasil #####
    data4_comp <- reactive({
      bloco4 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          prop_tx_cesariana_geral = "25%",
          prop_robson1_tx_cesariana = "10%",
          prop_robson2_tx_cesariana = "20 a 35%",
          prop_robson3_tx_cesariana = "3%",
          prop_robson4_tx_cesariana = "15%",
          prop_robson5_tx_cesariana = "50 a 60%",
          prop_robson6_a_9_tx_cesariana = "---",
          prop_robson10_tx_cesariana = "30%"
        ) |>
        dplyr::ungroup()
    })

    data4_comp_deslocamento <- reactive({
      bloco4_deslocamento_muni |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::summarise(
          prop_partos_municipio_res = round(sum(local, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_mun_res = round(sum(nao_local, na.rm=TRUE)/sum(destino_total, na.rm=TRUE)*100, 1),
          prop_partos_rsaude_res = round(sum(dentro_regiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_macro_rsaude_res = round(sum(dentro_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_macro_rsaude_res = round(sum(fora_macrorregiao_saude, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1),
          prop_partos_fora_uf_res = round(sum(outra_uf, na.rm = TRUE)/sum(destino_total, na.rm = TRUE) * 100, 1)
        )
    })

    data4_comp_deslocamento_macrorregiao <- reactive({
      bloco4_deslocamento_macrorregiao |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::summarise(
          prop_partos_com_4mais_uti = round((sum(partos_na_macro_com_4mais_uti) + sum(partos_fora_macro_com_4mais_uti)) / (sum(partos_na_macro_com_4mais_uti) + sum(partos_na_macro_sem_4mais_uti) + sum(partos_fora_macro_com_4mais_uti) + sum(partos_fora_macro_sem_4mais_uti)) * 100, 1),

        )
    })

    ##TAGGG
    data4_comp_profissional <- reactive({
      bloco4_profissional |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::summarise(
          prop_nasc_local_hospital = round(sum(nasc_local_hospital, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_outros_est_saude = round(sum(nasc_local_outros_est_saude, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_domicilio = round(sum(nasc_local_domicilio, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_outros = round(sum(nasc_local_outros, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_aldeia = round(sum(nasc_local_aldeia, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_sem_inf = round(sum(nasc_local_sem_inf, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_local_fora_hospital = round(sum(nasc_local_outros_est_saude, nasc_local_domicilio, nasc_local_outros,  nasc_local_aldeia, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100, 1),
          prop_nasc_assistido_enf_obs = round(sum(nasc_assistido_enf_obs, na.rm = TRUE)/sum(total_de_nascidos_vivos, na.rm = TRUE) * 100,1)
        )
    })



    ##### Criando a tabela para os indicadores do quarto bloco #####
    output$table4 <- reactable::renderReactable({
      grupo_robson <-  c(
        "Geral",
        "Grupo de Robson 1",
        "Grupo de Robson 2",
        "Grupo de Robson 3",
        "Grupo de Robson 4",
        "Grupo de Robson 5",
        "Grupos de Robson 6 a 9",
        "Grupo de Robson 10"
      )
      porc_nascidos <- c(
        "100%",
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson1, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson2, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson3, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson4, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson5, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson6_a_9, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_nasc_robson10, big.mark = ".", decimal.mark = ","), "%"), "---")
      )
      porc_cesareas <- c(
        paste0(formatC(data4()$prop_tx_cesariana_geral, big.mark = ".", decimal.mark = ","), "%"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson1_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson2_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson3_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson4_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson5_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson6_a_9_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---"),
        ifelse(filtros()$ano >= 2014, paste0(formatC(data4()$prop_robson10_tx_cesariana, big.mark = ".", decimal.mark = ","), "%"), "---")
      )
      referencia_porc_cesareas <- c(
        "15%",
        "10%",
        "20 a 35%",
        "3%",
        "15%",
        "50 a 60%",
        "---",
        "30%"
      )

      infos_comp <- data.frame(
        valores_comp = c(
          round(100 - 100*data4()$prop_tx_cesariana_geral/15, 1),
          ifelse(filtros()$ano >= 2014, round(100 - 100*data4()$prop_robson1_tx_cesariana/10, 1), NaN),
          ifelse(
            filtros()$ano >= 2014,
            dplyr::case_when(
              data4()$prop_robson2_tx_cesariana < 20 ~ round(100 - 100*data4()$prop_robson2_tx_cesariana/20, 1),
              data4()$prop_robson2_tx_cesariana > 35 ~ round(100 - 100*data4()$prop_robson2_tx_cesariana/35, 1),
              dplyr::between(data4()$prop_robson2_tx_cesariana, 20, 35) ~ 0
            ),
            NaN
          ),
          ifelse(filtros()$ano >= 2014, round(100 - 100*data4()$prop_robson3_tx_cesariana/3, 1), NaN),
          ifelse(filtros()$ano >= 2014, round(100 - 100*data4()$prop_robson4_tx_cesariana/15, 1), NaN),
          ifelse(
            filtros()$ano >= 2014,
            dplyr::case_when(
              data4()$prop_robson5_tx_cesariana < 50 ~ round(100 - 100*data4()$prop_robson5_tx_cesariana/50, 1),
              data4()$prop_robson5_tx_cesariana > 60 ~ round(100 - 100*data4()$prop_robson5_tx_cesariana/60, 1),
              dplyr::between(data4()$prop_robson5_tx_cesariana, 50, 60) ~ 0
            ),
            NaN
          ),
          NaN,
          ifelse(filtros()$ano >= 2014, round(100 - 100*data4()$prop_robson10_tx_cesariana/30, 1), NaN)
        ),
        invertido = c(
          rep(FALSE, 8)
        ),
        tem_meta = c(
          rep(TRUE, 8)
        ),
        tem_faixa_de_referencia = c(
          FALSE,
          FALSE,
          TRUE,
          FALSE,
          FALSE,
          TRUE,
          FALSE,
          FALSE
        )
      )

      cria_textos <- function(infos_comp) {
        textos_aux <- apply(infos_comp, 1, function(linha) {

          if (linha[2] == TRUE) {
            icone_comp <- dplyr::case_when(
              linha[1] > 0 ~ glue::glue("{fontawesome::fa('caret-down', fill = '#800000')}"),  #vermelho
              linha[1] < 0 ~ glue::glue("{fontawesome::fa('caret-up', fill = '#008000')}"),  #verde
              linha[1] == 0 | is.nan(linha[1]) ~ ""
            )
          } else {
            if (linha[4] == FALSE) {
              icone_comp <- dplyr::case_when(
                linha[1] < 0 ~ glue::glue("{fontawesome::fa('caret-up', fill = '#800000')}"),  #vermelho
                linha[1] > 0 ~ glue::glue("{fontawesome::fa('caret-down', fill = '#008000')}"),  #verde
                linha[1] == 0 | is.nan(linha[1]) ~ ""
              )
            } else {
              icone_comp <- dplyr::case_when(
                linha[1] < 0 ~ glue::glue("{fontawesome::fa('caret-up', fill = '#800000')}"),  #vermelho
                linha[1] > 0 ~ glue::glue("{fontawesome::fa('caret-down', fill = '#800000')}"),  #vermelho
                linha[1] == 0 | is.nan(linha[1]) ~ ""
              )
            }
          }

          if (isTruthy(linha[1])) {
            valor_comp_formatado <- formatC(abs(linha[1]), big.mark = '.', decimal.mark = ',')
          } else {
            valor_comp_formatado <- NaN
          }

          if (isTruthy(linha[1])) {
            razao <- round((100 - linha[1])/100, 1)
          } else {
            razao <- 0
          }

          if (razao >= 2) {
            texto_comp <- glue::glue("{icone_comp} {formatC(razao, big.mark = '.', decimal.mark = ',')} vezes maior que o valor de referência")
          } else {
            if (linha[4] == FALSE) {
              texto_comp <- dplyr::case_when(
                linha[1] < 0 ~ glue::glue("{icone_comp} {valor_comp_formatado}% maior que o valor de referência"),
                linha[1] > 0 ~ glue::glue("{icone_comp} {valor_comp_formatado}% menor que o valor de referência"),
                linha[1] == 0 & linha[3] == TRUE ~ glue::glue("{fontawesome::fa('check', fill = '#008000')} &nbsp; Igual ao valor de referência"),
                linha[1] == 0 & linha[3] == FALSE & filtros()$nivel != "nacional" ~ glue::glue("{fontawesome::fa('check', fill = '#008000')} &nbsp; Igual ao valor de referência"),
                linha[1] == 0 & linha[3] == FALSE & filtros()$nivel == "nacional" ~ glue::glue("Comparação não aplicável (a média nacional é o valor de referência)"),
                is.nan(linha[1]) ~ "Comparação não aplicável"
              )
            } else {
              texto_comp <- dplyr::case_when(
                linha[1] < 0 ~ glue::glue("{icone_comp} {valor_comp_formatado}% maior que o valor de referência máximo"),
                linha[1] > 0 ~ glue::glue("{icone_comp} {valor_comp_formatado}% menor que o valor de referência mínimo"),
                linha[1] == 0 & linha[3] == TRUE ~ glue::glue("{fontawesome::fa('check', fill = '#008000')} &nbsp; Dentro da faixa de referência"),
                linha[1] == 0 & linha[3] == FALSE & filtros()$nivel != "nacional" ~ glue::glue("{fontawesome::fa('check', fill = '#008000')} &nbsp; Dentro da faixa de referência"),
                linha[1] == 0 & linha[3] == FALSE & filtros()$nivel == "nacional" ~ glue::glue("Comparação não aplicável (a média nacional é o valor de referência)"),
                is.nan(linha[1]) ~ "Comparação não aplicável"
              )
            }

          }

          return(texto = texto_comp)
        }
        )
        return(texto = textos_aux)
      }

      comparacao_porc_cesareas <- c(
        cria_textos(infos_comp)[1],
        cria_textos(infos_comp)[2],
        cria_textos(infos_comp)[3],
        cria_textos(infos_comp)[4],
        cria_textos(infos_comp)[5],
        cria_textos(infos_comp)[6],
        cria_textos(infos_comp)[7],
        cria_textos(infos_comp)[8]
      )



      data.frame(grupo_robson, porc_nascidos, porc_cesareas, referencia_porc_cesareas, comparacao_porc_cesareas) |>
        reactable::reactable(
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold"),
            align = "center"
          ),
          columns = list(
            grupo_robson = reactable::colDef(
              name = "Categoria",
              minWidth = 60
            ),
            porc_nascidos = reactable::colDef(
              name = "Porcentagem de nascidos vivos",
              minWidth = 50
            ),
            porc_cesareas = reactable::colDef(
              name = "Porcentagem de cesarianas",
              minWidth = 50
            ),
            referencia_porc_cesareas = reactable::colDef(
              name = "Valor de referência para a % de cesarianas (OMS)",
              minWidth = 60
            ),
            comparacao_porc_cesareas = reactable::colDef(
              name = "Comparação para a % de cesarianas",
              minWidth = 80,
              html = TRUE
            )
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
        )
    })


    ##### Criando as caixinhas para os indicadores do quarto bloco #####
    output$caixa_b4_i1_muni_uf <- output$caixa_b4_i1_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "prop_partos_fora_macro_rsaude_res",
        titulo = "Porcentagem de partos ocorridos fora da macrorregião de saúde, mas dentro da UF de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_deslocamento()$prop_partos_fora_macro_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = dplyr::if_else(filtros()$nivel == "municipal" | filtros()$nivel == "estadual", 11, 12)
      )
    })

    output$caixa_b4_i2 <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_macrorregiao",
        titulo = "Mediana de deslocamento do total de partos ocorridos fora da macrorregião de saúde, mas na UF de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i3 <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_macrorregiao_baixa_complexidade",
        titulo = "Mediana de deslocamento para serviços de baixa complexidade em partos ocorridos fora da macrorregião de saúde, mas na UF de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i4 <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_macrorregiao_alta_complexidade",
        titulo = "Mediana de deslocamento para serviços de alta complexidade em partos ocorridos fora da macrorregião de saúde, mas na UF de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i6_muni_uf <- output$caixa_b4_i6_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_municipio",
        titulo = "Mediana de deslocamento do total de partos ocorridos fora do município de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i7_muni_uf <- output$caixa_b4_i7_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_municipio_baixa_complexidade",
        titulo = "Mediana de deslocamento para serviços de baixa complexidade do total de partos ocorridos fora do município de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i8_muni_uf <- output$caixa_b4_i8_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "km_partos_fora_municipio_alta_complexidade",
        titulo = "Mediana de deslocamento para serviços de alta complexidade do total de partos ocorridos fora do município de residência da mulher",
        tem_meta = FALSE,
        valor_de_referencia = NaN,
        tipo = "km",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i1_deslocamento_muni <- output$caixa_b4_i1_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "prop_partos_municipio_res",
        titulo = "Porcentagem de partos ocorridos no município de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_deslocamento()$prop_partos_municipio_res,
        tipo = "porcentagem",
        invertido = TRUE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i2_deslocamento_muni <- output$caixa_b4_i2_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "prop_partos_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na região de saúde, mas fora do município de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_deslocamento()$prop_partos_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i3_deslocamento_muni <- output$caixa_b4_i3_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "prop_partos_macro_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na macrorregião de saúde, mas fora da região de saúde de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_deslocamento()$prop_partos_macro_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i4_deslocamento_muni <- output$caixa_b4_i4_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "prop_partos_fora_macro_rsaude_res",
        titulo = "Porcentagem de partos ocorridos na UF, mas fora da macrorregião de saúde de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_deslocamento()$prop_partos_fora_macro_rsaude_res,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i5_deslocamento_muni <- output$caixa_b4_i5_deslocamento_resto <- renderUI({
      cria_caixa_server(
        dados = data4_deslocamento(),
        indicador = "prop_partos_fora_uf_res",
        titulo = "Porcentagem de partos ocorridos fora da UF de residência",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_deslocamento()$prop_partos_fora_uf_res,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        width_caixa = 11
      )
    })

    output$caixa_b4_i9_deslocamento_muni <- output$caixa_b4_i9_deslocamento_resto <- renderUI({ #[fff]
      tagList(
        div(
          style = "position: relative;",
          cria_caixa_server(
            dados = data4_deslocamento_macrorregiao(),
            indicador = "prop_partos_com_4mais_uti",
            # titulo = "Porcentagem de nascidos vivos com peso <1500g ocorridos em hospital com leito de UTI neonatal",
            titulo = "Porcentagem de nascidos vivos com peso <1500g ocorridos em serviço com quatro ou mais leitos de UTI neonatal",
            tem_meta = TRUE,
            valor_de_referencia = data4_comp_deslocamento_macrorregiao()$prop_partos_com_4mais_uti,
            # valor_de_referencia = data4_comp_deslocamento_macrorregiao()$prop_partos_sem_uti,
            tipo = "porcentagem",
            tipo_referencia = "média nacional",
            invertido = TRUE,
            pagina = "nivel_1",
            nivel_de_analise = filtros()$nivel,
            width_caixa = 11
          )
        )
      )
    })

    ##TAGGG
    output$caixa_b4_i1_profissional <- renderUI({
      cria_caixa_server(
        dados = data4_profissional(),
        indicador = "prop_nasc_local_fora_hospital",
        titulo = "Porcentagem de partos fora do hospital",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_profissional()$prop_nasc_local_fora_hospital,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    output$caixa_b4_i2_profissional <- renderUI({
      cria_caixa_server(
        dados = data4_profissional(),
        indicador = "prop_nasc_assistido_enf_obs",
        titulo = "Porcentagem de partos vaginais hospitalares assistidos por enfermeiras obstétricas",
        tem_meta = FALSE,
        valor_de_referencia = data4_comp_profissional()$prop_nasc_assistido_enf_obs,
        tipo = "porcentagem",
        invertido = TRUE,
        tamanho_caixa = dplyr::if_else(filtros()$comparar == "Sim", "273px", "300px"),
        #fonte_titulo = "15px",
        pagina = "bloco_4",
        nivel_de_analise = ifelse(
          filtros()$comparar == "Não",
          filtros()$nivel,
          ifelse(
            input$localidade_resumo4 == "escolha1",
            filtros()$nivel,
            filtros()$nivel2
          )
        )
      )
    })

    observeEvent(filtros()$pesquisar, {
      if (filtros()$nivel != "municipal" & filtros()$nivel != "estadual") {
        shinyjs::hide(id = "caixa_bloco4_muni_uf", anim = TRUE, animType = "fade", time = 0.001)
        shinyjs::show(id = "caixa_bloco4_resto", anim = TRUE, animType = "fade", time = 0.8)
      } else {
        shinyjs::hide(id = "caixa_bloco4_resto", anim = TRUE, animType = "fade", time = 0.001)
        shinyjs::show(id = "caixa_bloco4_muni_uf", anim = TRUE, animType = "fade", time = 0.8)
      }
    },
    ignoreNULL = FALSE
    )


    ##### Dados do quinto bloco de indicadores para a localidade escolhida #####
    malformacao2 <- malformacao |>
      dplyr::select(c(1:3), 5, 14) |>
      dplyr::group_by(ano, codmunres, municipio, uf) |>
      dplyr::summarise(nascidos_vivos_anomalia = sum(nascidos_vivos_anomalia))

    data5 <- reactive({
      bloco5 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          porc_nasc_baixo_peso = round(sum(nascidos_vivos_com_baixo_peso) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_nasc_peso_menor_1000 = round(sum(nascidos_vivos_peso_menor_1000) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_nasc_peso_1000_a_1499 = round(sum(nascidos_vivos_peso_1000_a_1499) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_nasc_peso_1500_a_2499= round(sum(nascidos_vivos_peso_1500_a_2499) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_baixo_peso_menor_1000 = round(sum(nascidos_vivos_peso_menor_1000) / sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_baixo_peso_1000_a_1499 = round(sum(nascidos_vivos_peso_1000_a_1499) / sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_baixo_peso_1500_a_2499 = round(sum(nascidos_vivos_peso_1500_a_2499) / sum(nascidos_vivos_com_baixo_peso) * 100, 1),
          porc_nasc_premat = round(sum(nascidos_vivos_prematuros) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_nasc_menos_de_28_semanas = round(sum(nascidos_vivos_menos_de_28_semanas) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_nasc_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_nasc_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_nasc_35_a_36_semanas = round(sum(nascidos_vivos_35_a_36_semanas) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_premat_menos_de_28_semanas = round(sum(nascidos_vivos_menos_de_28_semanas) / sum(nascidos_vivos_prematuros) * 100, 1),
          porc_premat_28_a_32_semanas = round(sum(nascidos_vivos_28_a_32_semanas) / sum(nascidos_vivos_prematuros) * 100, 1),
          porc_premat_33_a_34_semanas = round(sum(nascidos_vivos_33_a_34_semanas) / sum(nascidos_vivos_prematuros) * 100, 1),
          porc_premat_35_a_36_semanas = round(sum(nascidos_vivos_35_a_36_semanas) / sum(nascidos_vivos_prematuros) * 100, 1),
          porc_premat_faltantes = round((sum(nascidos_vivos_prematuros) - sum(dplyr::across(c(nascidos_vivos_menos_de_28_semanas,
                                                                                              nascidos_vivos_28_a_32_semanas,
                                                                                              nascidos_vivos_33_a_34_semanas,
                                                                                              nascidos_vivos_35_a_36_semanas)))) / sum(nascidos_vivos_prematuros) * 100, 1),
          porc_termo_precoce = round(sum(nascidos_vivos_termo_precoce) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_condicoes_ameacadoras = round(sum(nascidos_condicoes_ameacadoras) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1) / sum(total_nascidos) * 100, 1),
          porc_malformacao_geral = round(sum(total_de_nascidos_malformacao) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_malformacao_vigilancia = round(sum(nascidos_vivos_anomalia) / sum(total_de_nascidos_vivos) * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Dados do quinto bloco de indicadores para a comparação com o Brasil #####
    data5_comp <- reactive({
      bloco5 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_nasc_premat = 10,
          porc_termo_precoce = 20,
          porc_condicoes_ameacadoras = round(sum(nascidos_condicoes_ameacadoras) / total_de_nascidos_vivos * 100, 1),
          porc_nascidos_vivos_asfixia1 = round(sum(nascidos_vivos_asfixia1) / sum(total_nascidos) * 100, 1),
          porc_malformacao_vigilancia = round(sum(nascidos_vivos_anomalia) / sum(total_de_nascidos_vivos) * 100, 1),
          porc_malformacao_geral = round(sum(total_de_nascidos_malformacao) / sum(total_de_nascidos_vivos) * 100, 1)
        ) |>
        dplyr::ungroup()
    })

    data5_comp_baixo_peso <- reactive({
      base_referencia_baixo_peso |>
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            regiao %in% unique(tabela_aux_municipios$regiao)
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(nascidos, na.rm = TRUE),
          porc_nasc_baixo_peso = round(sum(nasc_baixo_peso, na.rm = TRUE)/total_de_nascidos_vivos * 100*0.7, 1),
        )
    })


    ##### Criando as caixinhas para os indicadores do quinto bloco #####
    output$caixa_b5_i1 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_nasc_baixo_peso",
        titulo = "Porcentagem de baixo peso ao nascer (< 2500 g)",
        tem_meta = TRUE,
        valor_de_referencia = data5_comp_baixo_peso()$porc_nasc_baixo_peso,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel,
        tipo_referencia = "meta de redução global"
      )
    })

    output$caixa_b5_i2 <- renderUI({
      cria_caixa_conjunta_bloco5(
        dados = data5(),
        indicador = "baixo peso",
        titulo = "Dentre os nascidos vivos com baixo peso (< 2500 g),"
      )
    })

    output$caixa_b5_i3 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_nasc_premat",
        titulo = "Porcentagem de nascimentos prematuros (com menos de 37 semanas de gestação)",
        tem_meta = TRUE,
        valor_de_referencia = 8,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "países desenvolvidos",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b5_i4 <- renderUI({
      cria_caixa_conjunta_bloco5(
        dados = data5(),
        indicador = "prematuridade",
        titulo = "Dentre os nascimentos prematuros (com menos de 37 semanas de gestação),"
      )
    })

    output$caixa_b5_i5 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_termo_precoce",
        titulo = "Porcentagem de nascimentos termo precoce (com 37 ou 38 semanas de gestação)",
        tem_meta = TRUE,
        valor_de_referencia = 20,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "países desenvolvidos",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b5_i6 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_condicoes_ameacadoras",
        titulo = "Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida",
        tem_meta = FALSE,
        valor_de_referencia = data5_comp()$porc_condicoes_ameacadoras,
        tipo = "porcentagem",
        invertido = FALSE,
        #fonte_titulo = "15px",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b5_i7 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_nascidos_vivos_asfixia1",
        titulo = "Porcentagem de nascidos vivos com asfixia dentre os nascidos vivos sem anomalias e com peso ≥ 2500 g",
        tem_meta = FALSE,
        valor_de_referencia = data5_comp()$porc_nascidos_vivos_asfixia1,
        tipo = "porcentagem",
        invertido = FALSE,
        #fonte_titulo = "15px",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b5_i8 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_malformacao_vigilancia",
        titulo = "Porcentagem de nascidos vivos com anomalias congênitas prioritárias para vigilância definidas pelo Ministério da Saúde",
        tem_meta = TRUE,
        valor_de_referencia = data5_comp()$porc_malformacao_vigilancia,
        tipo = "porcentagem",
        invertido = FALSE,
        #fonte_titulo = "15px",
        cor = "lightgrey",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b5_i9 <- renderUI({
      cria_caixa_server(
        dados = data5(),
        indicador = "porc_malformacao_geral",
        titulo = "Porcentagem de nascidos vivos com anomalias congênitas",
        tem_meta = TRUE,
        # valor_de_referencia = data5_comp()$porc_malformacao_geral,
        valor_de_referencia = c(3, 6),
        tipo = "porcentagem",
        invertido = FALSE,
        #fonte_titulo = "15px",
        cor = "lightgrey",
        pagina = "nivel_1",
        # tipo_referencia = "média nacional",
        tipo_referencia = "mundo",
        nivel_de_analise = filtros()$nivel
      )
    })


    ##### Dados do sexto bloco para a localidade escolhida #####
    data6 <- reactive({
      bloco6 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(nascidos),
          obitos_mat_totais = sum(obitos_mat_totais),
          rmm = round(obitos_mat_totais/total_de_nascidos_vivos * 100000, 1),
          prop_obitos_diretos = round(sum(obitos_mat_diretos)/sum(obitos_mat_totais) * 100, 1),
          prop_obitos_aborto = round(sum(obitos_mat_aborto)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_hipertens = round(sum(obitos_mat_hipertensao)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_hemo = round(sum(obitos_mat_hemorragia)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_infec = round(sum(obitos_mat_infec_puerperal)/sum(obitos_mat_diretos) * 100, 1),
          casos_mmg = sum(casos_mmg),
          prop_mmg_int_publicas = round(casos_mmg/sum(total_internacoes) * 100, 1),
          prop_mmg_hipertensao = round(sum(casos_mmg_hipertensao)/casos_mmg * 100, 1),
          prop_mmg_hemorragia = round(sum(casos_mmg_hemorragia)/casos_mmg * 100, 1),
          prop_mmg_infeccao = round(sum(casos_mmg_infeccoes)/casos_mmg * 100, 1),
          prop_mmg_uti = round(sum(casos_mmg_uti)/casos_mmg * 100, 1),
          prop_mmg_tmp = round(sum(casos_mmg_tmp)/casos_mmg * 100, 1),
          prop_mmg_transfusao = round(sum(casos_mmg_transfusao)/casos_mmg * 100, 1),
          prop_mmg_cirurgia = round(sum(casos_mmg_cirurgia)/casos_mmg * 100, 1)
        ) |>
        dplyr::ungroup()
    })

    data6_fator_de_correcao <- reactive({
      if (filtros()$nivel %in% c("estadual", "regional", "nacional") & filtros()$ano < 2023) {
        if (filtros()$nivel == "estadual") {
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$estado,
              ano == filtros()$ano
            ) |>
            dplyr::mutate(
              ano = filtros()$ano
            )
        } else if (filtros()$nivel == "regional") {
          rmm_corrigida |>
            dplyr::filter(
              localidade == filtros()$regiao,
              ano == filtros()$ano
            ) |>
            dplyr::mutate(
              ano = filtros()$ano
            )
        } else {
          rmm_corrigida |>
            dplyr::filter(
              localidade == "Brasil",
              ano == filtros()$ano
            ) |>
            dplyr::mutate(
              ano = filtros()$ano
            )
        }
      } else {
        data.frame(
          ano = filtros()$ano,
          localidade = dplyr::case_when(
            filtros()$nivel == "nacional" ~ "Brasil",
            filtros()$nivel == "regional" ~ filtros()$regiao,
            filtros()$nivel == "estadual" ~ filtros()$estado,
            filtros()$nivel == "macro" ~ filtros()$macro,
            filtros()$nivel == "micro" ~ filtros()$micro,
            filtros()$nivel == "municipal" ~ filtros()$municipio
          )
        )
      }
    })

    data6_rmm_corrigida_aux <- reactive({
      dplyr::full_join(data6(), data6_fator_de_correcao(), by = "ano")
    })

    data6_rmm_corrigida <- reactive({
      if (filtros()$nivel %in% c("nacional", "regional", "estadual") & filtros()$ano < 2023){
        data6_rmm_corrigida_aux() |>
          dplyr::mutate(
            rmm_c = RMM
          )
      } else {
        data6_rmm_corrigida_aux() |>
          dplyr::mutate(
            rmm_c = rmm
          )
      }

    })


    ##### Dados do sexto bloco para a comparação com o Brasil #####
    data6_comp <- reactive({
      bloco6 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(nascidos),
          obitos_mat_totais = sum(obitos_mat_totais),
          rmm = 30,
          prop_obitos_diretos = round(sum(obitos_mat_diretos)/sum(obitos_mat_totais) * 100, 1),
          prop_obitos_aborto = round(sum(obitos_mat_aborto)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_hipertens = round(sum(obitos_mat_hipertensao)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_hemo = round(sum(obitos_mat_hemorragia)/sum(obitos_mat_diretos) * 100, 1),
          prop_obitos_infec = round(sum(obitos_mat_infec_puerperal)/sum(obitos_mat_diretos) * 100, 1),
          casos_mmg = sum(casos_mmg),
          prop_mmg_int_publicas = round(casos_mmg/sum(total_internacoes) * 100, 1),
          prop_mmg_hipertensao = round(sum(casos_mmg_hipertensao)/casos_mmg * 100, 1),
          prop_mmg_hemorragia = round(sum(casos_mmg_hemorragia)/casos_mmg * 100, 1),
          prop_mmg_infeccao = round(sum(casos_mmg_infeccoes)/casos_mmg * 100, 1),
          prop_mmg_uti = round(sum(casos_mmg_uti)/casos_mmg * 100, 1),
          prop_mmg_tmp = round(sum(casos_mmg_tmp)/casos_mmg * 100, 1),
          prop_mmg_transfusao = round(sum(casos_mmg_transfusao)/casos_mmg * 100, 1),
          prop_mmg_cirurgia = round(sum(casos_mmg_cirurgia)/casos_mmg * 100, 1)
        ) |>
        dplyr::ungroup()
    })


    ##### Criando as caixinhas para os indicadores do sexto bloco #####

    output$caixa_b6_mort_i1 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "obitos_mat_totais",
        titulo = "Número de óbitos maternos",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$obitos_mat_totais,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        #cor = dplyr::if_else(filtros()$nivel == "nacional", "lightgrey", "#cbd6ff"),
        texto_footer = dplyr::if_else(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i2 <- renderUI({
      cria_caixa_server(
        dados = data6_rmm_corrigida(),
        indicador = "rmm_c",
        titulo = "Razão de mortalidade materna por 100.000 nascidos vivos",
        tem_meta = TRUE,
        valor_de_referencia = 30,
        tipo = "taxa",
        invertido = FALSE,
        pagina = "nivel_1",
        tipo_referencia = "meta ODS",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i3 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_diretos",
        titulo = "Porcentagem de óbitos maternos por causas obstétricas diretas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_diretos,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i4 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_aborto",
        titulo = "Porcentagem de óbitos maternos diretos por aborto",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_aborto,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i5 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_hemo",
        titulo = "Porcentagem de óbitos maternos diretos por causas hemorrágicas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_hemo,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i6 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_hipertens",
        titulo = "Porcentagem de óbitos maternos diretos por causas hipertensivas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_hipertens,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_mort_i7 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_obitos_infec",
        titulo = "Porcentagem de óbitos maternos diretos por infecção puerperal",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_obitos_infec,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i1 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_int_publicas",
        titulo = "Porcentagem de casos de morbidade materna grave no SUS",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_int_publicas,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i2 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_hipertensao",
        titulo = "Porcentagem de casos de morbidade materna grave por hipertensão",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_hipertensao,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i3 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_hemorragia",
        titulo = "Porcentagem de casos de morbidade materna grave por hemorragia",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_hemorragia,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i4 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_infeccao",
        titulo = "Porcentagem de casos de morbidade materna grave por infecção",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_infeccao,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i5 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_uti",
        titulo = "Porcentagem de casos de morbidade materna grave com internação em UTI",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_uti,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i6 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_tmp",
        titulo = "Porcentagem de casos de morbidade materna grave com Tempo de Permanência Prolongada",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_tmp,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i7 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_transfusao",
        titulo = "Porcentagem de casos de morbidade materna grave com transfusão sanguínea",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_transfusao,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b6_morb_i8 <- renderUI({
      cria_caixa_server(
        dados = data6(),
        indicador = "prop_mmg_cirurgia",
        titulo = "Porcentagem de casos de morbidade materna grave com intervenções cirúrgicas",
        tem_meta = FALSE,
        valor_de_referencia = data6_comp()$prop_mmg_cirurgia,
        tipo = "porcentagem",
        invertido = FALSE,
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    ######### Bloco 7

    ##### Dados do sétimo bloco para a localidade escolhida #####
    data7 <- reactive({
      bloco7 |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(

          obitos_neonat = sum(obitos_27dias, na.rm = T),
          obitos_neonat_menos1000 = sum(obitos_27dias_menos1000, na.rm = T),
          obitos_neonat_1000_1499 = sum(obitos_27dias_1000_1499, na.rm = T),
          obitos_neonat_1500_2499 = sum(obitos_27dias_1500_2499, na.rm = T),
          obitos_neonat_mais2500 = sum(obitos_27dias_mais2500, na.rm = T),
          mort_neonat = round(sum(obitos_27dias, na.rm = T)/sum(nascidos, na.rm = T) *1000, 1),
          mort_neonat_precoc = round(sum(obitos_6dias, na.rm = T)/sum(nascidos, na.rm = T) *1000, 1),
          mort_neonat_tardia = round(sum(obitos_7_27dias, na.rm = T)/sum(nascidos, na.rm = T) *1000, 1),
          mort_neonat_menos1000 = round(sum(obitos_27dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1),
          mort_neonat_precoc_menos1000 = round(sum(obitos_6dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1),
          mort_neonat_tardia_menos1000 = round(sum(obitos_7_27dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1),
          mort_neonat_1000_1499 = round(sum(obitos_27dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1),
          mort_neonat_precoc_1000_1499 = round(sum(obitos_6dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1),
          mort_neonat_tardia_1000_1499 = round(sum(obitos_7_27dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1),
          mort_neonat_1500_2499 = round(sum(obitos_27dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1),
          mort_neonat_precoc_1500_2499 = round(sum(obitos_6dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1),
          mort_neonat_tardia_1500_2499 =round(sum(obitos_7_27dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1),
          mort_neonat_mais2500 = round(sum(obitos_27dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1),
          mort_neonat_precoc_mais2500 = round(sum(obitos_6dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1),
          mort_neonat_tardia_mais2500 = round(sum(obitos_7_27dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1),
          obitos_fetais = sum(obitos_fetais_mais_22sem, na.rm = T),
          fetal_peso_menos_1000 = sum(fetal_peso_menos_1000, na.rm = T),
          fetal_peso_1000_1499 = sum(fetal_peso_1000_1499, na.rm = T),
          fetal_peso_1500_2499 = sum(fetal_peso_1500_2499, na.rm = T),
          fetal_peso_mais_2500 = sum(fetal_peso_mais_2500, na.rm = T),
          fetal_antes = sum(fetal_antes, na.rm = T),
          fetal_durante = sum(fetal_durante, na.rm = T),
          fetal_depois = sum(fetal_depois, na.rm = T),
          fetal_antes_peso_menos_1000 = sum(fetal_antes_peso_menos_1000, na.rm = T),
          fetal_antes_peso_1000_1499 = sum(fetal_antes_peso_1000_1499, na.rm = T),
          fetal_antes_peso_1500_2499 = sum(fetal_antes_peso_1500_2499, na.rm = T),
          fetal_antes_peso_mais_2500 = sum(fetal_antes_peso_mais_2500, na.rm = T),
          fetal_durante_peso_menos_1000 = sum(fetal_durante_peso_menos_1000, na.rm = T),
          fetal_durante_peso_1000_1499 = sum(fetal_durante_peso_1000_1499, na.rm = T),
          fetal_durante_peso_1500_2499 = sum(fetal_durante_peso_1500_2499, na.rm = T),
          fetal_durante_peso_mais_2500 = sum(fetal_durante_peso_mais_2500, na.rm = T),
          fetal_depois_peso_menos_1000 = sum(fetal_depois_peso_menos_1000, na.rm = T),
          fetal_depois_peso_1000_1499 = sum(fetal_depois_peso_1000_1499, na.rm = T),
          fetal_depois_peso_1500_2499 = sum(fetal_depois_peso_1500_2499, na.rm = T),
          fetal_depois_peso_mais_2500 = sum(fetal_depois_peso_mais_2500, na.rm = T),
          taxa_mort_fetal = round(sum(obitos_fetais_mais_22sem, na.rm = T)/(sum(nascidos, na.rm = T)+sum(obitos_fetais_mais_22sem, na.rm = T)) *1000, 1),
          taxa_mort_fetal_peso_menos_1000 = round(sum(fetal_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_peso_menos_1000, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_peso_1000_1499 =  round(sum(fetal_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_peso_1000_1499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_peso_1500_2499 =  round(sum(fetal_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_peso_1500_2499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_peso_mais_2500 =  round(sum(fetal_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_peso_mais_2500, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes =  round(sum(fetal_antes, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_antes, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante =  round(sum(fetal_durante, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_durante, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois =  round(sum(fetal_depois, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_depois, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes_peso_menos_1000 =  round(sum(fetal_antes_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_antes_peso_menos_1000, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes_peso_1000_1499 =  round(sum(fetal_antes_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_antes_peso_1000_1499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes_peso_1500_2499 =  round(sum(fetal_antes_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_antes_peso_1500_2499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes_peso_mais_2500 =  round(sum(fetal_antes_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_antes_peso_mais_2500, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante_peso_menos_1000 =  round(sum(fetal_durante_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_durante_peso_menos_1000, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante_peso_1000_1499 =  round(sum(fetal_durante_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_durante_peso_1000_1499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante_peso_1500_2499 =  round(sum(fetal_durante_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_durante_peso_1500_2499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante_peso_mais_2500 =  round(sum(fetal_durante_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_durante_peso_mais_2500, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois_peso_menos_1000 =  round(sum(fetal_depois_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_depois_peso_menos_1000, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois_peso_1000_1499 =  round(sum(fetal_depois_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_depois_peso_1000_1499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois_peso_1500_2499 =  round(sum(fetal_depois_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_depois_peso_1500_2499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois_peso_mais_2500 =  round(sum(fetal_depois_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_depois_peso_mais_2500, na.rm = T)) *1000, 1) ,
          # # Variáveis número de óbitos fetais mais de 28 semanas (critério oms)
          #obitos_fetais_oms =  sum(obitos_fetais_mais_28sem, na.rm=T),
          # fetal_oms_peso_menos_1000 =  sum(peso_menos_1000_mais_28sem, na.rm = T)",2),
          # fetal_oms_peso_1000_1499 =  sum(peso_1000_1499_mais_28sem, na.rm=T)",2),
          # fetal_oms_peso_1500_2499 =  sum(peso_1500_2499_mais_28sem, na.rm=T)",2),
          # fetal_oms_peso_mais2500 =  sum(peso_mais_2500_mais_28sem, na.rm=T) ,
          # fetal_oms_antes =  sum(perinatal_antes)",2),
          # fetal_oms_durante =  sum(perinatal_durante)",2),
          # fetal_oms_antes_peso_menos_1000 =  sum(perinatal_antes_peso_menos_1000)",2),
          # fetal_oms_antes_peso_1000_1499 =  sum(perinatal_antes_peso_1000_1499)",2),
          # fetal_oms_antes_peso_1500_2499 =  sum(perinatal_antes_peso_1500_2499)",2),
          # fetal_oms_antes_peso_mais_2500 =  sum(perinatal_antes_peso_mais_2500)",2),
          # fetal_oms_durante_peso_menos_1000 =  sum(perinatal_durante_peso_menos_1000)",2),
          # fetal_oms_durante_peso_1000_1499 =  sum(perinatal_durante_peso_1000_1499)",2),
          # fetal_oms_durante_peso_1500_2499 =  sum(perinatal_durante_peso_1500_2499)",2),
          # fetal_oms_durante_peso_mais_2500 =  sum(perinatal_durante_peso_mais_2500)",2),
          # # Variáveis sobre taxa de mortalidades fetal para mais de 28 semanas (critério oms)
          #taxa_mort_fetal_oms =  round(sum(obitos_fetais_mais_28sem)/(sum(nascidos)+sum(obitos_fetais_mais_28sem)) *1000, 1) ,
          # taxa_mort_fetal_oms_peso_menos_1000 =  round(sum(peso_menos_1000_mais_28sem)/(sum(nascidos_menos1000)+sum(peso_menos_1000_mais_28sem)),2)",2),
          # taxa_mort_fetal_oms_peso_1000_1499 =  round(sum(peso_1000_1499_mais_28sem)/(sum(nascidos_1000_1499)+sum(peso_1000_1499_28sem)),2)",2),
          # taxa_mort_fetal_oms_peso_1500_2499 =  round(sum(peso_1500_2499_mais_28sem)/(sum(nascidos_1500_2499)+sum(peso_1500_2499_28sem)),2)",2),
          # taxa_mort_fetal_oms_peso_mais_2500 =  round(sum(peso_mais_2500_mais_28sem)/(sum(nascidos_mais_2500)+sum(peso_mais_2500_28sem)),2)",2),
          # taxa_mort_fetal_oms_antes =  round(sum(perinatal_antes)/(sum(nascidos)+sum(perinatal_antes)),2)",2),
          # taxa_mort_fetal_oms_durante =  round(sum(perinatal_durante)/(sum(nascidos)+sum(perinatal_durante)),2)",2),
          # taxa_mort_fetal_oms_antes_peso_menos_1000 =  round(sum(perinatal_antes_peso_menos_1000)/(sum(nascidos_menos_1000)+sum(perinatal_antes_peso_menos_1000)),2)",2),

          perinatal_todos_total =  sum(perinatal_todos_total, na.rm = T),
          perinatal_todos_peso_menos_1000 =  sum(perinatal_todos_peso_menos_1000, na.rm = T),
          perinatal_todos_peso_1000_1499 =  sum(perinatal_todos_peso_1000_1499, na.rm = T),
          perinatal_todos_peso_1500_2499 =  sum(perinatal_todos_peso_1500_2499, na.rm = T)  ,
          perinatal_todos_peso_mais_2500 =  sum(perinatal_todos_peso_mais_2500, na.rm = T) ,
          # obitos_perinatal_oms =  sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias) ,
          # perinatal_oms_menos1000 =  sum(peso_menos_1000_mais_28sem, na.rm = T) + sum(obitos_6dias_menos1000) ,
          # perinatal_oms_1000_1499 =  sum(peso_1000_1499_mais_28sem, na.rm=T) + sum(obitos_6dias_1000_1499) ,
          # perinatal_oms_1500_2499 =  sum(peso_1500_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_2499) ,
          # perinatal_oms_mais2500 =  sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500) ,
          taxa_perinatal_total =  round((sum(obitos_fetais_mais_22sem, na.rm = T) + sum(obitos_6dias, na.rm = T))/(sum(obitos_fetais_mais_22sem, na.rm = T) + sum(nascidos, na.rm = T) )*1000, 1) ,
          taxa_perinatal_total_menos1000 =  round((sum(fetal_peso_menos_1000, na.rm = T) + sum(obitos_6dias_menos1000, na.rm = T))/(sum(fetal_peso_menos_1000, na.rm = T)+ sum(nascidos_menos1000, na.rm = T))*1000, 1) ,
          taxa_perinatal_total_1000_1499 =  round((sum(fetal_peso_1000_1499, na.rm = T) + sum(obitos_6dias_1000_1499, na.rm = T))/(sum(fetal_peso_1000_1499, na.rm = T)+sum(nascidos_1000_1499, na.rm = T))*1000, 1) ,
          taxa_perinatal_total_1500_2499 =  round((sum(fetal_peso_1500_2499, na.rm = T)+sum(obitos_6dias_1500_2499, na.rm = T))/(sum(fetal_peso_1500_2499, na.rm = T)+sum(nascidos_1500_2499, na.rm = T))*1000, 1) ,
          taxa_perinatal_total_mais2500 =  round((sum(fetal_peso_mais_2500, na.rm = T)+sum(obitos_6dias_mais2500, na.rm = T))/(sum(fetal_peso_mais_2500, na.rm = T)+sum(nascidos_mais2500, na.rm = T))*1000, 1) ,
          # taxa_perinatal_oms =  round((sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias))/(sum(obitos_fetais_mais_28sem, na.rm=T) + sum(nascidos) )*1000, 1) ,
          # taxa_perinatal_oms_menos1000 =  round((sum(peso_menos_1000_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1000))/(sum(peso_menos_1000_mais_28sem, na.rm=T)+ sum(nascidos_menos1000))*1000, 1) ,
          # taxa_perinatal_oms_1000_1499 =  round((sum(peso_1000_1499_mais_28sem, na.rm=T) + sum(obitos_6dias_1000_1499))/(sum(peso_1000_1499_mais_28sem, na.rm=T)+sum(nascidos_1000_1499))*1000, 1) ,
          # taxa_perinatal_oms_1500_2499 =  round((sum(peso_1500_2499_mais_28sem, na.rm=T)+sum(obitos_6dias_1500_2499))/(sum(peso_1500_2499_mais_28sem, na.rm=T)+sum(nascidos_1500_2499))*1000, 1) ,
          # taxa_perinatal_oms_mais2500 =  round((sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(obitos_6dias_mais2500))/(sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(nascidos_mais2500))*1000, 1) ,

          obitos_0dias =  sum(obitos_0dias, na.rm = T) ,
          obitos_0dias_menos1000 =  sum(obitos_0dias_menos1000, na.rm = T) ,
          obitos_0dias_1000_1499 =  sum(obitos_0dias_1000_1499, na.rm = T) ,
          obitos_0dias_1500_2499 =  sum(obitos_0dias_1500_2499, na.rm = T) ,
          obitos_0dias_mais2500 =  sum(obitos_0dias_mais2500, na.rm = T) ,
          obitos_1_6dias =  sum(obitos_1_6dias, na.rm = T) ,
          obitos_1_6dias_menos1000 =  sum(obitos_1_6dias_menos1000, na.rm = T) ,
          obitos_1_6dias_1000_1499 =  sum(obitos_1_6dias_1000_1499, na.rm = T) ,
          obitos_1_6dias_1500_2499 =  sum(obitos_1_6dias_1500_2499, na.rm = T) ,
          obitos_1_6dias_mais2500 =  sum(obitos_1_6dias_mais2500, na.rm = T) ,
          obitos_6dias =  sum(obitos_6dias, na.rm = T) ,
          obitos_6dias_menos1000 =  sum(obitos_6dias_menos1000, na.rm = T) ,
          obitos_6dias_1000_1499 =  sum(obitos_6dias_1000_1499, na.rm = T) ,
          obitos_6dias_1500_2499 =  sum(obitos_6dias_1500_2499, na.rm = T) ,
          obitos_6dias_mais2500 =  sum(obitos_6dias_mais2500, na.rm = T) ,
          obitos_27dias =  sum(obitos_27dias, na.rm = T) ,
          obitos_27dias_menos1000 =  sum(obitos_27dias_menos1000, na.rm = T) ,
          obitos_27dias_1000_1499 =  sum(obitos_27dias_1000_1499, na.rm = T) ,
          obitos_27dias_1500_2499 =  sum(obitos_27dias_1500_2499, na.rm = T) ,
          obitos_27dias_mais2500 =  sum(obitos_27dias_mais2500, na.rm = T) ,
          obitos_7_27dias =  sum(obitos_7_27dias, na.rm = T) ,
          obitos_7_27dias_menos1000 =  sum(obitos_7_27dias_menos1000, na.rm = T) ,
          obitos_7_27dias_1000_1499 =  sum(obitos_7_27dias_1000_1499, na.rm = T) ,
          obitos_7_27dias_1500_2499 =  sum(obitos_7_27dias_1500_2499, na.rm = T) ,
          obitos_7_27dias_mais2500 =  sum(obitos_7_27dias_mais2500, na.rm = T) ,

          # antes_dist_moment_obito_fetal =  round(sum(fetal_antes)/sum(obitos_fetais) * 100, 1),
          #
          # durante_dist_moment_obito_fetal = round(sum(fetal_durante)/sum(obitos_fetais) * 100, 1),
          #
          # faltante_dist_moment_obito_fetal =  round(100-antes_dist_moment_obito_fetal-durante_dist_moment_obito_fetal, 1),
          #
          # antes_dist_moment_obito_perinat =  round(sum(perinatal_antes)/sum(obitos_perinatal_oms) * 100, 1),
          #
          # durante_dist_moment_obito_perinat =  round(sum(perinatal_durante)/sum(obitos_perinatal_oms) * 100, 1),
          #
          # dia_0_dist_moment_obito_perinat =  round(sum(obitos_0dias)/sum(obitos_perinatal_oms) * 100, 1) ,
          #
          # dia_1_6_dist_moment_obito_perinat =  round(sum(obitos_1_6dias)/sum(obitos_perinatal_oms) * 100, 1) ,
          #
          # faltante_dist_moment_obito_perinat =  round(100 -antes_dist_moment_obito_perinat -durante_dist_moment_obito_perinat -dia_0_dist_moment_obito_perinat -dia_1_6_dist_moment_obito_perinat, 1) ,
          #
          # dia_0_dist_moment_obito_neonat =  round( sum(obitos_0dias)/sum(obitos_27dias) * 100, 1) ,
          #
          # dia_1_6dist_moment_obito_neonat =  round( sum(obitos_1_6dias)/sum(obitos_27dias) * 100, 1) ,
          #
          # dia_7_27dist_moment_obito_neonat =  round(sum(obitos_7_27dias)/sum(obitos_27dias) * 100, 1) ,
          #
          # faltante_moment_obito_neonat =  round(100 -dia_0_dist_moment_obito_neonat -dia_1_6dist_moment_obito_neonat -dia_7_27dist_moment_obito_neonat, 1) ,
          #
          # menos_1000_dist_peso_fetal =  round( sum(fetal_peso_menos_1000)/sum(obitos_fetais) * 100, 1) ,
          #
          # de_1000_1499_dist_peso_fetal =  round( sum(fetal_peso_1000_1499)/sum(obitos_fetais) * 100, 1) ,
          #
          # de_1500_2499_dist_peso_fetal =  round( sum(fetal_peso_1500_2499)/sum(obitos_fetais) * 100, 1) ,
          #
          # mais_2500_dist_peso_fetal =  round( sum(fetal_peso_mais_2500)/sum(obitos_fetais) * 100, 1) ,
          #
          # faltante_dist_peso_fetal =  round(100 -menos_1000_dist_peso_fetal-de_1000_1499_dist_peso_fetal-de_1500_2499_dist_peso_fetal -mais_2500_dist_peso_fetal, 1) ,
          #
          # menos_1000_dist_peso_perinat =  round( sum(perinatal_oms_menos1000)/sum(obitos_perinatal_oms) * 100, 1) ,
          #
          # de_1000_1499_dist_peso_perinat =  round( sum(perinatal_oms_1000_1499)/sum(obitos_perinatal_oms) * 100, 1) ,
          #
          #
          # de_1500_2499_dist_peso_perinat =  round( sum(perinatal_oms_1500_2499)/sum(obitos_perinatal_oms) * 100, 1) ,
          #
          # mais_2500_dist_peso_perinat =  round(sum(perinatal_oms_mais2500)/sum(obitos_perinatal_oms) * 100, 1) ,
          #
          # faltante_dist_peso_perinat =  round(100 -menos_1000_dist_peso_perinat -de_1000_1499_dist_peso_perinat -de_1500_2499_dist_peso_perinat -mais_2500_dist_peso_perinat, 1) ,
          #
          #
          # menos_1000_dist_peso_neonat =  round( sum(obitos_27dias_menos1000)/sum(obitos_27dias) * 100, 1) ,
          #
          # de_1000_1499_dist_peso_neonat =  round( sum(obitos_27dias_1000_1499)/sum(obitos_27dias) * 100, 1) ,
          #
          # de_1500_2499_dist_peso_neonat =  round( sum(obitos_27dias_1500_2499)/sum(obitos_27dias) * 100, 1) ,
          #
          # mais_2500_dist_peso_neonat =  round( sum(obitos_27dias_mais2500)/sum(obitos_27dias) * 100, 1) ,
          #
          # faltante_dist_peso_neonat =  round(100 -menos_1000_dist_peso_neonat -de_1000_1499_dist_peso_neonat -de_1500_2499_dist_peso_neonat -mais_2500_dist_peso_neonat, 1) ,

          antes_dist_moment_obito_fetal =  round(
            sum(c(fetal_antes_peso_menos_1000, fetal_antes_peso_1000_1499, fetal_antes_peso_1500_2499, fetal_antes_peso_mais_2500, fetal_antes)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(fetal_peso_menos_1000, fetal_peso_1000_1499, fetal_peso_1500_2499, fetal_peso_mais_2500, obitos_fetais)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          durante_dist_moment_obito_fetal =  round(
            sum(c(fetal_durante_peso_menos_1000, fetal_durante_peso_1000_1499, fetal_durante_peso_1500_2499, fetal_durante_peso_mais_2500, fetal_durante)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(fetal_peso_menos_1000, fetal_peso_1000_1499, fetal_peso_1500_2499, fetal_peso_mais_2500, obitos_fetais)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_moment_obito_fetal =  round(100-antes_dist_moment_obito_fetal-durante_dist_moment_obito_fetal, 1) ,

          antes_dist_moment_obito_perinat =  round(
            sum(c(fetal_antes_peso_menos_1000, fetal_antes_peso_1000_1499, fetal_antes_peso_1500_2499, fetal_antes_peso_mais_2500, fetal_antes)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          durante_dist_moment_obito_perinat =  round(
            sum(c(fetal_durante_peso_menos_1000, fetal_durante_peso_1000_1499, fetal_durante_peso_1500_2499, fetal_durante_peso_mais_2500, fetal_durante)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          dia_0_dist_moment_obito_perinat =  round(
            sum(c(obitos_0dias_menos1000, obitos_0dias_1000_1499, obitos_0dias_1500_2499, obitos_0dias_mais2500, obitos_0dias)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          dia_1_6_dist_moment_obito_perinat =  round(
            sum(c(obitos_1_6dias_menos1000, obitos_1_6dias_1000_1499, obitos_1_6dias_1500_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_moment_obito_perinat =  round(100 -antes_dist_moment_obito_perinat -durante_dist_moment_obito_perinat -dia_0_dist_moment_obito_perinat -dia_1_6_dist_moment_obito_perinat, 1) ,

          dia_0_dist_moment_obito_neonat =  round(
            sum(c(obitos_0dias_menos1000, obitos_0dias_1000_1499, obitos_0dias_1500_2499, obitos_0dias_mais2500, obitos_0dias)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          dia_1_6dist_moment_obito_neonat =  round(
            sum(c(obitos_1_6dias_menos1000, obitos_1_6dias_1000_1499, obitos_1_6dias_1500_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          dia_7_27dist_moment_obito_neonat =  round(
            sum(c(obitos_7_27dias_menos1000, obitos_7_27dias_1000_1499, obitos_7_27dias_1500_2499, obitos_7_27dias_mais2500, obitos_7_27dias)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          faltante_moment_obito_neonat =  round(100 -dia_0_dist_moment_obito_neonat -dia_1_6dist_moment_obito_neonat -dia_7_27dist_moment_obito_neonat, 1) ,

          menos_1000_dist_peso_fetal =  round(
            sum(c(fetal_antes_peso_menos_1000, fetal_durante_peso_menos_1000, fetal_peso_menos_1000)[c(rep(F, 2), T)], na.rm = T)/
              sum(c(fetal_antes, fetal_durante, obitos_fetais)[c(rep(F, 2), T)], na.rm = T)
            * 100, 1) ,

          de_1000_1499_dist_peso_fetal =  round(
            sum(c(fetal_antes_peso_1000_1499, fetal_durante_peso_1000_1499, fetal_peso_1000_1499)[c(rep(F, 2), T)], na.rm = T)/
              sum(c(fetal_antes, fetal_durante, obitos_fetais)[c(rep(F, 2), T)], na.rm = T)
            * 100, 1) ,

          de_1500_2499_dist_peso_fetal =  round(
            sum(c(fetal_antes_peso_1500_2499, fetal_durante_peso_1500_2499, fetal_peso_1500_2499)[c(rep(F, 2), T)], na.rm = T)/
              sum(c(fetal_antes, fetal_durante, obitos_fetais)[c(rep(F, 2), T)], na.rm = T)
            * 100, 1) ,

          mais_2500_dist_peso_fetal =  round(
            sum(c(fetal_antes_peso_mais_2500, fetal_durante_peso_mais_2500, fetal_peso_mais_2500)[c(rep(F, 2), T)], na.rm = T)/
              sum(c(fetal_antes, fetal_durante, obitos_fetais)[c(rep(F, 2), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_peso_fetal =  round(100 -menos_1000_dist_peso_fetal-de_1000_1499_dist_peso_fetal-de_1500_2499_dist_peso_fetal -mais_2500_dist_peso_fetal, 1) ,

          menos_1000_dist_peso_perinat =  round(
            sum(c(perinatal_todos_antes_menos_1000, perinatal_todos_durante_menos_1000, obitos_0dias_menos1000, obitos_1_6dias_menos1000, perinatal_todos_peso_menos_1000)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          de_1000_1499_dist_peso_perinat =  round(
            sum(c(perinatal_todos_antes_1000_1499, perinatal_todos_durante_1000_1499, obitos_0dias_1000_1499, obitos_1_6dias_1000_1499, perinatal_todos_peso_1000_1499)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,


          de_1500_2499_dist_peso_perinat =  round(
            sum(c(perinatal_todos_antes_1500_2499, perinatal_todos_durante_1500_2499, obitos_0dias_1500_2499, obitos_1_6dias_1500_2499, perinatal_todos_peso_1500_2499)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          mais_2500_dist_peso_perinat =  round(
            sum(c(perinatal_todos_antes_mais_2500 , perinatal_todos_durante_mais_2500, obitos_0dias_mais2500, obitos_1_6dias_mais2500, perinatal_todos_peso_mais_2500)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_peso_perinat =  round(100 -menos_1000_dist_peso_perinat -de_1000_1499_dist_peso_perinat -de_1500_2499_dist_peso_perinat -mais_2500_dist_peso_perinat, 1) ,


          menos_1000_dist_peso_neonat =  round(
            sum(c(obitos_0dias_menos1000, obitos_1_6dias_menos1000, obitos_7_27dias_menos1000, obitos_27dias_menos1000)[c(rep(F, 3), T)], na.rm = T)/
              sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[c(rep(F, 3), T)], na.rm = T)
            * 100, 1) ,

          de_1000_1499_dist_peso_neonat =  round(
            sum(c(obitos_0dias_1000_1499, obitos_1_6dias_1000_1499, obitos_7_27dias_1000_1499, obitos_27dias_1000_1499)[c(rep(F, 3), T)], na.rm = T)/
              sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[c(rep(F, 3), T)], na.rm = T)
            * 100, 1) ,

          de_1500_2499_dist_peso_neonat =  round(
            sum(c(obitos_0dias_1500_2499, obitos_1_6dias_1500_2499, obitos_7_27dias_1500_2499, obitos_27dias_1500_2499)[c(rep(F, 3), T)], na.rm = T)/
              sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[c(rep(F, 3), T)], na.rm = T)
            * 100, 1) ,

          mais_2500_dist_peso_neonat =  round(
            sum(c(obitos_0dias_mais2500, obitos_1_6dias_mais2500, obitos_7_27dias_mais2500, obitos_27dias_mais2500)[c(rep(F, 3), T)], na.rm = T)/
              sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[c(rep(F, 3), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_peso_neonat =  round(100 -menos_1000_dist_peso_neonat -de_1000_1499_dist_peso_neonat -de_1500_2499_dist_peso_neonat -mais_2500_dist_peso_neonat, 1),


          porc_condicoes_ameacadoras =  round(sum(nascidos_condicoes_ameacadoras, na.rm = T) / sum(nascidos, na.rm = T) * 100, 1) ,
          #porc_internacoes_menores_28_dias_sih_geral =  round(sum(internacoes_geral_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1) ,
          porc_internacoes_menores_28_dias_sih_geral =  round(sum(internacoes_geral_geral, na.rm = T) / sum(nascidos_estabelecimentos_publicos_sih, na.rm = T) * 100, 1) ,
          porc_internacoes_uti_menores_28_dias_sih_geral =  round(sum(internacoes_geral_geral_internado_uti, na.rm = T) / sum(nascidos_estabelecimentos_publicos_sih, na.rm = T) * 100, 1)

        ) |>
        dplyr::ungroup() |>
        dplyr::distinct(ano, .keep_all = T)
    })


    ##### Dados do sétimo bloco para a comparação com o Brasil #####
    data7_comp <- reactive({
      bloco7 |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          obitos_neonat = sum(obitos_27dias, na.rm = T),
          obitos_neonat_menos1000 = sum(obitos_27dias_menos1000, na.rm = T),
          obitos_neonat_1000_1499 = sum(obitos_27dias_1000_1499, na.rm = T),
          obitos_neonat_1500_2499 = sum(obitos_27dias_1500_2499, na.rm = T),
          obitos_neonat_mais2500 = sum(obitos_27dias_mais2500, na.rm = T),
          mort_neonat = 5,
          mort_neonat_precoc = 3.75,
          mort_neonat_tardia = 1.25,
          mort_neonat_menos1000 = round(sum(obitos_27dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1),
          mort_neonat_precoc_menos1000 = round(sum(obitos_6dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1),
          mort_neonat_tardia_menos1000 = round(sum(obitos_7_27dias_menos1000, na.rm = T)/sum(nascidos_menos1000, na.rm = T) *1000, 1),
          mort_neonat_1000_1499 = round(sum(obitos_27dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1),
          mort_neonat_precoc_1000_1499 = round(sum(obitos_6dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1),
          mort_neonat_tardia_1000_1499 = round(sum(obitos_7_27dias_1000_1499, na.rm = T)/sum(nascidos_1000_1499, na.rm = T) *1000, 1),
          mort_neonat_1500_2499 = round(sum(obitos_27dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1),
          mort_neonat_precoc_1500_2499 = round(sum(obitos_6dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1),
          mort_neonat_tardia_1500_2499 =round(sum(obitos_7_27dias_1500_2499, na.rm = T)/sum(nascidos_1500_2499, na.rm = T) *1000, 1),
          mort_neonat_mais2500 = round(sum(obitos_27dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1),
          mort_neonat_precoc_mais2500 = round(sum(obitos_6dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1),
          mort_neonat_tardia_mais2500 = round(sum(obitos_7_27dias_mais2500, na.rm = T)/sum(nascidos_mais2500, na.rm = T) *1000, 1),
          obitos_fetais = sum(obitos_fetais_mais_22sem, na.rm = T),
          fetal_peso_menos_1000 = sum(fetal_peso_menos_1000, na.rm = T),
          fetal_peso_1000_1499 = sum(fetal_peso_1000_1499, na.rm = T),
          fetal_peso_1500_2499 = sum(fetal_peso_1500_2499, na.rm = T),
          fetal_peso_mais_2500 = sum(fetal_peso_mais_2500, na.rm = T),
          fetal_antes = sum(fetal_antes, na.rm = T),
          fetal_durante = sum(fetal_durante, na.rm = T),
          fetal_depois = sum(fetal_depois, na.rm = T),
          fetal_antes_peso_menos_1000 = sum(fetal_antes_peso_menos_1000, na.rm = T),
          fetal_antes_peso_1000_1499 = sum(fetal_antes_peso_1000_1499, na.rm = T),
          fetal_antes_peso_1500_2499 = sum(fetal_antes_peso_1500_2499, na.rm = T),
          fetal_antes_peso_mais_2500 = sum(fetal_antes_peso_mais_2500, na.rm = T),
          fetal_durante_peso_menos_1000 = sum(fetal_durante_peso_menos_1000, na.rm = T),
          fetal_durante_peso_1000_1499 = sum(fetal_durante_peso_1000_1499, na.rm = T),
          fetal_durante_peso_1500_2499 = sum(fetal_durante_peso_1500_2499, na.rm = T),
          fetal_durante_peso_mais_2500 = sum(fetal_durante_peso_mais_2500, na.rm = T),
          fetal_depois_peso_menos_1000 = sum(fetal_depois_peso_menos_1000, na.rm = T),
          fetal_depois_peso_1000_1499 = sum(fetal_depois_peso_1000_1499, na.rm = T),
          fetal_depois_peso_1500_2499 = sum(fetal_depois_peso_1500_2499, na.rm = T),
          fetal_depois_peso_mais_2500 = sum(fetal_depois_peso_mais_2500, na.rm = T),
          taxa_mort_fetal = 5,
          taxa_mort_fetal_peso_menos_1000 = round(sum(fetal_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_peso_menos_1000, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_peso_1000_1499 =  round(sum(fetal_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_peso_1000_1499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_peso_1500_2499 =  round(sum(fetal_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_peso_1500_2499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_peso_mais_2500 =  round(sum(fetal_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_peso_mais_2500, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes =  round(sum(fetal_antes, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_antes, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante =  round(sum(fetal_durante, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_durante, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois =  round(sum(fetal_depois, na.rm = T)/(sum(nascidos, na.rm = T) + sum(fetal_depois, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes_peso_menos_1000 =  round(sum(fetal_antes_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_antes_peso_menos_1000, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes_peso_1000_1499 =  round(sum(fetal_antes_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_antes_peso_1000_1499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes_peso_1500_2499 =  round(sum(fetal_antes_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_antes_peso_1500_2499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_antes_peso_mais_2500 =  round(sum(fetal_antes_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_antes_peso_mais_2500, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante_peso_menos_1000 =  round(sum(fetal_durante_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_durante_peso_menos_1000, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante_peso_1000_1499 =  round(sum(fetal_durante_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_durante_peso_1000_1499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante_peso_1500_2499 =  round(sum(fetal_durante_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_durante_peso_1500_2499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_durante_peso_mais_2500 =  round(sum(fetal_durante_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_durante_peso_mais_2500, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois_peso_menos_1000 =  round(sum(fetal_depois_peso_menos_1000, na.rm = T)/(sum(nascidos_menos1000, na.rm = T)+sum(fetal_depois_peso_menos_1000, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois_peso_1000_1499 =  round(sum(fetal_depois_peso_1000_1499, na.rm = T)/(sum(nascidos_1000_1499, na.rm = T)+sum(fetal_depois_peso_1000_1499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois_peso_1500_2499 =  round(sum(fetal_depois_peso_1500_2499, na.rm = T)/(sum(nascidos_1500_2499, na.rm = T)+sum(fetal_depois_peso_1500_2499, na.rm = T)) *1000, 1) ,
          taxa_mort_fetal_depois_peso_mais_2500 =  round(sum(fetal_depois_peso_mais_2500, na.rm = T)/(sum(nascidos_mais2500, na.rm = T)+sum(fetal_depois_peso_mais_2500, na.rm = T)) *1000, 1) ,
          # # Variáveis número de óbitos fetais mais de 28 semanas (critério oms)
          #obitos_fetais_oms =  sum(obitos_fetais_mais_28sem, na.rm=T),
          # fetal_oms_peso_menos_1000 =  sum(peso_menos_1000_mais_28sem, na.rm = T)",2),
          # fetal_oms_peso_1000_1499 =  sum(peso_1000_1499_mais_28sem, na.rm=T)",2),
          # fetal_oms_peso_1500_2499 =  sum(peso_1500_2499_mais_28sem, na.rm=T)",2),
          # fetal_oms_peso_mais2500 =  sum(peso_mais_2500_mais_28sem, na.rm=T) ,
          # fetal_oms_antes =  sum(perinatal_antes)",2),
          # fetal_oms_durante =  sum(perinatal_durante)",2),
          # fetal_oms_antes_peso_menos_1000 =  sum(perinatal_antes_peso_menos_1000)",2),
          # fetal_oms_antes_peso_1000_1499 =  sum(perinatal_antes_peso_1000_1499)",2),
          # fetal_oms_antes_peso_1500_2499 =  sum(perinatal_antes_peso_1500_2499)",2),
          # fetal_oms_antes_peso_mais_2500 =  sum(perinatal_antes_peso_mais_2500)",2),
          # fetal_oms_durante_peso_menos_1000 =  sum(perinatal_durante_peso_menos_1000)",2),
          # fetal_oms_durante_peso_1000_1499 =  sum(perinatal_durante_peso_1000_1499)",2),
          # fetal_oms_durante_peso_1500_2499 =  sum(perinatal_durante_peso_1500_2499)",2),
          # fetal_oms_durante_peso_mais_2500 =  sum(perinatal_durante_peso_mais_2500)",2),
          # # Variáveis sobre taxa de mortalidades fetal para mais de 28 semanas (critério oms)
          #taxa_mort_fetal_oms =  5 ,
          # taxa_mort_fetal_oms_peso_menos_1000 =  round(sum(peso_menos_1000_mais_28sem)/(sum(nascidos_menos1000)+sum(peso_menos_1000_mais_28sem)),2)",2),
          # taxa_mort_fetal_oms_peso_1000_1499 =  round(sum(peso_1000_1499_mais_28sem)/(sum(nascidos_1000_1499)+sum(peso_1000_1499_28sem)),2)",2),
          # taxa_mort_fetal_oms_peso_1500_2499 =  round(sum(peso_1500_2499_mais_28sem)/(sum(nascidos_1500_2499)+sum(peso_1500_2499_28sem)),2)",2),
          # taxa_mort_fetal_oms_peso_mais_2500 =  round(sum(peso_mais_2500_mais_28sem)/(sum(nascidos_mais_2500)+sum(peso_mais_2500_28sem)),2)",2),
          # taxa_mort_fetal_oms_antes =  round(sum(perinatal_antes)/(sum(nascidos)+sum(perinatal_antes)),2)",2),
          # taxa_mort_fetal_oms_durante =  round(sum(perinatal_durante)/(sum(nascidos)+sum(perinatal_durante)),2)",2),
          # taxa_mort_fetal_oms_antes_peso_menos_1000 =  round(sum(perinatal_antes_peso_menos_1000)/(sum(nascidos_menos_1000)+sum(perinatal_antes_peso_menos_1000)),2)",2),

          perinatal_todos_total =  sum(perinatal_todos_total, na.rm = T),
          perinatal_todos_peso_menos_1000 =  sum(perinatal_todos_peso_menos_1000, na.rm = T),
          perinatal_todos_peso_1000_1499 =  sum(perinatal_todos_peso_1000_1499, na.rm = T),
          perinatal_todos_peso_1500_2499 =  sum(perinatal_todos_peso_1500_2499, na.rm = T)  ,
          perinatal_todos_peso_mais_2500 =  sum(perinatal_todos_peso_mais_2500, na.rm = T) ,
          # obitos_perinatal_oms =  sum(obitos_fetais_mais_28sem, na.rm=T) + sum(obitos_6dias) ,
          # perinatal_oms_menos1000 =  sum(peso_menos_1000_mais_28sem, na.rm = T) + sum(obitos_6dias_menos1000) ,
          # perinatal_oms_1000_1499 =  sum(peso_1000_1499_mais_28sem, na.rm=T) + sum(obitos_6dias_1000_1499) ,
          # perinatal_oms_1500_2499 =  sum(peso_1500_2499_mais_28sem, na.rm=T) + sum(obitos_6dias_1500_2499) ,
          # perinatal_oms_mais2500 =  sum(peso_mais_2500_mais_28sem, na.rm=T) + sum(obitos_6dias_mais2500) ,
          taxa_perinatal_total =  8.7 ,
          taxa_perinatal_total_menos1000 =  round((sum(fetal_peso_menos_1000, na.rm = T) + sum(obitos_6dias_menos1000, na.rm = T))/(sum(fetal_peso_menos_1000, na.rm = T)+ sum(nascidos_menos1000, na.rm = T))*1000, 1) ,
          taxa_perinatal_total_1000_1499 =  round((sum(fetal_peso_1000_1499, na.rm = T) + sum(obitos_6dias_1000_1499, na.rm = T))/(sum(fetal_peso_1000_1499, na.rm = T)+sum(nascidos_1000_1499, na.rm = T))*1000, 1) ,
          taxa_perinatal_total_1500_2499 =  round((sum(fetal_peso_1500_2499, na.rm = T)+sum(obitos_6dias_1500_2499, na.rm = T))/(sum(fetal_peso_1500_2499, na.rm = T)+sum(nascidos_1500_2499, na.rm = T))*1000, 1) ,
          taxa_perinatal_total_mais2500 =  round((sum(fetal_peso_mais_2500, na.rm = T)+sum(obitos_6dias_mais2500, na.rm = T))/(sum(fetal_peso_mais_2500, na.rm = T)+sum(nascidos_mais2500, na.rm = T))*1000, 1) ,
          # taxa_perinatal_oms =  8.7,
          # taxa_perinatal_oms_menos1000 =  round((sum(peso_menos_1000_mais_28sem, na.rm=T) + sum(obitos_6dias_menos1000))/(sum(peso_menos_1000_mais_28sem, na.rm=T)+ sum(nascidos_menos1000))*1000, 1) ,
          # taxa_perinatal_oms_1000_1499 =  round((sum(peso_1000_1499_mais_28sem, na.rm=T) + sum(obitos_6dias_1000_1499))/(sum(peso_1000_1499_mais_28sem, na.rm=T)+sum(nascidos_1000_1499))*1000, 1) ,
          # taxa_perinatal_oms_1500_2499 =  round((sum(peso_1500_2499_mais_28sem, na.rm=T)+sum(obitos_6dias_1500_2499))/(sum(peso_1500_2499_mais_28sem, na.rm=T)+sum(nascidos_1500_2499))*1000, 1) ,
          # taxa_perinatal_oms_mais2500 =  round((sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(obitos_6dias_mais2500))/(sum(peso_mais_2500_mais_28sem, na.rm=T)+sum(nascidos_mais2500))*1000, 1) ,

          obitos_0dias =  sum(obitos_0dias, na.rm = T) ,
          obitos_0dias_menos1000 =  sum(obitos_0dias_menos1000, na.rm = T) ,
          obitos_0dias_1000_1499 =  sum(obitos_0dias_1000_1499, na.rm = T) ,
          obitos_0dias_1500_2499 =  sum(obitos_0dias_1500_2499, na.rm = T) ,
          obitos_0dias_mais2500 =  sum(obitos_0dias_mais2500, na.rm = T) ,
          obitos_1_6dias =  sum(obitos_1_6dias, na.rm = T) ,
          obitos_1_6dias_menos1000 =  sum(obitos_1_6dias_menos1000, na.rm = T) ,
          obitos_1_6dias_1000_1499 =  sum(obitos_1_6dias_1000_1499, na.rm = T) ,
          obitos_1_6dias_1500_2499 =  sum(obitos_1_6dias_1500_2499, na.rm = T) ,
          obitos_1_6dias_mais2500 =  sum(obitos_1_6dias_mais2500, na.rm = T) ,
          obitos_6dias =  sum(obitos_6dias, na.rm = T) ,
          obitos_6dias_menos1000 =  sum(obitos_6dias_menos1000, na.rm = T) ,
          obitos_6dias_1000_1499 =  sum(obitos_6dias_1000_1499, na.rm = T) ,
          obitos_6dias_1500_2499 =  sum(obitos_6dias_1500_2499, na.rm = T) ,
          obitos_6dias_mais2500 =  sum(obitos_6dias_mais2500, na.rm = T) ,
          obitos_27dias =  sum(obitos_27dias, na.rm = T) ,
          obitos_27dias_menos1000 =  sum(obitos_27dias_menos1000, na.rm = T) ,
          obitos_27dias_1000_1499 =  sum(obitos_27dias_1000_1499, na.rm = T) ,
          obitos_27dias_1500_2499 =  sum(obitos_27dias_1500_2499, na.rm = T) ,
          obitos_27dias_mais2500 =  sum(obitos_27dias_mais2500, na.rm = T) ,
          obitos_7_27dias =  sum(obitos_7_27dias, na.rm = T) ,
          obitos_7_27dias_menos1000 =  sum(obitos_7_27dias_menos1000, na.rm = T) ,
          obitos_7_27dias_1000_1499 =  sum(obitos_7_27dias_1000_1499, na.rm = T) ,
          obitos_7_27dias_1500_2499 =  sum(obitos_7_27dias_1500_2499, na.rm = T) ,
          obitos_7_27dias_mais2500 =  sum(obitos_7_27dias_mais2500, na.rm = T) ,

          # antes_dist_moment_obito_fetal =  round(
          #   sum(c(fetal_antes))/
          #     sum(c(obitos_fetais))
          #   * 100, 1) ,
          #
          # durante_dist_moment_obito_fetal =
          #   round(fetal_durante/obitos_fetais * 100, 1) ,
          #
          # faltante_dist_moment_obito_fetal =  round(100-antes_dist_moment_obito_fetal-durante_dist_moment_obito_fetal, 1) ,
          #
          # antes_dist_moment_obito_perinat =  round(perinatal_antes/obitos_perinatal_oms * 100, 1) ,
          #
          # durante_dist_moment_obito_perinat =  round( perinatal_durante/obitos_perinatal_oms * 100, 1) ,
          #
          # dia_0_dist_moment_obito_perinat =  round(obitos_0dias/obitos_perinatal_oms * 100, 1) ,
          #
          # dia_1_6_dist_moment_obito_perinat =  round(obitos_1_6dias/obitos_perinatal_oms * 100, 1) ,
          #
          # faltante_dist_moment_obito_perinat =  round(100 -antes_dist_moment_obito_perinat -durante_dist_moment_obito_perinat -dia_0_dist_moment_obito_perinat -dia_1_6_dist_moment_obito_perinat, 1) ,
          #
          # dia_0_dist_moment_obito_neonat =  round( obitos_0dias/obitos_27dias * 100, 1) ,
          #
          # dia_1_6dist_moment_obito_neonat =  round( obitos_1_6dias/obitos_27dias * 100, 1) ,
          #
          # dia_7_27dist_moment_obito_neonat =  round(obitos_7_27dias/obitos_27dias * 100, 1) ,
          #
          # faltante_moment_obito_neonat =  round(100 -dia_0_dist_moment_obito_neonat -dia_1_6dist_moment_obito_neonat -dia_7_27dist_moment_obito_neonat, 1) ,
          #
          # menos_1000_dist_peso_fetal =  round( fetal_peso_menos_1000/obitos_fetais * 100, 1) ,
          #
          # de_1000_1499_dist_peso_fetal =  round( fetal_peso_1000_1499/obitos_fetais * 100, 1) ,
          #
          # de_1500_2499_dist_peso_fetal =  round( fetal_peso_1500_2499/obitos_fetais * 100, 1) ,
          #
          # mais_2500_dist_peso_fetal =  round( fetal_peso_mais_2500/obitos_fetais * 100, 1) ,
          #
          # faltante_dist_peso_fetal =  round(100 -menos_1000_dist_peso_fetal-de_1000_1499_dist_peso_fetal-de_1500_2499_dist_peso_fetal -mais_2500_dist_peso_fetal, 1) ,
          #
          # menos_1000_dist_peso_perinat =  round( perinatal_oms_menos1000/obitos_perinatal_oms * 100, 1) ,
          #
          # de_1000_1499_dist_peso_perinat =  round( perinatal_oms_1000_1499/obitos_perinatal_oms * 100, 1) ,
          #
          #
          # de_1500_2499_dist_peso_perinat =  round( perinatal_oms_1500_2499/obitos_perinatal_oms * 100, 1) ,
          #
          # mais_2500_dist_peso_perinat =  round(perinatal_oms_mais2500/obitos_perinatal_oms * 100, 1) ,
          #
          # faltante_dist_peso_perinat =  round(100 -menos_1000_dist_peso_perinat -de_1000_1499_dist_peso_perinat -de_1500_2499_dist_peso_perinat -mais_2500_dist_peso_perinat, 1) ,
          #
          #
          # menos_1000_dist_peso_neonat =  round( obitos_27dias_menos1000/obitos_27dias * 100, 1) ,
          #
          # de_1000_1499_dist_peso_neonat =  round( obitos_27dias_1000_1499/obitos_27dias * 100, 1) ,
          #
          # de_1500_2499_dist_peso_neonat =  round( obitos_27dias_1500_2499/obitos_27dias * 100, 1) ,
          #
          # mais_2500_dist_peso_neonat =  round( obitos_27dias_mais2500/obitos_27dias * 100, 1) ,
          #
          # faltante_dist_peso_neonat =  round(100 -menos_1000_dist_peso_neonat -de_1000_1499_dist_peso_neonat -de_1500_2499_dist_peso_neonat -mais_2500_dist_peso_neonat, 1) ,

          antes_dist_moment_obito_fetal =  round(
            sum(c(fetal_antes_peso_menos_1000, fetal_antes_peso_1000_1499, fetal_antes_peso_1500_2499, fetal_antes_peso_mais_2500, fetal_antes)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(fetal_peso_menos_1000, fetal_peso_1000_1499, fetal_peso_1500_2499, fetal_peso_mais_2500, obitos_fetais)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          durante_dist_moment_obito_fetal =  round(
            sum(c(fetal_durante_peso_menos_1000, fetal_durante_peso_1000_1499, fetal_durante_peso_1500_2499, fetal_durante_peso_mais_2500, fetal_durante)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(fetal_peso_menos_1000, fetal_peso_1000_1499, fetal_peso_1500_2499, fetal_peso_mais_2500, obitos_fetais)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_moment_obito_fetal =  round(100-antes_dist_moment_obito_fetal-durante_dist_moment_obito_fetal, 1) ,

          antes_dist_moment_obito_perinat =  round(
            sum(c(perinatal_todos_antes_menos_1000, perinatal_todos_antes_1000_1499, perinatal_todos_antes_1500_2499, perinatal_todos_antes_mais_2500, perinatal_todos_antes)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm = T)/
              sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm = T)
            *100, 1) ,

          durante_dist_moment_obito_perinat =  round(
            sum(c(perinatal_todos_durante_menos_1000, perinatal_todos_durante_1000_1499, perinatal_todos_durante_1500_2499, perinatal_todos_durante_mais_2500, perinatal_todos_durante)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm = T)/
              sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm = T)
            *100, 1) ,

          dia_0_dist_moment_obito_perinat =  round(
            sum(c(obitos_0dias_menos1000, obitos_0dias_1000_1499, obitos_0dias_1500_2499, obitos_0dias_mais2500, obitos_0dias)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm = T)/
              sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm = T)
            *100, 1) ,

          dia_1_6_dist_moment_obito_perinat =  round(
            sum(c(obitos_1_6dias_menos1000, obitos_0dias_1000_1499, obitos_1_6dias_1500_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[seleciona(aba = 'perinatal', indicador = 'momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm = T)/
              sum(c(perinatal_todos_peso_menos_1000, perinatal_todos_peso_1000_1499, perinatal_todos_peso_1500_2499, perinatal_todos_peso_mais_2500, perinatal_todos_total)[seleciona(aba = 'perinatal', indicador ='momento de obito por peso', input$faixa_peso_dist_moment_obit_perinat) %in% input$faixa_peso_dist_moment_obit_perinat], na.rm = T)
            *100, 1) ,

          faltante_dist_moment_obito_perinat =  round(100 -antes_dist_moment_obito_perinat -durante_dist_moment_obito_perinat -dia_0_dist_moment_obito_perinat -dia_1_6_dist_moment_obito_perinat, 1) ,

          dia_0_dist_moment_obito_neonat =  round(
            sum(c(obitos_0dias_menos1000, obitos_0dias_1000_1499, obitos_0dias_1500_2499, obitos_0dias_mais2500, obitos_0dias)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          dia_1_6dist_moment_obito_neonat =  round(
            sum(c(obitos_1_6dias_menos1000, obitos_1_6dias_1000_1499, obitos_1_6dias_1500_2499, obitos_1_6dias_mais2500, obitos_1_6dias)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          dia_7_27dist_moment_obito_neonat =  round(
            sum(c(obitos_7_27dias_menos1000, obitos_7_27dias_1000_1499, obitos_7_27dias_1500_2499, obitos_7_27dias_mais2500, obitos_7_27dias)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(obitos_27dias_menos1000, obitos_27dias_1000_1499, obitos_27dias_1500_2499, obitos_27dias_mais2500, obitos_27dias)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          faltante_moment_obito_neonat =  round(100 -dia_0_dist_moment_obito_neonat -dia_1_6dist_moment_obito_neonat -dia_7_27dist_moment_obito_neonat, 1) ,

          menos_1000_dist_peso_fetal =  round(
            sum(c(fetal_antes_peso_menos_1000, fetal_durante_peso_menos_1000, fetal_peso_menos_1000)[c(rep(F, 2), T)], na.rm = T)/
              sum(c(fetal_antes, fetal_durante, obitos_fetais)[c(rep(F, 2), T)], na.rm = T)
            * 100, 1) ,

          de_1000_1499_dist_peso_fetal =  round(
            sum(c(fetal_antes_peso_1000_1499, fetal_durante_peso_1000_1499, fetal_peso_1000_1499)[c(rep(F, 2), T)], na.rm = T)/
              sum(c(fetal_antes, fetal_durante, obitos_fetais)[c(rep(F, 2), T)], na.rm = T)
            * 100, 1) ,

          de_1500_2499_dist_peso_fetal =  round(
            sum(c(fetal_antes_peso_1500_2499, fetal_durante_peso_1500_2499, fetal_peso_1500_2499)[c(rep(F, 2), T)], na.rm = T)/
              sum(c(fetal_antes, fetal_durante, obitos_fetais)[c(rep(F, 2), T)], na.rm = T)
            * 100, 1) ,

          mais_2500_dist_peso_fetal =  round(
            sum(c(fetal_antes_peso_mais_2500, fetal_durante_peso_mais_2500, fetal_peso_mais_2500)[c(rep(F, 2), T)], na.rm = T)/
              sum(c(fetal_antes, fetal_durante, obitos_fetais)[c(rep(F, 2), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_peso_fetal =  round(100 -menos_1000_dist_peso_fetal-de_1000_1499_dist_peso_fetal-de_1500_2499_dist_peso_fetal -mais_2500_dist_peso_fetal, 1) ,

          menos_1000_dist_peso_perinat =  round(
            sum(c(perinatal_todos_antes_menos_1000, perinatal_todos_durante_menos_1000, obitos_0dias_menos1000, obitos_1_6dias_menos1000, perinatal_todos_peso_menos_1000)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          de_1000_1499_dist_peso_perinat =  round(
            sum(c(perinatal_todos_antes_1000_1499, perinatal_todos_durante_1000_1499, obitos_0dias_1000_1499, obitos_1_6dias_1000_1499, perinatal_todos_peso_1000_1499)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,


          de_1500_2499_dist_peso_perinat =  round(
            sum(c(perinatal_todos_antes_1500_2499, perinatal_todos_durante_1500_2499, obitos_0dias_1500_2499, obitos_1_6dias_1500_2499, perinatal_todos_peso_1500_2499)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          mais_2500_dist_peso_perinat =  round(
            sum(c(perinatal_todos_antes_mais_2500 , perinatal_todos_durante_mais_2500, obitos_0dias_mais2500, obitos_1_6dias_mais2500, perinatal_todos_peso_mais_2500)[c(rep(F, 4), T)], na.rm = T)/
              sum(c(perinatal_todos_antes, perinatal_todos_durante, obitos_0dias, obitos_1_6dias, perinatal_todos_total)[c(rep(F, 4), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_peso_perinat =  round(100 -menos_1000_dist_peso_perinat -de_1000_1499_dist_peso_perinat -de_1500_2499_dist_peso_perinat -mais_2500_dist_peso_perinat, 1) ,


          menos_1000_dist_peso_neonat =  round(
            sum(c(obitos_0dias_menos1000, obitos_1_6dias_menos1000, obitos_7_27dias_menos1000, obitos_27dias_menos1000)[c(rep(F, 3), T)], na.rm = T)/
              sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[c(rep(F, 3), T)], na.rm = T)
            * 100, 1) ,

          de_1000_1499_dist_peso_neonat =  round(
            sum(c(obitos_0dias_1000_1499, obitos_1_6dias_1000_1499, obitos_7_27dias_1000_1499, obitos_27dias_1000_1499)[c(rep(F, 3), T)], na.rm = T)/
              sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[c(rep(F, 3), T)], na.rm = T)
            * 100, 1) ,

          de_1500_2499_dist_peso_neonat =  round(
            sum(c(obitos_0dias_1500_2499, obitos_1_6dias_1500_2499, obitos_7_27dias_1500_2499, obitos_27dias_1500_2499)[c(rep(F, 3), T)], na.rm = T)/
              sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[c(rep(F, 3), T)], na.rm = T)
            * 100, 1) ,

          mais_2500_dist_peso_neonat =  round(
            sum(c(obitos_0dias_mais2500, obitos_1_6dias_mais2500, obitos_7_27dias_mais2500, obitos_27dias_mais2500)[c(rep(F, 3), T)], na.rm = T)/
              sum(c(obitos_0dias, obitos_1_6dias, obitos_7_27dias, obitos_27dias)[c(rep(F, 3), T)], na.rm = T)
            * 100, 1) ,

          faltante_dist_peso_neonat =  round(100 -menos_1000_dist_peso_neonat -de_1000_1499_dist_peso_neonat -de_1500_2499_dist_peso_neonat -mais_2500_dist_peso_neonat, 1),


          porc_condicoes_ameacadoras =  round(sum(nascidos_condicoes_ameacadoras, na.rm = T) / sum(nascidos, na.rm = T) * 100, 1) ,
          #porc_internacoes_menores_28_dias_sih_geral =  round(sum(internacoes_geral_geral[ano <= 2022]) / sum(nascidos_estabelecimentos_publicos_sih[ano <= 2022]) * 100, 1) ,
          porc_internacoes_menores_28_dias_sih_geral =  round(sum(internacoes_geral_geral, na.rm = T) / sum(nascidos_estabelecimentos_publicos_sih, na.rm = T) * 100, 1) ,
          porc_internacoes_uti_menores_28_dias_sih_geral =  round(sum(internacoes_geral_geral_internado_uti, na.rm = T) / sum(nascidos_estabelecimentos_publicos_sih, na.rm = T) * 100, 1)
        ) |>
        dplyr::ungroup()|>
        dplyr::distinct(ano, .keep_all = T)
    })



    ### Caixas para obitos potencialmente evitaveis ############################ [zzz]

    ### Dados Fetal, neonatal e perinatal

    data_filtrada_evitaveis_aux <- reactive({
      bloco7_distribuicao_cids |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano)
    })

    ##### Dados caixas
    bloco7_evitaveis_resumo <- reactive({
      data_filtrada_evitaveis_aux() |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & dplyr::contains("2")) | (dplyr::contains("evitaveis_neonatal")) |
                                          (dplyr::contains("evitaveis_perinatal")) | "obitos_fetais_totais" | "obitos_neonatais_totais" |
                                          "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante")))), na.rm = T),
          obitos_neonatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), na.rm = T),
          obitos_perinatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias")))), na.rm = T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante")))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias")))), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        dplyr::select(dplyr::contains(c("outros", "mal_definidas"))) |>
        dplyr::mutate(
          porc_evitavel_fetal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante")))), na.rm = T),
          porc_evitavel_neonatal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), na.rm = T),
          porc_evitavel_perinatal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias")))), na.rm = T)
        )
    })


    ### Dados para a referencia nacional
    bloco7_evitaveis_resumo_comp <- reactive({
      bloco7_distribuicao_cids |>
        dplyr::filter(ano == filtros()$ano) |>
        dplyr::summarise_at(dplyr::vars((dplyr::contains("evitaveis_fetal") & dplyr::contains("2")) | (dplyr::contains("evitaveis_neonatal")) |
                                          (dplyr::contains("evitaveis_perinatal")) | "obitos_fetais_totais" | "obitos_neonatais_totais" |
                                          "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          obitos_fetais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante")))), na.rm = T),
          obitos_neonatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), na.rm = T),
          obitos_perinatais_evitaveis_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias")))), na.rm = T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante")))), ~ (. / obitos_fetais_evitaveis_total * 100)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), ~ (. / obitos_neonatais_evitaveis_total * 100)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias")))), ~ (. / obitos_perinatais_evitaveis_total * 100)) |>
        dplyr::select(dplyr::contains(c("outros", "mal_definidas"))) |>
        dplyr::mutate(
          porc_evitavel_fetal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "evitaveis2", input = c("evitaveis_fetal_antes", "evitaveis_fetal_durante")))), na.rm = T),
          porc_evitavel_neonatal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="neonatal", grafico = "evitaveis", input = c("evitaveis_neonatal_0_dias","evitaveis_neonatal_1_6_dias","evitaveis_neonatal_7_27_dias")))), na.rm = T),
          porc_evitavel_perinatal = 100 - sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "evitaveis", input = c("evitaveis_perinatal_antes", "evitaveis_perinatal_durante","evitaveis_perinatal_0_dias", "evitaveis_perinatal_1_6_dias")))), na.rm = T)
        )
    })


    ### Caixas paras as 3 maiores causas de obitos ############################

    ### Aba fetal

    bloco7_principais_obito_fetal <- reactive({
      data_filtrada_evitaveis_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("fetal_grupos") | "obitos_fetais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_fetais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = c("fetal_grupos_antes","fetal_grupos_durante")))), na.rm = T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = c("fetal_grupos_antes","fetal_grupos_durante")))), ~ (. / obitos_fetais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="fetal", grafico = "grupos", input = c("fetal_grupos_antes","fetal_grupos_durante"))),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, porc_obitos) |>
        dplyr::mutate(
          grupo = dplyr::case_when(
            grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
            grepl("infeccoes", grupo_cid10) ~ "Infecções",
            grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
            grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
            grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
            grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
            grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
            grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
            grepl("outros", grupo_cid10) ~ "Demais causas"
          ),
          porc_obitos = round(porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })


    ### Aba perinatal

    bloco7_principais_obito_perinatal <- reactive({
      data_filtrada_evitaveis_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("perinatal_grupos") | "obitos_perinatais_totais"), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_perinatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = c("perinatal_grupos_antes", "perinatal_grupos_durante","perinatal_grupos_0_dias", "perinatal_grupos_1_6_dias")))), na.rm = T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input = c("perinatal_grupos_antes", "perinatal_grupos_durante","perinatal_grupos_0_dias", "perinatal_grupos_1_6_dias")))), ~ (. / obitos_perinatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_obitos(aba="perinatal", grafico = "grupos", input =  c("perinatal_grupos_antes", "perinatal_grupos_durante","perinatal_grupos_0_dias", "perinatal_grupos_1_6_dias"))),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, porc_obitos) |>
        dplyr::mutate(
          grupo = dplyr::case_when(
            grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
            grepl("infeccoes", grupo_cid10) ~ "Infecções",
            grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
            grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
            grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
            grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
            grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
            grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
            grepl("outros", grupo_cid10) ~ "Demais causas"
          ),
          porc_obitos = round(porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })

    ### Aba neonatal

    bloco7_principais_obito_neonatal <- reactive({
      data_filtrada_evitaveis_aux() |>
        dplyr::select(
          dplyr::contains("neonatal_grupos") &
            !dplyr::matches("0_dias|1_6_dias|7_27_dias")
        ) |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(obitos_neonatais_grupos_total = sum(dplyr::across(dplyr::contains("neonatal_grupos")), na.rm = T)) |>
        dplyr::mutate_at(dplyr::vars(dplyr::contains("neonatal_grupos")), ~ (. / obitos_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::contains("neonatal_grupos"),
          names_to = "grupo_cid10",
          values_to = "porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, porc_obitos) |>
        dplyr::mutate(
          grupo = dplyr::case_when(
            grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
            grepl("infeccoes", grupo_cid10) ~ "Infecções",
            grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
            grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
            grepl("respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
            grepl("gravidez", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
            grepl("afeccoes", grupo_cid10) ~ "Afecções originais no período perinatal",
            grepl("mal_definida", grupo_cid10) ~ "Mal definidas",
            grepl("outros", grupo_cid10) ~ "Demais causas"
          ),
          porc_obitos = round(porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })

    # Aba morbidade neonatal

    ### Aba morbidade neonatal

    data_filtrada_morbidade_aux <- reactive({
      bloco7_dist_morbidade |>
        dplyr::filter(ano == filtros()$ano) |>
        #if(filtros()$nivel == "estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "macro")
            macro_r_saude == filtros()$macro & uf == filtros()$estado_macro
          else if(filtros()$nivel == "micro")
            r_saude == filtros()$micro & uf == filtros()$estado_micro
          else if(filtros()$nivel == "municipal")
            municipio == filtros()$municipio & uf == filtros()$estado_municipio
        ) |>
        dplyr::group_by(ano)
    })

    bloco7_principais_internacoes_neonatal <- reactive({
      data_filtrada_morbidade_aux() |>
        dplyr::summarise_at(dplyr::vars(dplyr::contains("morbidade_neonatal_grupos")), sum) |>
        dplyr::rowwise() |>
        dplyr::mutate(internacoes_neonatais_grupos_total = sum(dplyr::c_across(dplyr::matches(momento_internacoes(input = c(
          "morbidade_neonatal_grupos_0_dias",
          "morbidade_neonatal_grupos_1_6_dias",
          "morbidade_neonatal_grupos_7_27_dias"
        )))))) |>
        dplyr::mutate_at(dplyr::vars(dplyr::matches(momento_internacoes(input = c(
          "morbidade_neonatal_grupos_0_dias",
          "morbidade_neonatal_grupos_1_6_dias",
          "morbidade_neonatal_grupos_7_27_dias"
        )))), ~ (. / internacoes_neonatais_grupos_total * 100)) |>
        tidyr::pivot_longer(
          cols = dplyr::matches(momento_internacoes(input = c(
            "morbidade_neonatal_grupos_0_dias",
            "morbidade_neonatal_grupos_1_6_dias",
            "morbidade_neonatal_grupos_7_27_dias"
          ))),
          names_to = "grupo_cid10",
          values_to = "br_porc_obitos"
        ) |>
        dplyr::select(grupo_cid10, br_porc_obitos) |>
        dplyr::mutate(
          grupo = dplyr::case_when(
            grepl("prematuridade", grupo_cid10) ~ "Prematuridade",
            grepl("infeccoes", grupo_cid10) ~ "Infecções",
            grepl("asfixia", grupo_cid10) ~ "Asfixia/Hipóxia",
            grepl("ma_formacao", grupo_cid10) ~ "Anomalia congênita",
            grepl("afeccoes_respiratorias", grupo_cid10) ~ "Afecções respiratórias do recém-nascido",
            grepl("fatores_maternos", grupo_cid10) ~ "Fatores maternos relacionados à gravidez",
            grepl("afeccoes_perinatal", grupo_cid10) ~ "Afecções originais no período perinatal",
            grepl("mal_definidas", grupo_cid10) ~ "Mal definidas",
            grepl("ictericia", grupo_cid10) ~ "Icterícia neonatal",
            grepl("endocrinos", grupo_cid10) ~ "Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido",
            grepl("alimentacao", grupo_cid10) ~ "Problemas de alimentação do recém-nascido",
            grepl("cardiacos_perinatal", grupo_cid10) ~ "Transtornos cardíacos originados no período perinatal",
            grepl("outros", grupo_cid10) ~ "Demais causas"
          ),
          porc_obitos = round(br_porc_obitos, 1)) |>
        dplyr::filter(!grepl("outros|mal_definidas", grupo_cid10)) |>
        dplyr::select(grupo, porc_obitos) |>
        dplyr::arrange(desc(porc_obitos)) |>
        dplyr::slice(1:3)
    })


    # Criando as caixinhas


    output$caixa_b7_fetal_i6 <- renderUI({
      cria_caixa_server(
        dados = bloco7_evitaveis_resumo(),
        indicador = "porc_evitavel_fetal",
        titulo = "Porcentagem de óbitos fetais potencialmente evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = bloco7_evitaveis_resumo_comp()$porc_evitavel_fetal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    ### Aba perinatal

    output$caixa_b7_perinatal_i6 <- renderUI({
      cria_caixa_server(
        dados = bloco7_evitaveis_resumo(),
        indicador = "porc_evitavel_perinatal",
        titulo = "Porcentagem de óbitos perinatais potencialmente evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = bloco7_evitaveis_resumo_comp()$porc_evitavel_perinatal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    ### Aba neonatal

    output$caixa_b7_neonatal_i8 <- renderUI({
      cria_caixa_server(
        dados = bloco7_evitaveis_resumo(),
        indicador = "porc_evitavel_neonatal",
        titulo = "Porcentagem de óbitos neonatais potencialmente evitáveis",
        tem_meta = FALSE,
        valor_de_referencia = bloco7_evitaveis_resumo_comp()$porc_evitavel_neonatal,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    ### Caixas principais causas de obito ######################################

    ### Aba fetal

    output$caixa_b7_fetal_i5 <- renderUI({
      cria_caixa_principais_evitaveis_bloco7(
        dados = bloco7_principais_obito_fetal(),
        titulo = "Dentre os óbitos fetais,"
      )
    })

    ### Aba perinatal

    output$caixa_b7_perinatal_i5 <- renderUI({
      cria_caixa_principais_evitaveis_bloco7(
        dados = bloco7_principais_obito_perinatal(),
        titulo = "Dentre os óbitos perinatais,"
      )
    })

    ### Aba neonatal

    output$caixa_b7_neonatal_i7 <- renderUI({
      cria_caixa_principais_evitaveis_bloco7(
        dados = bloco7_principais_obito_neonatal(),
        titulo = "Dentre os óbitos neonatais,"
      )
    })

    output$caixa_b7_fetal_i1 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "obitos_fetais",
        titulo = "Número de óbitos fetais",
        tem_meta = FALSE,
        valor_de_referencia = data7_comp()$obitos_fetais,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b7_fetal_i7 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "obitos_fetais_oms",
        titulo = "Número de óbitos fetais",
        tem_meta = FALSE,
        valor_de_referencia = data7_comp()$obitos_fetais_oms,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = dplyr::if_else(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })


    output$caixa_b7_fetal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "taxa_mort_fetal",
        titulo = "Taxa de mortalidade fetal",
        tem_meta = T,
        tipo_referencia = "meta ODS",
        valor_de_referencia = #ifelse(data7_resumo_referencia()[[taxa_mort_fetal]] >0 ,
          data7_comp()$taxa_mort_fetal,#, NaN),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",

        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b7_fetal_i8 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "taxa_mort_fetal_oms",
        titulo = "Taxa de mortalidade fetal",
        tem_meta = T,
        tipo_referencia = "meta ODS",
        valor_de_referencia = #dplyr::if_else(data7_resumo_referencia()[[taxa_mortalidade_fetal_oms()]] >0 ,
          data7_comp()$taxa_mort_fetal_oms,
        #, NaN),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$caixa_b7_fetal_i3 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7(),
        indicador = "fetal peso por idade gestacional",
        titulo = "Dentre os óbitos fetais,",
        tamanho_caixa = "330px"
      )
    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$caixa_b7_fetal_i4 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7(),
        indicador = "fetal momento do obito por peso",
        titulo = "Dentre os óbitos fetais,",
        tamanho_caixa = "330px"
      )
    })



    output$caixa_b7_neonatal_i1 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "mort_neonat",
        titulo = "Taxa de mortalidade neonatal por 1000 nascidos vivos",
        tem_meta = T,
        tipo_referencia = "meta ODS",
        valor_de_referencia = data7_comp()$mort_neonat,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    #### Taxa de mortalidade neonatal precoce por 1000 nascidos vivos ---------

    output$caixa_b7_neonatal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "mort_neonat_precoc",
        titulo = "Taxa de mortalidade neonatal precoce (0 a 6 dias) por 1000 nascidos vivos",
        tem_meta = T,
        tipo_referencia = "meta ODS",
        valor_de_referencia = data7_comp()$mort_neonat_precoc,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",

        nivel_de_analise = filtros()$nivel
      )
    })



    output$caixa_b7_neonatal_i3 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "mort_neonat_tardia",
        titulo = "Taxa de mortalidade neonatal tardia (7 a 27 dias) por 1000 nascidos vivos",
        tem_meta = T,
        tipo_referencia = "meta ODS",
        valor_de_referencia = data7_comp()$mort_neonat_tardia,
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })



    output$caixa_b7_neonatal_i4 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "obitos_neonat",
        titulo = "Número de óbitos neonatais",
        tem_meta = FALSE,
        valor_de_referencia = data7_comp()$obitos_neonat,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = ifelse(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })

    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$caixa_b7_neonatal_i5 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7(),
        indicador = "neonatal momento do obito por peso",
        titulo = "Dentre os óbitos neonatais,",
        tamanho_caixa = "330px"
      )
    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$caixa_b7_neonatal_i6 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7(),
        indicador = "neonatal peso por momento do obito",
        titulo = "Dentre os óbitos neonatais,",
        tamanho_caixa = "330px"
      )
    })


    ### Para os indicadores de mortalidade perinatal --------------------------

    #### Número de óbitos perinatais (definição 1) ----------------------------

    output$caixa_b7_perinatal_i7 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "perinatal_todos_total",
        titulo = "Número de óbitos perinatais",
        tem_meta = FALSE,
        valor_de_referencia = data7_comp()$perinatal_todos_total,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = ifelse(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })


    output$caixa_b7_perinatal_i8 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "taxa_perinatal_total",
        titulo = "Taxa de mortalidade perinatal por 1000 nascidos vivos",
        tem_meta = T,
        tipo_referencia = "meta ODS",
        valor_de_referencia = #ifelse(data7_comp()[[taxa_perinatal_oms]] >0 ,
          data7_comp()$taxa_perinatal_total, #, NaN),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    #### Número de óbitos perinatais (definição 2) ----------------------------

    output$caixa_b7_perinatal_i1 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "obitos_perinatal_oms",
        titulo = "Número de óbitos perinatais",
        tem_meta = FALSE,
        valor_de_referencia = data7_comp()$obitos_perinatal_oms,
        tipo = "número",
        invertido = FALSE,
        cor = "lightgrey",
        texto_footer = ifelse(
          filtros()$nivel == "nacional",
          "Comparação não aplicável (este é o valor de referência)",
          "{formatC(round(100*dados[[indicador]]/valor_de_referencia, 1), big.mark = '.', decimal.mark = ',')}% do total nacional, de {formatC(as.integer(valor_de_referencia), big.mark = '.', decimal.mark = ',')} óbitos"
        ),
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })


    output$caixa_b7_perinatal_i2 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "taxa_perinatal_oms",
        titulo = "Taxa de mortalidade perinatal",
        tem_meta = T,
        tipo_referencia = "meta ODS",
        valor_de_referencia = #ifelse(data7_comp()[[taxa_perinatal_oms]] >0 ,
          data7_comp()$taxa_perinatal_oms, #, NaN),
        tipo = "taxa",
        invertido = FALSE,
        tamanho_caixa = "330px",
        pagina = "nivel_1",
        nivel_de_analise = filtros()$nivel
      )
    })

    #### Distribuição percentual do momento do óbito por faixa de peso --------
    output$caixa_b7_perinatal_i3 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7(),
        indicador = "perinatal momento do obito por peso",
        titulo = "Dentre os óbitos perinatais,",
        tamanho_caixa = "330px",
      )
    })

    #### Distribuição percentual das faixas de peso por momento do óbito ------
    output$caixa_b7_perinatal_i4 <- renderUI({
      cria_caixa_conjunta_bloco7(
        dados = data7(),
        indicador = "perinatal peso por momento do obito",
        titulo = "Dentre os óbitos perinatais,",
        tamanho_caixa = "330px",
      )
    })

    ############ Para os de morbidade neonatal

    ### Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida ----
    output$caixa_b7_morbidade_i1 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "porc_condicoes_ameacadoras",
        titulo = "Porcentagem de nascidos vivos com condições potencialmente ameaçadoras à vida",
        tem_meta = TRUE,
        valor_de_referencia = data7_comp()$porc_condicoes_ameacadoras,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        #fonte_titulo = "15px",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })

    ### Porcentagem de internações em UTI neonatal até o 27º dia de bebês nascidos em hospitais com vínculo com o SUS  -----------
    output$caixa_b7_morbidade_i3 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "porc_internacoes_uti_menores_28_dias_sih_geral",
        titulo = "Porcentagem de internações neonatais (até o 27º dia de vida) em UTI no SUS",
        tem_meta = FALSE,
        valor_de_referencia = data7_comp()$porc_internacoes_uti_menores_28_dias_sih_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        #fonte_titulo = "15px",
        cor = "lightgrey",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })


    ### Porcentagem de internações em bebês com até 27 dias de vida nascidos em estabelecimentos com vínculo com o SUS ----
    output$caixa_b7_morbidade_i2 <- renderUI({
      cria_caixa_server(
        dados = data7(),
        indicador = "porc_internacoes_menores_28_dias_sih_geral",
        titulo = "Porcentagem de internações neonatais (até o 27º dia de vida) ocorridas no SUS",
        tem_meta = FALSE,
        valor_de_referencia = data7_comp()$porc_internacoes_menores_28_dias_sih_geral,
        tipo = "porcentagem",
        invertido = FALSE,
        tamanho_caixa = "320px",
        #fonte_titulo = "15px",
        cor = "lightgrey",
        pagina = "nivel_1",
        tipo_referencia = "média nacional",
        nivel_de_analise = filtros()$nivel
      )
    })

    output$caixa_b7_principais_morbidade_neonatal <- renderUI({
      cria_caixa_principais_evitaveis_bloco7(
        dados = bloco7_principais_internacoes_neonatal(),
        titulo = "Dentre as internações neonatais,"
      )
    })



  })
}

## To be copied in the UI
# mod_nivel_1_ui("nivel_1_1")

## To be copied in the server
# mod_nivel_1_server("nivel_1_1")
