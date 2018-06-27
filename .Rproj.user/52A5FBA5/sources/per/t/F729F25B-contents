ui <- fluidPage(
  
  titlePanel("Avaliação de Filmes"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      conditionalPanel(condition="input.tabselected==1",
      radioButtons("grafico", "Selecione uma representação:",
                   c("Histograma",
                     "Grafico de Pontos",
                     "Grafico de Linhas",
                     "Box-Plot"),
      br()),
      
      sliderInput("n",
                  "Número de Avaliações:",
                  value = 500,
                  min = 1,
                  max = 1000)
      ),
      
      conditionalPanel(condition="input.tabselected==2",
        textInput("nome1", "Digite o seu nome",""),
        textInput("nome2", "Digite o seu sobrenome",""),
        textInput("email", "Digite o seu email",""),
        selectInput("idade", "Selecione a sua idade",seq(7,80,1)),
        textInput("cidade", "Digite a sua cidade",""),
        radioButtons("categoria", "Selecione a sua categoria",list("Critico Amador","Critico Profissional"),""),
        actionButton("submitCadastro","Cadastrar")
        ),
      conditionalPanel(condition="input.tabselected==3",
                       textInput("avaliador_id", "Digite o seu ID de Crítico",""),
                       radioButtons("avaliadorCategoria", "Selecione a sua categoria",list("Critico Amador","Critico Profissional"),""),
                       textInput("filme_id", "Digite o ID do filme (pode ser encontrado na próxima aba)", ""),
                       sliderInput("nota", "Selecione a sua nota", min=0,max=100,value=50),
                       actionButton("submitAvaliacao","Avaliar")
      ),
      conditionalPanel(condition="input.tabselected==7",
                       textInput("deletadoID", "Digite o ID do crítico que deseja excluir",""),
                       radioButtons("deletadoCategoria", "Selecione a categoria do crítico que deseja excluir",list("Critico Amador","Critico Profissional"),""),
                       actionButton("submitDelete","Excluir")
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Consultas", value=1,
                           conditionalPanel(condition="input.grafico==\"Histograma\"",
                                            br(),
                                            plotOutput("histograma")),
                           conditionalPanel(condition="input.grafico==\"Grafico de Pontos\"",
                                            br(),
                                            plotOutput("pontos")),
                           conditionalPanel(condition="input.grafico==\"Grafico de Linhas\"",
                                            br(),
                                            plotOutput("linhas")),
                           conditionalPanel(condition="input.grafico==\"Box-Plot\"",
                                            br(),
                                            plotOutput("boxes")),
                           br(),br(),br(),br(),
                           h3("Trabalho de Banco de Dados II"),
                           h4("Leonardo Santos"),
                           h4("Marcus Vinícius"),
                           h4("Alexandre Oliveira")
                           ),
                  tabPanel("Cadastros", value=2,
                           h3("Por favor confirme seus dados abaixo"),
                           h1(" "),
                           h4(textOutput("outnome1")),
                           h1(" "),
                           h4(textOutput("outnome2")),
                           h1(" "),
                           h4(textOutput("outemail")),
                           h1(" "),
                           h4(textOutput("outidade")),
                           h1(" "),
                           h4(textOutput("outcidade")),
                           h1(" "),
                           h4(textOutput("outcategoria"))
                           ),
                  tabPanel("Avaliações",value=3,
                           h3("Por favor confirme seus dados abaixo"),
                           h1(" "),
                           h4(textOutput("outavaliador_id")),
                           h4(textOutput("outfilmes")),
                           h4(textOutput("outnota"))
                           ),
                  tabPanel("Filmes Disponíveis", value=4, DT::dataTableOutput(outputId = 'filmesdf')),
                  tabPanel("Críticos Cadastrados", value=5, DT::dataTableOutput(outputId = 'criticosdf')),
                  tabPanel("Publico Cadastrado", value=6, DT::dataTableOutput(outputId = 'espectadoresdf')),
                  tabPanel("Deletar Críticos", value=7, 
                           h3("Por favor confirme seus dados abaixo"),
                           h1(" "),
                           h4(textOutput("outDeletadoID")),
                           h4(textOutput("outDeletadoCategoria"))
                           ),
                  id = "tabselected"
                )
      )
      
    )
)
