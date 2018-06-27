server <- function(input, output, session) {
  
  con <- dbConnect(RMySQL::MySQL(),
                 dbname = "filmrating",
                  host = "127.0.0.1",
                  port = 3306,
                  user = "root",
                  password = "")
  
   
  queryFilmes <- "SELECT * FROM filme;"
  movies <- dbGetQuery(con, queryFilmes)
  queryCriticos <- "SELECT * FROM critico;"
  critics <- dbGetQuery(con, queryCriticos)
  queryPublico <- "SELECT * FROM espectador;"
  public <- dbGetQuery(con, queryPublico)
  
  output$histograma <- renderPlot({
    ggplot(data=movies, aes(x=orcamento),color=genero) + geom_histogram(binwidth = 10, aes(fill=genero), color="Black") +
      xlab("Orçamento (em milhões de dólares)") +
      ylab("Número de Filmes") +
      ggtitle("Distribuição dos Filmes por Orçamento") +
      guides(fill=guide_legend(title="Gênero")) +
      theme(axis.title.x = element_text(color="DarkGreen",size=20),
            axis.title.y = element_text(color="DarkGreen",size=20),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            legend.title = element_text(size=15),
            legend.text = element_text(size=10),
            plot.title = element_text(color="DarkBlue",size=20,hjust = 0.5))
  })
  
  queryViewPublicoOrcamentoGenero <- "SELECT * FROM view_publico_orcamento_genero;"
  publicoOrcamentoGenero <- dbGetQuery(con, queryViewPublicoOrcamentoGenero)
  output$pontos <- renderPlot({
    ggplot(data=publicoOrcamentoGenero, aes(x=orcamento,y=media_publico,color=genero,size=orcamento)) + geom_point() +
      xlab("Orçamento (em milhões de dólares)") +
      ylab("Avaliações do Público") +
      ggtitle("Relação Entre Satisfação do Público e Orçamento dos Filmes") +
      guides(color=guide_legend(title="Gênero"), size=guide_legend(title="Orçamento")) +
      theme(axis.title.x = element_text(color="DarkGreen",size=20),
            axis.title.y = element_text(color="DarkGreen",size=20),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            legend.title = element_text(size=15),
            legend.text = element_text(size=10),
            plot.title = element_text(color="DarkBlue",size=20,hjust = 0.5))
  })
  
  queryMedias <- "SELECT * FROM view_medias;"
  medias <- dbGetQuery(con, queryMedias)
  output$linhas <- renderPlot({
    ggplot(data=medias, aes(x=media_critica,y=media_publico,color=genero)) + geom_smooth(fill=NA) +
      xlab("Avaliações da Crítica") +
      ylab("Avaliações do Público") +
      ggtitle("Relação Entre as Avaliações da Crítica e a Satisfação do Público") +
      guides(color=guide_legend(title="Gênero")) +
      theme(axis.title.x = element_text(color="DarkGreen",size=20),
            axis.title.y = element_text(color="DarkGreen",size=20),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            legend.title = element_text(size=15),
            legend.text = element_text(size=10),
            plot.title = element_text(color="DarkBlue",size=20,hjust = 0.5))
  })
  
  output$boxes <- renderPlot({
    ggplot(data=medias, aes(x=genero,y=media_publico,color=genero)) + geom_jitter() + geom_boxplot(size=1.0,alpha=0.5) +
      xlab("Gênero dos Filmes") +
      ylab("Avaliações do Público") +
      ggtitle("Satisfação do Público em Relação ao Gênero do Filme") +
      guides(color=guide_legend(title="Gênero")) +
      theme(axis.title.x = element_text(color="DarkGreen",size=20),
            axis.title.y = element_text(color="DarkGreen",size=20),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            legend.title = element_text(size=15),
            legend.text = element_text(size=10),
            plot.title = element_text(color="DarkBlue",size=20,hjust = 0.5))
  })
  
  output$outnome1 <- renderText(paste("\nNome Principal: ",input$nome1))
  output$outnome2 <- renderText(paste("\nSobrenome: ",input$nome2))
  output$outemail <- renderText(paste("\nEmail: ",input$email))
  output$outidade <- renderText(paste("\nIdade: ",input$idade))
  output$outcidade <- renderText(paste("\nCidade: ",input$cidade))
  output$outcategoria <- renderText(paste("\nCategoria: ",input$categoria))
  
  output$outDeletadoID <- renderText(paste("\nID do excluído: ",input$deletadoID))
  output$outDeletadoCategoria <- renderText(paste("\nCategoria do excluído: ",input$deletadoCategoria))
  
  # observeEvent(input$tabs, {
  #   if(input$tabs == 4 || input$tabs == 5 || input$tabs == 6) {
  #     removeCssClass("Main", "col-sm-8")
  #     addCssClass("Main", "col-sm-12")
  #     shinyjs::hide(id = "Sidebar")
  #   }
  #   else {
  #     removeCssClass("Main", "col-sm-12")
  #     addCssClass("Main", "col-sm-8")
  #     shinyjs::show(id = "Sidebar")
  #     shinyjs::enable(id = "Sidebar")
  #   }
  # })
  
  cadastrarUsuario <- observeEvent(input$submitCadastro,{
     con2 <- dbConnect(RMySQL::MySQL(),
                       dbname = "filmrating",
                       host = "127.0.0.1",
                       port = 3306,
                       user = "root",
                       password = "")
    
     if ((input$nome1 == "") || (input$nome2 == "") || (input$email == "") || (input$idade == "") || (input$cidade == "")){
       sendSweetAlert(session,title = "Erro, dados não inseridos", text = "Por favor não deixe nenhum dos campos em branco", type = "error",
                      btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
       dbDisconnect(con2)
     }
     else{
      if (input$categoria == "Critico Amador"){
        queryEspectador <- paste("INSERT INTO espectador(primeiro_nome,ultimo_nome,email,idade,cidade) VALUES (","\"",input$nome1,"\"",",","\"",input$nome2,"\"",",","\"",input$email,"\"",",",as.integer(input$idade),",","\"",input$cidade,"\"",");")
        dbSendQuery(con2, queryEspectador)
        queryEspectadorID <- "SELECT id_espectador FROM espectador ORDER BY id_espectador DESC LIMIT 1;"
        idResultEspectador <- dbGetQuery(con2, queryEspectadorID)
        sendSweetAlert(session,title = paste("Seu ID de Crítico é: ",idResultEspectador[1,1]), text = "Seus dados foram cadastrados com sucesso!", type = "success",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        dbDisconnect(con2)
      }
      else if (input$categoria == "Critico Profissional"){
        queryCritico <- paste("INSERT INTO critico(primeiro_nome,ultimo_nome,email,idade,cidade) VALUES (","\"",input$nome1,"\"",",","\"",input$nome2,"\"",",","\"",input$email,"\"",",",as.integer(input$idade),",","\"",input$cidade,"\"",");")
        dbSendQuery(con2, queryCritico)
        queryCriticoID <- "SELECT id_critico FROM critico ORDER BY id_critico DESC LIMIT 1;"
        idResultCritico <- dbGetQuery(con2, queryCriticoID)
        sendSweetAlert(session,title = paste("Seu ID de Crítico é: ",idResultCritico[1,1]), text = "Seus dados foram cadastrados com sucesso!", type = "success",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        dbDisconnect(con2)
      }
     }
  })
  
  registrarAvaliacao <- observeEvent(input$submitAvaliacao,{
      con3 <- dbConnect(RMySQL::MySQL(),
                        dbname = "filmrating",
                        host = "127.0.0.1",
                        port = 3306,
                        user = "root",
                        password = "")
      
      if ((input$avaliador_id == "") || (input$filme_id == "")){
        sendSweetAlert(session,title = "Erro, dados não inseridos", text = "Por favor não deixe nenhum dos campos em branco", type = "error",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        dbDisconnect(con3)
      }
      
      else{
       
       if (input$avaliadorCategoria == "Critico Amador"){
       
        queryAvaliar1 <- paste("INSERT INTO avaliacao_publico(id_espectador,id_filme,nota) VALUES (",as.integer(input$avaliador_id),",",as.integer(input$filme_id),",",as.integer(input$nota),");")
        dbSendQuery(con3, queryAvaliar1)
       
        sendSweetAlert(session,title = paste("Sua avaliação foi registrada"), text = "A operação foi realizada com sucesso.", type = "success",
                      btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        dbDisconnect(con3)
     }
       else if (input$avaliadorCategoria == "Critico Profissional"){
         
         queryAvaliar2 <- paste("INSERT INTO avaliacao_critica(id_critico,id_filme,nota) VALUES (",as.integer(input$avaliador_id),",",as.integer(input$filme_id),",",as.integer(input$nota),");")
         dbSendQuery(con3, queryAvaliar2)
         
         sendSweetAlert(session,title = paste("Sua avaliação foi registrada"), text = "A operação foi realizada com sucesso.", type = "success",
                        btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
         dbDisconnect(con3)
       }
     }
  })
  
  deletarUsuario <- observeEvent(input$submitDelete,{
    con4 <- dbConnect(RMySQL::MySQL(),
                      dbname = "filmrating",
                      host = "127.0.0.1",
                      port = 3306,
                      user = "root",
                      password = "")
    
    #queryProcedure <- paste("CALL dadosUsuario(1,",as.integer(input$deletadoID),");")
    #dadosDeletado <- dbGetQuery(con4,queryProcedure)
    
    
    
    if (input$deletadoID == ""){
      sendSweetAlert(session,title = "Erro, dados não inseridos", text = "Por favor não deixe nenhum dos campos em branco", type = "error",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      dbDisconnect(con4)
    }
    else{
      if (input$deletadoCategoria == "Critico Amador"){
        
        queryDadosEspectador <- paste("SELECT * FROM espectador WHERE id_espectador = ",as.integer(input$deletadoID),";")
        dadosEspectador <- dbGetQuery(con4,queryDadosEspectador)
        
        queryDeletar1 <- paste("DELETE FROM espectador WHERE id_espectador = ",as.integer(input$deletadoID),";")
        dbSendQuery(con4, queryDeletar1)
        
        sendSweetAlert(session,title = paste("Você deletou o crítico amador nº ",dadosEspectador[1,1]), text = "A operação foi realizada com sucesso.", type = "success",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        dbDisconnect(con4)
      }
      else if (input$deletadoCategoria == "Critico Profissional"){
        
        queryDadosCritico <- paste("SELECT * FROM critico WHERE id_critico = ",as.integer(input$deletadoID),";")
        dadosCritico <- dbGetQuery(con4,queryDadosCritico)
        
        queryDeletar2 <- paste("DELETE FROM critico WHERE id_critico = ",as.integer(input$deletadoID),";")
        dbSendQuery(con4, queryDeletar2)
        
        sendSweetAlert(session,title = paste("Você deletou o crítico profissional nº ",dadosCritico[1,1]), text = "A operação foi realizada com sucesso.", type = "success",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        dbDisconnect(con4)
      }
    }
  })
  
  output$outavaliador_id <- renderText(paste("\nID do Crítico: ",input$avaliador_id))
  output$outfilmes <- renderText(paste("\nFilme Selecionado: ",input$filmes))
  output$outnota <- renderText(paste("\nNota: ",input$nota))
  
  output$filmesdf <- DT::renderDataTable({ movies })
  output$criticosdf <- DT::renderDataTable({ critics })
  output$espectadoresdf <- DT::renderDataTable({ public })
  
  dbDisconnect(con)
  
  # killDbConnections <- function () {
  #   
  #   all_cons <- dbListConnections(MySQL())
  #   
  #   print(all_cons)
  #   
  #   for(con in all_cons)
  #     +  dbDisconnect(con)
  #   
  #   print(paste(length(all_cons), " connections killed."))
  #   
  # }
}