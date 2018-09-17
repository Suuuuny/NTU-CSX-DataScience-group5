function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$wordcloud_news, input$wordcloud_news_candi, input$wordcloud_news_month)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    par(family=("Heiti TC Light"))
    wordcloud_rep(v$Var1,v$Freq,
                  min.freq=15,
                  random.order=TRUE,random.color=TRUE, 
                  rot.per=.1, colors=rainbow(length(row.names(target_file))),
                  ordered.colors=FALSE,use.r.layout=FALSE,
                  fixed.asp=TRUE)
    
  })
}