# Text of the books downloaded from:
# A Mid Summer Night's Dream:
#  http://www.gutenberg.org/cache/epub/2242/pg2242.txt
# The Merchant of Venice:
#  http://www.gutenberg.org/cache/epub/2243/pg2243.txt
# Romeo and Juliet:
#  http://www.gutenberg.org/cache/epub/1112/pg1112.txt

function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$wordcloud_Udn_candi,input$wordcloud_Udn_month)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  par(family=("Heiti TC Light"))
  output$plot <- renderPlot({
    v <- terms()
    par(family=("Heiti TC Light"))
    wordcloud_rep(v$Var1,v$Freq,
                  min.freq=input$freq,
                  max.words=input$max,
                  random.order=TRUE,random.color=TRUE, 
                  rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
                  ordered.colors=FALSE,use.r.layout=FALSE,
                  fixed.asp=TRUE)
    
  })
}