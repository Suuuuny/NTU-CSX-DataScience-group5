library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  output$FaceCloudPlot <- renderPlot({
    # 柯文哲
    # if(input$candiXD == 1 && input$yue ==0){
    #   read.table("")
    #   plot(P,axes = FALSE)
    if(input$candiXD == 1 && input$yue ==1){
      P <- read.table("Di_1.txt")
      getTermMatrix(P)
    }else if(input$candiXD == 1 && input$yue ==2){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==3){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==4){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==5){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_May.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 1 && input$yue ==6){
      P <- load.image("wc/FB/Ko_wordcloud/Ko_Jun.png")
      plot(P,axes = FALSE)
    }
    
    # 丁守中
    if(input$candiXD == 2 && input$yue ==0){
      P <- load.image("wc/FB/Di_wordcloud/Di_2018.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==1){
      P <- load.image("wc/FB/Di_wordcloud/Di_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==2){
      P <- load.image("wc/FB/Di_wordcloud/Di_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==3){
      P <- load.image("wc/FB/Di_wordcloud/Di_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==4){
      P <- load.image("wc/FB/Di_wordcloud/Di_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==5){
      P <- load.image("wc/FB/Di_wordcloud/Di_May.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 2 && input$yue ==6){
      P <- load.image("wc/FB/Di_wordcloud/Di_Jun.png")
      plot(P,axes = FALSE)
    }
    # 姚文智
    if(input$candiXD == 3 && input$yue ==0){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_2018.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==1){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Jan.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==2){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Feb.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==3){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Mar.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==4){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Apr.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==5){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_May.png")
      plot(P,axes = FALSE)
    }else if(input$candiXD == 3 && input$yue ==6){
      P <- load.image("wc/FB/Yao_wordcloud/Yao_Jun.png")
      plot(P,axes = FALSE)
    }
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
})