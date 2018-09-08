fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("wordcloud_Ap_candi", "請選擇候選人",
                  choices = list("柯文哲"="Ko", "丁守中"="Di", "姚文智"="Yao")),
      selectInput("wordcloud_Ap_month", "請選擇月份",
                  choices = list("一月"="1", "二月"="2", "三月"="3")),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)