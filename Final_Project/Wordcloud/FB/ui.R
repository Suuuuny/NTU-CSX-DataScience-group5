library(shiny)

shinyUI(tabPanel("臉書文字雲",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("selectionC", "Choose a candi:",
                                 choices = candi),
                     selectInput("selectionM", "Choose a month:",
                                 choices = month),
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
)