library(shiny)

shinyUI(tabPanel("臉書文字雲",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("candiXD", "北市候選人 :",
                                 choices = list("柯文哲" = 1, "丁守中" = 2, "姚文智" = 3),
                                 selected = 1),
                     radioButtons("yue", label = h3("月份"),
                                  choices = list("2018" = 0, "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6), 
                                  selected = 0),
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