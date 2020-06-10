library(shiny)
library(SentimentAnalysis)
library(SnowballC)
library(shinydashboard)
library(dplyr)


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)








sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sentiment Analysis", tabName = "sentiment"),
    menuItem("Descriptive Statistics", tabName = "descriptive"),
    menuItem("Dialogue Generator", tabName = "dialogue"),
    menuItem("Information", tabName = "information")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "sentiment"),
    
    tabItem(tabName = "desriptive"),
    
    tabItem(tabName = "dialogue",
            fluidRow(
            box(plotOutput("myplot", height = 250)),
            
            box(
              title = "Controls",
              sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )  
    ),
    
    tabItem(tabName = "information")
  )
)


ui <-  dashboardPage(skin = "red",
                     dashboardHeader(title = "Kenny Actually Has Dialogue", titleWidth = 300),
                     sidebar,
                     body
)




server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- scores_by_C$average_score
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    
    
    
  })
  output$myplot <- renderPlot(plot(mtcars$mpg, mtcars$hp))
  output$testing <- renderPrint("testest")
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}


shinyApp(ui, server)