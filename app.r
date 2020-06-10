library(shiny)
library(SentimentAnalysis)
library(SnowballC)
library(shinydashboard)
library(dplyr)
library(readr)
library(stringr)


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
options(readr.num_columns = 0)
charnames <- c("Cartman", "Stan", "Kyle", "Butters", "Randy", "Mr. Garrison", "Chef", "Kenny", "Sharon", "Mr. Mackey")


#importing all p mats and fw prob




 fws <- list()
 ps <- list()
 for (i in 1:10){
   for (i in 1:ntopchar){
     filestring <- paste("pmats/", charnames[i],"onewordm.csv", sep = "")

     ps[[i]] <- read_csv(filestring)

     filestring <- paste("pmats/", charnames[i],"fwprob.csv", sep = "")

     fws[[i]] <- read_csv(filestring)
   }
 }
uniqs <- list()
for (i in 1:ntopchar){
  filestring <- paste("pmats/", topcounts$Character[i],"uniqs.csv", sep = "")
  
  uniqs[[i]] <- read_csv(filestring)
}



firstword <- function(charnum){
  currentfws <- fws[[charnum]]
  p = runif(1)
  iter = 1
  sums = 0
  
  while (sums < p){
    currentp = currentfws[iter,1]
    sums = sums + currentp
    iter = iter + 1
  }
  
  loc = iter - 1
  
  return(uniqs[[charnum]][loc,1])
}


nextword <- function(charnum, word){
  p = runif(1)
  row = which.max(uniqs[[4]] == word)
  iter = 1
  sums = 0
  while (sums < p){
    currentp = ps[[charnum]][row,iter]
    sums = sums + currentp
    iter = iter + 1
  }
  
  loc = iter - 1
  
  return(uniqs[[charnum]][loc,1])
}

generate <- function(characterchoice, length){
  subtry <- fws[[3]]
  senlen = length
  print(characterchoice)
  senlist = list()
  
  senlist[1] = firstword(characterchoice)
  
  row = match(senlist[1], uniq[[characterchoice]])
  
  sumrow = sum(ps[[characterchoice]][row,])
  
  iter = 1
  
  while (sumrow > 0 && iter <= senlen){
    
    newchoice = nextword(characterchoice, senlist[iter])
    iter = iter + 1
    senlist[iter] = newchoice
  }
  
  senlist <- as.matrix(senlist)
  
  sentence <- ""
  
  for (i in senlist){
    sentence <- paste(sentence, i, "")
  }
  
  return(sentence)
}



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
    
    tabItem(tabName = "descriptive"),
    
    tabItem(tabName = "dialogue",
            fluidRow(
            box(textOutput("reprint")),
            
            box(
              title = "Character Choice",
              selectInput("char", "Character:", charnames)
              ),
            box(
              title = "# of Words in The Sentence",
              numericInput("senlength", "Sentence Length", 20, min = 2, max = 100)
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
  
  output$reprint <- renderText(generate(which.max(charnames == input$char), input$senlength))

}


shinyApp(ui, server)
