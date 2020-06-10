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

expl1 <- "This is a model that generates speech from each character based on their lines on the show. 
It randomly chooses a first word to start a sentence with based on what their lines begin with, 
and then looks at all the words they have said after that word and randomly 
(but proportionally chooses a next word. This repeats until a word is reached that always ended a line on the 
show (no positive probabilities for a next word) or the sentence reaches the user defined max number of words. 
This type of model is known as a Markov Chain. It does not do a very good job :(."
expl2 <- "I also developed a two-word Markov Chain that looked at every unique pair of words and then generated a likely word based on that. 
This gave much more realistic events but relied on a probability matrix that was ~1.2 gigabytes (These matrices are extremely sparse, 
and I really should have been able to wildly reduce this but ran out of time). The code for this model is in “modelbuilding.Rmd” as well 
as the code for one word Markov. Run it at your own (and your RAM’s) risk."
expl3 <- "For more check out this guys awesome Markov model for generating Trump tweets. He is much better at building efficient, working models than I am.
https://filiph.github.io/markov/"

ntopchar = 10
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
  filestring <- paste("pmats/", charnames[i],"uniqs.csv", sep = "")
  
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
  row = which.max(uniqs[[charnum]] == word)
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

generate <- function(characterchoice, len){
  senlen = len
  senlist = list()
  
  senlist[1] = firstword(characterchoice)
  
  myrow = which.max(uniqs[[characterchoice]] == senlist[1])
  
  sumrow = sum(ps[[characterchoice]][myrow,])
  
  iter = 1
  
  while (sumrow > 0 && iter <= senlen){
    
    newchoice = nextword(characterchoice, senlist[iter])
    iter = iter + 1
    senlist[iter] = newchoice
  }
  
  senlist <- as.matrix(senlist)
  
  senlist[length(senlist)] <- str_trim(senlist[length(senlist)] ,"both")
  
  senlist[1] <- str_to_title(senlist[1])
  
  sentence <- ""
  
  for (i in senlist){
    sentence <- paste(sentence, i, "")
  }
  
  sentence <- paste(sentence, ".", sep = "")
  
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
              box(textOutput("reprint"),title = "Generated Line:", solidHeader = T, background = "aqua"),
              
              box(
                title = "Character Choice", solidHeader = T, background = "light-blue",
                selectInput("char", "Character:", charnames)
                )
            ),
            fluidRow(
              box(expl1, br(), expl2, br(), expl3, title = "About This Model", background = "red"),
              box(
                title = "# of Words in The Sentence", solidHeader = T, background = "green",
                numericInput("senlength", "Choose Between 2 and 100 words.", 20, min = 2, max = 100),
                "The longer of line you generate, the more patient you are going to have to be with the model."
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
