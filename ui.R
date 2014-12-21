library(shiny)

shinyUI(fluidPage(
  titlePanel("NFL statastics in 2014 season"),
  
  sidebarLayout(position = "left",
    sidebarPanel(
      helpText("Statastics graphs for offence players from 2014 season until week 14"),
     
      radioButtons("var",
                   label = h3("Choose the player according to his position"),
                   choices = c("QuarterBack", "Running Back",  
                               "Wide Receiver", "Tight End"),
                   selected = "QuarterBack")
  ),
  mainPanel(plotOutput("plot1"),
            plotOutput("plot2"),
            plotOutput("plot3"))  
  
)
))