# server.R

load("stats.RData")
library(shiny)
library(ggplot2)

shinyServer(
        function(input, output) {
                output$plot1 <- renderPlot({
                        df <- switch(input$var,
                                     "QuarterBack" = qbstats,
                                     "Running Back" = rbstats,
                                     "Wide Receiver" = wrstats,
                                     "Tight End" = testats)
                        ggplot(df, aes(yards)) + geom_histogram() 
                });
                output$plot2 <- renderPlot({
                        df <- switch(input$var,
                                     "QuarterBack" = qbstats,
                                     "Running Back" = rbstats,
                                     "Wide Receiver" = wrstats,
                                     "Tight End" = testats)
                        ggplot(df, aes(touchdown)) + geom_histogram() 
                });
                output$plot3 <- renderPlot({
                        df <- switch(input$var,
                                     "QuarterBack" = qbstats,
                                     "Running Back" = rbstats,
                                     "Wide Receiver" = wrstats,
                                     "Tight End" = testats)
                        ggplot(df, aes(average)) + geom_histogram() 
                })
               

        }) 