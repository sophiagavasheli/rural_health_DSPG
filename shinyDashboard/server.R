#libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)

chr <- read.csv("../cleaned_data/cleanCHR25.csv")


#server
server <- function(input, output) {
  output$greeting = renderText({"Background on Health and Infrastructure in Rural Virginia"})
  
  output$normal_plot = renderPlot({
    samples = rnorm(1000, 0, 1) #random normal vector
    hist(samples, breaks=30, col="maroon", main="histogram", xlab="val")
  })

  output$welcome<- renderText({
    paste0("Welcome to our website, ", input$name, "!")
    })
  
  
  output$chr_plot <- renderPlot({
    ggplot(chr, aes(Pct_Households_with_Broadband_Access, Years_of_Potential_Life_Lost_Rate)) +
      geom_point(alpha = 0.5)+
      xlab("% Households with Broadband Access")+
      ylab("YPLL Rate per 100,000")
    })
  
  output$results <- renderText({
    paste("Results will go here.")
  })
  
  
}