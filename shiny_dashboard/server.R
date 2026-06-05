#libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)

chr <- read.csv("../cleaned_data/cleanCHR25.csv")


#server
server <- function(input, output) {

  output$welcome<- renderText({
    paste0("Welcome to our website, ", input$name, "!")
    })
  
  
  output$chr_plot <- renderPlot({
    ggplot(chr, aes(Pct_Households_with_Broadband_Access, Years_of_Potential_Life_Lost_Rate)) +
      geom_point(alpha = 0.5)+
      xlab("% Households with Broadband Access")+
      ylab("YPLL Rate per 100,000")
    })
  
  
}