#libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(bslib)

#all final data files must be in the shiny_dashboard directory

#UI
ui <- navbarPage("DSPG",
  
  tabPanel(
    "Overview", h3("Health and Infrastructure in Rural Virginia"),
    fluidPage(
      textOutput(outputId = 'welcome'),
      textInput(inputId = 'name', 
                label = 'Enter your name',
                value = 'Your name here'),
      
      
    )
  ),
  
  tabPanel("Literature Review",
    fluidRow(
      column(4, p("hellofhbfashefmsn da")),
      column(4, p("fjhqebfjqhewfblwefwe")),
      column(4, p("fljebfmnsd fmnse fmnf me"))
    )
  ),
  
  tabPanel(
    "Results",
    fluidPage(
      h1("Results")
      # additional content
    )
  ), 
  
  tabPanel(
    "Data Sources", 
    fluidRow(style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
             h1("Data Sources", align = "center")
      #additional content
    )
  ), 
  
  tabPanel(
    "About Us", 
    fluidPage(
      style = "margin-left: 50px; margin-right: 50px; font-size: 16px; font-family: 'Times New Roman', Times, serif;",
      h1("Meet the Interns!", 
         align = "center"), 
      div(
        style = "text-align:center;",
        img(src = "interns.jpg", 
          height = "400px", 
          width = "700px")
      #additional content
    )
  )
  )
  
  
  
) #end ui

chr <- read.csv("cleanCHR25.csv")

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
  
  
} #end server

shinyApp(ui, server)
