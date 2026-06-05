#libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(bslib)

#UI

# navbar
ui <- navbarPage("DSPG",
  
  tabPanel(
    "Overview", h3("Health and Infrastructure in Rural Virginia"),
    fluidPage(
      textOutput(outputId = 'welcome'),
      textInput(inputId = 'name', 
                label = 'Enter your name',
                value = 'Your name here'),
      plotOutput(outputId = 'chr_plot')
      
      
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
