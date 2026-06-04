#libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(bslib)

#UI

# navbar
ui <- navbarPage(
  title = "Health and Infrastructure in Rural Virginia",
  
  tabPanel(
    "Overview",
    fluidPage(
      textOutput(outputId = "greeting"),
      plotOutput(outputId = 'normal_plot')
    )
  ),
  
  tabPanel(
    "Results",
    fluidPage(
      # additional content
    )
  )
  
  
  
  
  
) #end ui
