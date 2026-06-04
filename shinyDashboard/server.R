#libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)

#server
server = function(input, output) {
  output$greeting = renderText({"Hello world!"})
  
  output$normal_plot = renderPlot({
    samples = rnorm(1000, 0, 1) #random normal vector
    hist(samples, breaks=30, col="maroon", main="histogram", xlab="val")
  })
  
  
  
  
  
}