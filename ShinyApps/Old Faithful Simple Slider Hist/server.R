
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  function(input, output) {
  output$semesterout <- renderText(input$semester)
  output$yearout <- renderText(input$year)
  }
  )
