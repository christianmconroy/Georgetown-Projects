#Template for a US
library(shiny)
# Define UI
shinyUI(fluidPage(
  titlePanel(title="Text input & output"),
 
  sidebarLayout(
    sidebarPanel("Input area",
                 textInput("semester", "Enter semester", ""),
                 textInput("year", "Enter year", "")),
    mainPanel("Output area",
              textOutput("semesterout"),
              textOutput("yearout"))
  )
  ))
