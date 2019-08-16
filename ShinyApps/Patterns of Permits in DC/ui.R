library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application that analyzes the patterns of permit subtypes in DC
shinyUI(fluidPage(
  
  # Change the theme to superhero
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Patterns of Permits in Washington DC"),
  
  # Four sidebars for uploading files, selecting permit subtypes, selecting wards, and checking whether or not to show fee circles
  
  sidebarLayout(
    sidebarPanel(
      
      # Create a file input
      fileInput("file","Choose A CSV File Please",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Create a multiple checkbox input for wards
      
      hr(),
      helpText("Please Select The Wards You Want To Analyze For Permits"),
      helpText("You Can Choose More Than One"),
      
      checkboxGroupInput("ward",
        "Wards:",
        choices = list("Ward 1"= 1,"Ward 2"= 2,"Ward 3"= 3,"Ward 4"= 4, "Ward 5"= 5,"Ward 6"= 6,"Ward 7"= 7, "Ward 8"=8)
      ),
      
    # Add a drop down menu to filter based on permit subtype
      
    selectInput("permitsub", 
    "Select a Permit Subtype",
    choices = list("ADDITION", "ADDITION ALTERATION REPAIR", "ALTERATION AND REPAIR" , "AWNING", "BOILER", "BUILDING", "CAPACITY PLACARD", "CIVIL PLANS", "DEMOLITION", "ELECTRICAL", "ELECTRICAL - GENERAL", "ELECTRICAL - HEAVY UP", "ELEVATOR - ALTERATION", "ELEVATOR - NEW", "ELEVATOR - REPAIR", "EXCAVATION ONLY", "EXPEDITED", "FENCE", "FOUNDATION ONLY", "GARAGE", "GAS FITTING", "MECHANICAL", "MISCELLANEOUS", "NEW BUILDING", "PLUMBING", "PLUMBING AND GAS",  "RAZE", "RETAINING WALL", "SHED", "SHEETING AND SHORING", "SIGN",  "SOLAR SYSTEM", "SPECIAL BUILDING", "SPECIAL SIGN", "SWIMMING POOL", "TENANT LAYOUT")
    ),
    
  # Add option to create circle around point reflecting size of fee paid for permit  
    checkboxInput('checkbox1', label = 'Show fees paid radius')
    ),
    
    # Make the sidebar on the right of the webpage
    position = "right",
    fluid = TRUE,

    # Create two tabs
    mainPanel(
      hr(),
      tabsetPanel(type="tabs",
                  
                  #Add a tab for problem description
                  tabPanel("Problem Description", textOutput("text")),
                  
                  #Add a tab for decriptive table
                  tabPanel("Descriptive Analysis",
        
                           #Add two subtabs
                           tabsetPanel(
                             tabPanel("Zoning Types",verbatimTextOutput("table1"))
                           )
                  ),
                                       
                  #Tab for the Leaflet Map
                  tabPanel("Map", leafletOutput("map", height=630))
      )
    )
  )
))