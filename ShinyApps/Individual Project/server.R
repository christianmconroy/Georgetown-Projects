options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(leaflet)
library(dplyr)

# Define server that analyzes the patterns of permits in DC
shinyServer(function(input, output) {
  
  # Create an output variable for problem description
  output$text <- renderText({
    
"This project uses the dataset 'Building Permits In 2017'. The dataset contains information for 2017 building permits approved in DC, including application status, issue date, business improvement district, permit type, permit subtype, full address, description of work, fees paid, ward, anc, psa, and neighborhood cluster.
Input the appropriate file by selecting 'Browse' and use the sidebar menu to select the appropriate options. The table in the tab labeled 'Descriptive Analysis' shows what zoning ordinances correspond to each permit subtype across wards. The map vizualization in the tab labeled 'Map' shows what types of permit subtypes are most prevalent in each ward; how the fees for each permit differ across subtypes; and whether fees for each subtype are consistent across wards." 
    
  })
  
  # Create a descriptive table for different zoning types
  output$table1 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file

    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    mydata$WARD <- as.factor(mydata$WARD)
    
    # Filter the data for different permit subtypes and different wards
    target1 <- c(input$ward)
    target2 <- c(input$permitsub)
    permit_df  <- filter(mydata, WARD == target1 & PERMIT_SUBTYPE_NAME == target2)
    
    # Create a table for zoning type
    table(permit_df$ZONING)
    
  })
  
  # Create a descriptive table for different zoning types
  output$table2 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file

    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different permit subtypes and different wards
    target1 <- c(input$ward)
    target2 <- c(input$permitsub)
    permit_df  <- filter(mydata, WARD == target1 & PERMIT_SUBTYPE_NAME == target2)
    
    # Create a table for zoning type
    table(permit_df$ZONING)
    
  })
  
  # Create a map output variable
  output$map <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different permit subtypes and different wards
    target1 <- c(input$ward)
    target2 <- c(input$permitsub)
    permit_df  <- filter(mydata, WARD == target1 & PERMIT_SUBTYPE_NAME == target2)
    
    # Create the leaflet function for data
    leaflet(permit_df) %>%
      
      # Set the default view
      setView(lng = -77.0369, lat = 38.9072, zoom = 12) %>%
      
      # Provide tiles
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      
      # Add circles
      addCircleMarkers(
        if(input$checkbox1){
        radius = ~ eval(permit_df$FEES_PAID/5000)} else {
        radius = 2}, 
        lng= permit_df$LONGITUDE,
        lat= permit_df$LATITUDE,
        stroke= FALSE,
        fillOpacity=0.4
      )
    })
  })