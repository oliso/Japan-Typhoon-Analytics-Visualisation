library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(rgeos)

setwd("D:/Users/Oliver/Projects/R/Shiny Japan Typhoon/ShinyApp")

#### UI ####

ui <- fluidPage(
  fluidRow(
    titlePanel("JPN WS Curves"), 
    
    sidebarLayout(
      
      sidebarPanel(
        
        sliderInput(inputId = "gate",
                    label = "Gate Number",
                    min = 1,
                    max = 9,
                    value = 5),
        leafletOutput(outputId = "LeafletViewer"),
        tableOutput(outputId = "StatsTable")
      ),
      
      mainPanel(
        
        plotOutput(outputId = "Plot1"),
        plotOutput(outputId = "Plot2")
        #plotOutput(outputId = "Plot3")
      )
    )
  )
)


#### Server ####

server <- function(input, output) {
  
  #Plot 1
  output$Plot1 <- renderPlot({
    
    load("Exc_by_Pressure.Rda")
    
    Gate_no <- input$gate
    
    C_Matrix = P_Cumul_Matrix %>% filter(Gate == Gate_no)
    ggplot(C_Matrix, aes(factor(StormCat), Rate, fill = DataSource)) + 
      geom_bar(stat="identity", position = "dodge") + 
      #scale_fill_brewer(palette = "Set1") +
      scale_fill_manual(values = c("Red","Black","Blue")) +
      ggtitle(paste('Exceedence rate (n/year) at Gate',Gate_no,'by Pressure Categories; JMA datapoints:',C_Matrix[6,4]*66))
    
  })
  
  #Plot 2
  output$Plot2 <- renderPlot({
    
    load("Exc_by_Wind.Rda")
    
    Gate_no <- input$gate
    
    C_Matrix = W_Cumul_Matrix %>% filter(Gate == Gate_no)
    ggplot(C_Matrix, aes(factor(StormCat), Rate, fill = DataSource)) + 
      geom_bar(stat="identity", position = "dodge") + 
      #scale_fill_brewer(palette = "Set1") +
      scale_fill_manual(values = c("Red","Black","Blue")) +
      ggtitle(paste('Exceedence rate (n/year) at Gate',Gate_no,'by Wind Categories; JMA datapoints:',C_Matrix[6,4]*40))
    
  })
  
  #Leaflet Plot
  output$LeafletViewer <- renderLeaflet({
    
    load("Gates_prepped.Rda")
    
    Gate_no <- input$gate
    
    SELECTED_GATE = Gates_prepped %>% filter(GroupID == Gate_no)
    
    #Creating the Gates spatiallines file:
    Coordinates = data.frame( long=SELECTED_GATE$Lon, lat=SELECTED_GATE$Lat, gate=SELECTED_GATE$GroupID)   #Original: Coordinates = data.frame( long=GATES$Long, lat=GATES$Lat, gate=rep( 1:(nrow(GATES)/2), each=2))
    Coords.split = split( Coordinates[ ,c( "long", "lat")], Coordinates$gate)
    gate.Lines  = lapply( Coords.split, Line)
    for( i in 1:length(gate.Lines)) gate.Lines[[i]] = Lines( gate.Lines[[i]], i)
    gate.SpatialLines = SpatialLines( gate.Lines)
    
    # Using rGeos to create the rectangles around our spatial lines - creates a spatial polygons object (width 1 = 100km)
    Buffer = gBuffer(gate.SpatialLines, width = 0.35, byid = T, capStyle = 'round' )
    
    leaflet(Buffer) %>%
      addTiles() %>%
      addPolygons(color = "Blue",opacity = 0.2) 
    
  })
  
  #Gate Info Table
  load("DataCounts.Rda")
  load("Gate_Lengths.Rda")
  
  output$StatsTable <- renderTable({
    
    Gate_no <- input$gate
    
    StatsMx = matrix(data=c("Gate:",Gate_no,
                            "Length (km):",round(Gate_Lengths$GateLength[Gate_no],digits=2),
                            "JMA Landfalls:",DataCounts$Count[DataCounts$DataSource == "JMA"][Gate_no]),
                     nrow = 3,
                     ncol = 2,
                     byrow = TRUE)  
    
    
    
    
  },
  colnames = FALSE)
  
}


#### shinyApp command ####

shinyApp(ui = ui, server = server)