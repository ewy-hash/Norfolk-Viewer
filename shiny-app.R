#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages("rsconnect")
library(rsconnect)
library(shiny)
library(tidyverse)
library(tidyterra)
library(sf)
library(terra)
library(tigris)
#install.packages("leaflet")
library(leaflet)
tide.input <- read_csv("deployable-data/Tide_Sensors_20251128.csv")
tide.input.clean <- tide.input |> 
  mutate(`Little Creek at 20th Bay St`= NULL) |> 
  mutate(`Elizabeth River Main Branch at Nauticus`= NULL) |> 
  drop_na() |> 
  mutate(useful.date = as.POSIXct(LocalTime, format = "%Y %b %d %I:%M:%S %p")) |> 
  mutate( `Elizabeth River Eastern Branch at Grandy Village` = (`Elizabeth River Eastern Branch at Grandy Village`*(1/3.28084))) |> 
  mutate(`Lafayette River at Mayflower Rd` = ( `Lafayette River at Mayflower Rd`*(1/3.28084))) |> 
  mutate(`Mason Creek at Granby St` = (`Mason Creek at Granby St`*(1/3.28084)))
p(
  "Welcome to Norfolk's flood hazard viewer!", br(),
  "You can use this site to navigate how rising sea water levels and tides will transform this city.",
  "Navigate to sensor view to view real flood gauge readings from sensors along the Elizabeth River, the Lafayette River and Mason Creek."
)



#not running again, need to read all this shit in again!!!!#####
unclean.norf.trimmed <- rast("deployable-data/raster-small5.tif")
unclean.norf.trimmed[is.na(unclean.norf.trimmed)] <- -99


ocean <- unclean.norf.trimmed

#there were some weird NA values in the top left corner that this fixes
ocean[ocean >= -90] <- NA

#plot(mask(unclean.norf.trimmed, ocean, inverse = TRUE))

ocean.patches <- patches(ocean)
ocean.patch <- ocean.patches
ocean.patch[ocean.patch != 1] <- NA
ocean.patch[ocean.patch == 1] <- -99


#these are the road polygons to be added in
# norfolk_roads <- roads(state = "VA", county = "Norfolk")
# hampton_roads <- roads(state = "VA", county = "Hampton")
# portsmouth_roads <- roads(state = "VA", county = "Portsmouth")
#whole map = [1], elizabeth = [2], lafayette = [3], mason creek =[4] thought this would make easier??


min_lng <- c(-76.389548, -76.334227, -76.33, -76.28)
max_lat <- c(36.971082, 36.867939, 36.92, 36.949)
max_lng <- c(-76.256, -76.25686953316985, -76.256, -76.256)
min_lat <- c(36.87, 36.8335194986698, 36.87, 36.923)
#36.8335194986698, -76.25686953316985

center_lng <- (min_lng + max_lng) / 2
center_lat <- (min_lat + max_lat) / 2
initial_zoom <- 12.4999999

#Function to compute elev####
ComputeRaster <- function(elevation) {
unclean.norf.trimmed.filtered <- unclean.norf.trimmed
unclean.norf.trimmed.filtered[unclean.norf.trimmed.filtered >= elevation] <- NA
#now to make the patches
norf.patches.test <- patches(unclean.norf.trimmed.filtered, 
                             directions = 8, 
                             values = FALSE, 
                             zeroAsNA=FALSE)

biggest.patch <- norf.patches.test
biggest.patch[biggest.patch != 1] <- NA

other.patches <- mask(norf.patches.test, biggest.patch, inverse=TRUE)

other.patches.uniform <- other.patches
other.patches.uniform[!is.na(other.patches.uniform)] <- 50

combined.elev <- merge(other.patches.uniform, biggest.patch)
combined.elev <- merge(ocean.patch, combined.elev)
combined.elev
}


#UI ####
# Define UI for application 
ui <- navbarPage("Flood Hazard Viewer Norfolk",
  tabPanel("Overall View",
  
  
  fluidPage(
  # Application title
  titlePanel("View Flooded Areas by Water Gauge Height"),
  p(
    "Welcome to Norfolk's flood hazard viewer!", br(),
    "You can use this site to navigate how rising sea water levels and tides will transform this city.",
    "Navigate to sensor view to view real flood gauge readings from sensors along the Elizabeth River, the Lafayette River and Mason Creek."
  ),
  
  
  # TOP panel, tide height, and norfplot
  fluidRow(
    column(width = 6, 
           offset = 1,
           sliderInput(inputId = "elev", 
                       label = "Tide Height in Feet",
                       min = 0,
                       max = 12,
                       value = 0,
                       step = .25,
           )),
    leafletOutput(outputId = "norfPlot")))),

  tabPanel("Sensor View",
           fluidPage(
  fluidRow(column(width = 6, offset = 1,
   sliderInput(inputId = "date", 
                              label = "Noreaster Surge",
                              min = min(tide.input.clean$useful.date),
                              max = max(tide.input.clean$useful.date),
                              value = min(tide.input.clean$useful.date),
                              step = 3600,
                              timeFormat = "%Y %b %d %H:%M:%S"), 
                  selectInput(inputId = "sensor",
                              label = "Sensor",
                              choices = c("Elizabeth River Eastern Branch at Grandy Village", 
                                          "Lafayette River at Mayflower Rd",
                                          "Mason Creek at Granby St"),
                              #selected = "1",
                              multiple = FALSE)),
   conditionalPanel(condition = "input.sensor == 'Elizabeth River Eastern Branch at Grandy Village'",
                    h3("Flooding along the Elizabeth River"),
                    leafletOutput("elizaPlot")
   ),
   conditionalPanel(
     condition = "input.sensor == 'Lafayette River at Mayflower Rd'",
     h3("Flooding along the Lafayette River"),
     leafletOutput("lafPlot")
   ),
   
   conditionalPanel(
     condition = "input.sensor == 'Mason Creek at Granby St'",
     h3("Flooding along Mason Creek"),
     leafletOutput("masPlot")
   )))
  
   ))
#SERVER####

server <- function(input, output) {
  
  output$norfPlot <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      setView(lng = center_lng[1], lat = center_lat[1], zoom = initial_zoom) |> 
      setMaxBounds(
        lng1 = min_lng, lat1 = min_lat, 
        lng2 = max_lng, lat2 = max_lat)
    #iterated. abunch here to try an dbring the box in, alas, to no avail
    
    
  })
  #making sub-plots

  output$elizaPlot <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      setView(lng = center_lng[2], lat = center_lat[2], zoom = initial_zoom)
  })
  
  output$lafPlot <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      setView(lng = center_lng[3], lat = center_lat[3], zoom = initial_zoom)
  })
  
  output$masPlot <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      setView(lng = center_lng[4], lat = center_lat[4], zoom = initial_zoom +2)
  })
  
  
#REACTIVES ####
  observeEvent(input$sensor, {
    if (input$sensor == "Elizabeth River Eastern Branch at Grandy Village") {
      withProgress(message = "plotting", {
        
        #determining height for gauge and time
        gauge.height <- tide.input.clean |> 
          select(useful.date, input$sensor) |> 
          filter(useful.date == input$date) |> 
          pull(input$sensor)
        
        
        #getting the raster for that tide height 
        computed.raster <- ComputeRaster(gauge.height)
        
        
        #now to make the leaflet proxy, which is needed because the map needs to update every slider input
        my_pal <- c(
          "#0B2E59", 
          "#3F8FCB", 
          "#64A36F"  )
        pal <- colorNumeric(
          palette = my_pal,
          domain = values(computed.raster),
          na.color = "transparent")
        leafletProxy("elizaPlot") %>%
          clearImages() %>%
          addRasterImage(computed.raster, colors = pal, opacity = 0.5)
        
      })
    }})
  observeEvent(input$date, {
    if (input$sensor == "Elizabeth River Eastern Branch at Grandy Village") {
      withProgress(message = "plotting", {
        
        #determining height for gauge and time
        gauge.height <- tide.input.clean |> 
          select(useful.date, input$sensor) |> 
          filter(useful.date == input$date) |> 
          pull(input$sensor)
        
        
        #getting the raster for that tide height 
        computed.raster <- ComputeRaster(gauge.height)
        
        
        #now to make the leaflet proxy, which is needed because the map needs to update every slider input
        my_pal <- c(
          "#0B2E59", 
          "#3F8FCB", 
          "#64A36F"  )
        pal <- colorNumeric(
          palette = my_pal,
          domain = values(computed.raster),
          na.color = "transparent")
        leafletProxy("elizaPlot") %>%
          clearImages() %>%
          addRasterImage(computed.raster, colors = pal, opacity = 0.5)
        
      })
    }})
  

  observeEvent(input$sensor, {
    if (input$sensor == "Lafayette River at Mayflower Rd") {
      withProgress(message = "plotting", {
        
        #determining height for gauge and time
        gauge.height <- tide.input.clean |> 
          select(useful.date, input$sensor) |> 
          filter(useful.date == input$date) |> 
          pull(input$sensor)
        
        
        #getting the raster for that tide height 
        computed.raster <- ComputeRaster(gauge.height)
        
        
        #now to make the leaflet proxy, which is needed because the map needs to update every slider input
        my_pal <- c(
          "#0B2E59", 
          "#3F8FCB", 
          "#64A36F"  )
        pal <- colorNumeric(
          palette = my_pal,
          domain = values(computed.raster),
          na.color = "transparent")
        leafletProxy("lafPlot") %>%
          clearImages() %>%
          addRasterImage(computed.raster, colors = pal, opacity = 0.5)
        
      })
    }})
  observeEvent(input$date, {
    if (input$sensor == "Lafayette River at Mayflower Rd") {
      withProgress(message = "plotting", {
        
        #determining height for gauge and time
        gauge.height <- tide.input.clean |> 
          select(useful.date, input$sensor) |> 
          filter(useful.date == input$date) |> 
          pull(input$sensor)
        
        
        #getting the raster for that tide height 
        computed.raster <- ComputeRaster(gauge.height)
        
        
        #now to make the leaflet proxy, which is needed because the map needs to update every slider input
        my_pal <- c(
          "#0B2E59", 
          "#3F8FCB", 
          "#64A36F"  )
        pal <- colorNumeric(
          palette = my_pal,
          domain = values(computed.raster),
          na.color = "transparent")
        leafletProxy("lafPlot") %>%
          clearImages() %>%
          addRasterImage(computed.raster, colors = pal, opacity = 0.5)
        
      })
    }})

  observeEvent(input$sensor, {
    if (input$sensor == "Mason Creek at Granby St") {
      withProgress(message = "plotting", {
        
        #determining height for gauge and time
        gauge.height <- tide.input.clean |> 
          select(useful.date, input$sensor) |> 
          filter(useful.date == input$date) |> 
          pull(input$sensor)
        
        
        #getting the raster for that tide height 
        computed.raster <- ComputeRaster(gauge.height)
        
        
        #now to make the leaflet proxy, which is needed because the map needs to update every slider input
        my_pal <- c(
          "#0B2E59", 
          "#3F8FCB", 
          "#64A36F"  )
        pal <- colorNumeric(
          palette = my_pal,
          domain = values(computed.raster),
          na.color = "transparent")
        leafletProxy("masPlot") %>%
          clearImages() %>%
          addRasterImage(computed.raster, colors = pal, opacity = 0.5)
        
      })
    }})
  observeEvent(input$date, {
    if (input$sensor == "Mason Creek at Granby St") {
      withProgress(message = "plotting", {
        
        #determining height for gauge and time
        gauge.height <- tide.input.clean |> 
          select(useful.date, input$sensor) |> 
          filter(useful.date == input$date) |> 
          pull(input$sensor)
        
        
        #getting the raster for that tide height 
        computed.raster <- ComputeRaster(gauge.height)
        
        
        #now to make the leaflet proxy, which is needed because the map needs to update every slider input
        my_pal <- c(
          "#0B2E59", 
          "#3F8FCB", 
          "#64A36F"  )
        pal <- colorNumeric(
          palette = my_pal,
          domain = values(computed.raster),
          na.color = "transparent")
        leafletProxy("masPlot") %>%
          clearImages() %>%
          addRasterImage(computed.raster, colors = pal, opacity = 0.5)
        
      })
    }})
  
  #apparently observe makes it update each time a value changes?
  observeEvent(input$elev, {
    withProgress(message = "plotting", {
      
      
      
      computed.raster <- ComputeRaster(input$elev/3.28084)
      
      
      #now to make the leaflet proxy, which is needed because the map needs to update every slider input
      my_pal <- c(
        "#0B2E59", 
        "#3F8FCB", 
        "#64A36F"  )
      pal <- colorNumeric(
        palette = my_pal,
        domain = values(computed.raster),
        na.color = "transparent")
      leafletProxy("norfPlot") %>%
        clearImages() %>%
        addRasterImage(computed.raster, colors = pal, opacity = 0.5)
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
