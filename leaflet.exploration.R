#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(tidyverse)
library(tidyterra)
library(sf)
library(terra)
library(tigris)
#install.packages("leaflet")
library(leaflet)
library(shiny)
tide.input <- read_csv("Data/Tide_Sensors_20251128.csv")
tide.input.clean <- tide.input |> 
  mutate(`Little Creek at 20th Bay St`= NULL) |> 
  mutate(`Elizabeth River Main Branch at Nauticus`= NULL) |> 
  drop_na() |> 
  mutate(useful.date = as.POSIXct(LocalTime, format = "%Y %b %d %I:%M:%S %p")) |> 
  mutate( `Elizabeth River Eastern Branch at Grandy Village` = (`Elizabeth River Eastern Branch at Grandy Village`*(1/3.28084))) |> 
  mutate(`Lafayette River at Mayflower Rd` = ( `Lafayette River at Mayflower Rd`*(1/3.28084))) |> 
  mutate(`Mason Creek at Granby St` = (`Mason Creek at Granby St`*(1/3.28084)))



# Define UI for application that 
ui <- fluidPage(
      
      # Application title
      titlePanel("Flood Hazard Map Norfolk"),
      
      # Sidebar with a slider input for tide height 
      sidebarLayout(
        sidebarPanel( sliderInput(inputId = "date", 
                     label = "Noreaster Surge",
                      min = min(tide.input.clean$useful.date),
                      max = max(tide.input.clean$useful.date),
                      value = min(tide.input.clean$useful.date),
                      step = 3600,
                     timeFormat = "%Y %b %d %H:%M:%S"), 
      sidebarPanel( #sliderInput(tide.input.clean, "Storm Surge")
          selectInput(inputId = "sensor",
                      label = "Sensor",
                      choices = c("Elizabeth River Eastern Branch at Grandy Village", 
                                  "Lafayette River at Mayflower Rd",
                                  "Mason Creek at Granby St"),
                      selected = "1",
                      multiple = FALSE,
                      width= '250%')
                      )),
        # Show a plot of the data
        mainPanel(
          width = 8,
          leafletOutput(outputId = "norfPlot"),
      #tiny panes
      conditionalPanel(condition = "input.sensor == 'Elizabeth River Eastern Branch at Grandy Village'",
                       h3 = "flooding along Elizabeth River",
                       plotOutput("elizaPlot"),
                       conditionalPanel(
                         condition = "input.sensor == 'Lafayette River at Mayflower Rd'",
                         h3("Flooding along Lafayette River"),
                         plotOutput("layfPlot")
                       ),
                       
                       conditionalPanel(
                         condition = "input.sensor == 'Mason Creek at Granby St'",
                         h3("Flooding along Mason Creek"),
                         plotOutput("masPlot")
  
      )
    ))))

#not running again, need to read all this shit in again!!!!#####
unclean.norf.trimmed <- rast("Data/raster-small5.tif")
unclean.norf.trimmed[is.na(unclean.norf.trimmed)] <- -99


ocean <- unclean.norf.trimmed

#there were some weird NA values in the top left corner that this fixes
ocean[ocean >= -90] <- NA

plot(mask(unclean.norf.trimmed, ocean, inverse = TRUE))
ocean.patches <-patches(ocean)
ocean.patch <- ocean.patches
ocean.patch[ocean.patch != 1] <- NA
ocean.patch[ocean.patch == 1] <- -99


#these are the road polygons to be added in
norfolk_roads <- roads(state = "VA", county = "Norfolk")
hampton_roads <- roads(state = "VA", county = "Hampton")
portsmouth_roads <- roads(state = "VA", county = "Portsmouth")
#whole map = [1], elizabeth = [2], lafayette = [3], mason creek =[4] thought this would make easier??
min_lng <- c(-76.389548, -76.334227, -76.33, -76.28)
max_lat <- c(36.971082, 36.867939, 36.92, 36.949)
max_lng <- c(-76.256, -76.256, -76.256, -76.256)
min_lat <- c(36.87, 36.835001, 36.87, 36.923)

center_lng <- (min_lng + max_lng) / 2
center_lat <- (min_lat + max_lat) / 2
initial_zoom <- 12.4999999




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
        setView(center_lng[2], lat = center_lat[2], zoom = initial_zoom)
    })
    
    output$layfPlot <- renderLeaflet({
      leaflet() |> 
        setView(lng =center_lng[3], lat = center_lat[3], zoom = initial_zoom)
    })
    
    output$masPlot <- renderLeaflet({
      leaflet() |> 
        setView(center_lng[4], lat = center_lat[4], zoom = initial_zoom)
    })
    
    

      
      #apparently observe makes it update each time a value changes?
      observe({
      withProgress(message = "plotting", {
        
        #determining height for gauge and time
        gauge.height <- tide.input.clean |> 
          select(useful.date, input$sensor) |> 
          filter(useful.date == input$date) |> 
          pull(input$sensor)
        
        
        #getting the raster for that tide height 
        unclean.norf.trimmed.filtered <- unclean.norf.trimmed
        unclean.norf.trimmed.filtered[unclean.norf.trimmed.filtered >= gauge.height] <- NA
        
        #sets the elev set by user to the map I thinkkkkk/hopeeee
        
        
        #now to make the patches
        norf.patches.test <- patches(unclean.norf.trimmed.filtered, 
                                     directions = 8, 
                                   values = FALSE, 
                                   zeroAsNA=FALSE)
      
      biggest.patch.elev.1 <- norf.patches.test
      biggest.patch.elev.1[biggest.patch.elev.1 != 1] <- NA
      
      other.patches.elev.1 <- mask(norf.patches.test, biggest.patch.elev.1, inverse=TRUE)
      
      other.patches.elev.1.uniform <- other.patches.elev.1
      other.patches.elev.1.uniform[!is.na(other.patches.elev.1.uniform)] <- 50
      
      combined.elev.1 <- merge(other.patches.elev.1.uniform, biggest.patch.elev.1)
      combined.elev.1 <- merge(ocean.patch, combined.elev.1)
      
      
      #now to make the leaflet proxy, which is needed because the map needs to update every slider input
      my_pal <- c(
        "#0B2E59", 
        "#3F8FCB", 
        "#64A36F"  )
      pal <- colorNumeric(
        palette = my_pal,
        domain = values(combined.elev.1),
        na.color = "transparent")
       leafletProxy("norfPlot") %>%
        clearImages() %>%
        addRasterImage(combined.elev.1, colors = pal, opacity = 0.5)
        
    })
    # ggplot()+
    #   geom_spatraster(data = combined.elev.1)+
    #   scale_fill_gradient2(low = "darkblue", 
    #                        high = "red",
    #                        mid = "steelblue", 
    #                        midpoint = 0, 
    #                        na.value = "grey50")+
    #   theme(
    #     plot.background  = element_rect(fill = "white", color = NA),
    #     panel.background = element_rect(fill = "white", color = NA)
    #   )+
        
       

        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
