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

# Define UI for application that draws a histogram
ui <- fluidPage(
      
      # Application title
      titlePanel("Flood Hazard Map Norfolk"),
      
      # Sidebar with a slider input for tide height 
      sidebarLayout(
        sidebarPanel(
          sliderInput("elev",
                      "Tide Height (m)",
                      min = 0,
                      max = 3,
                      value = 1,
                      step = 0.25)
        ),
        
        # Show a plot of the data
        mainPanel(
          width = 12,
          leafletOutput(outputId = "norfPlot")
        )
      )
    )

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


server <- function(input, output) {

    output$norfPlot <- renderLeaflet({
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    
        })
      
      #apparently observe makes it update each time a value changes?
      observe({
      withProgress(message = "plotting", {
        
        #getting the raster for that tide height 
        unclean.norf.trimmed.filtered <- unclean.norf.trimmed
        unclean.norf.trimmed.filtered[unclean.norf.trimmed.filtered >= input$elev] <- NA
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
      leafletProxy("norfPlot") %>%
        addRasterImage(combined.elev.1)
        
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
