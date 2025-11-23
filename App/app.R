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


# Define UI for application that draws a plot
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
                  value = 0,
                  step = 0.25)
    ),
    
    # Show a plot of the data
    mainPanel(
      plotOutput("spatPlot")
    )
  )
)
unclean.norf.trimmed <- rast("../Data/raster-small5.tif")

#I put this part outside so it would run only once


# Define server logic required to draw a spatraster plot
server <- function(input, output) {
  
  output$spatPlot <- renderPlot({
    withProgress(message = "plotting", {
      #this is just from the working doc
      
      ##dont need to use these since We saved the filtered file
      #unclean.norf <- rast("../Data/VA_Southern_GCS_3m_NAVDm.tif")
      #bbox <- ext(c(xmin = -76.389548, xmax = -76.154689, ymin = 36.834249, ymax = 36.971082))
      #unclean.norf.masked <- mask(unclean.norf, bbox, )
      #unclean.norf.trimmed <- trim(unclean.norf.masked)
      unclean.norf.trimmed.filtered <- unclean.norf.trimmed
      unclean.norf.trimmed.filtered[unclean.norf.trimmed.filtered >= input$elev] <- NA
      #set the elev set by user to the map I thinkkkkk/hopeeee
      
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
    })
      ggplot()+
        geom_spatraster(data = combined.elev.1)+
        scale_fill_gradient(low = "skyblue", high = "yellow")+
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
