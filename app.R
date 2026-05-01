library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

master <- readRDS("data/master_metros.rds")

pal <- colorNumeric(
  palette = "YlOrRd",
  domain  = master$afford_score
)

ui <- fluidPage(
  leafletOutput("map", height = "600px")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(master) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = ~pal(afford_score),
        fillOpacity = 0.7,
        color       = "#444444",
        weight      = 1,
        smoothFactor = 0.5
      )
  })
}

shinyApp(ui, server)