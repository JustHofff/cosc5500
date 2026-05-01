library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

master <- readRDS("data/master_metros.rds")

ui <- fluidPage(
  leafletOutput("map", height = "600px")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(master) %>%
      addTiles() %>%
      addPolygons()
  })
}

shinyApp(ui, server)