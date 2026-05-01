library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

master <- readRDS("data/master_metros.rds")

pal <- colorNumeric(
  palette = "YlGn",
  domain  = c(0, 100)
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Filter Metros"),
      sliderInput("afford", "Affordability (min score)",
                  min = 0, max = 100, value = 0),
      sliderInput("jobs", "Job Market (min score)",
                  min = 0, max = 100, value = 0),
      sliderInput("home", "Home Value (min score)",
                  min = 0, max = 100, value = 0),
      checkboxGroupInput("size", "City Size",
                         choices  = c("Small", "Medium", "Large", "Major"),
                         selected = c("Small", "Medium", "Large", "Major")),
      hr(),
      uiOutput("detail_panel")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  # Render base map once
  output$map <- renderLeaflet({
    leaflet(master) %>%
      addTiles() %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4)
  })
  
  # Filter data reactively
  filtered <- reactive({
    master %>%
      filter(
        afford_score    >= input$afford,
        job_score       >= input$jobs,
        homevalue_score >= input$home,
        size_category   %in% input$size
      )
  })
  
  # Update polygons when filters change
  observeEvent(filtered(), {
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data        = filtered(),
        fillColor   = ~pal(afford_score),
        fillOpacity = 0.7,
        color       = "#444444",
        weight      = 1,
        smoothFactor = 0.5,
        layerId      = ~GEOID
      )
  })
  
  # Store the clicked metro
  clicked_metro <- reactiveVal(NULL)
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    selected <- master %>%
      filter(GEOID == click$id) %>%
      st_drop_geometry()
    clicked_metro(selected)
  })
  
  # Render the detail panel
  output$detail_panel <- renderUI({
    metro <- clicked_metro()
    if (is.null(metro)) return(NULL)
    
    tagList(
      h4(metro$NAME),
      p(strong("Size: "), metro$size_category),
      p(strong("Affordability Score: "),  round(metro$afford_score, 1)),
      p(strong("Job Market Score: "),     round(metro$job_score, 1)),
      p(strong("Home Value Score: "),     round(metro$homevalue_score, 1)),
      p(strong("Median Rent: "),     paste0("$", formatC(metro$median_gross_rent, format="d", big.mark=","))),
      p(strong("Median Income: "),   paste0("$", formatC(metro$median_hh_income,  format="d", big.mark=","))),
      p(strong("Median Home Value:"),paste0("$", formatC(metro$median_home_value, format="d", big.mark=","))),
      p(strong("Unemployment: "),
        if (is.na(metro$unemployment_rate)) "N/A"
        else paste0(round(metro$unemployment_rate, 1), "%"))
    )
  })
}

shinyApp(ui, server)