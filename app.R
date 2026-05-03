library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

LNG <- -98.5
LAT <- 39.5
ZOOM <- 4

master <- readRDS("data/master_metros.rds")

pal <- colorNumeric(
  palette = "YlGn",
  domain = c(0, 100)
)

normalize_weights <- function(w1, w2, w3) {
  total <- w1 + w2 + w3
  if (total == 0) return(c(1/3, 1/3, 1/3))
  c(w1/total, w2/total, w3/total)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Your Priorities"),
      p(em("Drag to weight what matters most to you.")),
      sliderInput("w_afford", "Affordability",
                  min = 0, max = 10, value = 5),
      sliderInput("w_jobs", "Job Market",
                  min = 0, max = 10, value = 5),
      sliderInput("w_home", "Home Value",
                  min = 0, max = 10, value = 5),
      hr(),
      h4("Filters"),
      uiOutput("metro_count"),
      sliderInput("top_pct", "Show top % of metros",
                  min = 0, max = 100, value = 100, step = 5,
                  post = "%"),
      checkboxGroupInput("size", "City Size",
                         choices  = c("Small", "Medium", "Large", "Major"),
                         selected = c("Small", "Medium", "Large", "Major")),
      actionButton("reset", "Reset to Defaults", width = "100%"),
    ),
    mainPanel(
      leafletOutput("map", height = "600px"),
      uiOutput("empty_state")
    )
  )
)

server <- function(input, output, session) {
  
  # Render base map once
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = LNG, lat = LAT, zoom = ZOOM) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(0, 100),
        title = "Weighted Score",
        opacity = 0.7
      )
  })
  
  # Resets all inputs
  observeEvent(input$reset, {
    updateSliderInput(session, "w_afford", value = 5)
    updateSliderInput(session, "w_jobs",   value = 5)
    updateSliderInput(session, "w_home",   value = 5)
    updateSliderInput(session, "top_pct",  value = 100)
    updateCheckboxGroupInput(session, "size",
                             selected = c("Small", "Medium", "Large", "Major"))
    leafletProxy("map") %>% 
      setView(lng = LNG, lat = LAT, zoom = ZOOM)
  })
  
  # Compute composite scores reactively
  scored <- reactive({
    w <- normalize_weights(input$w_afford, input$w_jobs, input$w_home)
    master %>%
      mutate(
        composite_score = w[1]*afford_score + w[2]*job_score + w[3]*homevalue_score
      )
  })
  
  # Filter to top X% and selected sizes
  filtered <- reactive({
    cutoff <- quantile(scored()$composite_score, probs = 1 - input$top_pct/100, na.rm = TRUE)
    scored() %>%
      filter(
        composite_score >= cutoff,
        size_category %in% input$size
      )
  })
  
  # Different state if there are 0 filtered metros
  output$empty_state <- renderUI({
    if (nrow(filtered()) == 0) {
      div(
        style = "text-align: center; padding: 20px; color: #888;",
        h4("No metros match your current filters."),
        p("Try increasing your Top % slider or selecting more city sizes.")
      )
    }
  })
  
  # Displays the amount of filtered metros
  output$metro_count <- renderUI({
    p(em(paste0(nrow(filtered()), " metros shown")))
  })
  
  # Update polygons when filters or weights change
  observeEvent(filtered(), {
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(
        data = filtered(),
        fillColor = ~pal(composite_score),
        fillOpacity = 0.7,
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        layerId = ~GEOID,
        label = ~paste0(NAME, " — ", round(composite_score, 1))
      )
  })
  
  # Render detail Modal
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    metro <- scored() %>%
      filter(GEOID == click$id) %>%
      st_drop_geometry()
    
    showModal(modalDialog(
      title = metro$NAME,
      p(strong("Size: "), metro$size_category),
      p(strong("Weighted Score: "), round(metro$composite_score, 1)),
      p(strong("Affordability Score: "), round(metro$afford_score, 1)),
      p(strong("Job Market Score: "), round(metro$job_score, 1)),
      p(strong("Home Value Score: "), round(metro$homevalue_score, 1)),
      p(strong("Median Rent: "), paste0("$", formatC(metro$median_gross_rent, format="d", big.mark=","))),
      p(strong("Median Income: "), paste0("$", formatC(metro$median_hh_income,  format="d", big.mark=","))),
      p(strong("Median Home Value: "), paste0("$", formatC(metro$median_home_value, format="d", big.mark=","))),
      p(strong("Unemployment: "),
        if (is.na(metro$unemployment_rate)) "N/A"
        else paste0(round(metro$unemployment_rate, 1), "%")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

shinyApp(ui, server)