library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

LNG <- -98.5
LAT <- 39.5
ZOOM <- 4
TOP_N <- 15

master <- readRDS("data/master_metros.rds")

# --- Parse state abbreviation and assign US region ---
master <- master %>%
  mutate(
    state_abbr = str_extract(NAME, "[A-Z]{2}(?=-| Metro| Micro|$)"),
    region = case_when(
      state_abbr %in% c("CT","ME","MA","NH","RI","VT","NJ","NY","PA") ~ "Northeast",
      state_abbr %in% c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD") ~ "Midwest",
      state_abbr %in% c("DE","FL","GA","MD","NC","SC","VA","WV","DC",
                        "AL","KY","MS","TN","AR","LA","OK","TX") ~ "South",
      state_abbr %in% c("AZ","CO","ID","MT","NV","NM","UT","WY",
                        "AK","CA","HI","OR","WA") ~ "West",
      TRUE ~ "Other"
    )
  )

pal <- colorNumeric(
  palette = "YlGn",
  domain  = c(0, 100)
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
      sliderInput("w_afford", "Affordability", min=0, max=10, value=5),
      sliderInput("w_jobs",   "Job Market",    min=0, max=10, value=5),
      sliderInput("w_home",   "Home Value",    min=0, max=10, value=5),
      hr(),
      h4("Filters"),
      uiOutput("metro_count"),
      selectInput("region", "Region",
                  choices  = c("All regions", "Northeast", "Midwest", "South", "West"),
                  selected = "All regions"),
      selectInput("state", "State",
                  choices  = c("All states"),
                  selected = "All states"),
      checkboxGroupInput("size", "City Size",
                         choices  = c("Small","Medium","Large","Major"),
                         selected = c("Small","Medium","Large","Major")),
      hr(),
      checkboxInput("show_all", "Show all metros", value=FALSE),
      actionButton("reset", "Reset to Defaults", width="100%")
    ),
    mainPanel(
      leafletOutput("map", height="600px"),
      uiOutput("empty_state")
    )
  )
)

server <- function(input, output, session) {
  
  # Update state dropdown when region changes
  observeEvent(input$region, {
    if (input$region == "All regions") {
      states <- sort(unique(master$state_abbr))
    } else {
      states <- sort(unique(master$state_abbr[master$region == input$region]))
    }
    updateSelectInput(session, "state",
                      choices  = c("All states", states),
                      selected = "All states")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng=LNG, lat=LAT, zoom=ZOOM)
  })
  
  scored <- reactive({
    w <- normalize_weights(input$w_afford, input$w_jobs, input$w_home)
    master %>%
      mutate(composite_score = w[1]*afford_score + w[2]*job_score + w[3]*homevalue_score)
  })
  
  # All metros passing sidebar filters (region, state, size) — no cap yet
  pool <- reactive({
    df <- scored()
    if (input$region != "All regions") df <- df %>% filter(region == input$region)
    if (!is.null(input$state) && input$state != "All states")
      df <- df %>% filter(state_abbr == input$state)
    df %>% filter(size_category %in% input$size)
  })
  
  # Top N of pool, or full pool when show_all is checked
  filtered <- reactive({
    df <- pool() %>% arrange(desc(composite_score))
    if (!input$show_all) df <- df %>% slice_head(n = TOP_N)
    df
  })
  
  observeEvent(list(filtered(), input$show_all), {
    proxy <- leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% clearControls()
    
    df <- filtered()
    if (nrow(df) == 0) return()
    
    if (input$show_all) {
      # Choropleth polygon mode
      proxy %>%
        addPolygons(
          data        = df,
          fillColor   = ~pal(composite_score),
          fillOpacity = 0.7,
          color       = "#444444",
          weight      = 1,
          smoothFactor = 0.5,
          layerId     = ~GEOID,
          label       = ~paste0(NAME, " — ", round(composite_score, 1))
        ) %>%
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = c(0, 100),
          title    = "Weighted Score",
          opacity  = 0.7
        )
    } else {
      # Ranked numbered marker mode
      centroids <- df %>%
        mutate(
          rank = row_number(),
          cx   = map_dbl(geometry, ~st_centroid(.)[[1]]),
          cy   = map_dbl(geometry, ~st_centroid(.)[[2]])
        ) %>%
        st_drop_geometry()
      
      proxy %>%
        addPolygons(
          data        = df,
          fillColor   = "#1D9E75",
          fillOpacity = 0.08,
          color       = "#1D9E75",
          weight      = 1,
          smoothFactor = 0.5,
          layerId     = ~paste0("poly_", GEOID),
          label       = ~paste0(NAME, " — ", round(composite_score, 1))
        ) %>%
        addCircleMarkers(
          data        = centroids,
          lng         = ~cx,
          lat         = ~cy,
          radius      = 14,
          color       = "white",
          weight      = 2,
          fillColor   = "#1D9E75",
          fillOpacity = 1,
          layerId     = ~GEOID,
          label       = ~paste0("#", rank, " ", NAME, " — ", round(composite_score, 1)),
          labelOptions = labelOptions(
            style = list("font-weight" = "bold")
          )
        ) %>%
        addLabelOnlyMarkers(
          data        = centroids,
          lng         = ~cx,
          lat         = ~cy,
          label       = ~as.character(rank),
          labelOptions = labelOptions(
            noHide    = TRUE,
            direction = "center",
            textOnly  = TRUE,
            style     = list(
              "color"       = "white",
              "font-weight" = "bold",
              "font-size"   = "12px"
            )
          )
        )
    }
  })
  
  # Click handler — works for both numbered markers and choropleth polygons
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    show_detail(click$id)
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    id <- gsub("^poly_", "", click$id)
    show_detail(id)
  })
  
  show_detail <- function(geoid) {
    metro <- scored() %>%
      filter(GEOID == geoid) %>%
      st_drop_geometry()
    if (nrow(metro) == 0) return()
    showModal(modalDialog(
      title    = metro$NAME,
      p(strong("Size: "),             metro$size_category),
      p(strong("Region: "),           metro$region),
      p(strong("Weighted Score: "),  round(metro$composite_score, 1)),
      p(strong("Affordability: "),    round(metro$afford_score, 1)),
      p(strong("Job Market: "),       round(metro$job_score, 1)),
      p(strong("Home Value: "),       round(metro$homevalue_score, 1)),
      p(strong("Median Rent: "),      paste0("$", formatC(metro$median_gross_rent, format="d", big.mark=","))),
      p(strong("Median Income: "),    paste0("$", formatC(metro$median_hh_income,  format="d", big.mark=","))),
      p(strong("Median Home Value: "),paste0("$", formatC(metro$median_home_value, format="d", big.mark=","))),
      p(strong("Unemployment: "),
        if (is.na(metro$unemployment_rate)) "N/A"
        else paste0(round(metro$unemployment_rate, 1), "%")),
      easyClose = TRUE,
      footer    = modalButton("Close")
    ))
  }
  
  output$metro_count <- renderUI({
    n_shown  <- nrow(filtered())
    n_pool   <- nrow(pool())
    if (input$show_all) {
      msg <- paste0("Showing all ", n_shown, " metros")
    } else {
      msg <- paste0("Showing top ", n_shown, " of ", n_pool, " matching metros")
    }
    p(em(msg), style="color:#888; font-size:12px; margin-top:4px;")
  })
  
  output$empty_state <- renderUI({
    if (nrow(filtered()) == 0) {
      div(
        style="text-align:center; padding:20px; color:#888;",
        h4("No metros match your current filters."),
        p("Try selecting a different region or more city sizes.")
      )
    }
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "w_afford", value=5)
    updateSliderInput(session, "w_jobs",   value=5)
    updateSliderInput(session, "w_home",   value=5)
    updateSelectInput(session, "region",   selected="All regions")
    updateSelectInput(session, "state",    selected="All states")
    updateCheckboxGroupInput(session, "size",
                             selected=c("Small","Medium","Large","Major"))
    updateCheckboxInput(session, "show_all", value=FALSE)
    leafletProxy("map") %>% 
      setView(lng = LNG, lat = LAT, zoom = ZOOM)
  })
}

shinyApp(ui, server)