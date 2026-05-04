library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(plotly)

LNG <- -98.5
LAT <- 39.5
ZOOM <- 4
TOP_N <- 15
QUIZ_TOP_N <- 5

master <- readRDS("data/master_metros.rds")

# Cleanup abbreviations
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

# Color palette
pal <- colorNumeric(
  palette = "YlGn",
  domain = c(0, 100)
)

normalize_weights <- function(w1, w2, w3) {
  total <- w1 + w2 + w3
  if (total == 0) return(c(1/3, 1/3, 1/3))
  c(w1/total, w2/total, w3/total)
}

fema_label <- function(score) {
  case_when(
    score >= 75 ~ "Low",
    score >= 50 ~ "Moderate",
    score >= 25 ~ "High",
    TRUE ~ "Very High"
  )
}

# Quiz weight deltas — how much each answer adjusts each dimension
# Each answer is (housing_delta, jobs_delta, fema_delta)
quiz_weights <- list(
  q1 = list(
    a = c(-1, 3, 0),   # Career opportunity
    b = c(3, -1, 0),   # More affordable
    c = c(3, -1, 0),   # Buy a home
    d = c(0, 0, 0)     # No preference
  ),
  q2 = list(
    a = c(3, -1, 0),   # Keep monthly bills low
    b = c(3, -1, 0),   # Invest in a home
    c = c(-1, 3, 0),   # Career can grow
    d = c(0, 0, 0)     # No preference
  ),
  q3 = list(
    a = c(-1, -1, 4),  # A lot
    b = c(0, 0, 2),    # Somewhat
    c = c(0, 0, 0),    # No preference
    d = c(0, 0, -3)    # Not at all
  )
)

# Available metros for comparison
metro_choices <- master %>%
  st_drop_geometry() %>%
  arrange(NAME) %>%
  pull(NAME)

ui <- fluidPage(
  title = "Waypoint",
  titlePanel("Waypoint — Find Your Next City"),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Explore",
      sidebarLayout(
        sidebarPanel(
          h4("Your Priorities"),
          p(em("Drag to weight what matters most to you.")),
          sliderInput("w_housing", "Housing Affordability", min=0, max=10, value=5),
          sliderInput("w_jobs", "Job Market", min=0, max=10, value=5),
          sliderInput("w_fema", "Natural Hazard Safety", min=0, max=10, value=5),
          hr(),
          h4("Filters"),
          uiOutput("metro_count"),
          selectInput("region", "Region",
                      choices = c("All regions", "Northeast", "Midwest", "South", "West"),
                      selected = "All regions"),
          selectInput("state", "State",
                      choices = c("All states"),
                      selected = "All states"),
          checkboxGroupInput("size", "City Size",
                             choices = c("Small","Medium","Large","Major"),
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
    ),
    tabPanel(
      "Find My City",
      fluidRow(
        column(
          width = 6,
          offset = 3,
          br(),
          h3("Find My City"),
          p("Answer a few questions and we'll find your best matches."),
          hr(),
          radioButtons("q1", "What's bringing you to a new city?",
                       selected = character(0),
                       choices = c(
                         "Chasing a new career opportunity" = "a",
                         "A fresh start somewhere more affordable" = "b",
                         "Ready to put down roots and buy a home" = "c",
                         "No preference, just exploring my options" = "d"
                       )),
          hr(),
          radioButtons("q2", "How do you think about where your money goes?",
                       selected = character(0),
                       choices = c(
                         "I want to keep my monthly bills as low as possible" = "a",
                         "I'd rather invest in a home I can own" = "b",
                         "I want to be somewhere my career can grow" = "c",
                         "No preference" = "d"
                       )),
          hr(),
          radioButtons("q3", "How much does living somewhere safe from things like floods, wildfires, or hurricanes matter to you?",
                       selected = character(0),
                       choices = c(
                         "A lot, it's one of my top priorities" = "a",
                         "Somewhat, I'd prefer lower risk if possible" = "b",
                         "No preference, I'm open to anywhere" = "c",
                         "Not at all, it's not something I think about" = "d"
                       )),
          hr(),
          radioButtons("q4", "What kind of city feels like home to you?",
                       selected = character(0),
                       choices = c(
                         "Somewhere small and close-knit" = "Small",
                         "A mid-size city with room to grow" = "Medium",
                         "A big city with lots going on" = "Large",
                         "A major metro, the bigger the better" = "Major",
                         "No preference, I'm open to anything" = "all"
                       )),
          hr(),
          radioButtons("q5", "Is there a part of the country you're drawn to?",
                       selected = character(0),
                       choices = c(
                         "Northeast" = "Northeast",
                         "Midwest" = "Midwest",
                         "South" = "South",
                         "West" = "West",
                         "No preference, anywhere works for me" = "all"
                       )),
          hr(),
          actionButton("submit_quiz", "Find My City", class="btn-primary", width="100%"),
          br(), br()
        )
      )
    ),
    tabPanel(
      "Compare",
      br(),
      fluidRow(
        column(
          width = 4,
          selectInput("cmp1", "City 1",
                      choices = c("Select a city" = "", metro_choices),
                      selected = "",
                      selectize = TRUE)
        ),
        column(
          width = 4,
          selectInput("cmp2", "City 2",
                      choices = c("Select a city" = "", metro_choices),
                      selected = "",
                      selectize = TRUE)
        ),
        column(
          width = 4,
          selectInput("cmp3", "City 3 (optional)",
                      choices = c("None" = "", metro_choices),
                      selected = "",
                      selectize = TRUE)
        )
      ),
      uiOutput("compare_prompt"),
      uiOutput("compare_cards"),
      plotlyOutput("compare_chart", height="400px")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$region, {
    if (input$region == "All regions") {
      states <- sort(unique(master$state_abbr))
    } else {
      states <- sort(unique(master$state_abbr[master$region == input$region]))
    }
    updateSelectInput(session, "state",
                      choices = c("All states", states),
                      selected = "All states")
  })
  
  # Renders map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng=LNG, lat=LAT, zoom=ZOOM)
  })
  
  # Computes weighted/composite score
  scored <- reactive({
    w <- normalize_weights(input$w_housing, input$w_jobs, input$w_fema)
    master %>%
      mutate(composite_score = w[1]*housing_score + w[2]*job_score + w[3]*fema_score)
  })
  
  # Applies filters
  pool <- reactive({
    df <- scored()
    if (input$region != "All regions") df <- df %>% filter(region == input$region)
    if (!is.null(input$state) && input$state != "All states")
      df <- df %>% filter(state_abbr == input$state)
    df %>% filter(size_category %in% input$size)
  })
  
  # Gets filtered data
  filtered <- reactive({
    df <- pool() %>% arrange(desc(composite_score))
    if (!input$show_all) df <- df %>% slice_head(n = TOP_N)
    df
  })
  
  # Draws the leaflet map with outlines or centroids
  draw_map <- function(df, show_all) {
    proxy <- leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% clearControls()
    if (nrow(df) == 0) return()
    if (show_all) {
      proxy %>%
        addPolygons(
          data = df,
          fillColor = ~pal(composite_score),
          fillOpacity = 0.7,
          color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          layerId = ~GEOID,
          label = ~paste0(NAME, " -- ", round(composite_score, 1))
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = c(0, 100),
          title = "Weighted Score",
          opacity = 0.7
        )
    } else {
      centroids <- df %>%
        mutate(
          rank = row_number(),
          cx = map_dbl(geometry, ~st_centroid(.)[[1]]),
          cy = map_dbl(geometry, ~st_centroid(.)[[2]])
        ) %>%
        st_drop_geometry()
      proxy %>%
        addPolygons(
          data = df,
          fillColor = "#1D9E75",
          fillOpacity = 0.08,
          color = "#1D9E75",
          weight = 1,
          smoothFactor = 0.5,
          layerId = ~paste0("poly_", GEOID),
          label = ~paste0(NAME, " -- ", round(composite_score, 1))
        ) %>%
        addCircleMarkers(
          data = centroids,
          lng = ~cx,
          lat = ~cy,
          radius = 14,
          color = "white",
          weight = 2,
          fillColor = "#1D9E75",
          fillOpacity = 1,
          layerId = ~GEOID,
          label = ~paste0("#", rank, " ", NAME, " -- ", round(composite_score, 1)),
          labelOptions = labelOptions(
            style = list("font-weight" = "bold")
          )
        ) %>%
        addLabelOnlyMarkers(
          data = centroids,
          lng = ~cx,
          lat = ~cy,
          label = ~as.character(rank),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "white",
              "font-weight" = "bold",
              "font-size" = "12px"
            )
          )
        )
    }
  }
  
  observeEvent(list(filtered(), input$show_all), {
    draw_map(filtered(), input$show_all)
  })
  
  # Quiz submission
  observeEvent(input$submit_quiz, {
    # Validate all questions answered
    unanswered <- c(
      if (is.null(input$q1) || length(input$q1) == 0) "Question 1",
      if (is.null(input$q2) || length(input$q2) == 0) "Question 2",
      if (is.null(input$q3) || length(input$q3) == 0) "Question 3",
      if (is.null(input$q4) || length(input$q4) == 0) "Question 4",
      if (is.null(input$q5) || length(input$q5) == 0) "Question 5"
    )
    if (length(unanswered) > 0) {
      showModal(modalDialog(
        title = "Almost there!",
        p(paste0("Please answer ", paste(unanswered, collapse=", "), " before continuing.")),
        easyClose = TRUE,
        footer = modalButton("Got it")
      ))
      return()
    }

    w_housing <- 5
    w_jobs <- 5
    w_fema <- 5
    
    # Applies all questions weights
    # Q1
    delta <- quiz_weights$q1[[input$q1]]
    w_housing <- w_housing + delta[1]
    w_jobs <- w_jobs + delta[2]
    w_fema <- w_fema + delta[3]
    
    # Q2
    delta <- quiz_weights$q2[[input$q2]]
    w_housing <- w_housing + delta[1]
    w_jobs <- w_jobs + delta[2]
    w_fema <- w_fema + delta[3]
    
    # Q3
    delta <- quiz_weights$q3[[input$q3]]
    w_housing <- w_housing + delta[1]
    w_jobs <- w_jobs + delta[2]
    w_fema <- w_fema + delta[3]
    
    # Min of 0 for weights
    w_housing <- max(0, w_housing)
    w_jobs <- max(0, w_jobs)
    w_fema <- max(0, w_fema)
    
    # Compute scores using quiz weights
    w <- normalize_weights(w_housing, w_jobs, w_fema)
    quiz_scored <- master %>%
      mutate(composite_score = w[1]*housing_score + w[2]*job_score + w[3]*fema_score)
    
    # Q4 size filter
    if (input$q4 != "all") {
      quiz_scored <- quiz_scored %>% filter(size_category == input$q4)
    }
    
    # Q5 region filter
    if (input$q5 != "all") {
      quiz_scored <- quiz_scored %>% filter(region == input$q5)
    }
    
    # Update sliders with weights
    updateSliderInput(session, "w_housing", value=round(w_housing))
    updateSliderInput(session, "w_jobs", value=round(w_jobs))
    updateSliderInput(session, "w_fema", value=round(w_fema))
    
    # Update region and size filters
    if (input$q5 != "all") {
      updateSelectInput(session, "region", selected=input$q5)
    } else {
      updateSelectInput(session, "region", selected="All regions")
    }
    
    if (input$q4 != "all") {
      updateCheckboxGroupInput(session, "size", selected=input$q4)
    } else {
      updateCheckboxGroupInput(session, "size",
                               selected=c("Small","Medium","Large","Major"))
    }
    
    updateTabsetPanel(session, "main_tabs", selected="Explore")
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    show_detail(click$id)
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    id <- gsub("^poly_", "", click$id)
    show_detail(id)
  })
  
  # Popup modal for more info
  show_detail <- function(geoid) {
    metro <- scored() %>%
      filter(GEOID == geoid) %>%
      st_drop_geometry()
    if (nrow(metro) == 0) return()
    showModal(modalDialog(
      title = metro$NAME,
      p(strong("Size: "), metro$size_category),
      p(strong("Region: "), metro$region),
      p(strong("Weighted Score: "), round(metro$composite_score, 1)),
      hr(),
      p(strong("Housing Score: "), round(metro$housing_score, 1)),
      p(strong("Job Market Score: "), round(metro$job_score, 1)),
      p(strong("Hazard Risk: "), fema_label(metro$fema_score)),
      hr(),
      p(strong("Median Rent: "), paste0("$", formatC(metro$median_gross_rent, format="d", big.mark=","))),
      p(strong("Median Income: "), paste0("$", formatC(metro$median_hh_income, format="d", big.mark=","))),
      p(strong("Median Home Value: "), paste0("$", formatC(metro$median_home_value, format="d", big.mark=","))),
      p(strong("Unemployment: "),
        if (is.na(metro$unemployment_rate)) "N/A"
        else paste0(round(metro$unemployment_rate, 1), "%")),
      p(strong("Population Growth: "),
        if (is.na(metro$pop_growth_pct)) "N/A"
        else paste0(round(metro$pop_growth_pct, 1), "%")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }
  
  # Comparison tab logic
  cmp_data <- reactive({
    selected <- c(input$cmp1, input$cmp2, input$cmp3)
    selected <- selected[selected != ""]
    if (length(selected) < 2) return(NULL)
    master %>%
      st_drop_geometry() %>%
      filter(NAME %in% selected) %>%
      arrange(match(NAME, selected))
  })
  
  output$compare_prompt <- renderUI({
    if (is.null(cmp_data())) {
      div(
        style="text-align:center; padding:40px; color:#888;",
        p("Select at least 2 cities above to compare them.")
      )
    }
  })
  
  # Shows details for each metro
  output$compare_cards <- renderUI({
    df <- cmp_data()
    if (is.null(df)) return(NULL)
    cols <- lapply(seq_len(nrow(df)), function(i) {
      m <- df[i, ]
      column(
        width = floor(12 / nrow(df)),
        div(
          style="border:1px solid #ddd; border-radius:8px; padding:16px; margin-bottom:16px;",
          h4(m$NAME, style="margin-top:0;"),
          p(strong("Region: "), m$region),
          p(strong("Size: "), as.character(m$size_category)),
          hr(),
          p(strong("Housing Score: "), round(m$housing_score, 1)),
          p(strong("Job Market Score: "), round(m$job_score, 1)),
          p(strong("Hazard Risk: "), fema_label(m$fema_score)),
          hr(),
          p(strong("Median Rent: "), paste0("$", formatC(m$median_gross_rent, format="d", big.mark=","))),
          p(strong("Median Income: "), paste0("$", formatC(m$median_hh_income, format="d", big.mark=","))),
          p(strong("Median Home Value: "), paste0("$", formatC(m$median_home_value, format="d", big.mark=","))),
          p(strong("Unemployment: "),
            if (is.na(m$unemployment_rate)) "N/A"
            else paste0(round(m$unemployment_rate, 1), "%")),
          p(strong("Population Growth: "),
            if (is.na(m$pop_growth_pct)) "N/A"
            else paste0(round(m$pop_growth_pct, 1), "%"))
        )
      )
    })
    do.call(fluidRow, cols)
  })
  
  # Grouped bar chart for comparison
  output$compare_chart <- renderPlotly({
    df <- cmp_data()
    if (is.null(df)) return(NULL)
    
    colors <- c("#1D9E75", "#3478c5", "#e07b39")
    
    fig <- plot_ly()
    for (i in seq_len(nrow(df))) {
      m <- df[i, ]
      fig <- fig %>%
        add_trace(
          type = "bar",
          name = m$NAME,
          x = c("Housing", "Job Market", "Hazard Safety"),
          y = c(round(m$housing_score, 1),
                round(m$job_score, 1),
                round(m$fema_score, 1)),
          marker = list(color=colors[i])
        )
    }
    fig %>%
      layout(
        barmode = "group",
        yaxis = list(title="Score (0-100)", range=c(0, 100)),
        xaxis = list(title=""),
        legend = list(orientation="h", y=-0.2),
        margin = list(t=20, b=60)
      )
  })
  
  # Helper to show how many results there are
  output$metro_count <- renderUI({
    n_shown <- nrow(filtered())
    n_pool <- nrow(pool())
    if (input$show_all) {
      msg <- paste0("Showing all ", n_shown, " metros")
    } else {
      msg <- paste0("Showing top ", n_shown, " of ", n_pool, " matching metros")
    }
    p(em(msg), style="color:#888; font-size:12px; margin-top:4px;")
  })
  
  # If filters result in 0 matches
  output$empty_state <- renderUI({
    if (nrow(filtered()) == 0) {
      div(
        style="text-align:center; padding:20px; color:#888;",
        h4("No metros match your current filters."),
        p("Try selecting a different region or more city sizes.")
      )
    }
  })
  
  # Resets buttons
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "Find My City") {
      updateRadioButtons(session, "q1", selected=character(0))
      updateRadioButtons(session, "q2", selected=character(0))
      updateRadioButtons(session, "q3", selected=character(0))
      updateRadioButtons(session, "q4", selected=character(0))
      updateRadioButtons(session, "q5", selected=character(0))
    }
  })
  
  # Resets sliders and filters
  observeEvent(input$reset, {
    updateSliderInput(session, "w_housing", value=5)
    updateSliderInput(session, "w_jobs", value=5)
    updateSliderInput(session, "w_fema", value=5)
    updateSelectInput(session, "region", selected="All regions")
    updateSelectInput(session, "state", selected="All states")
    updateCheckboxGroupInput(session, "size",
                             selected=c("Small","Medium","Large","Major"))
    updateCheckboxInput(session, "show_all", value=FALSE)
    leafletProxy("map") %>%
      setView(lng=LNG, lat=LAT, zoom=ZOOM)
  })
}

shinyApp(ui, server)