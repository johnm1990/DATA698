### DATA698 Final Project
library(shiny)
library(shinydashboard)
library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(treemap)



app_data <- read_csv('https://raw.githubusercontent.com/johnm1990/DATA698/main/NYPD_Shooting_Incident_Data__Historic.csv') %>% 
  mutate(OCCUR_DATE = mdy(OCCUR_DATE),
         type = ifelse(STATISTICAL_MURDER_FLAG == TRUE, 'Murder', 'Non-murder shooting'),
         weekday = wday(OCCUR_DATE, label = TRUE, abbr = FALSE))

nyc_pop <- data.frame(
  BORO = c('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND'),
  pop = c(1472654, 2736074, 1694251, 2405464, 495747)
)

nyc_shp <- read_sf('https://github.com/codeforgermany/click_that_hood/raw/main/public/data/new-york-city-boroughs.geojson') %>% 
  mutate(BORO = toupper(name)) %>% 
  # st_transform(crs = st_crs(4326)) %>% 
  select(name, cartodb_id, BORO) %>% 
  inner_join(nyc_pop)



ui <- fluidPage(
  tags$head(
    tags$style(HTML("/*
 * Component: Info Box
                    * -------------------
                    */
                    .info-box {
                    display: block;
                    min-height: 90px;
                    background: #fff;
                    width: 100%;
                    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
                    border-radius: 2px;
                    margin-bottom: 15px;
                    }
                    .info-box small {
                    font-size: 14px;
                    }
                    .info-box .progress {
                    background: rgba(0, 0, 0, 0.2);
                    margin: 5px -10px 5px -10px;
                    height: 2px;
                    }
                    .info-box .progress,
                    .info-box .progress .progress-bar {
                    border-radius: 0;
                    }
                    .info-box .progress .progress-bar {
                    background: #fff;
                    }
                    .info-box-icon {
                    border-top-left-radius: 2px;
                    border-top-right-radius: 0;
                    border-bottom-right-radius: 0;
                    border-bottom-left-radius: 2px;
                    display: block;
                    float: left;
                    height: 90px;
                    width: 90px;
                    text-align: center;
                    font-size: 45px;
                    line-height: 90px;
                    background: rgba(0, 0, 0, 0.2);
                    }
                    .info-box-icon > img {
                    max-width: 100%;
                    }
                    .info-box-content {
                    padding: 5px 10px;
                    margin-left: 90px;
                    }
                    .info-box-number {
                    display: block;
                    font-weight: bold;
                    font-size: 18px;
                    }
                    .progress-description,
                    .info-box-text {
                    display: block;
                    font-size: 14px;
                    white-space: nowrap;
                    overflow: hidden;
                    text-overflow: ellipsis;
                    }
                    .info-box-text {
                    text-transform: uppercase;
                    }
                    .info-box-more {
                    display: block;
                    }
                    .progress-description {
                    margin: 0;
                    }
                    .bg-yellow,
                    .callout.callout-warning,
                    .alert-warning,
                    .label-warning,
                    .modal-warning .modal-body {
                      background-color: #f39c12 !important;
                    }
                    "))
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput('select_year', 'Select a year (2006-2020)',
                   value = 2020, max = 2020, min = 2006),
      selectInput('select_incident', 'Select incident',
                  choices = c('All incidents', 'Murder' = TRUE,
                              'Non-murder shooting' = FALSE), selected = 'All incidents'),
      selectInput('select_map', 'Select map type',
                  choices = c('Boroughs', 'Incidents'))
      
    ),
    mainPanel(
      # tableOutput('testtable'),
      leafletOutput('map1'),
      fluidRow(
        infoBoxOutput('tot_inc', width = 6),
        infoBoxOutput('pct_change', width = 6)
        
      ),
      plotOutput('heatmap')
      # tableOutput('testtable')
    )
  )
)

server <- function(input, output){
  
  map_data <- reactive({
    app_data %>% 
      filter(year(OCCUR_DATE) == input$select_year | year(OCCUR_DATE) == input$select_year -1,
             if(input$select_incident == 'All incidents') TRUE else STATISTICAL_MURDER_FLAG == input$select_incident)
  })
  
  w <- reactive({
    
    map_data() %>% 
      filter(year(OCCUR_DATE) == input$select_year) %>% 
      count(weekday) %>% 
      mutate(pct = n/sum(n) *100,
             pct_format = paste0(round(pct, 1), '%'), 
             full_data = paste(weekday, '\n', pct_format))
    
  })
  
  output$heatmap <- renderPlot({
    
    treemap(w(), index = c('full_data'), vSize = 'pct',
            title = 'Days of the week heatmap')
    
  })
  
  
  tot_inc1 <- reactive({
    dim(map_data() %>% 
          filter(year(OCCUR_DATE) == input$select_year))[[1]]
    
  })
  
  pct_change_data <- reactive({
    map_data() %>% 
      count(year(OCCUR_DATE)) %>% 
      spread(1,2) %>% 
      mutate(change = round(((.[[2]]-.[[1]])/.[[1]]*100), 1)) %>% 
      select(change) %>% pull()
  })
  
  output$testtable <- renderTable({
    tot_inc1()
  })
  
  output$tot_inc <- renderInfoBox({
    infoBox(
      "Total number of incidents", tot_inc1(), icon = icon("bar-chart"),
      color = "yellow"
    )
  })
  
  output$pct_change <- renderInfoBox({
    infoBox(
      "Percent change vs. previous year", paste0(pct_change_data(), '%'), icon = icon("line-chart"),
      color = "yellow"
    )
  })
  
  
  point_data <- reactive({
    
    map_data() %>% 
      filter(year(OCCUR_DATE) == input$select_year) %>% 
      st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)
    
  })
  
  borough_summaries <- reactive({
    
    map_data() %>% 
      filter(year(OCCUR_DATE) == input$select_year) %>% 
      count(BORO)  
    
  })
  
  nyc_shp_shiny <- reactive({
    
    nyc_shp %>% 
      inner_join(borough_summaries()) %>% 
      mutate(`Shooting rate` = n/pop*100000)
    
  })
  
  output$map1 <- renderLeaflet({
    
    leaflet() %>%         #set up initial leaflet map
      addTiles() %>% 
      setView(lng = -74.006, lat = 40.713, zoom = 10) 
    
  })
  
  observe({
    
    if(input$select_map == 'Boroughs') {
      
      pal <- colorNumeric('YlOrRd', domain = nyc_shp_shiny()$`Shooting rate`)
      
      leafletProxy('map1', data = nyc_shp_shiny()) %>% 
        clearMarkers() %>% 
        clearShapes() %>% 
        clearControls() %>% 
        # setView(lng = -74.006, lat = 40.713, zoom = 10) %>% 
        addPolygons(fillColor = ~pal(`Shooting rate`), fillOpacity = .9,
                    label = ~paste(name, round(`Shooting rate`))) %>% 
        addLegend(pal = pal,
                  values = ~`Shooting rate`)
      
      
      
      
      
    } else if(input$select_map == 'Incidents'){
      
      
      pal <- colorFactor(c("red", "navy"), domain = c("Non-murder shooting", "Murder"))
      
      leafletProxy('map1', data = point_data()) %>% 
        clearMarkers() %>% 
        clearShapes() %>% 
        clearControls() %>% 
        addCircleMarkers(
          radius = ~ifelse(type == "Murder", 10, 6),
          color = ~pal(type),
          stroke = FALSE, fillOpacity = 0.5,
          label = ~type
        ) %>% 
        addLegend(pal = pal,
                  values = ~type)
      
    }
    
  })
  
}

shinyApp(ui, server)
