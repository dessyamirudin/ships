library(shiny)
library(tidyverse)
library(geosphere)
library(purrr)
library(feather)

# load data
# data = readRDS("data/data.RDS")
data = feather::read_feather("data/data.feather")

# function to measure distance with input as vector
get_geo_distance = function(long1, lat1, long2, lat2) {
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = unlist(distance_list)
  
  distance_m
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Select Ship Type
  output$ship_type <- renderUI({
    selectInput(
      inputId = "ship_type", 
      label = "SHIP TYPE:",
      choices = c("",sort(unique(data$ship_type))) #,
      #selected = sort(unique(data$ship_type))[1]
      )
    
  })
  
  # Select Ship Name
  output$ship_name <- renderUI({
    if(input$ship_type ==""){
      selectInput(
        inputId = "ship_name", 
        label = "SHIP NAME:",
        choices = ""
      )
    } else {
      available_choices <- data %>% filter(ship_type==input$ship_type) %>% select(SHIPNAME)
      
      selectInput(
        inputId = "ship_name", 
        label = "SHIP NAME:",
        choices = sort(unique(available_choices$SHIPNAME))
      )
    }
  })
  
  # reactive measure distance
  longest_distance <- eventReactive(input$calculate,{
    # filter the data
    data = data %>% filter(ship_type == .env$input$ship_type & SHIPNAME == .env$input$ship_name) %>% 
      # sort the data
      arrange(SHIP_ID,DATETIME) %>% 
      # preparing additional variable
      mutate(LON_lag = lag(LON)) %>% 
      mutate(LAT_lag = lag(LAT)) %>% 
      # measure the distance
      mutate(distance = get_geo_distance(LON,LAT,LON_lag,LAT_lag))
    
    data_max = data %>% filter(distance == max(distance,na.rm=T)) %>% tail(1) %>% 
      select(LON,LAT,SHIPNAME,ship_type,SHIPNAME,LON_lag,LAT_lag,distance)
  })
  
  # Map Output
  output$mymap <- renderLeaflet({
    data_selected <- longest_distance()
    
    LON = c(data_selected$LON,data_selected$LON_lag)
    LAT = c(data_selected$LAT,data_selected$LAT_lag)
    Loc = c("End Point","Starting Point")
    
    data_plot_map = data.frame(LON,LAT)

    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = data_plot_map)
  })
  
  output$table_out <- renderTable({
    longest_distance() %>% select(LON,LAT,SHIPNAME,ship_type,distance) %>% as_tibble %>% rename_with(toupper)
  })

})
