library(shiny)
library(tidyverse)
library(geosphere)
library(purrr)
library(feather)
library(lubridate)

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

# Define server logic
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
    if(is.null(input$ship_type)){
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
  
  # initiate data to draw map with reactiveVal
  data_selected <- reactiveVal(
    data
  )
  
  # observe trigger event to update the initial map
  observeEvent(input$calculate,{
      if (input$ship_type == "" | input$ship_name ==""){
        create_modal(modal(
          id = "input-error",
          header = h2("INPUT ERROR"),
          "Please select SHIP TYPE and SHIP NAME"
        ))
        
        data_selected(data)
        
      } else {
        data = data %>% filter(ship_type == .env$input$ship_type & SHIPNAME == .env$input$ship_name) %>% 
          mutate(DATETIME=ymd_hms(DATETIME)) %>%
          # sort the data
          arrange(SHIP_ID,DATETIME) %>% 
          # preparing additional variable
          mutate(LON_lag = lag(LON)) %>% 
          mutate(LAT_lag = lag(LAT)) %>% 
          mutate(DATETIME_lag = lag(DATETIME)) %>% 
          # measure the distance
          mutate(distance = get_geo_distance(LON,LAT,LON_lag,LAT_lag))
        
        data_selected(data)
      }
  })
  
  
  # initiate longest distance final calculation using reactiveVal
  # initiating default value
  data_max_initiate <- data.frame(
    LON = numeric(),
    LAT = numeric(),
    SHIPNAME = character(),
    ship_type = character(),
    LON_lag = numeric(),
    LAT_lag = numeric(),
    distance = numeric(),
    DATETIME = as.Date(character()),
    DATETIME_lag = as.Date(character()))
  
  longest_distance_calc <- reactiveVal(
    data_max_initiate
  )
  
  # update maximum distance value
  observeEvent(input$calculate,{
    if (input$ship_type == "" | input$ship_name ==""){
      longest_distance_calc(data_max_initiate)
    } else {
      data_max = data_selected() %>% filter(distance == max(distance,na.rm=T)) %>% tail(1) %>% 
        select(LON,LAT,SHIPNAME,ship_type,LON_lag,LAT_lag,distance,DATETIME,DATETIME_lag)
      longest_distance_calc(data_max)
    } 
  })
  
  # Map Output
  output$mymap <- renderLeaflet({
    data_selected <- longest_distance_calc()
    
    LON = c(data_selected$LON,data_selected$LON_lag)
    LAT = c(data_selected$LAT,data_selected$LAT_lag)
    Loc = c("End Point","Starting Point")
    
    data_plot_map = data.frame(LON,LAT)
    
    leaflet(data_selected()) %>% addTiles() %>%
      addMarkers(data = data_plot_map) %>% 
      fitBounds(~min(LON)-0.005, ~min(LAT)-0.005, ~max(LON)+0.005, ~max(LAT)+0.005)
  })
  
  output$table_out <- renderTable({
    longest_distance_calc() %>% select(LON,LAT,SHIPNAME,ship_type,distance) %>% as_tibble %>% rename_with(toupper)
  })
})
