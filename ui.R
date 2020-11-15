library(shiny)
library(shiny.semantic)
library(leaflet)

# Define UI for application that call longest distance on a sequence of ships movement
shinyUI(semanticPage(

    # Application title
    title = "SHIP DASHBOARD",
    tags$label("ENTER SHIP TYPE AND SHIP NAME"),
    p(),
    htmlOutput("ship_type"),
    htmlOutput("ship_name"),
    br(),
    actionButton("calculate","CALCULATE"),
    p(),
    leafletOutput("mymap"),
    br(),
    tableOutput("table_out")

))
