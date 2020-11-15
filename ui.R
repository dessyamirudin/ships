
library(shiny)
library(shiny.semantic)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(semanticPage(

    # Application title
    titlePanel("Ships Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        
        sidebarPanel(
            htmlOutput("ship_type"),
            htmlOutput("ship_name"),
            p(),
            actionButton("calculate","CALCULATE")
            ),
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("mymap"),
            tableOutput("table_out")
        )
    )
))
