#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")
library("knitr")
library("leaflet")
library("usmap")
library(maps)
library("mapproj")

library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("fire_interactive_map")
)

server <- function(input, output) {
  output$fire_interactive_map <- renderLeaflet({
    pal <- colorFactor(c("navy", "red", "green"),
      domain = unique(cacopa$subregion)
    )

    leaflet(cacopa) %>%
      addTiles() %>%
      addCircleMarkers(
        color = ~ pal(subregion),
        stroke = FALSE, fillOpacity = 0.5,
        lng = ~long, lat = ~lat,
        label = ~ as.character(paste(
          subregion,
          ", Year of the Fire:", FIRE_YEAR,
          " , Rate of Fire:", Firerate
        ))
      )
  })
}

shinyApp(ui, server)
