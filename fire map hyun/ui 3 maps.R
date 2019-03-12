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


# in ui
#leafletOutput(outputId = "fire_map")

# in server()
# output$map 

ui <- shinyUI(fluidPage(
  titlePanel("Various Maps of California Wild Fires"),
  #mainPanel("The three maps show fire severity over time",
  fluidRow(
    splitLayout(cellWidths = c("40%", "40%", "40%"), 
      plotOutput("fire_map_2005"), 
      plotOutput("fire_map_2010"), 
      plotOutput("fire_map_2015"))
      )
    #)
  )
)






