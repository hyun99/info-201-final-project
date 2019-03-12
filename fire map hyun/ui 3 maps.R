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


library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("title panel"),

                mainPanel("main panel",
                          fluidRow(
                            splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                        plotOutput("fire_map_2012"), 
                                        plotOutput("fire_map_2014"), 
                                        plotOutput("fire_map_2016"))
                          )
                )
  )
)





