library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)
library(styler)
library(tidyr)
library(plotly)

asthma_data <- read.csv("summarized_asthma_data2.csv", stringsAsFactors = F)
years <- unique(asthma_data$Year)
county_names <- unique(asthma_data$County)

shinyUI(fluidPage(
  h1("Test"),
  sidebarLayout(
    sidebarPanel(
  
  selectInput(
    "state1_choice",
    "Select 1st County to Observe",
    choices = county_names
  ),
  
  checkboxInput(
    "dt1r",
    "See Table",
    value = F
  ),
  
  selectInput(
    "state2_choice",
    "Select 1st County to Observe",
    choices = county_names
  ),
  
  checkboxInput(
    "dt2r",
    "See Table",
    value = F
  ),
  
  #I'm hopefully going to eventually have a button to merge the two plots
  # checkboxInput(
  #   "merge",
  #   strong("Join the Plots"),
  #   value = F
  # ),
  tableOutput("ct1"),
  
  tableOutput("ct2")
  ),
  mainPanel(
  plotlyOutput("county1", width = "85%", height = "400px"),
  plotlyOutput("county2", width = "85%", height = "400px")
  )
)
))
