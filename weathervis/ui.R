#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scatterD3)
library(rsconnect)

choices_vec <- c("None" = "None",
                 "Fire Size" = "FIRE_SIZE",
                 "Fire Duration" = "time_span",
                 "Cause" = "STAT_CAUSE_DESCR")

page <- tabPanel(
  titlePanel("ScatterD3"),
  fluidRow(
    column(2,
      # Y Variable
      selectInput("y_var", "Fire Size Class: ",
                  choices = c("D" = "D",
                              "E" = "E",
                              "F" = "F",
                              "G" = "G"),
                  selected = "G"),
      checkboxInput("y_var_log", "Logarithmic y scale", value = FALSE),
      
      # Color Mapping Variable
      selectInput("col_var", "Color Mapping Variable: ",
                  choices = choices_vec,
                  selected = "FIRE_SIZE"
      ),
      checkboxInput("ell_var", "Confidence ellipses", value = FALSE),
      
      # Symbol Mapping Variable
      selectInput("sym_var", "Symbol Mapping Variable: ",
                  choices = choices_vec[-2:-3],
                  selected = "STAT_CAUSE_DESCR"
      ),
      
      # Size Mapping Variable
      selectInput("size_var", "Size Mapping Variable: ",
                  choices = choices_vec[-4],
                  selected = "FIRE_SIZE"
      ),
      
      # Opacity
      sliderInput("opacity", "Sample opacity :",
                  min = 0, max = 1, value = 0.5, step = 0.05
      ),
      
      tags$h4("Asthma"),
      
      selectInput("mode_var", "Mode: ",
                  choices = c("By year" = "Year",
                              "By county" = "County"),
                  selected = "Year"
      )
    ),
    
      
    
    # Visualization
    column(5,
      scatterD3Output("scatter_plot", height = "700px")
    ),
    column(5,
      plotlyOutput("bar_chart", height = "700px")
    )
  )
)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  
  # Application title
  titlePanel("Scorching Breath"),
  page
  )
)
