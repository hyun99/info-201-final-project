#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

fire <- read.csv("scatter_fire.csv", stringsAsFactors = FALSE)
asthma <- read.csv("data_asthma.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  
  # Wildfire reactive dataframe
  fire_react <- reactive({
    fire[fire$FIRE_SIZE_CLASS == input$y_var, ]
  })
  
  # Asthma reactive dataframe
  asthma_react <- reactive({
    asthma_child <- asthma %>%  
      group_by_(input$mode_var) %>% 
      summarise(visits = sum(Number.of.Visits[age == "child"]),
                age = "child")
    asthma_adult <- asthma %>%  
      group_by_(input$mode_var) %>% 
      summarise(visits = sum(Number.of.Visits[age == "adult"]),
                age = "adult")
    dplyr::union(asthma_child, asthma_adult)
  })
  
  # ScatterD3 output
  output$scatter_plot <- renderScatterD3({
    
    # Implement None choice in choices
    col_var <- if (input$col_var == "None") {NULL}
      else {fire_react()[, input$col_var]}
    sym_var <- if (input$sym_var == "None") {NULL}
      else {fire_react()[, input$sym_var]}
    size_var <- if (input$size_var == "None") {NULL}
      else {fire_react()[, input$size_var]}
    
    scatterD3(
      x = fire_react()[, "disc_decy"],
      y = fire_react()[, "FIRE_SIZE"],
      lab = fire_react()[, "FIRE_NAME"],
      xlab = "time",
      ylab = "Fire Size",
      x_log = FALSE,
      y_log = input$y_var_log,
      col_var = col_var,
      col_lab = input$col_var,
      ellipses = input$ell_var,
      symbol_var = sym_var,
      symbol_lab = input$sym_var,
      size_var = size_var,
      size_lab = input$size_var,
      labels_size = 0,
      key_var = rownames(fire_react()),
      point_opacity = input$opacity,
      transitions = TRUE, left_margin = 80
    )
  })
  
  # Plotly Barchart output
  output$bar_chart <- renderPlotly({
    ggplotly(
      ggplot(asthma_react(), aes_string(fill = "age", y = "visits", x = input$mode_var)) +
        geom_bar(stat="identity")
    )
  })
})
