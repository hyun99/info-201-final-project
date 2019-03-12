library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)
library(styler)
library(ggimage)
library(plotly)

final_server <- function(input, output) {
  
  ## FIRST TAB, PLOT OF THE FIRST COUNTY VISUAL
  output$county1 <- renderPlot({
    data1 <- asthma_data %>%
      filter(County == input$state1_choice)
    
    ggplot(data = data1, aes(x = data1$Year, y = data1$sum_visits)) +
      geom_image(image = "inhaler2.png", size = .075) +
      geom_smooth() +
      labs(x = "Year",
           y = "Asthma Emergency Department Visits per Year",
           title = input$state1_choice)
  })
  
  ## FIRST TAB, PLOT OF THE SECOND COUNTY VISUAL
  output$county2 <- renderPlot({
    data2 <- asthma_data %>%
      filter(County == input$state2_choice)
    #asthma_data$Year == #input$year_choice
    
    ggplot(data = data2, aes(x = data2$Year, y = data2$sum_visits)) +
      geom_image(image = "inhaler2.png", size = .075) + 
      geom_smooth() +
      labs(x = "Year",
           y = "Asthma Emergency Department Visits per Year",
           title = input$state2_choice)
    
  })
  
  ## Table to FIRST County Population's Total Hospital Visits in 
  ## the given years
  output$ct1 <- renderTable(
    if(input$dt1r) {
      asthma_data %>%
        filter(County == input$state1_choice)
    }
  )
  
  ## Table to Second County Population's Total Hospital Visits in 
  ## the given years
  output$ct2 <- renderTable(
    if(input$dt2r) {
      asthma_data %>%
        filter(County == input$state2_choice)
    }
  )
}

## Creating the IO
shinyApp(final_ui, final_server)
