library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)
library(styler)
library(ggimage)
library(plotly)

my_server <- function(input, output) {
  
  #County 1 plot
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
  
  #County 2 plot
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
  
  #Table1
  output$ct1 <- renderTable(
    if(input$dt1r) {
      asthma_data %>%
        filter(County == input$state1_choice)
    }
  )
  
  #Table 2
  output$ct2 <- renderTable(
    if(input$dt2r) {
      asthma_data %>%
        filter(County == input$state2_choice)
    }
  )
}