library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)
library(styler)
library(ggimage)
library(plotly)

asthma_data <- read.csv("summarized_asthma_data2.csv", stringsAsFactors = F)
years <- unique(asthma_data$Year)
county_names <- unique(asthma_data$County)

shinyServer(function(input, output) {
  
  #County 1 plot
  output$county1 <- renderPlotly({
    data1 <- asthma_data %>%
      rename("Visits" = "sum_visits") %>%
        filter(County == input$state1_choice)

    ggplotly(ggplot(data = data1, aes(x = Year, y = Visits)) +
      geom_image(image = "inhaler2.png", size = .075) +
      geom_smooth() +
      labs(x = "Year",
           y = "Asthma Emergency Department Visits per Year",
           title =paste0(input$state1_choice, " County"))) %>%
      #make image availabe
      layout(
        images = list(
          source = base64enc::dataURI(file = "inhaler2.png"),
          x = 0, y = 1, 
          sizex = 0.32, sizey = 0.16,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ),
        margin = list(t = 50)
      )
    
  })
  
  #County 2 plot
  output$county2 <- renderPlotly({
    data2 <- asthma_data %>%
      rename("Visits" = "sum_visits") %>%
        filter(County == input$state2_choice)
    #asthma_data$Year == #input$year_choice
    
    ggplotly(ggplot(data = data2, aes(x = Year, y = Visits)) +
      geom_image(image = "inhaler2.png", size = .075) + 
      geom_smooth() +
      labs(x = "Year",
           y = "Asthma Emergency Department Visits per Year",
           title = paste0(input$state2_choice, " County"))) %>%
      #makes asthma image available
      layout(
        images = list(
          source = base64enc::dataURI(file = "inhaler2.png"),
          x = 0, y = 1, 
          sizex = 0.32, sizey = 0.16,
          xref = "paper", yref = "paper", 
          xanchor = "left", yanchor = "bottom"
        ),
        margin = list(t = 50)
      )
  
  })
  
  #Table1
  output$ct1 <- renderTable(
    if(input$dt1r) {
      asthma_data %>%
        rename("Number of Visits" = "sum_visits") %>%
          filter(County == input$state1_choice)
    }
  )
  
  #Table 2
  output$ct2 <- renderTable(
    if(input$dt2r) {
      asthma_data %>%
        rename("Number of Visits" = "sum_visits") %>%
          filter(County == input$state2_choice)
    }
  )
})

