library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)
library(styler)
library(ggimage)
library(plotly)
library(scatterD3)
library(rsconnect)

### Formatting First Tab ###
asthma_data <- read.csv("summarized_asthma_data2.csv", stringsAsFactors = F)
years <- unique(asthma_data$Year)
county_names <- unique(asthma_data$County)

### Formatting Second Tab ###

# creates smaller data
ca_fire <- read.csv("ca_fire.csv", stringsAsFactors = FALSE)

get_fire_map <- function(year_var) {
  options(scipen = 999)
  
  fire <- ca_fire %>%
    select(FIRE_NAME, FIRE_YEAR, FIRE_SIZE, FIRE_SIZE_CLASS, STATE, COUNTY, FIPS_CODE, FIPS_NAME, "LOCATION" = SOURCE_REPORTING_UNIT_NAME)
  by_county <- fire %>%
    group_by(FIPS_NAME, FIRE_YEAR) %>%
    count(sort = T)
  
  by_county <- subset(by_county, FIRE_YEAR == year_var)
  
  states <- map_data("state")
  ca_df <- subset(states, region == "california")
  
  counties <- map_data("county")
  ca_county <- subset(counties, region == "california")
  
  ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) +
    geom_polygon(color = "black", fill = "gray")
  ca_base + theme_nothing()
  
  
  ca_base + theme_nothing() +
    geom_polygon(data = ca_county, fill = NA, color = "white") +
    geom_polygon(color = "black", fill = NA)
  
  by_county$FIPS_NAME <- tolower(by_county$FIPS_NAME)
  
  names(by_county)[names(by_county) == "FIPS_NAME"] <- "subregion"
  by_county
  
  cacopa <- inner_join(ca_county, by_county, by = "subregion")
  names(cacopa)[names(cacopa) == "n"] <- "Firerate"
  
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
  
  fire_map <- ca_base +
    geom_polygon(data = cacopa, aes(fill = Firerate), color = "white") +
    geom_polygon(color = "black", fill = NA) +
    theme_bw() +
    ditch_the_axes +
    labs(title = paste0(year_var, " California Wildfires"))
  fire_map + scale_fill_gradient(trans = "log10")
  
  fire_map <- fire_map +
    scale_fill_gradient(low = "red", high = "darkred")
  
  return(fire_map) 
}

fire <- ca_fire %>%
  select(FIRE_NAME, FIRE_YEAR, FIRE_SIZE, FIRE_SIZE_CLASS, STATE, COUNTY, FIPS_CODE, FIPS_NAME, "LOCATION" = SOURCE_REPORTING_UNIT_NAME)

# groups data by county and year
by_county <- fire %>%
  group_by(FIPS_NAME, FIRE_YEAR) %>%
  count(sort = T) 

# gets rid of nas
by_county <- na.omit(by_county)

# picks out cali from us map
states <- map_data("state")
ca_df <- subset(states, region == "california")

# picks our counties in cali
counties <- map_data("county")
ca_county <- subset(counties, region == "california")

# creates the base for cali
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()

# separates to counties
ca_base + theme_nothing() +
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)

# makes the county name to lower case
by_county$FIPS_NAME <- tolower(by_county$FIPS_NAME)

# changes the FIPS_NAME to subregion
names(by_county)[names(by_county) == "FIPS_NAME"] <- "subregion"

# joins the county datas by subregion (each county)
cacopa <- inner_join(ca_county, by_county, by = "subregion")
names(cacopa)[names(cacopa) == "n"] <- "Firerate"

### Formatting the Third Tab ###
fire <- read.csv("scatter_fire.csv", stringsAsFactors = FALSE)
asthma <- read.csv("data_asthma.csv", stringsAsFactors = FALSE)

### CREATING THE ACTUAL INTERFACE ###
shinyServer(function(input, output) {
  
  # FIRST TAB, County 1 plot #
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
  
  # FIRST TAB, County 2 plot #
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
  
  ### CREATING THE FIRE MAP OF 2005 ###
  output$fire_map_2005 <- renderPlot({
    fire_map_2005 <- get_fire_map("2005") 
    print(fire_map_2005)
  })
  output$fire_map_2010 <- renderPlot({
    fire_map_2010 <- get_fire_map("2010") 
    print(fire_map_2010)
  })
  output$fire_map_2015 <- renderPlot({
    fire_map_2015 <- get_fire_map("2015") 
    print(fire_map_2015)
  })
  
  output$fire_interactive_map <- renderLeaflet({
    pal <- colorFactor(c("navy", "red", "green"),
                       domain = unique(cacopa$subregion)
    )
    
    leaflet(cacopa) %>%
      addTiles() %>%
      addCircleMarkers(
        color = ~ pal(subregion),
        stroke = FALSE, fillOpacity = 0.5,
        radius = 3.5,
        lng = ~long, lat = ~lat,
        label = ~ as.character(paste(
          subregion,
          ", Year of the Fire:", FIRE_YEAR,
          " , Rate of Fire:", Firerate
        ))
      )
  })
    
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