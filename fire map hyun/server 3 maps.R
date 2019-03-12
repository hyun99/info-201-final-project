#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
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

ca_fire <- read.csv(file = "ca_fire.csv", stringsAsFactors = FALSE)
fire <- ca_fire %>%
  select(FIRE_NAME, FIRE_YEAR, FIRE_SIZE, FIRE_SIZE_CLASS, STATE, COUNTY, FIPS_CODE, FIPS_NAME, "LOCATION" = SOURCE_REPORTING_UNIT_NAME)


by_county <- fire %>%
  group_by(FIPS_NAME, FIRE_YEAR) %>%
  count(sort = T) # %>%
# ungroup()

by_county <- na.omit(by_county)


# map("county", regions=by_county$FIPS_NAME)

states <- map_data("state")
ca_df <- subset(states, region == "california")

#head(ca_df)

counties <- map_data("county")
ca_county <- subset(counties, region == "california")

#head(ca_county)

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()


ca_base + theme_nothing() +
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)

by_county$FIPS_NAME <- tolower(by_county$FIPS_NAME)

names(by_county)[names(by_county) == "FIPS_NAME"] <- "subregion"
#by_county

cacopa <- inner_join(ca_county, by_county, by = "subregion")
names(cacopa)[names(cacopa) == "n"] <- "Firerate"

server <- (function(input, output) {

  # fire map of 2005
  output$fire_map_2005 <- renderPlot({
    options(scipen = 999)

    ca_fire <- read.csv(file = "ca_fire.csv", stringsAsFactors = FALSE)
    fire <- ca_fire %>%
      select(FIRE_NAME, FIRE_YEAR, FIRE_SIZE, FIRE_SIZE_CLASS, STATE, COUNTY, FIPS_CODE, FIPS_NAME, "LOCATION" = SOURCE_REPORTING_UNIT_NAME)


    by_county <- fire %>%
      group_by(FIPS_NAME, FIRE_YEAR) %>%
      count(sort = T) # %>%
    # ungroup()

    by_county <- na.omit(by_county)

    by_county <- subset(by_county, FIRE_YEAR == "2005")

    # map("county", regions=by_county$FIPS_NAME)

    states <- map_data("state")
    ca_df <- subset(states, region == "california")

    head(ca_df)

    counties <- map_data("county")
    ca_county <- subset(counties, region == "california")

    head(ca_county)

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

    fire_map_2005 <- ca_base +
      geom_polygon(data = cacopa, aes(fill = Firerate), color = "white") +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      ditch_the_axes +
      labs(title = "2005 California Wildfires")
    fire_map_2005 + scale_fill_gradient(trans = "log10")

    fire_map_2005 <- fire_map_2005 +
      scale_fill_gradient(low = "red", high = "darkred")

    print(fire_map_2005)
  })

  # fire map of 2010
  output$fire_map_2010 <- renderPlot({
    options(scipen = 999)

    ca_fire <- read.csv(file = "ca_fire.csv", stringsAsFactors = FALSE)
    fire <- ca_fire %>%
      select(FIRE_NAME, FIRE_YEAR, FIRE_SIZE, FIRE_SIZE_CLASS, STATE, COUNTY, FIPS_CODE, FIPS_NAME, "LOCATION" = SOURCE_REPORTING_UNIT_NAME)


    by_county <- fire %>%
      group_by(FIPS_NAME, FIRE_YEAR) %>%
      count(sort = T) # %>%
    # ungroup()

    by_county <- na.omit(by_county)


    by_county <- subset(by_county, FIRE_YEAR == "2010")

    states <- map_data("state")
    ca_df <- subset(states, region == "california")

    head(ca_df)

    counties <- map_data("county")
    ca_county <- subset(counties, region == "california")

    head(ca_county)

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

    fire_map_2010 <- ca_base +
      geom_polygon(data = cacopa, aes(fill = Firerate), color = "white") +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      ditch_the_axes +
      labs(title = "2010 California Wildfires")
    fire_map_2010 + scale_fill_gradient(trans = "log10")

    fire_map_2010 <- fire_map_2010 +
      scale_fill_gradient(low = "red", high = "darkred")

    print(fire_map_2010)
  })

  # fire map of 2015
  output$fire_map_2015 <- renderPlot({
    options(scipen = 999)

    ca_fire <- read.csv(file = "ca_fire.csv", stringsAsFactors = FALSE)
    fire <- ca_fire %>%
      select(FIRE_NAME, FIRE_YEAR, FIRE_SIZE, FIRE_SIZE_CLASS, STATE, COUNTY, FIPS_CODE, FIPS_NAME, "LOCATION" = SOURCE_REPORTING_UNIT_NAME)


    by_county <- fire %>%
      group_by(FIPS_NAME, FIRE_YEAR) %>%
      count(sort = T) # %>%
    # ungroup()

    by_county <- na.omit(by_county)


    by_county <- subset(by_county, FIRE_YEAR == "2015")

    states <- map_data("state")
    ca_df <- subset(states, region == "california")

    head(ca_df)

    counties <- map_data("county")
    ca_county <- subset(counties, region == "california")

    head(ca_county)

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

    fire_map_2015 <- ca_base +
      geom_polygon(data = cacopa, aes(fill = Firerate), color = "white") +
      geom_polygon(color = "black", fill = NA) +
      theme_bw() +
      ditch_the_axes +
      labs(title = "2015 California Wildfires")
    fire_map_2015 + scale_fill_gradient(trans = "log10")

    fire_map_2015 <- fire_map_2015 +
      scale_fill_gradient(low = "red", high = "darkred")

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
        lng = ~long, lat = ~lat,
        label = ~ as.character(paste(
          subregion,
          ", Year of the Fire:", FIRE_YEAR,
          " , Rate of Fire:", Firerate
        ))
      )
  })
})

shinyApp(ui, server)
