library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggmap)
library(knitr)
library(leaflet)
library(usmap)
library(maps)
library(mapproj)
library(shinydashboard)

asthma_data <- read.csv(file = file.path("asthma_by_county.csv"), stringsAsFactors = F)
asthma_data <- asthma_data %>%
  select(Year, Age.Group, Number.of.Visits, County) %>%
  arrange(Year, County) %>%
  mutate(test <- replace_na(Number.of.Visits, 0)) %>%
  group_by(Year, County) %>%
  select(-Number.of.Visits) %>%
  rename("num_visits" = `test <- replace_na(Number.of.Visits, 0)`) %>%
  summarise(sum_visits = sum(num_visits)) %>%
  filter(
    Year == 2012 | Year == 2013 | Year == 2014 | Year == 2015 | Year == 2016
  )

years <- unique(asthma_data$Year)
county_names <- unique(asthma_data$County)

## CREATING THE DASHBOARD SIDEBAR
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Asthma", tabName = "Asthma", icon = icon("dashboard")),
    menuItem("Wildfire", tabName = "WildFires", icon = icon("widget"))
  )
)

# CREATING EACH INDIVIDUAL TAB's BODY
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Asthma",
            h2("Asthma Patients in California Counties from 2012 - 2016"),
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
      tableOutput("ct2"),
    
      fluidRow(
        plotOutput("county1"),
        plotOutput("county2")
      )
    ),
    ### CREATING SECOND TAB ###
    tabItem(tabName = "WildFires",
            h2("Severity of WildFires in California Counties for Every 5 Years
               Since 2005"),
            fluidRow(
              splitLayout(
                cellWidths = c("40%", "40%", "40%"),
                plotOutput("fire_map_2005"),
                plotOutput("fire_map_2010"),
                plotOutput("fire_map_2015")
              )
            ),
            leafletOutput("fire_interactive_map")
    )
  )
)

# CREATING MAIN UI
final_ui <- dashboardPage(
  dashboardHeader(title = "Asthma"),
  sidebar,
  body
)
  