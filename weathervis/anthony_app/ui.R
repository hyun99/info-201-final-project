library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)
library(styler)
library(tidyr)

asthma_data <- read.csv("../asthma_by_county.csv", stringsAsFactors = F)
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

my_ui <- fluidPage(
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
  plotOutput("county1"),
  plotOutput("county2")
  )
)
)
