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
library(scatterD3)
library(rsconnect)


asthma_data <- read.csv("asthma_by_county.csv", stringsAsFactors = F)
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

choices_vec <- c("None" = "None",
                 "Fire Size" = "FIRE_SIZE",
                 "Fire Duration" = "time_span",
                 "Cause" = "STAT_CAUSE_DESCR")

## CREATING THE DASHBOARD SIDEBAR
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Asthma", tabName = "Asthma",
             icon = icon("chart-line")),
    menuItem("Wildfire", tabName = "WildFires",
             icon = icon("globe-americas")),
    menuItem("Wildfire and Asthma", tabName = "Asthma Patients and WildFires",
             icon = icon("chart-bar"))
  )
)

# CREATING EACH INDIVIDUAL TAB's BODY
body <- dashboardBody(
  tags$head(tags$style(HTML('
      .main-header .logo {
                            font-family: "Georgia", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 24px;
                            }
                            '))),
  tabItems(
    tabItem(tabName = "Asthma",
            h2("Asthma Patients in California Counties from 2012 - 2016"),
            p("Our group was curious in the number of Asthma
              patients that had visited the hospital in Californian
              counties, because over the past couple of decades, there
              have been many wildfires within the state of California.
              We hypothesized that there may be some correlation between
              the number of asthma patients and the number of wildfires
              that have occured during a certain year. Thus, we wanted to
              see, from this data visualization shown below, how many
              asthma patients were admitted from years 2012-2016 and
              observe if there some years that were much higher than the other, or
              see if there are visible trends occuring. We wanted to see exactly
              how many asthma patients were in each respective county in order
              to observe the correlation with asthma patients and wildfires. The
              details of such as well as the details of wildfire severity can
              be found on the next pages."),
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
            p("Continuing from the Asthma Patient analysis, portrayed below is the Severity of Wildfires in California
              Counties for Every 5 Years starting year 2005. An interactive map is also made by differentiating the colors by
              the counties that the wildfires occured in and has informations of the year, amount of fires
              that have occured in that specific location in that year. By looking at both the map of the california 
              as well as the interactive map, we were able to examine the counties that had the most amount of fires
              throughout the late 1990s to 2016. We believe by looking at such visualization, we will be able to
              present a visual comparison between Asthma Patients and Wildfires to see if there exists
              a correlation that can be accounted for the Asthma hospitalizations"),
            fluidRow(
              splitLayout(
                cellWidths = c("33%", "33%", "33%"),
                plotOutput("fire_map_2005"),
                plotOutput("fire_map_2010"),
                plotOutput("fire_map_2015")
              )
            ),
            leafletOutput("fire_interactive_map")
    ),
    ### CREATING THIRD TAB ###
    tabItem(tabName = "Asthma Patients and WildFires",
            h2("Comparison of the Sizes of the WildFires
               and Asthma Patients"),
            p("As a compilation of the Asthma Patient analysis as well as the Wildfire
              analysis, portrayed blow is the visualization of Wildfires and the Asthma Patient Visits.
              We wanted to show a visual representation of Asthma Patient visits, not only just by year,
              but by county as well in order to see if there was correlation in wildfires in counties and
              asthma patient visit rates. In addition, we created a more detailed wildfire
              visualization that encompasses not only the fire size and county, but fire class, cause of fire,
              and fire duration as well to examine different aspects of the fire to see if the
              fire was caused by natural events or humans. We believe that understanding of
              such may help prevent possible wildfires that may be detrimental to the environment."),
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
  )
)

# CREATING MAIN UI
shinyUI(dashboardPage(skin = "black",
    dashboardHeader(title = "Asthma and California Wildfires",
                    titleWidth = 450
                    ),
    sidebar,
    body
  )
)
  