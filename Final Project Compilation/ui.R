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
library(plotly)

### FOR FIRST TAB ###
asthma_data <- read.csv("summarized_asthma_data2.csv", stringsAsFactors = F)
years <- unique(asthma_data$Year)
county_names <- unique(asthma_data$County)

### FOR THIRD TAB ###
choices_vec <- c("None" = "None",
                 "Fire Size" = "FIRE_SIZE",
                 "Fire Duration" = "time_span",
                 "Cause" = "STAT_CAUSE_DESCR")

## CREATING THE DASHBOARD SIDEBAR
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "intro",
             icon = icon("home")),
    menuItem("Asthma", tabName = "Asthma",
             icon = icon("chart-line")),
    menuItem("Wildfire", tabName = "WildFires",
             icon = icon("globe-americas")),
    menuItem("Wildfire and Asthma", tabName = "Asthma_and_Wildfires",
             icon = icon("chart-bar")),
    menuItem("Credits", tabName = "Credits",
             icon = icon("book-open"))
  )
)

# CREATING EACH INDIVIDUAL TAB's BODY
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "intro",
            tags$h2("What are wildfires?"),
            tags$p("A wildfire or wildland fire is a fire in an area of
                   combustible vegetation occurring in rural areas.
                   (Cambridge University Press, 2008)."),
            tags$p("Wildfires can be characterized in terms of the cause
                   of ignition, their physical properties, the combustible
                   material present, and the effect of weather on the fire.
                   Wildfires can cause damage to property and human life,
                   although naturally occurring wildfires may have beneficial
                   effects on native vegetation, animals, and ecosystems that
                   have evolved with fire. For humans, smoke from wildfires can
                   cause health problems, especially for children and those who
                   already have respiratory problems.",
                   tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6038982/", "(Source)"),
                   " Several epidemiological studies have demonstrated a close
                   association between air pollution and respiratory allergic
                   diseases such as bronchial asthma."),
          
            tags$h2("What is Asthma?"),
            tags$p("Asthma is a common long-term inflammatory disease of the
                   airways of the lungs. Symptoms involve recurring episodes
                   of wheezing, coughing, chest tightness, shortness of breath."),
            
            tags$h2("Scope"),
            tags$p("For the purpose of simplicity and efficiency, we have
                   chosen California as the target region for analysis.
                   The 2018 wildfire season was the deadliest and most
                   destructive season on record in California, thus making
                   CA the state for studying wildfire impact on Asthma
                   treatment."),
            tags$p("In the rest of the tabs, we will be covering the analysis
                   over wirdfire frequency, sizes, and asthma hospitalization
                   rates in California."),
            
            tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/a/a6/The_Rim_Fire_in_the_Stanislaus_National_Forest_near_in_California_began_on_Aug._17%2C_2013-0004.jpg", width = "100%", height = "50%"),
            tags$img(src = "http://www.conceptnutrition.co.uk/wp-content/uploads/2018/09/Asthma-Concept-nutrition.jpg", width = "100%", height = "50%")
            
            ),
    tabItem(tabName = "Asthma",
            h2("Asthma Patients in California Counties from 2012 - 2016"),
            p("This page gives an insight to the number of Asthma
              patients that had visited the hospital in Californian
              counties. Within the United States, there are over 11.5 million
              asthma patients, most of which are children and because over the past couple of decades, 
              there have been many wildfires within the state of California.
              We hypothesized that there may be a rise in the number of asthma patients entering the hospitals due to asthma attacks.
              It is a well-known fact that wildfires affect the airquality and risk of asthma episodes increase as the air quality decreases.
              Thus, we wanted to see, from this data visualization shown below, how many
              asthma patients were admitted from years 2012-2016 in respective Californian counties. We wanted to determine 
              if a trend existed throughout these counties or throughout parts of California (Socal, NorCal, Bay Area, etc).
              We furthered the correlation examination in the next pages to see if the asthma trends that we visualize on this current page.
              agree or disagree with the wildfire severity in California"),
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
            tableOutput("ct1"),
            tableOutput("ct2"),
    fluidPage(
      plotlyOutput("county1", width = "85%", height = "400px"),
      plotlyOutput("county2", width = "85%", height = "400px")
    )
  ),
    ### CREATING SECOND TAB ###
    tabItem(tabName = "WildFires",
        h2("Severity of WildFires in California Counties"),
        p("This page gives an insight to the severity of Wildfires in Californian
          counties. We have created a visualization of the severity of wildfires in
          Californian counties for Every 5 Years starting from year 2005.
          In addition, an interactive map was also created, differentiating the colors by
          the counties that the wildfires occured in and including informations of the year as well as the amount of fires
          that have occured in that specific location in that year.
          By looking at both the three maps of the severity of wildfires in Californian counties
          as well as the interactive map, we were able to examine the counties that had the most amount of fires
          throughout the late 1990s to 2016. There are counties that consistently have large amount of wildfires, mostly in socal,
          and if we observe the county, 'Riverside',
          the large amount of wildfires that have occured in the year 2015, correlates to a sudden spike of
          ER visits due to asthma in the year 2015 (determined by the Asthma page). To observe a more clear
          visual comparison between Asthma patients and Wildfires, both datas have been compiled on the
          'Wildfire and Asthma page'"),
        fluidRow(
          column(4,
            plotOutput("fire_map_2005")
          ),
          column(4,
            plotOutput("fire_map_2010")
          ),
          column(4,
            plotOutput("fire_map_2015")
          )
        ),
        leafletOutput("fire_interactive_map")
    ),
    ### CREATING THIRD TAB ###
    tabItem(tabName = "Asthma_and_Wildfires",
        h2("Comparison of the Sizes of the WildFires
           and Asthma Patients"),
        p("This page gives an insight to the compilation of the Asthma Patient analysis as well as the Wildfire
          analysis. We wanted to show a visual representation of Asthma Patient visits, not only just by year,
          but by county as well in order to see if there was correlation in wildfires in counties and
          asthma patient visit rates. In addition, we created a more detailed wildfire
          visualization that encompasses not only the fire size, county and the year of occurence,
          but also the fire class, cause of fire, and fire duration as well to examine different
          aspects of the fire to understand more about the wildfires that occur in California.
          One of the reasons we proceeded with this complex visualization is that we wanted to observe
          if we could understand why these fires were occuring. Will it be due to natural causes or humas errors?
          We believe that understanding of such may help prevent possible wildfires that
          may be detrimental to the airquality, which may affect asthma patients in California."),
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
    ),
    tabItem(tabName = "Credits",
        h2("Sources:"),
        br(),
        strong("Wildfire Data"),
        h6("https://www.kaggle.com/rtatman/188-million-us-wildfires"),
        br(),
        strong("Asthma Patient Data"),
        h6("https://data.chhs.ca.gov/dataset/asthma-emergency-department-visit-rates-by-zip-code"),
        br(),
        strong("California Community Foundation - Wildfire Relif Fund"),
        p("If you feel compelled to help those who have lost housing or displaced due to wildfires,
          please donate to the familes via link below."),
        h6("https://www.calfund.org/wildfire-relief-fund"),
        br(),
        br(),
        h2("Team Members"),
        strong("Sung Ahn, Anthony Cheng, Donghyun Lee, Zhewen Zheng", style = "font-size: 14pt"),
        p("We're a team comprised of Sophomores and Juniors who are
          all interested in going into the field of technology. We decided
          to look at the historic Asthma Patient visits in relation to 
          historic Wildfires in California because we wanted to determine if these recent wildfires in California
          may threaten children and adults who have asthma. Most have or had asthma, and/or have
          family members that are affected by asthma. Especially, one of our own team member, Anthony Cheng, was
          a victim of an asthma attack due to a wildfire that occured when he lived in California as a child.")
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