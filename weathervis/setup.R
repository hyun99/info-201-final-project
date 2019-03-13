library(leaflet)
library(lubridate)
library(dplyr)
library(RSQLite)

# Importing and subsetting California Wildfire dataset
ca_fire <- read.csv(file = "ca_fire.csv", stringsAsFactors = FALSE)
fire <- ca_fire %>% 
  select(FIRE_NAME, FIRE_SIZE, FIRE_SIZE_CLASS, STATE, COUNTY, "LOCATION" = SOURCE_REPORTING_UNIT_NAME)
