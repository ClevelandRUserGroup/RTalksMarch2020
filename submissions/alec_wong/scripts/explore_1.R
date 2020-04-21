# Setup ------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(purrr)
library(mapsapi)
library(xml2)

source("R/load_all_data.R")

# Data import -- use predefined function. Train-test are combined but labeled.

data = load_data()

# Investigate.

nrow(data[data$train_test == 1, ])

data %>% lapply(FUN = function(x){
  if (is.numeric(x)){
    summary(x)
  } else {
    table(x)
  }
})

# MSSubClass should be factorized.
# Neighborhood is interesting, want to try SAR model. City is Ames, IA.

api_key = Sys.getenv("gmaps_api_key")

ames_geocode = mp_geocode("Ames, Iowa", key = api_key)
mp_map(center = 'Ames, Iowa', zoom = 10, key = api_key, maptype = 'roadmap')


ames_bb = osmdata::getbb("ames, iowa")
q = osmdata::opq(bbox = ames_bb)

ames_sf = q %>% osmdata_sf()
