# Setup ------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(purrr)
library(mapsapi)

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

