# Setup ------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(purrr)
library(mapsapi)
library(xml2)
library(sf)
library(ggrepel)
library(xgboost)

source("R/load_all_data.R")

# Data import -- use predefined function. Train-test are combined but labeled.

data = load_data()

data = data %>% filter(train_test == 1)

# set factors where necessary

# Investigate.

data %>% lapply(FUN = function(x){
  if (is.numeric(x)){
    summary(x)
  } else {
    table(x)
  }
})

# MSSubClass should be factorized.
# Neighborhood is interesting, want to try geospatial model. City is Ames, IA.

api_key = Sys.getenv("gmaps_api_key")

ames_bb = mp_geocode("ames, iowa", key = api_key)
ames_bb %>% mp_get_bounds()

neighborhoods = readr::read_delim(file = 'data/neighborhoods_match.tsv', delim = '\t', col_names = c("abbreviation", "neighborhood"))
neighborhoods$search = stringr::str_c(neighborhoods$neighborhood, ", Ames Iowa")

# Note that the data have a value for neighborhood NAmes is not the same as the matching, Names. toLower all the things.
neighborhoods$abbreviation = tolower(neighborhoods$abbreviation)
data$Neighborhood = tolower(data$Neighborhood)

# Note that swisu and npkvill are not well found by Gmaps. Fix by altering the search:
neighborhoods = neighborhoods %>%
  mutate(search = case_when(
    abbreviation == 'swisu' ~ "Iowa State University",
    TRUE ~ search
  ))

# Expensive operation -- memoize or store result.
neigh_geocode = map(neighborhoods$search, .f = ~mp_geocode(.x, key = api_key))

neigh_centroids = map(.x = neigh_geocode, .f = ~.x %>% mp_get_points())

neigh_cent_df  = neigh_centroids %>% do.call(what = rbind, args = .)
neigh_cent_df = cbind(neigh_cent_df, st_coordinates(neigh_cent_df))

neighborhoods = cbind(neighborhoods, neigh_cent_df$pnt, st_coordinates(neigh_cent_df))


# neigh_bb = neigh_geocode %>% map(.f = ~try(mp_get_bounds(.x)))
# neigh_bb_errors = neigh_bb %>% map_lgl(.f = ~inherits(.x, "try-error"))
# neigh_bb = neigh_bb[!neigh_bb_errors]
# ames_pt = ames_bb %>% mp_get_points()
# ggmap::register_google(key = api_key)
ames_ia_map = ggmap::get_map(location = sf::st_coordinates(ames_pt), zoom = 12, source = 'google')
# n_points = map2(.x = neigh_bb, .y = neighborhoods$neighborhood[!neigh_bb_errors], .f = ~sf::st_coordinates(.x) %>% as.data.frame %>% cbind(.y, .))
# names(n_points) = neighborhoods$neighborhood[!neigh_bb_errors]
# # Polygons don't work so well -- not all neighborhoods return polygons; just take the centroids.
# ggmap(ames_ia_map) +
#   map(.x = n_points, .f = ~geom_polygon(data = .x, aes(x = X, y = Y), fill = NA, color = 'red4')) +
#   map(.x = n_points, .f = ~geom_label(data = .x , aes(x = mean(X), y = mean(Y), label = .y)))

ggmap(ggmap = ames_ia_map) +
  geom_point     (data = neighborhoods, aes(x = X, y = Y)) +
  geom_text_repel(data = neighborhoods, aes(x = X, y = Y, label = neighborhood))

# geospatial analysis ----------------------------------------------------------

# Set to UTM
neighborhoods = neighborhoods %>%
  mutate(geometry_utm = sf::st_transform(geometry, 32615),
         lon = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2])

# Remove NA's

data$neigh_centroid = neighborhoods$geometry_utm[match(x = data$Neighborhood, neighborhoods$abbreviation)]
data$neigh_centroid[data$Neighborhood == 'npkvill'] = NA
data = data[!sf::st_is_empty(data$neigh_centroid), ]
coords = data$neigh_centroid %>% st_coordinates()
coords_latlon = data$neigh_centroid %>% st_transform(4326) %>% st_coordinates()
data = data %>% bind_cols(utm_x = coords[,1], utm_y = coords[,2]) %>%
  bind_cols(lon = coords_latlon[,1], lat = coords_latlon[,2])

dist_mat = fields::rdist(coords)


vg = fields::vgram(loc = coords, y = data$SalePrice, d = dist_mat, type = 'variogram', breaks = seq(0,10000,length.out = 30))
plot(vg, N = 20)

ggmap(ggmap = ames_ia_map) +
  geom_point     (data = data, aes(x = lon, y = lat, color = log(SalePrice)), position = position_jitter(width = 0.003, height = 0.003)) +
  geom_text_repel(data = neighborhoods, aes(x = lon, y = lat, label = abbreviation))

ggplot(data = data) +
  geom_boxplot(aes(x = Neighborhood, y = SalePrice)) +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

# can we krige?

fit = geoR::likfit(coords = st_coordinates(data$neigh_centroid),
                   data = data$SalePrice,
                   cov.model = 'matern',
                   ini.cov.pars = c(var(data$SalePrice), 1000),
                   fix.nugget = FALSE,
                   nugget = 10,
                   messages = TRUE)

# No try some rf

train_test = sample(c(1,2), size = nrow(data), replace = TRUE, prob = c(0.75, 0.25))
data$train_test = train_test

train_data = data %>%
  filter(train_test == 1) %>%
  select(-Neighborhood, -neigh_centroid, -lon, -lat, -Id, -train_test, -Utilities, -PoolQC) %>%
  modify_if(.p = ~is.character(.x), .f = ~factor(.x)) %>%
  cbind(dist_mat[train_test == 1,])

test_data = data %>%
  filter(train_test == 2)  %>%
  select(-Neighborhood, -neigh_centroid, -lon, -lat, -Id, -train_test, -Utilities, -PoolQC)%>%
  modify_if(.p = ~is.character(.x), .f = ~factor(.x)) %>%
  cbind(dist_mat[train_test == 2,])

options(na.action = na.pass)
mm_data = model.matrix(SalePrice~., data = data %>%
                         select(-Neighborhood, -neigh_centroid, -lon, -lat, -Id, -train_test, -Utilities, -PoolQC))

mm_train = mm_data[data$train_test == 1,]
mm_test  = mm_data[data$train_test == 2,]

xgb = xgboost(data = mm_train, label = train_data$SalePrice, nrounds = 100, early_stopping_rounds = 10,
              max_depth = 12, subsample = 0.5)

xgb.cv(data = mm_train, label = train_data$SalePrice, nrounds = 100, early_stopping_rounds = 10,
       max_depth = 12, subsample = 0.5, nfold = 10, metrics = 'rmse')

prediction = predict(xgb, mm_train)
resid = prediction - train_data$SalePrice

predict_test = predict(xgb, mm_test)
resid_test = predict_test - test_data$SalePrice
plot(resid_test)
points(resid, col = 'red')

train_data$resid = resid
# RMSE
mean(sqrt(resid_test^2))

# Does Neighborhood make a more informative prediction?
train_data = data %>%
  filter(train_test == 1) %>%
  select(-utm_x, -utm_y, -neigh_centroid, -lon, -lat, -Id, -train_test, -Utilities, -PoolQC) %>%
  modify_if(.p = ~is.character(.x), .f = ~factor(.x))

test_data = data %>%
  filter(train_test == 2)  %>%
  select(-utm_x, -utm_y, -neigh_centroid, -lon, -lat, -Id, -train_test, -Utilities, -PoolQC)%>%
  modify_if(.p = ~is.character(.x), .f = ~factor(.x))

options(na.action = na.pass)
mm_data = model.matrix(SalePrice~., data = data %>%
                         select(-Neighborhood, -neigh_centroid, -lon, -lat, -Id, -train_test, -Utilities, -PoolQC))

mm_train = mm_data[data$train_test == 1,]
mm_test  = mm_data[data$train_test == 2,]

xgb = xgboost(data = mm_train, label = train_data$SalePrice, nrounds = 100, early_stopping_rounds = 10,
              max_depth = 12, subsample = 0.5)

xgb.cv(data = mm_train, label = train_data$SalePrice, nrounds = 100, early_stopping_rounds = 10,
       max_depth = 12, subsample = 0.5, nfold = 10, metrics = 'rmse')

prediction = predict(xgb, mm_train)
resid = prediction - train_data$SalePrice

predict_test = predict(xgb, mm_test)
resid_test = predict_test - test_data$SalePrice
plot(resid_test)
points(resid, col = 'red')

train_data$resid = resid
# RMSE
mean(sqrt(resid_test^2))

# Spatial info is slightly better than neighborhood.
