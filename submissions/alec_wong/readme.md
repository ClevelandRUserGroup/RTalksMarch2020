Housing data mini-analysis
================
Alec Wong

``` r
library(dplyr)
library(data.table)
library(readr)
library(purrr)
library(mapsapi)
library(xml2)
library(sf)
library(ggrepel)
library(xgboost)
library(ggmap)
library(memoise)

source("R/load_all_data.R")

data = load_data()

# The test dataset has no SalePrice to work with; omit these
data = data %>% filter(train_test == 1)

knitr::opts_chunk$set(comment = NA,
                      fig.path = 'output/figures/',
                      fig.width = 10,
                      fig.height = 5,
                      dpi = 300,
                      dev.args = list(type = "cairo"))
```

# Housing data

The data are sourced from the Kaggle competition found here:
<https://www.kaggle.com/c/house-prices-advanced-regression-techniques>

I used the Kaggle API to download the data:

    kaggle competitions download -c house-prices-advanced-regression-techniques

The `data_description.txt` file contains all the relevant metadata on
the dataset.

Among the many variables included in the dataset here, the
`Neighborhoods` variable caught my eye. It was clear from the
`data_description.txt` file that the neighborhoods were located in Ames,
Iowa. The objective of this short exploration became:

  - Geocode the neighborhoods.
  - See if including the neighborhood spatial information improves
    prediction.

## Neighborhood values

The neighborhood values take on the following, as described in the
`data_description.txt` file. It was tab-delimited already within the
text file, so I extracted it into its own `.tsv` file.

``` r
neighborhoods = readr::read_delim(file = 'data/neighborhoods_match.tsv',
                                  delim = '\t',
                                  col_names = c("abbreviation", "neighborhood"),
                                  col_types = cols(col_character(), col_character())
)

neighborhoods %>% print(n = 100)
```

``` 
# A tibble: 25 x 2
   abbreviation neighborhood                         
   <chr>        <chr>                                
 1 Blmngtn      Bloomington Heights                  
 2 Blueste      Bluestem                             
 3 BrDale       Briardale                            
 4 BrkSide      Brookside                            
 5 ClearCr      Clear Creek                          
 6 CollgCr      College Creek                        
 7 Crawfor      Crawford                             
 8 Edwards      Edwards                              
 9 Gilbert      Gilbert                              
10 IDOTRR       Iowa DOT and Rail Road               
11 MeadowV      Meadow Village                       
12 Mitchel      Mitchell                             
13 Names        North Ames                           
14 NoRidge      Northridge                           
15 NPkVill      Northpark Villa                      
16 NridgHt      Northridge Heights                   
17 NWAmes       Northwest Ames                       
18 OldTown      Old Town                             
19 SWISU        South & West of Iowa State University
20 Sawyer       Sawyer                               
21 SawyerW      Sawyer West                          
22 Somerst      Somerset                             
23 StoneBr      Stone Brook                          
24 Timber       Timberland                           
25 Veenker      Veenker                              
```

There are 25 neighborhoods, and after entering some of these by hand,
most of these have some definition using the [Google Maps Geocoding
API](https://developers.google.com/maps/documentation/geocoding/intro).

The neighborhoods have some moderate correlation with housing cost, and
intuitively one would assume that neighborhoods closer together might
covary more than neighborhoods farther apart.

``` r
data %>%
  ggplot() +
  geom_boxplot(aes(x = Neighborhood, y = SalePrice)) +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(labels = scales::dollar, breaks = seq(0, 1e6, by = 1e5))
```

![](output/figures/unnamed-chunk-2-1.png)<!-- -->

While I noticed this later on, one of the neighborhood values actually
doesn’t align with the description data *exactly*; the casing is
different:

``` r
setdiff(data$Neighborhood, neighborhoods$abbreviation)
```

    [1] "NAmes"

``` r
setdiff(neighborhoods$abbreviation, data$Neighborhood)
```

    [1] "Names"

Of course the name is artificial anyway, but for matching purposes later
on, I just `tolower` all the neighborhood references.

``` r
neighborhoods$abbreviation = tolower(neighborhoods$abbreviation)
data$Neighborhood = tolower(data$Neighborhood)
```

Additionally, two of the neighborhood locations don’t have any suitable
geocoded location from Google Maps, which puts them in Seattle; these
are the `swisu`, or `South & West of Iowa State University`, and
`npkvill`, or `Northpark Villa` locations. The first, `swisu`, I impute
instead simply Iowa State University as a stop-gap, since “south and
west” of it isn’t much more informative. The `Northpark Villa` is more
of an anomaly, as searching “Northpark” in Google Maps doesn’t turn
anything up in the relatively small town of Ames. I end up omitting
these records for this exercise.

## Testing out the Geocoding API

I make use of Google’s Geocode API and the corresponding package
`mapsapi` to interface with it through R.

``` r
api_key = Sys.getenv("gmaps_api_key")
```

I can obtain a bounding box for Ames, Iowa, and with `ggmap`, plot it.
Before I do, I want to memoise the `mp_geocode` function, which stores
the results in cache in memory and accesses those saved values for any
function calls with duplicate arguments. This will avoid unnecessary
calls to the API as I develop.

``` r
mm_mp_geocode = memoise::memoise(f = function(address, key){mp_geocode(address, key = api_key)})
```

What returns is essentially an XML response with some metadata and
location information, as well as return status.

``` r
ames_bb = mm_mp_geocode("ames, iowa", key = api_key)
```

    ames, iowa..............................OK

``` r
ames_bb
```

    $`ames, iowa`
    {xml_document}
    <GeocodeResponse>
    [1] <status>OK</status>
    [2] <result>\n  <type>locality</type>\n  <type>political</type>\n  <formatted ...

`mapsapi` interacts with this XML response directly, and we can get
points, polygons, and boundaries from the response.

``` r
ames_poly = mp_get_bounds(ames_bb)
ames_pt   = mp_get_points(ames_bb)
ggmap::register_google(key = api_key)
ames_ia_map = suppressMessages(ggmap::get_map(location = sf::st_coordinates(ames_pt), zoom = 12, source = 'google', messaging = FALSE))

ggmap(ggmap = ames_ia_map) +
  geom_polygon(data = ames_poly %>% st_coordinates() %>% as.data.frame, aes(x = X, y = Y),
               fill = NA, color = 'red4')
```

![](output/figures/plot-ames-map-1.png)<!-- -->

Great.

## Geocoding the neighborhoods

To input the neighborhoods into the Geocoding API, I can’t just give it
the names; I will need to give “Ames, Iowa” as context so that it
doesn’t get data from just any location. I make a new column in the
`neighborhoods` data to search with, by appending “Ames, Iowa” to the
neighborhood name. Also, as mentioned before, `swisu` and `npkvill`
don’t really have good matches, so I modify those appropriately.

``` r
# the search column will be use for the 'address' argument in mp_geocode
neighborhoods$search = stringr::str_c(neighborhoods$neighborhood, ", Ames Iowa")

# `South and West of Iowa State University` is no good, so I impute just the university name
neighborhoods = neighborhoods %>%
  mutate(search = case_when(
    abbreviation == 'swisu' ~ "Iowa State University",
    TRUE ~ search
  )) %>%
  filter(abbreviation != "npkvill")

neighborhoods$search
```

``` 
 [1] "Bloomington Heights, Ames Iowa"    "Bluestem, Ames Iowa"              
 [3] "Briardale, Ames Iowa"              "Brookside, Ames Iowa"             
 [5] "Clear Creek, Ames Iowa"            "College Creek, Ames Iowa"         
 [7] "Crawford, Ames Iowa"               "Edwards, Ames Iowa"               
 [9] "Gilbert, Ames Iowa"                "Iowa DOT and Rail Road, Ames Iowa"
[11] "Meadow Village, Ames Iowa"         "Mitchell, Ames Iowa"              
[13] "North Ames, Ames Iowa"             "Northridge, Ames Iowa"            
[15] "Northridge Heights, Ames Iowa"     "Northwest Ames, Ames Iowa"        
[17] "Old Town, Ames Iowa"               "Iowa State University"            
[19] "Sawyer, Ames Iowa"                 "Sawyer West, Ames Iowa"           
[21] "Somerset, Ames Iowa"               "Stone Brook, Ames Iowa"           
[23] "Timberland, Ames Iowa"             "Veenker, Ames Iowa"               
```

I’m ready to plug each of these addresses into the `mp_geocode`
function, or rather our memoised version of it.

``` r
neigh_geocode = neighborhoods$search %>% map(.f = ~mm_mp_geocode(address = .x, key = api_key))
```

    Bloomington Heights, Ames Iowa..........OK
    Bluestem, Ames Iowa.....................OK
    Briardale, Ames Iowa....................OK
    Brookside, Ames Iowa....................OK
    Clear Creek, Ames Iowa..................OK
    College Creek, Ames Iowa................OK
    Crawford, Ames Iowa.....................OK
    Edwards, Ames Iowa......................OK
    Gilbert, Ames Iowa......................OK
    Iowa DOT and Rail Road, Ames Iowa.......OK
    Meadow Village, Ames Iowa...............OK
    Mitchell, Ames Iowa.....................OK
    North Ames, Ames Iowa...................OK
    Northridge, Ames Iowa...................OK
    Northridge Heights, Ames Iowa...........OK
    Northwest Ames, Ames Iowa...............OK
    Old Town, Ames Iowa.....................OK
    Iowa State University...................OK
    Sawyer, Ames Iowa.......................OK
    Sawyer West, Ames Iowa..................OK
    Somerset, Ames Iowa.....................OK
    Stone Brook, Ames Iowa..................OK
    Timberland, Ames Iowa...................OK
    Veenker, Ames Iowa......................OK

First I tried to get the bounding boxes of each neighborhood to see the
result:

``` r
neigh_bb = neigh_geocode %>% map(.f = ~try(mp_get_bounds(.x)))
```

    Warning in data.frame(..., check.names = FALSE): row names were found from a
    short variable and have been discarded

    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT
    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT

    Warning in data.frame(..., check.names = FALSE): row names were found from a
    short variable and have been discarded

    Error in getClassDim(x, length(x), dim, "POINT") : 
      0 is an illegal number of columns for a POINT

Clearly there were a number of errors; some of the types returned were
points, having no bounding box. Already not too good of a start. What
have we got?

``` r
# Subset out the errors
neigh_bb_errors = neigh_bb %>% map_lgl(.f = ~inherits(.x, "try-error"))
neigh_bb = neigh_bb[!neigh_bb_errors]

# Print a sample
neigh_bb[[1]]
```

    Simple feature collection with 2 features and 3 fields
    geometry type:  POLYGON
    dimension:      XYZM
    bbox:           xmin: -93.64965 ymin: -93.64965 xmax: -93.6202 ymax: -93.63951
    z_range:        zmin: 42.05494 zmax: 42.05821
    m_range:        mmin: 42.05576 mmax: 42.05699
    CRS:            EPSG:4326
      status                        address                      address_google
    1     OK Bloomington Heights, Ames Iowa Bloomington Rd, Ames, IA 50010, USA
    2     OK Bloomington Heights, Ames Iowa Bloomington Rd, Ames, IA 50010, USA
                            geometry
    1 POLYGON ZM ((-93.64965 -93....
    2 POLYGON ZM ((-93.64965 -93....

We’re using the `sf` package for the “Simple Features” geometries, using
[well-known
text](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry)
values.

``` r
# Get the neighborhood bounding box points for mapping
n_points = map2(.x = neigh_bb,
                .y = neighborhoods$neighborhood[!neigh_bb_errors],
                .f = ~sf::st_coordinates(.x) %>% as.data.frame %>% cbind(.y, .)
                )

ggmap(ames_ia_map) +
  map(.x = n_points, .f = ~geom_polygon(data = .x, aes(x = X, y = Y), fill = NA, color = 'red4')) +
  map(.x = n_points, .f = ~geom_text(data = .x , aes(x = mean(X), y = mean(Y), label = .y)))
```

![](output/figures/unnamed-chunk-7-1.png)<!-- -->

Evidently polygon definitions for the neighborhoods isn’t as easily
obtained, so I resort to using centroids to define the spatial
information.

``` r
# Get the centroid points
neigh_centroids = map(.x = neigh_geocode, .f = ~.x %>% mp_get_points())
# Combine these simple features into a data.frame
neigh_cent_df  = neigh_centroids %>% do.call(what = rbind, args = .)
# Add the coordinates themselves to the data frame
neigh_cent_df = cbind(neigh_cent_df, st_coordinates(neigh_cent_df))
# Join this with our neighborhoods table
neighborhoods$geometry = neigh_cent_df$pnt
coords = st_coordinates(neigh_cent_df)
neighborhoods$X = coords[,1]
neighborhoods$Y = coords[,2]

neighborhoods %>% select(neighborhood, geometry, X, Y)
```

    # A tibble: 24 x 4
       neighborhood                       geometry     X     Y
       <chr>                           <POINT [°]> <dbl> <dbl>
     1 Bloomington Heights    (-93.63524 42.05642) -93.6  42.1
     2 Bluestem               (-93.66378 42.02309) -93.7  42.0
     3 Briardale               (-93.62882 42.0528) -93.6  42.1
     4 Brookside              (-93.63039 42.02865) -93.6  42.0
     5 Clear Creek             (-93.64883 42.0361) -93.6  42.0
     6 College Creek           (-93.65151 42.0222) -93.7  42.0
     7 Crawford                (-93.6489 42.01861) -93.6  42.0
     8 Edwards                (-93.68539 42.01551) -93.7  42.0
     9 Gilbert                (-93.64966 42.10693) -93.6  42.1
    10 Iowa DOT and Rail Road   (-93.62202 42.022) -93.6  42.0
    # ... with 14 more rows

``` r
ggmap(ames_ia_map) +
  geom_point(data = neighborhoods, aes(x = X, y = Y)) +
  geom_text_repel(data = neighborhoods, aes(x = X, y = Y, label = neighborhood)) +
  ggtitle("Neighborhood Centroids")
```

![](output/figures/unnamed-chunk-8-1.png)<!-- -->

Having the centroids, integrate this with the data:

``` r
# Set to UTM
neighborhoods = neighborhoods %>%
  mutate(geometry_utm = sf::st_transform(geometry, 32615),
         easting  = sf::st_coordinates(geometry_utm)[,1],
         northing = sf::st_coordinates(geometry_utm)[,2])

data$neigh_centroid = neighborhoods$geometry_utm[match(x = data$Neighborhood, neighborhoods$abbreviation)]
data = data[!sf::st_is_empty(data$neigh_centroid), ]
coords = data$neigh_centroid %>% st_coordinates()

data$easting = coords[,1]
data$northing = coords[,2]
```

## Models

### Modeling using neighborhood centroids

The question I had was; does modeling the neighborhoods as continuous
location variables improve prediction performance?

I split the data into training and test sets, and remove the
Neighborhood names themselves

``` r
train_test = sample(c(1,2), size = nrow(data), replace = TRUE, prob = c(0.75, 0.25))
data$train_test = train_test

train_data = data %>%
  filter(train_test == 1) %>%
  select(-Neighborhood, -neigh_centroid, -Id, -train_test, -Utilities, -PoolQC) %>%
  modify_if(.p = ~is.character(.x), .f = ~factor(.x))

test_data = data %>%
  filter(train_test == 2)  %>%
  select(-Neighborhood, -neigh_centroid, -Id, -train_test, -Utilities, -PoolQC)%>%
  modify_if(.p = ~is.character(.x), .f = ~factor(.x))

options(na.action = na.pass)
mm_data = model.matrix(SalePrice~ -1 + ., data = data %>%
                         select(-Neighborhood, -neigh_centroid, -Id, -train_test, -Utilities, -PoolQC))
mm_train = mm_data[data$train_test == 1,]
mm_test  = mm_data[data$train_test == 2,]
```

XGboost takes a matrix covariate input, so I use `model.matrix` to
format the input properly.

``` r
xgb = xgboost(data = mm_train, label = train_data$SalePrice, nrounds = 500, early_stopping_rounds = 10,
              max_depth = 3, subsample = 1, verbose = 0)
```

``` r
xgb
```

    ##### xgb.Booster
    raw: 322.6 Kb 
    call:
      xgb.train(params = params, data = dtrain, nrounds = nrounds, 
        watchlist = watchlist, verbose = verbose, print_every_n = print_every_n, 
        early_stopping_rounds = early_stopping_rounds, maximize = maximize, 
        save_period = save_period, save_name = save_name, xgb_model = xgb_model, 
        callbacks = callbacks, max_depth = 3, subsample = 1)
    params (as set within xgb.train):
      max_depth = "3", subsample = "1", silent = "1"
    xgb.attributes:
      best_iteration, best_msg, best_ntreelimit, best_score, niter
    callbacks:
      cb.evaluation.log()
      cb.early.stop(stopping_rounds = early_stopping_rounds, maximize = maximize, 
        verbose = verbose)
    # of features: 221 
    niter: 500
    best_iteration : 500 
    best_ntreelimit : 500 
    best_score : 1304.997 
    nfeatures : 221 
    evaluation_log:
        iter train_rmse
           1 143569.125
           2 104766.320
    ---                
         499   1310.428
         500   1304.997

Additionally, run cross-validation to assess out-of-sample measurement
error.

``` r
cv_locs = xgb.cv(data = mm_train, label = train_data$SalePrice, nrounds = 500, early_stopping_rounds = 10,
       max_depth = 12, subsample = 0.5, nfold = 10, metrics = 'rmse')
```

``` r
cv_locs
```

    ##### xgb.cv 10-folds
     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
        1      144339.784      1368.2977      144339.43     11709.392
        2      105514.420      1358.4320      106092.44     11373.916
        3       78394.067      1397.5424       79747.56     10873.094
        4       59272.918      1032.8095       61725.24     10513.452
        5       45861.793      1007.0082       50629.66     10589.578
        6       36740.552      1170.7309       43895.39     10703.187
        7       30385.424      1220.4809       39238.86     10064.152
        8       25755.501      1166.8715       36786.55      9878.085
        9       21876.633      1134.7200       34992.44      9828.948
       10       18984.118       837.4683       33992.11      9625.315
       11       17074.113       755.4265       33561.38      9499.660
       12       15555.357       953.1258       33034.36      9787.590
       13       14259.274       933.9219       33540.33     10055.228
       14       13132.919      1040.2940       33194.81     10192.514
       15       12114.239      1075.4550       33154.45     10921.564
       16       11388.389      1163.2522       32974.46     10957.675
       17       10581.504      1115.4385       32684.69     10976.951
       18        9818.824       957.0005       32555.46     11208.387
       19        9247.094       998.6904       32445.49     11365.839
       20        8700.407       963.5650       32379.00     11579.029
       21        8220.268       869.8125       32364.31     11721.073
       22        7736.533       747.0314       32520.41     11907.275
       23        7345.992       729.6712       32520.01     11798.504
       24        6963.522       792.5200       32547.29     11757.938
       25        6625.602       795.9331       32675.98     11764.950
       26        6325.999       776.0345       32756.72     11957.871
       27        5960.328       724.1539       32679.82     11982.124
       28        5573.125       740.2380       32723.42     12139.431
       29        5295.644       765.4489       32743.99     12173.555
       30        4998.000       711.6958       32763.46     12200.112
       31        4716.977       713.1119       32760.81     12153.934
     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
    Best iteration:
     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
       21        8220.268       869.8125       32364.31      11721.07

Out-of-sample test error:

``` r
prediction = predict(xgb, mm_train)
resid = prediction - train_data$SalePrice
train_data$resid = resid
predict_test = predict(xgb, mm_test)
resid_test = predict_test - test_data$SalePrice
test_data$resid = resid_test
# RMSE
rmse = mean(sqrt(resid_test^2))

plot(resid_test)
points(resid, col = 'red')
title(main = paste0("RMSE: ", scales::dollar(rmse)))
```

![](output/figures/unnamed-chunk-13-1.png)<!-- -->

### Modeling with neighborhoods as factors

Now, compare with using the neighborhood names as straight factors:

``` r
# Does Neighborhood make a more informative prediction?
train_data_fac = data %>%
  filter(train_test == 1) %>%
  select(-easting, -northing, -neigh_centroid, -Id, -train_test, -Utilities, -PoolQC) %>%
  modify_if(.p = ~is.character(.x), .f = ~factor(.x))

test_data_fac = data %>%
  filter(train_test == 2)  %>%
  select(-easting, -northing, -neigh_centroid, -Id, -train_test, -Utilities, -PoolQC)%>%
  modify_if(.p = ~is.character(.x), .f = ~factor(.x))

mm_data = model.matrix(SalePrice~ -1 + ., data = data %>%
                         select(-easting, -northing, -neigh_centroid, -Id, -train_test, -Utilities, -PoolQC))
mm_train = mm_data[data$train_test == 1,]
mm_test  = mm_data[data$train_test == 2,]
```

``` r
xgb_fac = xgboost(data = mm_train, label = train_data_fac$SalePrice, nrounds = 500, early_stopping_rounds = 10,
              max_depth = 3, subsample = 1, verbose = 0)
```

``` r
xgb_fac
```

    ##### xgb.Booster
    raw: 321.8 Kb 
    call:
      xgb.train(params = params, data = dtrain, nrounds = nrounds, 
        watchlist = watchlist, verbose = verbose, print_every_n = print_every_n, 
        early_stopping_rounds = early_stopping_rounds, maximize = maximize, 
        save_period = save_period, save_name = save_name, xgb_model = xgb_model, 
        callbacks = callbacks, max_depth = 3, subsample = 1)
    params (as set within xgb.train):
      max_depth = "3", subsample = "1", silent = "1"
    xgb.attributes:
      best_iteration, best_msg, best_ntreelimit, best_score, niter
    callbacks:
      cb.evaluation.log()
      cb.early.stop(stopping_rounds = early_stopping_rounds, maximize = maximize, 
        verbose = verbose)
    # of features: 242 
    niter: 500
    best_iteration : 500 
    best_ntreelimit : 500 
    best_score : 1305.62 
    nfeatures : 242 
    evaluation_log:
        iter train_rmse
           1 143593.875
           2 104808.289
    ---                
         499   1308.033
         500   1305.620

``` r
cv_fac = xgb.cv(data = mm_train, label = train_data_fac$SalePrice, nrounds = 500, early_stopping_rounds = 10,
       max_depth = 12, subsample = 0.5, nfold = 10, metrics = 'rmse')
```

``` r
cv_fac
```

    ##### xgb.cv 10-folds
     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
        1      144490.692       1125.492      145127.81      6583.322
        2      105428.246       1183.518      106782.35      6136.882
        3       78388.199       1237.987       80836.49      5824.466
        4       59057.476       1228.399       63121.10      6764.321
        5       46079.990       1868.557       52253.45      6871.262
        6       36906.102       2014.247       44800.85      6809.750
        7       30026.257       2154.867       39924.40      6843.648
        8       25258.841       2119.219       37436.68      7289.151
        9       21637.428       1973.670       35809.33      7815.024
       10       19187.614       2025.596       34779.35      8092.274
       11       17076.549       1617.090       34487.21      8243.543
       12       15568.900       1661.381       34204.96      8437.584
       13       14329.354       1757.867       33718.89      8335.770
       14       13487.654       1933.332       33658.11      8361.459
       15       12646.767       2043.739       33552.60      8432.525
       16       11924.193       2231.809       33194.92      8163.476
       17       11175.842       2282.863       33265.94      8029.601
       18       10643.591       2155.032       33488.85      8117.834
       19        9941.984       1602.091       33287.32      8092.174
       20        9473.489       1707.525       33262.97      8005.590
       21        8882.567       1675.484       33299.08      8007.352
       22        8482.149       1819.149       33254.42      8046.721
       23        7916.367       1455.444       33384.18      8014.287
       24        7446.682       1409.786       33473.57      8097.616
       25        6955.685       1181.266       33549.52      8136.300
       26        6628.965       1174.193       33510.82      8174.907
     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
    Best iteration:
     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
       16        11924.19       2231.809       33194.92      8163.476

Out-of-sample test error:

``` r
prediction = predict(xgb_fac, mm_train)
resid = prediction - train_data_fac$SalePrice
train_data_fac$resid_fac = resid

predict_test = predict(xgb_fac, mm_test)
resid_test = predict_test - test_data_fac$SalePrice
test_data_fac$resid_test = resid_test
# RMSE
rmse = mean(sqrt(resid_test^2))

plot(resid_test)
points(resid, col = 'red')
title(main = paste0("RMSE: ", scales::dollar(rmse)))
```

![](output/figures/unnamed-chunk-17-1.png)<!-- -->

# Conclusion

This turned out to be mostly an exercise in geocoding and formatting
spatial data; it doesn’t appear that geocoding the locations actually
reduces prediction error by a large margin.

-----

``` r
sessionInfo()
```

``` 
R version 3.6.3 (2020-02-29)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] memoise_1.1.0     ggmap_3.0.0       xgboost_1.0.0.2   ggrepel_0.8.2    
 [5] ggplot2_3.3.0     sf_0.9-2          xml2_1.3.1        mapsapi_0.4.5    
 [9] purrr_0.3.3       readr_1.3.1       data.table_1.12.8 dplyr_0.8.5      

loaded via a namespace (and not attached):
 [1] tidyselect_1.0.0    xfun_0.12           lattice_0.20-38    
 [4] colorspace_1.4-1    vctrs_0.2.4         htmltools_0.4.0    
 [7] yaml_2.2.1          utf8_1.1.4          rlang_0.4.5        
[10] e1071_1.7-3         pillar_1.4.3        glue_1.3.2         
[13] withr_2.1.2         DBI_1.1.0           sp_1.4-1           
[16] plyr_1.8.6          jpeg_0.1-8.1        lifecycle_0.2.0    
[19] stringr_1.4.0       munsell_0.5.0       gtable_0.3.0       
[22] RgoogleMaps_1.4.5.3 codetools_0.2-16    evaluate_0.14      
[25] labeling_0.3        knitr_1.28          curl_4.3           
[28] class_7.3-15        fansi_0.4.1         Rcpp_1.0.3         
[31] KernSmooth_2.23-16  scales_1.1.0        classInt_0.4-3     
[34] farver_2.0.3        rjson_0.2.20        hms_0.5.3          
[37] png_0.1-7           digest_0.6.25       stringi_1.4.6      
[40] grid_3.6.3          cli_2.0.2           tools_3.6.3        
[43] bitops_1.0-6        magrittr_1.5        tibble_3.0.0       
[46] tidyr_1.0.2         crayon_1.3.4        pkgconfig_2.0.3    
[49] ellipsis_0.3.0      Matrix_1.2-18       assertthat_0.2.1   
[52] rmarkdown_2.1       httr_1.4.1          R6_2.4.1           
[55] units_0.6-6         compiler_3.6.3     
```
