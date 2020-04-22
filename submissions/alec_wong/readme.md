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
source("R/modeling_functions.R")

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
neigh_geocode = mm_mp_geocode(address = neighborhoods$search, key = api_key)
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
