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
