Housing data
============

Among the many variables included in the dataset here, the
`Neighborhoods` variable caught my eye. It was clear from the
`data_description.txt` file that the neighborhoods were located in Ames,
Iowa. The objective became:

-   Geocode the neighborhoods as a bit of feature engineering.
-   See if including the neighborhood spatial information improves
    prediction any.

Neighborhood values
-------------------

The neighborhood values take on the following, as described in the
`data_description.txt` file. It was tab-delimited already, so I
extracted it into its own `.tsv` file.

    neighborhoods = readr::read_delim(file = 'data/neighborhoods_match.tsv', 
                                      delim = '\t', 
                                      col_names = c("abbreviation", "neighborhood"),
                                      col_types = cols(col_character(), col_character())
    )

    neighborhoods

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
    # ... with 15 more rows

    data %>% 
      ggplot() + 
      geom_boxplot(aes(x = Neighborhood, y = SalePrice)) + 
      theme_bw() + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))

![](output/figures/unnamed-chunk-2-1.png)
