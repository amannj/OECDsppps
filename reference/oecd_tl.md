# OECD Territorial correspondence table

OECD Territorial correspondence table (Territorial Level 2024, TL-2024)

## Usage

``` r
oecd_tl
```

## Format

A data frame with 3,750 rows and 17 columns containing names and
different classifications; see [OECD Territorial correspondence
table](https://stats.oecd.org/wbos/fileview2.aspx?IDFile=db68c5c3-5fd5-465c-b25b-b50aa14c2da1)
for more information.

## Source

[OECD Geographical
Definitions](https://www.oecd.org/en/data/datasets/oecd-geographical-definitions.html)
and [Regions, cities and local statistics](http://oe.cd/geostats)

## Examples

``` r
oecd_tl |> dim()
#> [1] 3750   17
oecd_tl |> names()
#>  [1] "ISO3"                                                                                                  
#>  [2] "REG_ID"                                                                                                
#>  [3] "Classification"                                                                                        
#>  [4] "TL"                                                                                                    
#>  [5] "TL2024 change (new)"                                                                                   
#>  [6] "Regional name (eng)"                                                                                   
#>  [7] "Regional name (fra)"                                                                                   
#>  [8] "Regional name (orig)"                                                                                  
#>  [9] "Regional name (publication-eng)"                                                                       
#> [10] "Hierarchical relations"                                                                                
#> [11] "NOG parent (for TL3 Canada, France, Germany)"                                                          
#> [12] "Typology access to functional urban areas"                                                             
#> [13] "Metropolitan region code"                                                                              
#> [14] "Metropolitan region name"                                                                              
#> [15] "TL3 to be agregated in a Metropolitan region (when several TL3 belong to the same Metropolitan region)"
#> [16] "Metropolitan region TL3 aggregation typology"                                                          
#> [17] "Typology rural/urban"                                                                                  
```
