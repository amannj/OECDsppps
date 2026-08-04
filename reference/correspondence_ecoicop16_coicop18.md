# ECOICOP 2016 - COICOP 2018 correspondence table

Correspondence table between ECOICOP 2018 (also referred to as "ECOICOP
1") and COICOP 2018. Note that COICOP 2018 corresponds to ECOICOP 2.

## Usage

``` r
correspondence_ecoicop16_coicop18
```

## Format

A data frame with 776 rows and 5 columns containing the variables

- "ecoicop_code" (ECOICOP 2016 code),

- "ecoicop_description" (ECOICOP 2016 description),

- "ecoicop_comments" (ECOICOP 2016 classification comments),

- "coicop18_code " (COICOP 2018 code),

- "coicop18_description" (COICOP 2018 description),

## Source

See Statistics Lithuania for the [correspondence
table](https://osp.stat.gov.lt/en_GB/individualaus-vartojimo-islaidu-pagal-paskirti-klasifikatorius-coicop),
as well as the United Nattions Statistics Division for more information
on [ECOICOP
2016](https://data.europa.eu/data/datasets/ecoicop?locale=en) and
[COICOP
2018](https://unstats.un.org/unsd/classifications/Family/Detail/2094),
respectively.

## References

There are no references for Rd macro `\insertAllCites` on this help
page.

## Examples

``` r
correspondence_ecoicop16_coicop18 |> dim()
#> [1] 776   5
correspondence_ecoicop16_coicop18 |> names()
#> [1] "ecoicop_code"         "ecoicop_description"  "ecoicop_comments"    
#> [4] "coicop18_code"        "coicop18_description"
correspondence_ecoicop16_coicop18 |> head()
#> # A tibble: 6 × 5
#>   ecoicop_code ecoicop_description              ecoicop_comments coicop18_code
#>   <chr>        <chr>                            <chr>            <chr>        
#> 1 01           FOOD AND NON-ALCOHOLIC BEVERAGES NA               01           
#> 2 01           FOOD AND NON-ALCOHOLIC BEVERAGES NA               07           
#> 3 01           FOOD AND NON-ALCOHOLIC BEVERAGES NA               07.4.9.2     
#> 4 01.1         FOOD                             NA               01.1         
#> 5 01.1         FOOD                             NA               01.3         
#> 6 01.1.1       Bread and cereals (ND)           NA               01.1.1       
#> # ℹ 1 more variable: coicop18_description <chr>
```
