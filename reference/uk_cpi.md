# UK CPI microdata extract

A subset of the official UK CIP microdata set

## Usage

``` r
uk_cpi
```

## Format

### `uk_cpi`

A data frame with 11,807 rows and 14 columns containing two products:
*White sliced loaf branded 750 grams* (COICOP 1010103) and *carpenter
hourly rate* (COICOP 410518).

- Year:

  Year

- Date of quote:

  year and month of data collection (yyyymm)

- COICOP5:

  COICOP 5-digit code

- Product code:

  Identification of the item collected

- Product description:

  Description of item collected

- Reference quantity:

  Reference quantity of commodity that *should* be surveyed

- Unit of reference quantity:

  Unit of reference quantity of commodity that *should* be surveyed

- Region:

  Region, following
  [TL2](https://www.oecd.org/en/data/datasets/oecd-geographical-definitions.html)/[ITL1](https://www.ons.gov.uk/methodology/geography/ukgeographies/eurostat)
  boundaries)

- Shop identifier:

  Code of shop that the price was collected from

- Type of shop:

  Either "Multiple" (10 or more outlets) or "Independents" (less than 10
  outlets)

- Quantity observed:

  Quantity observed of commodity that *was* be surveyed

- Unit of observed quantity:

  Unit of observed quantity of commodity that *was* be surveyed

- Price observed:

  Price observed of commodity that *was* be surveyed

- Reference quantity price:

  The *reference quantity price* scales the observed price to the
  quantity that *should* be surveyed. It is defined as `Price observed`
  / `Quantity observed` \* `Reference quantity` whenever
  `Unit of observed quantity` == `Unit of reference quantity`.

## Source

[ONS Consumer price inflation consumption segment indices and price
quotes](https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes)
, last access November 2025

## Examples

``` r
uk_cpi |> dim()
#> [1] 11807    14
uk_cpi |> dplyr::distinct(`Product description`)
#> # A tibble: 2 × 1
#>   `Product description`         
#>   <chr>                         
#> 1 WHITE SLICED LOAF BRANDED 750G
#> 2 CARPENTER HOURLY RATE         
uk_cpi |> names()
#>  [1] "Year"                       "Date of quote"             
#>  [3] "COICOP5"                    "Product code"              
#>  [5] "Product description"        "Reference quantity"        
#>  [7] "Unit of reference quantity" "Region"                    
#>  [9] "Shop identifier"            "Type of shop"              
#> [11] "Quantity observed"          "Unit of observed quantity" 
#> [13] "Price observed"             "Reference quantity price"  
```
