# Sample price quotes

Sample price quotes for 5 regions, 5 product groups, and 5 generic
products created using the `rdata()` function from the pricelevels
package; see Weinand (2025) .

## Usage

``` r
sampledata_prices
```

## Format

A data frame with 124 rows and 4 columns containing the variables

- "heading" (typically in reference to the COICOP or related
  classification),

- "region" (the subnational, spatial identifier),

- "product" (the generic product for which price quotes are surveyed),
  and

- "price" (the reported price quote).

## Source

See R package [pricelevels: Spatial Price Level
Comparisons](https://CRAN.R-project.org/package=pricelevels)

## References

Weinand S (2025). *pricelevels: Spatial Price Level Comparisons*.
[doi:10.32614/CRAN.package.pricelevels](https://doi.org/10.32614/CRAN.package.pricelevels)
. R package version 1.4.0,
<https://CRAN.R-project.org/package=pricelevels>.

## Examples

``` r
sampledata_prices |> dim()
#> [1] 125   4
sampledata_prices |> names()
#> [1] "heading" "region"  "product" "price"  
sampledata_prices |> head()
#> # A tibble: 6 × 4
#>   heading   region   product price
#>   <fct>     <fct>    <fct>   <dbl>
#> 1 heading_1 region_1 item_01 35.6 
#> 2 heading_1 region_2 item_01 11.6 
#> 3 heading_1 region_3 item_01  6.28
#> 4 heading_1 region_4 item_01 18.0 
#> 5 heading_1 region_5 item_01 13.9 
#> 6 heading_1 region_1 item_02 18.4 
```
