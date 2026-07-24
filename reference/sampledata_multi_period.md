# Sample price quotes and expenditure for a generic, multi-period sample

Sample price quotes and expenditure shares for 2 periods, 5 regions, 5
product groups, and 5 generic products, created using the `rdata()`
function from the pricelevels package; see Weinand (2025) .

## Usage

``` r
sampledata_multi_period
```

## Format

A data frame with 250 rows and 8 columns containing the variables

- "period" (the temporal identifier),

- "heading" (typically in reference to the COICOP or related
  classification),

- "region" (the subnational, spatial identifier),

- "product" (the generic product for which price quotes are surveyed),

- "price" (the reported price quote),

- "quantity" (the reported quantity of the surveyed product),

- "sale" (a sales flag), and

- "exp_wght" (the generic expenditure shares for the corresponding
  heading and region).

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
sampledata_multi_period |> dim()
#> [1] 250   8
sampledata_multi_period |> names()
#> [1] "period"   "heading"  "region"   "product"  "price"    "quantity" "sale"    
#> [8] "exp_wght"
sampledata_multi_period |> head()
#> # A tibble: 6 × 8
#>   period   heading   region   product price quantity sale  exp_wght
#>   <chr>    <fct>     <fct>    <fct>   <dbl>    <dbl> <lgl>    <dbl>
#> 1 period_1 heading_1 region_1 item_01 35.6      3317 FALSE   0.0738
#> 2 period_1 heading_1 region_2 item_01 11.6      6849 FALSE   0.0687
#> 3 period_1 heading_1 region_3 item_01  6.28    25953 FALSE   0.0687
#> 4 period_1 heading_1 region_4 item_01 18.0      2765 FALSE   0.0631
#> 5 period_1 heading_1 region_5 item_01 13.9      5989 FALSE   0.0673
#> 6 period_1 heading_1 region_1 item_02 18.4      6417 FALSE   0.0738
```
