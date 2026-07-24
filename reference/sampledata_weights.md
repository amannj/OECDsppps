# Sample expenditure shares

Sample expenditure shares for 5 regions, and 5 product groups, created
using the `rdata()` function from the pricelevels package; see Weinand
(2025) .

## Usage

``` r
sampledata_weights
```

## Format

A data frame with 25 rows and 3 columns containing the variables

- "heading" (typically in reference to the COICOP or related
  classification),

- "region" (the subnational, spatial identifier), and

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
sampledata_weights |> dim()
#> [1] 25  3
sampledata_weights |> names()
#> [1] "heading"  "region"   "exp_wght"
sampledata_weights |> head()
#> # A tibble: 6 × 3
#>   heading   region   exp_wght
#>   <fct>     <fct>       <dbl>
#> 1 heading_1 region_1   0.0738
#> 2 heading_1 region_2   0.0687
#> 3 heading_1 region_3   0.0687
#> 4 heading_1 region_4   0.0631
#> 5 heading_1 region_5   0.0673
#> 6 heading_2 region_1   0.0983
```
