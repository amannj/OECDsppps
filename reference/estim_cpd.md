# Country Product Dummy (CPD) regression model

`estim_cpd` in OECDsppps creates ...; see *Details* and World Bank
(2013) , for more information.

## Usage

``` r
estim_cpd(
  data,
  region = "region",
  product = "product",
  price = "price",
  base.region = NULL,
  output = "SPPP"
)
```

## Arguments

- data:

  Data frame, data table or tibble containing at least three columns
  identifying region, product and individual item-level price quotes

- region:

  Identifier for regions (within or across countries)

- product:

  Product identifier

- price:

  Individual item-level price quotes; duplicated region-product pairs
  are aggregated by way of averaging across region-product pairs.

- base.region:

  A character specifying the base to which the estimated logarithmic
  regional price levels are expressed. When NULL, they refer to the
  (unweighted) regional average, similar to
  [`contr.sum()`](https://rdrr.io/r/stats/contrast.html).

- output:

  Either "SPPP", which returns the estimated subnational purchasing
  purchasing power parities, that is, \\\hat{SPPP}\_r =
  exp(\hat{\alpha}\_r)\\ or "Std. Error", which returns the standard
  errors of the estimation; default is "SPPP"

## Value

A tibble (scalar) with estimated subnational purchasing purchasing power
parities, that is, \\\hat{SPPP}\_r = exp(\hat{\alpha}\_r)\\ if
`output = "SPPS"` (default), or vector containing the standard errors of
the estimation if `output = "Std. Error"`

## Details

**detailed** description goes \\i\\, \\P\_{i}\\, here \\ratio-to-average
= p\_{ij}/\mu_j\\

## References

World Bank (2013). *Measuring the Real Size of the World Economy: The
Framework, Methodology, and Results of the International Comparison
Program — ICP*. World Bank.
[doi:10.1596/978-0-8213-9728-2](https://doi.org/10.1596/978-0-8213-9728-2)
.

## Examples

``` r
suppressPackageStartupMessages(library(dplyr))
df <- tibble(
  region = as.factor(c(1, 2, 1, 2)),
  product = as.factor(c(1, 1, 2, 2)),
  price = c(25, 28, 23, 26)
)

estim_cpd(df)
#> # A tibble: 2 × 2
#>   region  SPPP
#>   <chr>  <dbl>
#> 1 1      0.943
#> 2 2      1.06 
estim_cpd(df, output = "Std. Error")
#> [1] 0.002318409
```
