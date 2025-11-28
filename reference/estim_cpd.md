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
  are aggregated by way of averaging across region-product pairs

- base.region:

  A character specifying the base to which the estimated logarithmic
  regional price levels are expressed When NULL, they refer to the
  (unweighted) regional average, similar to
  [`contr.sum()`](https://rdrr.io/r/stats/contrast.html)

- output:

  Either "SPPP", which returns the estimated subnational purchasing
  purchasing power parities, that is, \\\hat{SPPP}\_r =
  exp(\hat{\alpha}\_r)\\ or "Std. Error", which returns the standard
  errors of the estimation; default is "SPPP"

## Details

The CPD method is a regression-based approach for estimating price
parities. It is characterised by a fixed-effects specification, in which
country effects yield estimates of subnational purchasing power
parities, while commodity-specific effects generate estimates of
subnational price levels. The model can be written as a regression
equation in which all explanatory variables take the form of dummy
indicators for each region and commodity:

\\ln p\_{ij} = \alpha_1 D_1 + ... + \alpha_j D_j + ... +\alpha_R D_R +
\\ \eta_1 \mathcal{D}\_1 + ... + \eta_i \mathcal{D}\_i + ... + \eta_N
\mathcal{D}\_N + \varepsilon\_{ij}\\

where \\\alpha_j\\ is the the price level of region \\j\\ relative to
all other regions in the comparison. \\\alpha_j\\ can also be expressed
relative to a reference region, for example, the national price level.
Then, \\\alpha_j\\ represents the subnational purchasing power parity of
region \\j\\ given by \\\hat{PPP}\_j = exp(\hat{\alpha}\_j)\\.

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
