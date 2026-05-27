# The Fisher price index

`index_fisher()` in OECDsppps calculates the matrix of Fisher indices.
It returns a data frame containing the base region, region, and the
respective indices; see *Details* and World Bank (2013) , for more
information.

## Usage

``` r
index_fisher(
  data,
  region = "region",
  product = "product",
  ppp_bh = "ppp_bh",
  exp_wght = "exp_wght"
)
```

## Arguments

- data:

  A data frame or tibble containing at least four columns identifying
  region, product, subnational PPPs, and expenditure weights. The data
  is checked using `valid_index_data()` prior to index calculation.

- region:

  Identifier for regions

- product:

  Product identifier

- ppp_bh:

  Identifier for subnational PPPs

- exp_wght:

  Identifier for expenditure weights

## Details

The Fisher index for regions \\j\\ and \\k\\ is obtained as
\\sPPP_F^{j,k} = \left( sPPP_L^{j,k} \times sPPP_P^{j,k} \right)^{1/2}
\\ which is the geometric average of the Paasche and Laspeyres index.

The function returns a data frame containing the following variables:
'base_region' (region *j*), 'region' (region *k*), 'fisher_index' (final
indices).

## References

World Bank (2013). *Measuring the Real Size of the World Economy: The
Framework, Methodology, and Results of the International Comparison
Program — ICP*. World Bank.
[doi:10.1596/978-0-8213-9728-2](https://doi.org/10.1596/978-0-8213-9728-2)
.

## Examples

``` r
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))
tibble(
  region = c("region A", "region A", "region B", "region B"),
  product = c("product 1", "product 2", "product 1", "product 2"),
  ppp_bh = c(0.5, 0.7, 0.6, 0.9),
  exp_wght = c(0.5, 0.5, 0.6, 0.4)
) |>
  index_fisher()
#> # A tibble: 4 × 3
#>   base_region region   fisher_index
#>   <chr>       <chr>           <dbl>
#> 1 region A    region A        1    
#> 2 region A    region B        1.24 
#> 3 region B    region A        0.808
#> 4 region B    region B        1    
```
