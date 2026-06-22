# Dikhanov table

`valid_dikhanov()` generates the Dikhanov tables for all selected basic
headings; see World Bank (2013) and ICP (2021) .

## Usage

``` r
valid_dikhanov(
  data,
  region = "region",
  product = "product",
  price = "price",
  product_heading = "product_heading",
  product_heading_comparison = "all"
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

  Individual item-level price quotes; duplicate region-product pairs are
  aggregated by way of averaging across region-product pairs following
  the default options in
  [`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)

- product_heading:

  Variable identifying the corresponding product groups of the
  individual price quotes; typically corresponds to the basic headings,
  for example the 4-digit COICOP groups.

- product_heading_comparison:

  Specify the product groups identified via argument `product_heading`
  for which the Dikhanov tables should be computed; default is 'all',
  that is, for all product groups listed in `product_heading` the
  Dikhanov tables will be computed

## Details

The Dikhanov tables consist of:

- Summary information (PPPs, SDs, price level) by region for the
  aggregate;

- CPD residuals and product variation coefficients for products within
  basic headings.

The Dikhanov table facilitates the comparisons of PPPs across basic
headings; plausible variations in PPPs is expected across regions. Such
variations would indicate that, say, alcoholic beverages in region A are
x% higher than in region B. The CPD residuals help ensure that the
aggregate PPP variations are not driven by certain basic headings, or
isolated products therein, but are more reflective of common price-level
differences across regions.

The function first obtains CPD estimates through
[`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md).
It then calculates all required summary statistics and returns a list
containing Dikhanov tables for each of the selected basic headings.

## Examples

``` r
set.seed(123)
R <- 5 # number of regions
B <- 3 # number of product groups
N <- 5 # number of products
dt1 <- pricelevels::rdata(R = R, B = B, N = N)
# Dikhanov tables for products with product classification provided by
# variable 'group' for products with generic name "1" and "3"
valid_dikhanov(
  data = dt1,
  region = "region",
  product = "product",
  price = "price",
  product_heading = "group",
  product_heading_comparison = c("1", "3")
)
#> $`1`
#> # A tibble: 8 × 10
#>   variable            product     `1`     `2`      `3`      `4`      `5` `STD 1`
#>   <chr>               <fct>     <dbl>   <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
#> 1 sPPP                NA       0.849   1.28    1.14     0.867    9.33e-1 NA     
#> 2 STD 2               NA       0.0942  0.138   0.0756   0.0891   3.10e-2  1.90  
#> 3 No. of items priced NA       5       5       5        5        5   e+0 NA     
#> 4 NA                  01      -0.0126  0.0147  0.00299 -0.00910  4.07e-3  0.0110
#> 5 NA                  02       0.131  -0.190  -0.110    0.128    4.06e-2  0.144 
#> 6 NA                  03      -0.123   0.185   0.100   -0.116   -4.66e-2  0.137 
#> 7 NA                  04      -0.0370  0.0452  0.0211  -0.0310   1.76e-3  0.0347
#> 8 NA                  05       0.0417 -0.0559 -0.0137   0.0277   1.52e-4  0.0381
#> # ℹ 2 more variables: `Items per region` <dbl>, `Items/Countries` <dbl>
#> 
#> $`3`
#> # A tibble: 8 × 10
#>   variable         product      `1`      `2`      `3`      `4`      `5`  `STD 1`
#>   <chr>            <fct>      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1 sPPP             NA       0.887    1.19     1.10e+0  9.01e-1  0.957   NA      
#> 2 STD 2            NA       0.0156   0.0368   1.43e-2  2.63e-2  0.0115   1.90   
#> 3 No. of items pr… NA       5        5        5   e+0  5   e+0  5       NA      
#> 4 NA               11      -0.0101   0.0409   1.22e-2 -3.39e-2 -0.00923  0.0281 
#> 5 NA               12       0.00223  0.00584 -7.79e-4 -5.41e-4 -0.00675  0.00463
#> 6 NA               13      -0.00563 -0.00925  6.12e-3 -9.73e-4  0.00972  0.00791
#> 7 NA               14       0.0261  -0.0569  -2.42e-2  3.98e-2  0.0151   0.0397 
#> 8 NA               15      -0.0126   0.0194   6.58e-3 -4.45e-3 -0.00887  0.0130 
#> # ℹ 2 more variables: `Items per region` <dbl>, `Items/Countries` <dbl>
#> 

# Dikhanov tables for all three products contained in the data
valid_dikhanov(
  data = dt1,
  region = "region",
  product = "product",
  price = "price",
  product_heading = "group"
)
#> $`1`
#> # A tibble: 8 × 10
#>   variable            product     `1`     `2`      `3`      `4`      `5` `STD 1`
#>   <chr>               <fct>     <dbl>   <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
#> 1 sPPP                NA       0.849   1.28    1.14     0.867    9.33e-1 NA     
#> 2 STD 2               NA       0.0942  0.138   0.0756   0.0891   3.10e-2  1.90  
#> 3 No. of items priced NA       5       5       5        5        5   e+0 NA     
#> 4 NA                  01      -0.0126  0.0147  0.00299 -0.00910  4.07e-3  0.0110
#> 5 NA                  02       0.131  -0.190  -0.110    0.128    4.06e-2  0.144 
#> 6 NA                  03      -0.123   0.185   0.100   -0.116   -4.66e-2  0.137 
#> 7 NA                  04      -0.0370  0.0452  0.0211  -0.0310   1.76e-3  0.0347
#> 8 NA                  05       0.0417 -0.0559 -0.0137   0.0277   1.52e-4  0.0381
#> # ℹ 2 more variables: `Items per region` <dbl>, `Items/Countries` <dbl>
#> 
#> $`2`
#> # A tibble: 8 × 10
#>   variable          product      `1`     `2`      `3`      `4`      `5`  `STD 1`
#>   <chr>             <fct>      <dbl>   <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1 sPPP              NA       0.788    1.42    1.20e+0  0.810    0.919   NA      
#> 2 STD 2             NA       0.0169   0.0277  7.43e-3  0.00923  0.0130   1.90   
#> 3 No. of items pri… NA       5        5       5   e+0  5        5       NA      
#> 4 NA                06      -0.00673  0.0193  4.28e-3 -0.00108 -0.0158   0.0131 
#> 5 NA                07      -0.0177   0.0248  8.56e-3 -0.0113  -0.00431  0.0169 
#> 6 NA                08      -0.0101   0.0138 -3.16e-4 -0.00583  0.00248  0.00911
#> 7 NA                09       0.0107  -0.0185 -1.23e-3  0.0115  -0.00239  0.0122 
#> 8 NA                10       0.0239  -0.0394 -1.13e-2  0.00678  0.0200   0.0260 
#> # ℹ 2 more variables: `Items per region` <dbl>, `Items/Countries` <dbl>
#> 
#> $`3`
#> # A tibble: 8 × 10
#>   variable         product      `1`      `2`      `3`      `4`      `5`  `STD 1`
#>   <chr>            <fct>      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1 sPPP             NA       0.887    1.19     1.10e+0  9.01e-1  0.957   NA      
#> 2 STD 2            NA       0.0156   0.0368   1.43e-2  2.63e-2  0.0115   1.90   
#> 3 No. of items pr… NA       5        5        5   e+0  5   e+0  5       NA      
#> 4 NA               11      -0.0101   0.0409   1.22e-2 -3.39e-2 -0.00923  0.0281 
#> 5 NA               12       0.00223  0.00584 -7.79e-4 -5.41e-4 -0.00675  0.00463
#> 6 NA               13      -0.00563 -0.00925  6.12e-3 -9.73e-4  0.00972  0.00791
#> 7 NA               14       0.0261  -0.0569  -2.42e-2  3.98e-2  0.0151   0.0397 
#> 8 NA               15      -0.0126   0.0194   6.58e-3 -4.45e-3 -0.00887  0.0130 
#> # ℹ 2 more variables: `Items per region` <dbl>, `Items/Countries` <dbl>
#> 
```
