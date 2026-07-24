# The "Expenditure Shares Table"

`valid_est()` in OECDsppps creates the "Expenditure Shares Table" see
World Bank (2013), ICP (2021) and European Union/OECD et al. (2024) .
The function calculates the:

- `Nobs` - Number of data points by by group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Maximum`- Highest expenditure share based on expenditure shares by
  group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Median` - Median expenditure share based on expenditure shares by
  group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Minimum` - Lowest expenditure share based on expenditure shares by
  group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `max-median ratio test` and `median-min ratio test` - see *Details*
  for more information All expenditure shares that do not pass the two
  tests are flagged in columns `Max-median ratio FLAG`
  and`Median-min ratio FLAG`, respectively#'

## Usage

``` r
valid_est(data, shares = "Expenditure shares for basic headings")
```

## Arguments

- data:

  A data frame or tibble containing at least one column with individual
  item-level expenditure shares.

- shares:

  Column containing the individual item-level expenditure shares.

## Details

**Max-median ratio test:** The ratio between the maximal and median
observed expenditure shares for product \\j\\, \\w_j\\. Basic headings
where the maximal observed expenditure is more than 25 times as big as
the median are flagged in `Max-median ratio FLAG`: \\max-median~ratio =
max(w_j)/median(w_j)\\

**Median-min ratio test:** The ratio between the median and minimal
observed expenditure shares for product \\j\\, \\w_j\\. Basic headings
where the median observed expenditure is more than 25 times as big as
the minimum are flagged in `Median-min ratio FLAG`: \\median-min~ratio =
median(w_j)/min(w_j)\\

## References

European Union/OECD, Hearne D, Bailey D (2024). *Eurostat-OECD
Methodological Manual on Purchasing Power Parities (2023 Edition)*,
volume 12(1). OECD Publishing, Paris.
[doi:10.2785/384854](https://doi.org/10.2785/384854) .
<https://doi.org/10.1080/21681376.2025.2475115>.  
  
ICP (2021). “A Guide to the Compilation of Subnational Purchasing Power
Parities (PPPs).” International Comparison Program.
<https://thedocs.worldbank.org/en/doc/5064f2288436664bc8f9811c8a5b8c55-0050022021/original/Guide-Subnational-PPPs.pdf>.  
  
World Bank (2013). *Measuring the Real Size of the World Economy: The
Framework, Methodology, and Results of the International Comparison
Program — ICP*. World Bank.
[doi:10.1596/978-0-8213-9728-2](https://doi.org/10.1596/978-0-8213-9728-2)
.

## Examples

``` r
suppressPackageStartupMessages(library(dplyr))
library(OECDsppps)
uk_hhe |>
  group_by(coicop_4d) |>
  valid_est(shares = "expenditure_share")
#> # A tibble: 2 × 9
#>   coicop_4d  Nobs `Maximum expenditure share` `Median expenditure share`
#>   <chr>     <int>                       <dbl>                      <dbl>
#> 1 01.1.1       24                        14.7                       8.38
#> 2 04.3.2       24                        17.8                       8.45
#> # ℹ 5 more variables: `Minimum expenditure share` <dbl>,
#> #   `Max-median ratio` <dbl>, `Median-min ratio` <dbl>,
#> #   `Max-median ratio FLAG` <lgl>, `Median-min ratio FLAG` <lgl>
```
