# Create the "Price Observation Table"

`valid_pot` in OECDsppps creates the "Price Observation Table"; by
calculating two *individual price outlier statistics* for the individual
item-level price quotes, the *ratio-to-average price test* and the
*t-value test*; see World Bank (2013), ICP (2021) and European
Union/OECD et al. (2024) . All item-level price quotes that do not pass
the two tests are flagged in columns `Ratio-to-average price test FLAG`
and `T-value test FLAG`, respectively. The item-level price quotes
should be based on the *reference quantity price*; see *Details* and
World Bank (2013) , table 9.1a. for more information.

## Usage

``` r
valid_pot(data, price_quote = "Reference quantity price")
```

## Arguments

- data:

  Data frame or tibble containing at least one column with individual
  item-level price quotes.

- price_quote:

  Column containing the individual item-level price quotes, which should
  be based on the "reference quantity price"; see *Details* for more
  information.

## Details

**Reference quantity price:** Scales the observed price to the quantity
that *should* be surveyed. It is defined as: \\Reference~quantity~price
= \frac{Observed~price}{Observed~quantity} \times Reference~quantity\\
whenever the measurement unit of observed quantity is identical to the
measurement unit of the reference quantity.

**Ratio-to-average price test:** The ratio of an individual price
observation \\i\\, \\P\_{i}\\, of a specific product \\j\\ and the
observed average price for the product, \\\mu_j\\. An observed price
passes the this test if the ratio is between 0.5 and 1.5. This simple
check flags potential outlier values without relying on standard
deviation, which can itself be distorted by outliers: \\ratio-to-average
= p\_{ij}/\mu_j\\

**T-value test**: The ratio of the deviation of an individual price
observation from the average reference quantity price for the product
and the standard deviation of the product. To pass the test, the ratio
must be between -2.0 and 2.0 (any value outside that range is suspect
because it falls outside the 95 percent confidence interval): \\t-val =
(p\_{ij} - \mu\_{P_j}) / \sigma\_{P_j}\\

## References

European Union/OECD, Hearne D, Bailey D (2024). *Eurostat-OECD
Methodological Manual on Purchasing Power Parities (2023 Edition)*,
volume 12(1). OECD Publishing, Paris.
[doi:10.2785/384854](https://doi.org/10.2785/384854) ,
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
uk_cpi |>
  select(Year, `Product code`, `Reference quantity price`) |>
  group_by(Year, `Product code`) |>
  valid_pot(price_quote = "Reference quantity price") |>
  head()
#> # A tibble: 6 × 7
#> # Groups:   Year, Product code [1]
#>   Year  `Product code` `Reference quantity price` `Ratio-to-average price test`
#>   <chr>          <dbl>                      <dbl>                         <dbl>
#> 1 2018          210111                       1                            0.956
#> 2 2018          210111                       1                            0.956
#> 3 2018          210111                       1.45                         1.39 
#> 4 2018          210111                       1.45                         1.39 
#> 5 2018          210111                       1.05                         1.00 
#> 6 2018          210111                       1.05                         1.00 
#> # ℹ 3 more variables: `T-value test` <dbl>,
#> #   `Ratio-to-average price test FLAG` <lgl>, `T-value test FLAG` <lgl>
```
