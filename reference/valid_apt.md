# Create "Average Price Table"

`valid_apt()` in OECDsppps creates the "Average Price Table" by
calculating: the

- `number of observations` - Number of observations by group as
  specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `average price of product`- Average price based on item-level price
  quotes by group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `maximum price of product`- Highest price based on item-level price
  quotes by group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `minimum price of product` - Lowest price based on item-level price
  quotes by group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `standard deviation` - Standard deviation based on item-level price
  quotes by group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `max-min ratio test` and `coefficient of variation test` - see
  *Details* for more information All item-level price quotes that do not
  pass the two tests are flagged in columns `Max-min ratio FLAG`
  and`Coefficient of variation FLAG`, respectively; see World Bank
  (2013), ICP (2021) and European Union/OECD et al. (2024) .

## Usage

``` r
valid_apt(data, price_quote = "Reference quantity price")
```

## Arguments

- data:

  A data frame or tibble containing at least one column with individual
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

**Max-min ratio test:** The ratio between the maximal and minimal
observed price for product \\j\\, \\p_j\\. Products where the maximal
observed price is more than twice as big as the minimum are flagged in
`Max-min ratio FLAG`: \\max-min~ratio = max(p_j)/min(p_j)\\

**Coefficient-of-variation test:** The standard deviation
\\\sigma\_{p_j}\\ of product \\j\\'s price \\p_j\\ expressed as a
percentage of the average price for the product, \\\mu\_{p_j}\\.
Products with a coefficient of variation greater than 20% will be
flagged in `Coefficient of variation FLAG`: \\coefficient-to-variation:
\sigma\_{p_j} / \mu\_{p_j}\\

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
  select(Year, Region, `Product code`, `Reference quantity price`) |>
  group_by(Year, Region, `Product code`) |>
  valid_apt(price_quote = "Reference quantity price") |>
  head(n = 2) |>
  t()
#>                               [,1]            [,2]           
#> Year                          "2018"          "2018"         
#> Region                        "East Midlands" "East Midlands"
#> Product code                  "210111"        "410518"       
#> Number of observations        "258"           "192"          
#> Average price of product      " 1.078527"     "20.285417"    
#> Maximum price of product      " 1.3"          "33.6"         
#> Minimum price of product      "0.99"          "8.00"         
#> Standard deviation            "0.08018559"    "7.81303085"   
#> Max-min ratio                 "1.313131"      "4.200000"     
#> Coefficient of variation      "0.07434731"    "0.38515506"   
#> Max-min ratio FLAG            "FALSE"         "TRUE"         
#> Coefficient of variation FLAG "FALSE"         "TRUE"         
```
