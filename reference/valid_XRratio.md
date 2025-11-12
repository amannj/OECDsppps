# The "XR-ratio tables"

`valid_XRratio()` in OECDsppps calculates the exchange rate ratio
(XR-ratio), which is a region-country's XR-price Variability can be
compared to highlight products in countries/regions that are most
variable (high XR-ratio) across countries; see World Bank (2013), ICP
(2021) and European Union/OECD (2024) .

## Usage

``` r
valid_XRratio(
  data,
  average_price = "Average price of product",
  exchange_rate = "XR USD",
  ...
)
```

## Arguments

- data:

  A data frame or tibble containing at least a column with the average
  country-region prices and exchange rate.

- average_price:

  Average country-region prices of the individual item-level price
  quotes. Correspond to the "Average price of product" obtained in
  [`valid_apt()`](valid_apt.md).

- exchange_rate:

  National-level exchange rate for common currency; typically USD.

## Details

The **XR-ratio** uses the exchange-rate-converted prices to calculate
the *standardised price ratio (SPR)* For product \\1\\ and
country–region \\A\\, the SPR is defined as: \\SPR\_{1A} = \mu^\*\_{1A}
/ \left( \prod\_{n = A,\dots, N} \mu^\*\_{1n} \right)^{\frac{1}{N}}
\times 100\\ where \\\mu^{\*}\_{1A}\\ represents the *average converted
price* of product \\1\\ in country–region \\A\\, and \\N\\ is the total
number of country–regions.

## References

European Union/OECD (2024). *Eurostat-OECD Methodological Manual on
Purchasing Power Parities (2023 Edition)*. OECD Publishing, Paris.
[doi:10.2785/384854](https://doi.org/10.2785/384854) .  
  
ICP (2021). “A Guide to the Compilation of Subnational Purchasing Power
Parities (PPPs).” International Comparison Program.
<https://thedocs.worldbank.org/en/doc/5064f2288436664bc8f9811c8a5b8c55-0050022021/original/Guide-Subnational-PPPs.pdf>.  
  
World Bank (2013). *Measuring the Real Size of the World Economy: The
Framework, Methodology, and Results of the International Comparison
Program — ICP*. World Bank.
[doi:10.1596/978-0-8213-9728-2](https://doi.org/10.1596/978-0-8213-9728-2)
.
