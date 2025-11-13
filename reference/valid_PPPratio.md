# The "PPP-ratio tables"

`valid_PPPratio()` in OECDsppps calculates the PPP-ratio, which shows
the variation coefficient representing variability across products and
across country-regions; see World Bank (2013), ICP (2021) and European
Union/OECD et al. (2024) .

## Usage

``` r
valid_PPPratio(
  data,
  Year = "Year",
  `Product code` = "Product code",
  Region = "Region",
  `Average price of product` = "Average price of product",
  ...
)
```

## Arguments

- data:

  A data frame or tibble containing at least a column with the average
  country-region prices and a region and product identifier.

  Year

  :   Year

  `Product code`

  :   Product code identifier

  Region

  :   Identifier for regions (within or across countries)

  `Average price of product`

  :   Average country-region prices of the individual item-level price
      quotes. Correspond to the "Average price of product" obtained in
      [`valid_apt()`](valid_apt.md)

## Details

The country variation coefficient (row measure) represents the standard
deviation of product PPPs within country-regions, thereby identifying
countries exhibiting the greatest price variability. Conversely, the
product variation coefficient (column measure) represents the standard
deviation of PPP-ratios across country-regions, highlighting products
with the most significant cross-country variation.

The **PPP-ratio** uses the PPP-converted prices to calculate the
*standardised price ratio (SPR)* For product \\1\\ and country–region
\\A\\, the SPR is defined as: \\SPR\_{1A} = \mu^\*\_{1A} / \left(
\prod\_{n = A,\dots, N} \mu^\*\_{1n} \right)^{\frac{1}{N}} \times 100\\
where \\\mu^{\*}\_{1A}\\ represents the *average converted price* of
product \\1\\ in country–region \\A\\, and \\N\\ is the total number of
country–regions.

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
