# The Paasche-Laspeyres spread

`valid_pls()` in OECDsppps calculates the Paasche-Laspeyres spread
(PLS), see World Bank (2013), ICP (2021) and Hill (2011) , which
corresponds to the upper and lower price and quantity relatives to
determine whether the large values in the PLS are caused by PPPs or
expenditure outliers. Basic headings with large upper or lower quantity
or price relatives should be further examined.

## Usage

``` r
valid_pls(
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
  region, product, subnational PPPs, and expenditure weights.

- region:

  Identifier for regions

- product:

  Product identifier

- ppp_bh:

  Identifier for subnational PPPs

- exp_wght:

  Identifier for expenditure weights

## Details

The Paasche-Laspeyres spread for regions \\j\\ and \\k\\ is defined as:
\\PLS\_{j,k} = \frac{MAX(sPPPP\_{P}^{jk},
sPPPP\_{L}^{jk})}{MIN(sPPPP\_{P}^{jk}, sPPPP\_{L}^{jk})}\\

where \\sPPPP\_{P}^{jk}\\ and \\sPPPP\_{L}^{jk}\\ correspond to the
Paasche and Laspeyres indicies, respectively; see
[`index_paasche()`](https://amannj.github.io/OECDsppps/reference/index_paasche.md)
and
[`index_laspeyres()`](https://amannj.github.io/OECDsppps/reference/index_laspeyres.md)
for more information.

## References

Hill RJ (2011). “Linking the Regions in the International Comparisons
Program at Basic Heading Level and at Higher Levels of Aggregation.”
Technical Report 90626, World Bank.
<https://documents1.worldbank.org/curated/en/860281468157762500/pdf/906260WP008-020g0Box0385325B0PUBLIC.pdf>.  
  
ICP (2021). “A Guide to the Compilation of Subnational Purchasing Power
Parities (PPPs).” International Comparison Program.
<https://thedocs.worldbank.org/en/doc/5064f2288436664bc8f9811c8a5b8c55-0050022021/original/Guide-Subnational-PPPs.pdf>.  
  
World Bank (2013). *Measuring the Real Size of the World Economy: The
Framework, Methodology, and Results of the International Comparison
Program — ICP*. World Bank.
[doi:10.1596/978-0-8213-9728-2](https://doi.org/10.1596/978-0-8213-9728-2)
.
