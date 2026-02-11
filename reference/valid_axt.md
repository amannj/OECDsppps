# Create "Average Household Expenditure Share Table"

`valid_axt()` in OECDsppps creates the "Average Household Expenditure
Share Table" by calculating:

- `Minimum`- Highest household expenditure share by group as specified
  by [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Lower quartile`- Lower quartile household expenditure share by group
  as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Average`- Average household expenditure share by group as specified
  by [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Median`- Median household expenditure share by group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Upper quartile`- Upper quartile household expenditure share by group
  as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Maximum` - Lowest household expenditure share by group as specified
  by [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `Standard Deviation` - Standard deviation household expenditure share
  by group as specified by
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- `max-min ratio test` and `coefficient of variation test` - see
  *Details* for more information All household expenditure shares that
  do not pass the two tests are flagged in columns `Max-min ratio FLAG`
  and`Coefficient of variation FLAG`, respectively; see World Bank
  (2013), ICP (2021) and European Union/OECD et al. (2024) .

## Usage

``` r
valid_axt(data, expenditure_share = "expenditure_share")
```

## Arguments

- data:

  A data frame or tibble containing at least one column with expenditure
  shares.

- expenditure_share:

  Column containing the individual expenditure shares.

## Details

**Max-min ratio test:** The ratio between the maximal and minimal
observed expenditure share \\j\\, \\p_j\\. Expenditure shares where the
maximal observed share is more than twice as big as the minimum are
flagged in `Max-min ratio FLAG`: \\max-min~ratio = max(p_j)/min(p_j)\\

**Coefficient-of-variation test:** The standard deviation
\\\sigma\_{p_j}\\ of expenditure group \\j\\'s share \\p_j\\ expressed
as a percentage of the average share over time, \\\mu\_{p_j}\\.
Expenditure shares with a coefficient of variation greater than 20% will
be flagged in `Coefficient of variation FLAG`:
\\coefficient-to-variation: \sigma\_{p_j} / \mu\_{p_j}\\

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
