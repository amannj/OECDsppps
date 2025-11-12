# Validation

``` r
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(OECDsppps)
```

## Overview

The documentation for data validation is illustrated using a snippet of
the [UK CPI
microdata](https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes).

``` r
head(uk_cpi, n = 3) |> gt()
```

| Year | Date of quote | COICOP5 | Product code | Product description            | Reference quantity | Unit of reference quantity | Region     | Shop identifier | Type of shop | Quantity observed | Unit of observed quantity | Price observed | Reference quantity price |
|------|---------------|---------|--------------|--------------------------------|--------------------|----------------------------|------------|-----------------|--------------|-------------------|---------------------------|----------------|--------------------------|
| 2018 | 201801        | 1010103 | 210111       | WHITE SLICED LOAF BRANDED 750G | 750                | 1                          | South East | 1               | Multiple     | 750               | 1                         | 1.00           | 1.00                     |
| 2018 | 201801        | 1010103 | 210111       | WHITE SLICED LOAF BRANDED 750G | 750                | 1                          | South East | 1               | Multiple     | 750               | 1                         | 1.00           | 1.00                     |
| 2018 | 201801        | 1010103 | 210111       | WHITE SLICED LOAF BRANDED 750G | 750                | 1                          | North West | 1               | Multiple     | 750               | 1                         | 1.45           | 1.45                     |

The validation covers different dimensions, following international
recommendations (ICP 2021, 26) and (World Bank 2013, 249) and are
adapted to the subnational context

- Intra-regional validation analyses individual and aggregate price
  quotes within the *same region and across regions of the same country*

- Inter-region validation: prices validation *across all* regions and
  countries, ensuring that average prices are based on comparable
  products in regions across countries and that products have been
  accurately priced.

## Intra-regional validation

Intra-region validation establishes that price collectors within the
*same region and across regions of the same country* have priced
products that match the product specifications and that the prices they
have reported are correct. This is done in two stages, which correspond
to the outlier detection of (a) individual prices and (b) average price
aggregates.

### Individual price outlier statistcs

- For each product, a **Price Observation Table** is obtained,
  containing a characterisation of the individual product as well as two
  *individual price outlier statistics*, the *Ratio-to-average price
  test* and the *t-value test*; see World Bank (2013), table 9.1a for an
  extensive example.

- *Ratio-to-average price test*: The ratio of an individual price
  observation \\i\\, \\P\_{i}\\, of a specific product \\j\\ and the
  observed average price for the product, \\\mu_j\\. An observed price
  passes the this test if the ratio is between 0.5 and 1.5, meaning it
  is no less than half or no more than double the mean price. This
  simple check flags extreme values without relying on standard
  deviation, which can itself be distorted by outliers (World Bank 2013,
  251).

\\ ratio-to-average = p\_{ij}/\mu_j \\

- *t-value test*: The ratio of the deviation of an individual price
  observation from the average reference quantity price for the product
  and the standard deviation of the product. To pass the test, the ratio
  must be 2.0 or less (any value greater than 2.0 is suspect because it
  falls outside the 95 percent confidence interval).

\\ t-val = (p\_{ij} - \mu\_{P_j}) / \sigma\_{P_j} \\

- Individual price quotes that do not pass these tests are flagged in
  the price observation table. The two statistics are implemented in the
  OECD price statistics package `OECD_SPPP`. The price observation table
  is generated with the function
  [`valid_pot()`](../reference/valid_pot.md), which calls `valid_r2a`
  and `valid_tval`.

------------------------------------------------------------------------

**Example using UK CPI microdata**

``` r
#Price Observation Table
uk_pot <- uk_cpi |>
  select(Year, `Product code`, , `Product description`, `Reference quantity price`) |> 
  group_by(Year, `Product code`, `Product description`) |> 
  valid_pot(price_quote = "Reference quantity price")

head(uk_pot, n = 3) |>
  gt() |>
  tab_header(
    title = md("**Price Observation Table**"),
    subtitle = md("Example for `item_id` = 210111")
  ) |>
  fmt_number(
    columns = c(
      `Ratio-to-average price test`,
      `T-value test`
    ),
    decimals = 1
  )
```

| **Price Observation Table**                    |                             |              |                                  |                   |
|------------------------------------------------|-----------------------------|--------------|----------------------------------|-------------------|
| Example for `item_id` = 210111                 |                             |              |                                  |                   |
| Reference quantity price                       | Ratio-to-average price test | T-value test | Ratio-to-average price test FLAG | T-value test FLAG |
| 2018 - 210111 - WHITE SLICED LOAF BRANDED 750G |                             |              |                                  |                   |
| 1.00                                           | 1.0                         | −0.2         | FALSE                            | FALSE             |
| 1.00                                           | 1.0                         | −0.2         | FALSE                            | FALSE             |
| 1.45                                           | 1.4                         | 1.8          | FALSE                            | FALSE             |

``` r

uk_pot |>
  select(
    `Product code`,
    `Ratio-to-average price test`:`T-value test`
  ) |>
  pivot_longer(`Ratio-to-average price test`:`T-value test`) |>
  mutate(
    is.outlier = case_when(name == "Ratio-to-average price test" & (value < 0.5 | value > 1.5) ~ "Test not passed",
      name == "T-value test" & (value > 2) ~ "Test not passed",
      .default = "Test passed"
    ),
    is.outlier = factor(is.outlier, levels = c("Test passed", "Test not passed"))
  ) |>
  ggplot(aes(x = value, fill = is.outlier)) +
  facet_wrap(~name, scales = "free") +
  geom_histogram() +
  labs(
    title = "Individual price outlier statistcs",
    subtitle = "White sliced loaf branded 750G, 2018",
    x = "",
    fill = ""
  ) +
  theme_minimal()+
  theme(legend.position = "top")
#> Adding missing grouping variables: `Year`, `Product description`
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![](Validation_files/figure-html/uk_pot-1.png)

------------------------------------------------------------------------

### Aggregate price statistics

- This stage involves identifying extreme values among the average
  prices of the products listed in the average price table. An extreme
  value is defined as an individual price or average price that for a
  given test scores a value that falls outside a predetermined critical
  value and is build on two *average price outlier statistics*, which
  are summarised in the **Average Price table**; see World Bank (2013),
  table 9.2a and 9.2b for an extensive example. The two statisticis
  contained in this table are the *max-min ratio test* and the
  *coefficient to variation* test.

- *Max-min ratio test*: The ratio between the maximal and minimal
  observed price for product \\j\\. Products where the maximal observed
  price is more than twice as big as the minimum are flagged

\\ max-min~ratio = max(p_j)/min(p_j) \\

- *Coefficient to variation test*: The standard deviation for the
  product expressed as a percentage of the average price for the
  product. Products with a coefficient of variation greater than 20%
  will be flagged.

\\ coefficient-to-variation: \sigma\_{p_j} / \mu\_{p_j} \\

- Product price quotes that do not pass these tests are flagged in the
  *average price table* and is created using function `valid_apt` in the
  OECD_SPPPs package.

------------------------------------------------------------------------

**Example using UK CPI microdata**

``` r
# Average Price Table
uk_apt <- uk_cpi |>
    select(Year,Region, `Product code`, `Reference quantity price`)  |>
    group_by(Year,Region, `Product code`) |>
  valid_apt(price_quote = "Reference quantity price")

head(uk_apt, 2) |>
  gt() |>
  tab_header(
    title = md("**Average Price Table**"),
    subtitle = md("Example for `item_id`s = 210111 & 410518, **pre-cleaning**")
  ) |>
  fmt_number(
    columns = -c(Year, `Product code`),
    decimals = 1
  )
```

| **Average Price Table**                                    |                        |                          |                          |                          |                    |               |                          |                    |                               |
|------------------------------------------------------------|------------------------|--------------------------|--------------------------|--------------------------|--------------------|---------------|--------------------------|--------------------|-------------------------------|
| Example for `item_id`s = 210111 & 410518, **pre-cleaning** |                        |                          |                          |                          |                    |               |                          |                    |                               |
| Product code                                               | Number of observations | Average price of product | Maximum price of product | Minimum price of product | Standard deviation | Max-min ratio | Coefficient of variation | Max-min ratio FLAG | Coefficient of variation FLAG |
| 2018 - East Anglia                                         |                        |                          |                          |                          |                    |               |                          |                    |                               |
| 210111                                                     | 322.0                  | 1.1                      | 1.5                      | 0.6                      | 0.3                | 2.8           | 0.2                      | TRUE               | TRUE                          |
| 410518                                                     | 264.0                  | 23.1                     | 48.0                     | 15.0                     | 8.4                | 3.2           | 0.4                      | TRUE               | TRUE                          |

### Linking validation pipelines for intra-regional validation

The extent of validation required depends on the quality of the
underlying microdata. When working with unconsolidated or raw data, more
extensive revisions may be necessary.

For instance, this may involve validating and verifying the reference
quantity and price. In the case of the selected product, a bimodal
distribution was observed after outliers were removed.

Once these outliers are eliminated, the selected item passes all
intra-regional validation checks.

``` r
uk_irv <- uk_cpi |>
  select(Year, Region,`Product code`, , `Product description`, `Reference quantity price`) |> 
  group_by(Year, Region, `Product code`, `Product description`) |> 
  valid_pot() |> 
  # Condition on price quotes which pass the Price Observation Table tests
  filter(!`Ratio-to-average price test FLAG` & !`T-value test FLAG`) |>
  # Remove bimodal distribution
  filter(`Ratio-to-average price test` > 0.8) |>
  # Apply Average Price Table checks
  valid_apt()

head(uk_irv, 4) |>
  group_by(Year, `Product code`, `Product description`) |> 
  gt() |>
  tab_header(
    title = md("**Average Price Table**"),
    subtitle = md("Example for `item_id`s = 210111 & 410518, **post-cleaning**")
  ) |>
  fmt_number(
    columns = -c(Year, `Product code`),
    decimals = 1
  )
```

| **Average Price Table**                                     |                        |                          |                          |                          |                    |               |                          |                    |                               |
|-------------------------------------------------------------|------------------------|--------------------------|--------------------------|--------------------------|--------------------|---------------|--------------------------|--------------------|-------------------------------|
| Example for `item_id`s = 210111 & 410518, **post-cleaning** |                        |                          |                          |                          |                    |               |                          |                    |                               |
| Region                                                      | Number of observations | Average price of product | Maximum price of product | Minimum price of product | Standard deviation | Max-min ratio | Coefficient of variation | Max-min ratio FLAG | Coefficient of variation FLAG |
| 2018 - 210111 - WHITE SLICED LOAF BRANDED 750G              |                        |                          |                          |                          |                    |               |                          |                    |                               |
| East Anglia                                                 | 276.0                  | 1.2                      | 1.5                      | 0.9                      | 0.2                | 1.6           | 0.2                      | FALSE              | FALSE                         |
| East Midlands                                               | 228.0                  | 1.1                      | 1.1                      | 1.0                      | 0.0                | 1.2           | 0.0                      | FALSE              | FALSE                         |
| 2018 - 410518 - CARPENTER HOURLY RATE                       |                        |                          |                          |                          |                    |               |                          |                    |                               |
| East Anglia                                                 | 168.0                  | 22.3                     | 27.0                     | 20.0                     | 1.9                | 1.4           | 0.1                      | FALSE              | FALSE                         |
| East Midlands                                               | 96.0                   | 22.8                     | 28.2                     | 18.1                     | 4.0                | 1.6           | 0.2                      | FALSE              | FALSE                         |

------------------------------------------------------------------------

## Inter-regional validation

Inter-regional validation involves verifying prices across all regions
and countries to ensure that average prices are derived from comparable
products and that these products have been accurately priced.

The objective is to confirm that the average prices reflect genuine
comparability of products across countries and regions, and that pricing
accuracy has been maintained.

This is achieved by comparing the average prices of identical products
across multiple countries and identifying extreme values using the
cross-country *standardised price ratio (SPR)*.

For product \\1\\ and country–region \\A\\, the SPR is defined as:

\\ SPR\_{1A} = \mu^\*\_{1A} / \left( \prod\_{n = A,\dots, N}
\mu^\*\_{1n} \right)^{\frac{1}{N}} \times 100 \\

, where \\\mu^\*\_{1A}\\ represents the **average converted price** of
product \\1\\ in country–region \\A\\, and \\N\\ is the total number of
country–regions.

Two conversions are applied to make country–region prices comparable
across countries: exchange rates and purchasing power parities (PPPs)
(World Bank 2013, 258).

- SPRs derived from exchange rate–converted prices are referred to as
  *XR-ratios*.
- SPRs based on PPP-converted prices are referred to as *PPP-ratios*.

Both types of SPRs are used for validation; however, only PPP-ratios are
employed to measure dispersion. XR-ratios are considered more reliable
during the initial stage of cross-country validation.

XR- and PPP-ratios that fall outside the 80–125 range are flagged as
extreme values requiring verification.

### The XR-ratio

The function [`valid_XRratio()`](../reference/valid_XRratio.md) computes
the XR-ratio table, where a country–region’s XR price for a given
product is divided by the geometric mean of that product’s price; see
Table 9.3a in (World Bank 2013, 257).

In the resulting table, the degree of variability can be examined to
identify products and country–region combinations with the highest XR
ratios, that is, those showing the greatest variation across countries.

------------------------------------------------------------------------

**Example using CPI microdata**

> 🚧 Mock-up code only as comprehensive list of average product prices
> across multiple countries is currently not available.

``` r
# Build data ----------
## UK data
uk_irv <- uk_irv |>
  filter(Year == "2018") |>
  ungroup() |> 
  slice(1,3) |> 
  mutate(Region = paste0("UK - ", Region)) |>
  select(Region, Year, `Product code`, `Average price of product`) |>
  mutate(`XR USD` = 1.25)

## Dummy CZ data
cz_irv <- tibble(
  Region = c("CZ01","CZ02"),
  Year = "2018",
  `Product code` = 210111,
  `Average price of product` = c(4.22,3.88),
  `XR USD` = .4
)

## Dummy DE data
de_irv <- tibble(
  Region = c("DE01", "DE02"),
  Year = "2018",
  `Product code` = 210111,
  `Average price of product` = c(1.44,1.23),
  `XR USD` = 0.9
)

## Combine data
df_xr <- rbind(uk_irv, cz_irv, de_irv)

df_xrr <- df_xr |>
  group_by(Year, `Product code`) |> 
  valid_XRratio(average_price = "Average price of product",
                xr_usd = "XR USD")

df_xrr |>
  gt() |>
  tab_header(
    title = md("**XR-ratio Table**"),
    subtitle = md("Example for two items, **DE, UK, CZ**")
  ) |>
  fmt_number(
    columns = -c(Year, `Product code`),
    decimals = 1
  )
```

| **XR-ratio Table**                    |                          |        |          |
|---------------------------------------|--------------------------|--------|----------|
| Example for two items, **DE, UK, CZ** |                          |        |          |
| Region                                | Average price of product | XR USD | XR-ratio |
| 2018 - 210111                         |                          |        |          |
| UK - East Anglia                      | 1.2                      | 1.2    | 107.4    |
| UK - East Midlands                    | 1.1                      | 1.2    | 94.3     |
| CZ01                                  | 4.2                      | 0.4    | 120.8    |
| CZ02                                  | 3.9                      | 0.4    | 111.1    |
| DE01                                  | 1.4                      | 0.9    | 92.8     |
| DE02                                  | 1.2                      | 0.9    | 79.3     |

------------------------------------------------------------------------

### The PPP-ratio

The next stage of data validation employs purchasing power parities
(PPPs) to convert national product prices into a common currency,
enabling comparison through PPP-ratios.

This procedure is implemented using the
[`valid_PPPratio()`](../reference/valid_PPPratio.md) function, which
calculates the PPP-ratio; see Table 9.3b in (World Bank 2013, 258). The
coefficient of variation is used to assess variability across products
and countries; coefficients exceeding 33% are considered extreme and may
indicate the need for further verification of the underlying data.

Within each block, PPP-ratios—computed as the PPP-converted price
divided by the geometric mean of the product price—reflect the degree of
variability both across country-regions and across products.

The country variation coefficient (row measure) represents the standard
deviation of product PPPs within country-regions, thereby identifying
countries exhibiting the greatest price variability. Conversely, the
product variation coefficient (column measure) represents the standard
deviation of PPP-ratios across country-regions, highlighting products
with the most significant cross-country variation.

------------------------------------------------------------------------

**Example using CPI microdata**

> 🚧 Mock-up code only as comprehensive list of average product prices
> across multiple countries is currently not available.

``` r
# Random data
set.seed(123)
df_xr2 <- rbind(
  df_xr,
  df_xr |> mutate(
    `Product code` = `Product code` + 10,
    `Average price of product` = `Average price of product` + runif(3, 1, 2)
  )
) |>
  select(-`XR USD`)

# Calculations
df_out <- df_xr2 |> 
valid_PPPratio(Year = "Year",
                           `Product code` = "Product code",
                           Region = "Region",
                           `Average price of product` = "Average price of product") 

# Final table
df_out |>
  gt() |>
  tab_header(
    title = md("**PPP-ratio Table**"),
    subtitle = md("Example for two items, **DE, UK, CZ**")
  ) |>
  fmt_number(
    columns = -c(Year, `Product code`),
    decimals = 1
  ) |>
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) 
```

[TABLE]

------------------------------------------------------------------------

## Validation of alternative data sources

When official data required for the calculation of SPPPs are
unavailable, alternative data sources are employed. Examples include
historical price quotations obtained from private insurers’ websites and
other relevant non-official datasets.

The use of alternative data sources depends on the type and availability
of data and may vary across cases and countries. Validation of such
sources follows two main steps:

- **Plausibility validation**
- **Statistical validation**

**Plausibility validation** assesses the credibility of the identified
data source and the reasonableness of the information it contains. This
process involves cross-referencing data with additional alternative or
official sources. Once the the numerically most credible source is
identified, its plausibility is further verified through expert
consultation with project counterparts, researchers, and—most
importantly—the national statistical offices (NSOs) of the respective
countries. Only data sources deemed credible by experts proceed to the
next stage of processing.

**Statistical validation** encompasses the analytical checks described
in this vignette. However, depending on the nature, structure, and
completeness of the alternative data source, the extent of statistical
validation may be limited—or, in exceptional cases, not feasible. In
such instances, greater emphasis is placed on expert-led plausibility
validation to ensure the integrity of the data used.

## Basic heading SPPPs validation

### Numerical

- Ensures that prices are consistent not only within basic headings but
  also at aggregate level *across* basic headings. This can, for
  example, help address cross-country measurement inconsistency

- This is done using Dikhanov tables (World Bank 2013, 261/267), which
  consist of three parts:

- *Part 1*: Summary information (PPPs, SDs, price level) by country for
  the aggregate

- *Part 2*: Contained basic headings (same info as part 1).

- *Part 3*: CPD residuals and product variation coefficients for
  products within basic headings

- *Part 1/2* Facilitates the comparisons of PPPs across basic headings;
  plausible variations in PPPs is expected across regions. Such
  variations would indicate that, say, alcoholic beverages in country A
  are x% higher than in country B.

- *Part 3* Ensure that the aggregate PPP variations are not driven
  certain basic headings, or isolated products therein, but are more
  reflective of common price-level differences across regions/countries.

- Extreme values are identified based on CPD residuals and PPP ratio
  threshold values described in (**tbl-thresholds?**); see also (World
  Bank 2013, 261):

| CPD residuals                            | PPP-ratios                       | Flag         |
|------------------------------------------|----------------------------------|--------------|
| Between −0.25 and 0.25                   | Between 78 and 128               | OK           |
| Between −0.75 and −0.25 or 0.25 and 0.75 | Between 47 and 78 or 128 and 212 | *Flag 1*     |
| Between −2.0 and −0.75 or 0.75 and 2.0   | Between 14 and 47 or 212 and 739 | **Fag 2**    |
| Less than −2.0 or greater than 2.0       | Less than 14 or greater than 739 | ***Flag 3*** |

Threshold values: CPD residuals and PPP-ratios {#tbl-thresholds}

> 🚧 Code to be completed.

### Visualisation

- Examines the basic heading PPPs within countries, and by country
  within each basic heading to point at countries/headings which may
  require further review (World Bank 2013, 279).

- Validation in this section is based on basic heading PPP estimates

> 🚧 Code to be completed.

- Box plots of Price Level index (PLI), which is the ratio of a
  purchasing power parity (PPP) conversion factor to the corresponding
  market exchange rate between two countries by country and by basic
  heading.

## Expenditure weights validation

In line with (World Bank 2013, 285), within-country basic heading
expenditures and shares are reviewed for the following:

- Completeness – ensuring that, with few exceptions, expenditures are
  recorded for every basic heading.

- Plausibility – by comparing per capita values and expenditure shares
  across basic headings.

- Temporal consistency – by examining the coherence of expenditure
  breakdowns across different years.

The review process includes the following checks:

- Calculating total and per capita expenditure values, deriving
  expenditure shares, and comparing these shares across countries, using
  Table 10.1 in (World Bank 2013, 286) as a reference.

- Comparing minimum, maximum, and median values at the basic heading
  level to identify potential anomalies or inconsistencies.

> 🚧 Code to be completed.

## Beyond Basic Heading SPPPs validation

### Bilateral PPPs

- Next step of validation concerns validation of PPPs using expenditure
  weights together (World Bank 2013, 287)

- Objective is to check if there is a problem with the basic headings
  PPP and expenditures, or, in case the data is valid, there is an
  estimation issue.

- (World Bank 2013, 287) discusses various validations steps that can be
  used to validate the Fisher PPPs.

- Validation using the Laspeyres and Paasche quantity index and
  Paasche-Laspeyres spread (PLS) following (**hill2011cluster?**), who
  proposes computing upper and lower price and quantity relatives to
  determine whether the large values in the PLS are caused by PPPs or
  expenditure outliers. Basic headings with large upper or lower
  quantity or price relatives should be further examined.

\\ PLS\_{jk}^S = \frac{max(P_P^{jk}, P_L^{jk})}{min(P_P^{jk}, P_L^{jk})}
\\

\\ Q_L^{jk} = \frac{ \sum^n\_{i=1} p_i^j \times q_i^k }{ \sum^n\_{i=1}
p_i^j \times q_i^j} \\

\\ Q_P^{jk} = \frac{ \sum^n\_{i=1} p_i^k \times q_i^k }{ \sum^n\_{i=1}
p_i^k \times q_i^j} \\

> 🚧 Code to be completed.

## Multilateral PPPs

## References

ICP. 2021. “A Guide to the Compilation of Subnational Purchasing Power
Parities (PPPs).”
<https://thedocs.worldbank.org/en/doc/5064f2288436664bc8f9811c8a5b8c55-0050022021/original/Guide-Subnational-PPPs.pdf>.

World Bank. 2013. *Measuring the Real Size of the World Economy: The
Framework, Methodology, and Results of the International Comparison
Program ICP*. Washington DC: World Bank.
<https://thedocs.worldbank.org/en/doc/927971487091799574-0050022017/original/ICPBookeBookFINAL.pdf>.
