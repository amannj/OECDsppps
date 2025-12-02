# Estimation

``` r
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(OECDsppps)
library(pricelevels)
library(data.table)
```

## Overview

**The estimation** of subnational PPPs (sPPPs) starts with the
item-level prices that are progressively aggregated to higher levels. In
addition to aggregate subnational price indices, sub-indices, for
example, at the COICOP division level, can highlight more granular
regional price level differences. This process aligns with current
recommendations; see ICP ([2021](#ref-icp2021)), World Bank
([2013](#ref-worldbank2013)) and European Union/OECD
([2024](#ref-europeanunionEurostatOECDMethodologicalManual2024)) for
more information.

### Estimation steps

The estimation steps are:

1.  [Estimation of basic headings using item-level prices](#sec-step1),
    where price data are aggregated up to the level of basic headings,
    generally without the use of expenditure weights (unless such
    information is available, for instance, when retail scanner data
    provide detailed transaction-level records).

2.  [Estimation of higher level aggregates using basic heading
    indices](#sec-step2) to higher levels of the classification
    hierarchy, at which point household expenditure data are accessible
    and can be applied as weighting factors

### Estimation methods

The choice of estimation method depends on the availability of data and
the analytical objectives of the subnational PPP exercise. When the aim
is to ensure cross-country comparability and to exploit micro-level
price information, the **Country-Product-Dummy (CPD) -
Gini-Éltetö-Köves-Szulc (GEKS)** approach offers a flexible framework
for estimating basic heading indices ([ICP 2021](#ref-icp2021)).

In contrast, the **Eurostat OECD method (Jevons-GEKS)** methodology
employed at the national level imposes more stringent data requirements.
Such requirements may be more difficult to meet in the context of
deriving subnational PPPs based on existing microdata, particularly
regarding the representativeness of individual products across all
regions ([European Union/OECD
2024](#ref-europeanunionEurostatOECDMethodologicalManual2024)).

The estimation procedure in this vignette follows the CPD-GEKS approach
and highlights its similarities and differences with the Jevons-GEKS
approach whenever instructive. For a more comprehensive discussion on
price indices see World Bank ([2013](#ref-worldbank2013)) and European
Union/OECD
([2024](#ref-europeanunionEurostatOECDMethodologicalManual2024)).

### Data used

Data used in this vignette is taken from official UK microdata from the
United Kingdom Office for National Statistics (ONS). Similar data was
recently used in Hearne and Bailey ([2025](#ref-hearne2025)) and is
publicly available:

- [`uk_cpi()`](https://amannj.github.io/OECDsppps/reference/uk_cpi.md)
  is a snipped of the [UK CPI
  microdata](https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes)
  containing two products: White sliced loaf branded 750 grams
  (COICOP 1010103) and carpenter hourly rate (COICOP 410518).

- `uk_hhx()` is a snipped of the regional UK household expenditure data
  …🚧 work in progress….

------------------------------------------------------------------------

## 1 Estimation of basic headings using item-level prices

### 1.1 Overview

The CPD method is a regression-based approach for estimating price
parities. The underlying statistical model is

\\ p\_{ij} = PPP_j \times p_i \times \epsilon\_{ij} \tag{1}\\

where \\PPP_j\\ is the purchasing power parity of an arbitrary region
\\j\\, (\\r = 1,...,j,...,R\\), \\p_i\\ is the average regional price of
an arbitrary commodity \\i\\, (\\n = 1, ..., i, ... N\\), and
\\\epsilon\_{ij}\\ is a independently and identically distributed random
variable.[¹](#fn1) Taking logs of [Equation 1](#eq-ppp1) yields

\\ \begin{aligned} ln p\_{ij} & = ln PPP_j + ln p_i + ln \epsilon\_{ij}
\\ & = \alpha_j + \gamma_i + ln \varepsilon\_{ij} \end{aligned}
\tag{2}\\

where \\\alpha_j\\ is the the price level of region \\j\\ relative to
all other regions in the comparison. \\\alpha_j\\ can also be expressed
relative to a reference region, for example, the national price level.
Then, \\\alpha_j\\ represents the subnational purchasing power parity of
region \\j\\ given by \\\hat{PPP}\_j = exp(\hat{\alpha}\_j)\\.

### 1.2 Estimation

The CPD model in [Equation 2](#eq-ppp2) may be interpreted as a
fixed-effects specification, in which country effects yield estimates of
subnational purchasing power parities, while commodity-specific effects
generate estimates of subnational price levels. The model can be written
as a regression equation in which all explanatory variables take the
form of dummy indicators for each region and commodity.

\\ \begin{aligned} ln p\_{ij} = & \alpha_1 D_1 + ... + \alpha_j D_j +
... +\alpha_R D_R + \\ & \eta_1 \mathcal{D}\_1 + ... + \eta_i
\mathcal{D}\_i + ... + \eta_N \mathcal{D}\_N + \varepsilon\_{ij}
\end{aligned} \tag{3}\\

and \\\varepsilon\_{ij}\\ are independently and identically (normally)
distributed with a zero mean and variance \\\sigma^2\\, that is,
\\\varepsilon\_{ij} \sim N(0, \sigma^2)\\. The variables of interest,
\\PPP_j\\, can be estimated through the parameters \\\hat{\alpha}\_j\\
using ordinary least squares (OLS); see [Section 1.1.3
Implementation](#sec-implementation).

### 1.3 Implementation

#### 1.3.1 Using `pricelevels`

The `pricelevels` package ([Weinand 2025](#ref-pricelevels)) can be used
to estimate subnational purchasing power parities, which can also be
computed using standard OLS as outlined in examples [1](#sec-example1)
and [2](#sec-example2).

------------------------------------------------------------------------

#### Example 1: One product, two regions

``` r
# Data
df1 <- data.table(
  region = as.factor(c(1, 2, 1, 2)),
  product = as.factor(c(1, 1, 1, 1)),
  price = c(25, 28, 23, 26)
)
```

In the CPD regression model, the intercept corresponds to the
cross-regional average.

``` r
# Calculate cross-regional price average
df1 |>
  as_tibble() |>
  summarise(mean(price))
#> # A tibble: 1 × 1
#>   `mean(price)`
#>           <dbl>
#> 1          25.5
log(25.5)
#> [1] 3.238678
```

And the coefficient estimate is the price ratio of the average regional
prices.

``` r
# Calculate regional price averages and price relative
df1 |>
  as_tibble() |>
  group_by(region) |>
  summarise(mean(price))
#> # A tibble: 2 × 2
#>   region `mean(price)`
#>   <fct>          <dbl>
#> 1 1                 24
#> 2 2                 27
27 / 24
#> [1] 1.125
```

The same results can be obtained using
[`cpd()`](https://rdrr.io/pkg/pricelevels/man/cpd.html) from the
`pricelevels` package.

``` r
# With `pricelevels`- estimation with respect to regional average
df1[, cpd(p = price, r = region, n = product, q = NULL, base = NULL)]
#>        1        2 
#> 0.942809 1.060660
1.060660 / 0.942809
#> [1] 1.125

# With `pricelevels`- estimation with respect to region 1
df1[, cpd(p = price, r = region, n = product, q = NULL, base = "1")]
#>     1     2 
#> 1.000 1.125

# With `pricelevels`- estimation output
df1[, cpd(
  p = price, r = region, n = product, q = NULL, base = NULL,
  simplify = FALSE
)]
#> 
#> Call:
#> stats::lm(formula = cpd_mod, data = pdata, singular.ok = FALSE)
#> 
#> Coefficients:
#> (Intercept)        lnP.1  
#>     3.23695     -0.05889
exp(-0.05889)
#> [1] 0.9428105
```

And also with standard OLS.

``` r
# With OLS
pdata <- df1

# Model: add intercept to for price levels relative to base
cpd_mod <- log(price) ~ region + 1

# Transformation: equiv to "mean centring" of continuous predictor: shifting
## dummy encoding to -1/1 intercept is mean across all prices.
## Region1 is the 'main effect', i.e., the difference between levels of a given
## factor (region) across all other factors.
contrasts(x = pdata$region) <- contr.sum(levels(pdata$region))
colnames(contrasts(x = pdata$region)) <- levels(pdata$region)[-nlevels(pdata$region)]

# OLS regression
out <- lm(formula = cpd_mod, data = pdata)
out
#> 
#> Call:
#> lm(formula = cpd_mod, data = pdata)
#> 
#> Coefficients:
#> (Intercept)      region1  
#>     3.23617     -0.05898
exp(dummy.coef(out)[["region"]])
#>        1        2 
#> 0.942723 1.060757
exp(3.23617 - 0.05898)
#> [1] 23.97928
exp(3.23617 + 0.05898)
#> [1] 26.98146
```

#### Example 2: Two products, two regions

The procedure is identical for the more general case.

``` r
# Data
df2 <- data.table(
  region = as.factor(c(1, 2, 1, 2)),
  product = as.factor(c(1, 1, 2, 2)),
  price = c(25, 28, 23, 26)
)

# With `pricelevels` ------
## Estimation with respect to regional average
df2[, cpd(p = price, r = region, n = product, q = NULL, base = NULL)]
#>        1        2 
#> 0.942723 1.060757
1.060757 / 0.942723
#> [1] 1.125205

## Estimation with respect to region 1
df2[, cpd(p = price, r = region, n = product, q = NULL, base = "1")]
#>        1        2 
#> 1.000000 1.125205

## Estimation output
df2[, cpd(
  p = price, r = region, n = product, q = NULL, base = NULL,
  simplify = FALSE
)]
#> 
#> Call:
#> stats::lm(formula = cpd_mod, data = pdata, singular.ok = FALSE)
#> 
#> Coefficients:
#>     pi.1      pi.2     lnP.1  
#>  3.27554   3.19680  -0.05898
exp(-0.05898)
#> [1] 0.9427256

# OLS ------
## Data
pdata <- df2
## Model
cpd_mod <- log(price) ~ product + region - 1

## Transformation: equiv to "mean centring"
contrasts(x = pdata$region) <- contr.sum(levels(pdata$region))
colnames(contrasts(x = pdata$region)) <- levels(pdata$region)[-nlevels(pdata$region)]

## OLS regression
out <- lm(formula = cpd_mod, data = pdata)
out
#> 
#> Call:
#> lm(formula = cpd_mod, data = pdata)
#> 
#> Coefficients:
#> product1  product2   region1  
#>  3.27554   3.19680  -0.05898
exp(dummy.coef(out)[["region"]])
#>        1        2 
#> 0.942723 1.060757
```

------------------------------------------------------------------------

#### 1.3.2 Using `estim_cpd()`

Additionally, the function
[`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
provides an alternative estimation approach to provide numerically
identical sPPPs estimates.

#### Example 3: Generic - Multiple products, and regions

``` r
# Generate data with `pricelevels` -------
set.seed(123)
R <- 5 # number of regions
B <- 5 # number of product groups
N <- 5 # number of products
dt1 <- pricelevels::rdata(R = R, B = B, N = N)

# Estimating sPPPs with `pricelevels`, no weights --------
dt1[, cpd(p = price, r = region, n = product)]
#>         1         2         3         4         5 
#> 1.0163465 0.8543248 1.1667509 0.9950373 0.9920137

# Estimating sPPPs with `estim_cpd()`, no weights ---------
dt1 |>
  estim_cpd(
    region = "region",
    product = "product",
    price = "price"
  ) |>
  pull("sPPP")
#>         1         2         3         4         5 
#> 1.0163465 0.8543248 1.1667509 0.9950373 0.9920137
```

The behaviour of adding estimation weights is identical across both
packages; see
[`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
for more information.

``` r
# Estimating sPPPs with `pricelevels`, with weights --------
dt1[, cpd(p = price, r = region, n = product, w = weight)]
#>         1         2         3         4         5 
#> 1.0187925 0.8460806 1.1784210 0.9964223 0.9880038

# Estimating sPPPs with `estim_cpd()`, with weights ---------
dt1 |>
  estim_cpd(
    region = "region",
    product = "product",
    price = "price",
    weights_cpd = 'weight'
  ) |>
  pull("sPPP")
#>         1         2         3         4         5 
#> 1.0187925 0.8460806 1.1784210 0.9964223 0.9880038
```

The function
[`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
also has the option to export extended regression output of the CPD
model with argument `output = "Full"`, which summarises the key
information of the estimate CPD model in a tidy
[`tibble()`](https://tibble.tidyverse.org/reference/tibble.html) using .
Information in the extended regression output is used to support the
validation of CPD-based subnational PPPs at the basic-heading level; see
[Validation](https://amannj.github.io/OECDsPPPs/articles/Validation.html#sec-tobh)
vignette.

``` r
# Estimating sPPPs with `estim_cpd()` and obtain standard errors ---------
dt1 |>
  estim_cpd(
    region = "region",
    product = "product",
    price = "price",
    output = "Full"
  ) |> 
  gt() |> 
  fmt_number(decimals = 1) |> sub_missing(missing_text = "")
```

[TABLE]

#### Example 4: UK microdata - Two products, multiple regions

``` r
# Take UK CPI microdata ---------
red <- uk_cpi |>
  filter(Year == "2018") |>
  select(
    region = "Region",
    product = "Product code",
    price = "Reference quantity price"
  ) |>
  mutate(
    region = as.factor(region),
    product = as.factor(product)
  )

# Estimating sPPPs with `estim_cpd()` ---------
red |> estim_cpd() |> pull("sPPP")
#> Duplicated region-product pairs found in data and no weights provided: Data is aggregated to region-product pairs using unweighted means.
#>             East Midlands           East of England                    London 
#>                 0.9291930                 1.0171431                 1.3164839 
#>                     North                North West          Northern Ireland 
#>                 0.9631195                 0.9757530                 0.9888085 
#>                  Scotland                South East                South West 
#>                 1.0521466                 0.9977087                 0.9331900 
#>                     Wales             West Midlands Yorkshire and the Humberl 
#>                 0.8612814                 1.0457363                 0.9802726

# Estimating sPPPs with `pricelevels` --------
as.data.table(red)[, cpd(p = price, r = region, n = product)]
#> Warning: Duplicated observations found and aggregated
#>             East Midlands           East of England                    London 
#>                 0.9291930                 1.0171431                 1.3164839 
#>                     North                North West          Northern Ireland 
#>                 0.9631195                 0.9757530                 0.9888085 
#>                  Scotland                South East                South West 
#>                 1.0521466                 0.9977087                 0.9331900 
#>                     Wales             West Midlands Yorkshire and the Humberl 
#>                 0.8612814                 1.0457363                 0.9802726
```

The function
[`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
provides the option to add aggregation weight in case duplicate
region-product pairs found in data through the `weights` argument; see
[`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
for more information.

``` r
# Estimating sPPPs with `estim_cpd()`, with aggregation weights ---------
red  |> mutate(w = 1) |> estim_cpd(weights = "w") |> pull("sPPP")
#> Duplicated region-product pairs found in data and no weights provided: Data is aggregated to region-product pairs using weighted means, with weights provided in `weights`.
#>             East Midlands           East of England                    London 
#>                 0.9291930                 1.0171431                 1.3164839 
#>                     North                North West          Northern Ireland 
#>                 0.9631195                 0.9757530                 0.9888085 
#>                  Scotland                South East                South West 
#>                 1.0521466                 0.9977087                 0.9331900 
#>                     Wales             West Midlands Yorkshire and the Humberl 
#>                 0.8612814                 1.0457363                 0.9802726

# Estimating sPPPs with `estim_cpd()`, with aggregation weights ---------
set.seed(123)
red |> 
  ## Add random weights
  mutate(w = runif(nrow(red), 0, 1)) |> 
  estim_cpd(weights = "w") |> pull("sPPP")
#> Duplicated region-product pairs found in data and no weights provided: Data is aggregated to region-product pairs using weighted means, with weights provided in `weights`.
#>             East Midlands           East of England                    London 
#>                 0.9273710                 1.0071653                 1.3190512 
#>                     North                North West          Northern Ireland 
#>                 0.9643659                 0.9701565                 0.9970916 
#>                  Scotland                South East                South West 
#>                 1.0489943                 1.0051813                 0.9315536 
#>                     Wales             West Midlands Yorkshire and the Humberl 
#>                 0.8701807                 1.0331752                 0.9852731
```

------------------------------------------------------------------------

## 2 Estimation of higher level aggregates using basic heading indices

------------------------------------------------------------------------

> 🚧 Additional sections remain work in progress.

------------------------------------------------------------------------

## References

European Union/OECD. 2024. *Eurostat-OECD Methodological Manual on
Purchasing Power Parities (2023 Edition)*. OECD Publishing, Paris.
<https://doi.org/10.2785/384854>.

Hearne, David, and David Bailey. 2025. “Regional Prices Reconsidered.”
*Regional Studies, Regional Science* 12 (1): 338–56.
<https://doi.org/10.1080/21681376.2025.2475115>.

ICP. 2021. “A Guide to the Compilation of Subnational Purchasing Power
Parities (PPPs).”
<https://thedocs.worldbank.org/en/doc/5064f2288436664bc8f9811c8a5b8c55-0050022021/original/Guide-Subnational-PPPs.pdf>.

Weinand, Sebastian. 2025. *Pricelevels: Spatial Price Level
Comparisons*. <https://doi.org/10.32614/CRAN.package.pricelevels>.

World Bank. 2013. *Measuring the Real Size of the World Economy: The
Framework, Methodology, and Results of the International Comparison
Program ICP*. Washington DC: World Bank.
<https://thedocs.worldbank.org/en/doc/927971487091799574-0050022017/original/ICPBookeBookFINAL.pdf>.

------------------------------------------------------------------------

1.  Model [Equation 1](#eq-ppp1) is not identified and requires
    parametrisation before it can be estimated.
