# OECDsppps

`OECDsppps` provides functionalities for calculating subnational
Purchasing Power Parities (sPPPs) in OECD regions as part of the project
[Towards measuring purchasing power parity across OECD
regions](https://www.oecd.org/en/about/projects/towards-a-better-measuring-of-subnational-consumer-prices-and-purchasing-parity.html).

This documentation describes the processing, validation, aggregation and
estimation for the purpose of constructing subnational PPPs (sPPPs).

## Installation

⚠️ This package is still under development and not available on CRAN.

Install from source using
[devtools](https://cran.r-project.org/web/packages/devtools/index.html)
by running:

``` r

devtools::install_github('amannj/OECDsppps')
```

## Documentation

See tab **Articles** for detailed package documentation:

- [Implementation](https://amannj.github.io/OECDsppps/articles/Implementation.html) -
  discusses the production pipeline to create subnational PPPs

- [Validation](https://amannj.github.io/OECDsppps/articles/Validation.html) -
  validate price statistics at various levels of aggregation, from the
  initial item-level price quotes to the basic heading level and
  upwards, as well as comparing household expenditure weights across
  regions

- [Estimation](https://amannj.github.io/OECDsppps/articles/Estimation.html) -
  estimating subnational Purchasing Power Parities from item-level
  prices at basic heading level and beyond

`OECDsppps` is available in R, but section [Alternative
Software](https://amannj.github.io/OECDsppps/articles/altSoftware.html)
discusses how the package can be integrated into a Python or SAS
workflow.
