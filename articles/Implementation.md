# Implementation

``` r
library(OECDsppps)
```

## Overview

This vignette provides a fully documented production pipeline,
describing the data, processing, validation, aggregation and estimation
of the multiple data sources for the purpose of constructing subnational
PPPs in the testing countries. Data processing, validation, aggregation,
and estimation follow the international recommendations ICP
([2021](#ref-icp2021)).

The production pipeline covers the country-level data validation, the
aggregation and estimation of subnational purchasing power parities and
the harmonisation of these estimates to make sPPP indicators comparable
across countries ([Table 1](#tbl-implementation)). Only the raw data
validation, which derives a standard structure across all testing
countries, remains country- and dataset-specific. The individual stages
of the production pipeline are discussed in the subsequent paragraphs.

|                                                                                         | By whom                        | Using `OECDsppps` | Notes |
|-----------------------------------------------------------------------------------------|--------------------------------|-------------------|-------|
| Raw data processing (country by country)                                                | OECD or country representative | No                |       |
| Data validation (country by country)                                                    | OECD or country representative | Yes               |       |
| Aggregation and estimation of subnational purchasing power parities (country by country | OECD                           | Yes               |       |
| Making subnational purchasing power parity indicators comparable across countries       | OECD                           | Yes               |       |

Table 1: Implementation pipeline

------------------------------------------------------------------------

> 🚧 Additional sections remain work in progress.

------------------------------------------------------------------------

## References

European Union/OECD. 2024. *Eurostat-OECD Methodological Manual on
Purchasing Power Parities (2023 Edition)*. OECD Publishing, Paris.
<https://doi.org/10.2785/384854>.

ICP. 2021. “A Guide to the Compilation of Subnational Purchasing Power
Parities (PPPs).”
<https://thedocs.worldbank.org/en/doc/5064f2288436664bc8f9811c8a5b8c55-0050022021/original/Guide-Subnational-PPPs.pdf>.

World Bank. 2013. *Measuring the Real Size of the World Economy: The
Framework, Methodology, and Results of the International Comparison
Program ICP*. Washington DC: World Bank.
<https://thedocs.worldbank.org/en/doc/927971487091799574-0050022017/original/ICPBookeBookFINAL.pdf>.
