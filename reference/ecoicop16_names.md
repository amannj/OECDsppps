# Classification of Individual Consumption According to Purpose (ECOICOP 16)

COICOP is a reference classification published by the United Nations
Statistics Division (UNSD) used in National accounts, Household Budget
Surveys (HBS), Price Statistics (HICP) and Purchasing Power Parities
(PPP). Eurostat introduced a harmonised revision of COICOP, yielding a
single harmonised COICOP classification at the five-digit (sub-class)
level, referred to as “ECOICOP” (or "ECOICOP 1") for COICOP 1999
standard. The more recent COICOP 2018 standard is equivalent to ECOICOP
2.

## Usage

``` r
ecoicop16_names
```

## Format

### `ecoicop16_names`

A data frame with 479 rows and 4 columns containing the code and
description of the ECOICOP classification at various levels of
aggregation, from *division* (COICOP2, e.g.,
`01 Food and non-alcoholic beverages`) down to *subclasses* (COICOP5,
e.g., `01.1.1.1 Rice`).

- code:

  ECOICOP code

- coicop_level:

  Hierarchical level of ECOICOP classification, description

- coicop_level:

  Hierarchical level of ECOICOP classification, code

- description:

  ECOICOP description

## Source

[Classification of Individual Consumption According to Purpose
1999](https://unstats.un.org/unsd/classifications/Family/Detail/5) and
[eurostat Glossary:COICOP
HICP](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:COICOP_HICP)

## Examples

``` r
ecoicop16_names |> dim()
#> [1] 479   4
ecoicop16_names |> names()
#> [1] "code"              "coicop_level"      "coicop_level_code"
#> [4] "name"             
```
