# Classification of Individual Consumption According to Purpose (ECOICOP)

COICOP is reference classification published by the United Nations
Statistics Division (UNSD) used in National accounts, Household Budget
surveys (HBS), Price Statistics (HICP) and Purchasing Power Parities
(PPP). Eurostat introduced a harmonised and revision of COICOP, yielding
a single harmonised COICOP classification at the five-digit level.

## Usage

``` r
ecoicop16_names
```

## Format

### `ecoicop16_names`

A data frame with 479 rows and 3 columns containing the code and
description of the ECOICOP classification at various levels of
aggregation, from *division* (COICOP2, e.g.,
`01 Food and non-alcoholic beverages`) down to *subclasses* (COICOP5,
e.g., `01.1.1.1 Rice`).

- code:

  ECOICOP code

- level:

  Hierarchical level of ECOICOP classification

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
#> [1] 479   3
ecoicop16_names |> names()
#> [1] "code"  "level" "name" 
```
