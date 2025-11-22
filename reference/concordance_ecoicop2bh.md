# Concordance table from ECOICOP1 to Eurostat PPP Basic Headings

A concordance table to map European Classification of Individual
Consumption according to Purpose, version 1 (ECOICOP1) to Basic Headings
(BH) of the Eurostat PPP list of products for consumer goods and
services

## Usage

``` r
concordance_ecoicop2bh
```

## Format

### `concordance_ecoicop2bh`

A data frame with 358 rows and 5 columns containing the concordance to
map ECOICOP1s onto BHs.

- ecoicop1_code:

  ECOICOP1 code

- ecoicop1_name:

  ECOICOP1 description

- bh_code:

  Basic headings code

- bh_name:

  Basic headings description

- type:

  Type of concordance:

&nbsp;

- `1:1` direct concordance between ECOICOP1 and BH

- `N:1` multiple ECOICOP1 onto one BH:

  - e.g. ECOICOP1s *02.1.1.1*-Spirits and liqueurs and
    *02.1.1.2*-Alcoholic soft drinks onto BH *A.02.1.1.0*-Spirits

- `1:N` one ECOICOP1 onto multiple BHs:

  - e.g. ECOICOP1 *06.3*-Hospital services onto BHs *A.06.3.0.1*-General
    hospitals *A.06.3.0.2*-Mental health and substance abuse hospitals;
    *A.06.3.0.3*-Speciality hospitals and *A.06.3.0.4*-Nursing and
    residential care facilities

- `NA` not available: BHs go beyond household consumption

## Source

[European Classification of Individual Consumption according to Purpose,
version 1
(ECOICOP1)](https://showvoc.op.europa.eu/#/datasets/ESTAT_European_Classification_of_Individual_Consumption_according_to_Purpose_%28ECOICOP%29/data)
and [Eurostat PPP list of products for consumer goods and
services](https://ec.europa.eu/eurostat/web/purchasing-power-parities/methodology)

## Examples

``` r
concordance_ecoicop2bh |> dim()
#> [1] 358   5
concordance_ecoicop2bh |> names()
#> [1] "ecoicop1_code" "ecoicop1_name" "bh_code"       "bh_name"      
#> [5] "type"         
concordance_ecoicop2bh |> dplyr::distinct(type)
#> # A tibble: 4 × 1
#>   type 
#>   <chr>
#> 1 1:1  
#> 2 N:1  
#> 3 1:N  
#> 4 NA   
```
