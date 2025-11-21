# Concordance table from ECOICOP1 to Basic Headings

A concordance table to map ECOICOP1 to basic headings

## Usage

``` r
concordance_ecoicop2bh
```

## Format

### `concordance_ecoicop2bh`

A data frame with 358 rows and 5 columns containing the concordance to
map ECOICOP1 to basic headings.

- ecoicop1_code:

  ECOICOP1 code

- ecoicop1_name:

  ECOICOP1 description

- bh_code:

  Basic headings code

- bh_name:

  Basic headings description

- type :

  Type of concordance; i.e., 1:1 concordance between ECOICOP1 and basic
  headings; N:1 multiple ECOICOP1 onto 1 basic heading; 1:N one ECOICOP1
  onto multiple basic headings; NA not available

## Source

to be completed.

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
