# COICOP 2018 - COICOP 1999 correspondence table

Correspondence table between COICOP 2018 and COICOP 1999. Note that
COICOP 2018 corresponds to ECOICOP 2.

## Usage

``` r
correspondence_coicop18_coicop99
```

## Format

A data frame with 688 rows and 4 columns containing the variables

- "coicop18_code " (COICOP 2018 code),

- "coicop18_description" (COICOP 2018 description),

- "coicop99_code" (COICOP 1999 code),

- "coicop99_description" (COICOP 1999 description),

- "comment" (classification comments)

## Source

See Statistics Lithuania for the [correspondence
table](https://osp.stat.gov.lt/en_GB/individualaus-vartojimo-islaidu-pagal-paskirti-klasifikatorius-coicop),
as well as the United Nattions Statistics Division for more information
on [COICOP
1999](https://unstats.un.org/unsd/classifications/Family/Detail/5) and
[COICOP
2018](https://unstats.un.org/unsd/classifications/Family/Detail/2094),
respectively.

## References

There are no references for Rd macro `\insertAllCites` on this help
page.

## Examples

``` r
correspondence_coicop18_coicop99 |> dim()
#> [1] 688   5
correspondence_coicop18_coicop99 |> names()
#> [1] "coicop18_code"        "coicop18_description" "coicop99_code"       
#> [4] "coicop99_description" "comment"             
correspondence_coicop18_coicop99 |> head()
#> # A tibble: 6 × 5
#>   coicop18_code coicop18_description  coicop99_code coicop99_description comment
#>   <chr>         <chr>                 <chr>         <chr>                <chr>  
#> 1 01            Food and non-alcohol… -             -                    NA     
#> 2 01.1          Food                  01.1          Food                 NA     
#> 3 01.1.1        Cereals and cereal p… 01.1.1        Bread and cereals (… NA     
#> 4 01.1.1.1      Cereals (ND)          01.1.1        Bread and cereals (… NA     
#> 5 01.1.1.2      Flour of cereals (ND) 01.1.1        Bread and cereals (… NA     
#> 6 01.1.1.3      Bread and bakery pro… 01.1.1        Bread and cereals (… NA     
```
