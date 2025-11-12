# Opposite of `%in%`

`%!in%` exclude rows with values specified in a vector.

## Usage

``` r
x %!in% y
```

## Arguments

- x:

  The first vector

- y:

  The second vector

## Value

Vector excluding values specified.

## Examples

``` r
 x <- c(1,2,3)
 y <-  c(2,3)
 x %!in% y
#> [1]  TRUE FALSE FALSE
```
