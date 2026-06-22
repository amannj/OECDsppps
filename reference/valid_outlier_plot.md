# sPPPs outlier plot

`valid_outlier_plot()` produces some simple validation plots to check
subnational PPP estimates for potential outliers

## Usage

``` r
valid_outlier_plot(
  data,
  sPPPs = "sPPP",
  title = NULL,
  facet_var = NULL,
  facet_ncol = NULL,
  facet_scale = "fixed",
  bins = 70,
  outlier_cutoffs = c(1.5, 0.5),
  xlim_range = NULL
)
```

## Arguments

- data:

  A data frame or tibble containing at least one column with the
  subnational Purchasing Power Parity indices

- sPPPs:

  Vector with subnational Purchasing Power Parities

- title:

  Option to add a plot title; default is NULL

- facet_var:

  Option to wraps a 1d sequence of panels into 2d based on the provided
  variable following ggplot2's `facet_wrap()`; default is NULL

- facet_ncol:

  Option to change the number of column of the created facets following
  ggplot2's `facet_wrap()` argument `ncol`; default is 2 if `facet_var`
  is used

- facet_scale:

  Option to change wheterhe the facet scales should be fixed (`"fixed"`,
  the default), free (`"free"`), or free in one dimension (`"free_x"`,
  `"free_y"`); default is `"fixed"`

- bins:

  Number of bins following ggplots' `geom_histogram()` argument `bins`;
  Default is `70`

- outlier_cutoffs:

  Cutoffs to highlight potential outliers in the plot and need to be
  provided as a vector as
  `outlier_cutoffs = c(upper_limit, lower_limit)`; default is `1.5` and
  `0.5`, i.e. `outlier_cutoffs = c(1.5, 0.5)`

- xlim_range:

  Limits for the x and y axes, following ggplot2's `coord_cartesian()`
  argument and need to be provided as a vector as
  `outlier_cutoffs = c(upper_limit, lower_limit)`; default is NULL

## Examples

``` r
if (FALSE) { # \dontrun{
uk_cpi |>
  select(Year,
    region = "Region",
    product = "Product code",
    price = "Reference quantity price"
  ) |>
  mutate(
    region = as.factor(region),
    product = as.factor(product)
  ) |>
  estim_cpd() |>
  valid_outlier_plot(
    title = "sPPPs outlier with adjusted outlier cutoffs",
    # Adjust outlier cutoffs (default is 1.5 and 0.5)
    outlier_cutoffs = c(1.1, 0.9)
  )
} # }
```
