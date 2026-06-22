# CPD estimation to index calculation linking function

`estim_index_link()` enables linking CPD estimation to index calculation
within one pipe. Can fill in missing basic heading PPPs with a value
given by the user.

## Usage

``` r
estim_index_link(
  data,
  data_weights = data_weights,
  basic_heading = "basic_heading",
  region = "region",
  sPPP = "sPPP",
  exp_wght = "weight",
  complete_sppp = NA
)
```

## Arguments

- data:

  Data frame, data table or tibble containing at least three columns
  identifying region, product and respective sPPPs

- data_weights:

  Data frame, data table or tibble containing at least three columns
  identifying region, product and expenditure weights

- basic_heading:

  column containing the basic heading identifier

- region:

  Identifier for regions

- sPPP:

  Identifier for the basix heading sPPPs

- exp_wght:

  Identifier for expenditure weights

- complete_sppp:

  value to be imputed for missing basic heading PPPs

## Value

Returns a data frame containing the variables indicating the region
("region"), basic heading ("product"), basic heading PPP ("ppp_bh"), and
expenditure weights ("exp_wght"). This output can be directly fed into
[`index_laspeyres()`](https://amannj.github.io/OECDsppps/reference/index_laspeyres.md),
[`index_paasche()`](https://amannj.github.io/OECDsppps/reference/index_paasche.md),
[`index_fisher()`](https://amannj.github.io/OECDsppps/reference/index_fisher.md),
and
[`index_geks()`](https://amannj.github.io/OECDsppps/reference/index_geks.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate the price and weight data and estimate CPD at basic headings
dt1 <- pricelevels::rdata(
  R = R, B = B, N = N,
  weights = ~ r + n,
  settings = list(par.sd = c(
    lnP = 0.1, pi = exp(1),
    delta = 0.5, error = 0.8
  ))
)

dt1_wghts <- dt1 %>%
  distinct(group, region, .keep_all = TRUE) %>%
  select(group, region, weight)

dt1_prices <- dt1 %>%
  dplyr::select(group, region, product, price)

dt1_basic_headings <- dt1_prices %>%
  group_by(group) %>%
  group_modify(~ {
    estim_cpd(.x,
      region = "region",
      product = "product",
      price = "price",
      output = "sPPP"
    )
  }) %>%
  ungroup()

# Complete data
# -> returns complete standardized data frame

dt1_basic_headings %>%
  estim_index_link(
    data = .,
    data_weights = dt1_wghts,
    basic_heading = "group",
    region = "region",
    sPPP = "sPPP",
    exp_wght = "weight",
    complete_sppp = NA
  )


# Missing regional values
# -> returns an incomplete standardized data frame

dt1_basic_headings %>%
  filter(!(region %in% c("1", "2") & group == "1")) %>%
  estim_index_link(
    data = .,
    data_weights = dt1_wghts,
    basic_heading = "group",
    region = "region",
    sPPP = "sPPP",
    exp_wght = "weight",
    complete_sppp = NA
  )


# Missing regional values: Imputation
# -> returns a complete standardised data frame with a warnings,
#    listing the region/heading imputations
dt1_basic_headings %>%
  filter(!(region %in% c("1", "2") & group == "1")) %>%
  estim_index_link(
    data = .,
    data_weights = dt1_wghts,
    basic_heading = "group",
    region = "region",
    sPPP = "sPPP",
    exp_wght = "weight",
    complete_sppp = 1
  )
} # }
```
