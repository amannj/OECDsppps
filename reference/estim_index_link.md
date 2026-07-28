# CPD estimation to index calculation linking function

`estim_index_link()` enables linking CPD estimation to index calculation
within one pipe. Can fill in missing basic-heading PPPs with a value
given by the user.

## Usage

``` r
estim_index_link(
  data_sppps,
  data_weights,
  product_heading = "product_heading ",
  region = "region",
  sPPP = "sPPP",
  weights = "weight",
  complete_sppp = NA,
  complete_sppp_message = "short"
)
```

## Arguments

- data_sppps:

  Data frame, data table or tibble containing at least three columns
  identifying region, product and respective sPPPs

- data_weights:

  Data frame, data table or tibble containing at least three columns
  identifying region, product and expenditure weights

- product_heading:

  column containing the product heading, typically following the COICOP
  classification

- region:

  Identifier for regions

- sPPP:

  Identifier for the basix heading sPPPs

- weights:

  Identifier for expenditure weights

- complete_sppp:

  value to be imputed for missing basic-heading PPPs

- complete_sppp_message:

  specifies the length of the warning message triggered by imputing PPPs
  through complete_sppp. When set to "short", the function prints a
  warning that some PPPs were imputed. Complete list of all
  region/heading pairs is obtained by setting it to "full".

## Value

Returns a data frame containing the variables indicating the region
("region"), basic heading ("product"), basic-heading PPP ("ppp_bh"), and
expenditure weights ("exp_wght"). This output can be directly fed into
[`index_laspeyres()`](https://amannj.github.io/OECDsppps/reference/index_laspeyres.md),
[`index_paasche()`](https://amannj.github.io/OECDsppps/reference/index_paasche.md),
[`index_fisher()`](https://amannj.github.io/OECDsppps/reference/index_fisher.md),
and
[`index_geks()`](https://amannj.github.io/OECDsppps/reference/index_geks.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate the price and weight data and estimate CPD at basic-heading level
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
    data_sppps = .,
    data_weights = dt1_wghts,
    product_heading = "group",
    region = "region",
    sPPP = "sPPP",
    weights = "weight",
    complete_sppp = NA
  )


# Missing regional values
# -> returns an incomplete standardized data frame

dt1_basic_headings %>%
  filter(!(region %in% c("1", "2") & group == "1")) %>%
  estim_index_link(
    data_sppps = .,
    data_weights = dt1_wghts,
    product_heading = "group",
    region = "region",
    sPPP = "sPPP",
    weights = "weight",
    complete_sppp = NA
  )


# Missing regional values: Imputation
# -> returns a complete standardised data frame with a warnings,
#    listing the region/heading imputations
dt1_basic_headings %>%
  filter(!(region %in% c("1", "2") & group == "1")) %>%
  estim_index_link(
    data_sppps = .,
    data_weights = dt1_wghts,
    product_heading = "group",
    region = "region",
    sPPP = "sPPP",
    weights = "weight",
    complete_sppp = 1
  )
} # }
```
