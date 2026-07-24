## code to prepare `sampledata` datasets
## Generate data with pricelevels package

set.seed(123)

R <- 5 # number of regions
B <- 5 # number of product groups
N <- 5 # number of products

sampledata_one_year <- pricelevels::rdata(
  R = R, B = B, N = N,
  weights = ~ b + r,
  settings = list(par.sd = c(
    lnP = 0.1,
    pi = exp(1),
    delta = 0.5,
    error = 0.8
  ))
) %>%
  tibble::as_tibble() %>%
  dplyr::rename(heading = group, exp_wght = weight) %>%
  dplyr::mutate(
    heading = as.factor(paste("heading", heading, sep = "_")),
    region = as.factor(paste("region", region, sep = "_")),
    product = as.factor(paste("item", product, sep = "_"))
  )

## Sample data price quote for one year
sampledata_prices <- sampledata_one_year %>%
  dplyr::select(heading, region, product, price)

usethis::use_data(sampledata_prices, overwrite = TRUE)

## Corresponding weights in separate object
sampledata_weights <- sampledata_one_year %>%
  dplyr::distinct(heading, region, .keep_all = TRUE) %>%
  dplyr::select(heading, region, exp_wght)

usethis::use_data(sampledata_weights, overwrite = TRUE)

## Multi-period version of the data for more complex examples
sampledata_multi_period <- sampledata_one_year |>
  dplyr::mutate(period = "period_1") |>
  dplyr::bind_rows(pricelevels::rdata(
    R = R, B = B, N = N,
    weights = ~ b + r,
    settings = list(par.sd = c(lnP = 0.1, pi = exp(1), delta = 0.5, error = 0.8))
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(heading = group, exp_wght = weight) %>%
    dplyr::select(heading, region, product, price) %>%
    dplyr::mutate(
      heading = as.factor(paste("heading", heading, sep = "_")),
      region = as.factor(paste("region", region, sep = "_")),
      product = as.factor(paste("item", product, sep = "_")),
      period = "period_2"
    )) |>
  dplyr::select(period, heading, region, product, price, quantity, sale, exp_wght)

usethis::use_data(sampledata_multi_period, overwrite = TRUE)
