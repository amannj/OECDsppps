# Validate `index_laspeyres` ---------------
test_that("index_laspeyres: check matrix calculations", {
  lp <- tibble::tibble(
    region = c("region A", "region A", "region B", "region B"),
    product = c("product 1", "product 2", "product 1", "product 2"),
    ppp_bh = c(0.5, 0.7, 0.6, 0.9),
    exp_wght = c(0.5, 0.5, 0.6, 0.4)
  ) |>
    index_laspeyres(
      region = "region",
      product = "product",
      ppp_bh = "ppp_bh",
      exp_wght = "exp_wght"
    ) |>
    dplyr::pull(laspeyres_index)
  expect_equal(round(lp, 4), c(1.00, 1.2429, 0.8111, 1.00))
})

# Validate `index_paasche` ---------------
test_that("index_paasche: check matrix calculations", {
  pp <- tibble::tibble(
    region = c("region A", "region A", "region B", "region B"),
    product = c("product 1", "product 2", "product 1", "product 2"),
    ppp_bh = c(0.5, 0.7, 0.6, 0.9),
    exp_wght = c(0.5, 0.5, 0.6, 0.4)
  ) |>
    index_paasche(
      region = "region",
      product = "product",
      ppp_bh = "ppp_bh",
      exp_wght = "exp_wght"
    ) |>
    dplyr::pull(paasche_index)
  expect_equal(round(pp, 4), c(1.00, 1.2329, 0.8046, 1.00))
})


# Validate `index_fisher` ---------------
test_that("index_fisher: check matrix calculations", {
  fp <- tibble::tibble(
    region = c("region A", "region A", "region B", "region B"),
    product = c("product 1", "product 2", "product 1", "product 2"),
    ppp_bh = c(0.5, 0.7, 0.6, 0.9),
    exp_wght = c(0.5, 0.5, 0.6, 0.4)
  ) |>
    index_fisher(
      region = "region",
      product = "product",
      ppp_bh = "ppp_bh",
      exp_wght = "exp_wght"
    ) |>
    dplyr::pull(fisher_index)
  expect_equal(round(fp, 4), c(1.00, 1.2379, 0.8078, 1.00))
})

# Validate `index_geks` ---------------
test_that("index_geks: check matrix calculations", {
  gp <- tibble::tibble(
    region = c("region A", "region A", "region B", "region B"),
    product = c("product 1", "product 2", "product 1", "product 2"),
    ppp_bh = c(0.5, 0.7, 0.6, 0.9),
    exp_wght = c(0.5, 0.5, 0.6, 0.4)
  ) |>
    index_geks(
      region = "region",
      product = "product",
      ppp_bh = "ppp_bh",
      exp_wght = "exp_wght"
    ) |>
    dplyr::pull(geks_index)
  expect_equal(round(gp, 4), c(1.00, 1.2379, 0.8078, 1.00))
})
