# Validate `valid_index_data` ---------------
test_that("Valid_index_data: subnational price index is negative", {
  expect_error(
    tibble::tibble(
      region = "region A",
      product = "product 1",
      ppp_bh = -0.2,
      exp_wght = 1
    ) |>
      valid_index_data()
  )
})

test_that("Valid_index_data: weights are negative", {
  expect_error(
    tibble::tibble(
      region = "region A",
      product = "product 1",
      ppp_bh = 0.2,
      exp_wght = -0.1
    ) |>
      valid_index_data()
  )
})

test_that("Valid_index_data: weights do not sum to 1", {
  expect_error(
    tibble::tibble(
      region = "region A",
      product = "product 1",
      ppp_bh = 0.2,
      exp_wght = 0.1
    ) |>
      valid_index_data()
  )
})

test_that("Valid_index_data: weights sum to > 1", {
  expect_error(
    tibble::tibble(
      region = "region A",
      product = c("product 1", "product 2"),
      ppp_bh = 0.2,
      exp_wght = 0.6
    ) |>
      valid_index_data()
  )
})


# Validate `matrix_generator` ---------------
test_that("matrix_generator: check simple matrix generation", {
  test_matrix <- tibble::tibble(
    region = "region A",
    product = c("product 1", "product 2"),
    values = 0.2
  ) |>
    matrix_generator()

  expect_all_equal(
    test_matrix[1, 1],
    0.2
  )
})
