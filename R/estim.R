#' Country Product Dummy (CPD) regression model
#'
#' \loadmathjax
#' `estim_cpd` in  \pkg{OECDsppps} creates ...;
#' see *Details* and
#' \insertCite{worldbankMeasuringRealSize2013;textual}{OECDsppps},
#' for more information.
#'
#' **detailed** description goes
#' \mjseqn{i}, \mjseqn{P_{i}},
#' here
#' \mjdeqn{ratio-to-average = p_{ij}/\mu_j}{ratio-to-average = p_{ij}/\mu_j}
#'
#' @param data Data frame, data table or tibble containing at least three
#'  columns identifying region, product and individual item-level price quotes
#' @param region Identifier for regions (within or across countries)
#' @param product Product identifier
#' @param price Individual item-level price quotes; duplicated region-product
#' pairs are aggregated by way of averaging across region-product pairs.
#' @param base.region A character specifying the base to which the estimated
#' logarithmic regional price levels are expressed.
#' When NULL, they refer to the (unweighted) regional average,
#' similar to `contr.sum()`.
#' @param output Either "SPPP", which returns the estimated subnational
#' purchasing purchasing power parities, that is,
#' \mjseqn{\hat{SPPP}_r = exp(\hat{\alpha}_r)} or
#' "Std. Error", which returns the standard errors of the estimation;
#' default is "SPPP"
#'
#' @return A tibble (scalar)  with estimated subnational
#' purchasing purchasing power parities, that is,
#' \mjseqn{\hat{SPPP}_r = exp(\hat{\alpha}_r)}  if `output = "SPPS"` (default),
#' or vector containing the standard errors of the estimation
#' if `output = "Std. Error"`
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' df <- tibble(
#'   region = as.factor(c(1, 2, 1, 2)),
#'   product = as.factor(c(1, 1, 2, 2)),
#'   price = c(25, 28, 23, 26)
#' )
#'
#' estim_cpd(df)
#' estim_cpd(df, output = "Std. Error")
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom rlang :=
#' @export
estim_cpd <- function(data,
                      region = "region",
                      product = "product",
                      price = "price",
                      base.region = NULL,
                      output = "SPPP") {
  # Checks
  ## To be implemented
  ## change regions/products to factors
  ## check that prices are numeric
  ## no empty stuff

  ## Remove duplicates
  n_obs_raw <- data |> nrow()
  data <- data |>
    dplyr::group_by(.data[[region]], .data[[product]]) |>
    dplyr::summarise({{ price }} := mean(.data[[price]], na.rm = T),
      .groups = "drop"
    )

  # Dimensions
  n_obs <- data |> nrow()
  if (n_obs < n_obs_raw) {
    message("Duplicated region-product pairs found in data: Data is aggregated by averaging across region-product pairs.")
  }
  n_region <- data |>
    dplyr::distinct(region) |>
    nrow()
  n_product <- data |>
    dplyr::distinct(region) |>
    nrow()


  # Setting base region
  if (!is.null(base.region)) stop("Only estimation with respect to regional average currently implemented")

  # CPD regression formula
  ## Case 1: multiple regions, multiple products
  if (n_region > 1 & n_product > 1) {
    formula <- log(price) ~ product + region - 1
  }

  ## Case 2: one product, multiple regions
  if (n_product == 1) {
    formula <- log(price) ~  region + 1
  }

  ## Case 3: one regions
  if (n_region == 1) {
    stop("Only one region available. No regional comparison of prices possible.")
  }

  # Update contrasts
  if (is.null(base.region)) {
    stats::contrasts(x = data$region) <- stats::contr.sum(levels(data$region))
    colnames(stats::contrasts(x = data$region)) <- levels(data$region)[-nlevels(data$region)]
  } else {
    stats::contrasts(x = data$region) <- stats::contr.treatment(levels(data$region))
    colnames(stats::contrasts(x = data$region)) <- levels(data$region)[-1]
  }

  # Estimate
  est_out <- stats::lm(formula = formula, data = data, singular.ok = FALSE)

  # Output
  if (output == "SPPP") {
    out <- tibble(
      region = stats::dummy.coef(est_out)[["region"]] |> names(),
      "SPPP" = exp(stats::dummy.coef(est_out)[["region"]])
    )
  }
  if (output == "Std. Error") {
    m <- summary(est_out)
    out <- m$coefficients[nrow(m$coefficients), 2]
  }
  return(out)
}
