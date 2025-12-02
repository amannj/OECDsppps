#' Country Product Dummy (CPD) regression model
#'
#' \loadmathjax
#' `estim_cpd` in  \pkg{OECDsppps} creates ...;
#' see *Details* and
#' \insertCite{worldbankMeasuringRealSize2013;textual}{OECDsppps},
#' for more information.
#'
#' The CPD method is a regression-based approach for estimating price parities.
#' It is characterised by a fixed-effects specification, in which country effects
#' yield estimates of subnational purchasing power parities,
#' while commodity-specific effects generate estimates of subnational price
#' levels. The model can be written as a regression equation in which all
#' explanatory variables take the form of dummy indicators for each region
#'  and commodity:
#'
#' \mjdeqn{ln p_{ij}  = \alpha_1 D_1 + ... + \alpha_j D_j + ... +\alpha_R D_R + \\ \eta_1 \mathcal{D}_1 + ... + \eta_i \mathcal{D}_i + ... + \eta_N \mathcal{D}_N + \varepsilon_{ij}}{ln p_{ij}  = \alpha_1 D_1 + ... + \alpha_j D_j + ... +\alpha_R D_R + \\ \eta_1 \mathcal{D}_1 + ... + \eta_i \mathcal{D}_i + ... + \eta_N \mathcal{D}_N + \varepsilon_{ij}}
#'
#' where \mjseqn{\alpha_j} is the the price level of region \mjseqn{j} relative
#' to all other regions in the comparison. \mjseqn{\alpha_j} can also be
#' expressed relative to a reference region, for example, the national price
#' level. Then, \mjseqn{\alpha_j}  represents the subnational purchasing power
#' parity of region \mjseqn{j} given
#' by \mjseqn{\hat{PPP}_j = exp(\hat{\alpha}_j)}.
#'
#' @param data Data frame, data table or tibble containing at least three
#'  columns identifying region, product and individual item-level price quotes
#' @param region Identifier for regions (within or across countries)
#' @param product Product identifier
#' @param price Individual item-level price quotes; duplicated region-product
#' pairs are aggregated by way of averaging across region-product pairs
#' @param weights An optional vector of weights to be used whenever duplicate
#' regional-product pairs are found in the data; default is `NULL`, in which
#' case data is aggregated to region-product pairs using unweighted means. If
#' weights are provided and duplicate regional-product pairs are found,
#' these weights are used as part of the aggregation of average regional-product
#' pairs; see \pkg{stats} `weighted.mean()`
#' @param weights_cpd An optional vector of weights to be used in the fitting
#' process of the CPD regression model; default is `NULL` and ordinary least
#' squares is used. If non-`NULL`, weighted
#' least squares is used, with weights \mjseqn{w} provided by `weights`, to
#' minimise \mjseqn{\sum(w \times e^2)}; see 'Details' of  \pkg{stats} `lm()`
#' @param base.region An optional character specifying the base to which the estimated
#' logarithmic regional price levels are expressed
#' When `NULL`, they refer to the (unweighted) regional average,
#' similar to `contr.sum()`
#' @param output Either "sPPP", which returns the estimated subnational
#' purchasing purchasing power parities, that is,
#' \mjseqn{\hat{SPPP}_r = exp(\hat{\alpha}_r)} or
#' "Full", which summarises the key information about the estimate CPD model
#' in a tidy `tibble` using  \pkg{broom}.
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
#' estim_cpd(df, output = "sPPP")
#' estim_cpd(df, output = "Full")
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom rlang :=
#' @importFrom broom tidy
#' @importFrom broom glance
#' @importFrom stringr str_remove_all
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @importFrom stats mean
#' @importFrom stats weighted.mean
#' @importFrom stats contrasts
#' @importFrom dplyr tally
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr row_number
#' @importFrom dplyr summarise
#' @importFrom stats dummy.coef
#' @export
estim_cpd <- function(data,
                      region = "region",
                      product = "product",
                      price = "price",
                      weights = NULL,
                      weights_cpd = NULL,
                      base.region = NULL,
                      output = "sPPP") {
  # Checks
  ## To be implemented
  ## change regions/products to factors
  ## check that prices are numeric
  ## no empty stuff
  if (output %not.in% c("sPPP", "Full")) stop("Field `output` incorrectly speciefied. Please choose eiterh 'sPPP', or 'Full'.")

  ## Check for duplicates and aggregate
  ### Check dimensions
  n_obs_raw <- data |> nrow()
  n_obs <- data |>
    group_by(.data[[region]], .data[[product]]) |>
    tally() |>
    nrow()
  ### Aggregate: no weights provided
  if (n_obs < n_obs_raw & is.null(weights)) {
  data <- data |>
    group_by(.data[[region]], .data[[product]]) |>
    summarise({{ price }} := mean(.data[[price]], na.rm = T),
      .groups = "drop"
    )
  message("Duplicated region-product pairs found in data and no weights provided: Data is aggregated to region-product pairs using unweighted means.")
  }
  ### Aggregate: weights provided
  if (n_obs < n_obs_raw & !is.null(weights)) {
    data <- data |>
      group_by(.data[[region]], .data[[product]]) |>
      summarise({{ price }} := stats::weighted.mean(.data[[price]], w = .data[[weights]], na.rm = T),
                       .groups = "drop"
      )
    message("Duplicated region-product pairs found in data and no weights provided: Data is aggregated to region-product pairs using weighted means, with weights provided in `weights`.")
  }

  # Dimensions
  n_region <- data |>
    distinct(.data[[region]]) |>
    nrow()
  n_product <- data |>
    distinct(.data[[region]]) |>
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
    stats::contrasts(x = data[[region]]) <- stats::contr.sum(levels(data[[region]]))
    colnames(stats::contrasts(x = data[[region]])) <- levels(data[[region]])[-nlevels(data[[region]])]
  } else {
    stats::contrasts(x = data[[region]]) <- stats::contr.treatment(levels(data[[region]]))
    colnames(stats::contrasts(x = data[[region]])) <- levels(data[[region]])[-1]
  }

  # Estimate
  if (is.null(weights_cpd)) {
    est_out <- stats::lm(formula = formula, data = data, singular.ok = FALSE)
  } else if (!is.null(weights_cpd)) {
    est_out <- stats::lm(formula = formula, data = data, weights = eval(rlang::sym(weights_cpd)), singular.ok = FALSE)
  }

  # Output
  if (output == "sPPP") {
    out <- tibble(
      region = stats::dummy.coef(est_out)[[region]] |> names(),
      "sPPP" = exp(stats::dummy.coef(est_out)[[{{ region }}]])
    )
  }
  if (output == "Full") {
    ## Observations by region
    reg_nobs <- data |>
      group_by(.data[[region]]) |>
      tally(n = "nobs")

    ## Reg model
    m <- summary(est_out)

    ## Regression output
    out <- rbind(
      broom::tidy(m) |>
        dplyr::filter(grepl({{ region }}, term)) |>
        dplyr::mutate(
          term = stringr::str_remove_all(term, {{ region }}),
          r.squared = NA, adj.r.squared = NA, sigma = NA, df = NA, df.residual = NA
        ) |>
        dplyr::left_join(reg_nobs, by = c("term" = {{ region }})),
      broom::glance(m) |>
        dplyr::mutate(
          term = "Aggregate summary statistics",
          estimate = NA, std.error = NA, statistic = NA, p.value = NA
        )
    )
  }
  return(out)
}
