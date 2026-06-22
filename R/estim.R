#' The Country Product Dummy (CPD) regression model
#'
#' \loadmathjax
#' `estim_cpd` in  \pkg{OECDsppps} estimates subnational PPPs building on the
#' regional extension of the Country-Product-Dummy (CPD) method \insertCite{summers1973international;textual}{OECDsppps},
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
#' @param price Individual item-level price quotes; Duplicate region-product
#' pairs are aggregated by way of averaging across region-product pairs
#' @param weights An optional vector of weights to be used whenever duplicate
#' regional-product pairs are found in the data. Options:
#' - Default is `NULL`, in which case data is aggregated to region-product
#' pairs using unweighted means.
#' - If weights are provided and duplicate regional-product pairs are found,
#' these weights are used as part of the aggregation of average regional-product
#' pairs; see \pkg{stats} `weighted.mean()`.
#' - If `weights = 'raw'`, raw data is used with no additional aggregation to
#' region-product pairs.
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
#' "Full", which summarises the key information of the estimate CPD model:
#'  It provides the 'Regression output`as well as the individual 'Residuals'
#'  of the CPD regression. Note that the column `sPPP` is derived from the
#'  factor term contrasts using `stats::dummy.coef()`. The values in the
#'  column `estimate` correspond to the column `sPPP` as `sPPP = exp(estimate)`
#'  for all factors except the 'missing' category, for which they are zero.
#'  Consequently, the regression output for this category is reported as NA,
#'  while the sPPP value is reported as described above.
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
#' @importFrom stats weighted.mean
#' @importFrom stats contrasts
#' @importFrom dplyr tibble
#' @importFrom dplyr rename
#' @importFrom dplyr full_join
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
  ## Change regions/products to factors
  if (!is.factor(data[[{{ region }}]])) {
    data <- data |> mutate({{ region }} := as.factor(.data[[region]]))
    message("Variable `region` encoded to factor.")
  }
  if (!is.factor(data[[{{ product }}]])) {
    data <- data |> mutate({{ product }} := as.factor(.data[[product]]))
    message("Variable `product` encoded to factor.")
  }
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
    message("Duplicate region-product pairs found in data and no weights provided: Data is aggregated to region-product pairs using unweighted means.")
  }
  ### Aggregate: weights provided
  if (n_obs < n_obs_raw & !is.null(weights)) {
    if (weights != "raw") {
      data <- data |>
        group_by(.data[[region]], .data[[product]]) |>
        summarise({{ price }} := stats::weighted.mean(.data[[price]], w = .data[[weights]], na.rm = T),
          .groups = "drop"
        )
      message("Duplicate region-product pairs found in data and no weights provided: Data is aggregated to region-product pairs using weighted means, with weights provided in `weights`.")
    }
  }
  ### Aggregate: use raw data provided
  if (!is.null(weights)) {
    if (weights == "raw") {
      data <- data
      message("Duplicate region-product pairs found in data and `weights == 'raw'`: Raw data is used with no additional aggregation to region-product pairs.")
    }
  }

  # Dimensions
  n_region <- data |>
    distinct(.data[[region]]) |>
    nrow()
  n_product <- data |>
    distinct(.data[[product]]) |>
    nrow()


  # Setting base region
  if (!is.null(base.region)) stop("Only estimation with respect to regional average currently implemented")

  # CPD regression formula
  ## Case 1: multiple regions, multiple products
  if (n_region > 1 & n_product > 1) {
    formula <- paste0("log(", price, ") ~ ", product, " + ", region, " - 1")
  }

  ## Case 2: one product, multiple regions
  if (n_product == 1) {
    formula <- paste0("log(", price, ") ~ ", region, " + 1")
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
    data$`.weights_cpd` <- data[[weights_cpd]]
    est_out <- stats::lm(formula = formula, data = data, weights = .weights_cpd, singular.ok = FALSE)
  }

  # Output
  ## Regression output
  reg_out <- tibble(
    region = stats::dummy.coef(est_out)[[region]] |> names(),
    "sPPP" = exp(stats::dummy.coef(est_out)[[{{ region }}]])
  )
  if (output == "sPPP") {
    return(reg_out)
  }
  if (output == "Full") {
    ## Observations by region
    reg_nobs <- data |>
      group_by(.data[[region]]) |>
      tally(name = "nobs")

    ## Reg model
    m <- summary(est_out)

    ## Regression output
    out <-
      rbind(
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
      ) |>
      rename("region" = term) |>
      rename("Number of products per region" = nobs)


    ## Residuals
    resids <- broom::augment(est_out) |>
      select({{ region }}, .fitted, .resid, .std.resid)

    ## Finalise output
    full_out <- list(
      "Regression output" = reg_out |>
        full_join(out, by = {{ region }}),
      "Residuals" = resids
    )

    return(full_out)
  }
}


#' CPD estimation to index calculation linking function
#'
#' `estim_index_link()` enables linking CPD estimation to index calculation within
#' one pipe. Can fill in missing basic heading PPPs with a value given by the user.
#'
#' @param data Data frame, data table or tibble containing at least three
#'  columns identifying region, product and respective sPPPs
#' @param data_weights Data frame, data table or tibble containing at least three
#' columns identifying region, product and expenditure weights
#' @param basic_heading column containing the basic heading identifier
#' @param region Identifier for regions
#' @param sPPP Identifier for the basix heading sPPPs
#' @param exp_wght Identifier for expenditure weights
#' @param complete_sppp value to be imputed for missing basic heading PPPs
#'
#' @return Returns a data frame containing the variables indicating the region ("region"),
#' basic heading ("product"), basic heading PPP ("ppp_bh"), and expenditure weights ("exp_wght").
#' This output can be directly fed into `index_laspeyres()`, `index_paasche()`, `index_fisher()`,
#' and `index_geks()`.
#'
#' @examples
#' \dontrun{
#' # Generate the price and weight data and estimate CPD at basic headings
#' dt1 <- pricelevels::rdata(
#'   R = R, B = B, N = N,
#'   weights = ~ r + n,
#'   settings = list(par.sd = c(
#'     lnP = 0.1, pi = exp(1),
#'     delta = 0.5, error = 0.8
#'   ))
#' )
#'
#' dt1_wghts <- dt1 %>%
#'   distinct(group, region, .keep_all = TRUE) %>%
#'   select(group, region, weight)
#'
#' dt1_prices <- dt1 %>%
#'   dplyr::select(group, region, product, price)
#'
#' dt1_basic_headings <- dt1_prices %>%
#'   group_by(group) %>%
#'   group_modify(~ {
#'     estim_cpd(.x,
#'       region = "region",
#'       product = "product",
#'       price = "price",
#'       output = "sPPP"
#'     )
#'   }) %>%
#'   ungroup()
#'
#' # Complete data
#' # -> returns complete standardized data frame
#'
#' dt1_basic_headings %>%
#'   estim_index_link(
#'     data = .,
#'     data_weights = dt1_wghts,
#'     basic_heading = "group",
#'     region = "region",
#'     sPPP = "sPPP",
#'     exp_wght = "weight",
#'     complete_sppp = NA
#'   )
#'
#'
#' # Missing regional values
#' # -> returns an incomplete standardized data frame
#'
#' dt1_basic_headings %>%
#'   filter(!(region %in% c("1", "2") & group == "1")) %>%
#'   estim_index_link(
#'     data = .,
#'     data_weights = dt1_wghts,
#'     basic_heading = "group",
#'     region = "region",
#'     sPPP = "sPPP",
#'     exp_wght = "weight",
#'     complete_sppp = NA
#'   )
#'
#'
#' # Missing regional values: Imputation
#' # -> returns a complete standardised data frame with a warnings,
#' #    listing the region/heading imputations
#' dt1_basic_headings %>%
#'   filter(!(region %in% c("1", "2") & group == "1")) %>%
#'   estim_index_link(
#'     data = .,
#'     data_weights = dt1_wghts,
#'     basic_heading = "group",
#'     region = "region",
#'     sPPP = "sPPP",
#'     exp_wght = "weight",
#'     complete_sppp = 1
#'   )
#' }
#'
#' @importFrom tidyr unnest
#' @importFrom tidyr replace_na
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#'
#' @export
estim_index_link <- function(data,
                             data_weights = data_weights,
                             basic_heading = "basic_heading",
                             region = "region",
                             sPPP = "sPPP",
                             exp_wght = "weight",
                             complete_sppp = NA) {
  harmonised_data <- data %>%
    full_join(data_weights,
      by = c({{ basic_heading }}, {{ region }})
    ) %>%
    filter(!is.na({{ exp_wght }})) %>%
    rename(product = {{ basic_heading }}, ppp_bh = sPPP, exp_wght = {{ exp_wght }}, region = {{ region }})

  if (!is.na(complete_sppp)) {
    harmonised_data <- harmonised_data %>%
      select(product, region, ppp_bh) %>%
      pivot_wider(names_from = region, values_from = ppp_bh) %>%
      pivot_longer(!product, names_to = "region", values_to = "ppp_bh") %>%
      {
        completed_regions_headings_v <<- filter(., is.na(ppp_bh)) %>%
          mutate(region_heading = paste(region, product, sep = "/")) %>%
          pull(region_heading)
        .
      } %>%
      replace_na(list(ppp_bh = complete_sppp)) %>%
      full_join(data_weights,
        by = c(
          product = {{ basic_heading }},
          region = {{ region }}
        )
      ) %>%
      rename(exp_wght = {{ exp_wght }})

    if (length(completed_regions_headings_v) > 0) {
      warning(print(paste(
        "sPPP of",
        complete_sppp,
        "was imputed to the following region/headings pairs:",
        paste(completed_regions_headings_v, collapse = "; ")
      )))
    }
  }

  return(harmonised_data)
}
