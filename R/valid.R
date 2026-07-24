#' The "Price Observation Table"
#'
#' \loadmathjax
#' `valid_pot` creates the "Price Observation Table";
#' by calculating two *individual price outlier statistics* for the
#' individual item-level price quotes,  the *ratio-to-average price test* and the *t-value test*;
#' \insertCite{@see @worldbankMeasuringRealSize2013, @icpGuideCompilationSubnational2021 and @europeanunionEurostatOECDMethodologicalManual2024;textual}{OECDsppps}.
#'  All item-level price
#' quotes that do not pass the two tests are flagged in columns
#' `Ratio-to-average price test FLAG` and `T-value test FLAG`, respectively.
#' The item-level price quotes should be based on the
#' **reference quantity price**;
#' see *Details* and \insertCite{@worldbankMeasuringRealSize2013;textual}{OECDsppps},
#' Table 9.1a. for more information.
#'
#' **Reference quantity price:** Scales the observed price to the quantity
#' that *should* be surveyed. It is defined as:
#' \mjdeqn{Reference~quantity~price = \frac{Observed~price}{Observed~quantity} \times Reference~quantity}{Reference~quantity~price = \frac{Observed~price}{Observed~quantity} \times Reference~quantity}
#' whenever the measurement unit of observed quantity is identical to the measurement unit of the reference quantity.
#'
#' **Ratio-to-average price test:** The ratio of an individual price observation
#' \mjseqn{i}, \mjseqn{P_{i}}, of a specific product \mjseqn{j} and the observed average
#' price for the product, \mjseqn{\mu_j}. An observed price passes the this test
#' if the ratio is between 0.5 and 1.5. This simple check flags potential outlier values
#' without relying on standard deviation, which can itself be distorted by outliers:
#' \mjdeqn{ratio-to-average = p_{ij}/\mu_j}{ratio-to-average = p_{ij}/\mu_j}
#'
#'
#' **T-value test**: The ratio of the deviation of an individual price observation
#' from the average reference quantity price for the product and the standard
#' deviation of the product. To pass the test, the ratio must be between -2.0 and 2.0
#' (any value outside that range is suspect because it falls outside the 95 percent confidence interval):
#' \mjdeqn{t-val = (p_{ij} - \mu_{P_j}) / \sigma_{P_j}}{t-val = (p_{ij} - \mu_{P_j}) / \sigma_{P_j}}
#'
#' @param data Data frame or tibble containing at least one column with
#' individual item-level price quotes.
#' @param price_quote Column containing the individual item-level price quotes,
#' which should be based on the
#' "reference quantity price"; see *Details* for more information.
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' library(OECDsppps)
#' uk_cpi |>
#'   select(Year, `Product code`, `Reference quantity price`) |>
#'   group_by(Year, `Product code`) |>
#'   valid_pot(price_quote = "Reference quantity price") |>
#'   head()
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom dplyr mutate
#' @export
valid_pot <- function(data,
                      price_quote = "Reference quantity price") {
  data |>
    # Calculate individual price outlier statistics
    mutate(
      `Ratio-to-average price test` = .data[[price_quote]] / mean(.data[[price_quote]]),
      `T-value test` = (.data[[price_quote]] - mean(.data[[price_quote]])) / sd(.data[[price_quote]])
    ) |>
    # Add flags for selection rules
    mutate(
      `Ratio-to-average price test FLAG` = ifelse(`Ratio-to-average price test` < 0.5 | `Ratio-to-average price test` > 1.5, TRUE, FALSE),
      `T-value test FLAG` = ifelse(`T-value test` > 2 | `T-value test` < -2, TRUE, FALSE)
    )
}

#' The "Average Price Table"
#'
#' \loadmathjax
#' `valid_apt()` creates the "Average Price Table" containing:
#' - `Number of observations` - Number of observations by group as specified by `group_by()`
#' - `Average`- Average price based on item-level price quotes by group as specified by `group_by()`
#' - `Maximum`- Highest price based on item-level price quotes by group as specified by `group_by()`
#' - `Minimum` - Lowest price based on item-level price quotes by group as specified by `group_by()`
#' - `Standard deviation` - Standard deviation based on item-level price quotes by group as specified by `group_by()`
#' - `max-min ratio test` and `Coefficient of variation test` - see *Details* for more information
#'  All item-level price
#' quotes that do not pass the two tests are flagged in columns
#' `Max-min ratio FLAG` and`Coefficient of variation FLAG`, respectively;
#' \insertCite{@see @worldbankMeasuringRealSize2013, @icpGuideCompilationSubnational2021 and @europeanunionEurostatOECDMethodologicalManual2024;textual}{OECDsppps}.
#'
#' **Reference quantity price:** Scales the observed price to the quantity
#' that *should* be surveyed. It is defined as:
#' \mjdeqn{Reference~quantity~price = \frac{Observed~price}{Observed~quantity} \times Reference~quantity}{Reference~quantity~price = \frac{Observed~price}{Observed~quantity} \times Reference~quantity}
#' whenever the measurement unit of observed quantity is identical to the measurement unit of the reference quantity.
#'
#' **Max-min ratio test:** The ratio between the maximal and minimal observed price
#' for product \mjseqn{j}, \mjseqn{p_j}. Products where the maximal observed price is more than twice
#' as big as the minimum are flagged in `Max-min ratio FLAG`:
#' \mjdeqn{max-min~ratio = max(p_j)/min(p_j)}{max-min~ratio = max(p_j)/min(p_j)}
#'
#' **Coefficient-of-variation test:** The standard deviation \mjseqn{\sigma_{p_j}}
#' of product  \mjseqn{j}'s price \mjseqn{p_j}
#' expressed as a percentage of the average price for the product,  \mjseqn{\mu_{p_j}}. Products
#' with a coefficient of variation greater than 20% will be flagged in `Coefficient of variation FLAG`:
#' \mjdeqn{coefficient-to-variation: \sigma_{p_j} / \mu_{p_j}}{coefficient-to-variation: \sigma_{p_j} / \mu_{p_j}}
#'
#' **Using the "Average Price Table" for additional validation:**
#' In addition to the raw data validation, `valid_apt()` can be used to check for
#' outliers in the *household expenditure share* as well as price estimates
#' from the *CPD regression* model, in which cases the input argument `value`
#' takes either the reported item-level household expenditure shares, or `sPPP`
#' estimates, respectively.
#'
#' @param data A data frame or tibble containing at least one column with
#' individual item-level price quotes.
#' @param price_quote Column containing the individual item-level price quotes,
#' which should be based on the
#' "reference quantity price"; see *Details* for more information.
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' library(OECDsppps)
#' uk_cpi |>
#'   select(Year, Region, `Product code`, `Reference quantity price`) |>
#'   group_by(Year, Region, `Product code`) |>
#'   valid_apt(price_quote = "Reference quantity price") |>
#'   head(n = 2) |>
#'   t()
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom rlang .data
#' @export
valid_apt <- function(data,
                      price_quote = "Reference quantity price") {
  data |>
    ## Number of observations
    mutate(nobs = 1) |>
    ## Summary stats
    summarise(
      `Number of observations` = sum(nobs),
      `Average price of product` = mean(.data[[price_quote]]),
      `Maximum price of product` = max(.data[[price_quote]]),
      `Minimum price of product` = min(.data[[price_quote]]),
      `Standard deviation` = sd(.data[[price_quote]])
    ) |>
    ## Calculate tests
    mutate(
      `Max-min ratio` = `Maximum price of product` / `Minimum price of product`,
      `Coefficient of variation` = `Standard deviation` / `Average price of product`
    ) |>
    # Add flags for selection rules
    mutate(
      `Max-min ratio FLAG` = `Max-min ratio` > 2,
      `Coefficient of variation FLAG` = `Coefficient of variation` > .2
    )
}


#' The "XR-ratio Tables"
#'
#' \loadmathjax
#' `valid_ratio_xr()` in  \pkg{OECDsppps} calculates the exchange rate ratio (XR-ratio), which is a region-country's XR-price
#' Variability can be compared to highlight products in countries/regions
#' that are most variable (high XR-ratio) across countries;
#' \insertCite{@see @worldbankMeasuringRealSize2013, @icpGuideCompilationSubnational2021 and @europeanunionEurostatOECDMethodologicalManual2024;textual}{OECDsppps}.
#'
#' The **XR-ratio** uses the exchange-rate-converted prices to calculate the *standardised price ratio (SPR)*
#' For product \mjseqn{1} and country-region \mjseqn{A}, the SPR is defined as:
#' \mjdeqn{SPR_{1A} = \mu^*_{1A} / \left( \prod_{n = A,\dots, N} \mu^*_{1n}  \right)^{\frac{1}{N}} \times 100}{SPR_{1A} = \mu^*_{1A} / \left( \prod_{n = A,\dots, N} \mu^*_{1n}  \right)^{\frac{1}{N}} \times 100}
#' where
#' \mjseqn{\mu^{*}_{1A}} represents the *average converted price* of product \mjseqn{1} in country-region
#' \mjseqn{A}, and \mjseqn{N} is the total number of country-regions.
#'
#' @param data A data frame or tibble containing at least a column with the average country-region
#'  prices and exchange rate.
#' @param average_price Average country-region
#'  prices of the individual item-level price quotes. Correspond to the
#'  "Average price of product" obtained in `valid_apt()`.
#' @param exchange_rate National-level exchange rate for common currency; typically USD.
#'
#' @references
#'   \insertAllCited{}
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @export
valid_ratio_xr <- function(data,
                           average_price = "Average price of product",
                           exchange_rate = "XR USD") {
  data |>
    mutate(
      # Calculate exchange rate average price
      `XR Average price of product` = .data[[average_price]] * .data[[exchange_rate]],
      # Calculate XR-Ratio
      `Geometric mean` = exp(mean(log(`XR Average price of product`))),
      # Calculate XR-ratios
      `XR-ratio` = `XR Average price of product` / `Geometric mean` * 100
    ) |>
    # Select variables
    select(-c(`XR Average price of product`, `Geometric mean`))
}


#' The "PPP-ratio Tables"
#'
#' \loadmathjax
#' `valid_ratio_ppp()` in  \pkg{OECDsppps} calculates the PPP-ratio, which shows the variation
#' coefficient representing variability across products and across country-regions;
#' \insertCite{@see @worldbankMeasuringRealSize2013, @icpGuideCompilationSubnational2021 and @europeanunionEurostatOECDMethodologicalManual2024;textual}{OECDsppps}.
#'
#' The country variation coefficient (row measure) represents the standard
#' deviation of product PPPs within  country-regions, thereby identifying
#' countries exhibiting the greatest price variability. Conversely,
#' the product variation coefficient (column measure) represents the
#' standard deviation of PPP-ratios across country-regions, highlighting
#' products with the most significant cross-country variation.
#'
#' The **PPP-ratio** uses the PPP-converted prices to calculate the *standardised price ratio (SPR)*
#' For product \mjseqn{1} and country-region \mjseqn{A}, the SPR is defined as:
#' \mjdeqn{SPR_{1A} = \mu^*_{1A} / \left( \prod_{n = A,\dots, N} \mu^*_{1n}  \right)^{\frac{1}{N}} \times 100}{SPR_{1A} = \mu^*_{1A} / \left( \prod_{n = A,\dots, N} \mu^*_{1n}  \right)^{\frac{1}{N}} \times 100}
#' where
#' \mjseqn{\mu^{*}_{1A}} represents the *average converted price* of product \mjseqn{1} in country-region
#' \mjseqn{A}, and \mjseqn{N} is the total number of country-regions.
#'
#' @param data A data frame or tibble containing at least a column with the average country-region
#'  prices and a region and product identifier.
#' @param year Year
#' @param product_code Product code identifier
#' @param region Identifier for regions (within or across countries)
#' @param average_price  Average country-region
#'  prices of the individual item-level price quotes. Correspond to the
#'  "Average price of product" obtained in `valid_apt()`
#'
#' @references
#'   \insertAllCited{}
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom tidyselect everything
#' @importFrom tidyselect contains
#' @importFrom dplyr join_by
#' @export
valid_ratio_ppp <- function(data,
                            year = "Year",
                            product_code = "Product code",
                            region = "Region",
                            average_price = "Average price of product") {
  # Calculations
  tmp <- data |>
    # Calculate PPP price, first country is baseline
    group_by(.data[[product_code]]) |>
    mutate(
      PPP_item = ifelse(row_number() == 1, .data[[average_price]], NA),
      PPP_item = mean(PPP_item, na.rm = TRUE),
      PPP_item = .data[[average_price]] / PPP_item
    ) |>
    # Calculate aggregate PPP
    group_by(.data[[region]]) |>
    mutate(PPP_country = exp(mean(log(PPP_item)))) |>
    # Calculate PPP price relatives
    mutate(PPP_pricerel = .data[[average_price]] / PPP_country) |>
    # Calculate geometric mean
    group_by(.data[[product_code]]) |>
    mutate(gmean = exp(mean(log(PPP_pricerel)))) |>
    ungroup() |>
    # Calculate PPP ratios
    mutate(PPP_ratio = PPP_pricerel / gmean * 100) |>
    ungroup() |>
    # Calculate variation coefficients
    select({{ year }}, {{ region }}, {{ product_code }}, PPP_ratio) |>
    mutate(`VC Product` = sd(PPP_ratio), .by = {{ product_code }}) |>
    mutate(`VC Region` = sd(PPP_ratio), .by = {{ region }})

  # Variation coefficients
  x1 <- tmp |>
    select({{ year }}, {{ product_code }}, `VC Product`) |>
    distinct()

  x2 <- tmp |>
    select({{ year }}, {{ region }}, `VC Region`) |>
    distinct() |>
    pivot_wider(names_from = {{ region }}, values_from = `VC Region`) |>
    mutate({{ product_code }} := "Region variation coefficients") |>
    select({{ year }}, {{ product_code }}, tidyselect::everything()) |>
    mutate(`VC Product` = NA)

  # Final table
  tmp |>
    select(-contains("VC")) |>
    pivot_wider(names_from = {{ region }}, values_from = PPP_ratio) |>
    left_join(x1, by = join_by({{ year }}, {{ product_code }})) |>
    rbind(x2)
}

#' The "Expenditure Shares Table"
#'
#' \loadmathjax
#' `valid_est()` in  \pkg{OECDsppps} creates the "Expenditure Shares Table"
#' \insertCite{@see @worldbankMeasuringRealSize2013, @icpGuideCompilationSubnational2021 and @europeanunionEurostatOECDMethodologicalManual2024;textual}{OECDsppps}.
#' The function calculates the:
#' - `Nobs` - Number of data points by by group as specified by `group_by()`
#' - `Maximum`- Highest expenditure share based on expenditure shares by group as specified by `group_by()`
#' - `Median` - Median expenditure share based on expenditure shares by group as specified by `group_by()`
#' - `Minimum` - Lowest expenditure share based on expenditure shares by group as specified by `group_by()`
#' - `max-median ratio test` and `median-min ratio test` - see *Details* for more information
#'  All expenditure shares that do not pass the two tests are flagged in columns
#' `Max-median ratio FLAG` and`Median-min ratio FLAG`, respectively#'
#'
#' **Max-median ratio test:** The ratio between the maximal and median observed expenditure shares
#' for product \mjseqn{j}, \mjseqn{w_j}. Basic headings where the maximal observed expenditure is more than 25 times
#' as big as the median are flagged in `Max-median ratio FLAG`:
#' \mjdeqn{max-median~ratio = max(w_j)/median(w_j)}{max-median~ratio = max(w_j)/median(w_j)}
#'
#' **Median-min ratio test:** The ratio between the median and minimal observed expenditure shares
#' for product \mjseqn{j}, \mjseqn{w_j}. Basic headings where the median observed expenditure is more than 25 times
#' as big as the minimum are flagged in `Median-min ratio FLAG`:
#' \mjdeqn{median-min~ratio = median(w_j)/min(w_j)}{median-min~ratio = median(w_j)/min(w_j)}
#'
#'
#' @param data A data frame or tibble containing at least one column with
#' individual item-level expenditure shares.
#' @param shares Column containing the individual item-level expenditure shares.
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' library(OECDsppps)
#' uk_hhe |>
#'   group_by(coicop_4d) |>
#'   valid_est(shares = "expenditure_share")
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom rlang .data
#' @export
valid_est <- function(data,
                      shares = "Expenditure shares for basic headings") {
  data |>
    ## Summary stats
    summarise(
      Nobs = n(),
      `Maximum expenditure share` = max(.data[[shares]]),
      `Median expenditure share` = median(.data[[shares]]),
      `Minimum expenditure share` = min(.data[[shares]])
    ) |>
    ## Calculate tests
    mutate(
      `Max-median ratio` = `Maximum expenditure share` / `Median expenditure share`,
      `Median-min ratio` = ifelse(`Minimum expenditure share` == 0,
        NA,
        `Median expenditure share` / `Minimum expenditure share`
      )
    ) |>
    # Add flags for selection rules
    mutate(
      `Max-median ratio FLAG` = `Max-median ratio` > 25,
      `Median-min ratio FLAG` = `Median-min ratio` > 25
    )
}

#' The Paasche-Laspeyres spread
#'
#' \loadmathjax
#' `valid_pls()` in  \pkg{OECDsppps} calculates the Paasche-Laspeyres spread (PLS),
#' \insertCite{@see @worldbankMeasuringRealSize2013, @icpGuideCompilationSubnational2021 and @hillLinkingRegionsInternational2011;textual}{OECDsppps},
#' which corresponds to the upper and lower price and quantity relatives to determine whether the
#' large values in the PLS are caused by PPPs or expenditure outliers. Basic headings
#' with large upper or lower quantity or price relatives should be further examined.
#'
#'  The Paasche-Laspeyres spread for regions \mjseqn{j} and \mjseqn{k} is defined as:
#'  \mjdeqn{PLS_{j,k} = \frac{MAX(sPPPP_{P}^{jk}, sPPPP_{L}^{jk})}{MIN(sPPPP_{P}^{jk}, sPPPP_{L}^{jk})}}{PLS_{j,k} = \frac{MAX(sPPPP_{P}^{jk}, sPPPP_{L}^{jk})}{MIN(sPPPP_{P}^{jk}, sPPPP_{L}^{jk})}}
#'
#' where \mjseqn{sPPPP_{P}^{jk}} and \mjseqn{sPPPP_{L}^{jk}} correspond to the
#' Paasche and Laspeyres indicies, respectively;
#' see `index_paasche()` and `index_laspeyres()` for more information.
#'
#' @param data A data frame or tibble containing at least four columns identifying
#' region, product, subnational PPPs, and expenditure weights.
#' @param region Identifier for regions
#' @param product Product identifier
#' @param ppp_bh Identifier for subnational PPPs
#' @param exp_wght Identifier for expenditure weights
#' @references
#'   \insertAllCited{}
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @export
valid_pls <- function(data,
                      region = "region",
                      product = "product",
                      ppp_bh = "ppp_bh",
                      exp_wght = "exp_wght") {
  # Laspeyres Index
  lasp_index <- index_laspeyres(
    data,
    region,
    product,
    ppp_bh,
    exp_wght
  )

  # Paasche Index: matrix
  paas_index <- index_paasche(
    data,
    region,
    product,
    ppp_bh,
    exp_wght
  )

  output_pl_spread <- lasp_index %>%
    left_join(paas_index,
      by = c("base_region", "region")
    ) %>%
    group_by(base_region, region) %>%
    mutate(paasche_laspeyres_spread = (max(laspeyres_index, paasche_index) / min(laspeyres_index, paasche_index))) %>%
    ungroup()

  return(output_pl_spread)
}

#' sPPPs outlier plot
#'
#' `valid_outlier_plot()` produces some simple validation plots to check
#' subnational PPP estimates for potential outliers
#'
#'
#' @param data A data frame or tibble containing at least one column with
#' the subnational Purchasing Power Parity indices
#' @param sPPPs Vector with subnational Purchasing Power Parities
#' @param title Option to add a plot title; default is NULL
#' @param facet_var Option to wraps a 1d sequence of panels into 2d based on
#' the provided variable following ggplot2's `facet_wrap()`; default is NULL
#' @param facet_ncol Option to change the number of column of the created
#' facets following ggplot2's `facet_wrap()` argument `ncol`;
#' default is 2 if `facet_var` is used
#' @param facet_scale Option to change wheterhe the facet scales should be fixed
#' (`"fixed"`, the default), free (`"free"`), or free in one
#' dimension (`"free_x"`, `"free_y"`); default is `"fixed"`
#' @param bins Number of bins following ggplots' `geom_histogram()` argument `bins`;  Default is `70`
#' @param xlim_range Limits for the x and y axes, following
#' ggplot2's `coord_cartesian()` argument and need to be provided as a vector
#' as `outlier_cutoffs = c(upper_limit, lower_limit)`; default is NULL
#' @param outlier_cutoffs Cutoffs to highlight potential outliers in
#' the plot and need to be provided as a vector
#' as `outlier_cutoffs = c(upper_limit, lower_limit)`;
#' default is `1.5` and `0.5`, i.e. `outlier_cutoffs = c(1.5, 0.5)`
#'
#' @examples
#' \dontrun{
#' uk_cpi |>
#'   select(Year,
#'     region = "Region",
#'     product = "Product code",
#'     price = "Reference quantity price"
#'   ) |>
#'   mutate(
#'     region = as.factor(region),
#'     product = as.factor(product)
#'   ) |>
#'   estim_cpd() |>
#'   valid_outlier_plot(
#'     title = "sPPPs outlier with adjusted outlier cutoffs",
#'     # Adjust outlier cutoffs (default is 1.5 and 0.5)
#'     outlier_cutoffs = c(1.1, 0.9)
#'   )
#' }
#'
#' @importFrom ggplot2 ggplot
#' @export
valid_outlier_plot <- function(data,
                               sPPPs = "sPPP",
                               title = NULL,
                               facet_var = NULL,
                               facet_ncol = NULL,
                               facet_scale = "fixed",
                               bins = 70,
                               outlier_cutoffs = c(1.5, 0.5),
                               xlim_range = NULL) {
  p <- data %>%
    # Identify outlier
    mutate(outlier = .data[[sPPPs]] > outlier_cutoffs[1] | .data[[sPPPs]] < outlier_cutoffs[2]) |>
    # Base plot
    ggplot(aes(x = .data[[sPPPs]], fill = outlier)) +
    geom_histogram(bins = bins, position = "identity", alpha = 0.6) +
    geom_vline(xintercept = 1, color = "grey20", linetype = "dashed") +
    scale_fill_manual(
      values = c("#a3bbdd", "#2a4691"),
      labels = c("Within 0.5-1.5", "Outside 0.5-1.5"),
      name = ""
    ) +
    labs(
      x = "sPPPs distribution", y = "",
      title = title,
      subtitle = "Counts"
    ) +
    theme_minimal() +
    theme(legend.position = "top")

  # Add facet option
  if (!is.null(facet_var)) {
    ## Check ncol
    if (is.null(facet_ncol)) {
      facet_ncol <- 2
      message("Variable `facet_ncol` not provided and changed defaul `facet_ncol = 2`.")
    }
    ## Add facet
    p <- p + facet_wrap(~ .data[[facet_var]],
      ncol = facet_ncol,
      scale = facet_scale
    ) +
      labs(caption = paste0("Facetting variable: `", {{ facet_var }}, "`"))
  }

  # Add x-axis limits option
  if (!is.null(xlim_range)) {
    p <- p + coord_cartesian(xlim = xlim_range)
  }

  return(p)
}

#' Dikhanov Table
#'
#' `valid_dikhanov()` generates the Dikhanov tables for all selected basic headings;
#' \insertCite{@see @worldbankMeasuringRealSize2013 and @icpGuideCompilationSubnational2021;textual}{OECDsppps}.
#'
#' The Dikhanov tables consist of:
#'
#' - Summary information (PPPs, SDs, price level) by region for the aggregate;
#' - CPD residuals and product variation coefficients for products within basic headings.
#'
#' The Dikhanov Table facilitates the comparisons of PPPs across basic headings;
#' plausible variations in PPPs is expected across regions. Such variations would
#' indicate that, say, alcoholic beverages in region A are x% higher than in region B.
#' The CPD residuals help ensure that the aggregate PPP variations are not driven
#' by certain basic headings, or isolated products therein, but are more reflective
#' of common price-level differences across regions.
#'
#' The function first obtains CPD estimates through `estim_cpd()`. It then
#' calculates all required summary statistics and returns a list containing
#' Dikhanov tables for each of the selected basic headings.
#'
#' @param data Data frame, data table or tibble containing at least three
#'  columns identifying region, product and individual item-level price quotes
#' @param region Identifier for regions (within or across countries)
#' @param product Product identifier
#' @param price Individual item-level price quotes; duplicate region-product
#' pairs are aggregated by way of averaging across region-product pairs following the
#' default options in `estim_cpd()`
#' @param product_heading Variable identifying the corresponding product groups of the
#' individual price quotes; typically corresponds to the basic headings, for example
#' the  4-digit COICOP groups.
#' @param product_heading_comparison Specify the product groups identified via argument
#' `product_heading` for which the Dikhanov tables should be computed;
#' default is 'all', that is, for all product groups listed in `product_heading`
#' the Dikhanov tables will be computed
#'
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr group_map
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rowwise
#' @importFrom dplyr ungroup
#' @importFrom dplyr case_when
#' @importFrom dplyr pick
#' @importFrom dplyr arrange
#' @importFrom dplyr relocate
#' @importFrom dplyr c_across
#' @importFrom dplyr n_distinct
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map
#' @importFrom tidyselect everything
#'
#'
#' @examples
#' set.seed(123)
#' R <- 5 # number of regions
#' B <- 3 # number of product groups
#' N <- 5 # number of products
#' dt1 <- pricelevels::rdata(R = R, B = B, N = N)
#' # Dikhanov tables for products with product classification provided by
#' # variable 'group' for products with generic name "1" and "3"
#' valid_dikhanov(
#'   data = dt1,
#'   region = "region",
#'   product = "product",
#'   price = "price",
#'   product_heading = "group",
#'   product_heading_comparison = c("1", "3")
#' )
#'
#' # Dikhanov tables for all three products contained in the data
#' valid_dikhanov(
#'   data = dt1,
#'   region = "region",
#'   product = "product",
#'   price = "price",
#'   product_heading = "group"
#' )
#'
#' @export
valid_dikhanov <- function(data,
                           region = "region",
                           product = "product",
                           price = "price",
                           product_heading = "product_heading",
                           product_heading_comparison = "all") {
  # Pull full list of product headings for comparison if set to 'all'
  if (sum(product_heading_comparison == "all")) {
    product_heading_comparison <- data |>
      distinct(.data[[product_heading]]) |>
      pull(.data[[product_heading]])
  }

  dikhanov_table_list <- data %>%
    filter(.data[[product_heading]] %in% product_heading_comparison) %>%
    select({{ product_heading }}, {{ region }}, {{ product }}, {{ price }}) %>%
    group_by(.data[[product_heading]]) %>%
    group_map(~ {
      output_cpd <- estim_cpd(.x,
        region = {{ region }},
        product = {{ product }},
        price = {{ price }},
        output = "Full"
      )
      output_cpd[[2]] <- output_cpd[[2]] %>%
        mutate(product = .x[[product]])
      output_cpd
    }) %>%
    purrr::map(\(x) list(
      `Regression output` = x$`Regression output` %>%
        select(region, sPPP) %>%
        filter(region != "Aggregate summary statistics") %>%
        pivot_wider(names_from = "region", values_from = "sPPP") %>%
        mutate(variable = "sPPP"),
      Residuals = x$Residuals %>%
        select(region, product, .resid) %>%
        pivot_wider(names_from = "region", values_from = ".resid") %>%
        bind_rows(
          summarise(., across(-product, ~ sd(.x, na.rm = TRUE)))
        ) %>%
        mutate(variable = case_when(
          !is.na(product) ~ NA,
          is.na(product) ~ "STD 2"
        )) %>%
        bind_rows(
          summarise(., across(-c(product, variable), ~ sum(!is.na(.x)) - 1))
        ) %>%
        mutate(variable = case_when(!is.na(product) ~ NA,
          is.na(variable) & is.na(product) ~ "No. of items priced",
          .default = variable
        )) %>%
        rowwise() %>%
        mutate(
          `STD 1` = sd(c_across(-c(product, variable)), na.rm = TRUE),
          `Items per region` = sum(!is.na(c_across(-c(product, variable)))) - 1
        ) %>%
        ungroup() %>%
        mutate(
          `Items/Countries` = case_when(variable %in% c("No. of items priced") ~ n_distinct(product, na.rm = TRUE),
            variable %in% c("sPPP", "STD 2") ~ NA,
            .default = `Items per region`
          ),
          `STD 1` = case_when(variable %in% c("sPPP", "No. of items priced") ~ NA,
            variable %in% c("STD 2") ~ pick(everything()) %>%
              filter(!is.na(product)) %>%
              select(-variable, -product, -`STD 1`, -`Items per region`) %>%
              unlist() %>%
              sd(na.rm = TRUE),
            .default = `STD 1`
          )
        )
    )) %>%
    purrr::map(bind_rows) %>%
    purrr::map(.x = ., ~ .x %>%
      relocate(variable, product) %>%
      arrange(desc(variable)))

  names(dikhanov_table_list) <- product_heading_comparison

  print(dikhanov_table_list)
}
