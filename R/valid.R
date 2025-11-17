#' Create the "Price Observation Table"
#'
#' \loadmathjax
#' `valid_pot` in  \pkg{OECDsppps} creates the "Price Observation Table";
#' by calculating two *individual price outlier statistics* for the
#' individual item-level price quotes,  the *ratio-to-average price test* and the *t-value test*;
#' \insertCite{@see @worldbankMeasuringRealSize2013, @icpGuideCompilationSubnational2021 and @europeanunionEurostatOECDMethodologicalManual2024;textual}{OECDsppps}.
#'  All item-level price
#' quotes that do not pass the two tests are flagged in columns
#' `Ratio-to-average price test FLAG` and `T-value test FLAG`, respectively.
#' The item-level price quotes should be based on the
#' *reference quantity price*;
#' see *Details* and \insertCite{worldbankMeasuringRealSize2013;textual}{OECDsppps},
#' table 9.1a. for more information.
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
#'   select(Year, `Product code`, `Reference quantity price`) |>
#'   group_by(Year, `Product code`) |>
#'   valid_pot(price_quote = "Reference quantity price") |>
#'   head()
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @export
valid_pot <- function(data,
                      price_quote = "Reference quantity price",
                      ...) {
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

#' Create "Average Price Table"
#'
#' \loadmathjax
#' `valid_apt()` in  \pkg{OECDsppps} creates the "Average Price Table" by
#' calculating: the
#' - `number of observations` - Number of observations by group as specified by `group_by()`
#' - `average price of product`- Average price based on item-level price quotes by group as specified by `group_by()`
#' - `maximum price of product`- Highest price based on item-level price quotes by group as specified by `group_by()`
#' - `minimum price of product` - Lowest price based on item-level price quotes by group as specified by `group_by()`
#' - `standard deviation` - Standard deviation based on item-level price quotes by group as specified by `group_by()`
#' - `max-min ratio test` and `coefficient of variation test` - see *Details* for more information
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
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @export
valid_apt <- function(data,
                      price_quote = "Reference quantity price",
                      ...) {
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


#' The "XR-ratio tables"
#'
#' \loadmathjax
#' `valid_XRratio()` in  \pkg{OECDsppps} calculates the exchange rate ratio (XR-ratio), which is a region-country's XR-price
#' Variability can be compared to highlight products in countries/regions
#' that are most variable (high XR-ratio) across countries;
#' \insertCite{@see @worldbankMeasuringRealSize2013, @icpGuideCompilationSubnational2021 and @europeanunionEurostatOECDMethodologicalManual2024;textual}{OECDsppps}.
#'
#' The **XR-ratio** uses the exchange-rate-converted prices to calculate the *standardised price ratio (SPR)*
#' For product \mjseqn{1} and country–region \mjseqn{A}, the SPR is defined as:
#' \mjdeqn{SPR_{1A} = \mu^*_{1A} / \left( \prod_{n = A,\dots, N} \mu^*_{1n}  \right)^{\frac{1}{N}} \times 100}{SPR_{1A} = \mu^*_{1A} / \left( \prod_{n = A,\dots, N} \mu^*_{1n}  \right)^{\frac{1}{N}} \times 100}
#' where
#' \mjseqn{\mu^{*}_{1A}} represents the *average converted price* of product \mjseqn{1} in country–region
#' \mjseqn{A}, and \mjseqn{N} is the total number of country–regions.
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
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @export
valid_XRratio <- function(data,
                          average_price = "Average price of product",
                          exchange_rate = "XR USD",
                          ...) {
  data |>
    mutate(`XR Average price of product` = .data[[average_price]] * .data[[exchange_rate]]) |>
    # Calculate XR-Ratio
    mutate(`Geometric mean` = exp(mean(log(`XR Average price of product`)))) |>
    # Calculate XR-ratios
    mutate(`XR-ratio` = `XR Average price of product` / `Geometric mean` * 100) |>
    select(-c(`XR Average price of product`, `Geometric mean`))
}


#' The "PPP-ratio tables"
#'
#' \loadmathjax
#' `valid_PPPratio()` in  \pkg{OECDsppps} calculates the PPP-ratio, which shows the variation
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
#' For product \mjseqn{1} and country–region \mjseqn{A}, the SPR is defined as:
#' \mjdeqn{SPR_{1A} = \mu^*_{1A} / \left( \prod_{n = A,\dots, N} \mu^*_{1n}  \right)^{\frac{1}{N}} \times 100}{SPR_{1A} = \mu^*_{1A} / \left( \prod_{n = A,\dots, N} \mu^*_{1n}  \right)^{\frac{1}{N}} \times 100}
#' where
#' \mjseqn{\mu^{*}_{1A}} represents the *average converted price* of product \mjseqn{1} in country–region
#' \mjseqn{A}, and \mjseqn{N} is the total number of country–regions.
#'
#' @param data A data frame or tibble containing at least a column with the average country-region
#'  prices and a region and product identifier.
#' \describe{
#'   \item{Year}{Year}
#'   \item{`Product code`}{Product code identifier}
#'   \item{Region}{Identifier for regions (within or across countries)}
#'   \item{`Average price of product`}{Average country-region
#'  prices of the individual item-level price quotes. Correspond to the
#'  "Average price of product" obtained in `valid_apt()`}
#' }
#'
#' @references
#'   \insertAllCited{}
#'
#' @importFrom Rdpack reprompt
#' @importFrom mathjaxr preview_rd
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @export
valid_PPPratio <- function(data,
                           Year = "Year",
                           `Product code` = "Product code",
                           Region = "Region",
                           `Average price of product` = "Average price of product",
                           ...) {
  # Calculations
  tmp <- data |>
    # Calculate PPP price, first country is baseline
    group_by(.data[[`Product code`]]) |>
    mutate(
      PPP_item = ifelse(row_number() == 1, `Average price of product`, NA),
      PPP_item = mean(PPP_item, na.rm = TRUE),
      PPP_item = .data[[`Average price of product`]] / PPP_item
    ) |>
    # Calculate aggregate PPP
    group_by(.data[[Region]]) |>
    mutate(PPP_country = exp(mean(log(PPP_item)))) |>
    # Calculate PPP price relatives
    mutate(PPP_pricerel = .data[[`Average price of product`]] / PPP_country) |>
    # Calculate geometric mean
    group_by(.data[[`Product code`]]) |>
    mutate(gmean = exp(mean(log(PPP_pricerel)))) |>
    ungroup() |>
    # Calculate PPP ratios
    mutate(PPP_ratio = PPP_pricerel / gmean * 100) |>
    ungroup() |>
    # Calculate variation coefficients
    select({{ Year }}, {{ Region }}, {{ `Product code` }}, PPP_ratio) |>
    mutate(`VC Product` = sd(PPP_ratio), .by = `Product code`) |>
    mutate(`VC Region` = sd(PPP_ratio), .by = `Region`)

  # Variation coefficicients
  x1 <- tmp |>
    select({{ Year }}, {{ `Product code` }}, `VC Product`) |>
    distinct()

  x2 <- tmp |>
    select({{ Year }}, {{ Region }}, `VC Region`) |>
    distinct() |>
    pivot_wider(names_from = {{ Region }}, values_from = `VC Region`) |>
    mutate(`Product code` = "Region variation coefficients") |>
    select(Year, {{ `Product code` }}, everything()) |>
    mutate(`VC Product` = NA)

  # Final table
  tmp |>
    select(-contains("VC")) |>
    pivot_wider(names_from = {{ Region }}, values_from = PPP_ratio) |>
    left_join(x1, by = join_by(Year, `Product code`)) |>
    rbind(x2) |>
    rename("Product variation coefficients" = `VC Product`)
}
