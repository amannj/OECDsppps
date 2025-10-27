#' Create *Price Observation Table*
#'
#' `valid_pot` Creates the Price Observation Table used for Stage 1 of
#' the intra-regional validation process. In returns a tibble with the
#' input parameters (variables), as well as the `Ratio-to-average price test` and
#' `T-value test`. All price quotes that do not pass the two tests
#'  are flagged in columns
#' `Ratio-to-average price test FLAG` and `T-value test FLAG`, respectively.
#'  *TODO add description*
# ` * also add `Reference quantity price`
#' @param Year add
#' @param `Product code` add
#' @param `Product description` add
#' @param `Reference quantity` add
#' @param `Unit of reference quantity` add
#' @param `Date of quote` add
#' @param `Region` add
#' @param `Shop identifier` add
#' @param `Type of shop` add
#' @param `Quantity observed` add
#' @param `Unit of observed quantity` add
#' @param `Price observed` add
#' @param `Reference quantity price` add
valid_pot <- function(df,
                      Year = "Year",
                      `Product code` = "Product code",
                      `Product description` = "Product description",
                      `Reference quantity` = "Reference quantity",
                      `Unit of reference quantity` = "Unit of reference quantity",
                      `Date of quote` = "Date of quote",
                      `Region` = "Region",
                      `Shop identifier` = "Shop identifier",
                      `Type of shop` = "Type of shop",
                      `Quantity observed` = "Quantity observed",
                      `Unit of observed quantity` = "Unit of observed quantity",
                      `Price observed` = "Price observed",
                      `Reference quantity price` = "Reference quantity price") {
  df_tmp <- df |>
    select(
      Year = {{ Year }},
      `Product code` = {{ `Product code` }},
      `Product description` = {{ `Product description` }},
      `Reference quantity` = {{ `Reference quantity` }},
      `Unit of reference quantity` = {{ `Unit of reference quantity` }},
      `Date of quote` = {{ `Date of quote` }},
      `Region` = {{ `Region` }},
      `Shop identifier` = {{ `Shop identifier` }},
      `Type of shop` = {{ `Type of shop` }},
      `Quantity observed` = {{ `Quantity observed` }},
      `Unit of observed quantity` = {{ `Unit of observed quantity` }},
      `Price observed` = {{ `Price observed` }},
      `Reference quantity price` = {{ `Reference quantity price` }}
    )

  df_tmp |>
    # Calculate individual price outlier statistics
    group_by(Year, `Product code`, `Product description`) |>
    mutate(
      `Ratio-to-average price test` = `Reference quantity price` / mean(`Reference quantity price`),
      `T-value test` = (`Reference quantity price` - mean(`Reference quantity price`)) / sd(`Reference quantity price`)
    ) |>
    ungroup() |>
    # Add flags for selection rules
    mutate(
      `Ratio-to-average price test FLAG` = ifelse(`Ratio-to-average price test` < 0.5 | `Ratio-to-average price test` > 1.5, TRUE, FALSE),
      `T-value test FLAG` = ifelse(`T-value test` > 2, TRUE, FALSE)
    )
}

#' Create *Average Price Table*
#'
#' `valid_apt` Creates the Average Price Table used for Stage 2 of
#' the intra-regional validation process. In returns a tibble with the
#' input parameters (variables), as well as the
#' `Number of observations`,
#' `Average price of product`,
#' `Maximum price of product`,
#' `Minimum price of product`, and `Standard deviation`.
#' It furthermore provides the `Max-min ratio` and
#' `Coefficient of variation`
#' statistics. All products that do not pass the two tests
#'  are flagged in columns
#'
#' `Max-min ratio FLAG` and `Coefficient of variation FLAG`, respectively.
#'  *TODO add description*
# ` * also add `Reference quantity price`
#'
#' @param Year add
#' @param `Product code` add
#' @param `Product description` add
#' @param `Reference quantity` add
#' @param `Unit of reference quantity` add
#' @param `Reference quantity price` add
valid_apt <- function(df,
                      Year = "Year",
                      `Product code` = "Product code",
                      `Product description` = "Product description",
                      `Reference quantity` = "Reference quantity",
                      `Unit of reference quantity` = "Unit of reference quantity",
                      `Reference quantity price` = "Reference quantity price") {
  df_tmp <- df |>
    select(
      Year = {{ Year }},
      `Product code` = {{ `Product code` }},
      `Product description` = {{ `Product description` }},
      `Reference quantity` = {{ `Reference quantity` }},
      `Unit of reference quantity` = {{ `Unit of reference quantity` }},
      `Reference quantity price` = {{ `Reference quantity price` }}
    )

  df_tmp |>
    # Aggregate price by year and product
    group_by(Year, `Product code`, `Product description`) |>
    ## Number of observations
    mutate(nobs = 1) |>
    ## Summary stats
    summarise(
      `Number of observations` = sum(nobs),
      `Average price of product` = mean(`Reference quantity price`),
      `Maximum price of product` = max(`Reference quantity price`),
      `Minimum price of product` = min(`Reference quantity price`),
      `Standard deviation` = sd(`Reference quantity price`)
    ) |>
    ungroup() |>
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


#' Calculate *XR-ratio tables*
#'
#' `valid_XRratio` Calculates the XR-ratio, which is a region/country's XR-price
#' for a product divided by the geometric mean of the product price.
#' Variability can be compared to highlight products in countries/regions
#' that are most variable (high XR-ratio) across countries.
#'
#' @param Year add
#' @param `Product code` add
#' @param `Average price of product` add
#' @param `XR USD` add
valid_XRratio <- function(df,
                          Year = "Year",
                          `Product code` = "Product code",
                          `Average price of product` = "Average price of product",
                          `XR USD` = "XR USD") {
  df |>
    mutate(`XR Average price of product` = .data[[`Average price of product`]] * .data[[`XR USD`]]) |>
    # Calculate XR-Ratio
    group_by(.data[[`Product code`]]) |>
    mutate(`Geometric mean` = exp(mean(log(`XR Average price of product`)))) |>
    ungroup() |>
    # Calculate XR-ratios
    mutate(`XR-ratio` = `XR Average price of product` / `Geometric mean` * 100)
}


#' Calculate *PPP-ratio tables*
#'
#' `valid_PPPratio` Calculates the PPP-ratio, which shows the The variation
#' coefficient representing variability across products and across countries.
#'
#' - The (country) variation coefficient row measures the standard deviation of
#' the product PPPs for each country, thereby highlighting countries with the
#' highest variability.
#' - The (product) variation coefficient column measures the standard
#' deviation among the PPP ratios for each product.
#'
#' @param Year add
#' @param `Product code` add
#' @param `Average price of product` add
#' @param `XR USD` add
valid_PPPratio <- function(df,
                           Year = "Year",
                           `Product code` = "Product code",
                           `Country` = "Country",
                           `Average price of product` = "Average price of product") {
  # Calculations
  tmp <- df |>
    # Calculate PPP price, first country is baseline
    group_by(.data[[`Product code`]]) |>
    mutate(
      PPP_item = ifelse(row_number() == 1, `Average price of product`, NA),
      PPP_item = mean(PPP_item, na.rm = TRUE),
      PPP_item = .data[[`Average price of product`]] / PPP_item
    ) |>
    # Calculate aggregate PPP
    group_by(.data[[Country]]) |>
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
    select({{ Year }}, {{ Country }}, {{ `Product code` }}, PPP_ratio) |>
    mutate(`VC Product` = sd(PPP_ratio), .by = `Product code`) |>
    mutate(`VC Country` = sd(PPP_ratio), .by = `Country`)

  # Variation coefficicients
  x1 <- tmp |>
    select({{ Year }}, {{ `Product code` }}, `VC Product`) |>
    distinct()

  x2 <- tmp |>
    select({{ Year }}, {{ Country }}, `VC Country`) |>
    distinct() |>
    pivot_wider(names_from = {{ Country }}, values_from = `VC Country`) |>
    mutate(`Product code` = "Country variation coefficients") |>
    select(Year, {{ `Product code` }}, everything()) |>
    mutate(`VC Product` = NA)

  # Final table
  tmp |>
    select(-contains("VC")) |>
    pivot_wider(names_from = {{ Country }}, values_from = PPP_ratio) |>
    left_join(x1) |>
    rbind(x2) |>
    rename("Product variation coefficients" = `VC Product`)
}
