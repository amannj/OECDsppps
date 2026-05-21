#' The Laspeyres price index
#'
#' \loadmathjax
#' `index_laspeyres()` in \pkg{OECDsppps} calculates the complete matrix of Laspeyres indices. It
#' returns a data frame containing the base region, region, and the respective indices;
#' see *Details* and
#' \insertCite{worldbankMeasuringRealSize2013;textual}{OECDsppps},
#' for more information.
#'
#' The Laspeyres index is a bilateral price index. For regions \mjseqn{j} and \mjseqn{k} it is obtained as
#' \mjdeqn{sPPP_L^{j,k} = \sum_{n=1}^N  w_n^j \times sPPP_n^{j,k}}{sPPP_L^{j,k} = \sum_{n=1}^N  w_n^j \times sPPP_n^{j,k}}
#' which is the weighted average of subnational PPPs of region \mjseqn{j} to
#' region \mjseqn{k} across the \mjseqn{N} basic headings using region \mjseqn{j} weights.
#'
#' The function returns a data frame containing the following variables: 'base_region' (region *j*), 'region' (region *k*),
#' 'laspeyres_index' (final indices).
#'
#' @references
#'   \insertAllCited{}
#'
#' @param data A data frame or tibble containing at least four columns identifying
#' region, product, subnational PPPs, and expenditure weights. The data is checked
#' using `valid_index_data()` prior to index calculation.
#' @param region Identifier for regions
#' @param product Product identifier
#' @param ppp_bh Identifier for subnational PPPs
#' @param exp_wght Identifier for expenditure weights
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' suppressPackageStartupMessages(library(tibble))
#' tibble(
#'   region = c("region A", "region A", "region B", "region B"),
#'   product = c("product 1", "product 2", "product 1", "product 2"),
#'   ppp_bh = c(0.5, 0.7, 0.6, 0.9),
#'   exp_wght = c(0.5, 0.5, 0.6, 0.4)
#' ) |>
#'   index_laspeyres()
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tibble rownames_to_column
#'
#' @export
#'
index_laspeyres <- function(data,
                            region = "region",
                            product = "product",
                            ppp_bh = "ppp_bh",
                            exp_wght = "exp_wght") {
  # Checks
  valid_index_data(data, region, product, ppp_bh, exp_wght)

  # Generate matrices
  # -> weights
  exp_weights_matrix <- matrix_generator(data, region, product, exp_wght)

  # -> ppps
  ppp_matrix <- matrix_generator(data, region, product, ppp_bh)

  # Get the number of regions
  n_region <- nrow(ppp_matrix)

  # Calculation
  # -> initialize list to store the results
  results_lasp <- vector("list", n_region)

  # Laspeyres index
  for (i in 1:n_region) {
    results_lasp[[i]] <- (sweep(ppp_matrix, 2, ppp_matrix[i, ], FUN = "/")) %*% exp_weights_matrix[i, ]
  }

  # Save the results
  lasp_matrix <- do.call(cbind, results_lasp)
  lasp_matrix <- t(lasp_matrix)

  rownames(lasp_matrix) <- colnames(lasp_matrix)

  lasp_df <- as.data.frame(lasp_matrix) %>%
    rownames_to_column(var = "base_region") %>%
    pivot_longer(!c(base_region),
      names_to = "region",
      values_to = "laspeyres_index"
    )

  return(lasp_df)
}

#' The Paasche price index
#'
#' \loadmathjax
#' `index_paasche()` in \pkg{OECDsppps} calculates the matrix of Paasche indices. It
#' returns a data frame containing the base region, region, and the respective indices;
#' see *Details* and
#' \insertCite{worldbankMeasuringRealSize2013;textual}{OECDsppps},
#' for more information.
#'
#' Paasche index for regions \mjseqn{j} and \mjseqn{k} is obtained as
#' \mjdeqn{sPPP_P^{j,k} = \frac{1}{\sum_{n=1}^{N} \frac{w_n^k}{sPPP_n^{j,k}}}}{sPPP_P^{j,k} = \frac{1}{\sum_{n=1}^{N} \frac{w_n^k}{sPPP_n^{j,k}}}}
#' which is a weighted average of the subnational PPPs of region \mjseqn{j} to
#' region \mjseqn{k} across the \mjseqn{N} basic headings using region \mjseqn{k} weights.
#'
#' The function returns a data frame containing the following variables: 'base_region' (region *j*), 'region' (region *k*),
#' 'paasche_index' (final indices).
#'
#' @references
#'   \insertAllCited{}
#'
#' @param data A data frame or tibble containing at least four columns identifying
#' region, product, subnational PPPs, and expenditure weights. The data is checked
#' using `valid_index_data()` prior to index calculation.
#' @param region Identifier for regions
#' @param product Product identifier
#' @param ppp_bh Identifier for subnational PPPs
#' @param exp_wght Identifier for expenditure weights
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' suppressPackageStartupMessages(library(tibble))
#' tibble(
#'   region = c("region A", "region A", "region B", "region B"),
#'   product = c("product 1", "product 2", "product 1", "product 2"),
#'   ppp_bh = c(0.5, 0.7, 0.6, 0.9),
#'   exp_wght = c(0.5, 0.5, 0.6, 0.4)
#' ) |>
#'   index_paasche()
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tibble rownames_to_column
#'
#' @export
#'
index_paasche <- function(data,
                          region = "region",
                          product = "product",
                          ppp_bh = "ppp_bh",
                          exp_wght = "exp_wght") {
  # Checks
  valid_index_data(data, region, product, ppp_bh, exp_wght)

  # Generate matrices
  # -> weights
  exp_weights_matrix <- matrix_generator(data, region, product, exp_wght)

  # -> ppps
  ppp_matrix <- matrix_generator(data, region, product, ppp_bh)

  # Get the number of regions
  n_region <- nrow(ppp_matrix)

  # Get region names
  region_names <- exp_weights_matrix %>%
    rownames()

  # Calculation
  # -> initialize list to store the results
  results_paas <- vector("list", n_region)

  results_paas_v <- vector("list", n_region)

  # Paasche index
  for (i in 1:n_region) {
    normalized_matrix <- 1 / sweep(ppp_matrix, 2, ppp_matrix[i, ], FUN = "/")

    for (j in 1:n_region) {
      results_paas_v[[j]] <- sum(normalized_matrix[j, ] * exp_weights_matrix[j, ])
    }

    results_paas[[i]] <- do.call(cbind, results_paas_v)
  }

  # Save the results
  paas_matrix <- do.call(rbind, results_paas)

  paas_matrix <- (paas_matrix)^(-1)

  rownames(paas_matrix) <- region_names
  colnames(paas_matrix) <- region_names

  paas_df <- as.data.frame(paas_matrix) %>%
    rownames_to_column(var = "base_region") %>%
    pivot_longer(!c(base_region),
      names_to = "region",
      values_to = "paasche_index"
    )

  return(paas_df)
}

#' The Fisher price index
#'
#' \loadmathjax
#' `index_fisher()` in \pkg{OECDsppps} calculates the matrix of Fisher indices. It
#' returns a data frame containing the base region, region, and the respective indices;
#' see *Details* and
#' \insertCite{worldbankMeasuringRealSize2013;textual}{OECDsppps},
#' for more information.
#'
#' The Fisher index for regions \mjseqn{j} and \mjseqn{k} is obtained as
#' \mjdeqn{sPPP_F^{j,k} = \left( sPPP_L^{j,k} \times sPPP_P^{j,k} \right)^{1/2} }{ sPPP_F^{j,k} = \left( sPPP_L^{j,k} \times sPPP_P^{j,k} \right)^{1/2}}
#' which is the geometric average of the Paasche and Laspeyres index.
#'
#' The function returns a data frame containing the following variables: 'base_region' (region *j*), 'region' (region *k*),
#' 'fisher_index' (final indices).
#'
#' @references
#'   \insertAllCited{}
#'
#' @param data A data frame or tibble containing at least four columns identifying
#' region, product, subnational PPPs, and expenditure weights. The data is checked
#' using `valid_index_data()` prior to index calculation.
#' @param region Identifier for regions
#' @param product Product identifier
#' @param ppp_bh Identifier for subnational PPPs
#' @param exp_wght Identifier for expenditure weights
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' suppressPackageStartupMessages(library(tibble))
#' tibble(
#'   region = c("region A", "region A", "region B", "region B"),
#'   product = c("product 1", "product 2", "product 1", "product 2"),
#'   ppp_bh = c(0.5, 0.7, 0.6, 0.9),
#'   exp_wght = c(0.5, 0.5, 0.6, 0.4)
#' ) |>
#'   index_fisher()
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tibble rownames_to_column
#'
#' @export
#'
index_fisher <- function(data,
                         region = "region",
                         product = "product",
                         ppp_bh = "ppp_bh",
                         exp_wght = "exp_wght") {
  # Laspeyres Index: matrix
  lasp_matrix <- index_laspeyres(
    data,
    region,
    product,
    ppp_bh,
    exp_wght
  ) %>%
    pivot_wider(
      names_from = "region",
      values_from = "laspeyres_index"
    ) %>%
    column_to_rownames(var = deparse(substitute(base_region))) %>%
    as.matrix()

  # Paasche Index: matrix
  paas_matrix <- index_paasche(
    data,
    region,
    product,
    ppp_bh,
    exp_wght
  ) %>%
    pivot_wider(
      names_from = "region",
      values_from = "paasche_index"
    ) %>%
    column_to_rownames(var = deparse(substitute(base_region))) %>%
    as.matrix()

  fisher_matrix <- sqrt(paas_matrix * lasp_matrix)

  fisher_df <- as.data.frame(fisher_matrix) %>%
    rownames_to_column(var = "base_region") %>%
    pivot_longer(!c(base_region),
      names_to = "region",
      values_to = "fisher_index"
    )

  return(fisher_df)
}

#' The Gini-Éltetö-Köves-Szulc (GEKS) price index
#'
#' \loadmathjax
#' `index_gex()` in \pkg{OECDsppps} calculates the GEKS indices. It
#' returns a data frame containing the base region, region, and the respective indices;
#' see *Details* and
#' \insertCite{worldbankMeasuringRealSize2013;textual}{OECDsppps},
#' for more information.
#'
#' Subnational PPPs for region \mjseqn{k} with reference to region \mjseqn{j} are
#' \mjdeqn{sPPP_G^{j,k} = \prod_{r=1}^R \left( sPPP_F^{j,r} \times sPPP_F^{r,k} \right)^{1/R}}{sPPP_G^{j,k} = \prod_{r=1}^R \left( sPPP_F^{j,r} \times sPPP_F^{r,k} \right)^{1/R}}
#' and correspond to the geometric average of the Fisher indices of all direct
#' comparisons between region \mjseqn{j}  and region k, and indirect comparisons
#' across all regions \mjseqn{r = 1, \dots, j, k, \dots, R}.
#'
#' The function returns a data frame containing the following variables: 'base_region' (region *j*), 'region' (region *k*),
#' 'geks_index' (final indices).
#'
#' @references
#'   \insertAllCited{}
#'
#' @param data A data frame or tibble containing at least four columns identifying
#' region, product, subnational PPPs, and expenditure weights. The data is checked
#' using `valid_index_data()` prior to index calculation.
#' @param region Identifier for regions
#' @param product Product identifier
#' @param ppp_bh Identifier for subnational PPPs
#' @param exp_wght Identifier for expenditure weights
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' suppressPackageStartupMessages(library(tibble))
#' tibble(
#'   region = c("region A", "region A", "region B", "region B"),
#'   product = c("product 1", "product 2", "product 1", "product 2"),
#'   ppp_bh = c(0.5, 0.7, 0.6, 0.9),
#'   exp_wght = c(0.5, 0.5, 0.6, 0.4)
#' ) |>
#'   index_geks()
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tibble rownames_to_column
#'
#' @export
#'
index_geks <- function(data,
                       region = "region",
                       product = "product",
                       ppp_bh = "ppp_bh",
                       exp_wght = "exp_wght") {
  # Fisher Index Matrix
  fisher_matrix <- index_fisher(
    data,
    region,
    product,
    ppp_bh,
    exp_wght
  ) %>%
    pivot_wider(
      names_from = "region",
      values_from = "fisher_index"
    ) %>%
    column_to_rownames(var = deparse(substitute(base_region))) %>%
    as.matrix()

  n_region <- nrow(fisher_matrix)

  geks_results <- vector("list", n_region)

  for (i in 1:n_region) {
    geks_v <- sweep(fisher_matrix, 1, fisher_matrix[, i], FUN = "/") %>%
      apply(., 2, prod)

    geks_results[[i]] <- geks_v^(1 / n_region)
  }

  geks_matrix <- do.call(rbind, geks_results)

  rownames(geks_matrix) <- colnames(geks_matrix)

  geks_df <- as.data.frame(geks_matrix) %>%
    rownames_to_column(var = "base_region") %>%
    pivot_longer(!c(base_region),
      names_to = "region",
      values_to = "geks_index"
    )

  return(geks_df)
}
