#' Calculate the matrix of Laspeyres indices
#'
#' \loadmathjax
#' `index_laspeyres` in \pkg{OECDsppps} calculates the matrix of Laspeyres indices
#'
#' Laspeyres index for regions \mjseqn{j} and \mjseqn{k} is obtained as
#' \mjdeqn{PPP_L^jk = \sum(w_n^j PPP_n^jk}{PPP_L^jk = \sum(w_n^j PPP_n^jk}
#'
#' @param data A data frame containing at least four columns including the
#' region, product, PPPs, and expenditure weights.
#' @param region Column containing the region.
#' @param product Column containing the product identifier.
#' @param ppp_bh Column containing the PPPs.
#' @param exp_wght Column containing the expenditure weights.
#'
#'
#'
index_laspeyres <- function(data,
                            region = "region",
                            product = "product",
                            ppp_bh = "ppp_bh",
                            exp_wght = "exp_wght"
){
  # Generate matrices
  # -> weights
  exp_weights_matrix <- data %>%
    select({{ region }}, {{ product }}, {{ exp_wght }}) %>%
    pivot_wider(
      names_from = {{ product }},
      values_from = {{ exp_wght }}
    ) %>%
    remove_rownames %>%
    column_to_rownames(var = deparse(substitute(region))) %>%
    as.matrix()

  # -> ppps
  ppp_matrix <- data %>%
    select({{ region }}, {{ product }}, {{ ppp_bh }}) %>%
    pivot_wider(
      names_from = {{ product }},
      values_from = {{ ppp_bh }}
    ) %>%
    remove_rownames %>%
    column_to_rownames(var = deparse(substitute(region))) %>%
    as.matrix()

  # Get the number of regions
  n_region <- nrow(ppp_matrix)

  # Checks
  # 1. matrix completeness
  # -> basic heading PPPs
  if(sum(is.na(ppp_matrix)) > 0) stop("Incomplete basic heading matrix.")
  # -> weights
  if(sum(is.na(exp_weights_matrix)) > 0) stop("Incomplete expenditure weights matrix.")

  # 2. weights
  # -> non-negative
  if(sum(sum(exp_weights_matrix < 0) > 0)) stop("Expenditure weights contain negative values.")
  # -> all below 1
  if(sum(exp_weights_matrix > 1) > 0) stop("At least one expenditure weight exceeds 1.")
  # -> sum up to one
  if(sum(rowSums(exp_weights_matrix) != 1) > 0) stop("Sum of expenditure weights exceeds 1 for at least one region.")

  # 3. PPPs
  # -> non-negative
  if(sum(sum(ppp_matrix < 0) > 0)) stop("Some PPPs are negative.")

  # 4. Compatibility of weights and PPPs
  # -> PPPs and weight matrices have the same number of dimensions
  if(sum(dim(ppp_matrix) == dim(exp_weights_matrix)) != 2) stop("Supplied PPPs and expenditure weights imply different number of region/product combinations.")


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

  return(lasp_matrix)

}
