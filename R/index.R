#' Calculate the Laspeyres indices
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
                 values_to = "laspeyres_index")

  return(lasp_df)

}
#' Calculate the Paasche indices
#'
index_paasche <- function(data,
                          region = "region",
                          product = "product",
                          ppp_bh = "ppp_bh",
                          exp_wght = "exp_wght"
){
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

    normalized_matrix <- 1/sweep(ppp_matrix, 2, ppp_matrix[i, ], FUN = "/")

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
                 values_to = "paasche_index")

  return(paas_df)

}

#' Calculate the Fisher indices
#'
index_fisher <- function(data,
                         region = "region",
                         product = "product",
                         ppp_bh = "ppp_bh",
                         exp_wght = "exp_wght"
){
  # Laspeyres Index: matrix
  lasp_matrix <- index_laspeyres(data,
                                 region,
                                 product,
                                 ppp_bh,
                                 exp_wght) %>%
    pivot_wider(names_from = "region",
                values_from = "laspeyres_index") %>%
    column_to_rownames(var = deparse(substitute(base_region))) %>%
    as.matrix()

  # Paasche Index: matrix
  paas_matrix <- index_paasche(data,
                               region,
                               product,
                               ppp_bh,
                               exp_wght) %>%
    pivot_wider(names_from = "region",
                values_from = "paasche_index") %>%
    column_to_rownames(var = deparse(substitute(base_region))) %>%
    as.matrix()

  fisher_matrix <- sqrt(paas_matrix * lasp_matrix)

  fisher_df <- as.data.frame(fisher_matrix) %>%
    rownames_to_column(var = "base_region") %>%
    pivot_longer(!c(base_region),
                 names_to = "region",
                 values_to = "fisher_index")

  return(fisher_df)
}

#' GEKS
#'
index_geks <- function(data,
                       region = "region",
                       product = "product",
                       ppp_bh = "ppp_bh",
                       exp_wght = "exp_wght"
){
  # Fisher Index Matrix
  fisher_matrix <- index_fisher(data,
                                region,
                                product,
                                ppp_bh,
                                exp_wght) %>%
    pivot_wider(names_from = "region",
                values_from = "fisher_index") %>%
    column_to_rownames(var = deparse(substitute(base_region))) %>%
    as.matrix()

  n_region <- nrow(fisher_matrix)

  geks_results <- vector("list", n_region)

  for (i in 1:n_region) {

    geks_v <- sweep(fisher_matrix, 1, fisher_matrix[, i], FUN = "/") %>%
      apply(., 2, prod)

    geks_results[[i]] <- geks_v^(1/n_region)

  }

  geks_matrix <- do.call(rbind, geks_results)

  rownames(geks_matrix) <- colnames(geks_matrix)

  geks_df <- as.data.frame(geks_matrix) %>%
    rownames_to_column(var = "base_region") %>%
    pivot_longer(!c(base_region),
                 names_to = "region",
                 values_to = "geks_index")

  return(geks_df)

}

#' Validation
#'
valid_index_data <- function(data,
                             region = "region",
                             product = "product",
                             ppp_bh = "ppp_bh",
                             exp_wght = "exp_wght"
){
  # PPPs:
  # missing ppps
  missing_ppps <- data %>%
    select({{ region }}, {{ product }}, {{ ppp_bh }}) %>%
    pivot_wider(names_from = {{ product }},
                values_from = {{ ppp_bh }}) %>%
    pivot_longer(!c({{ region }}),
                 names_to = "product",
                 values_to = "ppp_bh") %>%
    filter(is.na(ppp_bh))

  # check: missing ppps
  missing_ppps_check <- nrow(missing_ppps) > 0

  # warning: missing ppps
  if(missing_ppps_check) stop(paste("Incomplete PPP matrix. Missing PPPs for the following region/product combinations:",
                                    paste(paste(missing_ppps[[region]],
                                                missing_ppps[[product]],
                                                sep = "/"),
                                          collapse = "; ")))

  # negative ppps
  negative_ppps <- data[data[[ppp_bh]] < 0, ]

  # check: negative ppps
  negative_ppps_check <- nrow(negative_ppps) > 0

  # warning: negative ppps
  if(negative_ppps_check) warning(paste(
    "Following region/product pairs have negative PPPs:",
    paste(
      paste(
        negative_ppps[[region]],
        negative_ppps[[product]],
        sep = "/"),
      collapse = "; ")
  ))

  # Expenditure weights:
  # missing weights
  missing_exp_wghts <- data %>%
    select({{ region }}, {{ product }}, {{ exp_wght }}) %>%
    pivot_wider(names_from = {{ product }},
                values_from = {{ exp_wght }}) %>%
    pivot_longer(!c({{ region }}),
                 names_to = "product",
                 values_to = "exp_wght") %>%
    filter(is.na(exp_wght))

  # check: missing weights
  missing_exp_check <- nrow(missing_exp_wghts) > 0

  # warning: missing weights
  if(missing_exp_check) stop(paste("Incomplete expenditure weights matrix. Missing weights for the following region/product combinations:",
                                   paste(paste(missing_exp_wghts[[region]],
                                               missing_exp_wghts[[product]],
                                               sep = "/"),
                                         collapse = "; ")))

  # negative weights
  negative_exp_wghts <- data[data[[exp_wght]] < 0, ]

  # weights above 1
  above_one_exp_wghts <- data[data[[exp_wght]] > 1, ]

  # non-unity regional sums
  non_unity_regional_sums <- data %>%
    group_by(.data[[region]]) %>%
    summarise(regional_sum = sum(.data[[exp_wght]])) %>%
    filter(regional_sum != 1)

  # check: negative weights
  negative_exp_check <- nrow(negative_exp_wghts) > 0

  # check: weights above 1
  above_one_check <- nrow(above_one_exp_wghts) > 0

  # check: non-unity regional sums
  unity_sum_check <- nrow(non_unity_regional_sums) > 0

  # warning: negative weights
  if(negative_exp_check) warning(paste(
    "Following region/product pairs have negative weights:",
    paste(
      paste(
        negative_exp_wghts[[region]],
        negative_exp_wghts[[product]],
        sep = "/"),
      collapse = "; ")
  ))

  # warning: weights above 1
  if(above_one_check) warning(paste(
    "Following region/product pairs have weights exceeding 1:",
    paste(
      paste(
        above_one_exp_wghts[[region]],
        above_one_exp_wghts[[product]],
        sep = "/"),
      collapse = "; ")
  ))

  # warning: non-unity regional sums
  if(unity_sum_check) warning(paste(
    "Following regions' weights do not sum to 1:",
    paste(non_unity_regional_sums[[region]],
          collapse = "; ")
  ))

  if(any(negative_ppps_check, negative_exp_check, above_one_check, unity_sum_check)) stop("Input data unsuitable for index calculation.")
}
#' Matrix generation
#'
matrix_generator <- function(data,
                             region = "region",
                             product = "product",
                             values = "values"){

  # generate the matrix
  output_matrix <- data %>%
    select({{ region }}, {{ product }}, {{ values }}) %>%
    pivot_wider(
      names_from = {{ product }},
      values_from = {{ values }}
    ) %>%
    remove_rownames %>%
    column_to_rownames(var = deparse(substitute(region))) %>%
    as.matrix()

  return(output_matrix)
}
