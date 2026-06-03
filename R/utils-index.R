#' Validation of input data for index calculations
#'
#' `valid_index_data()` checks the validity of the input arguments used in the
#' Laspeyres, Paasche, Fisher (and GEKS) index calculations
#'
#' Checks for negative subnational PPPs, negative expenditure weights,
#' expenditure weights above 1, and expenditure weights summing to > 1
#'
#' @param data A data frame containing at least four columns including the
#' region, product, PPPs, and expenditure weights
#' @param region Column containing the region
#' @param product Column containing the product identifier
#' @param ppp_bh Column containing the subnational PPPs
#' @param exp_wght Column containing the expenditure weights
#'
#' @return Returns an error message if any of the checks fail. The error message specifies the
#' reason, e.g., "Following region/product pairs have negative weights:", and lists the problematic
#' region/product pairs.
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr near
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#'
#' @noRd
valid_index_data <- function(data,
                             region = "region",
                             product = "product",
                             ppp_bh = "ppp_bh",
                             exp_wght = "exp_wght") {
  # PPPs:
  # missing ppps
  missing_ppps <- data %>%
    select({{ region }}, {{ product }}, {{ ppp_bh }}) %>%
    pivot_wider(
      names_from = {{ product }},
      values_from = {{ ppp_bh }}
    ) %>%
    pivot_longer(!c({{ region }}),
      names_to = "product",
      values_to = "ppp_bh"
    ) %>%
    filter(is.na(ppp_bh))

  # check: missing ppps
  missing_ppps_check <- nrow(missing_ppps) > 0

  # warning: missing ppps
  if (missing_ppps_check) {
    stop(paste(
      "Incomplete PPP matrix. Missing PPPs for the following region/product combinations:",
      paste(
        paste(missing_ppps[[region]],
          missing_ppps[[product]],
          sep = "/"
        ),
        collapse = "; "
      )
    ))
  }

  # negative ppps
  negative_ppps <- data[data[[ppp_bh]] < 0, ]

  # check: negative ppps
  negative_ppps_check <- nrow(negative_ppps) > 0

  # warning: negative ppps
  if (negative_ppps_check) {
    warning(paste(
      "Following region/product pairs have negative PPPs:",
      paste(
        paste(
          negative_ppps[[region]],
          negative_ppps[[product]],
          sep = "/"
        ),
        collapse = "; "
      )
    ))
  }

  # Expenditure weights:
  # missing weights
  missing_exp_wghts <- data %>%
    select({{ region }}, {{ product }}, {{ exp_wght }}) %>%
    pivot_wider(
      names_from = {{ product }},
      values_from = {{ exp_wght }}
    ) %>%
    pivot_longer(!c({{ region }}),
      names_to = "product",
      values_to = "exp_wght"
    ) %>%
    filter(is.na(exp_wght))

  # check: missing weights
  missing_exp_check <- nrow(missing_exp_wghts) > 0

  # warning: missing weights
  if (missing_exp_check) {
    stop(paste(
      "Incomplete expenditure weights matrix. Missing weights for the following region/product combinations:",
      paste(
        paste(missing_exp_wghts[[region]],
          missing_exp_wghts[[product]],
          sep = "/"
        ),
        collapse = "; "
      )
    ))
  }

  # negative weights
  negative_exp_wghts <- data[data[[exp_wght]] < 0, ]

  # weights above 1
  above_one_exp_wghts <- data[data[[exp_wght]] > 1, ]

  # non-unity regional sums
  non_unity_regional_sums <- data %>%
    group_by(.data[[region]]) %>%
    summarise(regional_sum = sum(.data[[exp_wght]])) %>%
    filter(!near(regional_sum, 1))

  # check: negative weights
  negative_exp_check <- nrow(negative_exp_wghts) > 0

  # check: weights above 1
  above_one_check <- nrow(above_one_exp_wghts) > 0

  # check: non-unity regional sums
  unity_sum_check <- nrow(non_unity_regional_sums) > 0

  # warning: negative weights
  if (negative_exp_check) {
    warning(paste(
      "Following region/product pairs have negative weights:",
      paste(
        paste(
          negative_exp_wghts[[region]],
          negative_exp_wghts[[product]],
          sep = "/"
        ),
        collapse = "; "
      )
    ))
  }

  # warning: weights above 1
  if (above_one_check) {
    warning(paste(
      "Following region/product pairs have weights exceeding 1:",
      paste(
        paste(
          above_one_exp_wghts[[region]],
          above_one_exp_wghts[[product]],
          sep = "/"
        ),
        collapse = "; "
      )
    ))
  }

  # warning: non-unity regional sums
  if (unity_sum_check) {
    warning(paste(
      "Following regions' weights do not sum to 1:",
      paste(non_unity_regional_sums[[region]],
        collapse = "; "
      )
    ))
  }

  if (any(negative_ppps_check, negative_exp_check, above_one_check, unity_sum_check)) stop("Input data unsuitable for index calculation.")
}

#' Matrix generation
#'
#' `matrix_generator()` simple matrix generation function
#'
#' @return Returns a matrix object
#'
#' @importFrom tibble column_to_rownames
#' @importFrom tibble remove_rownames
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom tidyr pivot_wider
#'
#' @noRd
matrix_generator <- function(data,
                             region = "region",
                             product = "product",
                             values = "values") {
  # generate the matrix
  output_matrix <- data %>%
    select({{ region }}, {{ product }}, {{ values }}) %>%
    pivot_wider(
      names_from = {{ product }},
      values_from = {{ values }}
    ) %>%
    tibble::remove_rownames() %>%
    {
      m <- .
      matrix <- m %>%
        select(-{{ region }}) %>%
        as.matrix()
      rownames(matrix) <- m %>%
        pull(1)
      matrix
    }

  return(output_matrix)
}

