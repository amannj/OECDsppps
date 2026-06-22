#' `%not.in%`: opposite of `%in%`
#'
#' `%not.in%` exclude rows with values specified in a vector.
#'
#' @param x The first vector
#' @param y The second vector
#'
#' @return Vector excluding values specified.
#' @noRd
"%not.in%" <- function(x, y) {
  !("%in%"(x, y))
}
