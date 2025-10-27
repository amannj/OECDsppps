#' Opposite of `%in%`
#'
#' `%!in%` exclude rows with values specified in a vector.
#'
#' @param x The first vector
#' @param y The second vector
#'
#' @return Vector excluding values specified.
#' @examples
#' c(1,2,3) %!in% c(2,3)
#' @export
"%!in%" <- function(x, y){ !("%in%"(x, y))}
