#' `%not.in%`: opposite of `%in%`
#'
#' `%not.in%` exclude rows with values specified in a vector.
#'
#' @param x The first vector
#' @param y The second vector
#'
#' @return Vector excluding values specified.
#' @examples
#'  x <- c(1,2,3)
#'  y <-  c(2,3)
#'  x %not.in% y
#' @export
"%not.in%" <- function(x, y){ !("%in%"(x, y))}
