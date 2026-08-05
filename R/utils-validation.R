#' Weighted std. deviation
#'
#' `safe_weighted_sd()` calculates weighted standard deviation
#'
#' @noRd
safe_weighted_sd <- function(x, w) {

  ok <- stats::complete.cases(x, w) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]

  if (length(x) < 2L) return(NA_real_)

  w <- w / sum(w)
  x.bar <- sum(w * x)

  sqrt(sum(w * (x - x.bar)^2))
}

#' Kendall's concordance coefficient
#'
#' `safe_kendall_b()` calculates Kendall's concordance coefficient
#'
#' @noRd
safe_kendall_b <- function(x, y) {

  ok <- stats::complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]

  if (length(x) < 2L) return(NA_real_)

  out <- suppressWarnings(stats::cor(x, y, method = "kendall"))

  if (is.nan(out)) NA_real_ else out
}

#' Pair-weighted concordance coefficient
#'
#' `safe_weighted_kendall_b()` calculates a weighted version of the concordance coefficient
#'
#' @noRd
safe_weighted_kendall_b <- function(x, y, w) {

  ok <- stats::complete.cases(x, y, w) & is.finite(w) & w > 0
  x <- x[ok]
  y <- y[ok]
  w <- w[ok]

  n <- length(x)
  if (n < 2L) return(NA_real_)

  C <- D <- Tx <- Ty <- 0

  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {

      wij <- w[i] * w[j]

      sx <- sign(x[i] - x[j])
      sy <- sign(y[i] - y[j])

      if (sx == 0 && sy == 0) {
        next
      } else if (sx == 0) {
        Tx <- Tx + wij
      } else if (sy == 0) {
        Ty <- Ty + wij
      } else if (sx == sy) {
        C <- C + wij
      } else {
        D <- D + wij
      }
    }
  }

  denom <- sqrt((C + D + Tx) * (C + D + Ty))

  if (!is.finite(denom) || denom == 0) {
    NA_real_
  } else {
    (C - D) / denom
  }
}
