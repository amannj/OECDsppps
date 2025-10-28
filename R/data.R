#' UK CPI microdata extract
#'
#' A subset of the official UK CIP microdata set
#'
#' @format ## `uk_cpi`
#' A data frame with 6,845 rows and 14 columns:
#' \describe{
#'   \item{Year}{Year}
#'   \item{COICOP5}{COICOP 5-digit code}
#'   \item{Product code}{identification of the item collected}
#'   \item{Product description}{Product description}
#'   \item{Reference quantity}{Reference quantity}
#'   \item{Unit of reference quantity}{Unit of reference quantity}
#'   \item{Date of quote}{year and month of data collection (yyyymm)}
#'   \item{Region}{Region (TL2/ITL1)}
#'   \item{Shop identifier}{code of shop that the price was collected from}
#'   \item{Type of shop}{1 = Multiple (10 or more outlets); 2 = Independents (less than 10 outlets)}
#'   \item{Quantity observed}{Quantity observed}
#'   \item{Unit of observed quantity}{Unit of observed quantity}
#'   \item{Price observed}{Price observed}
#'   \item{Reference quantity price}{Reference quantity price}
#' }
#' @examples
#' head(uk_cpi)
#'
#' @source <https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes>
"uk_cpi"
