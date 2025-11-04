#' UK CPI microdata extract
#'
#' A subset of the official UK CIP microdata set
#'
#' @format ## `uk_cpi`
#' A data frame with 11,807 rows and 14 columns containing two products:
#' *White sliced loaf branded 750 grams* (COICOP 1010103) and
#' *carpenter hourly rate* (COICOP 410518).
#'
#' \describe{
#'   \item{Year}{Year}
#'   \item{Date of quote}{year and month of data collection (yyyymm)}
#'   \item{COICOP5}{COICOP 5-digit code}
#'   \item{Product code}{Identification of the item collected}
#'   \item{Product description}{Description of item collected}
#'   \item{Reference quantity}{Reference quantity of commodity that *should* be surveyed}
#'   \item{Unit of reference quantity}{Unit of reference quantity of commodity that *should* be surveyed}
#'   \item{Region}{Region, following [TL2](https://www.oecd.org/en/data/datasets/oecd-geographical-definitions.html)/[ITL1](https://www.ons.gov.uk/methodology/geography/ukgeographies/eurostat) boundaries)}
#'   \item{Shop identifier}{Code of shop that the price was collected from}
#'   \item{Type of shop}{Either "Multiple" (10 or more outlets) or "Independents" (less than 10 outlets)}
#'   \item{Quantity observed}{Quantity observed of commodity that *was* be surveyed}
#'   \item{Unit of observed quantity}{Unit of observed quantity of commodity that *was* be surveyed}
#'   \item{Price observed}{Price observed of commodity that *was* be surveyed}
#'   \item{Reference quantity price}{The *reference quantity price* scales the observed price to the quantity that *should* be surveyed.
#'   It is defined as `Price observed` / `Quantity observed` * `Reference quantity` whenever `Unit of observed quantity` == `Unit of reference quantity`.}
#' }
#'
#' @examples
#' uk_cpi |> dim()
#' uk_cpi |> dplyr::distinct(`Product description`)
#' uk_cpi |> names()
#'
#' @source [ONS Consumer price inflation consumption segment indices and price quotes ](https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes), last access November 2025
"uk_cpi"



