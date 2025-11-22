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


#' Concordance table from ECOICOP1 to Eurostat PPP Basic Headings
#'
#' A concordance table to map European Classification of Individual Consumption according to Purpose, version 1 (ECOICOP1) to Basic Headings (BH) of the Eurostat PPP list of products for consumer goods and services
#'
#' @format ## `concordance_ecoicop2bh`
#' A data frame with 358 rows and 5 columns containing the concordance to map ECOICOP1s onto BHs.
#'
#' \describe{
#'   \item{ecoicop1_code}{ECOICOP1 code}
#'   \item{ecoicop1_name}{ECOICOP1 description}
#'   \item{bh_code}{Basic headings code}
#'   \item{bh_name}{Basic headings description}
#'   \item{type}{Type of concordance:}
#'   }
#'
#' - `1:1` direct concordance between ECOICOP1 and BH
#' - `N:1` multiple ECOICOP1 onto one BH:
#'   - e.g. ECOICOP1s *02.1.1.1*-Spirits and liqueurs and *02.1.1.2*-Alcoholic
#' soft drinks onto BH *A.02.1.1.0*-Spirits
#' - `1:N` one ECOICOP1 onto multiple BHs:
#'   - e.g. ECOICOP1 *06.3*-Hospital services onto BHs *A.06.3.0.1*-General hospitals
#' *A.06.3.0.2*-Mental health and substance abuse hospitals;
#' *A.06.3.0.3*-Speciality hospitals and *A.06.3.0.4*-Nursing and residential care facilities
#' - `NA` not available: BHs go beyond household consumption
#'
#' @examples
#' concordance_ecoicop2bh |> dim()
#' concordance_ecoicop2bh |> names()
#' concordance_ecoicop2bh |> dplyr::distinct(type)
#'
#' @source [European Classification of Individual Consumption according to Purpose, version 1 (ECOICOP1)](https://showvoc.op.europa.eu/#/datasets/ESTAT_European_Classification_of_Individual_Consumption_according_to_Purpose_%28ECOICOP%29/data)
#' and [Eurostat PPP list of products for consumer goods and services](https://ec.europa.eu/eurostat/web/purchasing-power-parities/methodology)
"concordance_ecoicop2bh"
