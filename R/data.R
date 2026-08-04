#' UK CPI microdata extract
#'
#' A subset of the official UK CIP microdata set published by the United
#' Kingdom Office for National Statistics (ONS)
#'
#' @format ## `uk_cpi`
#' A data frame with 11,807 rows and 14 columns containing two products:
#' *White sliced loaf branded 750 grams* (COICOP 01.1.1.3) and
#' *carpenter hourly rate* (COICOP 04.3.2.5).
#'
#' More information on COICOP classification can be found in the
#' [ONS Classification of household consumption headings](https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/satelliteaccounts/methodologies/consumertrendsuk/classificationofhouseholdconsumptionheadings2014tcm772368742.pdf) and in the
#' [UN Statistics Division’s COICOP 2018 statistical paper](https://unstats.un.org/unsd/classifications/unsdclassifications/COICOP_2018_pre_copy_edit_publication.pdf).
#'
#' \describe{
#'   \item{Year}{Year}
#'   \item{Date of quote}{year and month of data collection (yyyymm)}
#'   \item{coicop_5d}{COICOP 5-digit code}
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


#' UK Regional Household Final Consumption Expenditure data extract
#'
#' A subset of the official UK Regional Household Final Consumption Expenditure
#' data  set published by the United
#' Kingdom Office for National Statistics (ONS)
#'
#' @format ## `uk_hhe`
#' A data frame with 48 rows and 4 columns containing
#' regional household final consumption expenditure shares for two products:
#' *White sliced loaf branded 750 grams* (COICOP 01.1.1.3) and
#' *carpenter hourly rate* (COICOP 04.3.2.5).
#'
#' Note that for both products, the UK regional household final consumption
#' expenditures are available only at the *class* or four-digit level of the
#' COICOP classification, specifically `01.1.1` and `04.3.2`.
#'
#' More information on COICOP classification can be found in the
#' [ONS Classification of household consumption headings](https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/satelliteaccounts/methodologies/consumertrendsuk/classificationofhouseholdconsumptionheadings2014tcm772368742.pdf) and in the
#' [UN Statistics Division’s COICOP 2018 statistical paper](https://unstats.un.org/unsd/classifications/unsdclassifications/COICOP_2018_pre_copy_edit_publication.pdf).
#'
#' \describe{
#'   \item{Year}{Year}
#'   \item{Region}{Region, following [TL2](https://www.oecd.org/en/data/datasets/oecd-geographical-definitions.html)/[ITL1](https://www.ons.gov.uk/methodology/geography/ukgeographies/eurostat) boundaries)}
#'   \item{coicop_4d}{COICOP 4-digit code}
#'   \item{expenditure_share}{Regional household final consumption expenditure share}
#' }
#'
#' @examples
#' uk_hhe |> dim()
#' uk_hhe |> dplyr::distinct(coicop_4d)
#' uk_hhe |> names()
#'
#' @source [Regional household final consumption expenditure](https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalhouseholdfinalconsumptionexpenditureinternationalterritoriallevel1countriesandregionsandinternationalterritoriallevel2subregions)
"uk_hhe"

#' ECOICOP 2016 - Eurostat PPP Basic Headings correspondence table
#'
#' A correspondence table to map European Classification of Individual Consumption
#' according to Purpose, version 1 (also referred to as "ECOICOP 2016" or "ECOICOP 1")
#' to Basic Headings (BH) of the Eurostat PPP list of products for consumer goods and services.
#'
#' @format ## `concordance_ecoicop2bh`
#' A data frame with 358 rows and 5 columns containing the concordance to map ECOICOP codes onto BHs.
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
#' correspondence_ecoicop_bh |> dim()
#' correspondence_ecoicop_bh |> names()
#' correspondence_ecoicop_bh |> dplyr::distinct(type)
#'
#' @source [European Classification of Individual Consumption according to Purpose, version 1 (ECOICOP1)](https://showvoc.op.europa.eu/#/datasets/ESTAT_European_Classification_of_Individual_Consumption_according_to_Purpose_%28ECOICOP%29/data)
#' and [Eurostat PPP list of products for consumer goods and services](https://ec.europa.eu/eurostat/web/purchasing-power-parities/methodology)
"correspondence_ecoicop_bh"


#' Classification of Individual Consumption According to Purpose (ECOICOP 16)
#'
#' COICOP is a reference classification published by the United Nations Statistics
#' Division (UNSD) used in National accounts, Household Budget Surveys (HBS),
#' Price Statistics (HICP) and Purchasing Power Parities (PPP).
#' Eurostat introduced a harmonised revision of COICOP, yielding a single
#' harmonised COICOP classification at the five-digit (sub-class) level, referred to as “ECOICOP”
#' (or "ECOICOP 1") for COICOP 1999 standard.
#' The more recent COICOP 2018 standard is equivalent to ECOICOP 2.
#'
#'
#' @format ## `ecoicop16_names`
#' A data frame with 479 rows and 4 columns containing the code and description of the ECOICOP classification at various levels of aggregation,
#' from *division* (COICOP2, e.g., `01 Food and non-alcoholic beverages`) down to *subclasses* (COICOP5, e.g., `01.1.1.1 Rice`).
#'
#' \describe{
#'   \item{code}{ECOICOP code}
#'   \item{coicop_level}{Hierarchical level of ECOICOP classification, description}
#'   \item{coicop_level}{Hierarchical level of ECOICOP classification, code}
#'   \item{description}{ECOICOP description}
#'   }
#'
#'
#' @examples
#' ecoicop16_names |> dim()
#' ecoicop16_names |> names()
#'
#' @source [ Classification of Individual Consumption According to Purpose 1999 ](https://unstats.un.org/unsd/classifications/Family/Detail/5)
#' and [eurostat Glossary:COICOP HICP](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:COICOP_HICP)
"ecoicop16_names"


#'  OECD Territorial correspondence table
#'
#' OECD Territorial correspondence table (Territorial Level 2024, TL-2024)
#'
#' @format
#' A data frame with 3,750 rows and 17 columns containing names and different
#' classifications; see [OECD Territorial correspondence table ](https://stats.oecd.org/wbos/fileview2.aspx?IDFile=db68c5c3-5fd5-465c-b25b-b50aa14c2da1)
#' for more information.
#'
#'
#'
#' @examples
#' oecd_tl |> dim()
#' oecd_tl |> names()
#'
#' @source [ OECD Geographical Definitions ](https://www.oecd.org/en/data/datasets/oecd-geographical-definitions.html)
#' and [Regions, cities and local statistics](http://oe.cd/geostats)
"oecd_tl"

#'  Sample price quotes
#'
#' Sample price quotes for 5 regions, 5 product groups, and 5 generic products
#' created using the `rdata()` function from the \pkg{pricelevels} package; see
#' \insertCite{pricelevels;textual}{OECDsppps}.
#'
#' @format
#' A data frame with 124 rows and 4 columns containing the variables
#' - "heading" (typically in reference to the COICOP or related classification),
#' - "region" (the subnational, spatial identifier),
#' - "product" (the generic product for which price quotes are surveyed), and
#' - "price" (the reported price quote).
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' sampledata_prices |> dim()
#' sampledata_prices |> names()
#' sampledata_prices |> head()
#'
#' @source See R package  [ pricelevels: Spatial Price Level Comparisons](https://CRAN.R-project.org/package=pricelevels)
"sampledata_prices"

#'  Sample expenditure shares
#'
#' Sample expenditure shares for 5 regions, and 5 product groups,
#' created using the `rdata()` function from the \pkg{pricelevels} package; see
#' \insertCite{pricelevels;textual}{OECDsppps}.
#'
#' @format
#' A data frame with 25 rows and 3 columns containing the variables
#' - "heading" (typically in reference to the COICOP or related classification),
#' - "region" (the subnational, spatial identifier), and
#' - "exp_wght" (the generic expenditure shares for the corresponding heading and region).
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' sampledata_weights |> dim()
#' sampledata_weights |> names()
#' sampledata_weights |> head()
#'
#' @source See R package  [ pricelevels: Spatial Price Level Comparisons](https://CRAN.R-project.org/package=pricelevels)
"sampledata_weights"

#'  Sample price quotes and expenditure for a generic, multi-period sample
#'
#' Sample price quotes and expenditure shares for
#' 2 periods, 5 regions, 5 product groups, and 5 generic products,
#' created using the `rdata()` function from the \pkg{pricelevels} package; see
#' \insertCite{pricelevels;textual}{OECDsppps}.
#'
#' @format
#' A data frame with 250 rows and 8 columns containing the variables
#' - "period" (the temporal identifier),
#' - "heading" (typically in reference to the COICOP or related classification),
#' - "region" (the subnational, spatial identifier),
#' - "product" (the generic product for which price quotes are surveyed),
#' - "price" (the reported price quote),
#' - "quantity" (the reported quantity of the surveyed product),
#' - "sale" (a sales flag), and
#' - "exp_wght" (the generic expenditure shares for the corresponding heading and region).
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' sampledata_multi_period |> dim()
#' sampledata_multi_period |> names()
#' sampledata_multi_period |> head()
#'
#' @source See R package  [ pricelevels: Spatial Price Level Comparisons](https://CRAN.R-project.org/package=pricelevels)
"sampledata_multi_period"


#'  COICOP 2018 - COICOP 1999 correspondence table
#'
#' Correspondence table between COICOP 2018 and COICOP 1999. Note that
#' COICOP 2018 corresponds to ECOICOP 2.
#'
#' @format
#' A data frame with 688 rows and 4 columns containing the variables
#' - "coicop18_code " (COICOP 2018 code),
#' - "coicop18_description" (COICOP 2018 description),
#' - "coicop99_code" (COICOP 1999 code),
#' - "coicop99_description" (COICOP 1999 description),
#' - "comment" (classification comments)
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' correspondence_coicop18_coicop99 |> dim()
#' correspondence_coicop18_coicop99 |> names()
#' correspondence_coicop18_coicop99 |> head()
#'
#' @source See Statistics Lithuania for the  [correspondence table](https://osp.stat.gov.lt/en_GB/individualaus-vartojimo-islaidu-pagal-paskirti-klasifikatorius-coicop),
#' as well as the United Nattions Statistics Division for more information on
#' [COICOP 1999](https://unstats.un.org/unsd/classifications/Family/Detail/5) and
#' [COICOP 2018](https://unstats.un.org/unsd/classifications/Family/Detail/2094), respectively.
"correspondence_coicop18_coicop99"


#'  ECOICOP 2016 - COICOP 2018 correspondence table
#'
#' Correspondence table between ECOICOP 2018 (also referred to as "ECOICOP 1")
#'  and COICOP 2018.
#'  Note that COICOP 2018 corresponds to ECOICOP 2.
#'
#' @format
#' A data frame with 776 rows and 5 columns containing the variables
#' - "ecoicop_code" (ECOICOP 2016 code),
#' - "ecoicop_description" (ECOICOP 2016 description),
#' - "ecoicop_comments" (ECOICOP 2016 classification comments),
#' - "coicop18_code " (COICOP 2018 code),
#' - "coicop18_description" (COICOP 2018 description),
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' correspondence_ecoicop16_coicop18 |> dim()
#' correspondence_ecoicop16_coicop18 |> names()
#' correspondence_ecoicop16_coicop18 |> head()
#'
#' @source See Statistics Lithuania for the  [correspondence table](https://osp.stat.gov.lt/en_GB/individualaus-vartojimo-islaidu-pagal-paskirti-klasifikatorius-coicop),
#' as well as the United Nattions Statistics Division for more information on
#' [ECOICOP 2016](https://data.europa.eu/data/datasets/ecoicop?locale=en) and
#' [COICOP 2018](https://unstats.un.org/unsd/classifications/Family/Detail/2094), respectively.
"correspondence_ecoicop16_coicop18"
