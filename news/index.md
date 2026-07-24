# Changelog

## Version `0.0.0.1`

Initial validation

#### Additions

- Validation vignette sections: *Intra-regional validation*,
  *Inter-regional validation* and *Validation of alternative data
  sources*

------------------------------------------------------------------------

## Version `0.0.0.2`

Initial estimation and extending validation

#### Additions

- Estimation vignette section: *Estimation of item-level prices to basic
  heading level*
- Validation vignette section: *Validation at basic-heading level*
- Implementation vignette section: *Overview*

#### Improvements

- Readability of vignettes
- Fix function bindings to pass checks
- Fix `uk_cpi`: Region names; harmonise COICOP code formats

------------------------------------------------------------------------

## Version `0.0.0.3`

Completing estimation procedure for basic headings

#### Additions

- Add COICOP reference classification table `ecoicop16_names`
- Add option to output residuals for CPD regression
  [`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
- Describe workflow to derive initial CPD regression results at
  basic-heading level in
  [vignettes/Implementation](https://amannj.github.io/OECDsppps/articles/Implementation.html)

#### Bug fixes

- Fixed bug(s) in
  [`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)

------------------------------------------------------------------------

## Version `0.0.0.4`

CPD regression update, checks for basic-heading CPD regression outputs
and household expenditure weights, add OECD Territorial correspondence
table and various bug fixes

#### Additions

- OECD Territorial correspondence table `oecd_tl`
- Option to run
  [`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
  with price-region pair duplicates in raw data using option
  `weights = 'raw'`
- Validation checks for CPD regression output and household expenditure
  weights

#### Bug fixes

- Fixed names in `ecoicop16_names`
- Fixed bugs in
  [`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
  and `valid_PPPratio()`

------------------------------------------------------------------------

## Version `0.0.0.5`

Validate household expenditure data and add Laspeyres, Paasche, Fisher
and GEKS index calculation

#### Additions

- Add function to validate household expenditure data
  [`valid_est()`](https://amannj.github.io/OECDsppps/reference/valid_est.md)
- Add a snippet of UK [Regional household final consumption
  expenditure](https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalhouseholdfinalconsumptionexpenditureinternationalterritoriallevel1countriesandregionsandinternationalterritoriallevel2subregions)
  data

------------------------------------------------------------------------

## Version `0.0.0.6`

Add index calculation functions, description of using `OECDsppps` in
Python and SAS, better implementation description

#### Additions

- Add functions
  [`index_laspeyres()`](https://amannj.github.io/OECDsppps/reference/index_laspeyres.md),
  [`index_paasche()`](https://amannj.github.io/OECDsppps/reference/index_paasche.md),
  [`index_fisher()`](https://amannj.github.io/OECDsppps/reference/index_fisher.md)
  and
  [`index_geks()`](https://amannj.github.io/OECDsppps/reference/index_geks.md)
  for estimating subnational PPPs using Laspeyres, Paasche, Fischer and
  Gini-Éltetö-Köves-Szulc (GEKS) price indices
- Add
  [description](https://amannj.github.io/OECDsppps/articles/altSoftware.html)
  data on using `OECDsppps` in Python and SAS
- Update
  [implementation](https://amannj.github.io/OECDsppps/articles/Implementation.html)
  vignette

------------------------------------------------------------------------

## Version `0.0.0.7`

Complete estimation and validation pipelines and improve visualisation

#### Additions

- Add functions
  [`valid_outlier_plot()`](https://amannj.github.io/OECDsppps/reference/valid_outlier_plot.md),
  [`valid_dikhanov()`](https://amannj.github.io/OECDsppps/reference/valid_dikhanov.md)
  and
  [`estim_index_link()`](https://amannj.github.io/OECDsppps/reference/estim_index_link.md)
  to complete the validation and estimation pipelines
- Rename functions
  [`valid_ratio_ppp()`](https://amannj.github.io/OECDsppps/reference/valid_ratio_ppp.md)
  and
  [`valid_ratio_xr()`](https://amannj.github.io/OECDsppps/reference/valid_ratio_xr.md)
  and fix naming inconsistencies
- Update
  [implementation](https://amannj.github.io/OECDsppps/articles/Implementation.html)
  to describe complete sPPPs production pipeline describing the use of
  [`estim_index_link()`](https://amannj.github.io/OECDsppps/reference/estim_index_link.md)
- Improve visualisation of package vignette

------------------------------------------------------------------------

## Version `0.0.0.8`

Big housekeeping: Streamline documentation, vignettes, simplify and
harmonise function names and parameters, grammar, etc.

#### Additions

- Added numbers of observations to
  [`valid_est()`](https://amannj.github.io/OECDsppps/reference/valid_est.md)
- Add sample data sets
  [`sampledata_prices()`](https://amannj.github.io/OECDsppps/reference/sampledata_prices.md),
  [`sampledata_weights()`](https://amannj.github.io/OECDsppps/reference/sampledata_weights.md)
  and
  [`sampledata_multi_period()`](https://amannj.github.io/OECDsppps/reference/sampledata_multi_period.md)
- Add lifeycle with experimental badge

#### Bug fixes

- Fix
  [`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
  in [\#8](https://github.com/amannj/OECDsppps/issues/8)
