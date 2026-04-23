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

- Add COICOP is reference classification table `ecoicop16_names`
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
  and
  [`valid_PPPratio()`](https://amannj.github.io/OECDsppps/reference/valid_PPPratio.md)

------------------------------------------------------------------------

## Version `0.0.0.5`

Validate household expenditure data

#### Additions

- Add function to validate household expenditure data
  [`valid_est()`](https://amannj.github.io/OECDsppps/reference/valid_est.md)
- Add snipped of UK [Regional household final consumption
  expenditure](https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalhouseholdfinalconsumptionexpenditureinternationalterritoriallevel1countriesandregionsandinternationalterritoriallevel2subregions)
  data
