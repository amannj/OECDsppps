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

CPD regression update, household expenditure weight checks and bug fixes

#### Additions

- Add option to run
  [`estim_cpd()`](https://amannj.github.io/OECDsppps/reference/estim_cpd.md)
  with covariates and interactions
- Validation checks for household expenditure weights added:
  [`valid_axt()`](https://amannj.github.io/OECDsppps/reference/valid_axt.md)
- Describe workflow to derive initial CPD regression results at
  basic-heading level in
  [vignettes/Implementation](https://amannj.github.io/OECDsppps/articles/Implementation.html)

#### Bug fixes

- Fixed names in `ecoicop16_names`
