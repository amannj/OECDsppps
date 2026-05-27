# OECDsppps 

Development version.

------------

# Version `0.0.0.1`

Initial validation

### Additions

  - Validation vignette sections: 
  *Intra-regional validation*,
  *Inter-regional validation* and
  *Validation of alternative data sources*
  
------------

# Version `0.0.0.2`

Initial estimation and extending validation

### Additions

- Estimation vignette section: *Estimation of item-level prices to basic heading level*
- Validation vignette section: *Validation at basic-heading level*
- Implementation vignette section: *Overview*

### Improvements

  - Readability of vignettes
  - Fix function bindings to pass checks
  - Fix `uk_cpi`: Region names; harmonise COICOP code formats  
    

------------

# Version `0.0.0.3`

Completing estimation procedure for basic headings

### Additions

- Add COICOP is reference classification table `ecoicop16_names`
- Add option to output residuals for CPD regression `estim_cpd()`
- Describe workflow to derive initial CPD regression results at basic-heading level in [vignettes/Implementation](https://amannj.github.io/OECDsppps/articles/Implementation.html)

### Bug fixes

- Fixed bug(s) in `estim_cpd()`


------------

# Version `0.0.0.4`

CPD regression update, checks for basic-heading CPD regression outputs and household 
expenditure weights, add OECD Territorial correspondence table and various bug fixes

### Additions

- OECD Territorial correspondence table  `oecd_tl`
- Option to run `estim_cpd()` with price-region pair duplicates in raw data 
using option `weights = 'raw'`
- Validation checks for CPD regression output and household expenditure 
weights 

### Bug fixes

- Fixed names in `ecoicop16_names`
- Fixed bugs in `estim_cpd()` and `valid_PPPratio()`

------------

# Version `0.0.0.5`

Validate household expenditure data and add Laspeyres, Paasche, Fischer and GEKS index calculation

### Additions

- Add function to validate household expenditure data `valid_est()`
- Add a snippet of UK [Regional household final consumption expenditure](https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalhouseholdfinalconsumptionexpenditureinternationalterritoriallevel1countriesandregionsandinternationalterritoriallevel2subregions) data

------------

# Version `0.0.0.6`

Add index calculation functions, description of using `OECDsppps` in Python and SAS, 
better implementation description

### Additions

- Add function `index_laspeyres()`, `index_paasche()`, `index_fisher()` 
and `index_geks()` for estimating subnational PPPs using Laspeyres, Paasche, 
Fischer and Gini-Éltetö-Köves-Szulc (GEKS) price indices 
- Add [description](https://amannj.github.io/OECDsppps/articles/altSoftware.html) data
 on using `OECDsppps` in Python and SAS
- Update [implementation](https://amannj.github.io/OECDsppps/articles/Implementation.html)
