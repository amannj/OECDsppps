# Codes to integrate  concordance tables into OECDsppps
library(tidyverse)

# Concordance tables ECOICOP1 to basic headings
ecoicop2bh <- readxl::read_excel("V:/SUBNATIONAL_PRICES/sources/conversions/COICOP_to_BH.xlsx",
                           sheet = "ECOICOP1_to_EUR")

concordance_ecoicop2bh <- ecoicop2bh |>
  select(ecoicop1_code = COICOP1,
         ecoicop1_name = NAME_EN,
         bh_code = EUR_Code,
         bh_name = EUR_name,
         type = type)

usethis::use_data(concordance_ecoicop2bh, overwrite = TRUE)
