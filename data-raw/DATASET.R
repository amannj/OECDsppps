## code to prepare `DATASET` dataset goes here
library(tidyverse)

loc_src <- "V:/SUBNATIONAL_PRICES/sandbox/UK/0_PulledfromSources"

df_uk <- read_rds(paste0(loc_src, "/CPI microdata/CPI_monthly_yrs2018-20-23.rds")) |>
  # Picking random product
  filter(item_id == 210111) |>
  # Need to clean up the UK data to match the format
  mutate(
    ref_quant = as.numeric(substr(item_desc, 27, 29)),
    ref_quant_u = substr(item_desc, 30, 30),
    obs_quant = ref_quant,
    obs_quant_u = ref_quant_u,
  )


df_uk <- df_uk |> # Check if units of reference and observed quantities are identical
  mutate(`Unit of reference und observed quantity are identical` = ref_quant_u == obs_quant_u) |>
  # Calculate reference quantity price
  mutate(ref_quant_p = ifelse(`Unit of reference und observed quantity are identical`,
    price / obs_quant * ref_quant,
    NA
  ))

# Price Observation Table
uk_cpi <- df_uk |>
  select(
    Year = year,
    COICOP5 = coicop5_id,
    `Product code` = item_id,
    `Product description` = item_desc,
    `Reference quantity` = ref_quant,
    `Unit of reference quantity` = ref_quant_u,
    `Date of quote` = quote_date,
    `Region` = region,
    `Shop identifier` = shop_code,
    `Type of shop` = shop_type,
    `Quantity observed` = obs_quant,
    `Unit of observed quantity` = obs_quant_u,
    `Price observed` = price,
    `Reference quantity price` = ref_quant_p
  )

usethis::use_data(uk_cpi, overwrite = TRUE)
