## code to prepare `uk_cpi` dataset
library(tidyverse)

loc_src <- "V:/SUBNATIONAL_PRICES/sources/UK/0_PulledfromSources/"

uk_cpi <- read_rds(paste0(loc_src, "CPI microdata/CPI_monthly_yrs2018-20-23.rds")) |>
  # Picking random product
  filter(item_id %in% c(210111, 410518)) |>
  # Need to clean up the UK data to match the format
  mutate(
    ref_quant = as.numeric(substr(item_desc, 27, 29)),
    ref_quant = ifelse(is.na(ref_quant), 1, ref_quant),
    ref_quant_u = as.numeric(substr(item_desc, 30, 30)),
    ref_quant_u = ifelse(is.na(ref_quant_u), 1, ref_quant_u),
    obs_quant = ref_quant,
    obs_quant_u = ref_quant_u,
  )

uk_cpi <- uk_cpi |>
  # Check if units of reference and observed quantities are identical
  # NAs for reported units only
  mutate(`Unit of reference und observed quantity are identical` = case_when(ref_quant_u == obs_quant_u ~ T,
    is.na(ref_quant_u) & is.na(obs_quant_u) ~ T,
    .default = F
  )) |>
  # Calculate reference quantity price
  mutate(ref_quant = ifelse(is.na(ref_quant), 1, ref_quant)) |>
  mutate(obs_quant = ifelse(is.na(obs_quant), 1, obs_quant)) |>
  mutate(ref_quant_p = ifelse(`Unit of reference und observed quantity are identical`,
    price / obs_quant * ref_quant,
    NA
  ))

# Update variable names
uk_cpi <- uk_cpi |>
  select(
    Year = year,
    `Date of quote` = quote_date,
    coicop_5d,
    `Product code` = item_id,
    `Product description` = item_desc,
    `Reference quantity` = ref_quant,
    `Unit of reference quantity` = ref_quant_u,
    `Region` = region,
    `Shop identifier` = shop_code,
    `Type of shop` = shop_type,
    `Quantity observed` = obs_quant,
    `Unit of observed quantity` = obs_quant_u,
    `Price observed` = price,
    `Reference quantity price` = ref_quant_p
  )

usethis::use_data(uk_cpi, overwrite = TRUE)
