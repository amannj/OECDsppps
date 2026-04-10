## code to prepare `uk_hhe` dataset
library(tidyverse)

loc_src <- "V:/SUBNATIONAL_PRICES/sources/UK/0_PulledfromSources/"

uk_hhe <- read_rds(paste0(loc_src, "HFCE/HFCE_yrs2018-20-23.rds"))

uk_hhe <- uk_hhe |>
  filter(year %in% c(2018, 2020)) |>
  filter(coicop %in% c("04.3.2", "01.1.1")) |>
  mutate(Region = case_when(
    itl_code == "TLI" ~ "London",
    itl_code == "TLJ" ~ "South East",
    itl_code == "TLK" ~ "South West",
    itl_code == "TLH" ~ "East of England",
    itl_code == "TLF" ~ "East Midlands",
    itl_code == "TLG" ~ "West Midlands",
    itl_code == "TLE" ~ "Yorkshire and the Humber",
    itl_code == "TLD" ~ "North West",
    itl_code == "TLC" ~ "North",
    itl_code == "TLL" ~ "Wales",
    itl_code == "TLM" ~ "Scotland",
    itl_code == "TLN" ~ "Northern Ireland",
    .default = NA
  )) |>
  select(
    Year = year,
    Region,
    coicop_4d = coicop,
    expenditure_share = shr
  )

usethis::use_data(uk_hhe, overwrite = TRUE)
