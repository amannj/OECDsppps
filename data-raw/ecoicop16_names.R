ecoicop16_names <- readr::read_rds("V:/SUBNATIONAL_PRICES/sandbox/_General/ecoicop16_names.rds")

ecoicop16_names <- ecoicop16_names |>
  dplyr::mutate(coicop_level = dplyr::case_when(
    level == 1 ~ "Division",
    level == 2 ~ "Group",
    level == 3 ~ "Class",
    level == 4 ~ "Subclass",
    .default = NA
  )) |>
  dplyr::mutate(coicop_level_code = dplyr::case_when(
    level == 1 ~ "COICOP2",
    level == 2 ~ "COICOP3",
    level == 3 ~ "COICOP4",
    level == 4 ~ "COICOP5",
    .default = NA
  )) |>
  select(code, coicop_level, coicop_level_code, name)

usethis::use_data(ecoicop16_names, overwrite = TRUE)
