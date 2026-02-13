ecoicop16_names <- readr::read_rds("V:/SUBNATIONAL_PRICES/sandbox/_General/ecoicop16_names.rds")

ecoicop16_names <- ecoicop16_names |>
  dplyr::mutate(level = dplyr::case_when(level == 1 ~ "Division  (COICOP2)",
    level == 2 ~ "Group (COICOP3)",
    level == 3 ~ "Class (COICOP4)",
    level == 4 ~ "Subclass  (COICOP5)",
    .default = NA
  ))

usethis::use_data(ecoicop16_names, overwrite = TRUE)
