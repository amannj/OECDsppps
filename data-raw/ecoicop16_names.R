ecoicop16_names <- readr::read_delim("V:/SUBNATIONAL_PRICES/sandbox/_General/klass-version-485-codes.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) |>
  select(code, level, name)

usethis::use_data(ecoicop16_names, overwrite = TRUE)
