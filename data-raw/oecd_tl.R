oecd_tl <- readxl::read_excel("V:/SUBNATIONAL_PRICES/sandbox/_General/OECD Territorial correspondence - TL2024.xlsx",
  sheet = "Regions TL2024", skip = 2
)

usethis::use_data(oecd_tl, overwrite = TRUE)
