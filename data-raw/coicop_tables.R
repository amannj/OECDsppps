# Input files from Lithuanian Statistical Office
## https://osp.stat.gov.lt/en_GB/individualaus-vartojimo-islaidu-pagal-paskirti-klasifikatorius-coicop


# ECOICOP -------
ecoicop16_names <- readr::read_rds("V:/SUBNATIONAL_PRICES/sources/_General/ecoicop16_names.rds")

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


# COICO 2018 - COICOP 1999 correspondence table
correspondence_coicop18_coicop99 <- readxl::read_xlsx("V:/SUBNATIONAL_PRICES/sources/_General/COICOP2018_COICOP1999_correspondence_table_final.xlsx",
  sheet = "Correspondence 2018-1999"
) |>
  select(
    coicop18_code = `COICOP 2018 Code`,
    coicop18_description = `COICOP 2018 Title`,
    coicop99_code = `COICOP 1999 Code`,
    coicop99_description = `COICOP 1999 Title`,
    comment = `Note/common content`
  )

usethis::use_data(correspondence_coicop18_coicop99, overwrite = TRUE)


# ECOICOP 2016 - COICO 2018 correspondence table
correspondence_ecoicop16_coicop18 <- readxl::read_xlsx("V:/SUBNATIONAL_PRICES/sources/_General/ECOICOP_COICOP_2018_EN.xlsx",
  sheet = "ECICOP_COICOP 2018 EN"
) |>
  select(
    ecoicop_code = ECOICOP, ecoicop_description = `ECOICOP Description`, ecoicop_comments = Coments,
    coicop18_code = `COICOP 2018`,
    coicop18_description = `COICOP 2018 Description`
  )

usethis::use_data(correspondence_ecoicop16_coicop18, overwrite = TRUE)
