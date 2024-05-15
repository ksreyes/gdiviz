
library(tidyverse)

pivot_years <- function(data) {
  tidyr::pivot_longer(
    data,
    cols = tidyselect::starts_with("Y"),
    names_to = "t",
    names_prefix = "Y",
    names_transform = as.integer,
    values_to = "v"
  )
}

# Metadata ----------------------------------------------------------------

countrynames <- readxl::read_excel("data-raw/metadata.xlsx", sheet = "countries")
usethis::use_data(countrynames, overwrite = TRUE)

gdidata <- readxl::read_excel("data-raw/metadata.xlsx", sheet = "datasets")
usethis::use_data(gdidata, overwrite = TRUE)

gdiplots <- readxl::read_excel("data-raw/metadata.xlsx", sheet = "plots")
usethis::use_data(gdiplots, overwrite = TRUE)

# Stocks ------------------------------------------------------------------

stocks <- dplyr::bind_rows(
  readr::read_csv("data-raw/raw/Stocks_total.csv") |>
    pivot_years() |>
    dplyr::mutate(sex = "Total"),
  readr::read_csv("data-raw/raw/Stocks_female.csv") |>
    pivot_years() |>
    dplyr::mutate(sex = "Female"),
  readr::read_csv("data-raw/raw/Stocks_male.csv") |>
    pivot_years() |>
    dplyr::mutate(sex = "Male")
) |>
  dplyr::select(from = orig, to = dest, sex, t, v) |>
  dplyr::arrange(from, to, sex, t)

usethis::use_data(stocks, overwrite = TRUE)

# Disasters ---------------------------------------------------------------

wrangle_disaster <- function(category_code, category) {

  files <- list.files(
    "data-raw/raw",
    pattern = stringr::str_glue("^{category_code}_.*$"),
    full.names = TRUE
  )

  df <- tibble::tibble()

  for (file in files) {

    type <- gsub(
      stringr::str_glue("^{category_code}_(.*?)\\.csv$"),
      "\\1",
      basename(file)
    )

    df_i <- readr::read_csv(file, show_col_types = FALSE) |>
      pivot_years() |>
      dplyr::mutate(category = category, type = type) |>
      dplyr::select(iso = CountryCode, category, type, t, v)

    df <- dplyr::bind_rows(df, df_i)
  }

  return(df)
}

disasters <- dplyr::bind_rows(
  wrangle_disaster("Displacements", "People displaced"),
  wrangle_disaster("PeopleAffected", "People affected"),
  wrangle_disaster("PeopleKilled", "People killed"),
  wrangle_disaster("EconomicDamage", "Economic damage"),
) |>
  dplyr::filter(type != "Disasters") |>
  dplyr::mutate(
    type = dplyr::case_when(
      type == "Droughts"                                ~ "Drought",
      type == "Earthquakes"                             ~ "Earthquake",
      type %in% c("Extreme Temperature", "Extreme temperatures")
                                                        ~ "Extreme temperature",
      type == "Floods"                                  ~ "Flood",
      type %in% c("Mass Movement", "Mass movements")    ~ "Mass movement",
      type == "Storms"                                  ~ "Storm",
      type == "Volcanic activities"                     ~ "Volcanic activity",
      type == "Wildfires"                               ~ "Wildfire",
      .default = type
    )
  ) |>
  tidyr::drop_na()

# usethis::use_data(disasters, overwrite = TRUE)

# IDMC --------------------------------------------------------------------

file <- "IDMC_Internal_Displacement_Conflict-Violence_Disasters.xlsx"
idmc <- readxl::read_excel(stringr::str_glue("data-raw/{file}")) |>
  select(
    -ends_with("(Raw)"),
    -ends_with("Stock Displacement")
  ) |>
  pivot_longer(
    cols = ends_with("Displacements"),
    names_to = "category",
    names_pattern = "([A-Za-z]+) Internal Displacements",
    values_to = "v"
  ) |>
  replace_na(list(v = 0)) |>
  select(iso = ISO3, category, t = Year, v) |>
  mutate(
    t = as.integer(t),
    longitude = NA,
    latitude = NA,
    v = as.integer(v)
  ) |>
  select(iso, category, t, v) |>
  arrange(iso, category, t)

usethis::use_data(idmc, overwrite = TRUE)

# WPP ---------------------------------------------------------------------

wpp <- readr::read_csv(
  "data-raw/WPP2022_PopulationBySingleAgeSex_Medium_1950-2021.csv",
  col_types = list(Time = "integer", AgeGrp = "integer")
) |>
  select(
    iso = ISO3_code,
    t = Time,
    age = AgeGrp,
    Male = PopMale,
    Female = PopFemale,
    Total = PopTotal
  ) |>
  pivot_longer(
    cols = c(Male, Female, Total),
    names_to = "sex",
    values_to = "v"
  ) |>
  drop_na() |>
  filter(t %in% c(seq(1950, 2020, 10), 2021)) |>
  select(iso, t, sex, age, v) |>
  arrange(iso, t, sex, age)

usethis::use_data(wpp, overwrite = TRUE)

# Internal ----------------------------------------------------------------

file <- "Displacements_geolocated.csv"
idmc_geo <- readr::read_csv(stringr::str_glue("data-raw/raw/{file}")) |>
  select(
    iso = CountryCode,
    category = type,
    type = category,
    t = year,
    longitude, latitude,
    v = displacements
  ) |>
  mutate(
    t = as.integer(t),
    v = as.integer(v),
    type = dplyr::case_when(
      type == "Extreme Temperature" ~ "Extreme temperature",
      type == "Mass Movement" ~ "Mass movement",
      type == "Mix disasters" ~ "Mixed",
      .default = type
    ),
    category = dplyr::case_when(
      category == "Other" ~ "Conflict",
      .default = category
    )
  ) |>
  arrange(iso, category, type, t)

captions <- readr::read_csv("data-raw/captions.csv")

usethis::use_data(
  disasters,
  idmc_geo,
  captions,
  internal = TRUE,
  overwrite = TRUE
)
