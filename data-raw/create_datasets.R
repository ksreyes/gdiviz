
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

# usethis::use_data(stocks, overwrite = TRUE)


# Indicators --------------------------------------------------------------

varlist <- tibble::tribble(
  ~code,      ~file,
  "pop",      "Population",
  "nmig",     "NetMigration",
  "remin",    "RemittancesReceived",
  "remout",   "RemittancesPaid",
  "fdiin",    "FDIinflow",
  "fdiout",   "FDIoutflow",
  "birth",    "BirthRate",
  "depd",     "AgeDependency",
  "inc",      "Income",
  "unem",     "Unemployment",
  "inf",      "Inflation",
  "grieve",   "GroupGrievance",
  "polstb",   "PoliticalStabilityNoViolence"
)

add_table <- function(code, file) {
  readr::read_csv(stringr::str_glue("data-raw/raw/{file}.csv")) |>
    pivot_years() |>
    dplyr::mutate(var = code) |>
    dplyr::select(iso = CountryCode, var, t, v) |>
    tidyr::drop_na()
}

indicators <- tibble::tibble()

for (i in 1:nrow(varlist)) {
  indicator_i <- add_table(code = varlist[i, ]$code, file = varlist[i, ]$file)
  indicators <- dplyr::bind_rows(indicators, indicator_i)
}

indicators <- dplyr::arrange(indicators, iso, var, t)

# usethis::use_data(indicators, overwrite = TRUE)


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

# Displacements by country and by coordinates

idmc1 <- "IDMC_Internal_Displacement_Conflict-Violence_Disasters.xlsx"
# idmc2 <- "IDMC_GIDD_Internal_Displacement_Disaggregated.xlsx"
idmc2 <- "Displacements_geolocated.csv"

idmc_nat <- readxl::read_excel(stringr::str_glue("data-raw/raw/{idmc1}")) |>
  dplyr::select(
    -tidyselect::ends_with("(Raw)"),
    -tidyselect::ends_with("Stock Displacement")
  ) |>
  tidyr::pivot_longer(
    cols = tidyselect::ends_with("Displacements"),
    names_to = "category",
    names_pattern = "([A-Za-z]+) Internal Displacements",
    values_to = "v"
  ) |>
  tidyr::replace_na(list(v = 0)) |>
  dplyr::select(iso = ISO3, category, t = Year, v) |>
  tidyr::complete(
    iso,
    t = tidyr::full_seq(t, 1),
    category,
    fill = list(v = 0)
  ) |>
  dplyr::mutate(
    dataset = "national",
    t = as.integer(t),
    longitude = NA,
    latitude = NA,
    v = as.integer(v)
  )

idmc_geo <- readr::read_csv(stringr::str_glue("data-raw/raw/{idmc2}")) |>
  dplyr::select(
    iso = CountryCode,
    category = type,
    type = category,
    t = year,
    longitude, latitude,
    v = displacements
  ) |>
  dplyr::mutate(
    dataset = "geo",
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
  )

idmc <- dplyr::bind_rows(idmc_nat, idmc_geo) |>
  dplyr::select(dataset, iso, category, type, t, longitude, latitude, v) |>
  dplyr::arrange(dataset, iso, category, type, t)

# usethis::use_data(idmc, overwrite = TRUE)


# WPP ---------------------------------------------------------------------

# Population shares by age and sex from UN World Population Prospects

wpp <- readr::read_csv(
  "data-raw/raw/WPP2022_PopulationBySingleAgeSex_Medium_Percentage_1950-2021.csv",
  col_types = list(Time = "integer", AgeGrp = "integer")
) |>
  dplyr::select(
    iso = ISO3_code,
    t = Time,
    age = AgeGrp,
    Male = PopMale,
    Female = PopFemale,
    Total = PopTotal
  ) |>
  tidyr::pivot_longer(
    cols = c(Male, Female, Total),
    names_to = "sex",
    values_to = "v"
  ) |>
  tidyr::drop_na() |>
  dplyr::filter(t == 2021) |>
  dplyr::select(iso, t, sex, age, v) |>
  dplyr::arrange(iso, t, sex, age)

# usethis::use_data(wpp, overwrite = TRUE)



# Captions ----------------------------------------------------------------

captions <- readr::read_csv("data-raw/captions.csv")

# usethis::use_data(captions, overwrite = TRUE)

usethis::use_data(
  stocks,
  indicators,
  disasters,
  idmc,
  wpp,
  captions,
  internal = TRUE,
  overwrite = TRUE
)
