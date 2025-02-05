
library(tidyverse)

# WDI ---------------------------------------------------------------------

wbcountries <- read_csv("data-raw/Metadata_Country.csv") |>
  select(`Country Code`, Region, TableName) |>
  drop_na() |>
  full_join(countrynames, by = c("Country Code" = "iso3")) |>
  select(`Country Code`, TableName) |>
  drop_na() |>
  pull(`Country Code`) |>
  tolower() |>
  paste(collapse = ";")

wdi_codes <- tribble(
  ~code,                  ~var,
  "SP.POP.TOTL",          "pop",
  "SM.POP.NETM",          "nmig",
  "NY.GDP.PCAP.PP.KD",    "income",
  "SL.UEM.TOTL.ZS",       "unem",
  "FP.CPI.TOTL.ZG",       "inf",
  "BM.TRF.PWKR.CD.DT",    "remout",
  "BX.TRF.PWKR.CD.DT",    "remin",
  "BX.KLT.DINV.CD.WD",    "fdiin",
  "BM.KLT.DINV.CD.WD",    "fdiout",
  "SP.DYN.CBRT.IN",       "birth",
  "SP.POP.DPND",          "depend"
)

build_url <- function(code) {
  paste0(
    "http://api.worldbank.org/v2/country/",
    wbcountries,
    # "nor",
    "/indicator/",
    code,
    "?format=json&date=1990:2050&per_page=9999"
  )
}

library(httr)
url <- build_url("SP.POP.TOTL")
response <- GET("http://api.worldbank.org/v2/country/nor/indicator/SP.POP.TOTL?format=json")
status_code(response)

wdi <- tibble()

for (i in 2:nrow(wdi_codes)) {

  wdi_i <- jsonlite::fromJSON(build_url(wdi_codes$code[i])) |>
    as.data.frame() |>
    mutate(var = wdi_codes$var[i]) |>
    select(iso = countryiso3code, var, t = date, v = value) |>
    drop_na()

  wdi <- bind_rows(wdi, wdi_i)
}

# FSI ---------------------------------------------------------------------

fsi_files <- list.files("data-raw/fragile_states_index")

fsi <- tibble()

for (file in fsi_files) {

  fsi_i <- readxl::read_excel(
    paste0("data-raw/fragile_states_index/", file)
  ) |>
    mutate(
      t = str_extract(file, "(\\d){4}"),
      var = "grieve",
      Country = case_when(
        Country == "Israel and West Bank" ~ "Israel",
        Country == "Micronesia" ~ "Federated States of Micronesia",
        .default = Country
      ),
      iso = countrycode::countrycode(
        Country,
        origin = "country.name",
        destination = "iso3c"
      )) |>
    select(iso, var, t, v = `C3: Group Grievance`)

  fsi <- bind_rows(fsi, fsi_i)
}

# WGI ---------------------------------------------------------------------

cols <- readxl::read_excel(
  "data-raw/wgidataset.xlsx",
  sheet = "Political StabilityNoViolence",
  skip = 13,
  col_names = FALSE,
  n_max = 2
)

colnames <- c(
  as.vector(unlist(cols[2, 1:2])),
  paste0(unlist(cols[1, -(1:2)]), "_", unlist(cols[2, -(1:2)]))
)

wgi <- readxl::read_excel(
  "data-raw/wgidataset.xlsx",
  sheet = "Political StabilityNoViolence",
  skip = 15,
  col_names = colnames,
  na = "#N/A"
) |>
  pivot_longer(
    cols = -c(`Country/Territory`, Code),
    names_to = c("t", "measure"),
    names_sep = "_",
    values_to = "v"
  ) |>
  filter(measure == "Estimate") |>
  mutate(
    iso = case_when(
      Code == "ADO" ~ "AND",
      Code == "KSV" ~ "XKX",
      Code == "WBG" ~ "PSE",
      Code == "ROM" ~ "ROU",
      Code == "TMP" ~ "TLS",
      Code == "ZAR" ~ "COD",
      .default = Code
    ),
    var = "polstb",
  ) |>
  drop_na() |>
  select(iso, var, t, v)

# Consolidate -------------------------------------------------------------

indicators <- bind_rows(wdi, fsi, wgi) |>
  mutate(t = as.integer(t)) |>
  arrange(iso, var, t)

usethis::use_data(indicators, overwrite = TRUE)
