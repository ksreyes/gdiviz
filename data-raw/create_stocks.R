library(tidyverse)

file1 <- "undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx"
file2 <- "aggregates_correspondence_table_2020_1.xlsx"

undesa <- readxl::read_excel(
  str_glue("data-raw/{file1}"),
  sheet = "Table 1",
  skip = 10
) |>
  select(
    code_dest = `Location code of destination`,
    code_orig = `Location code of origin`,
    `1990...8`:`2020...28`
  )

notes <- readxl::read_excel(
  str_glue("data-raw/{file2}"),
  skip = 10
) |>
  select(code = `Location code`, iso = `ISO3 Alpha-code`, type = `Name...6`) |>
  distinct()

# Remove entries that aren't countries

df <- undesa |>
  left_join(notes, by = c("code_dest" = "code")) |>
  rename(to = iso, type_dest = type) |>
  left_join(notes, by = c("code_orig" = "code")) |>
  rename(from = iso, type_orig = type) |>
  mutate(
    from = ifelse(code_orig == 2003, "XXX", from),
    type_orig = ifelse(code_orig == 2003, "Country/Area", type_orig),
  ) |>
  filter(type_dest == "Country/Area" & type_orig == "Country/Area") |>
  select(to, from, `1990...8`:`2020...28`)

# Pivot longer by sex

comb <- expand.grid(seq(1990, 2020, 5), c("Total", "Male", "Female"))
cols <- paste0(comb$Var1, "_", comb$Var2)

colnames(df)[-(1:2)] <- cols

stocks <- df |>
  pivot_longer(
    cols = -c(from, to),
    names_to = c("t", "sex"),
    names_sep = "_",
    values_to = "v"
  ) |>
  filter(sex != "Total" & v > 0)

usethis::use_data(stocks, overwrite = TRUE)

# Check if totals match raw UN DESA file

check_tot <- stocks |>
  summarise(v = sum(v), .by = c(to, t))

# Check for any odd countries

check_to <- stocks |> select(to) |> distinct() |> mutate(undesa = "check") |>
  full_join(countrynames, by = c("to" = "iso3c"))

check_from <- stocks |> select(from) |> distinct() |> mutate(undesa = "check") |>
  full_join(countrynames, by = c("from" = "iso3c"))
