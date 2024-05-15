#' Check available data for country
#'
#' Display the variables and years for which data is available for a given
#' country.
#'
#' @param country 3-letter ISO code of country of interest.
#'
#' @return Tibble.
#'
#' @export
check_data <- function(country) {

  name <- countryname(country)

  results <- gdidata |>
    dplyr::mutate(iso = country, name = name) |>
    dplyr::select(
      .data$iso, .data$name, .data$table, .data$var, .data$description
    )

  stocks_iso <- stocks |>
    dplyr::filter(.data$from == country)

  if (nrow(stocks_iso) > 0) {

    stocks_sum <- stocks_iso |>
      dplyr::summarise(
        earliest_year = min(.data$t),
        latest_year = max(.data$t),
        obs = dplyr::n(),
      ) |>
      dplyr::mutate(table = "stocks") |>
      dplyr::select(
        .data$table, .data$earliest_year, .data$latest_year, .data$obs
      )

  } else {
    stocks_sum <- tibble()
  }

  indicators_sum <- indicators |>
    dplyr::filter(.data$iso == country) |>
    dplyr::summarise(
      earliest_year = min(.data$t),
      latest_year = max(.data$t),
      obs = dplyr::n(),
      .by = .data$var
    ) |>
    dplyr::mutate(table = "indicators") |>
    dplyr::select(
      .data$table,
      .data$var,
      .data$earliest_year,
      .data$latest_year,
      .data$obs
    )

  idmc_iso <- idmc |>
    dplyr::filter(.data$iso == country)

  if (nrow(idmc_iso) > 0) {

    idmc_sum <- idmc_iso |>
      dplyr::summarise(
        earliest_year = min(.data$t),
        latest_year = max(.data$t),
        obs = dplyr::n(),
      ) |>
      dplyr::mutate(table = "idmc") |>
      dplyr::select(
        .data$table, .data$earliest_year, .data$latest_year, .data$obs
      )

  } else {
    idmc_sum <- tibble()
  }

  summary <- bind_rows(stocks_sum, indicators_sum, idmc_sum)

  results |>
    dplyr::left_join(summary, by = c("table", "var"))
}
