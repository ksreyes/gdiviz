#' Check available data for country
#'
#' @description Display the variables and years for which data is available for
#'   a given country.
#'
#' @details Table of results shows variable name, the earliest and latest dates
#'   available, the number of observations in the time series, and the
#'   date of last update. If `verbose = TRUE`, additionally shows the `table`
#'   and `var` names of the variables.
#'
#' @param iso 3-letter ISO code of country of interest.
#' @param verbose Whether to show all details.
#'
#' @return Tibble.
#'
#' @export
check_data <- function(iso, verbose = FALSE) {

  if (nchar(iso) != 3) {
    cli::cli_abort("Enter a 3-letter ISO code.")
  }

  name <- countryname(iso, exact = TRUE, quiet = TRUE)
  if (is.na(name)) {
    cli::cli_abort(
      '"{iso}" is not a valid ISO country code. Run countrynames to see the
      list of available codes.'
    )
  }

  country <- toupper(iso)

  # Check stocks table

  stocks_iso <- dplyr::filter(stocks, .data$from == country)

  if (nrow(stocks_iso) > 0) {

    stocks_sum <- stocks_iso |>
      dplyr::summarise(
        earliest = min(.data$t) |> as.character(),
        latest = max(.data$t) |> as.character(),
        obs = length(unique(.data$t)),
      ) |>
      dplyr::mutate(table = "stocks") |>
      dplyr::select(
        .data$table, .data$earliest, .data$latest, .data$obs
      )

  } else {
    stocks_sum <- tibble::tibble()
  }

  # Check indicators table

  indicators_sum <- dplyr::filter(indicators, .data$iso == country) |>
    dplyr::summarise(
      earliest = min(.data$t) |> as.character(),
      latest = max(.data$t) |> as.character(),
      obs = dplyr::n(),
      .by = .data$var
    ) |>
    dplyr::mutate(table = "indicators") |>
    dplyr::select(
      .data$table,
      .data$var,
      .data$earliest,
      .data$latest,
      .data$obs
    )

  # Check idmc table

  idmc_iso <- dplyr::filter(idmc, .data$iso == country)

  if (nrow(idmc_iso) > 0) {

    idmc_sum <- idmc_iso |>
      dplyr::summarise(
        earliest = min(.data$t) |> as.character(),
        latest = max(.data$t) |> as.character(),
        obs = length(unique(.data$t))
      ) |>
      dplyr::mutate(table = "idmc") |>
      dplyr::select(
        .data$table, .data$earliest, .data$latest, .data$obs
      )

  } else {
    idmc_sum <- tibble::tibble()
  }

  # Check wpp table

  wpp_iso <- dplyr::filter(wpp, .data$iso == country)

  if (nrow(wpp_iso) > 0) {

    wpp_sum <- wpp_iso |>
      dplyr::summarise(
        earliest = min(.data$t) |> as.character(),
        latest = max(.data$t) |> as.character(),
        obs = length(unique(.data$t))
      ) |>
      dplyr::mutate(table = "wpp") |>
      dplyr::select(
        .data$table, .data$earliest, .data$latest, .data$obs
      )

  } else {
    wpp_sum <- tibble::tibble()
  }

  # Check mpp table

  mpp_iso <- dplyr::filter(mpp, .data$location == country)

  if (nrow(mpp_iso) > 0) {

    mpp_sum <- mpp_iso |>
      dplyr::summarise(
        earliest = min(.data$date, na.rm = TRUE) |> as.character(),
        latest = max(.data$date, na.rm = TRUE) |> as.character(),
        obs = dplyr::n()
      ) |>
      dplyr::mutate(table = "mmp") |>
      dplyr::select(
        .data$table, .data$earliest, .data$latest, .data$obs
      )

  } else {
    mpp_sum <- tibble::tibble()
  }

  # Check disrupt table

  disrupt_iso <- dplyr::filter(disrupt, .data$iso == country)

  if (nrow(disrupt_iso) > 0) {

    disrupt_sum <- disrupt_iso |>
      dplyr::summarise(
        earliest = min(.data$t, na.rm = TRUE) |> as.character(),
        latest = max(.data$t, na.rm = TRUE) |> as.character(),
        obs = length(unique(.data$t))
      ) |>
      dplyr::mutate(table = "disrupt") |>
      dplyr::select(
        .data$table, .data$earliest, .data$latest, .data$obs
      )

  } else {
    disrupt_sum <- tibble::tibble()
  }

  # Consolidate

  summary <- dplyr::bind_rows(
    stocks_sum, indicators_sum, idmc_sum, wpp_sum, mpp_sum, disrupt_sum
  )
  result <- dplyr::left_join(gdidata, summary, by = c("table", "var")) |>
    dplyr::select(
      .data$table,
      .data$var,
      .data$description,
      .data$earliest,
      .data$latest,
      .data$obs,
      .data$last_updated
    )

  if (verbose) return(result)
  else {
    result <- result |> dplyr::select(
      variable = .data$description,
      .data$earliest,
      .data$latest,
      .data$obs
    )
    return(result)
  }
}
