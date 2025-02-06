
#' Grab data behind a preset plot
#'
#' @param plot_key Plot key.
#' @param iso Country.
#' @param years If `TRUE`, returns a string showing the year range of the data
#'   (default`=FALSE`).
#' @param year_max If `TRUE`, returns the maximum year of the data
#'   (default`=FALSE`).
#' @param print If `TRUE`, returns the dataset formatted for printing.
#'
#' @export
grab_plot_data <- function(plot_key,
                           iso,
                           years = FALSE,
                           year_max = FALSE,
                           print = FALSE) {

  ranger <- function(data) c(min(data$t), max(data$t))
  name <- namer(iso)

  df <- tibble::tibble()
  df_print <- tibble::tibble()
  range <- ""
  max_t <- ""

  if (plot_key == "stocks") {

    range <- ranger(gdidata::undesa_stocks)
    max_t <- max(gdidata::undesa_stocks$t)

    df <- dplyr::bind_rows(

      gdidata::undesa_stocks |>
        dplyr::left_join(countrynames, by = c("geo" = "iso3")) |>
        dplyr::filter(.data$from == iso) |>
        dplyr::summarise(
          n = sum(.data$n),
          .by = c(.data$from, .data$region, .data$t)
        ) |>
        dplyr::mutate(panel = "emig") |>
        dplyr::rename(geo = .data$from),

      gdidata::undesa_stocks |>
        dplyr::left_join(countrynames, by = c("from" = "iso3")) |>
        dplyr::filter(.data$geo == iso) |>
        dplyr::summarise(
          n = sum(.data$n),
          .by = c(.data$geo, .data$region, .data$t)
        ) |>
        dplyr::mutate(
          region = ifelse(is.na(.data$region), "Unknown", .data$region),
          panel = "immig"
        )
    )

    df_print <- df |>
      dplyr::mutate(
        country = countryname(.data$geo),
        panel = ifelse(.data$panel == "emig", "Emigrants", "Immigrants")
      ) |>
      dplyr::select(.data$country, .data$panel, .data$region, .data$t, .data$n) |>
      tidyr::pivot_wider(names_from = .data$t, values_from = .data$n) |>
      dplyr::rename(
        Country = .data$country,
        Panel = .data$panel,
        Region = .data$region
      )

  }

  if (plot_key == "nats") {

    range <- ranger(gdidata::undesa_stocks)
    max_t <- max(gdidata::undesa_stocks$t)

    panel_dest <- "Destinations of emigrants"
    panel_orig <- "Origins of immigrants"

    df <- dplyr::bind_rows(

      gdidata::undesa_stocks |>
        dplyr::left_join(countrynames, by = c("geo" = "iso3")) |>
        dplyr::filter(.data$t == max_t & .data$from == iso) |>
        dplyr::summarise(n = sum(n), .by = c(from, geo, region, t)) |>
        dplyr::mutate(panel = panel_dest) |>
        dplyr::rename(nat = .data$geo, iso = .data$from),

      gdidata::undesa_stocks |>
        dplyr::left_join(countrynames, by = c("from" = "iso3")) |>
        dplyr::filter(.data$t == max_t & .data$geo == iso) |>
        dplyr::summarise(
          n = sum(.data$n),
          .by = c(.data$geo, .data$from, .data$region, .data$t)
        ) |>
        dplyr::mutate(
          region = ifelse(is.na(.data$region), "Unknown", .data$region),
          panel = panel_orig
        ) |>
        dplyr::rename(iso = .data$geo, nat = .data$from)
    ) |>
      dplyr::group_by(.data$panel) |>
      dplyr::arrange(dplyr::desc(.data$n), .by_group = TRUE) |>
      dplyr::ungroup()

    destin <- dplyr::filter(df, .data$panel == panel_dest) |>
      dplyr::slice_head(n = 5) |>
      dplyr::pull(.data$nat)

    origin <- dplyr::filter(df, .data$panel == panel_orig) |>
      dplyr::slice_head(n = 5) |>
      dplyr::pull(.data$nat)

    destin_order <- countryname(destin, to = "name_text") |> rev()
    origin_order <- countryname(origin, to = "name_text") |> rev()

    df_destin1 <- dplyr::filter(df, .data$panel == panel_dest) |>
      dplyr::mutate(country = dplyr::case_when(
        .data$nat %in% destin ~
          countryname(.data$nat, from = "iso3", to = "name_text"),
        .default = "Others"
      )) |>
      dplyr::mutate(region = ifelse(
        .data$country == "Others",
        "Others",
        .data$region
      )) |>
      dplyr::summarise(
        n = sum(.data$n),
        .by = c(.data$panel, .data$country, .data$region)
      )

    df_destin <- df_destin1 |>
      dplyr::arrange(match(.data$country, c("Others", destin_order)))
    df_destin_print <- df_destin1 |>
      dplyr::arrange(match(.data$country, c(rev(destin_order), "Others")))

    df_origin1 <- dplyr::filter(df, .data$panel == panel_orig) |>
      dplyr::mutate(country = dplyr::case_when(
        .data$nat %in% origin ~
          countryname(.data$nat, from = "iso3", to = "name_text"),
        .default = "Others"
      )) |>
      dplyr::mutate(region = ifelse(
        .data$country == "Others",
        "Others",
        .data$region
      )) |>
      dplyr::summarise(
        n = sum(.data$n),
        .by = c(.data$panel, .data$country, .data$region)
      )

    df_origin <- df_origin1 |>
      dplyr::arrange(match(.data$country, c("Others", origin_order)))
    df_origin_print <- df_origin1 |>
      dplyr::arrange(match(.data$country, c(rev(origin_order), "Others")))

    df <- dplyr::bind_rows(df_destin, df_origin) |>
      dplyr::mutate(share = 100 * .data$n / sum(.data$n), .by = .data$panel)
    df_print <- dplyr::bind_rows(df_destin_print, df_origin_print) |>
      dplyr::mutate(share = 100 * .data$n / sum(.data$n), .by = .data$panel) |>
      dplyr::rename(
        Panel = .data$panel,
        Country = .data$country,
        Region = .data$region,
        Count = .data$n,
        Share = .data$n
      )
  }

  if (plot_key == "nmig") {

    nmig <- dplyr::filter(gdidata::indicators, .data$var == "nmig") |>
      dplyr::rename(nmig = .data$v) |>
      dplyr::select(-.data$var)
    pop <- dplyr::filter(gdidata::indicators, .data$var == "pop") |>
      dplyr::rename(pop = .data$v) |>
      dplyr::select(-.data$var)
    data <- dplyr::inner_join(nmig, pop, by = c("geo", "t")) |>
      dplyr::mutate(v = 1000 * .data$nmig / .data$pop)

    range <- ranger(data)
    max_t <- max(data$t)

    df <- dplyr::filter(data, .data$geo == iso)

    if (nrow(df) > 0) {
      df_print <- df |>
        dplyr::mutate(country = countryname(.data$geo, to = "name_text")) |>
        dplyr::rename(
          Country = .data$country,
          Year = .data$t,
          `Migrants per 1000 population` = .data$v
        )
    } else {
      df_print <- tibble::tibble()
    }
  }

  if (years) return(range)
  if (year_max) return(max_t)
  if (print) return(df_print)

  else return(df)
}
