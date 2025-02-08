
#' Grab data behind a preset plot
#'
#' @param key Plot key.
#' @param iso Country.
#'
#' @export
gdi_plot_data <- function(key, iso) {

  ranger <- function(data) c(min(data$t), max(data$t))
  name <- namer(iso)

  output <- list(
    data = NULL,
    print = NULL,
    range = NULL,
    max_t = NULL
  )

  if (key == "stocks") {

    output$data <- bind_rows(

      gdidata::undesa_stocks |>
        left_join(countrynames, by = c("geo" = "iso3")) |>
        filter(.data$from == iso) |>
        summarise(
          n = sum(.data$n),
          .by = c(.data$from, .data$region, .data$t)
        ) |>
        mutate(panel = "emig") |>
        rename(geo = .data$from),

      gdidata::undesa_stocks |>
        left_join(countrynames, by = c("from" = "iso3")) |>
        filter(.data$geo == iso) |>
        summarise(
          n = sum(.data$n),
          .by = c(.data$geo, .data$region, .data$t)
        ) |>
        mutate(
          region = ifelse(is.na(.data$region), "Unknown", .data$region),
          panel = "immig"
        )
    )

    output$print <- output$data |>
      mutate(
        country = countryname(.data$geo),
        panel = ifelse(.data$panel == "emig", "Emigrants", "Immigrants")
      ) |>
      select(.data$country, .data$panel, .data$region, .data$t, .data$n) |>
      tidyr::pivot_wider(names_from = .data$t, values_from = .data$n) |>
      rename(
        Country = .data$country,
        Panel = .data$panel,
        Region = .data$region
      )

    output$range <- ranger(gdidata::undesa_stocks)
  }

  if (key == "nats") {

    topn <- 5

    panel_dest <- "Destinations of emigrants"
    panel_orig <- "Origins of immigrants"

    df <- bind_rows(

      gdidata::undesa_stocks |>
        left_join(countrynames, by = c("geo" = "iso3")) |>
        filter(.data$t == max(.data$t) & .data$from == iso) |>
        summarise(
          n = sum(.data$n),
          .by = c(.data$from, .data$geo, .data$region, .data$t)
        ) |>
        mutate(panel = panel_dest) |>
        rename(nat = .data$geo, iso = .data$from),

      gdidata::undesa_stocks |>
        left_join(countrynames, by = c("from" = "iso3")) |>
        filter(.data$t == max(.data$t) & .data$geo == iso) |>
        summarise(
          n = sum(.data$n),
          .by = c(.data$geo, .data$from, .data$region, .data$t)
        ) |>
        mutate(
          region = ifelse(is.na(.data$region), "Unknown", .data$region),
          panel = panel_orig
        ) |>
        rename(iso = .data$geo, nat = .data$from)
    ) |>
      group_by(.data$panel) |>
      arrange(desc(.data$n), .by_group = TRUE) |>
      ungroup()

    destin <- filter(df, .data$panel == panel_dest) |>
      slice_head(n = topn) |>
      pull(.data$nat)

    origin <- filter(df, .data$panel == panel_orig) |>
      slice_head(n = topn) |>
      pull(.data$nat)

    destin_order <- countryname(destin, to = "name_text") |> rev()
    origin_order <- countryname(origin, to = "name_text") |> rev()

    df_destin1 <- filter(df, .data$panel == panel_dest) |>
      mutate(country = case_when(
        .data$nat %in% destin ~
          countryname(.data$nat, from = "iso3", to = "name_text"),
        .default = "Others"
      )) |>
      mutate(region = ifelse(
        .data$country == "Others",
        "Others",
        .data$region
      )) |>
      summarise(
        n = sum(.data$n),
        .by = c(.data$panel, .data$country, .data$region)
      )

    df_destin <- df_destin1 |>
      arrange(match(.data$country, c("Others", destin_order)))
    df_destin_print <- df_destin1 |>
      arrange(match(.data$country, c(rev(destin_order), "Others")))

    df_origin1 <- filter(df, .data$panel == panel_orig) |>
      mutate(country = case_when(
        .data$nat %in% origin ~
          countryname(.data$nat, from = "iso3", to = "name_text"),
        .default = "Others"
      )) |>
      mutate(region = ifelse(
        .data$country == "Others",
        "Others",
        .data$region
      )) |>
      summarise(
        n = sum(.data$n),
        .by = c(.data$panel, .data$country, .data$region)
      )

    df_origin <- df_origin1 |>
      arrange(match(.data$country, c("Others", origin_order)))
    df_origin_print <- df_origin1 |>
      arrange(match(.data$country, c(rev(origin_order), "Others")))

    output$data <- bind_rows(df_destin, df_origin) |>
      mutate(share = 100 * .data$n / sum(.data$n), .by = .data$panel)
    output$print <- bind_rows(df_destin_print, df_origin_print) |>
      mutate(share = .data$n / sum(.data$n), .by = .data$panel) |>
      select(
        Panel = .data$panel,
        Country = .data$country,
        Region = .data$region,
        Count = .data$n,
        Share = .data$share
      )

    output$range <- rep(max(gdidata::undesa_stocks$t), 2)
  }

  if (key == "nmig") {

    nmig <- filter(gdidata::wdi, .data$var == "nmig") |>
      rename(nmig = .data$v) |>
      select(-.data$var)
    pop <- filter(gdidata::wdi, .data$var == "pop") |>
      rename(pop = .data$v) |>
      select(-.data$var)
    data <- inner_join(nmig, pop, by = c("geo", "t")) |>
      mutate(v = 1000 * .data$nmig / .data$pop)

    output$data <- filter(data, .data$geo == iso)

    if (nrow(output$data) > 0) {
      output$print <- output$data |>
        mutate(country = countryname(.data$geo, to = "name_text")) |>
        select(
          Country = .data$country,
          Year = .data$t,
          `Net migration` = .data$nmig,
          Population = .data$pop,
          `Net migrants per 1000 population` = .data$v
        )
    }

    output$range <- ranger(data)
  }

  if (key == "idp") {

    causes <- c("Environmental impacts", "Conflict and violence")

    output$data <- filter(gdidata::idmc_flows, .data$geo == iso) |>
      mutate(cause = ifelse(
        .data$cause == "conflict",
        causes[2],
        causes[1])
      ) |>
      summarise(
        n = sum(.data$n),
        .by = c(.data$geo, .data$t, .data$cause)
      )

    if (nrow(output$data) > 0) {
      output$print <- output$data |>
        mutate(Country = countryname(.data$geo, to = "name_sort")) |>
        select(.data$Country, Year = .data$t, .data$cause, .data$n) |>
        pivot_wider(names_from = .data$cause, values_from = .data$n) |>
        arrange(.data$Year)
    }

    output$range <- ranger(gdidata::idmc_flows)
  }

  if (key == "mmp") {

    causes <- c(
      "Drowning",
      "Transport hazards",
      "Harsh conditions",
      "Accidental",
      "Sickness",
      "Violence",
      "Mixed or unknown"
    )

    data_full <- gdidata::iom_mmp |>
      mutate(t = lubridate::year(.data$t)) |>
      summarise(n = sum(.data$dead), .by = .data$t)

    output$data <- filter(gdidata::iom_mmp, .data$geo == iso) |>
      mutate(
        t = lubridate::year(t),
        cause = case_when(
          str_detect(.data$cause, "Accidental") ~ "Accidental",
          str_detect(.data$cause, "Harsh")      ~ "Harsh conditions",
          str_detect(.data$cause, "Sickness")   ~ "Sickness",
          str_detect(.data$cause, "transport")  ~ "Transport hazards",
          .default = .data$cause
        )
      ) |>
      summarise(
        n = sum(.data$dead),
        .by = c(.data$geo, .data$t, .data$cause)
      ) |>
      drop_na()

    if (nrow(output$data) > 0) {

      agg <- output$data |>
        summarise(n = sum(.data$n), .by = .data$t) |>
        mutate(geo = iso, cause = "Total")

      output$print <- bind_rows(output$data, agg) |>
        mutate(Country = countryname(.data$geo, to = "name_text")) |>
        arrange(.data$t) |>
        pivot_wider(names_from = .data$t, values_from = .data$n) |>
        rename(Cause = .data$cause) |>
        arrange(match(.data$Cause, c("Total", causes)))
    }

    output$range <- ranger(data_full)
  }

  if (key == "refug") {

    panel_orig <- paste0(
      "Refugees from ",
      name,
      " and where they are hosted"
    )
    panel_host <- paste0(
      "Refugees hosted in ",
      name,
      " and where they come from"
    )
    regions <- c(
      "Africa"   = pal("blues", 2),
      "Americas" = pal("greens"),
      "Asia"     = pal("reds", 2),
      "Europe"   = pal("yellows"),
      "Oceania"  = pal("unblues", 2),
      "Unknown"  = pal("grays", 3)
    )

    output$data <- bind_rows(

      gdidata::unhcr |>
        left_join(countrynames, by = c("geo" = "iso3")) |>
        filter(.data$from == iso & .data$group == "refugee") |>
        summarise(
          n = sum(.data$n),
          .by = c(.data$from, .data$region, .data$t)
        ) |>
        mutate(
          region = ifelse(is.na(.data$region), "Unknown", .data$region),
          panel = "orig"
        ) |>
        rename(geo = .data$from),

      gdidata::unhcr |>
        left_join(countrynames, by = c("from" = "iso3")) |>
        filter(.data$geo == iso & .data$group == "refugee") |>
        summarise(
          n = sum(.data$n),
          .by = c(.data$geo, .data$region, .data$t)
        ) |>
        mutate(
          region = ifelse(is.na(.data$region), "Unknown", .data$region),
          panel = "host"
        )
    )

    if (nrow(output$data) > 0) {

      output$print <- output$data |>
        mutate(Panel = ifelse(.data$panel == "orig", panel_orig, panel_host)) |>
        arrange(.data$t) |>
        select(.data$Panel, Region = .data$region, .data$t, .data$n) |>
        pivot_wider(names_from = .data$t, values_from = .data$n) |>
        arrange(.data$Panel, match(.data$Region, names(regions)))
    }

    output$range <- ranger(gdidata::unhcr)
  }

  return(output)
}

