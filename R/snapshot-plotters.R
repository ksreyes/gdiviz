
namer <- function(iso) {
  name_iso <- dplyr::filter(countrynames, .data$iso3 == iso)
  if (name_iso$with_the == 1) name <- paste0("the ", name_iso$name_text)
  else name <- name_iso$name_text
  return(name)
}

breakers <- tibble::tribble(
  ~from, ~to,
  "Plurinational State of Bolivia", "Plurinational State\nof Bolivia",
  "China, Taiwan Province of China", "Taiwan Province of China",
  "Democratic People's Republic of Korea", "Democratic People's Republic\nof Korea",
  "Democratic Republic of the Congo", "Democratic Republic\nof the Congo",
  "Lao People's Democratic Republic", "Lao People's Democratic\nRepublic",
  "Federated States of Micronesia", "Federated States\nof Micronesia",
  "Occupied Palestinian Territory", "Occupied Palestinian\nTerritory",
  "Bolivarian Republic of Venezuela", "Bolivarian Republic\nof Venezuela"
)

plot_empty <- function(title, source, time, basesize, msg = "No data") {

  time_minor <- time[time %% 5 == 0]

  plot <- ggplot2::ggplot(
    data.frame(t = time),
    ggplot2::aes(x = .data$t)
  ) +
    ggplot2::labs(title = title, caption = source) +

    ggplot2::scale_x_continuous(
      minor_breaks = seq(min(time_minor), max(time_minor), 5),
      expand = ggplot2::expansion(mult = .025),
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 10),
      breaks = 0,
      expand = ggplot2::expansion(mult = 0)
    ) +

    apply_theme(type = "line", basesize) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(
        color = pal("blues"),
        linewidth = k(.05)
      ),
      plot.margin = ggplot2::margin(k(2), k(2), k(2), k(2))
    )

  plot <- cowplot::ggdraw(plot) +
    cowplot::draw_label(
      msg,
      y = .55,
      fontfamily = "Gill Sans Nova",
      color = pal("blues", 3),
      size = k(3)
    )

  return(plot)
}


# Migrant stocks ----------------------------------------------------------

plot_migstocks <- function(hero,
                           basesize,
                           title = paste0(
                               "Migrant populations, ",
                               grab_plot_data("stocks", hero, years = TRUE) |>
                                 paste(collapse = "–")
                             )
                           ) {

  k <- function(factor = 1) factor * basesize / ggplot2::.pt
  source <- "Source: UN DESA."
  name <- namer(hero)

  regions <- dplyr::filter(countrynames, !is.na(.data$region)) |>
    dplyr::distinct(.data$region) |>
    dplyr::arrange(.data$region) |>
    dplyr::pull(.data$region)
  regions <- c(regions, "Unknown")

  timespan <- unique(gdidata::undesa_stocks$t)

  panel_emig_display <- "Emigrants from {name}\nand where they reside" |>
    stringr::str_glue()
  panel_immig_display <- "Immigrants in {name}\nand where they come from" |>
    stringr::str_glue()

  df <- grab_plot_data("stocks", hero) |>
    dplyr::mutate(panel = ifelse(
      .data$panel == "emig",
      panel_emig_display,
      panel_immig_display
    ))

  plot_elements <- list(
    ggplot2::geom_area(stat = "identity"),
    ggplot2::facet_wrap(~.data$panel),
    ggplot2::labs(title = title, caption = source),
    ggplot2::scale_x_continuous(
      breaks = seq(1990, 2020, 10),
      expand = ggplot2::expansion(mult = .05),
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ),
    ggplot2::scale_fill_manual(values = c(
      "Africa" = pal("blues", 2),
      "Americas" = pal("greens"),
      "Asia" = pal("reds", 2),
      "Europe" = pal("yellows"),
      "Oceania" = pal("unblues", 2),
      "Unknown" = pal("grays", 3)
    )),
    apply_theme("bar-vertical", basesize, facets = TRUE),
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        size = basesize,
        margin = ggplot2::margin(r = k(2))
      ),
      axis.ticks.x = ggplot2::element_line(
        color = pal("blues"),
        linewidth = k(.05)),
      legend.position = "right",
      panel.spacing.x = grid::unit(k(.5), "lines"),
      plot.margin = ggplot2::margin(k(2), 0, k(2), k(2)),
      strip.text = ggplot2::element_text(
        size = basesize,
        margin = ggplot2::margin(t = 0, b = k())
      )
    )
  )

  if (nrow(df) > 0) {

    check_max <- df |>
      summarise(n = sum(n, na.rm = TRUE), .by = c(panel, t))
    max_n <- max(check_max$n)

    set_breaks <- ggplot2::waiver()
    set_labels <- function(x) x / 10^6
    set_ytitle <- "Millions of individuals"

    if (max_n < 1200) {
      set_ytitle <- "Individuals"
      set_labels <- ggplot2::waiver()
    }
    if (max_n >= 1200 & max_n < 1.20 * 10^6) {
      set_ytitle <- "Thousands of individuals"
      set_labels <- function(x) x / 1000
    }
    if (max_n >= 1.20 * 10^6 & max_n < 1.40 * 10^6) {
      set_breaks <- seq(0, 1.25 * 10^6, .25 * 10^6)
      set_labels <- c("0", "0.25", "0.50", "0.75", "1", "1.25")
    }
    if (max_n >= 1.40 * 10^6 & max_n < 1.80 * 10^6) {
      set_breaks <- seq(0, 1.50 * 10^6, .50 * 10^6)
      set_labels <- c("0", "0.5", "1", "1.5")
    }

    df <- df |>
      tidyr::complete(
        region = regions,
        t = timespan,
        panel,
        fill = list(n = 0)
      )

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$t, y = n, group = .data$region, fill = .data$region)
    ) +
      plot_elements +
      ggplot2::scale_y_continuous(
        name = set_ytitle,
        breaks = set_breaks,
        labels = set_labels,
        expand = ggplot2::expansion(mult = c(0, .025))
      )

  } else {

    df <- df |>
      tidyr::complete(
        region = regions,
        t = timespan,
        panel = c(panel_emig, panel_immig),
        fill = list(n = 0)
      )

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$t, y = .data$n, group = .data$region, fill = .data$region)) +
      plot_elements +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())

    plot <- cowplot::ggdraw(plot) +
      cowplot::draw_label(
        "No data", x = .2240, y = .5,
        fontfamily = "Gill Sans Nova",
        color = pal("blues", 3),
        size = k(3)
      ) +
      cowplot::draw_label(
        "No data", x = .6575, y = .5,
        fontfamily = "Gill Sans Nova",
        color = pal("blues", 3),
        size = k(3)
      )
  }

  return(plot)
}


# Nationalities -----------------------------------------------------------

plot_nats <- function(hero,
                      basesize,
                      title = paste0(
                          "Destinations and origins of migrants, ",
                          grab_plot_data("nats", hero, year_max = TRUE)
                        )
                      ) {

  k <- function(factor = 1) factor * basesize / .pt
  p_source <- "Source: UN DESA."
  df <- grab_plot_data("nats", hero)

  if (nrow(df) > 0) {

    df <- df |>
      dplyr::mutate(country = dplyr::case_when(
        .data$country == breakers$from[1] ~ breakers$to[1],
        .data$country == breakers$from[2] ~ breakers$to[2],
        .data$country == breakers$from[3] ~ breakers$to[3],
        .data$country == breakers$from[4] ~ breakers$to[4],
        .data$country == breakers$from[5] ~ breakers$to[5],
        .data$country == breakers$from[6] ~ breakers$to[6],
        .data$country == breakers$from[7] ~ breakers$to[7],
        .data$country == breakers$from[8] ~ breakers$to[8],
        .default = .data$country
      ))

    df_destin <- dplyr::filter(df, str_detect(.data$panel, "Destinations"))
    df_origin <- dplyr::filter(df, str_detect(.data$panel, "Origins"))

    df_destin$country <- factor(df_destin$country, levels = df_destin$country)
    df_origin$country <- factor(df_origin$country, levels = df_origin$country)

    scales <- list(
      ggplot2::scale_x_continuous(
        labels = function(x) prettylabel(x, pct = TRUE),
        expand = ggplot2::expansion(mult = c(.02, .15))
      ),
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = .15)),
      ggplot2::scale_fill_manual(values = c(
        "Africa"   = pal("blues", 2),
        "Americas" = pal("greens"),
        "Asia"     = pal("reds", 2),
        "Europe"   = pal("yellows"),
        "Oceania"  = pal("unblues", 2),
        "Others"   = pal("grays", 4)
      )),
      ggplot2::scale_color_manual(values = c(
        "Africa"   = pal("blues"),
        "Americas" = pal("greens"),
        "Asia"     = pal("reds"),
        "Europe"   = pal("oranges"),
        "Oceania"  = pal("unblues"),
        "Others"   = pal("grays", 2)
      ))
    )

    p_destin <- ggplot2::ggplot(
      df_destin,
      ggplot2::aes(x = share, y = country, fill = region)
    ) +
      ggplot2::geom_bar(stat = "identity", width = .7, show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = prettylabel(share, pct = TRUE),
          color = region
        ),
        size = k(),
        family = "Gill Sans Nova",
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = max(df_destin$share) / 30,
        show.legend = FALSE
      ) +
      ggplot2::ggtitle("Destinations of emigrants") +

      scales +

      # Aesthetics
      apply_theme(type = "bar-horizontal", basesize) +
      theme(
        axis.text.y = element_text(
          size = basesize - 1,
          lineheight = k(.35)
        ),
        plot.title = element_text(
          size = basesize,
          face = "plain",
          margin = margin(t = 0, b = k(2))
        ),
        plot.margin = margin(0, k(2), 0, 0)
      )

    p_origin <- ggplot(df_origin, aes(x = share, y = country, fill = region)) +
      geom_bar(stat = "identity", width = .7, show.legend = FALSE) +
      geom_text(
        mapping = aes(label = prettylabel(share, pct = TRUE), color = region),
        size = k(),
        family = "Gill Sans Nova",
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = max(df_origin$share) / 30,
        show.legend = FALSE
      ) +
      ggtitle("Origins of immigrants") +

      scales +

      # Aesthetics
      apply_theme(type = "bar-horizontal", basesize) +
      theme(
        axis.text.y = element_text(
          size = basesize - 1,
          lineheight = k(.35)
        ),
        plot.title = element_text(
          size = basesize,
          face = "plain",
          margin = margin(t = 0, b = k(2))
        ),
        plot.margin = margin(0, 0, 0, k(2))
      )

    plot <- cowplot::plot_grid(p_destin, p_origin, nrow = 1)

    plot_title <- ggplot() +
      ggtitle(title) +
      apply_theme(type = "map", basesize) +
      theme(plot.margin = margin(0, 0, 0, 0))

    plot_caption <- ggplot() +
      labs(caption = p_source) +
      apply_theme(type = "map", basesize) +
      theme(plot.margin = margin(0, 0, 0, 0))

    plot <- cowplot::plot_grid(
      plot_title, plot, plot_caption,
      nrow = 3,
      rel_heights = c(.05, 1, .05)
    ) +
      theme(plot.margin = margin(k(2), k(2), k(2), k(2)))

  } else {

    p_destin <- ggplot() +
      ggtitle("Emigrant destinations") +
      apply_theme(type = "void", basesize) +
      theme(
        panel.background = element_blank(),
        plot.title = element_text(
          size = basesize,
          face = "plain",
          margin = margin(t = 0, b = k())
        )
      )
    p_destin <- cowplot::ggdraw(p_destin) +
      cowplot::draw_label(
        "No data", y = .4,
        fontfamily = "Gill Sans Nova",
        color = pal("blues", 3),
        size = k(3)
      )

    p_origin <- ggplot() +
      ggtitle("Immigrant origins") +
      apply_theme(type = "void", basesize) +
      theme(
        panel.background = element_blank(),
        plot.title = element_text(
          size = basesize,
          face = "plain",
          margin = margin(t = 0, b = k())
        )
      )
    p_origin <- cowplot::ggdraw(p_origin) +
      cowplot::draw_label(
        "No data", y = .4,
        fontfamily = "Gill Sans Nova",
        color = pal("blues", 3),
        size = k(3)
      )

    plot <- cowplot::plot_grid(p_destin, p_origin, nrow = 1)

    plot_title <- ggplot() +
      ggtitle(title) +
      apply_theme(type = "map", basesize) +
      theme(plot.margin = margin(0, 0, 0, 0))

    plot_caption <- ggplot() +
      labs(caption = p_source) +
      apply_theme(type = "map", basesize) +
      theme(plot.margin = margin(0, 0, 0, 0))

    plot <- plot_grid(
      plot_title, plot, plot_caption,
      nrow = 3,
      rel_heights = c(.05, 1, .05)
    ) +
      theme(plot.margin = margin(k(2), k(2), k(2), k(2)))
  }

  return(plot)
}


# Net migration -----------------------------------------------------------

plot_nmig <- function(hero,
                      basesize,
                      title = paste0(
                          "Net migration rate, ",
                          grab_plot_data("nmig", hero, years = TRUE) |>
                            paste(collapse = "–")
                        )
                      ) {

  k <- function(factor = 1) factor * basesize / ggplot2::.pt
  source <- "Source: World Bank."
  t0 <- grab_plot_data("nmig", hero, years = TRUE)[1]
  t1 <- grab_plot_data("nmig", hero, years = TRUE)[2]
  timespan <- t0:t1
  df <- grab_plot_data("nmig", hero)

  if (nrow(df) > 0) {

    t0 <- min(df$t)
    t1 <- max(df$t)

    plot <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = netmigration)) +
      ggplot2::geom_bar(stat = "identity", width = .7, fill = pal("blues", 2)) +
      ggplot2::geom_hline(
        yintercept = 0,
        color = pal("blues"), linewidth = k(.1)
      ) +
      ggplot2::labs(title = title, caption = source) +

      ggplot2::scale_x_continuous(
        breaks = seq(t0, t1, 10),
        expand = ggplot2::expansion(mult = c(.03, .03)),
        guide = ggplot2::guide_axis(minor.ticks = TRUE)
      ) +
      ggplot2::scale_y_continuous(name = "Migrants per 1000 population") +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 2),
        linetype =  ggplot2::guide_legend(nrow = 2)
      ) +

      apply_theme(type = "bar-vertical", basesize) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = basesize,
          margin = ggplot2::margin(r = k(2))
        ),
        axis.ticks.x = ggplot2::element_line(
          color = pal("blues"),
          linewidth = k(.05)
        ),
        legend.position = "none",
        plot.margin = ggplot2::margin(k(2), k(2), k(2), k(2))
      )

  } else {

    plot <- plot_empty(title, source, timespan, basesize)
  }

  return(plot)
}


# IDPs --------------------------------------------------------------------

plot_idp <- function(hero,
                     basesize,
                     title = paste0(
                         "New internal displacements, ",
                         grab_plot_data("idp", hero, years = TRUE) |>
                           paste(collapse = "–")
                       )
                     ) {

  k <- function(factor = 1) factor * basesize / ggplot2::.pt
  source <- "Source: IDMC."
  t0 <- grab_plot_data("nmig", hero, years = TRUE)[1]
  t1 <- grab_plot_data("nmig", hero, years = TRUE)[2]
  timespan <- t0:t1

  timespan <- idmc_flows |> distinct(t) |> pull(t)
  causes <- c(
    "Environmental impacts",
    "Conflict and violence"
  )

  data <- filter(idmc_flows, geo == hero) |>
    mutate(cause = case_when(
      cause == "conflict" ~ causes[2],
      .default = causes[1]
    )) |>
    summarise(n = sum(n), .by = c(geo, t, cause))

  if (nrow(data) > 0) {

    df_agg <- summarise(data, n = sum(n), .by = t) |> filter(n > 0)

    check_max <- data |> summarise(n = sum(n), .by = t)
    max_n <- max(check_max$n)

    set_breaks <- waiver()
    set_labels <- function(x) x / 10^6
    set_ytitle <- "Millions of displacements"

    if (max_n < 1.20 * 10^6) {
      set_ytitle <- "Thousands of displacements"
      set_labels <- function(x) x / 1000
    }
    if (max_n >= 1.20 * 10^6 & max_n < 1.40 * 10^6) {
      set_breaks <- seq(0, 1.25 * 10^6, .25 * 10^6)
      set_labels <- c("0", "0.25", "0.50", "0.75", "1", "1.25")
    }
    if (max_n >= 1.40 * 10^6 & max_n < 1.80 * 10^6) {
      set_breaks <- seq(0, 1.50 * 10^6, .50 * 10^6)
      set_labels <- c("0", "0.5", "1", "1.5")
    }

    df <- complete(data, t = timespan, cause = causes)

    plot <- ggplot() +
      geom_bar(
        aes(x = factor(t), y = n, fill = fct_rev(cause)), df,
        stat = "identity", position = "stack", width = .7
      ) +
      scale_fill_manual(values = c(pal("blues", 2), pal("reds", 2))) +

      # Annotations
      geom_text(
        aes(x = factor(t), y = n, label = prettylabel(n)),
        df_agg,
        size = k(.9), color = pal("blues"), vjust = 0,
        family = "Gill Sans Nova", fontface = "bold",
        nudge_y = max(df$n, na.rm = TRUE) / 40
      ) +
      labs(title = str_glue("{p_title}, {range2(df)}"), caption = p_source) +

      # Aesthetics
      apply_theme(type = "bar-vertical", basesize) +
      scale_y_continuous(
        name = set_ytitle,
        breaks = set_breaks,
        labels = set_labels
      ) +
      theme(
        axis.title.y = element_text(
          size = size$text,
          margin = margin(r = k(2))
        ),
        axis.text.x = element_text(margin = margin(t = -k(.5))),
        plot.margin = margin(k(4), k(4), k(.25), k(4))
      )

  } else {

    df <- complete(data, t = timespan, cause = causes, fill = list(n = 0))

    plot <- ggplot() +
      geom_bar(
        aes(x = factor(t), y = n, fill = fct_rev(cause)), df,
        stat = "identity", position = "stack", width = .7
      ) +
      scale_fill_manual(values = c(pal("blues", 2), pal("reds", 2))) +
      labs(title = p_title, caption = p_source) +
      apply_theme(type = "bar-vertical", basesize) +
      scale_y_continuous(limits = c(0, 10), breaks = 0) +
      theme(
        axis.text.x = element_text(margin = margin(t = -k(.5))),
        axis.text.y = element_blank(),
        legend.key.size = unit(size$text, "points"),
        legend.text = element_text(
          size = size$text,
          margin = margin(r = 0, l = k(.75))
        ),
        plot.margin = margin(k(4), k(4), k(.75), k(4))
      )

    plot <- ggdraw(plot) +
      draw_label(
        "No data",
        y = .55,
        fontfamily = "Gill Sans Nova",
        color = pal("blues", 3),
        size = k(3)
      )
  }

  return(plot)
}
