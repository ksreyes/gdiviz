

plot_empty <- function(title, source, time, basesize, font, msg = "No data") {

  k <- function(factor = 1) factor * basesize / .pt
  time_minor <- time[time %% 5 == 0]

  plot <- ggplot(data.frame(t = time), aes(x = .data$t)) +
    labs(title = title, caption = source) +
    scale_x_continuous(
      minor_breaks = seq(min(time_minor), max(time_minor), 5),
      expand = expansion(mult = .025),
      guide = guide_axis(minor.ticks = TRUE)
    ) +
    apply_theme(type = "line", basesize, font) +
    theme(
      axis.text.y = element_blank(),
      panel.background = element_rect(color = NA, fill = pal("unblues", 5)),
      plot.margin = margin(k(2), k(2), k(2), k(2))
    )

  plot <- ggdraw(plot) +
    draw_label(
      msg,
      y = .55, fontfamily = font, color = pal("blues", 3), size = k(3)
    )

  return(plot)
}


# Migrant stocks ----------------------------------------------------------

plot_stocks <- function(hero,
                        basesize,
                        font,
                        title = paste0(
                          "Migrant populations, ",
                          gdi_plot_data("stocks", hero)$range |>
                            paste(collapse = "\u2013")
                        )) {

  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UN DESA."
  name <- namer(hero)
  timespan <- unique(gdidata::undesa_stocks$t)

  emig_disp <- paste0("Emigrants from ", name, "\nand where they reside")
  immig_disp <- paste0("Immigrants in ", name, "\nand where they come from")

  data <- gdi_plot_data("stocks", hero)$data |>
    mutate(panel = ifelse(.data$panel == "emig", emig_disp, immig_disp))

  plot_elements <- list(
    facet_wrap(~.data$panel),
    geom_area(stat = "identity"),
    labs(title = title, caption = source),
    scale_x_continuous(
      breaks = seq(1990, 2020, 10),
      expand = expansion(mult = .05),
      guide = guide_axis(minor.ticks = TRUE)
    ),
    scale_fill_manual(values = regions),
    apply_theme("bar-vertical", basesize, font, facets = TRUE),
    theme(
      axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      legend.position = "right",
      panel.spacing.x = unit(k(.5), "lines"),
      plot.margin = margin(k(2), 0, k(2), k(2)),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k())
      )
    )
  )

  if (nrow(data) > 0) {

    df_agg <- data |>
      summarise(n = sum(.data$n, na.rm = TRUE), .by = c(.data$panel, .data$t))
    axis <- set_axis(df_agg$n, "Persons")

    df <- data |>
      complete(
        region = names(regions),
        t = timespan,
        .data$panel,
        fill = list(n = 0)
      )

    plot <- ggplot(
      df,
      aes(x = .data$t, y = .data$n, group = .data$region, fill = .data$region)
    ) +
      plot_elements +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(0, .025))
      ) +
      theme(axis.title.y = element_text(
        size = basesize,
        margin = margin(r = k(2))
      ))

  } else {

    df <- expand.grid(
      region = names(regions),
      t = timespan,
      panel = c(emig_disp, immig_disp),
      n = 0
    )

    plot <- ggplot(
      df,
      aes(x = .data$t, y = .data$n, group = .data$region, fill = .data$region)
    ) +
      plot_elements +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 5)),
        panel.grid.major.y = element_blank(),
      )

    plot <- ggdraw(plot) +
      draw_label(
        "No data",
        x = .2240,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      ) +
      draw_label(
        "No data",
        x = .6575,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }

  return(plot)
}


# Nationalities -----------------------------------------------------------

plot_nats <- function(hero,
                      basesize,
                      font,
                      title = paste0(
                        "Destinations and origins of migrants, ",
                        gdi_plot_data("nats", hero)$range[2]
                      )) {

  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UN DESA."
  data <- gdi_plot_data("nats", hero)$data

  if (nrow(data) > 0) {

    df <- mutate(data, country = break_lines(.data$country))

    df_destin <- filter(df, str_detect(.data$panel, "Destinations"))
    df_origin <- filter(df, str_detect(.data$panel, "Origins"))
    df_destin$country <- factor(df_destin$country, levels = df_destin$country)
    df_origin$country <- factor(df_origin$country, levels = df_origin$country)

    plot_elements <- list(
      geom_bar(stat = "identity", width = .7, show.legend = FALSE),
      geom_text(
        aes(label = prettylabel(.data$share, pct = TRUE), color = .data$region),
        size = k(),
        family = font,
        fontface = "bold",
        hjust = 0, vjust = .5,
        nudge_x = max(df$share) / 30,
        show.legend = FALSE
      ),
      scale_x_continuous(
        labels = function(x) prettylabel(x, pct = TRUE),
        expand = expansion(mult = c(.02, .15))
      ),
      scale_y_discrete(expand = expansion(mult = .15)),
      scale_fill_manual(values = c(
        "Africa"   = pal("blues", 2),
        "Americas" = pal("greens"),
        "Asia"     = pal("reds", 2),
        "Europe"   = pal("yellows"),
        "Oceania"  = pal("unblues", 2),
        "Others"   = pal("grays", 4),
        "Unknown"  = pal("grays", 3)
      )),
      scale_color_manual(values = c(
        "Africa"   = pal("blues"),
        "Americas" = pal("greens"),
        "Asia"     = pal("reds"),
        "Europe"   = pal("oranges"),
        "Oceania"  = pal("unblues"),
        "Others"   = pal("grays", 2),
        "Unknown"  = pal("grays", 2)
      )),
      apply_theme(type = "bar-horizontal", basesize, font),
      theme(
        axis.text.y = element_text(size = basesize - 1, lineheight = k(.35)),
        plot.title = element_text(
          size = basesize,
          face = "plain",
          margin = margin(t = 0, b = k(2))
        )
      )
    )

    p_destin <- ggplot(
      df_destin,
      aes(x = .data$share, y = .data$country, fill = .data$region)
    ) +
      ggtitle("Destinations of emigrants") +
      plot_elements +
      theme(plot.margin = margin(0, k(2), 0, 0))

    p_origin <- ggplot(
      df_origin,
      aes(x = .data$share, y = .data$country, fill = .data$region)
    ) +
      ggtitle("Origins of immigrants") +
      plot_elements +
      theme(plot.margin = margin(0, 0, 0, k(2)))

    plot <- plot_grid(p_destin, p_origin, nrow = 1)

    plot_title <- ggplot() +
      ggtitle(title) +
      apply_theme(type = "map", basesize) +
      theme(plot.margin = margin(0, 0, 0, 0))

    plot_caption <- ggplot() +
      labs(caption = source) +
      apply_theme(type = "map", basesize) +
      theme(plot.margin = margin(0, 0, 0, 0))

    plot <- plot_grid(
      plot_title, plot, plot_caption,
      nrow = 3,
      rel_heights = c(.1, 1, .1)
    ) +
      theme(plot.margin = margin(k(2), k(2), k(2), k(2)))

  } else {

    panel_empty_bar <- function(title) {

      plot_base <- ggplot() +
        ggtitle(title) +
        apply_theme(type = "void", basesize, font) +
        theme(
          panel.background = element_rect(color = NA, fill = pal("unblues", 5)),
          plot.title = element_text(
            size = basesize,
            face = "plain",
            margin = margin(t = 0, b = k(3))
          )
        )

      plot <- ggdraw(plot_base) +
        draw_label(
          "No data",
          y = .4,
          fontfamily = font,
          color = pal("blues", 3),
          size = k(3)
        )

      return(plot)
    }

    plot <- plot_grid(
      panel_empty_bar("Emigrant destinations"),
      panel_empty_bar("Immigrant origins"),
      nrow = 1
    )

    plot_title <- ggplot() +
      ggtitle(title) +
      apply_theme(type = "map", basesize, font) +
      theme(plot.margin = margin(0, 0, 0, 0))

    plot_caption <- ggplot() +
      labs(caption = source) +
      apply_theme(type = "map", basesize, font) +
      theme(plot.margin = margin(0, 0, 0, 0))

    plot <- plot_grid(
      plot_title, plot, plot_caption,
      nrow = 3,
      rel_heights = c(.075, 1, .05)
    ) +
      theme(plot.margin = margin(k(2), k(2), k(2), k(2)))
  }

  return(plot)
}


# Net migration -----------------------------------------------------------

plot_nmig <- function(hero,
                      basesize,
                      font,
                      title = paste0(
                        "Net migration rate, ",
                        gdi_plot_data("nmig", hero)$range |>
                          paste(collapse = "\u2013")
                      )) {

  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: World Bank."
  t0 <- gdi_plot_data("nmig", hero)$range[1]
  t1 <- gdi_plot_data("nmig", hero)$range[2]
  data <- gdi_plot_data("nmig", hero)$data

  if (nrow(data) > 0) {

    plot <- ggplot(data, aes(x = .data$t, y = .data$v)) +
      geom_bar(stat = "identity", width = .7, fill = pal("blues", 2)) +
      geom_hline(
        yintercept = 0,
        color = pal("blues"),
        linewidth = k(.1)
      ) +
      labs(title = title, caption = source) +

      scale_x_continuous(
        breaks = seq(t0, t1, 10),
        expand = expansion(mult = c(.03, .03)),
        guide = guide_axis(minor.ticks = TRUE)
      ) +
      scale_y_continuous(name = "Migrants per 1000 population") +
      guides(
        color = guide_legend(nrow = 2),
        linetype =  guide_legend(nrow = 2)
      ) +

      apply_theme(type = "bar-vertical", basesize, font) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.ticks.x = element_line(
          color = pal("blues"),
          linewidth = k(.05)
        ),
        legend.position = "none",
        plot.margin = margin(k(2), k(2), k(2), k(2))
      )

  } else plot <- plot_empty(title, source, t0:t1, basesize, font)

  return(plot)
}


# IDPs --------------------------------------------------------------------

plot_idp <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "New internal displacements, ",
                       gdi_plot_data("idp", hero)$range |>
                         paste(collapse = "\u2013")
                     )) {

  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: IDMC."
  t0 <- gdi_plot_data("idp", hero)$range[1]
  t1 <- gdi_plot_data("idp", hero)$range[2]

  causes <- c(
    "Environmental impacts" = pal("blues", 2),
    "Conflict and violence" = pal("reds", 2)
  )

  data <- gdi_plot_data("idp", hero)$data

  plot_elements <- list(
    geom_bar(
      aes(x = factor(.data$t), y = .data$n, fill = fct_rev(.data$cause)),
      stat = "identity",
      position = "stack",
      width = .7
    ),
    labs(title = title, caption = source),
    scale_fill_manual(values = causes),
    apply_theme(type = "bar-vertical", basesize, font),
    theme(plot.margin = margin(k(2), k(2), k(2), k(2)))
  )

  if (nrow(data) > 0) {

    df_agg <- summarise(data, n = sum(.data$n), .by = .data$t) |>
      filter(.data$n > 0)
    axis <- set_axis(df_agg$n, "Displacements")

    df <- complete(data, t = t0:t1, cause = names(causes))

    plot <- ggplot(df) +
      plot_elements +
      geom_text(
        aes(x = factor(.data$t), y = .data$n, label = prettylabel(.data$n)),
        df_agg,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(.9),
        vjust = 0,
        nudge_y = max(df$n, na.rm = TRUE) / 40
      ) +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(.02, .05))
      ) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.text.x = element_text(margin = margin(t = -k(.5))),
      )

  } else {

    df <- expand.grid(t = t0:t1, cause = names(causes), n = 0)

    plot <- ggplot(df) +
      plot_elements +
      theme(
        axis.text.y = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 5)),
        panel.grid.major.y = element_blank(),
      )

    plot <- ggdraw(plot) +
      draw_label(
        "No data",
        y = .55,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }

  return(plot)
}


# Dead or missing migrants ------------------------------------------------

plot_mmp <- function(hero,
                     basesize,
                     font,
                     title = paste0(
                       "Dead or missing international migrants, ",
                       gdi_plot_data("mmp", hero)$range |>
                         paste(collapse = "\u2013")
                     )) {

  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: IOM Missing Migrants Project."
  t0 <- gdi_plot_data("mmp", hero)$range[1]
  t1 <- gdi_plot_data("mmp", hero)$range[2]

  causes <- c(
    "Drowning"          = pal("blues", 2),
    "Transport hazards" = pal("blues", 4),
    "Harsh conditions"  = pal("greens"),
    "Accidental"        = pal("greens", 3),
    "Sickness"          = pal("yellows"),
    "Violence"          = pal("reds", 2),
    "Mixed or unknown"  = pal("grays", 3)
  )

  data <- gdi_plot_data("mmp", hero)$data

  plot_elements <- list(
    geom_bar(
      aes(
        x = factor(.data$t),
        y = .data$n,
        fill = factor(.data$cause, levels = names(causes))
      ),
      stat = "identity",
      position = "stack",
      width = .7
    ),
    labs(title = title, caption = source),
    scale_fill_manual(name = "Cause", values = causes),
    apply_theme(type = "bar-vertical", basesize, font),
    theme(
      legend.key.size = unit(basesize, "points"),
      legend.text = element_text(
        size = basesize - 1,
        margin = margin(r = 0, l = k(.75))
      ),
      legend.box.margin = margin(t = k(-3), b = 0),
      plot.margin = margin(k(2), k(2), k(2), k(2))
    )
  )

  if (nrow(data) > 0) {

    df_agg <- summarise(data, n = sum(.data$n), .by = .data$t) |>
      filter(.data$n > 0)
    df <- complete(data, t = t0:t1, cause = names(causes))

    plot <- ggplot(df) +
      plot_elements +
      geom_text(
        aes(x = factor(.data$t), y = .data$n, label = prettylabel(.data$n)),
        df_agg,
        color = pal("blues"),
        family = font,
        fontface = "bold",
        size = k(),
        vjust = 0,
        nudge_y = max(df$n, na.rm = TRUE) / 40
      ) +
      scale_y_continuous(
        name = "Persons",
        labels = function(x) prettylabel(x, spell = TRUE)
      ) +
      theme(
        axis.title.y = element_text(
          size = basesize,
          margin = margin(r = k(2))
        ),
        axis.text.x = element_text(margin = margin(t = -k(.5))),
      )

  } else {

    df <- expand.grid(t = t0:t1, cause = names(causes), n = 0)

    plot <- ggplot(df) +
      plot_elements +
      theme(
        axis.text.y = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 5)),
        panel.grid.major.y = element_blank(),
      )

    plot <- ggdraw(plot) +
      draw_label(
        "None recorded",
        y = .6,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }

  return(plot)
}


# Refugees ----------------------------------------------------------------

plot_refug <- function(hero,
                       basesize,
                       font,
                       title = paste0(
                         "Refugee populations, ",
                         gdi_plot_data("refug", hero)$range |>
                           paste(collapse = "\u2013")
                       )) {

  k <- function(factor = 1) factor * basesize / .pt
  source <- "Source: UNHCR."
  name <- namer(hero)
  t0 <- gdi_plot_data("refug", hero)$range[1]
  t1 <- gdi_plot_data("refug", hero)$range[2]

  orig_disp <- paste0("Refugees from ", name, "\nand where they are hosted")
  host_disp <- paste0("Refugees hosted in ", name, "\nand where they come from")

  data <- gdi_plot_data("refug", hero)$data |>
    mutate(panel = ifelse(.data$panel == "orig", orig_disp, host_disp))

  plot_elements <- list(
    geom_area(stat = "identity"),
    facet_wrap(~fct_relevel(.data$panel, host_disp, after = Inf)),
    labs(title = title, caption = source),
    scale_x_continuous(
      breaks = seq(1990, 2020, 10),
      expand = expansion(mult = .05),
      guide = guide_axis(minor.ticks = TRUE)
    ),
    scale_fill_manual(values = regions),
    apply_theme("bar-vertical", basesize, font, facets = TRUE),
    theme(
      axis.ticks.x = element_line(color = pal("blues"), linewidth = k(.05)),
      legend.position = "right",
      panel.spacing.x = unit(k(.5), "lines"),
      plot.margin = margin(k(2), 0, k(2), k(2)),
      strip.text = element_text(
        size = basesize,
        margin = margin(t = 0, b = k())
      )
    )
  )

  if (nrow(data) > 0) {

    df_agg <- summarise(data, n = sum(.data$n), .by = c(.data$t, .data$panel))
    axis <- set_axis(df_agg$n, "Persons")

    df <- data |>
      complete(
        region = names(regions),
        t = t0:t1,
        panel = c(orig_disp, host_disp),
        fill = list(n = 0)
      )

    plot <- ggplot(
      df,
      aes(x = .data$t, y = .data$n, group = .data$region, fill = .data$region)) +
      plot_elements +
      scale_y_continuous(
        name = axis$title,
        breaks = axis$breaks,
        labels = axis$labels,
        expand = expansion(mult = c(0, .025))
      ) +
      theme(axis.title.y = element_text(
        size = basesize,
        margin = margin(r = k(2))
      ))

  } else {

    df <- expand.grid(
      region = names(regions),
      t = t0:t1,
      panel = c(orig_disp, host_disp),
      n = 0
    )

    plot <- ggplot(
      df,
      aes(x = .data$t, y = .data$n, group = .data$region, fill = .data$region)
    ) +
      plot_elements +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(color = NA, fill = pal("unblues", 5)),
        panel.grid.major.y = element_blank(),
      )

    plot <- ggdraw(plot) +
      draw_label(
        "No data",
        x = .2240,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      ) +
      draw_label(
        "No data",
        x = .6575,
        y = .5,
        fontfamily = font,
        color = pal("blues", 3),
        size = k(3)
      )
  }

  return(plot)
}





