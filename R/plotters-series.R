
palettes <- list(
  n2 = c(pal("blues", 2), pal("blues", 3)),
  n3 = c(pal("blues"), pal("reds", 2), pal("greens")),
  n4 = c(pal("blues"), "black", pal("reds"), pal("greens"))
)

# Net migration -----------------------------------------------------------

plot_nmig <- function(hero,
                      basesize = 8,
                      title = TRUE,
                      caption = FALSE,
                      width = 12,
                      height = 8) {

  # Parameters
  size <- sizer(basesize)
  max_name <- (width / basesize) * 15
  max_caption <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / ggplot2::.pt
  ids <- plot_ider("nmig")
  name <- namer(hero, max_name)

  # Data
  df <- dplyr::left_join(
    dplyr::filter(indicators, .data$iso == hero & .data$var == "pop") |>
      dplyr::select(-.data$var) |>
      dplyr::rename(pop = .data$v),
    dplyr::filter(indicators, .data$iso == hero & .data$var == "nmig") |>
      dplyr::select(-.data$var) |>
      dplyr::rename(nmig = .data$v),
    by = c("iso", "t")
  ) |>
    dplyr::mutate(v = 1000 * .data$nmig / .data$pop)

  if (nrow(df) > 0) {

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = factor(.data$t), y = .data$v)
    ) +
      ggplot2::geom_bar(stat = "identity", width = .7, fill = pal("blues")) +
      linemarker("h") +

      # Aesthetics
      apply_theme(type = "bar-vertical") +
      ggplot2::scale_x_discrete(
        breaks = seq(span(df)[1], span(df)[2], 10),
        expand = ggplot2::expansion(mult = c(.03, .03))
      ) +
      ggplot2::scale_y_continuous(name = "migrants per 1000 people") +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 2),
        linetype =  ggplot2::guide_legend(nrow = 2)
      ) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = size$text,
          margin = ggplot2::margin(r = k(2))
        ),
        legend.position = "none"
      )

    # Title
    title_auto <- paste0(ids$title, ", ", range(df))
    if (is.logical(title) & title == TRUE) {
      plot <- plot + ggplot2::ggtitle(title_auto)
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }

    # Caption
    caption_set <- dplyr::filter(captions, .data$CountryCode == hero)
    if (is.logical(caption) & caption == TRUE & nrow(caption_set) > 0) {
      cap <- gsub(pattern = hero, replacement = name, caption_set$Netmigration)
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(cap, max = max_caption)
      )
    } else if (is.character(caption) | is.numeric(caption)) {
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(caption, max = max_caption)
      )
    } else {
      caption_text <- format_source(
        ids$source,
        basesize = basesize,
        space_after = FALSE
      )
    }
    plot <- plot + ggplot2::labs(caption = caption_text)

  } else {

    # Fallback if data is missing

    plot <- ggplot2::ggplot() + apply_theme(type = "void", basesize)
    if (is.logical(title) & title == TRUE) {
      plot <- plot + ggplot2::ggtitle(ids$title)
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }
    if (is.character(caption) | is.numeric(caption)) {
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(caption, max = max_caption)
      )
    } else {
      caption_text <- format_source(
        ids$source,
        basesize = basesize,
        space_after = FALSE
      )
    }
    plot <- plot + ggplot2::labs(caption = caption_text)
    plot <- cowplot::ggdraw(plot) +
      cowplot::draw_label("No data", color = pal("blues", 3), size = size$text)
  }

  return(plot)
}

# Remittances -------------------------------------------------------------

plot_remt <- function(hero,
                      basesize = 8,
                      title = TRUE,
                      caption = FALSE,
                      width = 12,
                      height = 8) {

  # Parameters
  size <- sizer(basesize)
  max_name <- (width / basesize) * 15
  max_caption <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / ggplot2::.pt
  ids <- plot_ider("remt")
  name <- namer(hero, max_name)

  # Data
  df <- dplyr::filter(
    indicators,
    .data$var %in% c("remin", "remout") & .data$iso == hero
  )

  if (nrow(df) > 0) {

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = factor(.data$t),
        y = .data$v,
        color = .data$var,
        group = .data$var
      )) +
      ggplot2::geom_line(linewidth = k(.3), na.rm = TRUE) +
      linemarker("h") +
      ggplot2::scale_color_manual(
        label = c("Remittance inflow", "Remittance outflow"),
        values = palettes$n3
      ) +
      add_labels(
        df, .data$var,
        basesize = basesize,
        currency = "$", worldmedian = FALSE,
        colorscale = palettes$n3
      ) +

      # Aesthetics
      apply_theme(type = "line") +
      ggplot2::scale_x_discrete(
        breaks = seq(span(df)[1], span(df)[2], 10),
        expand = ggplot2::expansion(mult = c(.01, .10))
      ) +
      ggplot2::scale_y_continuous(
        labels = function(x) prettylabel(x, currency = "$")
      )

    # Title
    title_auto <- paste0(ids$title, ", ", range(df))
    if (is.logical(title) & title == TRUE) {
      plot <- plot + ggplot2::ggtitle(title_auto)
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }

    # Caption
    caption_set <- dplyr::filter(captions, .data$CountryCode == hero)
    if (is.logical(caption) & caption == TRUE & nrow(caption_set) > 0) {
      cap <- gsub(pattern = hero, replacement = name, caption_set$Remittances)
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(cap, max = max_caption)
      )
    } else if (is.character(caption) | is.numeric(caption)) {
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(caption, max = max_caption)
      )
    } else {
      caption_text <- format_source(
        ids$source,
        basesize = basesize,
        space_after = FALSE
      )
    }
    plot <- plot + ggplot2::labs(caption = caption_text)

  } else {

    # Fallback if data is missing

    plot <- ggplot2::ggplot() + apply_theme(type = "void", basesize)
    if (is.logical(title) & title == TRUE) {
      plot <- plot + ggplot2::ggtitle(ids$title)
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }
    if (is.character(caption) | is.numeric(caption)) {
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(caption, max = max_caption)
      )
    } else {
      caption_text <- format_source(
        ids$source,
        basesize = basesize,
        space_after = FALSE
      )
    }
    plot <- plot + ggplot2::labs(caption = caption_text)
    plot <- cowplot::ggdraw(plot) +
      cowplot::draw_label("No data", color = pal("blues", 3), size = size$text)
  }

  return(plot)
}

# FDI ---------------------------------------------------------------------

plot_fdi <- function(hero,
                     basesize = 8,
                     title = TRUE,
                     caption = FALSE,
                     width = 12,
                     height = 8) {

  # Parameters
  size <- sizer(basesize)
  max_name <- (width / basesize) * 15
  max_caption <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / ggplot2::.pt
  ids <- plot_ider("fdi")
  name <- namer(hero, max_name)

  # Data
  df <- dplyr::filter(
    indicators,
    .data$var %in% c("fdiin", "fdiout") & .data$iso == hero
  )

  if (nrow(df) > 0) {

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = factor(.data$t),
        y = .data$v,
        color = .data$var,
        group = .data$var
      )) +
      ggplot2::geom_line(linewidth = k(.3), na.rm = TRUE) +
      linemarker("h") +
      ggplot2::scale_color_manual(
        label = c("FDI inflow", "FDI outflow"),
        values = palettes$n3
      ) +
      add_labels(
        df, .data$var,
        basesize = basesize,
        currency = "$", worldmedian = FALSE,
        colorscale = palettes$n3
      ) +

      # Aesthetics
      apply_theme(type = "line") +
      ggplot2::scale_x_discrete(
        breaks = seq(span(df)[1], span(df)[2], 10),
        expand = ggplot2::expansion(mult = c(.01, .10))
      ) +
      ggplot2::scale_y_continuous(
        labels = function(x) prettylabel(x, currency = "$")
      )

    # Title
    title_auto <- paste0(ids$title, ", ", range(df))
    if (is.logical(title) & title == TRUE) {
      plot <- plot + ggplot2::ggtitle(title_auto)
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }

    # Caption
    caption_set <- dplyr::filter(captions, .data$CountryCode == hero)
    if (is.logical(caption) & caption == TRUE & nrow(caption_set) > 0) {
      cap <- gsub(pattern = hero, replacement = name, caption_set$`Foreign direct investments`)
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(cap, max = max_caption)
      )
    } else if (is.character(caption) | is.numeric(caption)) {
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(caption, max = max_caption)
      )
    } else {
      caption_text <- format_source(
        ids$source,
        basesize = basesize,
        space_after = FALSE
      )
    }
    plot <- plot + ggplot2::labs(caption = caption_text)

  } else {

    # Fallback if data is missing

    plot <- ggplot2::ggplot() + apply_theme(type = "void", basesize)
    if (is.logical(title) & title == TRUE) {
      plot <- plot + ggplot2::ggtitle(ids$title)
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }
    if (is.character(caption) | is.numeric(caption)) {
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(caption, max = max_caption)
      )
    } else {
      caption_text <- format_source(
        ids$source,
        basesize = basesize,
        space_after = FALSE
      )
    }
    plot <- plot + ggplot2::labs(caption = caption_text)
    plot <- cowplot::ggdraw(plot) +
      cowplot::draw_label("No data", color = pal("blues", 3), size = size$text)
  }

  return(plot)
}
