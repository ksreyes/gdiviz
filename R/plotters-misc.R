
# Internal displacements --------------------------------------------------

plot_idp <- function(hero,
                     basesize = 8,
                     title = TRUE,
                     caption = FALSE,
                     caption_maxchar = NULL,
                     width = 12,
                     height = 8) {

  # Parameters
  size <- sizer(basesize)
  name_maxchar <- (width / basesize) * 15
  caption_maxchar <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / ggplot2::.pt
  ids <- plot_ider("idp")
  name <- namer(hero, name_maxchar)

  # Data
  df <- idmc |>
    dplyr::filter(.data$dataset == "national") |>
    tidyr::complete(
      .data$iso,
      t = tidyr::full_seq(.data$t, 1),
      .data$category,
      fill = list(v = 0)
    ) |>
    dplyr::filter(.data$iso == hero)

  if (nrow(dplyr::filter(df, .data$v > 0)) > 0) {

    labs <- c(
      "Geological, water, and\nextreme weather hazards",
      "Conflict and\nviolence"
    )

    df_agg <- dplyr::summarise(df, v = sum(.data$v), .by = .data$t) |>
      dplyr::filter(.data$v > 0)

    plot <- ggplot2::ggplot() +
      ggplot2::geom_bar(
        ggplot2::aes(
          x = factor(.data$t),
          y = .data$v,
          fill = forcats::fct_rev(.data$category)
        ),
        df,
        stat = "identity", position = "stack", width = .7
      ) +
      ggplot2::scale_fill_manual(
        values = palettes$n2,
        labels = labs
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = factor(.data$t),
          y = .data$v,
          label = prettylabel(.data$v)
        ),
        df_agg,
        size = k(.8), color = pal("blues"), fontface = "bold",
        nudge_y = max(df$v) / 50, vjust = 0
      ) +

      # Aesthetics
      apply_theme(type = "bar-vertical", basesize) +
      ggplot2::scale_y_continuous(
        name = "thousands of persons",
        labels = scale_labels
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          margin = ggplot2::margin(t = -k(.5))
        ))

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
        format_caption(cap, max = caption_maxchar)
      )
    } else if (is.character(caption) | is.numeric(caption)) {
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(caption, max = caption_maxchar)
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
        format_caption(caption, max = caption_maxchar)
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

# Causes of displacements -------------------------------------------------

plot_idcause <- function(hero,
                         basesize = 8,
                         title = TRUE,
                         caption = FALSE,
                         caption_maxchar = NULL,
                         width = 12,
                         height = 8) {

  # Parameters
  size <- sizer(basesize)
  name_maxchar <- (width / basesize) * 15
  caption_maxchar <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / ggplot2::.pt
  ids <- plot_ider("idcause")
  name <- namer(hero, name_maxchar)

  # Data
  df_raw <- disasters |>
    dplyr::filter(
      .data$iso == hero,
      .data$category == "People displaced",
      .data$t >= 2010
    )
  df <- df_raw |>
    dplyr::summarise(v = sum(.data$v), .by = c(.data$iso, .data$type)) |>
    dplyr::filter(.data$v > 0) |>
    dplyr::arrange(dplyr::desc(.data$v))

    if (nrow(df) > 3) {
      df <- df |>
        dplyr::mutate(
          rank = 1:dplyr::n(),
          type = dplyr::case_when(
            .data$rank >= 4 ~ "Others",
            .default = .data$type
          )) |>
        dplyr::summarise(v = sum(.data$v), .by = c(.data$iso, .data$type))
    }

  if (nrow(df) > 0) {

    df <- df |>
      dplyr::mutate(
        share = .data$v / sum(.data$v),
        ymax = cumsum(.data$share),
        ymin = c(0, .data$ymax[-dplyr::n()]),
        pos = (.data$ymax + .data$ymin) / 2,
        label = ifelse(
          .data$share >= .02,
          prettylabel(100 * .data$share, pct = TRUE, omit_zero = TRUE),
          NA
        )
      )

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        ymax = .data$ymax,
        ymin = .data$ymin,
        xmax = 4,
        xmin = 3,
        fill = forcats::fct_reorder(.data$type, -.data$share)
      )
    ) +
      ggplot2::geom_rect() +
      ggplot2::scale_fill_manual(values = c(palettes$n3, pal("grays"))) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::geom_text(
        ggplot2::aes(y = .data$pos, label = .data$label),
        size = k(), color = "white", fontface = "bold",
        x = 3.5, hjust = .5, vjust = .5
      ) +

      # Aesthetics
      apply_theme(type = "bar-vertical", basesize) +
      ggplot2::scale_x_continuous(limits = c(2, 4)) +
      ggplot2::scale_y_continuous(expand = ggplot2::waiver()) +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank()
      )

    # Title
    title_auto <- paste0(ids$title, ", ", range(df_raw))
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
        format_caption(cap, max = caption_maxchar)
      )
    } else if (is.character(caption) | is.numeric(caption)) {
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        format_caption(caption, max = caption_maxchar)
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
        format_caption(caption, max = caption_maxchar)
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
