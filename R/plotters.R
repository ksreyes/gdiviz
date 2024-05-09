
# Migrant stocks ----------------------------------------------------------

plot_stocks <- function(hero,
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
  ids <- plot_ider("stocks")
  name <- namer(hero, max_name)

  # Data
  df <- dplyr::bind_rows(
    stocks |>
      dplyr::filter(.data$from == hero & .data$sex == "Total") |>
      dplyr::summarise(v = sum(.data$v), .by = c(.data$from, .data$t)) |>
      dplyr::mutate(type = "Emigrants") |>
      dplyr::rename(iso = .data$from),
    stocks |>
      dplyr::filter(.data$to == hero & .data$sex == "Total") |>
      dplyr::summarise(v = sum(.data$v), .by = c(.data$to, .data$t)) |>
      dplyr::mutate(type = "Immigrants") |>
      dplyr::rename(iso = .data$to)
  )

  if (nrow(df) > 0) {

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = factor(.data$t),
        y = .data$v,
        color = .data$type,
        group = .data$type
      )
    ) +
      ggplot2::geom_line(linewidth = k(.3)) +
      ggplot2::scale_color_manual(values = c(pal("blues"), pal("reds", 2))) +
      add_labels(
        df, .data$type,
        basesize = basesize,
        colorscale = c(pal("blues"), pal("reds", 2))
      ) +

      # Aesthetics
      apply_theme(type = "line", basesize) +
      ggplot2::scale_x_discrete(
        breaks = seq(span(df)[1], span(df)[2], 10),
        expand = ggplot2::expansion(mult = c(.01, .1))
      ) +
      ggplot2::scale_y_continuous(
        name = scale_title(df),
        labels = scale_labels
      ) +
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          size = size$text,
          margin = ggplot2::margin(r = k(3.5))
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
      cap1 <- gsub(
        pattern = hero,
        replacement = name,
        paste(caption_set$Emigration_2020, caption_set$Emigration_trend)
      )
      cap2 <- gsub(
        pattern = hero,
        replacement = name,
        paste(caption_set$Immigration_2020, caption_set$Immigration_trend)
      )
      caption_text <- paste0(
        format_source(ids$source, basesize = basesize),
        "\u00B7 ",
        format_caption(cap1, max = max_caption, bullet = TRUE),
        "<br>", "\u00B7 ",
        format_caption(cap2, max = max_caption, bullet = TRUE)
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

# Migrant sex ratios ------------------------------------------------------

plot_srat <- function(hero,
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
  ids <- plot_ider("srat")
  name <- namer(hero, max_name)

  # Data
  df <- dplyr::bind_rows(
    stocks |>
      dplyr::filter(
        .data$from == hero &
          .data$sex != "Total" &
          .data$t == max(.data$t)
      ) |>
      dplyr::summarise(
        v = sum(.data$v),
        .by = c(.data$from, .data$sex, .data$t)
      ) |>
      dplyr::mutate(type = "Emigrants") |>
      dplyr::rename(iso = .data$from),
    stocks |>
      dplyr::filter(
        .data$to == hero &
          .data$sex != "Total" &
          .data$t == max(.data$t)
      ) |>
      dplyr::summarise(
        v = sum(.data$v),
        .by = c(.data$to, .data$sex, .data$t)
      ) |>
      dplyr::mutate(type = "Immigrants") |>
      dplyr::rename(iso = .data$to)
  ) |>
    dplyr::mutate(
      v = .data$v / sum(.data$v),
      pos = dplyr::case_when(.data$sex == "Male" ~ .1, .default = .9),
      .by = c(.data$t, .data$type)
    )

  if (nrow(df) > 0) {

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = 100 * .data$v, y = factor(.data$t), fill = .data$sex)
    ) +
      ggplot2::geom_bar(stat = "identity", position = "fill", width = .7) +
      ggplot2::scale_fill_manual(values = c(pal("blues", 3), pal("blues", 2))) +
      ggplot2::facet_wrap(~ type, nrow = 2) +
      linemarker("v", at = .5) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = .data$pos,
          label = prettylabel(100 * .data$v, pct = TRUE)
        ),
        size = k(), fontface = "bold", color = "white", hjust = .5
      ) +

      # Aesthetics
      apply_theme(type = "bar-horizontal", basesize, facets = TRUE) +
      ggplot2::scale_x_continuous(breaks = .5, labels = "50%") +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::theme(panel.spacing.y = grid::unit(k(5), "points"))

    # Title
    if (is.logical(title) & title == TRUE) {
      plot <- plot +
        ggplot2::ggtitle(stringr::str_glue("{ids$title}, {df$t[1]}"))
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }

    # Caption
    if (is.logical(caption) & caption == TRUE) {
      caption_set <- dplyr::filter(captions, .data$CountryCode == hero)
      cap <- gsub(pattern = hero, replacement = name, caption_set$Sex)
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

# Destinations of emigrants -----------------------------------------------

plot_dest <- function(hero,
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
  ids <- plot_ider("dest")
  name <- namer(hero, max_name)
  threshold <- 5

  # Data
  df <- stocks |>
    dplyr::filter(
      .data$from == hero &
        .data$sex == "Total" &
        .data$t == max(.data$t)
    ) |>
    dplyr::arrange(dplyr::desc(.data$v)) |>
    dplyr::mutate(
      rank = 1:dplyr::n(),
      country = dplyr::case_when(
        .data$rank > threshold ~ "Others",
        nchar(countryname(.data$to)) <= max_name ~ countryname(.data$to),
        .default = .data$to
      )
    ) |>
    dplyr::summarise(
      v = sum(.data$v),
      .by = c(.data$from, .data$t, .data$country)
    ) |>
    dplyr::mutate(
      v = .data$v / sum(.data$v),
      country = forcats::fct_reorder(.data$country, .data$v),
      country = forcats::fct_relevel(.data$country, "Others", after = 0)
    )

  if (nrow(df) > 0) {

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = 100 * .data$v, y = .data$country)
    ) +
      ggplot2::geom_bar(stat = "identity", fill = pal("blues", 2), width = .7) +
      linemarker("v") +
      ggplot2::geom_text(
        mapping = ggplot2::aes(label = prettylabel(100 * .data$v, pct = TRUE)),
        size = k(), color = pal("blues"), fontface = "bold",
        hjust = 0, vjust = .5, nudge_x = 1
      ) +

      # Aesthetics
      apply_theme(type = "bar-horizontal", basesize) +
      ggplot2::scale_x_continuous(
        labels = function(x) prettylabel(x, pct = TRUE),
        expand = ggplot2::expansion(mult = c(.02, .15))
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(k(5), k(3.5), k(5), k(3.5)))

    # Title
    if (is.logical(title) & title == TRUE) {
      plot <- plot +
        ggplot2::ggtitle(stringr::str_glue("{ids$title} ({df$t[1]})"))
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }

    # Caption
    if (is.logical(caption) & caption == TRUE) {
      caption_set <- dplyr::filter(captions, .data$CountryCode == hero)
      cap <- gsub(pattern = hero, replacement = name, caption_set$DestinationSkew)
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

# Origin of immigrants ----------------------------------------------------

plot_orig <- function(hero,
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
  ids <- plot_ider("orig")
  name <- namer(hero, max_name)
  threshold <- 5

  # Data
  df1 <- stocks |>
    dplyr::filter(
      .data$to == hero & .data$sex == "Total" & .data$t == max(.data$t)
    )

  if (nrow(df1) > 0) {

    df <- df1 |>
      dplyr::filter(.data$from != "XXX") |>
      dplyr::arrange(dplyr::desc(.data$v)) |>
      dplyr::mutate(rank = 1:dplyr::n()) |>
      dplyr::bind_rows(dplyr::filter(df1, .data$from == "XXX")) |>
      dplyr::mutate(country = dplyr::case_when(
        .data$rank > threshold ~ "Others",
        .data$from == "XXX" ~ "Others",
        nchar(countryname(.data$from)) <= maxchar ~ countryname(.data$from),
        .default = .data$from
      )) |>
      dplyr::summarise(
        v = sum(.data$v),
        .by = c(.data$to, .data$t, .data$country)
      ) |>
      dplyr::mutate(
        v = .data$v / sum(.data$v),
        country = forcats::fct_reorder(.data$country, .data$v),
        country = forcats::fct_relevel(.data$country, "Others", after = 0)
      )

    if (nrow(df) > threshold) {
      df <- df |>
        dplyr::mutate(
          country = forcats::fct_relevel(.data$country, "Others", after = 0)
        )
    }

    plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = 100 * .data$v, y = .data$country)
    ) +
      ggplot2::geom_bar(stat = "identity", fill = pal("blues", 3), width = .7) +
      linemarker("v") +

      # Annotation
      ggplot2::geom_text(
        mapping = ggplot2::aes(label = prettylabel(100 * .data$v, pct = TRUE)),
        size = k(), color = pal("blues"), fontface = "bold",
        hjust = 0, vjust = .5, nudge_x = 1
      ) +

      # Aesthetics
      apply_theme(type = "bar-horizontal", basesize) +
      ggplot2::scale_x_continuous(
        labels = function(x) prettylabel(x, pct = TRUE),
        expand = ggplot2::expansion(mult = c(.02, .15))
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(k(5), k(3.5), k(5), k(3.5)))


    # Title
    if (is.logical(title) & title == TRUE) {
      plot <- plot +
        ggplot2::ggtitle(stringr::str_glue("{ids$title} ({df$t[1]})"))
    } else if (is.character(title) | is.numeric(title)) {
      plot <- plot + ggplot2::ggtitle(title)
    }

    # Caption
    if (is.logical(caption) & caption == TRUE) {
      caption_set <- dplyr::filter(captions, .data$CountryCode == hero)
      cap <- gsub(pattern = hero, replacement = name, caption_set$OriginSkew)
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

