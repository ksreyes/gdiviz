
library(dplyr)
library(ggplot2)

# Migrant stocks ----------------------------------------------------------

plot_stocks <- function(hero,
                        basesize = 8,
                        index = NULL,
                        index_h = NULL,
                        title = TRUE,
                        caption = FALSE,
                        width = 12,
                        height = 8) {

  # Parameters
  size <- sizer(basesize)
  max_name <- (width / basesize) * 15
  max_caption <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / .pt
  ids <- plot_ider("stocks")
  name <- namer(hero, max_name)

  # Data
  df <- bind_rows(
    stocks |>
      filter(from == hero & sex == "Total") |>
      summarise(v = sum(v), .by = c(from, t)) |>
      mutate(type = "Emigrants") |>
      rename(iso = from),
    stocks |>
      filter(to == hero & sex == "Total") |>
      summarise(v = sum(v), .by = c(to, t)) |>
      mutate(type = "Immigrants") |>
      rename(iso = to)
  )

  # Chart
  plot <- ggplot(
    df,
    aes(x = factor(t), y = v, color = type, group = type)
  ) +
    geom_line(linewidth = k(.3)) +
    scale_color_manual(values = c(pal("blues"), pal("reds"))) +

    # Annotation
    add_labels(
      df,
      type,
      basesize = basesize,
      colorscale = c(pal("blues"), pal("reds"))
    ) +

    # Aesthetics
    apply_theme(type = "line", basesize) +
    scale_x_discrete(
      breaks = seq(span(df)[1], span(df)[2], 10),
      expand = expansion(mult = c(.01, .1))
    ) +
    scale_y_continuous(name = scale_title(df), labels = scale_labels) +
    theme(
      axis.title.y = element_text(
        size = size$text,
        margin = margin(r = k(3.5))
      ))

  # Title
  if (is.logical(title) & title == TRUE) {
    plot <- plot + ggtitle(stringr::str_glue("{ids$title}, {range(df)}"))
  } else if (is.character(title) | is.numeric(title)) {
    plot <- plot + ggtitle(title)
  }

  # Caption
  if (is.logical(caption) & caption == TRUE) {
    caption_set <- filter(captions, CountryCode == hero)
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
      "• ",
      format_caption(cap1, max = max_caption, bullet = TRUE),
      "<br>", "• ",
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
  plot <- plot + labs(caption = caption_text)

  if (!is.null(index)) plot <- plot_label(plot, index, h = index_h)

  return(plot)
}

# Migrant sex ratios ------------------------------------------------------

plot_srat <- function(hero,
                      basesize = 8,
                      index = NULL,
                      index_h = NULL,
                      title = TRUE,
                      caption = FALSE,
                      width = 12,
                      height = 8) {

  # Parameters
  size <- sizer(basesize)
  max_name <- (width / basesize) * 15
  max_caption <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / .pt
  ids <- plot_ider("stocks")
  name <- namer(hero, max_name)

  # Data
  df <- bind_rows(
    stocks |>
      filter(from == hero & sex != "Total" & t == max(t)) |>
      summarise(v = sum(v), .by = c(from, sex, t)) |>
      mutate(type = "Emigrants") |>
      rename(iso = from),
    stocks |>
      filter(to == hero & sex != "Total" & t == max(t)) |>
      summarise(v = sum(v), .by = c(to, sex, t)) |>
      mutate(type = "Immigrants") |>
      rename(iso = to)
  ) |>
    mutate(
      v = v / sum(v),
      pos = case_when(sex == "Male" ~ .1, .default = .9),
      .by = c(t, type)
    )

  # Chart
  plot <- ggplot(df, aes(x = 100 * v, y = factor(t), fill = sex)) +
    geom_bar(stat = "identity", position = "fill", width = .7) +
    scale_fill_manual(values = c(pal("blues", 3), pal("blues", 2))) +
    facet_wrap(~ type, nrow = 2) +
    linemarker("v", at = .5) +
    geom_text(
      aes(x = pos, label = prettylabel(100 * v, pct = TRUE)),
      size = k(), fontface = "bold", color = "white", hjust = .5
    ) +

    # Aesthetics
    apply_theme(type = "bar-horizontal", basesize, facets = TRUE) +
    scale_x_continuous(breaks = .5, labels = "50%") +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(panel.spacing.y = unit(k(5), "points"))

  # Title
  if (is.logical(title) & title == TRUE) {
    plot <- plot + ggtitle(stringr::str_glue("{ids$title}, {df$t[1]}"))
  } else if (is.character(title) | is.numeric(title)) {
    plot <- plot + ggtitle(title)
  }

  # Caption
  if (is.logical(caption) & caption == TRUE) {
    caption_set <- filter(captions, CountryCode == hero)
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
  plot <- plot + labs(caption = caption_text)

  if (!is.null(index)) plot <- plot_label(plot, index, h = index_h)

  return(plot)
}

# Destinations of emigrants -----------------------------------------------

plot_dest <- function(hero,
                      basesize = 8,
                      index = NULL,
                      index_h = NULL,
                      title = TRUE,
                      caption = FALSE,
                      width = 12,
                      height = 8) {

  # Parameters
  size <- sizer(basesize)
  max_name <- (width / basesize) * 15
  max_caption <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / .pt
  ids <- plot_ider("stocks")
  name <- namer(hero, max_name)

  # Data
  df <- stocks |>
    filter(from == hero & sex == "Total" & t == max(t)) |>
    arrange(desc(v)) |>
    mutate(
      rank = 1:n(),
      country = case_when(
        rank > threshold ~ "Others",
        nchar(countryname(to)) <= max_name ~ countryname(to),
        .default = to
      )
    ) |>
    summarise(v = sum(v), .by = c(from, t, country)) |>
    mutate(
      v = v / sum(v),
      country = forcats::fct_reorder(country, v),
      country = forcats::fct_relevel(country, "Others", after = 0)
    )

  # Chart
  plot <- ggplot(df, aes(x = 100 * v, y = country)) +
    geom_bar(stat = "identity", fill = pal("blues", 2), width = .7) +
    linemarker("v") +

    # Annotation
    geom_text(
      mapping = aes(label = prettylabel(100 * v, pct = TRUE)),
      size = k(), color = pal("blues"), fontface = "bold",
      hjust = 0, vjust = .5, nudge_x = 1
    ) +

    # Aesthetics
    apply_theme(type = "bar-horizontal", basesize) +
    scale_x_continuous(
      labels = function(x) prettylabel(x, pct = TRUE),
      expand = expansion(mult = c(.02, .15))
    ) +
    theme(plot.margin = margin(k(5), k(3.5), k(5), k(3.5)))

  # Title
  if (is.logical(title) & title == TRUE) {
    plot <- plot + ggtitle(stringr::str_glue("{ids$title} ({df$t[1]})"))
  } else if (is.character(title) | is.numeric(title)) {
    plot <- plot + ggtitle(title)
  }

  # Caption
  if (is.logical(caption) & caption == TRUE) {
    caption_set <- filter(captions, CountryCode == hero)
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
  plot <- plot + labs(caption = caption_text)

  if (!is.null(index)) plot <- plot_label(plot, index, h = index_h)

  return(plot)
}

# Origin of immigrants ----------------------------------------------------

plot_orig <- function(hero,
                      basesize = 8,
                      index = NULL,
                      index_h = NULL,
                      title = TRUE,
                      caption = FALSE,
                      width = 12,
                      height = 8) {

  # Parameters
  size <- sizer(basesize)
  max_name <- (width / basesize) * 15
  max_caption <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / .pt
  ids <- plot_ider("stocks")
  name <- namer(hero, max_name)

  # Data
  df <- stocks |>
    filter(to == hero & sex == "Total" & t == max(t)) |>
    arrange(desc(v)) |>
    mutate(
      rank = 1:n(),
      country = case_when(
        from == "XXX" ~ "Others",
        rank > threshold ~ "Others",
        nchar(countryname(from)) <= max_name ~ countryname(from),
        .default = from
      )
    ) |>
    summarise(v = sum(v), .by = c(to, t, country)) |>
    mutate(
      v = v / sum(v),
      country = forcats::fct_reorder(country, v),
      country = forcats::fct_relevel(country, "Others", after = 0)
    )

  if (nrow(df) > threshold) {
    df <- df |>
      mutate(country = forcats::fct_relevel(country, "Others", after = 0))
  }

  # Chart
  plot <- ggplot(df, aes(x = 100 * v, y = country)) +
    geom_bar(stat = "identity", fill = pal("blues", 3), width = .7) +
    linemarker("v") +

    # Annotation
    geom_text(
      mapping = aes(label = prettylabel(100 * v, pct = TRUE)),
      size = k(), color = pal("blues"), fontface = "bold",
      hjust = 0, vjust = .5, nudge_x = 1
    ) +

    # Aesthetics
    apply_theme(type = "bar-horizontal", basesize) +
    scale_x_continuous(
      labels = function(x) prettylabel(x, pct = TRUE),
      expand = expansion(mult = c(.02, .15))
    ) +
    theme(plot.margin = margin(k(5), k(3.5), k(5), k(3.5)))


  # Title
  if (is.logical(title) & title == TRUE) {
    plot <- plot + ggtitle(stringr::str_glue("{ids$title} ({df$t[1]})"))
  } else if (is.character(title) | is.numeric(title)) {
    plot <- plot + ggtitle(title)
  }

  # Caption
  if (is.logical(caption) & caption == TRUE) {
    caption_set <- filter(captions, CountryCode == hero)
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
  plot <- plot + labs(caption = caption_text)

  if (!is.null(index)) plot <- plot_label(plot, index, h = index_h)

  return(plot)
}

