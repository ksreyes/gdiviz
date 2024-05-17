
namer <- function(hero, maxchar) {
  ifelse(
    nchar(countryname(hero)) <= maxchar,
    countryname(hero),
    hero
  )
}

plot_ider <- function(plot_code) {
  list(
    code = plot_code,
    title = dplyr::filter(gdiplots, .data$code == plot_code)$title,
    source = dplyr::filter(gdiplots, .data$code == plot_code)$source
  )
}

get_avgs <- function(data, hero, world = TRUE) {

  data_iso <- dplyr::filter(data, .data$iso == hero)

  if (nrow(data_iso) > 0) {

    weights <- dplyr::bind_rows(

      dplyr::filter(stocks, .data$from == hero) |>
        dplyr::summarise(v = sum(.data$v), .by = c(.data$to, .data$t)) |>
        dplyr::mutate(
          share = .data$v / sum(.data$v),
          type = "Destin",
          .by = .data$t
        ) |>
        dplyr::select(.data$type, .data$t, iso = .data$to, .data$share),

      dplyr::filter(stocks, .data$to == hero) |>
        dplyr::summarise(v = sum(.data$v), .by = c(.data$from, .data$t)) |>
        dplyr::mutate(
          share = .data$v / sum(.data$v),
          type = "Origin",
          .by = .data$t
        ) |>
        dplyr::select(.data$type, .data$t, iso = .data$from, .data$share)
    )

    destin <- dplyr::left_join(
      data,
      dplyr::filter(weights, .data$type == "Destin"),
      by = c("iso", "t")
    ) |>
      dplyr::mutate(weight = .data$v * .data$share) |>
      tidyr::drop_na() |>
      dplyr::summarise(weight = sum(.data$weight), .by = .data$t) |>
      dplyr::mutate(
        series = stringr::str_glue("{hero} emigrant-destination countries (avg)")
      ) |>
      dplyr::select(.data$t, .data$series, v = .data$weight)

    origin <- dplyr::left_join(
      data,
      dplyr::filter(weights, .data$type == "Origin"),
      by = c("iso", "t")
    ) |>
      dplyr::mutate(weight = .data$v * .data$share) |>
      tidyr::drop_na() |>
      dplyr::summarise(weight = sum(.data$weight), .by = .data$t) |>
      dplyr::mutate(
        series = stringr::str_glue("{hero} immigrant-origin countries (avg)")
      ) |>
      dplyr::select(.data$t, .data$series, v = .data$weight)

    isoseries <- data_iso |>
      dplyr::rename(series = .data$iso)

    data_sum <- dplyr::bind_rows(isoseries, destin, origin) |>
      dplyr::mutate(series = forcats::fct_relevel(.data$series, hero, after = 0))

    if (world) {
      world_median <- dplyr::summarise(
        data,
        v = stats::median(.data$v, na.rm = TRUE),
        .by = .data$t
      ) |>
        dplyr::mutate(series = "World median") |>
        dplyr::select(.data$t, .data$series, .data$v)
      data_sum <- dplyr::bind_rows(data_sum, world_median) |>
        dplyr::mutate(
          series = forcats::fct_relevel(.data$series, "World median", after = 1)
        )
    }

    return(data_sum)

  } else {
    return(data_iso)
  }
}

get_last <- function(data, group, group2 = NULL) {
  data1 <- data |>
    dplyr::select(.data$t, {{ group }}, {{ group2 }}, .data$v) |>
    tidyr::drop_na() |>
    dplyr::mutate(max = max(.data$t), .by = {{ group }})
  dplyr::left_join(data, data1) |>
    dplyr::filter(.data$t == .data$max) |>
    suppressMessages()
}

add_labels <- function(df,
                       group,
                       group2 = NULL,
                       basesize,
                       pct = FALSE,
                       currency = NULL,
                       worldmedian = TRUE,
                       colorscale) {

  size <- sizer(basesize)

  labels <- list(
    ggrepel::geom_label_repel(
      mapping = ggplot2::aes(
        x = factor(.data$t), y = .data$v,
        fill = {{ group }},
        label = prettylabel(.data$v, pct = pct, currency = currency)
      ),
      data = get_last({{ df }}, {{ group }}, {{ group2 }}) |>
        dplyr::filter({{ group }} != "World median"),
      color = "white",
      size = size$text / ggplot2::.pt,
      fontface = "bold",
      hjust = .5,
      vjust = .5,
      box.padding = grid::unit(.14, "lines"),
      label.padding = grid::unit(.15, "lines"),
      label.r = grid::unit(.05, "lines"),
      label.size = .1,
      segment.colour = pal("blues", 3),
      point.size = NA,
      show.legend = FALSE,
      direction = "y"
    ),
    ggplot2::scale_fill_manual(values = colorscale)
  )

  if (worldmedian) {
    labels <- c(
      labels,
      list(ggplot2::scale_linetype_manual(
        values = c("solid", "22", "solid", "solid")
      ))
    )
  } else {
    labels <- c(labels, list(ggplot2::scale_linetype_manual(values = "solid")))
  }

  return(labels)
}

span <- function(data) c(min(data$t), max(data$t)) |> as.numeric()

range <- function(data, collapse = "-") {
  stringr::str_flatten(span(data), collapse)
}

plot_label <- function(plot, label, basesize = basesize, span = 2, h = .06) {

  box <- grid::rectGrob(gp = grid::gpar(fill = pal("blues", 4), col = NA))

  width <- .025 + .015 * nchar(label)
  height <- h
  if (span == 1) width <- width / 2
  if (span == 3) width <- width * 1.5

  cowplot::ggdraw(plot) +
    cowplot::draw_grob(
      box,
      x = 0, y = 1,
      vjust = 1,
      width = width, height = height
    ) +
    cowplot::draw_label(
      label,
      x = width / 2, y = 1 - height / 2,
      hjust = .5, vjust = .5,
      size = basesize, fontface = "bold", color = "white"
    )
}

scale_labels <- function(N) {
  max <- max(N, na.rm = TRUE)
  if (max < 1200) {
    n <- prettylabel(N, d = 1, shorten = FALSE, , dotzero = FALSE)
  }
  if ((max >= 1200) & (max < 1200000)) {
    n <- prettylabel(N / 10^3, d = 1, shorten = FALSE, dotzero = FALSE)
  }
  if (max >= 1200000) {
    n <- prettylabel(N / 10^6, d = 1, shorten = FALSE, , dotzero = FALSE)
  }
  return(n)
}

scale_title <- function(data) {
  if (max(data$v) < 1200) {
    title <- "persons"
  }
  if ((max(data$v) >= 1200) & (max(data$v) < 1200000)) {
    title <- "thousands of persons"
  }
  if (max(data$v) >= 1200000) {
    title <- "millions of persons"
  }
  return(title)
}

break_lines2 <- function(text, max, bullet = FALSE, bg = "white") {

  collapse <- "<br>"
  if (bullet) {
    collapse <- stringr::str_glue("<br><span style='color:{bg}'>\u00B7 </span>")
    max <- max - 2
  }

  # Remove --
  text <- gsub("^-- ", "", text)
  text <- gsub("-- ", " ", text)

  # Bold key figures
  text <- gsub("#", "**", text)

  # Escape special
  text <- gsub("-", "\\\\-", text)
  text <- gsub("'", "\\\\'", text)
  text <- gsub("~", "\\\\~", text)

  strip <- c(
    "\\(TOP10\\)",
    "\\(TOP33\\)",
    "\\(MID33\\)",
    "\\(BOT33\\)",
    "\\(BOT10\\)",
    "\\*\\*",
    "\\\\"
  )

  words <- strsplit(text, "\\s+")[[1]]

  current_line <- ""
  current_line_stripped <- ""
  output_lines <- c()

  for (word in words) {

    word_stripped <- gsub(paste(strip, collapse = "|"), "", word)

    if (nchar(current_line_stripped) + nchar(word_stripped) + 1 > max) {
      output_lines <- c(output_lines, current_line)
      current_line <- paste0(word, " ")
      current_line_stripped <- paste0(word_stripped, " ")

    } else {
      current_line <- paste0(current_line, word, " ")
      current_line_stripped <- paste0(current_line_stripped, word_stripped, " ")
    }

  }
  output_lines <- c(output_lines, current_line)

  result <- paste(output_lines, collapse = collapse)
  return(result)
}

format_caption <- function(text, max, bullet = FALSE, bgcolor = "white") {

  result <- break_lines2(text, max = max, bullet = bullet, bg = bgcolor)

  top10path <- system.file("images/top10.png", package = "gdiviz")
  top33path <- system.file("images/top33.png", package = "gdiviz")
  mid33path <- system.file("images/mid33.png", package = "gdiviz")
  bot33path <- system.file("images/bot33.png", package = "gdiviz")
  bot10path <- system.file("images/bot10.png", package = "gdiviz")

  # Insert icons
  result <- gsub(
    "\\(TOP10\\)",
    stringr::str_glue("<img src='{top10path}' width=5>"),
    result
  )
  result <- gsub(
    "\\(TOP33\\)",
    stringr::str_glue("<img src='{top33path}' width=5>"),
    result
  )
  result <- gsub(
    "\\(MID33\\)",
    stringr::str_glue("<img src='{mid33path}' width=5>"),
    result
  )
  result <- gsub(
    "\\(BOT33\\)",
    stringr::str_glue("<img src='{bot33path}' width=5>"),
    result
  )
  result <- gsub(
    "\\(BOT10\\)",
    stringr::str_glue("<img src='{bot10path}' width=5>"),
    result
  )

  return(result)
}
