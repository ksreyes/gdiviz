

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
    tidyr::complete(
      .data$iso,
      t = tidyr::full_seq(.data$t, 1),
      .data$category,
      fill = list(.data$v = 0)
    ) |>
    dplyr::filter(.data$dataset == "national" & .data$iso == hero)

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
