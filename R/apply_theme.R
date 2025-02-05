#' Apply IOM theme to a plot
#'
#' Adds thematic presets adhering to the IOM visual branding to a ggplot object.
#'
#' The `basesize` parameter is used for the font size of axis labels and titles
#' and plot annotations.
#'
#' @param type What type of plot is being themed? Accepts "line",
#'    "bar-horizontal", "bar-vertical", "map", and "void".
#' @param basesize Font size in points. Default is 10.
#' @param facets Is this a faceted plot?
#'
#' @export
apply_theme <- function(type, basesize = 8, facets = FALSE) {

  # Size parameters
  size <- list(
    text = basesize,
    title = basesize + 2,
    stext = basesize - 1,
    footnote = basesize - 2
  )

  # Scaler
  k <- function(factor = 1) factor * size$text / ggplot2::.pt

  panel_grid_color <- pal("blues", 5)
  panel_grid_width <- k(.1)
  bar_key_size <- grid::unit(1.25 * size$text, "points")
  line_key_height <- grid::unit(size$text, "points")
  line_key_width <- grid::unit(2 * size$text, "points")
  steps_key_height <- grid::unit(.75 * size$text, "points")
  steps_key_width <- grid::unit(2 * size$text, "points")

  base <- list(
    ggplot2::theme(
      # text = ggplot2::element_text(color = pal("blues")),
      text = ggplot2::element_text(
        family = "Gill Sans Nova",
        color = pal("blues")
      ),

      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = size$text, color = pal("blues")),
      axis.ticks = ggplot2::element_blank(),

      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.key.spacing.y = grid::unit(.25 * size$text, "points"),
      legend.text = ggplot2::element_text(
        size = size$text,
        margin = ggplot2::margin(r = k(2.5), l = k())
      ),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box.margin = ggplot2::margin(t = k(-3), b = k(-3)),

      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      plot.background = ggplot2::element_blank(),
      # plot.title = ggtext::element_markdown(
      #   size = size$title,
      #   face = "bold",
      #   hjust = .5,
      #   margin = ggplot2::margin(b = k(3.5))
      # ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = size$title,
        hjust = .5,
        margin = ggplot2::margin(b = k(3.5))
      ),
      plot.title.position = "plot",
      # plot.caption = ggtext::element_markdown(
      #   size = size$text,
      #   hjust = 0,
      #   lineheight = 1.2,
      #   margin = ggplot2::margin(t = k(3.5), r = k(1), l = k(1))
      # ),
      plot.caption = ggplot2::element_text(
        face = "italic",
        color = pal("blues", 3),
        size = size$text,
        hjust = 0,
        lineheight = 1.2,
        margin = ggplot2::margin(t = k(3.5), r = k(1), l = k(1))
      ),
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(k(4), k(4), k(4), k(4))
    ))

  if (type == "void") {

    theme_void <- list(
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(
          fill = NA,
          color = pal("blues", 5),
          linewidth = k(.2)
        ),
        panel.grid.major = ggplot2::element_blank()
      ))
    theme <- c(base, theme_void)
  }

  if (type == "bar-horizontal") {

    theme_barh <- list(
      ggplot2::theme(
        legend.key.size = bar_key_size,
        panel.grid.major.x = ggplot2::element_line(
          linewidth = panel_grid_width,
          color = panel_grid_color
        ),
        panel.grid.major.y = ggplot2::element_blank(),
      ))
    theme <- c(base, theme_barh)
  }

  if (type == "bar-vertical") {

    theme_barv <- list(
      ggplot2::theme(
        legend.key.size = bar_key_size,
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(
          linewidth = panel_grid_width,
          color = panel_grid_color
        ),
      ))
    theme <- c(base, theme_barv)
  }

  if (type == "line") {

    theme_line <- list(
      ggplot2::theme(
        legend.key.height = line_key_height,
        legend.key.width = line_key_width,
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(
          linewidth = panel_grid_width,
          color = panel_grid_color
        ),
      ))
    theme <- c(base, theme_line)
  }

  if (type == "scatter") {

    theme_scat <- list(
      ggplot2::theme(
        legend.key.size = bar_key_size,
        panel.grid.major = ggplot2::element_line(
          linewidth = panel_grid_width,
          color = panel_grid_color
        ),
      ))
    theme <- c(base, theme_scat)
  }

  if (type == "map") {

    theme_map <- list(
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        legend.key.height = steps_key_height,
        legend.key.width = steps_key_width,
        legend.text = ggplot2::element_text(
          size = size$stext,
          margin = ggplot2::margin(t = k(1.5))
        ),
        panel.grid.major = ggplot2::element_blank()
      ))
    theme <- c(base, theme_map)
  }

  if (facets) {

    theme_facets <- list(
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = NA),
        strip.text = ggplot2::element_text(
          size = size$title,
          color = pal("blues"),
          margin = ggplot2::margin(b = k(3.5))
        )
      ))
    theme <- c(theme, theme_facets)

  }

  return(theme)
}
