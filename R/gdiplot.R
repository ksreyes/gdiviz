
#' Generate a plot
#'
#' @param code The plot code. See `gdiplots` for available options.
#' @param country The 3-letter ISO country code.
#' @param basesize Base font size.
#' @param title Set to `TRUE` to include default plot title or input a string
#'   for a custom title. Set to `FALSE` or `NULL` for no title.
#' @param caption Set to `TRUE` to include default plot caption or input a
#'   string for a custom caption. Set to `FALSE` or `NULL` for no caption.
#' @param export Set to either "svg" or "png" to export plot in those formats.
#' @param folder Export destination. Leave as `NULL` to export to current
#'   directory.
#' @param width Exported plot width in cm. Leave as `NULL` for default width.
#' @param height Exported plot height in cm. Leave as `NULL` for default height.
#'
#' @return A ggplot object.
#'
#' @export
gdiplot <- function(code,
                    country,
                    basesize = 8,
                    title = TRUE,
                    caption = FALSE,
                    export = "none",
                    folder = NULL,
                    width = NULL,
                    height = NULL) {

  if (code == "stocks") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_stocks(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "srat") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_srat(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "dest") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_dest(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "orig") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_orig(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "nmig") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_nmig(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "remt") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_remt(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "fdi") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_fdi(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "pop") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_pop(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "birth") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_birth(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  if (code == "depend") {

    if (is.null(width)) width <- 12
    if (is.null(height)) {
      if (caption == TRUE | is.character(caption)) height <- 8 * 1.25
      else height <- 8
    }

    plot <- plot_depend(
      hero = country,
      basesize = basesize,
      title = title,
      caption = caption,
      width = width,
      height = height
    )
  }

  # Export ----------------------------------------------------------------

  if (!is.null(folder)) {
    if(!dir.exists(folder)) dir.create(folder)
    folder <- paste0(folder, "/")
  }
  if (export == "svg") {
    ggplot2::ggsave(
      paste0(folder, code, "_", country, ".svg"),
      device = "svg", width = width, height = height, units = "cm"
    )
  }
  if (export == "png") {
    ggplot2::ggsave(
      paste0(folder, code, "_", country, ".png"),
      device = "png", width = width, height = height, units = "cm"
    )
  }

  return(suppressMessages(plot))
}
