
#' Generate a plot
#'
#' @param code The plot code. Run `gdiplots` for available options.
#' @param country The 3-letter ISO country code.
#' @param basesize Base font size.
#' @param title Set to `TRUE` to include default plot title or input a string
#'   for a custom title. Set to `FALSE` or `NULL` for no title.
#' @param caption Set to `TRUE` to include default plot caption or input a string
#'   for a custom caption. Set to `FALSE` or `NULL` for no caption.
#' @param export Set to either "svg" or "png" to export plot in those formats.
#' @param folder
#' @param width
#' @param height
#' @param units
#'
#' @return
#' @examples
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


  # Export ----------------------------------------------------------------

  if (!is.null(folder)) {
    if(!dir.exists(folder)) dir.create(folder)
    folder <- paste0(folder, "/")
  }
  if (export == "svg") {
    ggsave(
      paste0(folder, code, "_", country, ".svg"),
      device = "svg", width = width, height = height, units = "cm"
    )
  }
  if (export == "png") {
    ggsave(
      paste0(folder, code, "_", country, ".png"),
      device = "png", width = width, height = height, units = "cm"
    )
  }

  return(plot)
}
