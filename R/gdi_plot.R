
#' Generate a preset plot
#'
#' @param plot_key Plot key.
#' @param iso Country.
#' @param basesize Base text size.
#' @param title Plot title.
#' @param dims If `TRUE`, returns the default plot dimensions in cm
#'   (default`=FALSE`).
#'
#' @returns `ggplot2` object.
#'
#' @export
gdi_plot <- function(plot_key,
                     iso,
                     basesize = 7,
                     title,
                     dims = FALSE
                     ) {

  dimensions <- c()

  if (!dims) {

    if (plot_key == "stocks") {
      if (missing(title)) plot <- plot_migstocks(iso, basesize)
      else plot <- plot_migstocks(iso, basesize, title)
      dimensions <- c(16, 7)
    }

    if (plot_key == "nats") {
      if (missing(title)) plot <- plot_nats(iso, basesize)
      else plot <- plot_nats(iso, basesize, title)
      dimensions <- c(16, 7)
    }

    if (plot_key == "nmig") {
      if (missing(title)) plot <- plot_nmig(iso, basesize)
      else plot <- plot_nmig(iso, basesize, title)
      dimensions <- c(8, 7)
    }

    return(plot)

  } else {

    if (plot_key == "stocks") dimensions <- c(16, 7)
    if (plot_key == "nats") dimensions <- c(16, 7)
    if (plot_key == "nmig") dimensions <- c(8, 7)

    return(dimensions)

  }
}
