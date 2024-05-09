#' Insert line marker in plot
#'
#' @param direction "h" for a horizontal line or "v" for a vertical line.
#' @param at x- or y-intercept.
#'
#' @export
linemarker <- function(direction, at = 0) {

  if (direction == "h") {
    ggplot2::geom_hline(yintercept = at, color = pal("blues"), linewidth = .25)
  }

  if (direction == "v") {
    ggplot2::geom_vline(xintercept = at, color = pal("blues"), linewidth = .25)
  }
}
