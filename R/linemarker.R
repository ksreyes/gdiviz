#' Insert line marker in plot
#'
#' @param direction "h" for a horizontal line or "v" for a vertical line.
#' @param at x- or y-intercept.
#'
#' @return A ggplot object.
#'
#' @export
linemarker <- function(direction, at = 0) {

  if (direction == "h") {
    geom_hline(yintercept = at, color = pal("blues"), linewidth = .25)
  }

  if (direction == "v") {
    geom_vline(xintercept = at, color = pal("blues"), linewidth = .25)
  }
}
