#' Get size parameters for plot
#'
#' @param basesize Font size in pt.
#'
#' @return List comprising `text` for general text, `title` for plot title,
#'   `stext` for slightly smaller text, and `footnote` for caption text.
#'
#' @export
sizer <- function(basesize) {
  list(
    text = basesize,
    title = basesize + 2,
    stext = basesize - 1,
    footnote = basesize - 2
  )
}
