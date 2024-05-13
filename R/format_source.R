#' Format plot source in HTML
#'
#' @param source String of the plot's source.
#' @param basesize Base text size of plot.
#' @param space_after Whether to add line breaks after the source. Use when plot has a caption.
#'
#' @return String.
#'
#' @export
format_source <- function(source, basesize = 8, space_after = TRUE) {

  size <- sizer(basesize)$footnote
  color <- pal("blues", 3)
  style <- stringr::str_glue("font-size:{size}pt; color:{color}")

  spaces <- ifelse(space_after, "<br><br>", "")

  stringr::str_glue("<span style='{style}'><i>Source: {source}.</i>{spaces}</span>")
}
