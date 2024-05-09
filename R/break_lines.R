#' Add line breaks to string
#'
#' @param text String to add line breaks to.
#' @param maxchars Maximum number of characters per line.
#'
#' @export
break_lines <- function(text, maxchars, strip = NULL) {

  collapse <- "<br>"

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
