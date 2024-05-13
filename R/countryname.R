#' Toggle between country names and ISO codes
#'
#' See `countrynames` table for full dictionary.
#'
#' @param key Name or ISO code of country.
#' @param from Current type of `key`.
#' @param to Target type of `key`.
#'
#' @return String or vector of strings.
#'
#' @examples
#' countryname("DEU")
#' # Germany
#'
#' countryname("Germany", from = "unname", to = "iso3c")
#' # DEU
#'
#' @export
countryname <- function(key, from = "iso3c", to = "unname") {
  out <- c()
  for (k in key) {
    hit <- countrynames[{to}][countrynames[{from}] == toupper(k)]
    if (length(hit) == 0) out <- c(out, NA)
    else out <- c(out, hit)
  }
  return(out)
}
