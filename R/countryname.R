#' Toggle between country names and ISO codes
#'
#' @description Easily convert a country name to its ISO code and vice versa.
#'   Official United Nations country names are used. See `countrynames` for full
#'   dictionary.
#'
#' @details Under the default `exact = FALSE`, matching is done using
#'   `stringr::str_detect`, which means keys need not be exact. For example,
#'   `countryname("iran", from = "name", to = "iso3c)` will return the correct
#'   ISO code "IRN" even if the dictionary name is "Islamic Republic of Iran".
#'   One can therefore check a country's official name by running
#'   `countryname("iran", from = "name")`.
#'
#'   If there are multiple matches, `countryname()` will only return the first
#'   match. Thus, while "guinea" matches with four countries (Equatorial Guinea,
#'   Guinea, Guinea-Bissau, Papua New Guinea), `countryname("guinea")` will only
#'   return the keys for Equatorial Guinea, which is a problem if one wants to
#'   obtain the keys for Guinea. Set `exact = TRUE` to use exact matching.
#'
#'   Regardless of how `exact` is set, matching remains case-insensitive,
#'
#' @param key (Case-insensitive) name or ISO code of country.
#' @param from Current type of `key`.
#' @param to Target type of `key`.
#' @param exact Whether to use exact matching.
#' @param quiet Turn off warning messages.
#'
#' @return String or vector of strings.
#'
#' @examples
#' countryname("deu")
#' # "Germany"
#'
#' countryname("germany", from = "name", to = "iso3c")
#' # "DEU"
#'
#' countryname("guinea", from = "name")
#' # "Equatorial Guinea"
#'
#' countryname("guinea", from = "name", exact = TRUE)
#' # "Guinea"
#'
#' countryname("united kingdom", from = "name")
#' # "United Kingdom of Great Britain and Northern Ireland"
#'
#' @export
countryname <- function(key,
                        from = "iso3c",
                        to = "name",
                        exact = FALSE,
                        quiet = FALSE) {

  out <- c()
  nohits <- c()

  for (k in key) {

    from_col <- countrynames[{from}] |> unlist() |> unname()
    to_col <- countrynames[{to}] |> unlist() |> unname()

    if (exact) {
      row <- which(tolower(from_col) == tolower(k))
    } else {
      row <- which(stringr::str_detect(tolower(from_col), tolower(k)))
    }
    hit <- to_col[row]

    if (length(hit) == 0) {
      nohits <- c(nohits, paste0('"', k, '"'))
      out <- c(out, NA)
    }
    else {
      if (length(hit) > 1) {
        if (!quiet) cli::cli_warn('"{k}" matched with multiple rows. Only the first is returned. For more control over matching, set `exact = TRUE`.')
      }
      out <- c(out, hit[1])
    }
  }

  if (length(nohits) > 0) {
    if (!quiet) cli::cli_warn(c(
      'Unable to parse the following (returned NA) :',
      "*" = '{paste(nohits, collapse = ", ")}'
    ))
  }

  return(out)
}
