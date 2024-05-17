#' Get members of a country group
#'
#' Currently accepts "EU", "Schengen", "Balkans".
#'
#' @param group (Case-insensitive) name of grouping.
#'
#' @return Vector of ISO codes.
#'
#' @export
get_members <- function(group) {
  groupings$iso3c[groupings[{tolower(group)}] == 1]
}
