
#' Generate a preset plot
#'
#' @param key Plot key.
#' @param iso Country.
#' @param basesize Base text size. Default is 7.
#' @param font Font family. Default is Open Sans.
#' @param ... Optional arguments.
#'
#' @description If `iso=NULL`, function returns `key` plot's metadata.
#'
#' @returns `ggplot2` object or a list of metadata.
#'
#' @export
gdi_plot <- function(key,
                     iso = NULL,
                     basesize = 7,
                     font = "Open Sans",
                     ...) {

  keys <- list(
    "stocks" = list(
      "dims" = c(16, 7)
    ),
    "nats" = list(
      "dims" = c(16, 7)
    ),
    "nmig" = list(
      "dims" = c(8, 7)
    ),
    "idp" = list(
      "dims" = c(8, 7)
    ),
    "mmp" = list(
      "dims" = c(8, 7)
    ),
    "refug" = list(
      "dims" = c(16, 7)
    )
  )

  if (key %in% names(keys)) {

    if (!is.null(iso)) {

      if (iso %in% countrynames$iso3) {

        plotter <- match.fun(paste0("plot_", key))
        return(plotter(iso, basesize, font, ...))

      } else cli::cli_abort("{iso} is not a valid ISO3 code.")

    } else return(keys[[key]])

  } else cli::cli_abort("{key} is not a valid plot key.")

}
