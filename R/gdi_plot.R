
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

  plots <- list(
    "stocks" = list(
      "func" = plot_migstocks,
      "meta" = list(dims = c(16, 7))
    ),
    "nats" = list(
      "func" = plot_nats,
      "meta" = list(dims = c(16, 7))
    ),
    "nmig" = list(
      "func" = plot_nmig,
      "meta" = list(dims = c(8, 7))
    ),
    "idp" = list(
      "func" = plot_idp,
      "meta" = list(dims = c(8, 7))
    )
  )

  if (key %in% names(plot_list)) {

    if (!is.null(iso)) {

      if (iso %in% countrynames$iso3) {

        return(plots[[key]]$func(iso, basesize, font, ...))

      } else {

        cli::cli_abort("{iso} is not a valid ISO3 code.")

      }

    } else {

      return(plots[[key]]$meta)

    }

  } else cli::cli_abort("{key} is not a valid plot key.")

}
