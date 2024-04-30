#' Pretty number formatting
#'
#' @description
#' Formats numbers for display in charts.
#'  - `pretty` adds comma separators and/or a percent sign and provides control
#'       over decimal places.
#'  - `prettyabv`
#'
#' @param N Number or vector of numbers to be formatted.
#' @param d Decimal places. Does not apply if `shorten=TRUE`.
#' @param dotzero Whether to include zeros after the decimal point.
#' @param pct Whether to include a percent sign.
#' @param currency Whether to add a currency sign.
#' @param omit_zero Whether to omit zero. Useful for labeling plot axes.
#'
#' @return String or vector of strings.
#' @examples
#'
#' @export
prettylabel <- function(N,
                        d = 0,
                        shorten = TRUE,
                        dotzero = TRUE,
                        pct = FALSE,
                        currency = NULL,
                        omit_zero = FALSE) {

  nsmall <- ifelse(dotzero, d, 0)

  labels <- c()

  for (n in N) {

    if (shorten) {

      magnitude <- abs(n) |> log10() |> floor()

      # if (is.na(magnitude)) {
      #   labels <- c(labels, NA)
      #   next
      # }

      labeler <- function(divide = 1, suffix = NULL) {

        m <- n / divide
        new_magnitude <- abs(m) |> log10() |> floor()

        # If m is single digit, add decimal
        if (new_magnitude <= 0) {
          paste0(
            currency,
            format(m, digits = 1, nsmall = nsmall),
            suffix
          )
        } else {
          paste0(
            currency,
            format(m, digits = 1, nsmall = 0),
            suffix
          )
        }
      }

      label <- dplyr::case_when(
        magnitude >= 12 & magnitude < 15 ~ labeler(10^12, "T"),
        magnitude >= 9  & magnitude < 12 ~ labeler(10^9, "B"),
        magnitude >= 6  & magnitude < 9  ~ labeler(10^6, "M"),
        magnitude >= 3  & magnitude < 6  ~ labeler(10^3, "K"),
        magnitude < 3                    ~ labeler(),
        .default = "0"
      )

    } else {

      if (d == 0) {
        label <- paste0(
          currency,
          format(n, digits = 1, big.mark = ",", nsmall = 0)
        )
      } else {
        label <- paste0(
          currency,
          format(n, digits = d, big.mark = ",", nsmall = nsmall)
        )
      }

    }

    if (pct) label <- paste0(label, "%")
    if (omit_zero & n == 0) label <- ""

    labels <- c(labels, label)
  }

  return(labels)
}


