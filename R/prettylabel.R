#' Pretty number formatting
#'
#' @description Formats numbers for display in charts.
#'
#' @param N Number or vector of numbers to be formatted.
#' @param d Decimal places. Does not apply if `shorten=TRUE`.
#' @param shorten Whether to abbreviate long numbers.
#' @param spell Whether to spell out "millions", "billions", and "trillions".
#' @param dotzero Whether to include zeros after the decimal point.
#' @param pct Whether to include a percent sign.
#' @param currency Whether to add a currency sign.
#' @param omit_zero Whether to omit zero. Useful for labeling plot axes.
#'
#' @return String or vector of strings.
#'
#' @export
prettylabel <- function(N,
                        d = 0,
                        shorten = TRUE,
                        spell = FALSE,
                        dotzero = TRUE,
                        pct = FALSE,
                        currency = NULL,
                        omit_zero = FALSE) {

  nsmall <- ifelse(dotzero, d, 0)

  labels <- c()

  for (n in N) {

    if (shorten) {

      if (spell) {
        tn <- " trillion"
        bn <- " billion"
        mn <- " million"
      } else {
        tn <- "T"
        bn <- "B"
        mn <- "M"
      }

      magnitude <- abs(n) |> log10() |> floor()

      if (is.na(magnitude)) {
        labels <- c(labels, NA)
        next
      }

      labeler <- function(n, magnitude, divide = 1, suffix = NULL) {

        m <- n / divide
        new_magnitude <- abs(m) |> log10() |> floor()

        # If m is single digit, add decimal
        if (n != 0 & new_magnitude <= 0) {
          paste0(
            currency,
            format(
              m,
              trim = TRUE,
              digits = 1,
              big.mark = ",",
              nsmall = nsmall,
              scientific = FALSE
            ),
            suffix
          )
        } else {
          paste0(
            currency,
            format(
              m,
              trim = TRUE,
              digits = 1,
              big.mark = ",",
              nsmall = 0,
              scientific = FALSE
            ),
            suffix
          )
        }
      }

      label <- dplyr::case_when(
        magnitude >= 12 & magnitude < 15 ~ labeler(n, magnitude, 10^12, tn),
        magnitude >= 9 & magnitude < 12 ~ labeler(n, magnitude, 10^9, bn),
        magnitude >= 6 & magnitude < 9 ~ labeler(n, magnitude, 10^6, mn),
        magnitude >= 3 & magnitude < 6 & !spell ~
          labeler(n, magnitude, 10^3,  "K"),
        magnitude >= 3 & magnitude < 6 & spell ~
          labeler(round(n, -3), magnitude),
        # magnitude >= 2 & magnitude < 3 ~ labeler(round(n, -2), magnitude),
        magnitude < 3 ~ labeler(n, magnitude),
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
