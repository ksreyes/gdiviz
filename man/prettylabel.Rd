% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prettylabel.R
\name{prettylabel}
\alias{prettylabel}
\title{Pretty number formatting}
\usage{
prettylabel(
  N,
  d = 0,
  shorten = TRUE,
  spell = FALSE,
  dotzero = TRUE,
  pct = FALSE,
  currency = NULL,
  omit_zero = FALSE
)
}
\arguments{
\item{N}{Number or vector of numbers to be formatted.}

\item{d}{Decimal places. Does not apply if \code{shorten=TRUE}.}

\item{shorten}{Whether to abbreviate long numbers.}

\item{spell}{Whether to spell out "millions", "billions", and "trillions".}

\item{dotzero}{Whether to include zeros after the decimal point.}

\item{pct}{Whether to include a percent sign.}

\item{currency}{Whether to add a currency sign.}

\item{omit_zero}{Whether to omit zero. Useful for labeling plot axes.}
}
\value{
String or vector of strings.
}
\description{
Formats numbers for display in charts.
}
