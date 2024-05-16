
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdiviz

<!-- badges: start -->
<!-- badges: end -->

This package is a collection of data and tools that allows for the easy
production of charts.

## Installation

You can install the development version of gdiviz from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ksreyes/gdiviz")
```

## Getting started

The key function of this package is `gdiplot()`, which generates a
`ggplot` chart. It requires two arguments: `code`, which determines the
variable and chart type to be created, and `country`, which is the
3-letter ISO code of the country to chart.

``` r
library(gdiviz)
gdiplot(code = "stocks", country = "DEU")
```

To see the list of available charts and their respective codes, run
`gdiplots`. Charts can be exported as SVG or PNG files with the `export`
argument. See `?gdiplot` for more information on this and other
arguments.

The underlying data are bundled with this package and are available to
users. Run `gdidata` to see the list. To check data availability for a
country, run `check_data(iso)`, where `iso` is the 3-letter ISO code.

Datasets are indexed by ISO codes. For the user’s convenience,
`countryname()` can be used to look up the corresponding country name.
For example, `countryname("DEU")` returns “Germany”. The reverse is also
possible, with `countryname("Germany", from = "name", to = "iso3c")`
returning “DEU”. Alternatively, run `countrynames` to see the full list
of countries.

## Disclaimer

I wrote this package to make my job easier. Nothing here has the
official endorsement of my employer.
