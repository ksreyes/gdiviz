
namer <- function(iso) {
  name_iso <- filter(gdidata::countrynames, .data$iso3 == iso)
  if (name_iso$with_the == 1) name <- paste0("the ", name_iso$name_text)
  else name <- name_iso$name_text
  return(name)
}

break_lines <- function(column) {

  dict <- c(
    "Plurinational State of Bolivia" =
      "Plurinational State\nof Bolivia",
    "China, Taiwan Province of China" =
      "Taiwan Province\nof China",
    "Democratic People's Republic of Korea" =
      "Democratic People's Republic\nof Korea",
    "Democratic Republic of the Congo" =
      "Democratic Republic\nof the Congo",
    "Lao People's Democratic Republic" =
      "Lao People's Democratic\nRepublic",
    "Federated States of Micronesia" =
      "Federated States\nof Micronesia",
    "Occupied Palestinian Territory" =
      "Occupied Palestinian\nTerritory",
    "Bolivarian Republic of Venezuela" =
      "Bolivarian Republic\nof Venezuela"
  )

  indices <- which(column %in% names(dict))

  new_col <- replace(
    column,
    indices,
    dict[column[indices]]
  )

  return(new_col)
}

set_axis <- function(values, units = "Persons") {

  max_n <- max(values)

  output <- list(
    breaks = waiver(),
    labels = function(x) x / 10^6,
    title = paste0("Millions of ", tolower(units))
  )

  if (max_n < 12) {
    output$title <- units
    output$breaks <- c(0, 5, 10)
    output$labels <- waiver()
  }
  if (max_n >= 12 & max_n < 1200) {
    output$title <- units
    output$labels <- waiver()
  }
  if (max_n >= 1200 & max_n < 1.20 * 10^6) {
    output$title <- paste0("Thousands of ", tolower(units))
    output$labels <- function(x) x / 1000
  }
  if (max_n >= 1.20 * 10^6 & max_n < 1.40 * 10^6) {
    output$breaks <- seq(0, 1.25 * 10^6, .25 * 10^6)
    output$labels <- c("0", "0.25", "0.50", "0.75", "1", "1.25")
  }
  if (max_n >= 1.40 * 10^6 & max_n < 1.80 * 10^6) {
    output$breaks <- seq(0, 1.50 * 10^6, .50 * 10^6)
    output$labels <- c("0", "0.5", "1", "1.5")
  }

  return(output)
}

regions <- c(
  "Africa"   = pal("blues", 2),
  "Americas" = pal("greens"),
  "Asia"     = pal("reds", 2),
  "Europe"   = pal("yellows"),
  "Oceania"  = pal("unblues", 2),
  "Unknown"  = pal("grays", 3)
)
