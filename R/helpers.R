
# Helper functions for country brief generation

library(tidyverse)
library(ggrepel)

scale_labels <- function(N) {
  max <- max(N, na.rm = TRUE)
  if (max < 1200) {
    n <- pretty(N, d = 1, dotzero = FALSE)
  }
  if ((max >= 1200) & (max < 1200000)) {
    n <- pretty(N / 10^3, d = 1, dotzero = FALSE)
  }
  if (max >= 1200000) {
    n <- pretty(N / 10^6, d = 1, dotzero = FALSE)
  }
  return(n)
}

scale_title <- function(data) {
  if (max(data$v) < 1200) {
    title <- "persons"
  }
  if ((max(data$v) >= 1200) & (max(data$v) < 1200000)) {
    title <- "thousands of persons"
  }
  if (max(data$v) >= 1200000) {
    title <- "millions of persons"
  }
  return(title)
}

pivot_years <- function(data) {
  pivot_longer(
    data,
    cols = starts_with("Y"), 
    names_to = "Year", 
    names_prefix = "Y", 
    values_to = "Values"
  )
}

span <- function(data) c(min(data$Year), max(data$Year)) |> as.numeric()
span2 <- function(data) c(min(data$t), max(data$t)) |> as.numeric()
range <- function(data, collapse = "–") str_flatten(span(data), collapse)
range2 <- function(data, collapse = "–") str_flatten(span2(data), collapse)

get_avgs <- function(iso, data, world = TRUE) {
  
  # Duplicate last year of migration stock data
  stocks <- read_csv("Data/Stocks_total.csv")
  latest_year <- colnames(stocks)[ncol(stocks)]
  ahead_year <- paste0("Y", as.numeric(substr(latest_year, 2, 5)) + 1)
  stocks[ahead_year] <- stocks[latest_year]
  stocks <- pivot_years(stocks)
  
  weights <- bind_rows(
    filter(stocks, Origin == iso) |> 
      mutate(Share = Values / sum(Values), Type = "Destin", .by = Year) |> 
      select(Type, Year, CountryCode = Destin, Share),
    filter(stocks, Destin == iso) |> 
      mutate(Share = Values / sum(Values), Type = "Origin", .by = Year) |> 
      select(Type, Year, CountryCode = Origin, Share)
  )
  
  destin <- left_join(
    data, 
    filter(weights, Type == "Destin"), by = c("CountryCode", "Year")
  ) |> 
    mutate(Weight = Values * Share) |> 
    drop_na() |> 
    summarise(Weight = sum(Weight), .by = Year) |> 
    mutate(Series = str_glue("{iso} emigrant-destination countries (avg)")) |> 
    select(Year, Series, Values = Weight)
  
  origin <- left_join(
    data, 
    filter(weights, Type == "Origin"), by = c("CountryCode", "Year")
  ) |> 
    mutate(Weight = Values * Share) |> 
    drop_na() |> 
    summarise(Weight = sum(Weight), .by = Year) |> 
    mutate(Series = str_glue("{iso} immigrant-origin countries (avg)")) |> 
    select(Year, Series, Values = Weight)
  
  isoseries <- filter(data, CountryCode == iso) |> rename(Series = CountryCode)
  
  data_sum <- bind_rows(isoseries, destin, origin) |> 
    mutate(Series = fct_relevel(Series, iso, after = 0))
  
  if (world) {
    world_median <- summarise(data, Values = median(Values, na.rm = TRUE), .by = Year) |> 
      mutate(Series = "World median") |> 
      select(Year, Series, Values)
    data_sum <- bind_rows(data_sum, world_median) |> 
      mutate(Series = fct_relevel(Series, "World median", after = 1))
  }
  
  return(data_sum)  
}

get_last <- function(data, group, group2 = NULL) {
  data1 <- data |> select(t, {{ group }}, {{ group2 }}, n) |> 
    drop_na() |> 
    mutate(Max = max(t), .by = {{ group }})
  left_join(data, data1) |> 
    filter(t == Max) |> 
    suppressMessages()
}

add_labels2 <- function(
    df, 
    group, 
    group2 = NULL,
    basesize = size,
    pct = FALSE, 
    d = 0,
    currency = NULL, 
    worldmedian = TRUE,
    colorscale) {
  
  labels <- list(
    geom_label_repel(
      mapping = aes(
        x = factor(t), y = n, 
        fill = {{ group }}, 
        label = prettylabel(n, d = d, pct = pct, currency = currency)
      ), 
      data = get_last({{ df }}, {{ group }}, {{ group2 }}) |> 
        filter({{ group }} != "World median"),
      color = "white", 
      size = size$text / .pt, 
      fontface = "bold", 
      family = "Gill Sans Nova",
      hjust = .5, vjust = .5, 
      box.padding = unit(.14, "lines"),
      label.padding = unit(.15, "lines"),
      label.r = unit(.05, "lines"),
      label.size = .1,
      segment.colour = pal("blues", 3),
      point.size = NA,
      show.legend = FALSE,
      direction = "y"
    ),
    scale_fill_manual(values = colorscale)
  )
  
  if (worldmedian) {
    labels <- c(
      labels, 
      list(scale_linetype_manual(values = c("solid", "22", "solid", "solid")))
    )
  } else {
    labels <- c(labels, list(scale_linetype_manual(values = "solid")))
  }
  
  return(labels)
}

plot_label <- function(plot, label, span = 2, h = .06) {
  
  box <- grid::rectGrob(gp = grid::gpar(fill = pal("blues", 4), col = NA))
  
  width <- .025 + .015 * nchar(label)
  height <- h
  if (span == 1) width <- width / 2
  if (span == 3) width <- width * 1.5
  
  ggdraw(plot) + 
    draw_grob(
      box, 
      x = 0, 
      y = 1, 
      vjust = 1, 
      width = width, 
      height = height
    ) + 
    draw_label(
      label, 
      x = width / 2, 
      y = 1 - height / 2,
      hjust = .5, 
      vjust = .5, 
      size = size$text, 
      fontface = "bold", 
      fontfamily = "Gill Sans Nova", 
      color = "white"
    )
}


kosovo_disclaimer <- function(hero) {
  
  text <- paste(
    "References to Kosovo shall be understood to be in the context of United",
    "Nations Security Council resolution 1244 (1999)."
  )
  
  if (hero == "XKX") {
    
    return(text)
  
  } else {
    
    return_text <- ""
    
    t1 <- max(undesa_stocks$t)
    
    df <- bind_rows(
      undesa_stocks |> 
        left_join(countrynames, by = c("geo" = "iso3")) |> 
        filter(t == t1 & from == hero) |> 
        summarise(n = sum(n), .by = c(from, geo, region, t)) |> 
        mutate(panel = "destin") |> 
        rename(nat = geo, hero = from),
      undesa_stocks |> 
        left_join(countrynames, by = c("from" = "iso3")) |> 
        filter(t == t1 & geo == hero) |> 
        summarise(n = sum(n), .by = c(geo, from, region, t)) |> 
        mutate(
          region = case_when(is.na(region) ~ "Unknown", .default = region),
          panel = "origin"
        ) |> 
        rename(hero = geo, nat = from)
    )
    
    if (nrow(df) > 0) {
      
      df <- df |> 
        group_by(panel) |> 
        arrange(desc(n), .by_group = TRUE) |> 
        ungroup()
      
      destin <- filter(df, panel == "destin") |> slice_head(n = 5) |> pull(nat)
      origin <- filter(df, panel == "origin" & nat != "OOO") |> 
        slice_head(n = 5) |> pull(nat)
      
      if ("XKX" %in% c(destin, origin)) return_text <- text
    }
    
    return(return_text)
  }
}




# c(
#   "Flood" = pal("blues", 2),
#   "Storm" = pal("greens"),
#   "Wildfire" = pal("reds", 2),
#   "Drought" = pal("yellows"),
#   
#   "Earthquake" = pal("blues", 2),
#   "Volcanic activity" = pal("blues", 2),
#   "Mass movement" = pal("blues", 2),
#   "Extreme temperature" = pal("blues", 2),
#   "Wave action" = pal("blues", 2),
# )
