
# Net migration -----------------------------------------------------------

plot_nmigmap <- function(hero,
                         basesize = 8,
                         title = TRUE,
                         caption = FALSE,
                         caption_maxchar = NULL,
                         width = 12,
                         height = 12) {

  # Parameters
  size <- sizer(basesize)
  name_maxchar <- (width / basesize) * 15
  caption_maxchar <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / ggplot2::.pt
  ids <- plot_ider("nmigmap")
  name <- namer(hero, name_maxchar)

  # Data
  border <- rnaturalearth::ne_countries(scale = 50) |>
    dplyr::mutate(
      adm0_a3 = dplyr::case_when(
        .data$adm0_a3 == "KOS" ~ "XKX", .default = .data$adm0_a3)
    ) |>
    dplyr::filter(.data$adm0_a3 == hero)

  border_sub <- system.file(
    "borders",
    "ne_10m_admin_1_states_provinces.shp",
    package = "gdiviz"
  ) |>
    sf::st_read() |>
    dplyr::mutate(
      adm0_a3 = dplyr::case_when(
        .data$adm0_a3 == "KOS" ~ "XKX",
        .default = .data$adm0_a3
      )
    ) |>
    dplyr::filter(.data$adm0_a3 == hero)

  nmig_rast <- system.file("griddata", "nmig.tif", package = "gdiviz") |>
    raster::raster()
  pop_rast <- system.file("griddata", "pop.tif", package = "gdiviz") |>
    raster::raster()

  rast <- 1000 * nmig_rast / pop_rast
  rast <- terra::crop(
    terra::mask(rast, sf::as_Spatial(border)),
    sf::as_Spatial(border)
  )

  df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)

  threshold <- quantile(df$layer, .95)
  magnitude <- log10(threshold) |> floor()
  interval <- (threshold %/% 10^magnitude) * 10^magnitude

  # Chart
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = border, fill = pal("grays", 2), color = NA) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = .data$layer),
      data = df
    ) +
    ggplot2::scale_fill_steps2(
      n.breaks = 12,
      name = "per 1000 people",
      labels = function(x) dplyr::case_when(
        x == 0 | abs(x) == interval ~ prettylabel(x),
        .default = ""
      ),
      show.limits = TRUE,
      limits = c(-threshold, threshold),
      low = pal("reds"),
      high = pal("blues"),
      midpoint = 0
    ) +
    ggplot2::geom_sf(
      data = border,
      fill = NA, color = pal("grays"), linewidth = .25
    ) +
    ggplot2::geom_sf(
      data = border_sub,
      fill = NA, color = pal("grays"), linewidth = .1
    ) +

    # Aesthetics
    apply_theme(type = "map", basesize) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  if (hero == "USA") {
    plot <- plot +
      ggplot2::coord_sf(xlim = c(-125, -66), ylim = c(50, 24), expand = FALSE)
  }

  # Title
  title_auto <- paste0(ids$title, ", 2020")
  if (is.logical(title) & title == TRUE) {
    plot_title <- ggplot2::ggplot() +
      ggplot2::ggtitle(title_auto) +
      apply_theme(type = "map") +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  } else if (is.character(title) | is.numeric(title)) {
    plot_title <- ggplot2::ggplot() +
      ggplot2::ggtitle(title) +
      apply_theme(type = "map") +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  } else {
    plot_title <- NULL
  }

  # Caption
  if (is.character(caption) | is.numeric(caption)) {
    caption_text <- paste0(
      format_source(ids$source, basesize = basesize),
      format_caption(caption, max = caption_maxchar)
    )
  } else {
    caption_text <- format_source(
      ids$source,
      basesize = basesize,
      space_after = FALSE
    )
  }
  plot_caption <- ggplot2::ggplot() +
    ggplot2::labs(caption = caption_text) +
    apply_theme(type = "map") +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  plot <- cowplot::plot_grid(
    plot_title, plot, plot_caption,
    nrow = 3,
    rel_heights = c(.1, 1, .1)
  ) +
    ggplot2::theme(plot.margin = ggplot2::margin(k(5), k(3), k(5), k(3)))

  return(plot)
}

# Population --------------------------------------------------------------

plot_popmap <- function(hero,
                        basesize = 8,
                        title = TRUE,
                        caption = FALSE,
                        caption_maxchar = NULL,
                        width = 12,
                        height = 12) {

  # Parameters
  size <- sizer(basesize)
  name_maxchar <- (width / basesize) * 15
  caption_maxchar <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / ggplot2::.pt
  ids <- plot_ider("popmap")
  name <- namer(hero, name_maxchar)

  # Data
  border <- rnaturalearth::ne_countries(scale = 50) |>
    dplyr::mutate(
      adm0_a3 = dplyr::case_when(
        .data$adm0_a3 == "KOS" ~ "XKX", .default = .data$adm0_a3)
    ) |>
    dplyr::filter(.data$adm0_a3 == hero)

  border_sub <- system.file(
    "borders",
    "ne_10m_admin_1_states_provinces.shp",
    package = "gdiviz"
  ) |>
    sf::st_read() |>
    dplyr::mutate(
      adm0_a3 = dplyr::case_when(
        .data$adm0_a3 == "KOS" ~ "XKX",
        .default = .data$adm0_a3
      )
    ) |>
    dplyr::filter(.data$adm0_a3 == hero)

  pop_rast <- system.file("griddata", "pop.tif", package = "gdiviz") |>
    raster::raster()

  rast <- terra::crop(
    terra::mask(pop_rast, sf::as_Spatial(border)),
    sf::as_Spatial(border)
  )

  df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)

  threshold <- quantile(df$pop, .95)
  magnitude <- log10(threshold) |> floor()
  interval <- (threshold %/% 10^magnitude) * 10^magnitude

  # Chart
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = border, fill = pal("grays", 2), color = NA) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = .data$pop),
      data = df
    ) +
    ggplot2::scale_fill_steps(
      show.limits = TRUE,
      limits = c(0, threshold),
      low = pal("blues", 5),
      high = pal("blues"),
      labels = prettylabel
    ) +
    ggplot2::geom_sf(
      data = border,
      fill = NA, color = pal("grays"), linewidth = .25
    ) +
    ggplot2::geom_sf(
      data = border_sub,
      fill = NA, color = pal("grays"), linewidth = .1
    ) +

    # Aesthetics
    apply_theme(type = "map", basesize) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  if (hero == "USA") {
    plot <- plot +
      ggplot2::coord_sf(xlim = c(-125, -66), ylim = c(50, 24), expand = FALSE)
  }

  # Title
  title_auto <- paste0(ids$title, ", 2020")
  if (is.logical(title) & title == TRUE) {
    plot_title <- ggplot2::ggplot() +
      ggplot2::ggtitle(title_auto) +
      apply_theme(type = "map") +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  } else if (is.character(title) | is.numeric(title)) {
    plot_title <- ggplot2::ggplot() +
      ggplot2::ggtitle(title) +
      apply_theme(type = "map") +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  } else {
    plot_title <- NULL
  }

  # Caption
  if (is.character(caption) | is.numeric(caption)) {
    caption_text <- paste0(
      format_source(ids$source, basesize = basesize),
      format_caption(caption, max = caption_maxchar)
    )
  } else {
    caption_text <- format_source(
      ids$source,
      basesize = basesize,
      space_after = FALSE
    )
  }
  plot_caption <- ggplot2::ggplot() +
    ggplot2::labs(caption = caption_text) +
    apply_theme(type = "map") +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  plot <- cowplot::plot_grid(
    plot_title, plot, plot_caption,
    nrow = 3,
    rel_heights = c(.1, 1, .1)
  ) +
    ggplot2::theme(plot.margin = ggplot2::margin(k(5), k(3), k(5), k(3)))

  return(plot)
}

# GNI per capita ----------------------------------------------------------

plot_incmap <- function(hero,
                        basesize = 8,
                        title = TRUE,
                        caption = FALSE,
                        caption_maxchar = NULL,
                        width = 12,
                        height = 12) {

  # Parameters
  size <- sizer(basesize)
  name_maxchar <- (width / basesize) * 15
  caption_maxchar <- (width / basesize) * 55
  k <- function(factor = 1) factor * size$text / ggplot2::.pt
  ids <- plot_ider("incmap")
  name <- namer(hero, name_maxchar)

  # Data
  border <- rnaturalearth::ne_countries(scale = 50) |>
    dplyr::mutate(
      adm0_a3 = dplyr::case_when(
        .data$adm0_a3 == "KOS" ~ "XKX", .default = .data$adm0_a3)
    ) |>
    dplyr::filter(.data$adm0_a3 == hero)

  border_sub <- system.file(
    "borders",
    "ne_10m_admin_1_states_provinces.shp",
    package = "gdiviz"
  ) |>
    sf::st_read() |>
    dplyr::mutate(
      adm0_a3 = dplyr::case_when(
        .data$adm0_a3 == "KOS" ~ "XKX",
        .default = .data$adm0_a3
      )
    ) |>
    dplyr::filter(.data$adm0_a3 == hero)

  inc_rast <- system.file("griddata", "income.tif", package = "gdiviz") |>
    raster::raster()

  rast <- terra::crop(
    terra::mask(inc_rast, sf::as_Spatial(border)),
    sf::as_Spatial(border)
  )

  df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)

  threshold <- quantile(df$income, .99)
  # d <- ifelse(threshold - min(df$income) < 2000, 1, 0)

  # threshold <- quantile(df$pop, .95)
  # magnitude <- log10(threshold) |> floor()
  # interval <- (threshold %/% 10^magnitude) * 10^magnitude

  # Chart
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = border, fill = pal("grays", 2), color = NA) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(x = .data$x, y = .data$y, fill = .data$income),
      data = df
    ) +
    ggplot2::scale_fill_steps2(
      limits = c(min(df$income), threshold),
      show.limits = TRUE,
      nice.breaks = FALSE,
      low = pal("reds"),
      mid = "white",
      high = pal("blues"),
      midpoint = median(dplyr::filter(df, .data$income < threshold)$income),
      labels = function(x) prettylabel(x, currency = "$")
    ) +
    ggplot2::geom_sf(
      data = border,
      fill = NA, color = pal("grays"), linewidth = .25
    ) +
    ggplot2::geom_sf(
      data = border_sub,
      fill = NA, color = pal("grays"), linewidth = .1
    ) +

    # Aesthetics
    apply_theme(type = "map", basesize) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  if (hero == "USA") {
    plot <- plot +
      ggplot2::coord_sf(xlim = c(-125, -66), ylim = c(50, 24), expand = FALSE)
  }

  # Title
  title_auto <- paste0(ids$title, ", 2020")
  if (is.logical(title) & title == TRUE) {
    plot_title <- ggplot2::ggplot() +
      ggplot2::ggtitle(title_auto) +
      apply_theme(type = "map") +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  } else if (is.character(title) | is.numeric(title)) {
    plot_title <- ggplot2::ggplot() +
      ggplot2::ggtitle(title) +
      apply_theme(type = "map") +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  } else {
    plot_title <- NULL
  }

  # Caption
  if (is.character(caption) | is.numeric(caption)) {
    caption_text <- paste0(
      format_source(ids$source, basesize = basesize),
      format_caption(caption, max = caption_maxchar)
    )
  } else {
    caption_text <- format_source(
      ids$source,
      basesize = basesize,
      space_after = FALSE
    )
  }
  plot_caption <- ggplot2::ggplot() +
    ggplot2::labs(caption = caption_text) +
    apply_theme(type = "map") +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  plot <- cowplot::plot_grid(
    plot_title, plot, plot_caption,
    nrow = 3,
    rel_heights = c(.1, 1, .1)
  ) +
    ggplot2::theme(plot.margin = ggplot2::margin(k(5), k(3), k(5), k(3)))

  return(plot)
}

