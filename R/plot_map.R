#' Plot Spatial Map of a Climate Variable
#'
#' Create a raster map of spatial values (e.g., trend, evaporation, precipitation, and drought index)
#' with full control over color scale, palette, legend, axis formatting, and font.
#'
#' @param x A data.table with columns: lon, lat, value.
#' @param value_limits Numeric vector of length 2. Color scale limits. Default is c(-5, 5).
#' @param palette Character (palette name) or character vector (custom colors). Default is "vik" from `scico`.
#' @param legend_title Character. Title for the legend. Default is "Slope (mm/year)".
#' @param font_family Character. Font family. Default is "Lato".
#' @param use_google_font Logical. If TRUE, loads font from Google Fonts. Default is TRUE.
#' @param x_label Character. Label for x-axis. Default is "Longitude".
#' @param y_label Character. Label for y-axis. Default is "Latitude".
#' @param x_breaks Numeric vector. Breaks for x-axis.
#' @param y_breaks Numeric vector. Breaks for y-axis.
#' @param legend_position Character. Position of legend ("right", "bottom", "top", "left"). Default is "right".
#' @param show_legend Logical. Whether to display the legend. Default is TRUE.
#' @param na_color Character. Color for NA values. Default is "white".
#' @param reverse_palette Logical. If TRUE, reverses the color palette. Default is FALSE.
#' @param add_borders Logical. Whether to add country borders. Default is TRUE.
#' @param border_color Character. Border color. Default is "gray10".
#' @param border_size Numeric. Border thickness. Default is 0.2.
#'
#' @return A ggplot2 object.
#'
#' @import ggplot2
#' @importFrom scico scale_fill_scico scico_palette_names
#' @importFrom viridisLite viridis
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @export
plot_map <- function(x,
                     value_limits = c(-5, 5),
                     palette = "vik",
                     legend_title = "Slope (mm/year)",
                     font_family = "Lato",
                     use_google_font = TRUE,
                     x_label = "Longitude",
                     y_label = "Latitude",
                     x_breaks = seq(-180, 180, by = 30),
                     y_breaks = seq(-90, 90, by = 30),
                     legend_position = "right",
                     show_legend = TRUE,
                     na_color = "white",
                     reverse_palette = FALSE,
                     add_borders = TRUE,
                     border_color = "gray10",
                     border_size = 0.2) {
  
  if (use_google_font) {
    if (!font_family %in% sysfonts::font_families()) {
      sysfonts::font_add_google(name = font_family, family = font_family)
    }
    showtext::showtext_auto()
  }
  
  x_range <- range(x$lon, na.rm = TRUE)
  y_range <- range(x$lat, na.rm = TRUE)
  
  if (is.character(palette) && length(palette) == 1) {
    if (palette %in% scico::scico_palette_names()) {
      fill_scale <- scico::scale_fill_scico(
        palette = palette,
        limits = value_limits,
        oob = scales::oob_squish,
        name = legend_title,
        direction = ifelse(reverse_palette, -1, 1),
        na.value = na_color
      )
    } else if (palette %in% c("viridis", "magma", "plasma", "inferno", "cividis", "turbo")) {
      fill_scale <- ggplot2::scale_fill_viridis_c(
        option = palette,
        limits = value_limits,
        oob = scales::oob_squish,
        name = legend_title,
        direction = ifelse(reverse_palette, -1, 1),
        na.value = na_color
      )
    } else {
      warning("Unknown palette. Using default 'vik' from scico.")
      fill_scale <- scico::scale_fill_scico(
        palette = "vik",
        limits = value_limits,
        oob = scales::oob_squish,
        name = legend_title,
        direction = ifelse(reverse_palette, -1, 1),
        na.value = na_color
      )
    }
  } else if (is.character(palette) && length(palette) > 1) {
    fill_scale <- ggplot2::scale_fill_gradientn(
      colors = if (reverse_palette) rev(palette) else palette,
      limits = value_limits,
      oob = scales::oob_squish,
      name = legend_title,
      na.value = na_color
    )
  } else {
    stop("Invalid `palette`. Must be a palette name or a vector of colors.")
  }
  
  p <- ggplot() +
    geom_tile(data = x, aes(x = lon, y = lat, fill = value)) +
    fill_scale
  
  if (add_borders) {
    p <- p + borders("world", colour = border_color, size = border_size)
  }
  
  p <- p +
    coord_cartesian(xlim = x_range, ylim = y_range, expand = FALSE) +
    scale_x_continuous(
      name = x_label,
      breaks = x_breaks,
      labels = function(x) paste0(x, "°")
    ) +
    scale_y_continuous(
      name = y_label,
      breaks = y_breaks,
      labels = function(y) paste0(y, "°")
    ) +
    theme_minimal(base_family = font_family) +
    theme(
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, face = "bold", color = "black"),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 11),
      plot.margin = margin(12, 12, 12, 12),
      legend.position = if (show_legend) legend_position else "none"
    )
  
  return(p)
}