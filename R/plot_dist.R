#' Plot Distribution of a Climate Variable
#'
#' Generate a distribution plot from tidy climate data with optional customization for
#' histograms, density curves, boxplots, and ECDFs.
#'
#' @details
#' This function supports multiple representations of climate variable distributions:
#' - "histogram": shows raw value frequency,
#' - "density": shows a smoothed estimate of the value distribution,
#' - "boxplot": summarizes data spread and central tendency per group,
#' - "ecdf": shows cumulative probability of observed values.
#'
#' Users can combine display options (e.g., c("histogram", "density")). The function
#' supports optional color grouping via fill_var, and returns a ggplot2 object.
#' 
#' @param x A data.table with at least date (Date) and value (numeric) columns.
#' @param colors Optional named vector of colors for each group. If NULL, a default palette is used.
#' @param bins Integer. Number of bins for histogram. Default is 30.
#' @param show_legend Logical. Show legend? Default is FALSE.
#' @param fill_var Character. Column name for color grouping. Default is "dataset".
#' @param display Character vector. One or more of "histogram", "density", "boxplot", "ecdf".
#' Default is c("histogram", "density").
#' @param legend_title Character. Legend title. Default is the value of fill_var.
#' @param x_label Character. X-axis label. Default is "Value".
#' @param y_label Character. Y-axis label. Default is "Density".
#' @param title Character. Optional plot title.
#' @param subtitle Character. Optional subtitle.
#' @param caption Character. Optional caption.
#' @param use_google_font Logical. If TRUE, font_family is loaded from Google Fonts. Default is TRUE.
#' @param font_family Character. Font family name. Default is "Lato". Should match a Google Font name if use_google_font = TRUE.
#'
#' @return A ggplot2 object.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @import ggplot2
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @export
plot_dist <- function(x,
                      colors = NULL,
                      bins = 30,
                      show_legend = FALSE,
                      fill_var = "dataset",
                      display = c("histogram", "density"),
                      legend_title = NULL,
                      x_label = "Value",
                      y_label = "Density",
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL,
                      use_google_font = TRUE,
                      font_family = "Lato") {
  
  display <- match.arg(display, choices = c("histogram", "density", "boxplot", "ecdf"), several.ok = TRUE)
  
  if (use_google_font) {
    sysfonts::font_add_google(name = font_family, family = font_family)
    showtext::showtext_auto()
  }
  
  default_palette <- c(
    "#3283FE", "#FEAF16", "#1CFFCE", "#B00068", "#2ED9FF", "#5A5156",
             "#F6222E", "#16FF32", "#AA0DFE", "#C4451C", "#FE00FA", "#325A9B",
             "#DEA0FD", "#F8A19F", "#90AD1C", "#85660D", "#F6222E", "#FF6600"
  )
  
  has_fill <- fill_var %in% names(x)
  fill_values <- if (has_fill) unique(x[[fill_var]]) else NULL
  if (is.null(legend_title)) legend_title <- fill_var
  
  if (has_fill) {
    if (is.null(colors)) {
      colors <- setNames(default_palette[seq_along(fill_values)], fill_values)
    } else if (is.null(names(colors))) {
      colors <- setNames(colors, fill_values)
    } else {
      colors <- colors[fill_values]
    }
  }
  
  p <- ggplot(x, aes(x = value))
  
  if ("histogram" %in% display) {
    p <- p +
      geom_histogram(
        aes(y = ..density.., fill = .data[[fill_var]]),
        bins = bins,
        color = "black",
        linewidth = 0.4,
        alpha = 0.5
      )
  }
  
  if ("density" %in% display) {
    p <- p +
      geom_density(
        aes(color = .data[[fill_var]], fill = NULL),
        size = 1.0
      )
  }
  
  if ("boxplot" %in% display) {
    p <- p +
      geom_boxplot(
        aes(y = value, x = .data[[fill_var]], fill = .data[[fill_var]]),
        width = 0.5,
        outlier.shape = NA,
        alpha = 0.6
      ) +
      coord_flip()
  }
  
  if ("ecdf" %in% display) {
    p <- p +
      stat_ecdf(
        aes(color = .data[[fill_var]]),
        geom = "step",
        linewidth = 1
      )
  }
  
  p <- p +
    labs(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme_minimal(base_family = font_family) +
    theme(
      axis.text = element_text(size = 17, color = "black"),
      axis.title = element_text(size = 22, face = "bold"),
      axis.ticks = element_line(size = 0.6),
      axis.ticks.length = unit(0.22, "cm"),
      axis.line = element_line(size = 0.8, color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(size = 0.5, color = "grey85"),
      plot.margin = margin(16, 16, 16, 16),
      legend.position = if (show_legend) "right" else "none",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 18),
      plot.caption = element_text(size = 14, hjust = 1)
    )
  
  if (has_fill && !is.null(colors)) {
    if (any(display %in% c("histogram", "boxplot"))) {
      p <- p + scale_fill_manual(values = colors, name = legend_title)
    }
    if (any(display %in% c("density", "ecdf"))) {
      p <- p + scale_color_manual(values = colors, name = legend_title)
    }
  }
  
  return(p)
}
