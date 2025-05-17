#' Plot Time Series
#'
#' Generate a line plot from tidy time-series data with optional customization for
#' multi-series datasets, automatic frequency detection, and optional median line.
#'
#' @details
#' The function supports plotting of daily, monthly, or yearly time series. It detects
#' time frequency based on the first two dates of one dataset. If the year difference is 1,
#' the data is treated as yearly; otherwise, it uses monthly formatting ("%Y-%m").
#' Optionally, a black dashed median line can be plotted when multiple series exist.
#'
#' @param x A `data.table` or `data.frame` with columns: "date", "value", "name".
#' @param type_analysis Character, either "single" or "multi". Determines if one or multiple
#' series are being plotted.
#' @param show_median Logical. If TRUE (and type_analysis is "multi"), a dashed black
#' median line across series is added. Default is FALSE.
#' @param colors Optional vector of colors. Can be named or unnamed.
#' @param title Character. Plot title. Default is "Time Series Plot".
#' @param x_label Character. X-axis label. Default is "Time (year)".
#' @param y_label Character. Y-axis label. Default is "Value".
#' @param x_breaks Character. ggplot2-compatible time break (e.g., "1 year", "3 months").
#' @param x_text Logical. Show x-axis text? Default is TRUE.
#' @param y_text Logical. Show y-axis text? Default is TRUE.
#' @param legend Logical. Show legend? Default is TRUE.
#'
#' @return A ggplot object
#'
#'@rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom lubridate year
#' @import ggplot2
#' @export
plot_ts <- function(
    x,
    type_analysis = c("single", "multi"),
    show_median = FALSE,
    colors = NULL,
    title = "Time Series Plot",
    x_label = "Time (year)",
    y_label = "Value",
    x_breaks = NULL,
    x_text = TRUE,
    y_text = TRUE,
    legend = TRUE
) {
  type_analysis <- match.arg(type_analysis)
  x <- as.data.table(x)
  x[, date := as.IDate(date)]
  
  dataset_names <- unique(x$name)
  n_datasets <- length(dataset_names)
  x_sample <- x[name == dataset_names[1]][order(date)]
  first_date <- x_sample$date[1]
  second_date <- x_sample$date[2]
  year_diff <- year(second_date) - year(first_date)
  if (year_diff == 1) {
    label_format <- "%Y"
    default_breaks <- "1 year"
  } else {
    label_format <- "%Y-%m"
    default_breaks <- "1 month"
  }
  
  breaks_used <- if (is.null(x_breaks)) default_breaks else x_breaks
  
  default_palette <- c(
    "#3283FE", "#FEAF16", "#1CFFCE", "#B00068", "#2ED9FF", "#5A5156",
             "#F6222E", "#16FF32", "#AA0DFE", "#C4451C", "#FE00FA", "#325A9B",
             "#DEA0FD", "#F8A19F", "#90AD1C", "#85660D", "#F6222E", "#FF6600"
  )
  
  if (is.null(colors)) {
    colors <- setNames(default_palette[seq_len(n_datasets)], dataset_names)
  } else if (is.null(names(colors))) {
    colors <- setNames(colors, dataset_names)
  } else {
    if (!all(dataset_names %in% names(colors))) {
      missing <- setdiff(dataset_names, names(colors))
      stop("Missing color assignments for: ", paste(missing, collapse = ", "))
    }
    colors <- colors[dataset_names]
  }
  
  if (type_analysis == "multi" && show_median) {
    med <- x[, .(value = median(value, na.rm = TRUE)), by = date]
    med[, name := "Median"]
    x <- rbind(x, med, fill = TRUE)
  }
  
  ggplot(x, aes(x = date, y = value, color = name)) +
    geom_line(data = x[name != "Median"], size = 0.8) +
    geom_line(
      data = x[name == "Median"],
      color = "black", linetype = "dashed", size = 1, show.legend = FALSE
    ) +
    scale_color_manual(values = colors, name = "Dataset") +
    scale_x_date(date_labels = label_format, date_breaks = breaks_used) +
    labs(title = title, x = x_label, y = y_label) +
    theme_bw() +
    theme(
      legend.position = if (legend) "right" else "none",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.text.x = if (x_text) element_text(size = 12) else element_blank(),
      axis.ticks.x = element_line(),
      axis.text.y = if (y_text) element_text(size = 12) else element_blank(),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, hjust = 0.5)
    )
}
