#' Plot AET > PET Exceedance Frequency
#'
#' Generates a grouped bar plot showing the average spatial frequency
#' at which actual evapotranspiration (AET) exceeds potential evapotranspiration (PET),
#' optionally grouped by dataset, PET method, and time (month or season).
#'
#' @details
#' If the input includes `dataset` (AET data source) and `method` (PET method),
#' bars are grouped by dataset and filled by PET method. The function supports
#' monthly or seasonal faceting. Output is a ggplot2 object.
#'
#' @param x A `data.table` with columns: `lon`, `lat`, `date`, `aet`, `pet`.
#'   Optionally: `dataset` and `method`.
#' @param facet_by Character. One of `"none"`, `"month"`, or `"season"`. Default = `"none"`.
#' @param title Optional character. Title of the plot. If `NULL`, a default is generated.
#' @param palette Optional named character vector of colors for PET methods.
#'   If `NULL`, the `scico` palette `"roma"` is used.
#' @param legend_title Character. Legend title. Default = `"PET Method"`.
#' @param legend_position Character. Legend position. Default = `"right"`.
#' @param x_label Character. X-axis label. Default = `NULL`.
#' @param y_label Character. Y-axis label. Default = `"Exceedance Frequency"`.
#' @param use_google_font Logical. Use Google Fonts. Default = `TRUE`.
#' @param font_family Character. Font family name. Default = `"Lato"`.
#' @param font_size Numeric. Base font size. Default = `13`.
#'
#' @return A `ggplot` object.
#'
#' @import data.table
#' @import ggplot2
#' @importFrom scico scico
#' @importFrom scales percent_format
#' @importFrom lubridate month
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @export
plot_aet_exceedance <- function(
    x,
    facet_by = c("none", "month", "season"),
    title = NULL,
    palette = NULL,
    legend_title = "PET Method",
    legend_position = "right",
    x_label = NULL,
    y_label = "Exceedance Frequency",
    use_google_font = TRUE,
    font_family = "Lato",
    font_size = 13
) {
  facet_by <- match.arg(facet_by)
  
  if (use_google_font) {
    font_add_google(name = font_family, family = font_family)
    showtext_auto()
  }
  
  dt <- copy(x)
  has_method <- "method" %in% names(dt)
  has_dataset <- "dataset" %in% names(dt)
  
  dt[, exceed := as.integer(aet > pet)]
  dt[, month := month(date)]
  
  dt[, season := factor(fcase(
    month %in% c(12, 1, 2), "DJF (Winter)",
    month %in% c(3, 4, 5), "MAM (Spring)",
    month %in% c(6, 7, 8), "JJA (Summer)",
    month %in% c(9, 10, 11), "SON (Autumn)"
  ), levels = c("DJF (Winter)", "MAM (Spring)", "JJA (Summer)", "SON (Autumn)"))]
  
  group_vars <- c("lon", "lat")
  if (facet_by == "month") group_vars <- c(group_vars, "month")
  if (facet_by == "season") group_vars <- c(group_vars, "season")
  if (has_dataset) group_vars <- c(group_vars, "dataset")
  if (has_method) group_vars <- c(group_vars, "method")
  
  freq_dt <- dt[, .(freq_pct = mean(exceed, na.rm = TRUE) * 100), by = group_vars]
  plot_vars <- setdiff(group_vars, c("lon", "lat"))
  plot_dt <- freq_dt[, .(mean_freq = mean(freq_pct, na.rm = TRUE)), by = plot_vars]
  
  if (!has_dataset) plot_dt[, dataset := "AET"]
  if (!has_method) plot_dt[, method := "PET"]
  
  plot_dt[, dataset := factor(dataset, levels = unique(dataset))]
  plot_dt[, method := factor(method, levels = unique(method))]
  
  method_names <- levels(plot_dt$method)
  
  if (is.null(palette)) {
    palette <- setNames(scico(length(method_names), palette = "roma"), method_names)
  } else {
    if (is.null(names(palette))) {
      palette <- setNames(palette, method_names)
    }
    missing <- setdiff(method_names, names(palette))
    if (length(missing) > 0) {
      stop("Missing color assignments for: ", paste(missing, collapse = ", "))
    }
    palette <- palette[method_names]
  }
  
  if (is.null(title)) {
    title <- "Frequency of AET > PET"
    if (facet_by == "month") title <- paste(title, "(by Month)")
    if (facet_by == "season") title <- paste(title, "(by Season)")
  }
  
  dataset_angle <- if (length(unique(plot_dt$dataset)) > 3) 45 else 0
  
  p <- ggplot(plot_dt, aes(x = dataset, y = mean_freq / 100, fill = method)) +
    geom_col(position = position_dodge(0.7), width = 0.6) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1)
    ) +
    scale_fill_manual(values = palette) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = legend_title
    ) +
    theme_minimal(base_family = font_family) +
    theme(
      axis.text = element_text(size = font_size, color = "black"),
      axis.title = element_text(size = font_size, face = "bold"),
      plot.title = element_text(size = font_size, face = "bold", hjust = 0.5),
      legend.title = element_text(size = font_size, face = "bold"),
      legend.text = element_text(size = font_size),
      strip.text = element_text(size = font_size, hjust = 0, face = "bold"),
      strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(16, 16, 16, 16),
      axis.text.x = element_text(angle = dataset_angle, hjust = ifelse(dataset_angle == 0, 0.5, 1)),
      legend.position = legend_position
    )
  
  if (facet_by == "month") {
    plot_dt[, month := factor(month, levels = 1:12, labels = month.name)]
    p <- p + facet_wrap(~month, ncol = 4)
  } else if (facet_by == "season") {
    p <- p + facet_wrap(~season)
  }
  
  return(p)
}
