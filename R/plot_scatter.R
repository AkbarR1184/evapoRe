#' Scatter Plot of PET vs AET by Method
#'
#' Generates a scatter plot comparing potential evapotranspiration (PET)
#' with actual evapotranspiration (AET), optionally grouped by PET estimation method.
#' Each method (if present) is displayed with a unique color and shape.
#' A 1:1 dashed reference line is added to assess agreement between PET and AET.
#'
#' @param x A `data.table` with columns: `lon`, `lat`, `date`, `pet`, `aet`.
#'   Optionally: `method`.
#' @param x_label Character. X-axis label. Default = `"PET"`.
#' @param y_label Character. Y-axis label. Default = `"ET"`.
#' @param title Optional character. Plot title. Default = `NULL`.
#' @param colors Optional named character vector of hex codes for methods.
#'   If `NULL` and `method` exists, a `scico` palette is used.
#' @param shapes Optional named integer vector of point shapes per method.
#'   If `NULL`, sequential shape codes are assigned.
#' @param font_family Character. Font family. Default = `"Lato"`.
#' @param use_google_font Logical. Load font from Google Fonts. Default = `TRUE`.
#'
#' @return A `ggplot` object.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @import ggplot2
#' @importFrom scico scico
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @export
plot_scatter <- function(
    x,
    x_label = "PET",
    y_label = "ET",
    title = NULL,
    colors = NULL,
    shapes = NULL,
    font_family = "Lato",
    use_google_font = TRUE
) {
  stopifnot(data.table::is.data.table(x))
  
  has_method <- "method" %in% names(x)
  
  if (use_google_font) {
    font_add_google(name = font_family, family = font_family)
    showtext_auto()
  }
  
  if (has_method) {
    method_vals <- unique(x$method)
    n_methods <- length(method_vals)
    
    if (is.null(colors)) {
      colors <- setNames(scico(n_methods, palette = "roma"), method_vals)
    } else {
      if (is.null(names(colors))) {
        colors <- setNames(colors, method_vals)
      }
      missing <- setdiff(method_vals, names(colors))
      if (length(missing) > 0) {
        stop("Missing color assignments for: ", paste(missing, collapse = ", "))
      }
      colors <- colors[method_vals]
    }
    
    if (is.null(shapes)) {
      shapes <- setNames(seq_len(n_methods), method_vals)
    } else {
      if (is.null(names(shapes))) {
        shapes <- setNames(shapes, method_vals)
      }
      missing <- setdiff(method_vals, names(shapes))
      if (length(missing) > 0) {
        stop("Missing shape assignments for: ", paste(missing, collapse = ", "))
      }
      shapes <- shapes[method_vals]
    }
  }
  
  p <- ggplot(x, aes(x = pet, y = aet)) +
    geom_point(
      aes(color = if (has_method) method else NULL,
          shape = if (has_method) method else NULL),
      alpha = 0.3, size = 0.6
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = if (has_method) "Method" else NULL,
      shape = if (has_method) "Method" else NULL
    ) +
    theme_minimal(base_family = font_family) +
    theme(
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 13, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11),
      legend.key.size = unit(0.5, "cm"),
      legend.box = "vertical",
      legend.position = "right",
      plot.margin = margin(12, 12, 12, 12)
    )
  
  if (has_method) {
    p <- p +
      scale_color_manual(values = colors) +
      scale_shape_manual(values = shapes)
  }
  
  return(p)
}
