#' Plot Extreme Evaporation Events
#'
#' Convenient and aesthetic visualization of extreme evaporation events.
#'
#' @details
#' `x` must be a `data.table` with the following columns:
#' - `"date"`: Date of observation  
#' - `"value"`: Evaporation value  
#' - `"extreme_id"`: Identifier for extreme evaporation events (can be `NA`)  
#' - `"above_low_thres_id"`: Identifier for values above a lower threshold  
#' - `"event_id"`: Identifier for broader event periods (can be `NA`)  
#'
#' `exeve_pal` is a character vector of three colors used to style:
#' (1) extreme points, (2) event points, and (3) event markers on the x-axis.
#'
#' `shapes` is a numeric vector of three shape codes corresponding to:
#' (1) extreme points, (2) event points, and (3) event markers.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @import ggplot2
#' @param x A data.table (see details)
#' @param exeve_pal Character vector of 3 colors: (1) extreme, (2) event, (3) marker
#' @param title Optional character string; plot title
#' @param shapes Numeric vector of 3 shape codes (see details)
#' @return A ggplot object
#' @export
#' @examples
#' \donttest{
#' dt <- data.table(
#'   date = seq(as.Date("2020-01-01"), as.Date("2020-03-01"), by = "day"),
#'   value = runif(61, 0, 5),
#'   extreme_id = c(rep(NA, 30), 1, NA, NA, 2, rep(NA, 26), 3),
#'   above_low_thres_id = NA,
#'   event_id = c(rep(NA, 25), rep(1, 10), rep(NA, 26))
#' )
#'
#' plot_exeve(dt, exeve_pal = c("#4D648D", "#337BAE", "#a9cce0"))
#' }
plot_exeve <- function(
    x,
    exeve_pal = c("#4D648D", "#337BAE", "#a9cce0"),
    title = NULL,
    shapes = c(0, 16, 15)
) {
  stopifnot("data.table" %in% class(x))
  if (length(exeve_pal) != 3) stop("exeve_pal must have 3 colors.")
  if (length(shapes) != 3) stop("shapes must have 3 numeric values.")
  
  required_cols <- c("date", "value", "extreme_id", "above_low_thres_id", "event_id")
  if (!all(required_cols %in% names(x))) {
    stop("Missing columns: ", paste(setdiff(required_cols, names(x)), collapse = ", "))
  }
  
  ggplot(x) +
    geom_line(aes(date, value)) +
    
    geom_point(
      data = x[!is.na(extreme_id)],
      aes(date, value),
      col = exeve_pal[1],
      size = 4,
      shape = shapes[1]
    ) +
    
    geom_point(
      data = x[!is.na(event_id)],
      aes(date, value),
      col = exeve_pal[2],
      size = 3,
      shape = shapes[2],
      alpha = 0.5
    ) +
    
    geom_point(
      data = x[!is.na(event_id)],
      aes(date, y = 0.6),
      col = exeve_pal[3],
      size = 2,
      shape = shapes[3]
    ) +
    
    scale_x_date(
      expand = c(0, 0),
      date_breaks = "1 month",
      date_labels = "%b",
      minor_breaks = NULL
    ) +
    xlab("Time (day)") +
    ylab("Evaporation (mm/day)") +
    theme_linedraw() +
    theme(plot.margin = unit(c(0.5, 1, 0.5, 1), "cm")) +
    ggtitle(title)
}
