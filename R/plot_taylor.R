#' Plot Taylor Diagram for Model Evaluation
#'
#' Generate a Taylor diagram from tidy climate model and observation data with full
#' support for grouping, seasonal faceting, normalization, and spatial site selection.
#'
#' @details
#' This function wraps openair's TaylorDiagram to provide a streamlined interface for
#' plotting model evaluation metrics. It accepts a tidy data.table and supports:
#'
#' - Grouping by model name or other attributes via `group`
#' - Faceting by time, season, or spatial location via `type`
#' - Custom color palettes and point shapes
#' - Optional filtering by longitude and latitude range
#'
#' Input `x` must be a long-format `data.table` with at least: `lon`, `lat`, `date`, `value`, and `dataset`.
#' It should include both observed and model values in the same column (`value`), distinguished by `dataset`.
#'
#' @param x A `data.table` with columns: `lon`, `lat`, `date`, `value`, `dataset`, and optionally `source`.
#' @param type Character or character vector for faceting, e.g. "season", "site". Default is "default".
#' @param group Character or character vector for grouping. Default is "dataset".
#' @param palette Character. A scico palette name or color vector. Default is "roma".
#' @param shapes Integer vector of point shapes. Default is 20 (auto).
#' @param normalise Logical. If TRUE, normalize SD and RMSD. Default is TRUE.
#' @param lon_range Numeric vector of length 2 for filtering by longitude. Default is NULL.
#' @param lat_range Numeric vector of length 2 for filtering by latitude. Default is NULL.
#' @param ... Additional arguments passed to `TaylorDiagram()`.
#'
#' @return An openair object (S3), including the Taylor plot and processed data.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom openair TaylorDiagram
#' @importFrom scico scico
#' @export
plot_taylor <- function(x,
                        type       = "default",
                        group      = "dataset",
                        palette    = "roma",
                        shapes     = 20,
                        normalise  = TRUE,
                        lon_range  = NULL,
                        lat_range  = NULL,
                        ...) {
  
  x <- data.table::copy(x)
  
  stopifnot(is.data.table(x))
  
  required_cols <- c("lon", "lat", "date", "value", "dataset")
  if (!all(required_cols %in% names(x))) {
    stop("Input must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  if (!is.null(lon_range) && !is.null(lat_range)) {
    if (length(lon_range) != 2 || length(lat_range) != 2) {
      stop("`lon_range` and `lat_range` must be numeric vectors of length 2.")
    }
    x <- x[lon >= lon_range[1] & lon <= lon_range[2] &
             lat >= lat_range[1] & lat <= lat_range[2]]
    if (nrow(x) == 0) stop("No data found in the specified lon/lat range.")
  }
  
  if (!"site" %in% names(x) && all(c("lon", "lat") %in% names(x))) {
    x[, site := paste0("(", lon, ", ", lat, ")")]
  }
  
  if (!"id" %in% names(x) && all(c("lon", "lat") %in% names(x))) {
    x[, id := .GRP, by = .(lon, lat)]
  }
  
  if (!"season" %in% names(x)) {
    x[, season := factor(
      fifelse(month(date) %in% c(3, 4, 5), "Spring (MAM)",
              fifelse(month(date) %in% c(6, 7, 8), "Summer (JJA)",
                      fifelse(month(date) %in% c(9,10,11), "Autumn (SON)", "Winter (DJF)"))),
      levels = c("Spring (MAM)", "Summer (JJA)", "Autumn (SON)", "Winter (DJF)")
    )]
  }
  
  if (!"month" %in% names(x)) {
    x[, month := factor(month(date), levels = 1:12, labels = month.abb)]
  }
  
  wide_dt <- dcast(x, lon + lat + date + site + id + season + month ~ dataset,
                   value.var = "value")
  wide_dt <- as.data.table(wide_dt)
  
  if (!"observation" %in% names(wide_dt)) {
    stop("Observation data with dataset == 'observation' must be present.")
  }
  
  model_cols <- setdiff(names(wide_dt),
                        c("lon","lat","date","site","id","season","month","observation"))
  
  dt_list <- lapply(model_cols, function(mod_name) {
    temp <- wide_dt[!is.na(wide_dt[[mod_name]]) & !is.na(observation)]
    data.table(
      lon     = temp$lon,
      lat     = temp$lat,
      date    = temp$date,
      obs     = temp$observation,
      mod     = temp[[mod_name]],
      dataset = mod_name,
      site    = temp$site,
      id      = temp$id,
      season  = temp$season,
      month   = temp$month
    )
  })
  
  combined <- rbindlist(dt_list, use.names = TRUE)
  
  group_col <- if (length(group) == 1) combined[[group]] else interaction(combined[, ..group])
  n_groups  <- length(unique(group_col))
  
  if (length(palette) == 1 && is.character(palette)) {
    colors <- scico(n_groups, palette = palette)
  } else if (length(palette) >= n_groups) {
    colors <- palette
  } else {
    stop("Palette must be a scico name or a vector of at least `n_groups` colors.")
  }
  
  TaylorDiagram(
    mydata     = combined,
    obs        = "obs",
    mod        = "mod",
    group      = group,
    type       = type,
    cols       = colors,
    pch        = shapes,
    normalise  = normalise,
    ylab       = "Standard Deviation (Normalized)",
    ...
  )
}
