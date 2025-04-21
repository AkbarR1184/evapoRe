#' Convert latent heat flux (LE) to evapotranspiration (ET)
#'
#' The function \code{le_2_et} computes ET from latent heat flux.
#'
#' @details
#' Provide inputs in the following formats:
#' * For \code{Raster*} objects supply raster objects or file paths for
#'   `le` (MJ m-2 day-1) and, optionally, `tavg` (°C) when a temperature‑
#'   dependent latent heat of vaporization is desired.
#' * For a \code{data.table} pass a single table with the columns:
#'   "lon", "lat", "date", "le" and, optionally, "tavg".
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick calc getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @param le   Raster* object or file path; latent heat flux (MJ m-2 day-1)
#' @param tavg Optional Raster* object or file path; mean air temperature (°C)
#' @param x    A \code{data.table} with columns "lon", "lat", "date", "le",
#'             and optionally "tavg".
#'
#' @return RasterBrick or data.table of ET values (mm/day)
#'
#' @examples
#' \donttest{
#' # Raster input
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   le_r <- raster::raster(nrows = 10, ncols = 10)
#'   raster::values(le_r) <- runif(raster::ncell(le_r), 10, 15)  
#'   tavg_r <- le_r + 5                                         
#'   et_raster <- le_2_et(le = le_r, tavg = tavg_r)
#' }
#'
#' # File path input
#' le_path <- file.path(tempdir(), "le_flux.nc")
#' if (file.exists(le_path)) {
#'   et_file <- le_2_et(le_path)
#' }
#'
#' # data.table input
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   dt <- data.table::data.table(
#'     lon = c(10.0, 10.5),
#'     lat = c(45.0, 45.5),
#'     date = as.Date(c("2025-01-01", "2025-01-02")),
#'     le = c(12.3, 13.1),   # MJ m-2 day-1
#'     tavg = c(20, 21)      # °C
#'   )
#'   et_dt <- le_2_et(x = dt)
#' }
#' }
#'
#' @export
setGeneric("le_2_et", function(le, tavg = NULL, x = NULL) {
  standardGeneric("le_2_et")
})

#' @rdname le_2_et
#' @export
setMethod("le_2_et",
          signature(le = "missing", tavg = "missing"),
          function(x) {
            if ("tavg" %in% names(x)) {
              x[, lambda := 2.501 - 0.002361 * tavg]
            } else {
              x[, lambda := 2.45]
            }
            x[, et := le / lambda]
            x[, et := fifelse(et < 0, NA_real_, et)]
            x[, .(lon, lat, date, value = et)]
          })

#' @rdname le_2_et
#' @export
setMethod("le_2_et",
          signature(le = "Raster"),
          function(le, tavg = NULL, x = NULL) {
            no_cores <- detectCores() - 1
            if (no_cores < 1 || is.na(no_cores)) no_cores <- 1
            registerDoParallel(cores = no_cores)
            
            dummie <- foreach(layer_index = 1:nlayers(le)) %dopar% {
              dummie_le <- le[[layer_index]]
              if (is.null(tavg)) {
                lambda <- 2.45
              } else {
                dummie_ta <- tavg[[layer_index]]
                lambda <- 2.501 - 0.002361 * dummie_ta
              }
              dummie_et <- dummie_le / lambda
              dummie_et <- calc(dummie_et, function(v) { v[v < 0] <- NA; v })
              dummie_et
            }
            dummie <- brick(dummie)
            dummie <- setZ(dummie, getZ(le))
            dummie
          })

#' @rdname le_2_et
#' @export
setMethod("le_2_et",
          signature(le = "character"),
          function(le, tavg = NULL, x = NULL) {
            dummie_le <- brick(le)
            dummie_ta <- if (!is.null(tavg)) brick(tavg) else NULL
            le_2_et(le = dummie_le, tavg = dummie_ta)
          })