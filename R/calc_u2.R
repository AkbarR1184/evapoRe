#' Calculate Wind Speed at 2 Meters (u2)
#'
#' Computes wind speed at 2 meters (u2) based on wind speed measured at a different height (`z_u`).
#'
#' @details
#' For raster inputs, provide a `Raster*` object or file path (`character`) for wind speed at height `z_u`. 
#' For `data.table` input, provide a single `data.table` with columns: `"lon"`, `"lat"`, `"date"`, and `"value"`, 
#' where `"value"` contains wind speeds at height `z_u`.
#'
#' If `z_u` is 2, the function returns the input unchanged.
#'
#' @rawNamespace import(data.table, except = c("month", "yday", "year"))
#' @importFrom raster brick getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @param x A `Raster*` object, a file path (`character`) to a raster file, or a `data.table` containing wind speed data.
#' @param z_u Measurement height (m) of the provided wind speed. Default is 10.
#'
#' @return Wind speed adjusted to 2 meters, returned as:
#' \itemize{
#'   \item A `RasterBrick`, if input is a raster object or file path.
#'   \item A `data.table` with updated `"value"` column, if input is a `data.table`.
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Raster input
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   wind_path <- file.path(tempdir(), "wind_speed.nc")
#'
#'   if (file.exists(wind_path)) {
#'     dummie_u <- raster::brick(wind_path)
#'     u2_raster <- calc_u2(dummie_u, z_u = 10)
#'   }
#' }
#'
#' # File path input
#' wind_path <- file.path(tempdir(), "wind_speed.nc")
#' if (file.exists(wind_path)) {
#'   u2_from_file <- calc_u2(wind_path, z_u = 10)
#' }
#'
#' # data.table input
#' if (requireNamespace("data.table", quietly = TRUE)) {
#'   dt <- data.table::data.table(
#'     lon = c(10.0, 10.5),
#'     lat = c(45.0, 45.5),
#'     date = as.Date(c("2001-06-01", "2001-06-02")),
#'     value = c(3.5, 4.1)
#'   )
#'   u2_dt <- calc_u2(dt, z_u = 10)
#' }
#' }

setGeneric("calc_u2", function(x, z_u = 10) {
  standardGeneric("calc_u2")
})

#' @rdname calc_u2
setMethod("calc_u2", 
          signature(x = "Raster"), 
          function(x, z_u = 10) {
            if (z_u == 2) {
              return(x)
            }
            
            no_cores <- detectCores() - 1
            if (is.na(no_cores) || no_cores < 1) no_cores <- 1
            registerDoParallel(cores = no_cores)
            
            dummie_u2 <- foreach(layer_index = 1:nlayers(x)) %dopar% {
              dummie_layer <- x[[layer_index]] * (4.87 / log(67.8 * z_u - 5.42))
              return(dummie_layer)
            }
            
            dummie_u2 <- brick(dummie_u2)
            dummie_u2 <- setZ(dummie_u2, getZ(x))
            return(dummie_u2)
          }
)

#' @rdname calc_u2
setMethod("calc_u2", 
          signature(x = "character"), 
          function(x, z_u = 10) {
            dummie_x <- brick(x)
            dummie_u2 <- calc_u2(dummie_x, z_u = z_u)
            return(dummie_u2)
          }
)

#' @rdname calc_u2
setMethod("calc_u2", 
          signature(x = "data.table"), 
          function(x, z_u = 10) {
            if (z_u == 2) {
              return(x)
            }
            
            x[, value := value * (4.87 / log(67.8 * z_u - 5.42))]
            return(x)
          }
)
