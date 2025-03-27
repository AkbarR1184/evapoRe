#' Calculate Wind Speed at 2m (u2)
#'
#' Computes wind speed at 2 meters (u2) based on measured wind speed \code{u} at 
#' a certain height \code{z_u}. If \code{z_u} is already 2, the function 
#' returns the input as-is.
#'
#' @details
#' For raster inputs, provide either a \code{Raster*} object or a file path 
#' (character) to a raster file that contains wind speed values at height 
#' \code{z_u}. For \code{data.table} input, provide a single \code{data.table} with 
#' columns: \code{"lon"}, \code{"lat"}, \code{"date"}, and \code{"value"}. The 
#' \code{"value"} column should contain wind speeds at height \code{z_u}. 
#'
#' @import data.table
#' @importFrom raster brick getZ setZ nlayers
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @param u A \code{Raster*} object, a file path (character) to a raster file, 
#'   or a \code{data.table} with columns \code{"lon"}, \code{"lat"}, \code{"date"}, 
#'   and \code{"value"}. See \strong{Details}.
#' @param z_u \code{numeric}. The measurement height (meters) of the provided wind 
#'   speed, typically 10. Defaults to 10.
#'
#' @return Depending on the input:
#' \itemize{
#'   \item If \code{u} is a \code{Raster*}, returns a \code{RasterBrick} of wind 
#'         speeds adjusted to 2 meters.
#'   \item If \code{u} is a character (file path), returns a \code{RasterBrick} 
#'         after reading the file and adjusting speeds to 2 meters.
#'   \item If \code{u} is a \code{data.table}, returns a \code{data.table} with an 
#'         updated \code{"value"} column representing wind speeds at 2 meters.
#' }
#' 
#' @keywords internal
#'
setGeneric("calc_u2", function(u, z_u = 10) {
  standardGeneric("calc_u2")
})
#' @rdname calc_u2
setMethod("calc_u2", 
          signature(u = "Raster"), 
          function(u, z_u = 10) {
            if (z_u == 2) {
              return(u)
            }
            no_cores <- detectCores() - 1
            if (is.na(no_cores) || no_cores < 1) no_cores <- 1
            registerDoParallel(cores = no_cores)
            u_dates <- getZ(u)
            dummie <- foreach(idx = 1:nlayers(u)) %dopar% {
              u_layer <- u[[idx]] * (4.87 / log(67.8 * z_u - 5.42))
              return(u_layer)
            }
            u2_brick <- brick(dummie)
            if (!is.null(u_dates)) {
              u2_brick <- setZ(u2_brick, u_dates)
            }
            
            return(u2_brick)
          }
)
#' @rdname calc_u2
setMethod("calc_u2", 
          signature(u = "character"), 
          function(u, z_u = 10) {
            if (!file.exists(u)) {
              stop("File not found: ", u)
            }
            u_raster <- brick(u)
            calc_u2(u_raster, z_u = z_u)
          }
)
#' @rdname calc_u2
setMethod("calc_u2", 
          signature(u = "data.table"), 
          function(u, z_u = 10) {
            if (z_u == 2) {
              return(u)
            }
            required_cols <- c("lon", "lat", "date", "value")
            missing_cols <- setdiff(required_cols, names(u))
            if (length(missing_cols) > 0) {
              stop("Missing column(s): ", paste(missing_cols, collapse = ", "))
            }
            u[, value := value * (4.87 / log(67.8 * z_u - 5.42))]
            
            return(u)
          }
)
