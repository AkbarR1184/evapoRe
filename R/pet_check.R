#' PET method name checker
#'
#' Function to check if PET method is available
#'
#' @importFrom methods is
#' @param method a character string.
#' @return No return value, called to download the data set.
#' @keywords internal

pet_check <- function(method){
  if (!Reduce("&", is.element(method, c( "br", "hs", "jh",
                                              "mb", "od")))){
    stop("Error: method is not available.
    Select from br, hs, jh, mb, od")
  }
}
