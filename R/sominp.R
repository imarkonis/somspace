#' Transform a data table object in tidy format to a somsp object
#'
#' @param x The data table object that will be tranformed 
#' 
#' @details x should contain four columns: time, latitude, longitude and variable
#' 
#' @return A sominp object. It contains a matrix that can be used as input for the som() function of 
#' the kohonen package (input_for_som), a data table linking ids with latitude and 
#' longitude (coords) and the original dataset (input_dt)
#' 
#' @examples
#' som_input(my_tidy_dt)
#' 
#' @export

sominp <- function(x, ...) UseMethod("sominp") 

sominp.default <- function(x, ...)
{
  out <- list()
  names(x) <- c("time", "lat", "lon", "variable")
  x[, id := .GRP, by = list(lat, lon)]
  setkey(x, id, time)

  input_for_som <- x[, .(id, time, variable)]
  input_for_som <- as.matrix(reshape(input_for_som, 
                                     ids = "id", 
                                     timevar = "time", 
                                     direction = "wide"))
  
  input_for_som <- input_for_som[complete.cases(input_for_som), ] 
  id_coords <- unique(x[id %in% input_for_som[, 1], .(id, lat, lon)])
  input_for_som <- input_for_som[, -1] 

  out$input_for_som <- input_for_som
  out$coords <- id_coords
  out$input_dt <- x
  out
  class(out) <- 'sominp'
  return(out)
}
