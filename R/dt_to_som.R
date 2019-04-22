#' Transform a data table object in tidy format to a matrix object 
#'
#' @param dataset A four-column data table object: time, latitude, longitude and variable
#' @param dir Name of the directory that the new objects will be saved
#' 
#' @return A list with a matrix that can be used as input for the som() function of 
#' the kohonen package (input_for_som), a data table linking ids with latitude and 
#' longitude (coords) and the original dataset (input_dt)
#' @examples
#' dt_to_som(my_tidy_dt)
#' @export

dt_to_som <- function(dataset){
  out <- list()
  names(dataset) <- c("time", "lat", "lon", "variable")
  dataset[, id := .GRP, by = list(lat, lon)]
  setkey(dataset, id, time)

  input_for_som <- dataset[, .(id, time, variable)]
  input_for_som <- as.matrix(reshape(input_for_som, 
                                     ids = "id", 
                                     timevar = "time", 
                                     direction = "wide"))
  
  input_for_som <- input_for_som[complete.cases(input_for_som), ] 
  id_coords <- unique(dataset[id %in% input_for_som[, 1], .(id, lat, lon)])
  input_for_som <- input_for_som[, -1] 

  out$input_for_som <- input_for_som
  out$coords <- id_coords
  out$input_dt <- dataset
  return(out)
}
