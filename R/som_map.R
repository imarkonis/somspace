#' Transform a data table object in tidy format to a matrix object 
#'
#' @param data_som A list prepared by dt_to_som() function
#' @param ... Arguments of som() function of kohonen package.
#' 
#' @details ToDo
#' 
#' @return A list of a data table and a som object
#' 
#' \itemize{
#' 
#' \item{summary}{coordinates of each SOM node, the distances of objects 
#' to their corresponding winning unit, the number of points of each node, as well as the median 
#' latitude and longitude of each node coordinates, as well as their standard deviation}
#' 
#' \item{som}{Self-Organizing Map (see \code{\link{som}})}
#' }
#' 
#' @examples
#' aa <- dt_to_som(my_tidy_dt)
#' bb <- somspace(aa), relen = 1000, grid = somgrid(6, 6, "hexagonal"))
#' bb$summary
#' bb$som
#' 
#' @seealso \code{\link{kohonen}} 
#' 
#' @export

somspace <- function(data_som, ...){
  data_som$som <- som(X = data_som$input_for_som, ...) 
  som_results <- data.table(id = data_som$coords$id, 
                            lat = data_som$coords$lat, 
                            lon = data_som$coords$lon,
                            node = data_som$som$unit.classif, 
                            distance = data_som$som$distances)
  som_results[, node_lat := median(lat), by = node]
  som_results[, node_lon := median(lon), by = node]
  som_results[, node_sd_lat := sd(lat), by = node]
  som_results[, node_sd_lon := sd(lon), by = node]
  som_results[, node_counts := .N, by = node]
  return(list(summary = som_results, som = data_som$som))
}