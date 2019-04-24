#' Transform a data table object in tidy format to a matrix object 
#'
#' @param x A somsp object
#' @param ... Arguments of som() of kohonen package
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
#' input_for_som <- sominp(my_dt)
#' my_som <- somspa(aa), relen = 1000, grid = somgrid(6, 6, "hexagonal"))
#' my_som$summary
#' my_som$som
#' 
#' @seealso \code{\link{kohonen}} 
#' 
#' @export

somspa <- function(x, ...){
  x$som <- som(X = x$input_for_som, ...) 
  som_results <- data.table(id = x$coords$id, 
                            lat = x$coords$lat, 
                            lon = x$coords$lon,
                            node = x$som$unit.classif, 
                            distance = x$som$distances)
  som_results[, node_lat := median(lat), by = node]
  som_results[, node_lon := median(lon), by = node]
  som_results[, node_sd_lat := sd(lat), by = node]
  som_results[, node_sd_lon := sd(lon), by = node]
  som_results[, node_counts := .N, by = node]
  out <- list(summary = som_results, 
              som = x$som, 
              input_dt = merge(som_results[, .(id, node)],
                               x$input_dt, 
                               by = 'id'))
  class(out) <- 'somsp'
  return(out)
}


