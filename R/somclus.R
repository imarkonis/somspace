#' Transform a data table object in tidy format to a matrix object 
#'
#' @param data_som List returned by dt_to_som() 
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
#' aa <- dt_to_som(my_tidy_dt)
#' bb <- somspace(aa), relen = 1000, grid = somgrid(6, 6, "hexagonal"))
#' bb$summary
#' bb$som
#' 
#' @seealso \code{\link{kohonen}} 
#' 
#' @export


som_clusters <- function(som_data, nclusters, ...){
  out <- data.table(som_data$summary)
  for(i in 2:nclusters){
    som_hc <- unname(cutree(hclust(dist(som_data$som$codes[[1]]), ...), i))
    out[, paste0("clusters.", i) := som_hc[out$node]]
  }
  return(cl_rename(out))
}