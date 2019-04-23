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
#' aa <- dt_to_som(my_tidy_dt)
#' bb <- somspace(aa), relen = 1000, grid = somgrid(6, 6, "hexagonal"))
#' bb$summary
#' bb$som
#' 
#' @seealso \code{\link{kohonen}} 
#' 
#' @export

somsp <- function(x, ...) UseMethod("somsp") 

somsp.default <- function(x, ...){
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
  out <- list(summary = som_results, som = x$som)
  class(out) <- 'somsp'
  return()
}

somsp.plot <- function(...){
  colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                         "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                         "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
  palette_light_qual <- colorRampPalette(colset_light_qual)
  nnodes = max(som_data$summary$node)
  my_col = palette_light_qual(nnodes)
  plot(lat ~ lon, data = som_data$summary, pch = 15, col = my_col[node])
  maps::map("world", add = TRUE)
}
