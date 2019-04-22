#' Plots SOM maps with node characteristics
#'
#' @param som_data List returned by dt_to_som  
#' 
#' @return Plots four plots presenting
#' 
#' \itemize{
#' 
#' \item{Distance}{Mean distance of objects to their corresponding winning unit per node}
#' 
#' \item{Counts}{Number of counts of spatial points per node}
#' 
#' \item{Lat_sd}{Standard deviation of latitude of spatial points per node}
#' 
#' \item{Long_sd}{Standard deviation of longitude of spatial points per node}
#' 
#' @examples
#' plot_som_summary(my_som)
#' 
#' @export

plot_som_summary <- function(som_data, ...){
  colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                         "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                         "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
  palette_light_qual <- colorRampPalette(colset_light_qual)
  
  nnodes = max(som_data$summary$node)
  my_col = palette_light_qual(nnodes)

  par(mfrow = c(2, 2), 
      mar = c(2, 2, 2, 2), 
      ps = 12, bg = "white", mgp = c(3, 0.2, 0))
  
  plot(node_lat ~ node_lon, data = som_data$summary, cex = distance / 1000, pch = 16, col = my_col[node], xlab = "", ylab = "", main = "Distance")
  maps::map("world", add = TRUE)
  plot(node_lat ~ node_lon, data = som_data$summary, cex = node_counts / 20, pch = 16, col = my_col[node], xlab = "", ylab = "", main = "Counts")
  maps::map("world", add = TRUE)
  plot(node_lat ~ node_lon, data = som_data$summary, cex = node_sd_lat, pch = 16, col = my_col[node], xlab = "", ylab = "", main = "Lat sd")
  maps::map("world", add = TRUE)
  plot(node_lat ~ node_lon, data = som_data$summary, cex = node_sd_lon, pch = 16, col = my_col[node], xlab = "", ylab = "", main = "Lon sd")
  maps::map("world", add = TRUE)
  par(mfrow = c(1, 1))
}
