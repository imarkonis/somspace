#' Plots SOM map
#'
#' @param som_data List created by som_map 
#' 
#' @return Plots the nodes of SOM in space 
#' 
#' @examples
#' plot_som_map(my_som)
#' 
#' @export

plot_som_space <- function(som_data, ...){
  colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                         "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                         "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
  palette_light_qual <- colorRampPalette(colset_light_qual)
  nnodes = max(som_data$summary$node)
  my_col = palette_light_qual(nnodes)
  plot(lat ~ lon, data = som_data$summary, pch = 15, col = my_col[node])
  maps::map("world", add = TRUE)
}

