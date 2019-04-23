#' Spatial SOM class
#' 
#' @details ToDo
#' 
#' @examples
#' my_som <- somspa(aa), relen = 1000, grid = somgrid(6, 6, "hexagonal"))
#' summary(my_som)
#' plot(my_som)
#' 
#' @export

somsp <- list()
class(somsp) <- "somsp"

plot.somsp <- function(x, ...){
  colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                         "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                         "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
  palette_light_qual <- colorRampPalette(colset_light_qual)
  nnodes = max(x$summary$node)
  my_col = palette_light_qual(nnodes)
  plot(lat ~ lon, data = x$summary, pch = 15, col = my_col[node])
  maps::map("world", add = TRUE)
}

summary.somsp <- function(x, ...){
  out <- unique(x$summary[, .(node, node_counts, node_lat, node_lon, node_sd_lat, node_sd_lon)])
  setorder(out, node)
  return(out)
}
