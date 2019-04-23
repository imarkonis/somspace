#' Spatial SOM regions class
#' 
#' @details ToDo
#' 
#' @examples
#' my_som <- somspa(aa), relen = 1000, grid = somgrid(6, 6, "hexagonal"))
#' summary(my_som)
#' plot(my_som)
#' 
#' @export

regs <- list()
class(regs) <- "regs"

plot.regs <- function(x, regions, nrow, ncol, ...){
  colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                         "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                         "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
  palette_light_qual <- colorRampPalette(colset_light_qual)
  colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                   "#F4CC70", "#EBB582",  "#BF9A77",
                   "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
  colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
  
  nnodes = max(x$regions$node)
  my_col = c(colset_light_qual, colset_mid_qual)
  
  par(mfrow = c(nrow, ncol), mar = c(2, 2, 2, 2), ps = 12, bg = "white", mgp = c(3, 0.2, 0))
  for(i in regions){
    print(plot(lat ~ lon, 
               data = x$regions, 
               pch = 15, 
               col = my_col[as.matrix(x$regions)[, 9 + i]]))
    maps::map("world", add = TRUE)
  }
  par(mfrow = c(1, 1))
} 

regs.get_ts <- function(x, nregions){
  ids <- data.table(id = x$regions[[1]], region = x$regions[[nregions + nregions]])
  out <- merge(x$input_dt, ids, by = "id")  
  out <- unique(out[, .(variable = mean(variable)), .(time, region)])
  return(out)
}

regs.ts_plot <- function(x, nregions){
  ids <- data.table(id = x$regions[[1]], region = x$regions[[nregions + nregions]])
  to_plot <- merge(x$input_dt, ids, by = "id")  
  to_plot <- unique(to_plot[, .(variable = mean(variable)), .(time, region)])
  ggplot(to_plot, aes(x = time, y = variable)) +
    geom_line(alpha = 0.3) +
    geom_smooth(method = 'loess', span = 0.1, col =  "black", fill = 'dark red') +
    facet_wrap(~region) +
    labs(x = "Time", y = "Variable") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey20")) +
    theme(strip.text.x = element_text(colour = "grey90", size = 10))
}
