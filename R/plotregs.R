#' Plots the classified regions maps
#'
#' @param x Data table created by somreg 
#' @param regions Subset of regions to be ploted
#' @param nrow Number of plots in each column 
#' @param ncol Number of plots in each row
#'  
#' @return Plots the classfication regions, representing homogeneous regions of SOM in space 
#' 
#' @examples
#' plot_regions(my_som_regions)
#' 
#' @export

plotregs <- function(x, regions, nrow, ncol, ...){
  colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                         "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                         "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
  palette_light_qual <- colorRampPalette(colset_light_qual)
  colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                   "#F4CC70", "#EBB582",  "#BF9A77",
                   "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
  colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
  
  nnodes = max(x$node)
  my_col = c(colset_light_qual, colset_mid_qual)
  
par(mfrow = c(nrow, ncol), mar = c(2, 2, 2, 2), ps = 12, bg = "white", mgp = c(3, 0.2, 0))
  for(i in regions){
    print(plot(lat ~ lon, 
               data = x, 
               pch = 15, 
               col = my_col[as.matrix(x)[, 9 + i]]))
    maps::map("world", add = TRUE)
  }
par(mfrow = c(1, 1))
}

reg_rename <- function(dta) {
  reg_numbers_old <- dta[, grepl('regions', colnames(dta)), with = F] 
  reg_numbers_raw <- as.numeric(gsub('regions.', '', names(dta[, grepl('regions', colnames(dta)), with = F])))
  id <- 1:dim(dta)[1]
  reg_id <- list()
  
  for(j in 1:dim(reg_numbers_old)[2]) {
    tmp <- list()
    for(i in 1:max(reg_numbers_old[, j, with = F])) {
      tmp[[i]] <- id[which(reg_numbers_old[, j, with = F] == i)]
    }
    reg_id[[j]] <- tmp
    names(reg_id[[j]]) <- paste0(1:max(reg_numbers_old[, j,  with = F]))
  }
  
  for(i in 1:(dim(reg_numbers_old)[2] - 1)) {
    names(reg_id[[i + 1]][reg_id[[i + 1]] %in% reg_id[[i]]])
    names(reg_id[[i + 1]]) <- as.character(names(reg_id[[i + 1]]))
    names(reg_id[[i + 1]])[which(reg_id[[i + 1]] %in% reg_id[[i]])] <- names(reg_id[[i]])[which(reg_id[[i]] %in% reg_id[[i + 1]])]
    
    if(length(reg_id[[i + 1]][!(reg_id[[i + 1]] %in% reg_id[[i]])]) != 2) next 
    if(length(reg_id[[i + 1]][!(reg_id[[i + 1]] %in% reg_id[[i]])][[1]]) >= length(reg_id[[i + 1]][!(reg_id[[i + 1]] %in% reg_id[[i]])][[2]])) {
      names(reg_id[[i + 1]])[!(reg_id[[i + 1]] %in% reg_id[[i]])][1] <- names(reg_id[[i]])[!(reg_id[[i]] %in% reg_id[[i + 1]])]
      names(reg_id[[i + 1]])[!(reg_id[[i + 1]] %in% reg_id[[i]])][2] <- reg_numbers_raw[i + 1]
    } else {
      names(reg_id[[i + 1]])[!(reg_id[[i + 1]] %in% reg_id[[i]])][1] <- reg_numbers_raw[i + 1]
      names(reg_id[[i + 1]])[!(reg_id[[i + 1]] %in% reg_id[[i]])][2] <- names(reg_id[[i]])[!(reg_id[[i]] %in% reg_id[[i + 1]])]
    }
  }
  
  reg_numbers_new <- as.data.frame(reg_numbers_old)
  
  for(i in 1:length(reg_id)) {
    for(j in 1:length(reg_id[[i]])) {
      reg_numbers_new[, i][reg_id[[i]][[j]]] <- as.numeric(names(reg_id[[i]])[j])
    }
  }
  
  out <- as.data.frame(dta)
  out[, grepl('regions', colnames(dta))] <- reg_numbers_new
  
  return(data.table(out))
}

