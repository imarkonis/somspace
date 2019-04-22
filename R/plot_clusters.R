#' Plots classification maps
#'
#' @param som_clusters Data table created by som_clusters() 
#' @param clusters Subset of clusters to be ploted
#' @param nrow Number of plots in each column 
#' @param ncol Number of plots in each row
#'  
#' @return Plots the classfication clusters, representing homogeneous regions of SOM in space 
#' 
#' @examples
#' plot_clusters(my_som_clusters)
#' 
#' @export

plot_clusters <- function(som_clusters, clusters, nrow, ncol, ...){
  colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                         "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                         "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
  palette_light_qual <- colorRampPalette(colset_light_qual)
  colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                   "#F4CC70", "#EBB582",  "#BF9A77",
                   "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
  colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
  
  nnodes = max(som_clusters$node)
  my_col = c(colset_light_qual, colset_mid_qual)
  
par(mfrow = c(nrow, ncol), mar = c(2, 2, 2, 2), ps = 12, bg = "white", mgp = c(3, 0.2, 0))
  for(i in clusters){
    print(plot(lat ~ lon, 
               data = som_clusters, 
               pch = 15, 
               col = my_col[as.matrix(som_clusters)[, 9 + i]]))
    maps::map("world", add = TRUE)
  }
par(mfrow = c(1, 1))
}

cl_rename <- function(dta) {
  cl_numbers_old <- dta[, grepl('clusters', colnames(dta)), with = F] 
  cl_numbers_raw <- as.numeric(gsub('clusters.', '', names(dta[, grepl('clusters', colnames(dta)), with = F])))
  id <- 1:dim(dta)[1]
  cl_id <- list()
  
  for(j in 1:dim(cl_numbers_old)[2]) {
    tmp <- list()
    for(i in 1:max(cl_numbers_old[, j, with = F])) {
      tmp[[i]] <- id[which(cl_numbers_old[, j, with = F] == i)]
    }
    cl_id[[j]] <- tmp
    names(cl_id[[j]]) <- paste0(1:max(cl_numbers_old[, j,  with = F]))
  }
  
  for(i in 1:(dim(cl_numbers_old)[2] - 1)) {
    names(cl_id[[i + 1]][cl_id[[i + 1]] %in% cl_id[[i]]])
    names(cl_id[[i + 1]]) <- as.character(names(cl_id[[i + 1]]))
    names(cl_id[[i + 1]])[which(cl_id[[i + 1]] %in% cl_id[[i]])] <- names(cl_id[[i]])[which(cl_id[[i]] %in% cl_id[[i + 1]])]
    
    if(length(cl_id[[i + 1]][!(cl_id[[i + 1]] %in% cl_id[[i]])]) != 2) next 
    if(length(cl_id[[i + 1]][!(cl_id[[i + 1]] %in% cl_id[[i]])][[1]]) >= length(cl_id[[i + 1]][!(cl_id[[i + 1]] %in% cl_id[[i]])][[2]])) {
      names(cl_id[[i + 1]])[!(cl_id[[i + 1]] %in% cl_id[[i]])][1] <- names(cl_id[[i]])[!(cl_id[[i]] %in% cl_id[[i + 1]])]
      names(cl_id[[i + 1]])[!(cl_id[[i + 1]] %in% cl_id[[i]])][2] <- cl_numbers_raw[i + 1]
    } else {
      names(cl_id[[i + 1]])[!(cl_id[[i + 1]] %in% cl_id[[i]])][1] <- cl_numbers_raw[i + 1]
      names(cl_id[[i + 1]])[!(cl_id[[i + 1]] %in% cl_id[[i]])][2] <- names(cl_id[[i]])[!(cl_id[[i]] %in% cl_id[[i + 1]])]
    }
  }
  
  cl_numbers_new <- as.data.frame(cl_numbers_old)
  
  for(i in 1:length(cl_id)) {
    for(j in 1:length(cl_id[[i]])) {
      cl_numbers_new[, i][cl_id[[i]][[j]]] <- as.numeric(names(cl_id[[i]])[j])
    }
  }
  
  out <- as.data.frame(dta)
  out[, grepl('clusters', colnames(dta))] <- cl_numbers_new
  
  return(data.table(out))
}

