#' Transform a data table object in tidy format to a matrix object 
#'
#' @param x List returned by dt_to_som() 
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

somregs <- function(x, nregions, ...){
  out <- list()
  temp <- data.table(x$summary)
  for(i in 2:nregions){
    som_hc <- unname(cutree(hclust(dist(x$som$codes[[1]]), ...), i))
    temp[, paste0("regions.", i) := som_hc[temp$node]]
  }
  out$regions <- reg_rename(temp)
  out$input_dt <- x$input_dt
  class(out) <- "srgs"
  return(out)
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
