#' Complex network analysis
#' 
#' @description `regs_ts` returns a `data.table` with the values single classification scheme. 
#'
#' @param x regs object. 
#' @param n number of regions in the chosen classification.
#' 
#' @details `regs_ts` function estimates the cross-correlation matrix of the average time series of 
#' each region and plots a map linking the regions with cross-correlations above the selected threshold.
#' 
#' @examples
#' dummy <- owda[Time <= 1600] #toy example
#' inp_som <- sominp(dummy)
#' my_som <- somspa(inp_som, rlen = 100, grid = somgrid(3, 3, "hexagonal"))
#' my_regions <- somregs(my_som, nregions = 6) 
#' my_regions_5 <- select_regs(my_regions, n = 5)
#' \donttest{
#' inp_som <- sominp(owda)
#' my_som <- somspa(inp_som, rlen = 1000, grid = somgrid(6, 6, "hexagonal"))
#' my_regions <- somregs(my_som, nregions = 15) 
#' my_regions_12 <- select_regs(my_regions, n = 12)}
#' @export

regs_ts <- function(x, nregions){
  ids <- data.table(id = x$regions[[1]], region = x$regions[[9 + nregions]])
  out <- merge(x$input_dt, ids, by = "id")  
  out <- unique(out[, .(values = mean(values)), .(time, region)])
  return(out)
}
