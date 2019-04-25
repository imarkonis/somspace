#' Regions class
#' 
#' @details The `regs` class contains: 
#' 
#' \itemize{
#' 
#' \item{A summary `data.table` which updates the `somsp` object with the region ids of all classification schemes
#' up to `nregions`. Each different classification scheme is stored as an individual region, e.g. `regions.2`, 
#' `regions.3`, etc.}
#' 
#' \item{A `data.table` with the original data set, as in `somsp`.}
#' }
#' 
#' It can be plotted by `plot` and `plot_ts`. 
#' If `plot` is used, three additional arguments are needed; a set with the classification schemes 
#' that will be ploted, number of rows and number of columns of the plotted panels.
#' `plot_ts` plots all the time series of a given classification scheme.
#' 
#' @seealso \code{\link{somsp}} 
#' @seealso \code{\link{somregs}} 
#' 
#' @examples
#' \dontrun{}
#' my_som <- somspa(inp_som)
#' my_regions <- somregs(my_som) 
#' plot(my_regions, c(2, 5, 9, 13), nrow = 2, ncol = 2) #nrow, ncol corresponds to the number of 
#' plot_ts(my_regions, 2)
#' 
#' @export

regs <- list()
class(regs) <- "regs"

