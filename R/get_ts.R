#' @export

get_ts <- function(x, nregions){
  ids <- data.table(id = x$regions[[1]], region = x$regions[[9 + nregions]])
  out <- merge(x$input_dt, ids, by = "id")  
  out <- unique(out[, .(variable = mean(variable)), .(time, region)])
  return(out)
}

#' @export

cor_mat <- function(x) {
  mat <- acast(x, time~region, value.var = "variable")
  out <- cor(mat)
  return(out)
}

#' @export

cor_regs <- function(x, n, ...){
  reg_ts <- get_ts(x, n)
  mat <- cor_mat(reg_ts)
  return(mat)
}


