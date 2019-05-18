globalVariables(c(".", "region"))

cor_mat <- function(x) {
  mat <- as.matrix(dcast(x, time~region, value.var = "values")[, -1])
  out <- cor(mat)
  return(out)
}

cor_regs <- function(x, n, ...){
  reg_ts <- regs_ts(x, n)
  mat <- cor_mat(reg_ts)
  return(mat)
}



