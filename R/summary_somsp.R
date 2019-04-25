#' @export

summary.somsp <- function(x){
  out <- unique(x$summary[, .(node, node_counts, node_lat, node_lon, node_sd_lat, node_sd_lon)])
  setorder(out, node)
  return(out)
}
