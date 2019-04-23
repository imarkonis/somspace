

som_clusters <- function(som_data, nclusters, ...){
  out <- data.table(som_data$summary)
  for(i in 2:nclusters){
    som_hc <- unname(cutree(hclust(dist(som_data$som$codes[[1]]), ...), i))
    out[, paste0("clusters.", i) := som_hc[out$node]]
  }
  return(cl_rename(out))
}