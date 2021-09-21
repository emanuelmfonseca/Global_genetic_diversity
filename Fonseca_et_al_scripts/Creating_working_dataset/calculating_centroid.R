calculating_centroid <- function(occ){
  unique_occ <- occ %>% distinct(latitude, longitude, .keep_all = TRUE)
  
  if (length(unique_occ[,1]) < 4){
    index <- length(unique_occ[,1])
    
    pol <- unique_occ[,c(3,2)]
    
    for (x in 1:(4-index)){
      pol <- rbind(pol,pol[sample(index,1),] + runif(1,0.01,0.05))
    }} else {
      pol <- unique_occ[,c(3,2)]
    }
  
  pol <- colMeans(do.call("cbind",pol))
  
  return(pol)
}
