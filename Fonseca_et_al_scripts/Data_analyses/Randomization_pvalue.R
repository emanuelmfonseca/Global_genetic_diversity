randomization_pvalue <- function(randomization_info){
  
  min_tropical <- length(which(randomization_info[[1]][which(randomization_info[[1]][,2] == "Tropical"),1] < randomization_info[[2]][1]))
  max_tropical <- length(which(randomization_info[[1]][which(randomization_info[[1]][,2] == "Tropical"),1] > randomization_info[[2]][1]))
  
  pvalue_tropical <- min(min_tropical,max_tropical)/(length(randomization_info[[1]][,1])/2)
  
  min_non_tropical <- length(which(randomization_info[[1]][which(randomization_info[[1]][,2] == "Non_Tropical"),1] < randomization_info[[2]][2]))
  max_non_tropical <- length(which(randomization_info[[1]][which(randomization_info[[1]][,2] == "Non_Tropical"),1] > randomization_info[[2]][2]))
  
  pvalue_non_tropical <- min(min_non_tropical,max_non_tropical)/(length(randomization_info[[1]][,1])/2)
  
  pvalue <- c(pvalue_tropical,pvalue_non_tropical)
  names(pvalue) <- c("pvalue_tropical", "pvalue_non_tropical")
  
  return(pvalue)
  
}