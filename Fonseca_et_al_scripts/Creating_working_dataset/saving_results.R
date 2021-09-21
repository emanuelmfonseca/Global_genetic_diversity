saving_results <- function(species_info,nuc_div,TajimasD,R2,centroid_occ,mut_by_bp){
  
  Results_ <- data.frame(matrix(NA,0,14))
  colnames(Results_) <- c("Group", "Species","Nucleotide_diversity","TajimasD","D_pvalue","R2","R2_pvalue,","Latitude","Longitude","Gene","mut_1_bp","mut_2_bp","mut_3_bp","Geographic_region")
  
  Results_[1,1] <- species_info[[1]][2]
  Results_[1,2] <- species_info[[1]][5]
  Results_[1,3] <- nuc_div
  Results_[1,4] <- TajimasD$D
  Results_[1,5] <- TajimasD$Pval.normal
  Results_[1,6] <- R2$R2
  Results_[1,7] <- R2$P.val
  Results_[1,8] <- centroid_occ[2]
  Results_[1,9] <- centroid_occ[1]
  Results_[1,10] <- gene
  Results_[1,11] <- mut_by_bp[1]
  Results_[1,12] <- mut_by_bp[2]
  Results_[1,13] <- mut_by_bp[3]
  
  if(centroid_occ[2] > 23.5 | centroid_occ[2] < -23.5){
    Results_[1,14] <- "Non_Tropical"
  } else {
    Results_[1,14] <- "Tropical"
  }
  
  return(Results_)
  
}