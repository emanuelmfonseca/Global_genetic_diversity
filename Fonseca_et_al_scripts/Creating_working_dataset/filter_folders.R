filter_folders <- function(path,taxonomic_groups){
  setwd(path)
  folders <- list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
  count <- lengths(regmatches(folders, gregexpr("/", folders)))
  index <- which(count == max(count))
  folders <- folders[index]
  
  new_folders <- character()
  
  for (x in 1:length(folders)){
    group <- strsplit(folders[x], "/")
    group <- group[[1]][2]
    
    if (group %in% taxonomic_groups){
      new_folders <- c(new_folders, folders[x])
    }
  }
  
  return(new_folders)
  
}
