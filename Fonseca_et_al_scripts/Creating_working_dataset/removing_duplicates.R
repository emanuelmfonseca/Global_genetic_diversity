removing_duplicates <- function(alignment,occurences,accession,acesssion_georeferenced){
  
  alignment_position <- seq(1,length(accession)*2, by=2)
  alignment_position <- alignment_position[which(accession %in% acesssion_georeferenced)]
  
  new_sequence = character()
  for (x in alignment_position){
    new_sequence <- c(new_sequence, gsub("(>\\w+\\d+)_.*","\\1",alignment[x]))
    new_sequence <- c(new_sequence, sub("(\\d+)\\s+(\\w+)", "\\2", alignment[x+1]))
  }
  
  unique_names <- new_sequence[seq(1,length(new_sequence),by=2)]
  del <- which(duplicated(unique_names) == TRUE)
  
  if(length(del) != 0){
    new_sequence <- new_sequence[-c(seq(1,length(new_sequence),by=2)[del],seq(1,length(new_sequence),by=2)[del]+1)]
  }
  
  occurences[,1] <- gsub("(.*)-.*","\\1",occurences[,1])
  
  acesssion_georeferenced_position <- which(occurences[,1] %in% acesssion_georeferenced)
  new_occurence <- occurences[acesssion_georeferenced_position,]
  
  return(list(new_sequence,new_occurence))
  
}