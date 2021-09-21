georeferenced_data <- function(alignment, occurences, knob){
  
  accession <- alignment[seq(1,length(alignment), by=2)]
  
  if(knob == 1){
    accession <- gsub(">(\\w+\\d+)_?.*","\\1",accession)
  } else {
    accession <- gsub(">(\\w+\\d+)_.*","\\1",accession)
  }
  
  if (knob == 1){
    occurences$accession <- gsub("(.*)-+.*","\\1",occurences$accession)
  }
  
  acesssion_georeferenced <- accession[which(accession %in% occurences$accession == TRUE)]
  
  return(list(acesssion_georeferenced,accession))
  
}