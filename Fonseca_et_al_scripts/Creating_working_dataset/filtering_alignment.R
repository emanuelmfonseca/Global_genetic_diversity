filtering_alignment <- function(alignment, knob){
  
  seq_index <- seq(1,length(alignment),by =2)
  if(knob == 1){
    names <- alignment[seq_index]
    names <- sub("^[^_]*_", ">", names)
    alignment[seq_index] <- names
  }
  
  rem_seq <- numeric()
  seq_index <- seq(2,length(alignment),by =2)
  for (x in 1:length(seq_index)){
    rem_seq <- c(rem_seq,nchar(gsub("[A-Za-z]","",alignment[seq_index[x]]))/nchar(alignment[seq_index[x]]))
  }
  
  if(any(rem_seq > 0.5)){
    del <- which(rem_seq > 0.5) 
    alignment <- alignment[-c(seq_index[del]-1,seq_index[del])]
  }
 
  return(alignment)
   
}