occurences <- function(current_directory){
  
  setwd(current_directory)
  
  occurences <- read.table("occurrences.txt",h=T, fill = TRUE, sep="\t")[,c(1,3:4)]
  
  knob <- 0
  if(any(is.na(occurences[,1]))){
    occurences <- read.table("occurrences.txt",h=T, fill = TRUE, sep="\t")[,c(1,3:4)]
    occurences[,1] <- read.table("occurrences.txt",h=T, fill = TRUE, sep="\t")[,2]
    knob <- 1
  } else {
    occurences <- read.table("occurrences.txt",h=T, fill = TRUE, sep="\t")[,c(1,3:4)]  
  }
  
  return(list(occurences,knob))
  
}
