nucleotide_diversity <- function(new_sequence){
  
  index <- seq(1, length(new_sequence), by=2)
  index2 <- combn(index,2)
  
  nucleotide_div <- numeric()
  ss <- numeric()
  for (x in 1:length(index2[1,])){
    new_sequence2 = character()
    index3 <- index2[,x]
    new_sequence2 <- c(new_sequence2, new_sequence[index3[1]])
    new_sequence2 <- c(new_sequence2, new_sequence[index3[1]+1])
    new_sequence2 <- c(new_sequence2, new_sequence[index3[2]])
    new_sequence2 <- c(new_sequence2, new_sequence[index3[2]+1])
    new_sequence2 <- gsub('-',"N",new_sequence2)
    
    write.table(toupper(new_sequence2), paste0("@Alignment_",gene,".fas"), quote = FALSE, row.names = FALSE, col.names = FALSE)  
    
    seqs <- read.FASTA(paste0("@Alignment_",gene,".fas"))
    
    
    if(dist.dna(seqs) < 0.1){
      nucleotide_div <- c(nucleotide_div, pegas::nuc.div(seqs))
    }}
  
  unlink(paste0("@Alignment_",gene,".fas"))
  
  return(nucleotide_div)
  
}