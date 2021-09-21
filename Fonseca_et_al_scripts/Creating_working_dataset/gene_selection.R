gene_selection <- function(current_directory){
  
  setwd(current_directory)
  
  genes <- c("COI","CYTB","ND4","ATP8","ATP6","16S","ND1","ND2","COII","12S","ND6","ND3","ND1","RBCL","MATK","PSBA","TRN","RPOB","ATPF","ATPH")
  
  gene_selection <- sub(".*(-.*)","\\1",list.files(pattern=".afa"))
  gene_selection <- sub("-(.*).afa","\\1", gene_selection)
  gene_selection <- gene_selection[which(gene_selection %in% genes == TRUE)]
  
  if(length(gene_selection) > 0){
    
    if(length(gene_selection) > 1){
      l <- numeric()
      for (x in 1:length(gene_selection)){
        l <- c(l, length(read.FASTA(list.files(pattern=paste0(gene_selection[x],".afa")))))
      }
      
      gene_p <- which(max(l) == l)
      
      if (length(gene_p) > 1){
        gene <- gene_selection[sample(gene_p,1)]
      } else {
        gene <- gene_selection[gene_p]
      }} else {
        gene <- gene_selection[1]
      }} else {
        gene <- character(0)
      }
  
  return(gene)
  
}