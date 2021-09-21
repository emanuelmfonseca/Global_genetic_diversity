calculating_codon_mutation <- function(Alignment,group){

index <- seq(2,length(Alignment),2)

Alignment <- Alignment[index]
Alignment <- strsplit(Alignment, "")

df <- data.frame(matrix(NA,length(Alignment),length(Alignment[[1]])))

for (x in 1:length(Alignment)){
  df[x,] <-Alignment[[x]]
}

position1 <- 0
position2 <- 0
position3 <- 0

seq1 <- seq(1,length(df[1,]),3)
seq2 <- seq(2,length(df[1,]),3)
seq3 <- seq(3,length(df[1,]),3)

for (x in 1:length(df[1,])){
  t <- table(df[,x])
  if (!identical(which(names(t) %in% "-" == TRUE),integer(0))){
    index <- which(names(t) %in% "-" == TRUE)
    t <- t[-index]
  }
  if(length(t) > 1){
    if(any(seq1 %in% x)){
      position1 <- position1 + 1
    } else if (any(seq2 %in% x)){
      position2 <- position2 + 1
    } else if (any(seq3 %in% x)){
      position3 <- position3 + 1
    }}
}

Vertebrate <- c("Amphibia", "Reptilia", "Mammalia", "Aves","Myxini","Actinopterygii","Sarcopterygii","Elasmobranchii","Holocephali")
Invertebrate <- c("Arachnida","Insecta")

if(any(Vertebrate %in% group)){
  code_n <- 2
} else if (any(Invertebrate %in% group)){
  code_n <- 5
} else {
  code_n <- 1
}

Alignment2 <- Alignment <- read.FASTA(paste0("@Alignment_",gene,".fas"))

if (length(capture_warnings(trans(Alignment2, code = code_n, codonstart = 1))) == 0){
  mut_by_bp <- c(position1,position2,position3)
} else if (length(capture_warnings(trans(Alignment2, code = code_n, codonstart = 2))) == 0){
  mut_by_bp <- c(position2,position3,position1)
} else if (length(capture_warnings(trans(Alignment2, code = code_n, codonstart = 3))) == 0){
  mut_by_bp <- c(position3,position2,position1)
} else {
  mut_by_bp <- c(NA,NA,NA)
}

return(mut_by_bp)

}