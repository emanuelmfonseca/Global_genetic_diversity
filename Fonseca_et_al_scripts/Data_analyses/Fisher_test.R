###############################################################################################
# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity #
                  # Fonseca EM; Pelletier TA, Decker SK,Parsons DJ, Carstens BC #
                                    # September 2021 #
###############################################################################################

### Path to the scripts folder
path_to_folder <- "~/Desktop"

### Reading working
data <- read.table(file.path(path_to_folder,"Fonseca_et_al_scripts/Working_dataset/Working_dataset.txt"),h=T)

### Taxonomic groups
Vertebrates <- c("Amphibia", "Reptilia", "Mammalia", "Aves","Myxini","Actinopterygii","Sarcopterygii","Elasmobranchii","Holocephali")
Insects <- "Insecta"
Plants <- c("Magnoliopsida","Pinopsida","Liliopsida","Florideophyceae","Ulvophyceae","Polypodiopsida","Bryopsida","Bangiophyceae","Jungermanniopsida","Psilotopsida","Pteridopsida","Chlorophyceae","Cycadopsida","Lycopodiopsida","Gnetopsida","Klebsormidiophyceae","Zygnematophyceae","Charophyceae","Polytrichopsida","Trebouxiophyceae","Compsopogonophyceae")
Arachnids <- "Arachnida"

tax_groups <- c("Vertebrates", "Insects", "Plants", "Arachnids")

### Creating an emptydata frame to calculate the proportion of species with significant values of Tajima's D and R2
df_proportion <- data.frame(matrix(NA,8,6))
colnames(df_proportion) <- c("Taxonomic_group",	"n_of_species",	"Average_Tajimas_D",	"n_of_significant_species_D",	"Average_R2",	"n_of_significant_species_R2")

### Creating a loop to create the data frame
index <- 1
for (x in tax_groups){
  data2 <- data[which(data[,1] %in% get(x) == TRUE),]
  
  df_proportion[index,1] <- paste0(x, "_Tropical")
  df_proportion[index,2] <- length(data2[,1])
  df_proportion[index,3] <- mean(data2[which(data2[,"Geographic_region"] == "Tropical"),"TajimasD"])
  df_proportion[index,4] <- length(which(data2[which(data2[,"Geographic_region"] == "Tropical"),"D_pvalue"] < 0.05))
  df_proportion[index,5] <- mean(data2[which(data2[,"Geographic_region"] == "Tropical"),"R2"])
  df_proportion[index,6] <- length(which(data2[which(data2[,"Geographic_region"] == "Tropical"),"R2_pvalue"] < 0.05))
  index <- index + 1
  
  df_proportion[index,1] <- paste0(x, "_Non_Tropical")
  df_proportion[index,2] <- length(data2[,1])
  df_proportion[index,3] <- mean(data2[which(data2[,"Geographic_region"] == "Non_Tropical"),"TajimasD"])
  df_proportion[index,4] <- length(which(data2[which(data2[,"Geographic_region"] == "Non_Tropical"),"D_pvalue"] < 0.05))
  df_proportion[index,5] <- mean(data2[which(data2[,"Geographic_region"] == "Non_Tropical"),"R2"])
  df_proportion[index,6] <- length(which(data2[which(data2[,"Geographic_region"] == "Non_Tropical"),"R2_pvalue"] < 0.05))
  index <- index + 1
  
}

### Creating an empty vector to save the results
pvalues_D <- numeric()
pvalues_R2 <- numeric()

### Fisher's exact test for count data
### Vertebrates
df_D <- matrix(c(df_proportion[1,4],(df_proportion[1,2]-df_proportion[1,4]),df_proportion[2,4],(df_proportion[2,2]-df_proportion[2,4])),ncol=2,byrow = F)
pvalues_D <- c(pvalues_D, fisher.test(df_D)$p.value)

df_R2 <- matrix(c(df_proportion[1,6],(df_proportion[1,2]-df_proportion[1,6]),df_proportion[2,6],(df_proportion[2,2]-df_proportion[2,6])),ncol=2,byrow = F)
pvalues_R2 <- c(pvalues_R2, fisher.test(df_R2)$p.value)

### Insects
df_D <- matrix(c(df_proportion[3,4],(df_proportion[3,2]-df_proportion[3,4]),df_proportion[4,4],(df_proportion[4,2]-df_proportion[4,4])),ncol=2,byrow = F)
pvalues_D <- c(pvalues_D, fisher.test(df_D)$p.value)

df_R2 <- matrix(c(df_proportion[3,6],(df_proportion[3,2]-df_proportion[3,6]),df_proportion[4,6],(df_proportion[4,2]-df_proportion[4,6])),ncol=2,byrow = F)
pvalues_R2 <- c(pvalues_R2, fisher.test(df_R2)$p.value)

### Arachnids
df_D <- matrix(c(df_proportion[5,4],(df_proportion[5,2]-df_proportion[5,4]),df_proportion[6,4],(df_proportion[6,2]-df_proportion[6,4])),ncol=2,byrow = F)
pvalues_D <- c(pvalues_D, fisher.test(df_D)$p.value)

df_R2 <- matrix(c(df_proportion[5,6],(df_proportion[5,2]-df_proportion[5,6]),df_proportion[6,6],(df_proportion[6,2]-df_proportion[6,6])),ncol=2,byrow = F)
pvalues_R2 <- c(pvalues_R2, fisher.test(df_R2)$p.value)

### Plants
df_D <- matrix(c(df_proportion[7,4],(df_proportion[7,2]-df_proportion[7,4]),df_proportion[8,4],(df_proportion[8,2]-df_proportion[8,4])),ncol=2,byrow = F)
pvalues_D <- c(pvalues_D, fisher.test(df_D)$p.value)

df_R2 <- matrix(c(df_proportion[7,6],(df_proportion[7,2]-df_proportion[7,6]),df_proportion[8,6],(df_proportion[8,2]-df_proportion[8,6])),ncol=2,byrow = F)
pvalues_R2 <- c(pvalues_R2, fisher.test(df_R2)$p.value)

##Bonferroni correction
pvalues_ajusted_D <- p.adjust(pvalues_D, method = "bonferroni")
names(pvalues_ajusted_D) <- tax_groups

pvalues_ajusted_R2 <- p.adjust(pvalues_R2, method = "bonferroni")
names(pvalues_ajusted_R2) <- tax_groups
