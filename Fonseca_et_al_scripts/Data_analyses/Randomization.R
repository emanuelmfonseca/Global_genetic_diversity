###############################################################################################
# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity #
              # Fonseca EM; Pelletier TA, Decker SK,Parsons DJ, Carstens BC #
                                    # September 2021 #
###############################################################################################
library(ggplot2)
library(cowplot)
library(colortools)
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Data_analyses/Create_Randomization.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Data_analyses/Randomization_pvalue.R"))

cols <- splitComp("firebrick")

### Path to the scripts folder
path_to_folder <- "~/Desktop"

### Reading working
data <- read.table(file.path(path_to_folder,"Fonseca_et_al_scripts/Working_dataset/Working_dataset.txt"),h=T)

### Taxonomic groups
Vertebrates <- c("Amphibia", "Reptilia", "Mammalia", "Aves","Myxini","Actinopterygii","Sarcopterygii","Elasmobranchii","Holocephali")
Insects <- "Insecta"
Plants <- c("Magnoliopsida","Pinopsida","Liliopsida","Florideophyceae","Ulvophyceae","Polypodiopsida","Bryopsida","Bangiophyceae","Jungermanniopsida","Psilotopsida","Pteridopsida","Chlorophyceae","Cycadopsida","Lycopodiopsida","Gnetopsida","Klebsormidiophyceae","Zygnematophyceae","Charophyceae","Polytrichopsida","Trebouxiophyceae","Compsopogonophyceae")

tax_groups <- c("Vertebrates", "Insects", "Plants")

### Creating an empty vector to save the results
pvalue_Nucleotide_diversity_tropical <- numeric()
pvalue_D_tropical <- numeric()
pvalue_R2_tropical <- numeric()

pvalue_Nucleotide_diversity_non_tropical <- numeric()
pvalue_D_non_tropical <- numeric()
pvalue_R2_non_tropical <- numeric()

###Nucleotide diversity
### Vertebrates
data2 <- data[which(data[,1] %in% Vertebrates == TRUE),]
randomization_info <- randomization(data2,1000,"Nucleotide_diversity","Vertebrates")

pvalue_Nucleotide_diversity_tropical <- c(pvalue_Nucleotide_diversity_tropical,randomization_pvalue(randomization_info)[1])
pvalue_Nucleotide_diversity_non_tropical <- c(pvalue_Nucleotide_diversity_non_tropical,randomization_pvalue(randomization_info)[2])

p1 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

### Insects
data2 <- data[which(data[,1] %in% Insects == TRUE),]
randomization_info <- randomization(data2,1000,"Nucleotide_diversity","Insects")

pvalue_Nucleotide_diversity_tropical <- c(pvalue_Nucleotide_diversity_tropical,randomization_pvalue(randomization_info)[1])
pvalue_Nucleotide_diversity_non_tropical <- c(pvalue_Nucleotide_diversity_non_tropical,randomization_pvalue(randomization_info)[2])

p2 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

### Plants
data2 <- data[which(data[,1] %in% Plants == TRUE),]
randomization_info <- randomization(data2,1000,"Nucleotide_diversity","Plants")

pvalue_Nucleotide_diversity_tropical <- c(pvalue_Nucleotide_diversity_tropical,randomization_pvalue(randomization_info)[1])
pvalue_Nucleotide_diversity_non_tropical <- c(pvalue_Nucleotide_diversity_non_tropical,randomization_pvalue(randomization_info)[2])

p3 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

###Tajima's D
### Vertebrates
data2 <- data[which(data[,1] %in% Vertebrates == TRUE),]
randomization_info <- randomization(data2,1000,"TajimasD","Vertebrates")

pvalue_D_tropical <- c(pvalue_D_tropical,randomization_pvalue(randomization_info)[1])
pvalue_D_non_tropical <- c(pvalue_D_non_tropical,randomization_pvalue(randomization_info)[2])

p4 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

### Insects
data2 <- data[which(data[,1] %in% Insects == TRUE),]
randomization_info <- randomization(data2,1000,"TajimasD","Insects")

pvalue_D_tropical <- c(pvalue_D_tropical,randomization_pvalue(randomization_info)[1])
pvalue_D_non_tropical <- c(pvalue_D_non_tropical,randomization_pvalue(randomization_info)[2])

p5 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

### Plants
data2 <- data[which(data[,1] %in% Plants == TRUE),]
randomization_info <- randomization(data2,1000,"TajimasD","Plants")

pvalue_D_tropical <- c(pvalue_D_tropical,randomization_pvalue(randomization_info)[1])
pvalue_D_non_tropical <- c(pvalue_D_non_tropical,randomization_pvalue(randomization_info)[2])

p6 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

###R2
### Vertebrates
data2 <- data[which(data[,1] %in% Vertebrates == TRUE),]
randomization_info <- randomization(data2,1000,"R2","Vertebrates")

pvalue_R2_tropical <- c(pvalue_R2_tropical,randomization_pvalue(randomization_info)[1])
pvalue_R2_non_tropical <- c(pvalue_R2_non_tropical,randomization_pvalue(randomization_info)[2])

p7 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

### Insects
data2 <- data[which(data[,1] %in% Insects == TRUE),]
randomization_info <- randomization(data2,1000,"R2","Insects")

pvalue_R2_tropical <- c(pvalue_R2_tropical,randomization_pvalue(randomization_info)[1])
pvalue_R2_non_tropical <- c(pvalue_R2_non_tropical,randomization_pvalue(randomization_info)[2])

p8 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

### Plants
data2 <- data[which(data[,1] %in% Plants == TRUE),]
randomization_info <- randomization(data2,1000,"R2","Plants")

pvalue_R2_tropical <- c(pvalue_R2_tropical,randomization_pvalue(randomization_info)[1])
pvalue_R2_non_tropical <- c(pvalue_R2_non_tropical,randomization_pvalue(randomization_info)[2])

p9 <- ggplot(randomization_info[[1]], aes(x=Distribution,fill=region,color=region))+
  geom_histogram(alpha=0.5,bins=50) +
  scale_color_manual(values=c(cols[3], cols[1])) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  ylab("Frequency\n")+
  xlab("\nNucleotide diversity") +
  ggtitle(gr) +
  labs(fill="Null distribution") +
  labs(color="Null distribution") +
  theme(legend.position="bottom",plot.title = element_text(size=14, face="bold"))+
  xlim(randomization_info[[3]][1], randomization_info[[3]][2]) +
  geom_vline(xintercept = randomization_info[[2]][1],size=2, colour=cols[3])+
  geom_vline(xintercept = randomization_info[[2]][2],size=2, colour=cols[1])

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, labels = c('a)', 'b)', "c)", "d)", "e)", "f)", "g)", "h)", "i)"))

### Bonferroni correction - pvalues
### Nucleotide diversity
### Tropical
pvalue_Nucleotide_diversity_tropical_ajusted <- round(p.adjust(pvalue_Nucleotide_diversity_tropical, method = "bonferroni"),digits=3)
names(pvalue_Nucleotide_diversity_tropical_ajusted) <- tax_groups

### Non_tropical
pvalue_Nucleotide_diversity_non_tropical_ajusted <- round(p.adjust(pvalue_Nucleotide_diversity_non_tropical, method = "bonferroni"),digits=3)
names(pvalue_Nucleotide_diversity_non_tropical_ajusted) <- tax_groups

### Tajima's D
### Tropical
pvalue_D_tropical_ajusted <- round(p.adjust(pvalue_D_tropical, method = "bonferroni"),digits=3)
names(pvalue_D_tropical_ajusted) <- tax_groups

### Non_tropical
pvalue_D_non_tropical_ajusted <- round(p.adjust(pvalue_D_non_tropical, method = "bonferroni"),digits=3)
names(pvalue_D_non_tropical_ajusted) <- tax_groups

### R2
### Tropical
pvalue_R2_tropical_ajusted <- round(p.adjust(pvalue_R2_tropical, method = "bonferroni"),digits=3)
names(pvalue_R2_tropical_ajusted) <- tax_groups

### Non_tropical
pvalue_R2_non_tropical_ajusted <- round(p.adjust(pvalue_R2_non_tropical, method = "bonferroni"),digits=3)
names(pvalue_R2_non_tropical_ajusted) <- tax_groups
