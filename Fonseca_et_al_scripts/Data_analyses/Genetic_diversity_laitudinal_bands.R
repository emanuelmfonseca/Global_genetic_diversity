###############################################################################################
# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity #
              # Fonseca EM; Pelletier TA, Decker SK,Parsons DJ, Carstens BC #
                              # September 2021 #
###############################################################################################
library(ggplot2)
library(cowplot)
library(colortools)
source(file.path(path_to_folder,"/Fonseca_et_al_scripts/Data_analyses/Latitudinal_bands.R"))

cols <- splitComp("firebrick")

### Path to the scripts folder
path_to_folder <- "~/Desktop"

### Reading working
data <- read.table(file.path(path_to_folder,"Fonseca_et_al_scripts/Working_dataset/Working_dataset.txt"),h=T)

### Taxonomic groups
Vertebrates <- c("Amphibia", "Reptilia", "Mammalia", "Aves","Myxini","Actinopterygii","Sarcopterygii","Elasmobranchii","Holocephali")
Insects <- "Insecta"
Plants <- c("Magnoliopsida","Pinopsida","Liliopsida","Florideophyceae","Ulvophyceae","Polypodiopsida","Bryopsida","Bangiophyceae","Jungermanniopsida","Psilotopsida","Pteridopsida","Chlorophyceae","Cycadopsida","Lycopodiopsida","Gnetopsida","Klebsormidiophyceae","Zygnematophyceae","Charophyceae","Polytrichopsida","Trebouxiophyceae","Compsopogonophyceae")
Arachnids <- "Arachnida"

### Calculating nucleotide diversity over latitudinal bands of 10 degrees
### Vertebrates
lat_band <- calculating_latitudinal_bands(data[which(data[,1] %in% Vertebrates == TRUE),])

p1 <- ggplot(lat_band, aes(x=lat_band, y=Nucleotide_diversity,fill=Geographic_region)) +
  geom_errorbar(aes(ymax = Nucleotide_diversity+sd, ymin = ifelse(Nucleotide_diversity-sd < 0, 0, Nucleotide_diversity-sd))) +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  theme(legend.position="none") +
  xlab("Latitudinal bands")+
  ylab("\nNucleotide diversity") +
  coord_flip() +
  ggtitle("Vertebrates")

### Insects
lat_band <- calculating_latitudinal_bands(data[which(data[,1] %in% Insects == TRUE),])

p2 <- ggplot(lat_band, aes(x=lat_band, y=Nucleotide_diversity,fill=Geographic_region)) +
  geom_errorbar(aes(ymax = Nucleotide_diversity+sd, ymin = ifelse(Nucleotide_diversity-sd < 0, 0, Nucleotide_diversity-sd))) +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  theme(legend.position="none") +
  xlab("Latitudinal bands")+
  ylab("\nNucleotide diversity") +
  coord_flip() +
  ggtitle("Insects")

### Plants
lat_band <- calculating_latitudinal_bands(data[which(data[,1] %in% Plants == TRUE),])

p3 <- ggplot(lat_band, aes(x=lat_band, y=Nucleotide_diversity,fill=Geographic_region)) +
  geom_errorbar(aes(ymax = Nucleotide_diversity+sd, ymin = ifelse(Nucleotide_diversity-sd < 0, 0, Nucleotide_diversity-sd))) +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  theme(legend.position="none") +
  xlab("Latitudinal bands")+
  ylab("\nNucleotide diversity") +
  coord_flip() +
  ggtitle("Plants")

### Arachnids
lat_band <- calculating_latitudinal_bands(data[which(data[,1] %in% Arachnids == TRUE),])

p4 <- ggplot(lat_band, aes(x=lat_band, y=Nucleotide_diversity,fill=Geographic_region)) +
  geom_errorbar(aes(ymax = Nucleotide_diversity+sd, ymin = ifelse(Nucleotide_diversity-sd < 0, 0, Nucleotide_diversity-sd))) +
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  scale_fill_manual(values=c(cols[3], cols[1])) +
  theme(legend.position="none") +
  xlab("Latitudinal bands")+
  ylab("\nNucleotide diversity") +
  coord_flip() +
  ggtitle("Arachnids")

plot_grid(p1, p2, p3, p4, labels = c('a)', 'b)', "c)", "d)"))
