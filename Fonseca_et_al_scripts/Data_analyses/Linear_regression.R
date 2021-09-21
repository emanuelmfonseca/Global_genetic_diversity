###############################################################################################
# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity #
              # Fonseca EM; Pelletier TA, Decker SK,Parsons DJ, Carstens BC #
                                   # September 2021 #
###############################################################################################
library(ggplot2)
library(cowplot)
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Data_analyses/Raster_points.R"))

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
pvalue_D <- numeric()

### Linear regressions
### Vertebrates
linear_reg <- summary(lm(abs(TajimasD)~abs(Latitude), data[which(data[,1] %in% Vertebrates == TRUE),]))
pvalue_D <- c(pvalue_D,linear_reg$coefficients[2,"Pr(>|t|)"])

### Insects
linear_reg <- summary(lm(abs(TajimasD)~abs(Latitude), data[which(data[,1] %in% Insects == TRUE),]))
pvalue_D <- c(pvalue_D,linear_reg$coefficients[2,"Pr(>|t|)"])

### Plants
linear_reg <- summary(lm(abs(TajimasD)~abs(Latitude), data[which(data[,1] %in% Plants == TRUE),]))
pvalue_D <- c(pvalue_D,linear_reg$coefficients[2,"Pr(>|t|)"])

### Bonferroni correction
pvalues_ajusted_D <- round(p.adjust(pvalue_D, method = "bonferroni"),digits=3)
names(pvalues_ajusted_D) <- tax_groups

### Linear regression plots
cols <- rev(colorRampPalette(c("firebrick", "gold"))(255))
cols2 <- splitComp("gold")

data2 <- data[which(data[,1] %in% Vertebrates == TRUE),]
r <- raster_points(x=data2$Nucleotide_diversity,y=abs(data2$Latitude))

p1 <- ggplot(r, aes(x=x, y=y)) +
  ylab("|Latitude|")+
  xlab("Nucleotide Diversity") +
  geom_tile(data = r , aes(x = x, y = y, fill = log(richness))) +
  scale_fill_gradientn(name="richness",colours = cols) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(expand = FALSE) +
  geom_smooth(method=lm,col=cols2[2],se=F,size=1.5)

data2 <- data[which(data[,1] %in% Insects == TRUE),]
r <- raster_points(x=data2$Nucleotide_diversity,y=abs(data2$Latitude))

p2 <- ggplot(r, aes(x=x, y=y)) +
  ylab("|Latitude|")+
  xlab("Nucleotide Diversity") +
  geom_tile(data = r , aes(x = x, y = y, fill = log(richness))) +
  scale_fill_gradientn(name="richness",colours = cols) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(expand = FALSE) +
  geom_smooth(method=lm,col=cols2[2],se=F,size=1.5)

data2 <- data[which(data[,1] %in% Plants == TRUE),]
r <- raster_points(x=data2$Nucleotide_diversity,y=abs(data2$Latitude))

p3 <- ggplot(r, aes(x=x, y=y)) +
  ylab("|Latitude|")+
  xlab("Nucleotide Diversity") +
  geom_tile(data = r , aes(x = x, y = y, fill = log(richness))) +
  scale_fill_gradientn(name="richness",colours = cols) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(expand = FALSE) +
  geom_smooth(method=lm,col=cols2[2],se=F,size=1.5)

data2 <- data[which(data[,1] %in% Vertebrates == TRUE),]
r <- raster_points(x=abs(data2$TajimasD),y=abs(data2$Latitude))

p4 <- ggplot(r, aes(x=x, y=y)) +
  ylab("|Latitude|")+
  xlab("|Tajima's D|") +
  geom_tile(data = r , aes(x = x, y = y, fill = log(richness))) +
  scale_fill_gradientn(name="richness",colours = cols) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(expand = FALSE) +
  geom_smooth(method=lm,col=cols2[2],se=F,size=1.5)

data2 <- data[which(data[,1] %in% Insects == TRUE),]
r <- raster_points(x=abs(data2$TajimasD),y=abs(data2$Latitude))

p5 <- ggplot(r, aes(x=x, y=y)) +
  ylab("|Latitude|")+
  xlab("|Tajima's D|") +
  geom_tile(data = r , aes(x = x, y = y, fill = log(richness))) +
  scale_fill_gradientn(name="richness",colours = cols) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(expand = FALSE) +
  geom_smooth(method=lm,col=cols2[2],se=F,size=1.5)

data2 <- data[which(data[,1] %in% Plants == TRUE),]
r <- raster_points(x=abs(data2$TajimasD),y=abs(data2$Latitude))

p6 <- ggplot(r, aes(x=x, y=y)) +
  ylab("|Latitude|")+
  xlab("|Tajima's D|") +
  geom_tile(data = r , aes(x = x, y = y, fill = log(richness))) +
  scale_fill_gradientn(name="richness",colours = cols) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(expand = FALSE) +
  geom_smooth(method=lm,col=cols2[2],se=F,size=1.5)

plot_grid(p1, p2, p3, p4, p5, p6, labels = c('a)', 'b)', "c)","e)","f)", "g)"))
