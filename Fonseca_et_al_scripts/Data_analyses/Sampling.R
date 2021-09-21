###############################################################################################
# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity #
            # Fonseca EM; Pelletier TA, Decker SK,Parsons DJ, Carstens BC #
                                   # September 2021 #
###############################################################################################
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(rgdal)
library(ggplot2)
library(broom)
library(sf)
library(raster)
library(sp)
library(maps)
library(hexbin)
library(RColorBrewer)

### Path to the scripts folder
path_to_folder <- "~/Desktop"

### Reading working
data <- read.table(file.path(path_to_folder,"Fonseca_et_al_scripts/Working_dataset/Working_dataset.txt"),h=T)

### Getting shapefiles
### LGM glaciation
my_spdf <- shapefile(file.path(path_to_folder,"Fonseca_et_al_scripts/Shapefiles_and_rasters/LGM/lgm.shp"))

### LGM permafrost
permafrost <- raster(file.path(path_to_folder,"Fonseca_et_al_scripts/Shapefiles_and_rasters/Permafrost/Permafrost.tif"))
permafrost2 <- as.data.frame(permafrost,xy=T)

xy <- xyFromCell(permafrost, 1:ncell(permafrost))
v <- getValues(permafrost) 
permafrost2 <- data.frame(xy, v)

colnames(permafrost2) <- c("x","y","Permafrost")
permafrost2 <- permafrost2[complete.cases(permafrost2),]

### Preparing shapefile for ggplot2
spdf_fortified <- tidy(my_spdf)

### Getting world map
world <- map_data("world")

### Figure 2
pdf("~/Desktop/oi.pdf")
ggplot()+ 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#767676", color = "#767676")+
  geom_tile(data = permafrost2, aes(x = x, y = y, group = Permafrost), fill="#C6DCEF", color= "#C6DCEF", alpha = .8) +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#6AA1DB", color= NA, alpha = .9) +
  geom_hex(data, bins=100, mapping=aes(x=Longitude, y=Latitude)) +
  geom_tile(data = permafrost2, aes(x = x, y = y, group = Permafrost), fill="#C6DCEF",color= NA, alpha = .6) +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#6AA1DB", color= NA,  alpha = .4) +
  scale_fill_gradient(
    low = "#F7EB39", high = "#B22222", guide = "colourbar", aesthetics = "fill", trans = "log") +
  theme(
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    panel.grid.major = element_line(color = "#EFEFEF", size = 0.2),
    panel.grid.minor = element_line(color = "#EFEFEF", size = 0.2),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.background = element_rect(fill = "#FFFFFF", color = NA), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 
dev.off()
#####Sampling Maps

### Taxonomic groups
Vertebrates <- c("Amphibia", "Reptilia", "Mammalia", "Aves","Myxini","Actinopterygii","Sarcopterygii","Elasmobranchii","Holocephali")
Insects <- "Insecta"
Plants <- c("Magnoliopsida","Pinopsida","Liliopsida","Florideophyceae","Ulvophyceae","Polypodiopsida","Bryopsida","Bangiophyceae","Jungermanniopsida","Psilotopsida","Pteridopsida","Chlorophyceae","Cycadopsida","Lycopodiopsida","Gnetopsida","Klebsormidiophyceae","Zygnematophyceae","Charophyceae","Polytrichopsida","Trebouxiophyceae","Compsopogonophyceae")

### Creating seperate data frames
data_vert <- data[which(data[,1] %in% Vertebrates == TRUE),]
data_insect <- data[which(data[,1] %in% Insects == TRUE),]
data_plant <- data[which(data[,1] %in% Plants == TRUE),]
data_arac <- data[which(data[,1] %in% Arachnids == TRUE),]

### Plotting figures by grpup
### Vertebrates
ggplot()+ 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#767676", color = "#767676", size = .05)+
  geom_hex(data_vert, bins=80, mapping=aes(x=Longitude, y=Latitude)) +
  scale_fill_gradient(
    low = "#F7EB39", high = "#B22222", guide = "colourbar", aesthetics = "fill", trans = "log") +
  theme(
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    panel.grid.major = element_line(color = "#EFEFEF", size = 0.2),
    panel.grid.minor = element_line(color = "#EFEFEF", size = 0.2),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.background = element_rect(fill = "#FFFFFF", color = NA), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 

### Insects
ggplot()+ 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#767676", color = "#767676", size = .05)+
  geom_hex(data_insect, bins=80, mapping=aes(x=Longitude, y=Latitude)) +
  scale_fill_gradient(
    low = "#F7EB39", high = "#B22222", guide = "colourbar", aesthetics = "fill", trans = "log") +
  theme(
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    panel.grid.major = element_line(color = "#EFEFEF", size = 0.2),
    panel.grid.minor = element_line(color = "#EFEFEF", size = 0.2),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.background = element_rect(fill = "#FFFFFF", color = NA), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 

### Plants
ggplot()+ 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#767676", color = "#767676", size = .05)+
  geom_hex(data_plant, bins=80, mapping=aes(x=Longitude, y=Latitude)) +
  scale_fill_gradient(
    low = "#F7EB39", high = "#B22222", guide = "colourbar", aesthetics = "fill", trans = "log") +
  theme(
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    panel.grid.major = element_line(color = "#EFEFEF", size = 0.2),
    panel.grid.minor = element_line(color = "#EFEFEF", size = 0.2),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.background = element_rect(fill = "#FFFFFF", color = NA), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 

### Arachnids
ggplot()+ 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#767676", color = "#767676", size = .05)+
  geom_hex(data_arac, bins=80, mapping=aes(x=Longitude, y=Latitude)) +
  scale_fill_gradient(
    low = "#F7EB39", high = "#B22222", guide = "colourbar", aesthetics = "fill", trans = "log") +
  theme(
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    panel.grid.major = element_line(color = "#EFEFEF", size = 0.2),
    panel.grid.minor = element_line(color = "#EFEFEF", size = 0.2),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    panel.background = element_rect(fill = "#FFFFFF", color = NA), 
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 
