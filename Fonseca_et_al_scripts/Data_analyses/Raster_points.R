raster_points <- function(y,x){

library(rgdal)
library(sp)
library(raster)
library(scales)

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
  }

x1 <- range01(x)
y1 <- range01(y)

e <- c(min(x1),max(x1),min(y1),max(y1))
e <- extent(e)

myPoints <- data.frame(x1,y1)
colnames(myPoints) <- c('x', 'y')
coordinates(myPoints) <- c('x', 'y')

bb <- bbox(myPoints)

cs <- 0.01 # grid size
myPointsGrid <- GridTopology(cellcentre.offset =  bb[,1] + (cs/2), cellsize = c(cs,cs), cells.dim = ceiling(diff(t(bb))/cs))
myPointsGrid <- SpatialGrid(myPointsGrid)

pts <- data.frame(x1,y1)
colnames(pts) <- c('x', 'y')
coordinates(pts) <- c('x', 'y')

grid_assignment<-table(over(pts,myPointsGrid))

density_points <- data.frame(matrix(NA,ceiling(diff(t(bb))/cs)[1]*ceiling(diff(t(bb))/cs)[2],3))
colnames(density_points) <- c("x", "y", "richness")

density_points[,c(1:2)] <- as.data.frame(myPointsGrid)

for (i in 1:length(grid_assignment)){
  index <- as.numeric(names(grid_assignment[i]))
  density_points[index,3] <- grid_assignment[i]
}

density_points[,1] <- rescale(density_points[,1], to=c(min(x),max(x)))
density_points[,2] <- rescale(density_points[,2], to=c(min(y),max(y)))

density_points <- density_points[-which(is.na(density_points[,3]) == TRUE),]

return(density_points)

}
