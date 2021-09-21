calculating_latitudinal_bands <- function(data){
  
  lat_band <- numeric()
  for (x in 1:length(data[,1])){
    lat_band <- c(lat_band, round(data[x,"Latitude"]/10) * 10)
  }
  
  data <- cbind(data,lat_band)
  
  data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sd = sd(x[[col]], na.rm=TRUE))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  
  Results_ <- data_summary(data,"Nucleotide_diversity","lat_band")
  
  Geographic_region <- numeric()
  for (x in Results_$lat_band){
    if (x > 23.5 || x < -23.5){
      Geographic_region <- c(Geographic_region, "Temperate")
    } else {
      Geographic_region <- c(Geographic_region, "Tropics")
    }
  }
  
  Results_ <- cbind(Results_, data.frame(Geographic_region))
  
  return(Results_)
}
