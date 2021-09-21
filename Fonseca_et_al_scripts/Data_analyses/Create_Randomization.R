randomization <- function(data,n_rand,summary_stat,gr){

data_rand <- data.frame(matrix(NA,length(data[,1]),n_rand))

for (x in 1:n_rand){
  data_rand[,x] <- sample(data[,"Geographic_region"],r=F)
}

data_rand <- cbind(data[,summary_stat],data$Geographic_region,data_rand)

non_tropical <- numeric()
tropical <- numeric()
for (x in 1:(n_rand+1)){
  non_tropical <- c(non_tropical, mean(data_rand[which(data_rand[,x+1] == "Non_Tropical"),1]))
  tropical <- c(tropical, mean(data_rand[which(data_rand[,x+1] == "Tropical"),1]))
}

obs_non_tropical <- non_tropical[1]
non_tropical <- non_tropical[-1]
obs_tropical <- tropical[1]
tropical <- tropical[-1]

distribution <- rbind(data.frame(non_tropical),data.frame(non_tropical))
distribution <- cbind(distribution,data.frame(rep(c("Non_Tropical","Tropical"),e=1000)))
colnames(distribution) <- c("Distribution","region")

min_value <- min(c(obs_non_tropical,non_tropical,obs_tropical,tropical))-0.0001
max_value <- max(c(obs_non_tropical,non_tropical,obs_tropical,tropical))+0.0001

randomization_info <- list(distribution,c(obs_non_tropical,obs_tropical),c(min_value,max_value))
names(randomization_info) <- c("Null_distribution","Observed","Min_Max")

return(randomization_info)

}