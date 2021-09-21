###############################################################################################
# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity #
              # Fonseca EM; Pelletier TA, Decker SK,Parsons DJ, Carstens BC #
                                # September 2021 #
###############################################################################################
library(ggplot2)
library(cowplot)

### Path to the scripts folder
path_to_folder <- "~/Desktop"

### Reading working
data <- read.table(file.path(path_to_folder,"Fonseca_et_al_scripts/Working_dataset/Working_dataset.txt"),h=T)

### Taxonomic groups
Vertebrates <- c("Amphibia", "Reptilia", "Mammalia", "Aves","Myxini","Actinopterygii","Sarcopterygii","Elasmobranchii","Holocephali")
Insects <- "Insecta"
Plants <- c("Magnoliopsida","Pinopsida","Liliopsida","Florideophyceae","Ulvophyceae","Polypodiopsida","Bryopsida","Bangiophyceae","Jungermanniopsida","Psilotopsida","Pteridopsida","Chlorophyceae","Cycadopsida","Lycopodiopsida","Gnetopsida","Klebsormidiophyceae","Zygnematophyceae","Charophyceae","Polytrichopsida","Trebouxiophyceae","Compsopogonophyceae")

tax_groups <- c("Vertebrates", "Insects", "Plants")

### Converting geographic areas to binary (i.e., 0 and 1)
data <- data %>% mutate(Geographic_region = ifelse(Geographic_region == "Tropical",0,1))

### Creating an empty vector to save the results
pvalue_Nucleotide_diversity <- numeric()
pvalue_D <- numeric()
pvalue_R2 <- numeric()

### Logistic regressions
### Vertebrates
data2 <- data[which(data[,1] %in% Vertebrates == TRUE),]
logistic_reg <- glm(Geographic_region~Nucleotide_diversity, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_Nucleotide_diversity <- c(pvalue_Nucleotide_diversity,res$coefficients[2,"Pr(>|z|)"])

logistic_reg <- glm(Geographic_region~TajimasD, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_D <- c(pvalue_D,res$coefficients[2,"Pr(>|z|)"])

logistic_reg <- glm(Geographic_region ~ R2, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_R2 <- c(pvalue_R2,res$coefficients[2,"Pr(>|z|)"])

### Insects
data2 <- data[which(data[,1] %in% Insects == TRUE),]
logistic_reg <- glm(Geographic_region~Nucleotide_diversity, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_Nucleotide_diversity <- c(pvalue_Nucleotide_diversity,res$coefficients[2,"Pr(>|z|)"])

logistic_reg <- glm(Geographic_region~TajimasD, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_D <- c(pvalue_D,res$coefficients[2,"Pr(>|z|)"])

logistic_reg <- glm(Geographic_region ~ R2, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_R2 <- c(pvalue_R2,res$coefficients[2,"Pr(>|z|)"])

### Plants
data2 <- data[which(data[,1] %in% Plants == TRUE),]
logistic_reg <- glm(Geographic_region~Nucleotide_diversity, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_Nucleotide_diversity <- c(pvalue_Nucleotide_diversity,res$coefficients[2,"Pr(>|z|)"])

logistic_reg <- glm(Geographic_region~TajimasD, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_D <- c(pvalue_D,res$coefficients[2,"Pr(>|z|)"])

logistic_reg <- glm(Geographic_region ~ R2, family = binomial, data = data2)
res <- summary(logistic_reg)
pvalue_R2 <- c(pvalue_R2,res$coefficients[2,"Pr(>|z|)"])

### Bonferroni correction
pvalues_ajusted_Nucleotide_diversity <- round(p.adjust(pvalue_Nucleotide_diversity, method = "bonferroni"),digits=3)
names(pvalues_ajusted_Nucleotide_diversity) <- tax_groups

pvalues_ajusted_D <- round(p.adjust(pvalue_D, method = "bonferroni"),digits=3)
names(pvalues_ajusted_D) <- tax_groups

pvalues_ajusted_R2 <- round(p.adjust(pvalue_R2, method = "bonferroni"),digits=3)
names(pvalues_ajusted_R2) <- tax_groups

### Logistic regression plots
p1 <- ggplot(data[which(data[,1] %in% Vertebrates == TRUE),], aes(x=Nucleotide_diversity, y=Geographic_region)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),col="blue") + 
  ylab("Geographic region") +
  xlab("Nucleotide diversity")

p2 <- ggplot(data[which(data[,1] %in% Insects == TRUE),], aes(x=Nucleotide_diversity, y=Geographic_region)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),col="blue") + 
  ylab("Geographic region") +
  xlab("Nucleotide diversity")

p3 <- ggplot(data[which(data[,1] %in% Plants == TRUE),], aes(x=Nucleotide_diversity, y=Geographic_region)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),col="blue") + 
  ylab("Geographic region") +
  xlab("Nucleotide diversity")

plot_grid(p1, p2, p3, labels = c('a)', 'b)', "c)"))
