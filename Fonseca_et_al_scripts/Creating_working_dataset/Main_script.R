###############################################################################################
# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity #
                  # Fonseca EM; Pelletier TA, Decker SK,Parsons DJ, Carstens BC #
                                    # September 2021 #
###############################################################################################

library(ape)
library(pegas)
library(testthat)

### Path to the scripts folder
path_to_folder <- "~/Desktop"

source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/filter_folders.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/gene_selection.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/occurences.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/filtering_alignment.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/georeferenced_data.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/removing_duplicates.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/calculating_centroid.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/nucleotide_diversity.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/calculating_codon_position.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/saving_results.R"))
source(file.path(path_to_folder,"Fonseca_et_al_scripts/Creating_working_dataset/quiet.R"))

### Creating resultds data frame
Results <- data.frame(matrix(NA,0,14))
colnames(Results) <- c("Group", "Species","Nucleotide_diversity","TajimasD","D_pvalue","R2","R2_pvalue,","Latitude","Longitude","Gene","mut_1_bp","mut_2_bp","mut_3_bp","Geographic_region")

### Selecting taxonomic groups
taxonomic_groups <- c("Amphibia", "Reptilia", "Mammalia", "Aves","Myxini","Actinopterygii","Sarcopterygii","Elasmobranchii","Holocephali","Insecta","Magnoliopsida","Pinopsida","Liliopsida","Florideophyceae","Ulvophyceae","Polypodiopsida","Bryopsida","Bangiophyceae","Jungermanniopsida","Psilotopsida","Pteridopsida","Chlorophyceae","Cycadopsida","Lycopodiopsida","Gnetopsida","Klebsormidiophyceae","Zygnematophyceae","Charophyceae","Polytrichopsida","Trebouxiophyceae","Compsopogonophyceae","Arachnida")

### Path to phylogatr folder
path_folders <- "/Users/emanuelfonseca/Desktop/phylogatr-results"

### Filtering folders
folders <- filter_folders(path_folders,taxonomic_groups)

for (x in 1:length(folders)){
  
  if (x == 1){
    print(noquote("Creating working dataset"))
    progress <- txtProgressBar(min = 0, max = length(folders), style = 3)
  }
  
  setwd(path_folders)
  setwd(folders[x])
  species_info <- strsplit(folders[x], "/")
  
  ### Selecting genes
  gene <- gene_selection(getwd())
  
  if (!identical(list.files(pattern=paste0(gene,".afa")), character(0))){
    
    ### Extracting geographic coordinates
    occ_ <- occurences(getwd())
    occ <- occ_[[1]]
    knob <- occ_[[2]]
    
    ### Reading alignment
    alignment <- readLines(list.files(pattern=paste0(gene,".afa")))
    
    ### Removing sequences with more than 50% of missing data
    alignment <- filtering_alignment(alignment,knob)
    
    ### Getting georeferenced data 
    geo_data <- georeferenced_data(alignment, occ, knob)
    acesssion_georeferenced <- geo_data[[1]]
    accession <- geo_data[[2]]
    
    if (!identical(acesssion_georeferenced, character(0))){
      
      ### Removing potential duplicates
      new_alignment <- removing_duplicates(alignment,occ,accession,acesssion_georeferenced)[[1]]
      new_occ <- removing_duplicates(alignment,occ,accession,acesssion_georeferenced)[[2]]
      
      ### Extracting centroid from coordinates
      centroid_occ <- calculating_centroid(new_occ)
      
      if(length(new_alignment) > 2){
        
        #### Calculating pairwise nucleotide diversity
        nucleotide_div <- nucleotide_diversity(new_alignment)
        
        if(length(nucleotide_div) > 0){
          
          ## Calculating mean nucleotide diversity
          nuc_div <- mean(nucleotide_div)
          
          ### Saving filtered alignment
          write.table(toupper(new_alignment), paste0("@Alignment_",gene,".fas"), quote = FALSE, row.names = FALSE, col.names = FALSE)
          
          ### Reading filtered alignment
          Alignment <- read.FASTA(paste0("@Alignment_",gene,".fas"))
          
          #### Calculating Tajima's D using pegas
          TajimasD <- suppressWarnings(tajima.test(Alignment))
          
          #### Calculating R2 using pegas
          R2 <- quiet(R2.test(Alignment, plot=F))
          
          ### Calculating mutational position
          mut_by_bp <- calculating_codon_mutation(new_alignment,species_info[[1]][2])
          
          ### Deleting filtered alignment
          unlink(paste0("@Alignment_",gene,".fas"))
          
          ### Saving results results
          Results_ <- saving_results(species_info,nuc_div,TajimasD,R2,centroid_occ,mut_by_bp)
          
          if(!any(is.na(Results_))){
            Results <- rbind(Results, Results_)
          }
          
        }}}}
  
  setTxtProgressBar(progress, x)
  
  if (x ==length(folders)){
    close(progress)
  }
}

write.table(Results,file.path(path_to_folder,"/Fonseca_et_al_scripts/Working_dataset/Working_dataset.txt"), row.names = F, quote = F)
