# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity

This repository contains the scripts used to generate the working dataset and analyze the dataset used in Fonseca et al. (Fonseca EM, Pelletier TA, Decker SK,Parsons DJ, Carstens BC. Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity. In review).

## Step 1: Generating working dataset

 - Access the phylogatR website (http://phylogatr.osc.edu) and download all the data available.
 - Open the folder **Fonseca_et_al_scripts** and access the subfolder **Creating_working_dataset**.
 - Only the script entitled **Main_script.R** is required to generate the working dataset. All other scripts in this subfolder contain functions imported by the main script.
 - Running **Main_script.R** will generate an output file that can then be used to perform the following analyses. 

## Step 2: Data analyses

 - All the analyses listed below can be run using scripts found in the **Fonseca_et_al_scripts** subfolder, **Data_analyses** and use the file generated in the previous step as input.
   - This version of this file used in the manuscript can also be found in the **Fonseca_et_al_scripts** subfolder, **Working_dataset**  
 - The script entitled **Genetic_diversity_laitudinal_bands.R** is used to create the latitudinal plot in Figure 1 and Figure S1.
 - The script entitled **Linear_regression.R** is used to perform the linear regression analyses and create Figure 2a–c;e–g.
 - The script entitled **Sampling.R** is used to create Figure 2d and Figure S4–S7.
 - The script entitled **Logistic_regression.R** is used to perform the logistic regression analyses and create Figure S2.
 - The script entitled **Randomization.R** is used to perform the randomization procedure and create the plot in Figure S3.
 - The script entitled **Fisher_test.R** is used to perform Fisher's test.
 
 *All other scripts contain functions imported by some of the previous scripts*

If you have any questions, please email emanuelmfonseca@gmail.com.
