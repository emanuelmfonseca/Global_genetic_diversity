# Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity

This repository contains the scripts used to generate the working dataset and analyze the data from Fonseca et al. (Fonseca EM; Pelletier TA, Decker SK,Parsons DJ, Carstens BC. Pleistocene glaciations caused the latitudinal gradient of within-species genetic diversity. In review).

## Step 1: Generating working dataset

 - Access phylogatr website (http://phylogatr.osc.edu) and download all the data available.
 - Access the folder **Creating_working_dataset**.
 - Only the script entitled **Main_script.R** is required to generate the working dataset. All other scripts contain functions imported by the main script.

## Step 2: Data analyses

 - All the analysis use the file generated in the previous step and that can be found in the folder **Working_dataset**.
 - The script entitled **Genetic_diversity_laitudinal_bands.R** is used to create latitudinal plot in Figure 1 and Figure S1.
 - The script entitled **Linear_regression.R** is used to perform the linear regression analysis and create Figure 1a–c;e–g.
 - The script entitled **Logistic_regression.R** is used to perform the logistic regression analysis and create Figure S2.
 - The script entitled **Randomization.R** is used to perform the randomization procedure and create the plot in Figure S3.
 - The script entitled **Fisher_test.R** is used to perform Fisher's test.
 
 *All other scripts contain functions imported by some of the previous scripts*

If you have any questions, please email emanuelmfonseca@gmail.com.
