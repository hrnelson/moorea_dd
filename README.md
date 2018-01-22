# moorea_dd
*Density-dependence mediates coral community structure in Mo’orea, French Polynesia*

**Authors:** Edmunds, P. J., L. Bramanti, and H. R. Nelson

**Description:** This repository provides data and code to perform the simulation, analyze the results, and create the corresponding figures for the mensurative test in the above manuscript.

**Contents:** Four R script files, two data files, one output file, and a README.md file.

**Scripts:** 
* observed_NN_summary.R: This script imports observed_NN.csv and summarizes the observed nearest neighbor data.
* simulation.R: This script imports observed_sizes.csv, performs the simulation, and outputs the results as simulation_output.csv
* simulation_summary.R: This script imports both simulation_output.csv and observed_NN.csv, and summarizes the simulation.
* figures_grayscale.R: This script imports both simulation_output.csv and observed_NN.csv, and creates the figures that are combined in Figure 5 of the manuscript.

**Data:**
* observed_NN.csv: This is the raw data containing observed nearest neighbor distances
* observed_sizes.csv: This is the raw data containing observed sizes of corals

**Output:**
* simulation_output.csv: This is the output of the simulation produced by simulation.R
