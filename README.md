# Full_Compositional_Analysis
The software is intended for statistical analysis of proportions that add up to unity. 
This upload acts as a repository for the compositional analysis software used for the analysis of IHC data for the manuscript "Increased mTOR activation in idiopathic multicentric Castleman disease" by Arenas et al. All code is in R.

This version, V1, is specifically for comparing proportions of different IHC staining strengths between a subject group and a comparison group.  This version of the software will perform a centered log-rate transformation, a Mann-Whitney U test comparison between the two groups, and report p-values.

Example input data files are provided (pS6_All_idiopathic.csv and pS6_Sentinel.csv). Two example input files are provided, one for subjects and one for controls. Each file has the percentages of weak, medium, strong, and negative staining for each individual. The file can contain multiple data for each subject (i.e. the four percentages for different lymph-node structures). 

Future versions of the software will offer different options for input file formats
