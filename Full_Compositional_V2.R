#Filename: compo_analyzeositional_V2.R
library(car)

#-------------------------
#Sourcing additional files
#-------------------------
#Set the directory
setwd("C:/Users/arenasd/Desktop/FullCompositional_Updated013120")
print(getwd())
#Source the file containing the functions
source("functions.R")

#------------------------------
#Experiment specific parameters
#-------------------------------
#Define resolution, so that you can replace zeros with this value
area <- 10000
resolution <<- 1/area

#-------------------------------
#Input-files information
#-------------------------------
a_file <- "pS6_122017_TAFRO.csv"
b_file <- "pS6_122017_Sentinels.csv"
#Set a few global variables
percentage <<- FALSE     #Is the input data in percentage? If not, it should be in proportions
paired_boolean <<- FALSE #Are you comparing paired data?
t_test_alternative <<- "greater"   #two.sided, less, or greater

#-----------------------------------------------------------------
#Choosing which columns you are interested in and which to collapse for two-compositional analysis
#------------------------------------------------------------------
wanted_columns <- c(13:16)
collapsed_vector1 <- c(1,2,3)
collapsed_vector2 <- c(4)

#-----------------------------------
#Perform Full Compositional analysis
#-----------------------------------
#Pull the collapsed columns using the function  "pull" into data frames
s_df <- pull(a_file, wanted_columns)
c_df <- pull(b_file, wanted_columns)
#Run two compositional analysis on the collapsed columns
r2 = two_compo_analyze(s_df,c_df,collapsed_vector1,collapsed_vector2)
#Run full compositional analysis
rf = compo_analyze(s_df,c_df)
#Write to file
print(r2)
print(rf)
write.csv(r2, "C:/Users/arenasd/Desktop/output_two_compo_analysis.csv")
write.csv(rf, "C:/Users/arenasd/Desktop/output_full_compo_analysis.csv")