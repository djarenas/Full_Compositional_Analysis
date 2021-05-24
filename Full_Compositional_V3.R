library(car)  #For Levene test

#----------------------------
#Directory and function-file
#----------------------------
#Set the directory
setwd("C:/Users/CCC/Desktop/FullCompositional_Updated022420")
print(getwd())
#Source the file containing the functions
source("functions.R")

#------------------------------
#Experiment specific parameters
#-------------------------------
#"Resolution": Replaces zeros with this value
area <- 10000
resolution <- 1/area
percentage <- TRUE #Is the input data in percentage instead of proportions?
paired_boolean <<- FALSE #Comparing paired data?
t_test_alternative <- "greater" #two.sided, less, or greater

#-------------------------------
#Input-files information
#Which columns are you interested in?
#Any columns you want to combine for 2-composition analysis?
#-------------------------------
a_file <- "Figure1_pS6_122017_TAFRO.csv"
b_file <- "Figure1_pS6_122017_Sentinels.csv"
wanted_columns <- c(1:4)
collapsed_vector1 <- c(1,2,3) #First set of columns to collapse into one
collapsed_vector2 <- c(4)     #Second set of columns to collapse into one

#---------------------------------------------------------------------
#Pull the columns you want from the file (pull)
#Replace zeros, change percentages to proportions (clean_modify)
#---------------------------------------------------------------------
s_df <- pull(a_file, wanted_columns)
c_df <- pull(b_file, wanted_columns)
s_df <- clean_modify(s_df, p_boolean, resolution)  
c_df <- clean_modify(c_df, p_boolean, resolution)  

#---------------------------------------------------------------------
#Perform Full Compositional analysis
#---------------------------------------------------------------------
#Full compositional analysis
rf = compo_analyze(s_df,c_df, paired_boolean, t_test_alternative)
print(rf)
write.csv(rf, "output_full_compo_analysis.csv")

#---------------------------------------------------------------------
#Perform two-compositional analysis
#---------------------------------------------------------------------
#Combine columns so that you only compare two proportions
s_df_2 = full_to_two(s_df, collapsed_vector1, collapsed_vector2)
c_df_2 = full_to_two(c_df, collapsed_vector1, collapsed_vector2)
r2 = compo_analyze(s_df_2,c_df_2, paired_boolean, t_test_alternative)
# write.csv(r2, "output_two_compo_analysis.csv")
print(r2)