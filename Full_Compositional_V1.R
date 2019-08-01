#Set the directory
setwd(" ") #Set directory path

#File containing the functions
source("Functions.R")

#File-information
A_file <- "pS6_All_idiopathic.csv"
B_file <- "pS6_Sentinel.csv"

#Average analyzed area in um^2
area = 100000

#Choose which columns you want to collapse for two-compositional analysis.
#In the example input data, columns 1,2 and 3 are for weak, medium, and strong staining.
#Therefore choosing 1, 2, and 3 collapses them into a column for "positive" staining.
collapsed_vector1 <- c(1,2,3)
collapsed_vector2 <- c(4)

#Follicles
cat("\nFollicles")
s <- Pull(A_file, c(1:4))
c <- Pull(B_file, c(1:4))
Report(s, c, collapsed_vector1, collapsed_vector2)

#GC
cat("\nGerminal Centers")
s <- Pull(A_file, c(5:8))
c <- Pull(B_file, c(5:8))
Report(s, c, collapsed_vector1, collapsed_vector2)

#MZ
cat("\nMantle Zones")
s <- Pull(A_file, c(9:12))
c <- Pull(B_file, c(9:12))
Report(s, c, collapsed_vector1, collapsed_vector2)

#Interfollicular
cat("\nInterfollicular Zones")
s <- Pull(A_file, c(13:16))
c <- Pull(B_file, c(13:16))
Report(s, c, collapsed_vector1, collapsed_vector2)