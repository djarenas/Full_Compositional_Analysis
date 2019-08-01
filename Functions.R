Geometric_mean <- function(vector){
  #Input: A vector of floats (any size)
  #Output: The geometric mean as a float
  mul = 1
  for (xi in vector){
    mul = mul*xi
  }
  exponent = 1/length(vector)
  gm = mul^(exponent)
  return(gm)
}

DFtoGM <- function(df){
  #Input: Dataframe where columns are categories (k = 1,2,...) and rows are samples/subjects
  #Output: A vector of geometric means for each row.
  gm = NULL
  for (i in c(1:nrow(df))){
    #Find geometric mean for that i (sample)  
    x_i = df[i,]
    gm_i = Geometric_mean(x_i)
    gm = rbind(gm, gm_i)
  }
  return(gm)
}

CLRT <- function(df){
  #Input: Dataframe (xik), where columns are categories (k = 1,2,...) and rows are samples/subjects i=1,2...)
  #Output: Dataframe with centered log-rate transformation (Zik)
  z_df = NULL
  for (i in c(1:nrow(df))){
    x_i = df[i,]
    gm_i = Geometric_mean(x_i)
    z_i = log(x_i/gm_i)
    z_df = rbind(z_df, z_i )
  }
  return(z_df)
}

Pull <- function(s_file, st_vectors){
  #Input: 1) File name. 2) The columns you want to pull (i.e. Follicles are 1-4, GC are 5-8,...)
  #Output: A DataFrame with only the data you want. 
  
  #Read percertanges from Files
  a = read.csv(s_file, header = TRUE)

  #Convert entire file from percentages to proportions, and replace zero values with 1/area
  a = a/100
  a[a==0]<- 1/area

  #Pull out the structure you are interested in
  pd = a[,st_vectors]

  return(pd)
}

Full_comp <- function(df1,df2){
  #Input: Two data frames to compare by compositional analysis.
  #Output: A vector of the p-values for each composition (i.e. weak, medium,...) comparison.

  #Check that both files have values bounded between 0 and 1.
  negatives = sum(df1<0, na.rm = TRUE)+sum(df2<0, na.rm = TRUE)
  overs = sum(df1>1, na.rm = TRUE)+sum(df2>1, na.rm = TRUE)
  nulls = sum(is.na(df1)) + sum(is.na(df2))
  if (negatives + overs + nulls != 0){
    print("Cannot perform compositional analysis")
    print("Please ensure all entries are numerical and bounded between 0 and 1")
    return(NULL)
  }  
    
  #Perform centered log-ratio transformation
  z_subj = CLRT(df1)
  z_ctrl = CLRT(df2)
    
  #Perform Wilcox t-test on the Z
  t_vector = vector(mode = "numeric", length = 0 )
  for (k in c(1:ncol(z_subj))){
    t = wilcox.test(z_subj[,k], z_ctrl[,k],var.equal = FALSE, alternative="two.sided")
    t_vector = append(t_vector, t$p.value)    
  }
  return(t_vector)
}

full_to_two<-function(df,c_vector1, c_vector2){
  #Input: A data frame of n columns. A vector specifying which columns to add into one.
  #Output: Two columns data frame
  collapsed1 = rowSums(df[c_vector1])
  collapsed2 = rowSums(df[c_vector2])
  return(cbind(collapsed1, collapsed2))
}

Report <- function(df1, df2, c_vector1, c_vector2){
  #Two-compositional
  s1 = full_to_two(df1,c_vector1, c_vector2)
  c1 = full_to_two(df2,c_vector1, c_vector2)
  cat("\nTwo-compositional test. p-values: \n")
  print(Full_comp(s1,c1))
  
  #Four-compositional
  cat("Four-compositional test. p-values: \n")
  print(Full_comp(df1, df2))
}