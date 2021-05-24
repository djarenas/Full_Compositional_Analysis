#------------------------------------
calc_geom_mean <- function(vector){
  #Input: A vector, of any size, of numeric values
  #Output: The geometric mean 

  mul = 1
  for (xi in vector){
    mul = mul*xi
  }
  exponent = 1/length(vector)
  gm = mul^(exponent)
  return(gm)
}
#------------------------------------
calc_gm_eachrow <- function(df){
  #Input: dataframe 
  #   Columns (k = 1,2,...) are compositions
  #   Rows (i = 1, 2, ...) are subjects
  #Output: A vector of geometric means for each row.

  gm = NULL  #A vector of geometric means
  for (i in c(1:nrow(df))){
    x_i = df[i,]  #Pick ith row (ith subject)
    gm_i = calc_geom_mean(x_i) #Find the geometric mean accross all compositions
    gm = rbind(gm, gm_i) #Update the gm vector with the new geometric mean
  }
  return(gm)
}
#------------------------------------
transform_clr <- function(df){
  #Input: dataframe 
  #   Columns (k = 1,2,...) are compositions 
  #   Rows (i = 1, 2, ...) are subjects
  #Output: dataframe where every value has been centered log-rate transformed 
  
  z_df = NULL  #dataframe of centered log-rate transformed values
  #for every subject
  for (i in c(1:nrow(df))){
    x_i = df[i,] #Pick ith subject
    gm_i = calc_geom_mean(x_i) #Calculate geoemetric mean
    z_i = log(x_i/gm_i)  #CLR transformation
    z_df = rbind(z_df, z_i ) #Update results dataframe
  }
  return(z_df)
}
#------------------------------------
clr_only_two <- function(d1){
  #Input: dataframe 
  #   Columns (k = 1,2,...) are compositions 
  #   Rows (i = 1, 2, ...) are subjects
  #Output: dataframe where every value has been centered log-rate transformed 
  z_d1 = NULL
  for (j in c(1:ncol(d1))){
    x_j = d1[, j ]
    gm_j = sqrt(x_j*(1-x_j))
    z_j = log(x_j/gm_j)
    z_d1 = cbind(z_d1, z_j)
  }
  colnames(z_d1) <- c(1:ncol(d1))
  return(z_d1)
}
#------------------------------------------------------------------------------
pull <- function(s_file, st_vectors, p_boolean){
  #Input: 
  #   s_file: File name (string) 
  #   st_vectors: Vector of column numbers you want to pull 
  #   p_boolean: Is the data on percentages instead of proportion? (boolean)
  #Output: dataframe 
  
  #Read values from Files
  a = read.csv(s_file, header = TRUE)
  #Pull out the structure you are interested in
  pd = a[,st_vectors]
  return(pd)
}
#------------------------------------------------------------------------------
clean_modify <- function(df, p_boolean, resolution){
  #Input: dataframe
  #Output: dataframe 
  
  #If they are percentages, divide by 100 to get proportions
  if (percentage == TRUE){df = df/100}
  #Replace zero values with resolution
  df[df==0]<- resolution
  return(df)  
}
#------------------------------------------------------------------------------
check_proportions_only <- function(df1,df2){
  #
  #check that both files have values bounded between 0 and 1.
  negatives = sum(df1<0, na.rm = TRUE)+sum(df2<0, na.rm = TRUE)
  overs = sum(df1>1, na.rm = TRUE)+sum(df2>1, na.rm = TRUE)
  nulls = sum(is.na(df1)) + sum(is.na(df2))
  if (negatives + overs + nulls != 0){
    print("Cannot perform compositional analysis")
    print("Please ensure all entries are numerical and bounded between 0 and 1")
    result = 1
  }    
  else{result = 0}
  return(result)
}
#------------------------------------------------------------------------------
compo_analyze <- function(df1,df2, p_boolean, tta){
  # Performs compositional analysis and returns a data frame with results 
  # Input: 
  #   Two data frames, one for Group1 (df1) and Group2 (df2) 
  #           composition1   composition2   composition3  .... composition_k
  #   subject1
  #   subject2
  #   ...
  # Output: Dataframe with results for each composition

  #----------------------------------------------
  #Calculate means and variances of original data
  #----------------------------------------------
  m1_vector = vector(mode = "numeric", length = 0 ) 
  m2_vector = vector(mode = "numeric", length = 0 ) 
  s1_vector = vector(mode = "numeric", length = 0 ) 
  s2_vector = vector(mode = "numeric", length = 0 ) 
  #For every composition k
  for (k in c(1:ncol(df1))){
    m1_vector = append(m1_vector, mean(df1[,k]))
    s1_vector = append(s1_vector, sd(df1[,k]))
    m2_vector = append(m2_vector, mean(df2[,k]))
    s2_vector = append(s2_vector, sd(df2[,k]))
  }  
  #-----------------------------------------
  #Perform centered log-ratio transformation
  #-----------------------------------------
  check_proportions_only(df1,df2) #Check both groups
  z_subj = transform_clr(df1)
  z_ctrl = transform_clr(df2)
  #------------------------------------------------------------------
  #For each composition: 
  #Test for difference in means and variances across the two groups
  #------------------------------------------------------------------
  compositions_vector = vector(mode = "character", length = 0)
  t_vector = vector(mode = "numeric", length = 0 ) #We will store p-values from t-test here
  l_vector = vector(mode = "numeric", length = 0 ) #we will store p-values from Levene test here
  #For every composition k
  for (k in c(1:ncol(z_subj))){
    #Test for difference in means, always assume unequal variances
    t = wilcox.test(z_subj[,k], z_ctrl[,k], paired = p_boolean, alternative=tta)
    #Get data on stacked format, perform levene test
    g1 <- z_subj[,k]; g2 <- z_ctrl[,k]; my.data = stack(list(g1=g1, g2=g2)) #getting the data into 'stacked' format
    vt <- leveneTest(values~ind, my.data)
    l_vector = append(l_vector, vt$`Pr(>F)`[1]) #Store results from levene
    t_vector = append(t_vector, t$p.value)      #Store results from t-test
    compositions_vector = append(compositions_vector, colnames(df1[k]))
  }
  return(data.frame("compositions" = compositions_vector,"mean_Group1" = m1_vector, "sd_Group1" = s1_vector, "mean_Group2" = m2_vector, "sd_Group2" = s2_vector,"Z_transf p_values" = t_vector, "Z_transf Brown-Forsythe p_values" = l_vector))
}
#------------------------------------------------------------------------------
full_to_two<-function(df,c_vector1, c_vector2){
  #Input: A data frame of n columns. A vector specifying which columns to add into one.
  #Output: Two columns data frame
  collapsed1 = rowSums(df[c_vector1])
  collapsed2 = rowSums(df[c_vector2])
  result <- data.frame(collapsed1, collapsed2)
  return(result)
}
