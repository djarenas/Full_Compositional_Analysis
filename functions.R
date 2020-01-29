calc_geom_mean <- function(vector){
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

calc_gm_eachrow <- function(df){
  #Input: Dataframe where columns are categories (k = 1,2,...) and rows are samples/subjects
  #Output: A vector of geometric means for each row.
  gm = NULL
  for (i in c(1:nrow(df))){
    #Find geometric mean for that i (sample)  
    x_i = df[i,]
    gm_i = calc_geom_mean(x_i)
    gm = rbind(gm, gm_i)
  }
  return(gm)
}

transform_clr <- function(df){
  #Input: Dataframe (xik), where columns are categories (k = 1,2,...) and rows are samples/subjects i=1,2...)
  #Output: Dataframe with centered log-rate transformation (Zik)
  z_df = NULL
  for (i in c(1:nrow(df))){
    x_i = df[i,]
    gm_i = calc_geom_mean(x_i)
    z_i = log(x_i/gm_i)
    z_df = rbind(z_df, z_i )
  }
  return(z_df)
}

pull <- function(s_file, st_vectors){
  #Input: 1) File name. 2) The columns you want to pull (i.e. Follicles are 1-4, GC are 5-8,...)
  #Output: A DataFrame with only the data you want. 
  
  #Read values from Files
  a = read.csv(s_file, header = TRUE)

  #If they are percentages, divide by 100 to get proportions
  if (percentage == TRUE){a = a/100}
  
  #Replace zero values with resolution
  a[a==0]<- resolution

  #pull out the structure you are interested in
  pd = a[,st_vectors]

  return(pd)
}

check_proportions_only <- function(df1,df2){
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



compo_analyze <- function(df1,df2){
  #Input: Two data frames to compare by compositional analysis.
  #Output: A vector of the p-values for each composition (i.e. weak, medium,...) comparison.
  #Expecting the following global variables: paired_boolean, t_test_alternative
  
  #----------------------------------------------
  #Calculate means and variances of original data
  #----------------------------------------------
  m1_vector = vector(mode = "numeric", length = 0 ) #We will store p-values from t-test here
  m2_vector = vector(mode = "numeric", length = 0 ) #we will store p-values from Levene test here  
  s1_vector = vector(mode = "numeric", length = 0 ) #We will store p-values from t-test here
  s2_vector = vector(mode = "numeric", length = 0 ) #we will store p-values from Levene test here  
  #For every category k
  for (k in c(1:ncol(df1))){
    m1_vector = append(m1_vector, mean(df1[,k]))
    s1_vector = append(s1_vector, sd(df1[,k]))
    m2_vector = append(m2_vector, mean(df2[,k]))
    s2_vector = append(s2_vector, sd(df2[,k]))
  }  
    #-----------------------------------------
  #Perform centered log-ratio transformation
  #-----------------------------------------
  #check both groups have proportions only (0 to 1)
  check_proportions_only(df1,df2)
  z_subj = transform_clr(df1)
  z_ctrl = transform_clr(df2)
  #------------------------------------------------------------------
  #For each category: Test for difference in means and variances 
  #across the two groups
  #------------------------------------------------------------------
  categories_vector = vector(mode = "character", length = 0)
  t_vector = vector(mode = "numeric", length = 0 ) #We will store p-values from t-test here
  l_vector = vector(mode = "numeric", length = 0 ) #we will store p-values from Levene test here
  #For every category k
  for (k in c(1:ncol(z_subj))){
    
    #Test for difference in means, always assume unequal variances
    t = wilcox.test(z_subj[,k], z_ctrl[,k],var.equal = FALSE, paired = paired_boolean, alternative=t_test_alternative)
    #Get data on stacked format, perform levene test
    g1 <- z_subj[,k]; g2 <- z_ctrl[,k]; my.data = stack(list(g1=g1, g2=g2)) #getting the data into 'stacked' format
    vt <- leveneTest(values~ind, my.data)
    l_vector = append(l_vector, vt$`Pr(>F)`[1]) #Store results from levene
    t_vector = append(t_vector, t$p.value)      #Store results from t-test
    categories_vector = append(categories_vector, colnames(df1[k]))
  }
  return(data.frame("compositions" = categories_vector,"mean_Group1" = m1_vector, "sd_Group1" = s1_vector, "mean_Group2" = m2_vector, "sd_Group2" = s2_vector,"Z_transf p_values" = t_vector, "Z_transf Brown-Forsythe p_values" = l_vector))
}

full_to_two<-function(df,c_vector1, c_vector2){
  #Input: A data frame of n columns. A vector specifying which columns to add into one.
  #Output: Two columns data frame
  collapsed1 = rowSums(df[c_vector1])
  collapsed2 = rowSums(df[c_vector2])
  result <- data.frame(collapsed1, collapsed2)
  return(result)
}

two_compo_analyze <- function(df1, df2, c_vector1, c_vector2){
  #----------------------------
  #Collapse to two compositions
  #----------------------------
  s1 = full_to_two(df1,c_vector1, c_vector2)
  c1 = full_to_two(df2,c_vector1, c_vector2)
  #--------------------------------------
  #Run compositional analysis and return
  #--------------------------------------
  result <- compo_analyze(s1,c1)
  return(result)
}