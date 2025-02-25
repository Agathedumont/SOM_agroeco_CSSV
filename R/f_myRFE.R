onestep_RFE = function(x_l,y_l){
  
  var_perf = NA # empty va of performance 
  var_absmeancor = NA # va vector of absolute mean correlation
  var_nbvar = NA # empty va of number of variable 
  var_vadelet = NA # empty va of va delete at each step of the backward selection
  
  nbvar = base::ncol(x_l) # number of variable
  
  # backward selection test
  rf_l = randomForest::randomForest(x_l, y_l, importance = TRUE) # random forest
  importance_l = randomForest::importance(rf_l , type = 1) # Mean decrease accuracy
  perf_l = base::mean(rf_l$err.rate[,1]) # mean performance in term of OOB error
  corx_l = stats::cor(x_l) # correlation between variables
  base::diag(corx_l) = NA # delete auto correlation
  veccorx_l = as.vector(corx_l) # get vector of correlation
  abscorx_l = base::abs(base::mean(veccorx_l, na.rm = TRUE)) # get the absolute mean correlation between variables
  
  # get the least important variable
  vas_delete = as.data.frame(importance_l) %>%
    dplyr::filter(MeanDecreaseAccuracy == base::min(MeanDecreaseAccuracy)) %>%
    base::rownames()
  random_va_delete = base::sample(vas_delete, 1) # if equality occur, select randomly one variable
  
  # return object
  return(
    list(
      va_nbvar = nbvar,
      # return number of variable at each step of the backward selection
      va_perf  = perf_l,
      # return the performance
      va_absmeancor = abscorx_l,
      # return the absolute mean correlation
      va_vadelet = random_va_delete # return de variable delete
    )
  )
}

RFE = function(x,y){
  # Initialization
  x_l = x
  y_l = y
  vec_perf = vector() # empty vec of performance 
  vec_absmeancor = vector() # vec vector of absolute mean correlation
  vec_nbvar = vector() # empty vec of number of variable 
  vec_vadelet = vector() # empty vec of va delete at each step of the backward selection
  rf_list =list()
  N = ncol(x)-1
  # Since de last variable
  for(i in 1:N){
    step1RFE = onestep_RFE(x_l = x_l,y_l = y_l)
    x_l = x_l %>% dplyr::select(-all_of(step1RFE$va_vadelet))
    vec_perf[i] = step1RFE$va_perf
    vec_absmeancor[i] = step1RFE$va_absmeancor
    vec_nbvar[i] = step1RFE$va_nbvar
    vec_vadelet[i] = step1RFE$va_vadelet
    rf_list[[i]] = step1RFE$rf
  }
  
  return(list(vec_perf = vec_perf,
              vec_absmeancor = vec_absmeancor,
              vec_nbvar = vec_nbvar,
              vec_vadelet = vec_vadelet))
}

get_va_rank = function(i, x, RFE) {
  
  colnamex = colnames(x)  # Get column names of the dataset
  
  # Identify the variables that were not deleted in the RFE process
  last = colnamex[which(colnamex %in% RFE[[i]]$vec_vadelet == FALSE)]  
  
  n = length(RFE[[i]]$vec_vadelet)  # Total number of deleted variables
  
  before = RFE[[i]]$vec_vadelet  # Variables that were deleted
  
  # Combine deleted and remaining variables, reversing the order to reflect selection rank
  va_selected = rev(c(before, last))
  
  # Assign ranks from 1 to the number of selected variables
  rank = seq(1, length(va_selected))
  
  # Return a list containing variable names and their ranks
  return(list(vaname = va_selected, rank = rank))
}



variables_ranking_tab = function(get_va_rank_res, ni, ranktextesize = 1) {
  
  # Extract variable names from the ranking results
  firstva_name = as.vector(unlist(lapply(get_va_rank_res, function(x) { x$vaname })))
  
  # Extract ranking values from the ranking results
  firstrank = as.vector(unlist(lapply(get_va_rank_res, function(x) { x$rank })))
  
  # Create a dataframe with variable names and their associated ranks
  data_vaselected = data.frame(vaname = firstva_name, rank = firstrank)
  
  # Compute the mean rank for each variable
  df_meanrank = data_vaselected %>%
    reframe(meanrank = round(mean(rank), 1), .by = vaname)
  
  # Compute the median rank for each variable
  df_medrank = data_vaselected %>%
    reframe(medrank = round(median(rank), 1), .by = vaname)
  
  # Compute the sum of ranks for each variable
  df_sumrank = data_vaselected %>%
    reframe(sumrank = sum(rank), .by = vaname) 
  
  # Merge all computed ranking data into a single dataframe
  df_vaselected = full_join(df_meanrank, df_medrank, by = "vaname") %>%
    full_join(df_sumrank, by = "vaname") %>%
    arrange(sumrank)  # Sort by total rank sum
  
  # Return the final ranking table
  return(tab = df_vaselected)
}


