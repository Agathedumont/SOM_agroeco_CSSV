# get_upper_tri <- function(cormat){
#   cormat[lower.tri(cormat)]<- NA
#   return(cormat)
# }

corranalysis = function(df) {
  res_cor = cor(df)  # Compute correlation matrix
  corr_plot = corrplot(
    res_cor,
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45
  )  # Generate correlation plot with hierarchical clustering
  corr_res = rcorr(as.matrix(res_cor))  # Compute correlation coefficients and p-values
  
  return(list(corr_plot = corr_plot, corr_res = corr_res))  # Return both the plot and correlation results
}

corr_plot_matrix <- function(df) {
  corr_res = corranalysis(df)  # Perform correlation analysis
  cormat = corr_res$corr_res$r  # Extract correlation coefficients
  pmat = corr_res$corr_res$P  # Extract p-values
  
  ut <- upper.tri(cormat)  # Identify upper triangular part of the matrix
  
  # Create a dataframe containing correlation pairs and their p-values
  datacorr = data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    # Row names of significant correlations
    column = rownames(cormat)[col(cormat)[ut]],
    # Column names of significant correlations
    cor  = (cormat)[ut],
    # Correlation values
    p = pmat[ut]  # Corresponding p-values
  )
  
  return(list(data_corr = datacorr, corr_plot = corr_res$corr_plot))  # Return data and plot
}
