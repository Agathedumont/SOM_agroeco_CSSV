#' @description
#' @param
#' @return 
#' @export
#' @examples

listmetric = function(vectorvabyday){
  ## Mean year
  mean = mean(vectorvabyday)
  ## Minimum year
  minimum = min(vectorvabyday)
  ## Maximum year
  maximum = max(vectorvabyday)
  ## Range year
  range = maximum - minimum
  ## Median year
  med = median(vectorvabyday)
  ## Variance year
  var = var(vectorvabyday)
  
  return(list(mean = mean, min = minimum, max = maximum,range = range, med = med, var = var))
}
