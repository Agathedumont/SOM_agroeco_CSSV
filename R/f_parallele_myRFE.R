myRFE_par <- function(x, y, n) {
  # Parallelization setup
  Ncpus <- parallel::detectCores() - 1  # Get number of available CPUs (leave one free)
  cl <- parallel::makeCluster(Ncpus)  # Create a cluster with Ncpus workers
  doParallel::registerDoParallel(cl)  # Register the parallel cluster for use
  
  # Export necessary variables and functions to the cluster workers
  clusterExport(cl = cl,
                varlist = c('onestep_RFE', 'RFE', 'x', 'y'))
  
  # Perform parallel computation using foreach loop
  res <- foreach::foreach(i = 1:n,
                          .packages = c("dplyr", "randomForest")) %dopar% {
                            resRFE = RFE(x = x, y = y)  # Call RFE function (Recursive Feature Elimination)
                            return(resRFE)  # Return the result of each RFE execution
                          }
  
  # Stop the parallel cluster after computation is finished
  parallel::stopCluster(cl)
  
  # Return the results of the parallelized RFE computations
  return(res)
}

