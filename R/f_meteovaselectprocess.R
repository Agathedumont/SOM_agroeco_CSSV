meteovaselectprocess = function(pathsomRDS, va_clim, k) {
  
  # Load the trained SOM model
  SOM_model = readRDS(pathsomRDS)
  
  # Perform hierarchical clustering on the SOM model
  cluster_hclust = SOM_clustering(SOM_model = SOM_model)
  
  # Assign clusters based on hierarchical clustering with specified k
  clustsomtemp = hclust_somvector(cluster_hclust = cluster_hclust,
                                  k1 = 2, k2 = 3, k3 = k,  # Different cluster levels
                                  SOM_model = SOM_model,
                                  df = data_Vis)  # Dataset for visualization
  
  # Select relevant climate variable(s) and cluster assignment (Ward’s method with 3 clusters)
  data_cluster = clustsomtemp$data_vis %>% 
    select(contains(va_clim) | contains("cluster.Ward3"))
  
  # Train a Random Forest model to predict cluster assignments (Ward 3) based on selected variables
  rf = randomForest(as.factor(cluster.Ward3) ~ ., data = data_cluster)
  
  # Extract variable importance from the Random Forest model
  importance_rf = as.data.frame(rf$importance) %>%
    mutate(va_name = rownames(importance_rf))  # Add variable names
  
  # Sort variables by importance (Mean Decrease in Gini index)
  imp_tbl = importance_rf %>% arrange(desc(MeanDecreaseGini))
  
  # Return the importance table, the trained RF model, and the clustered dataset
  return(list(importance = imp_tbl, rf = rf, data_vis = clustsomtemp$data_vis))
}

  
