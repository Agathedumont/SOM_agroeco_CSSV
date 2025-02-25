# SOM training and clustering ----------

# from (Stephan et al., 2022)
# Github repository : https://github.com/stephanpau/ SOM_TunaFisheries

SOM_training = function(df, r) {
  # Calculate model parameters (grid size and dimensions)
  gridSize <- ceiling((nrow(df) ^ (1 / 2)) * 5)  # Define grid size based on the dataset size
  svd.data <- svd(df)  # Perform Singular Value Decomposition (SVD) on the data
  svd1 <- svd.data$d[1]  # First singular value
  svd2 <- svd.data$d[2]  # Second singular value
  
  # Calculate the grid dimensions (x and y) based on the singular values
  y <- round(sqrt(gridSize * svd2 / svd1))  # Calculate y dimension
  x <- round(svd1 / svd2 * y)  # Calculate x dimension
  dimgrid = x * y  # Total number of grid units
  
  # Train the SOM model with the computed grid dimensions
  set.seed(2023)  # Set seed for reproducibility
  SOM_model <- som(df, grid = somgrid(x, y, "hexagonal"), rlen = r)  # Train the SOM with hexagonal grid
  
  # Return the trained SOM model
  return(SOM_model)
}

SOM_clustering = function(SOM_model, supervized = FALSE) {
  # Extract codes (coordinates of the nodes)
  getcode = getCodes(SOM_model)
  
  # Compute distance matrix between the codebook vectors (nodes)
  dc <- dist(getcode)  # Compute distance of codebook vectors (Euclidean distance)
  distM = as.matrix(dc)  # Convert distance object into a matrix
  
  # Perform hierarchical clustering using the "ward.D2" method
  cluster_hclust <- hclust(dc, method = "ward.D2")
  
  # Return the hierarchical clustering result and the number of clusters
  return(cluster_hclust = cluster_hclust)
}


hclust_somvector = function(cluster_hclust,k,SOM_model,df){
  
  # Clustering
  treecut = cutree(tree = cluster_hclust,k = k)
  treeCut = cbind(cluster.Ward = treecut)
  # Incorporate cluster assignation to dataset
  clusters =
    as_tibble(treeCut) %>%
    rowid_to_column(var = "node") 
  
  nodes =
    as_tibble(SOM_model$unit.classif) %>%
    mutate(node = value) %>% 
    dplyr::select(-value) %>%
    mutate(obsID = df$obsID)
  
  cluster_node <- inner_join(nodes, clusters, by = "node")
  
  df = df %>% 
    inner_join(cluster_node, by = "obsID",suffix = c("","")) 
  
  return(list(data_vis = df,
              tree1 = treecut,
              cluster_node = clusters))
}

# Results function ---------

apply_pairwise_comparaison = function(var,
                                      data,
                                      N_correction = NULL,
                                      var_profile,
                                      correction = TRUE) {
  # Perform pairwise Wilcoxon test
  Matrix = pairwise.wilcox.test(
    x = data[[var]],
    g = as.factor(data[[var_profile]]),
    paired = FALSE,
    p.adjust.method = "none"
  )
  
  # If correction is applied, adjust p-values using the Bonferroni method
  if (correction == TRUE) {
    n = length(unique(data[[var_profile]])) - 1  # Number of unique profiles
    # Apply Bonferroni correction to the p-values
    Matrixadjust = matrix(
      nrow = n,
      p.adjust(Matrix$p.value, method = "bonferroni", n = N_correction),
      byrow = F
    )
    colnames(Matrixadjust) = colnames(Matrix$p.value)
    rownames(Matrixadjust) = rownames(Matrix$p.value)
  } else {
    # If no correction, use the unadjusted p-values
    Matrixadjust = Matrix$p.value
  }
  
  # Create a full matrix for letter comparison
  FullMatrix = fullPTable(Matrixadjust)
  
  # If any NA values (which indicate equal groups in Wilcoxon test), replace them with 1
  FullMatrix[is.na(FullMatrix)] <- 1
  
  # Perform multiple comparison using the letters for significance
  multicompletter = multcompLetters(FullMatrix)
  
  # Create a table with profile names and the corresponding significance letters
  tabcompletter = as.data.frame(cbind(
    Profile = names(multicompletter$Letters),
    letter = unname(multicompletter$Letters)
  ))
  
  # Calculate mean and standard deviation by profile
  tab = data %>% group_by(Profile = as.character(.data[[var_profile]])) %>%
    summarise(mean = round(mean(.data[[var]]), digit = 2),
              ecart_type = round(sd(.data[[var]]), digits = 2))
  
  # Merge the tables to combine mean, standard deviation, and significance letters
  tabcomp = full_join(x = tab, y = tabcompletter, by = "Profile")
  
  # Format the final table to display mean with significance letters
  tabcomp[[var]] = paste0("(",
                          tabcomp$letter,
                          ") ",
                          tabcomp$mean,
                          " ± ",
                          tabcomp$ecart_type)
  
  # Remove intermediate columns
  tabcomp = tabcomp %>% select(-mean, -letter, -ecart_type)
  
  # Return the final table with comparison results and significance letters
  return(list(tab_comp = tabcomp, letter_signif = multicompletter))
  
}

boxplotClusterFunction = function(va_legend = NULL,
                                  va,
                                  df,
                                  legend = "left",
                                  cluster.Ward,
                                  color_cluster,
                                  N_correction = NULL,
                                  correction = FALSE) {
  
  # Perform pairwise comparison for the specified variable and clusters
  tab_comp = apply_pairwise_comparaison(
    var = va,
    data = df,
    N_correction = N_correction,
    var_profile = cluster.Ward,
    correction = correction
  )
  
  # Extract the significance letters from the pairwise comparison results
  letter_signif = tab_comp$letter_signif
  
  # Set the vertical position for the significance letters to be above the max value of the variable
  position = max(df[[va]]) + 0.3 * max(df[[va]])
  
  # Get the cluster profile (ward)
  profile = df[[cluster.Ward]]
  
  # Create a data frame for the significance letters with profile and position
  signif_letters <- data.frame(
    profile = tab_comp$tab_comp$Profile,
    # Names of the cocoa profiles 
    y_position = rep(position, length(tab_comp$tab_comp$Profile)),
    # Vertical position for the significance letters
    label = letter_signif$Letters  # Significance letters
  )
  
  # If no legend is specified, set the legend to the variable name
  if (is.null(va_legend) == TRUE) {
    va_legend = va
  }
  
  # Create the ggplot boxplot
  plot = ggplot(data = df, aes(x = as.factor(.data[[cluster.Ward]]), y = .data[[va]])) +
    geom_boxplot(aes(fill = as.factor(.data[[cluster.Ward]]))) +
    # Color the boxes according to the specified cluster colors
    scale_fill_manual(values = color_cluster) +
    # Themes
    theme_minimal() +
    theme(legend.position = legend, axis.text.x = element_blank()) +
    # Add labels
    xlab(va) +
    ylab("") +
    guides(fill = guide_legend(title = "Clusters")) +
    # Add the significance letters on the plot
    geom_text(
      data = signif_letters,
      aes(x = profile, y = y_position, label = label),
      vjust = -0.5,
      size = 4,
      color = "black"
    ) +
    # Expand y-axis to fit the letters
    expand_limits(y = max(signif_letters$y_position) * 1.4)
  
  # Return the boxplot
  return(plot)
}

# Function to plot clusters on the map  
plot_cluster_loc = function(data, data_unique, colclust,carte){
  ggplot() +
    scale_fill_gradient(low = "white", high = colclust) +
    # Plot Côte d'Ivoire country borders  
    geom_sf(data = st_geometry(carte), fill = NA, color = "black", size = 0.8) +
    # Add cluster labels  
    geom_text(data = data_unique, aes(x = longitude, y = latitude, label = id), 
              size = 8, nudge_y = 0.15) +
    # Add cluster points  
    geom_point(data = data, aes(longitude, latitude), colour = colclust, size = 2, alpha = 0.1) +
    theme_void() +
    labs(fill = "Normalized KDE")
}
