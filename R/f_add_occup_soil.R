stat_occup_soil = function(uniqueID_plot_name, id_sst, d, d_r, intersection_geom){
  # Compute statistics per 'code_parcelle'
  
  # Calculate the area of each intersected geometry (in square meters)
  intersection_geom$area_m2 = st_area(intersection_geom)
  
  # Convert spatial object to a regular data frame by dropping geometry
  intersection_df = intersection_geom %>% st_drop_geometry()
  
  # Extract unique plot identifiers
  codedate = unique(get(uniqueID_plot_name, intersection_df))
  
  # Compute the proportion of each soil occupation type within the intersection
  stat = t(sapply(codedate, function(i) {
    # Filter data for the current plot
    code_inter = intersection_df %>% filter(codedate == i)
    
    # Sum the areas per soil occupation type (id_sst)
    Area_codesst = as.vector(unlist(lapply(id_sst, function(x) {
      code_inter %>% filter(code_sst == x) %>% summarise(sum(area_m2))
    })))
  }))
  
  # Normalize the values by dividing each row by its total sum to get proportions
  stat = t(apply(stat, MARGIN = 1, function(x) x / sum(x)))
  
  # Rename columns to indicate soil occupation type
  colnames(stat) = paste0("sst_", id_sst)
  
  return(stat)
}


categorized_data_soil_occupency = function(plot_soil_occupency, forest, barrier, humain, cacao_cafe, dist){
  # Aggregate soil occupancy categories into broader classes
  
  # Aggregate forest-related variables
  forest_data = plot_soil_occupency %>%
    select(all_of(forest)) %>%
    mutate(foret = rowSums(.)) %>% # Sum all forest-related variables
    select(-all_of(forest)) %>% # Remove individual columns after summation
    rownames_to_column()
  
  # Aggregate barrier-related variables
  barrier_data = plot_soil_occupency %>%
    select(all_of(barrier)) %>%
    mutate(barrier = rowSums(.)) %>%
    select(-all_of(barrier)) %>%
    rownames_to_column()
  
  # Aggregate human-related variables
  humain_data = plot_soil_occupency %>%
    select(all_of(humain)) %>%
    mutate(humain = rowSums(.)) %>%
    select(-all_of(humain)) %>%
    rownames_to_column()
  
  # Aggregate cacao and coffee-related variables
  cacao_cafe_data = plot_soil_occupency %>%
    select(all_of(cacao_cafe)) %>%
    mutate(cacao_cafe = rowSums(.)) %>%
    select(-all_of(cacao_cafe)) %>%
    rownames_to_column()
  
  # Merge all categories into one dataset
  category = full_join(forest_data, barrier_data, join_by("rowname")) %>% 
    full_join(humain_data, join_by("rowname")) %>% 
    full_join(cacao_cafe_data, join_by("rowname"))
  
  # Rename columns to include distance information
  colnames(category) = paste0(colnames(category), "_", dist)
  
  return(category)
}


