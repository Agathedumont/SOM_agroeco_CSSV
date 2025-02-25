extract_raster_data = function(data_point, occup_soil, lon, lat, code, date){
  
  # Select only the coordinate columns (latitude and longitude)
  points_coords = data_point %>% dplyr::select(lat, lon)
  
  # Convert points to an sf object with the geographic coordinate system WGS 84 (EPSG:4326)
  points_sf = st_as_sf(points_coords,
                       coords = c(lon, lat),
                       crs = 4326)
  
  # Transform the coordinates of the points to match the raster's reference system
  points_sf_zone30 <- st_transform(points_sf, crs = crs(occup_soil))
  
  # Create buffers (buffer zones) around the points at different distances
  buffers_50 <- st_buffer(points_sf_zone30, dist = 50, include_xy = TRUE)
  buffers_300 <- st_buffer(points_sf_zone30, dist = 300, include_xy = TRUE)
  buffers_1000 <- st_buffer(points_sf_zone30, dist = 1000, include_xy = TRUE)
  
  # Extract raster values and compute the percentage of each category within the buffer
  ## 50-meter buffer
  extracted_values_C4F_50 <- exact_extract(occup_soil_rc, buffers_50, fun = "frac")
  colnames(extracted_values_C4F_50) = paste0(colnames(extracted_values_C4F_50), "_50")
  
  ## 300-meter buffer
  extracted_values_C4F_300 <- exact_extract(occup_soil_rc, buffers_300, fun = "frac")
  colnames(extracted_values_C4F_300) = paste0(colnames(extracted_values_C4F_300), "_300")
  
  ## 1000-meter buffer
  extracted_values_C4F_1000 <- exact_extract(occup_soil_rc, buffers_1000, fun = "frac")
  colnames(extracted_values_C4F_1000) = paste0(colnames(extracted_values_C4F_1000), "_1000")
  
  # Combine extracted values for all buffer sizes and add additional information from data_point
  perc_soil_occup_2021 = cbind(
    extracted_values_C4F_50,
    extracted_values_C4F_300,
    extracted_values_C4F_1000,
    get(code, data_point) # Retrieve additional column based on 'code'
  )
  
  # Return the final dataset with extracted values
  return(perc_soil_occup_2021)
}
