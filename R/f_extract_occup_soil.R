extract_data_occupsoil_2017 = function(data_point, lon, lat, code, date, occup_soil) {
  
  # Select only the relevant columns (longitude, latitude, date, and code)
  data_point = data_point %>% select(lon, lat, date, code)
  
  # Create a unique identifier by combining 'code' and 'date'
  data_point$codedate = paste(get(code, data_point), get(date, data_point))
  
  # Convert data points into an sf object with geographic coordinates (EPSG:4326)
  data_geom = st_as_sf(x = data_point, coords = c(lon, lat), crs = 4326)
  
  # Reproject to UTM Zone 30N (EPSG:32630) for distance-based calculations
  data_geom = st_transform(x = data_geom, crs = 32630)
  
  # Disable s2 geometry processing (to avoid topology issues in buffering operations)
  sf_use_s2(FALSE)
  
  # Define buffer distance (1000 meters)
  dist_buffer = 1000
  
  # Create a buffer of 1000 meters around each point
  buffer_geom <- st_buffer(data_geom, dist = dist_buffer)
  
  # Perform spatial intersection to extract land use data within the 1000m buffer
  intersection = st_intersection(occup_soil, buffer_geom)
  
  # Create additional buffers of 50m and 300m
  buffer_geom_50 <- st_buffer(data_geom, dist = 50)
  buffer_geom_300 <- st_buffer(data_geom, dist = 300)
  
  # Compute spatial intersections for the 50m and 300m buffers
  intersection_50 = st_intersection(occup_soil, buffer_geom_50)
  intersection_300 = st_intersection(occup_soil, buffer_geom_300)
  
  # Store all buffer intersections in a list
  list_intersection = list(buffer_1000 = intersection, 
                           buffer_50 = intersection_50, 
                           buffer_300 = intersection_300)
  return(list_intersection)
  
}
