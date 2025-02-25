va_concat_nc_function = function(Path_data_raw = path_data_raw,
                                 fichier_concat,
                                 fichiers_to_concat){
  # get the days nc files
  dossier_path <- paste(Path_data_raw,fichiers_to_concat,sep = "")
 
  fichier_list = list.files(dossier_path)
  
  # days in the new folder
  new_ncdf= nc_open(paste(Path_data_raw,fichier_concat,sep = ""),write = TRUE)
  day_new_nc = as.vector(ncvar_get(new_ncdf,"time"))
  nc_close(new_ncdf)
  
  # filling the ncdf file for each day value
  for ( i in 1:length(fichier_list)){
    # get the file i
    nc_file_i <- nc_open(paste(dossier_path,"/",fichier_list[i],sep = ""))
    # variables
    va_name = nc_file_i$var[[1]]$name
    # Date
    date_day = as.vector(ncvar_get(nc_file_i,"time"))
    # Values
    variable_file=ncvar_get(nc_file_i,va_name)
    # Close file
    nc_close(nc_file_i)
    
    # we want to add the values of day i for each pixel 
    id_date_day = which(day_new_nc == date_day)
    new_ncdf= nc_open(paste(Path_data_raw,fichier_concat,sep = ""),write = TRUE)
    lenght_lon = length(new_ncdf$dim$lon$vals)
    lenght_lat = length(new_ncdf$dim$lat$vals)
    ncvar_put(new_ncdf,
              varid = va_name,
              vals = variable_file,
              start = c(1,1,id_date_day),
              count = c(lenght_lon,lenght_lat,1))
    nc_close(new_ncdf)
  }
  
}
