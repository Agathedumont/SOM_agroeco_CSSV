#' @description
#' @param
#' @return 
#' @export
#' @examples
#' 
#' path_data_raw_zip = "Data/Data_raw/data_clim/"
#' una31 = c(una12,as.character(seq(from = 13, to = 31, by = 1)))
#' zone = c(8,-9 ,3, -2)
#' user_id = "184625"
#' extract_clim_va(Va = "2m_temperature", Year = "2018", Month = una12, download_path = path_data_raw_zip,Stat = "24_hour_mean",Day = una31, Zone_chr = "CI", Zone = zone,user_id = user_id)

extract_clim_va = function(Va, # variable 
                           Year, # ex : "2020"
                           download_path = path_data_raw_zip, # location of downloaded data  
                           Stat = "24_hour_mean", # day mean 
                           Month, # ex: "01" for January
                           Day = una31, # ex : "O1" for Monday
                           Zone_chr = "CI", # for target name
                           Zone = zone, # box (maxLat,minLon,minLat,maxLon)
                           Time = NA,
                           user_id = user_id){ # for humidity
  # Input variable : 
  # Va, variable 
  # Year_lag, ex : "2020"
  # download_path = path_data_raw_zip, location of downloaded data  
  # Stat = "24_hour_mean", day mean 
  # Month, ex: "01" for January
  # Day = una31, ex : "O1" for Monday
  # Zone_chr = "CI", or target name
  # Zone = zone, box (maxLat,minLon,minLat,maxLon)
  # Time, hour for humidity
  
  # Output : 
  # File with 1 day nc files
  
  # For the humidity data we need to specify the hour 
  if (Va != "2m_relative_humidity"){ 
    
    request_data = list(
      statistic = Stat,
      dataset_short_name = "sis-agrometeorological-indicators",
      format = "zip",
      variable = Va,
      year = Year,
      month = Month,
      day = Day,
      area = Zone,
      target = paste("agrometeorological_copernicus_",
                     Year,"_",Va,
                     "_",Zone_chr,".zip",
                     sep = ""),
      version ="1_1"
    )
    
    # For the other variable the hour is not an option  
  }else{ 
    request_data = list(
      statistic = Stat,
      dataset_short_name = "sis-agrometeorological-indicators",
      format = "zip",
      variable = Va,
      year = Year,
      month = Month,
      day = Day,
      time = Time,
      area = Zone,
      target = paste( "agrometeorological_copernicus_",
                      Year,"_",Va, "_",
                      Time,"_",
                      Zone_chr,".zip",
                      sep = ""),
      version ="1_1"
    )
    
  }
  
  # Download  nc files
  file <- wf_request(request = request_data,
                     user = user_id,
                     transfer = TRUE,
                     path = download_path,
                     verbose = TRUE)
}
