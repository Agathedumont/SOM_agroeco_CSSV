#' @description
#' @param
#' @return 
#' @export
#' @examples
#' 
#' source("R/f_getva_yearmonth.R")
#' path_data_clim_nc = "Data/Data_raw/data_clim/dataclim_2020212223_CI.nc"
#' va = "Relative_Humidity_2m_09h"
#' long = -3
#' lat = 5.4
#' year = 2022
#' 
#'# For one month
#' month = 12
#' getva_yearmonth(path_data_clim_nc,va,lat,long,year,month)
#'
#'# For one year
#' month = NA
#' getva_yearmonth(path_data_clim_nc,va,lat,long,year,month)

getva_yearmonth = function(path_data_clim_nc , va , lat , long , year , month = NA){
  
  data_clim_nc = nc_open(path_data_clim_nc)
  # Time 
  time_nc = ncvar_get(data_clim_nc,"time")
  time_date = as.POSIXct(time_nc * 86400, origin = "1900-01-01", tz = "UTC")
  
  # If month is not specified (NA), it's by year 
  if (is.na(month) == TRUE){
    id_year_nc = which(format(time_date, "%Y") == paste(year))
    # If month is specified, it's for one month 
  }else{
    m = ifelse(nchar(as.character(month)) == 2, month, paste(0,month,sep = ""))
    id_year_nc = which(format(time_date, "%Y-%m") == paste(year, "-", m, sep = ""))  
  }
  
  time_s = id_year_nc[1]
  # Latitude
  lat_nc = ncvar_get(data_clim_nc,"lat")
  lat_s = which.min(abs(lat_nc-lat))
  # longitude
  long_nc = ncvar_get(data_clim_nc,"lon")
  long_s =  which.min(abs(long_nc-long))
  
  # Get the variable for one year and a specific month 
  va = ncvar_get(
    data_clim_nc,
    varid = va,
    start = c(long_s, lat_s, time_s) ,
    count = c(1, 1, length(id_year_nc))
  )
  
  nc_close(data_clim_nc)
  
  return(va)
  
}