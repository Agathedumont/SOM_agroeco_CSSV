#' @description
#' @param
#' @return 
#' @export
#' @examples
#' 
#' source("R/f_getva_yearmonth.R")
#' source("R/f_apply_getva_multimonth.R")
#' m = 12
#' year_s = 2022
#' long = -3
#' lat = 5.4
#' va = "Relative_Humidity_2m_09h"
#' path_data_clim_nc = "Data/Data_raw/data_clim/dataclim_2020212223_CI.nc" 
#' getva_yearmonth(path_data_clim_nc,va,lat,long,year_s,m)
#' apply_getva_multimonth(m,path_data_clim_nc,year_s,va,lat,long)

apply_getva_multimonth = function(m,path_data_clim_nc,year_s,va,lat,long){
  va_year_month = getva_yearmonth(
    path_data_clim_nc = path_data_clim_nc,
    va = va,
    lat = lat,
    long = long,
    year = year_s,
    month = m)
  return(va_year_month)
}
