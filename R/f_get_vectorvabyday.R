#' @description
#' @param
#' @return 
#' @export
#' @examples
#' 
#' source("R/f_getva_yearmonth.R")
#' source("R/f_apply_getva_multimonth.R")
#' source("R/f_get_vectorvabyday.R")
#' 
#' year_s = 2022
#' long = -3
#' lat = 5.4
#' va = "Relative_Humidity_2m_09h"
#' path_data_clim_nc = "Data/Data_raw/data_clim/dataclim_2020212223_CI.nc" 
#' 
#'#By year 
#' by = "year"
#' month_s = NA
#' mean(get_vectorvabyday(path_data_clim_nc,va,long,lat,by,month_s = NA,year_s))
#' 
#' # By season
#' by = "season"
#' month_s = c(1:12)
#' mean(get_vectorvabyday(path_data_clim_nc,va,long,lat,by,month_s = NA,year_s))
#' 
#'# by = "years"
#'# year_s =c(2020,2021,2022)
#'# month_s = NA

get_vectorvabyday = function(path_data_clim_nc,va,long,lat,by,month_s = NA,year_s = NA){
  
  # For one year
  if (by == "year") {
    if (length(year_s) == 1 & is.na(month_s) == TRUE) {
      # vector va by day for one year
      vectorvabyday = getva_yearmonth(
        path_data_clim_nc = path_data_clim_nc,
        va = va,
        lat = lat,
        long = long,
        year = year_s,
        month = month_s
      )
    } else {
      print("Error : year_s must contain only one year and month_s must be NA")
    }
  }
  
  # For a season, one or several month of one year
  if (by == "season"){
    if (length(year_s) == 1){
      # vector va by day for each month
      vectorvabyday = unlist(lapply(month_s,
        apply_getva_multimonth,
          path_data_clim_nc = path_data_clim_nc,
          year_s = year_s,
          va = va,
          lat = lat,
          long = long)
        )
    } else {
        print("Error : year_s must contain only one year")
    }
  }
  
  # # For mutli year
  # if(by == "years"){
  #   if (is.na(month_s) == TRUE){
  #     vectorvabyday = unlist(lapply(year_s, apply_getva_multiyear))
  #   } else {
  #     print("Error : month_s must be NA")
  #   }
  # }
    
return(vectorvabyday)
}
