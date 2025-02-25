#' @description
#' @param
#' @return 
#' @export
#' @examples
#' source("R/f_add_vaclim_to_dataframe_byseason.R")
#' source("R/f_get_vectorvabyday.R")
#' source("R/f_getva_yearmonth.R")
#' source("R/f_apply_getva_multimonth.R")
#' 
#' path_data = "Data/Data_raw/data_C4F/data_C4F_cutlongcutlat.csv"
#' data = read.csv(path_data)
#' path_data_clim = "Data/Data_raw/data_clim/dataclim_2020212223_CI.nc"
#' va = "Relative_Humidity_2m_09h"
#' lag_year = 1
#' year = NA
#' month_s = c(1:12)
#' season_name = ("toto")
#' 
#' relatif = add_vaclim_to_dataframe_byseason(data,path_data_clim,va,lag_year=lag_year,year = NA, month_s = month_s,absolute_time = FALSE,season_name = season_name)
#' relatif = as.data.frame(relatif)
#' relatif$mean_Relative_Humidity_2m_09h_byseason_toto_lag1
#' 
#' absolute = add_vaclim_to_dataframe_byseason(data,path_data_clim,va,lag_year=NA,year = 2020, month_s = month_s,absolute_time = TRUE,season_name = season_name)
#' absolute = as.data.frame(absolute)

add_vaclim_to_dataframe_byseason = function(data, path_data_clim, va,lag_year=NA,year=NA,month_s,season_name,absolute_time = F) {
  
  if (absolute_time == TRUE){
    lag_year = 0
    # data is absolute, meaning that all data have for example the level of temperature in year = 2020
    vecbyyears = rep(year,nrow(data))
    relabs = paste0("absolute",year)
  }
 
  if (absolute_time == FALSE){
    # data is relative, meaning that each observation will have the temperature relative to a date ( date of sampling for exemple, here data_usable)
    vecbyyears = as.numeric(base::format(as.POSIXct(data$date), "%Y")) - lag_year
    relabs = "relative"
  }

  # list of variables by day for each data point = observation
  day_va_obs =
    lapply(1:nrow(data) ,
           function(x) {
             get_vectorvabyday(
               path_data_clim_nc = path_data_clim,
               va = va,
               long = data$cut_lon[x],
               lat = data$cut_lat[x],
               by = "season" ,
               year_s = vecbyyears[x],
               month_s = month_s
             )
           })
  # list of all observation list of aggregate data applying metrics
  list_metric = lapply(day_va_obs, listmetric)
  df_metric = base::do.call(rbind, list_metric)
  colnames(df_metric) = paste0(colnames(df_metric), "_", va,"_byseason_",season_name,"_",relabs,"_lag",lag_year)
  
  return(df_metric)
}

