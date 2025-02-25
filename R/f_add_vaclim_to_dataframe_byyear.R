#' @description
#' @param
#' @return 
#' @export
#' @examples
#' devtools::load_all(here::here())
#' path_data = "Data/Data_raw/data_C4F/data_C4F_cutlongcutlat.csv"
#' data = read.csv(path_data)
#' path_data_clim = "Data/Data_raw/data_clim/dataclim_2020212223_CI.nc"
#' va = "Relative_Humidity_2m_09h"
#' 
#' relatif = add_vaclim_to_dataframe_byyear(data,path_data_clim,va,lag_year=1,year=NA,absolute_time = FALSE)
#' head(relatif)
#' 
#' absolute = add_vaclim_to_dataframe_byyear(data,path_data_clim,va,year = 2018,absolute_time = TRUE)
#' head(absolute)


add_vaclim_to_dataframe_byyear = function(data, path_data_clim, va,lag_year=NA,year=NA,absolute_time = FALSE) {
  
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
               by = "year" ,
               year_s = vecbyyears[x]
             )
           })
  # list of all observation list of aggregate data applying metrics
  list_metric = lapply(day_va_obs, listmetric)
  df_metric = base::do.call(rbind, list_metric)
  colnames(df_metric) = paste0(colnames(df_metric), "_", va,"_byyear_",relabs,"_lag",lag_year)
  
  return(df_metric)
}

