#' @description
#' @param
#' @return 
#' @export
#' @examples
#' path_data_clim_nc = "Data/Data_raw/data_clim/dataclim_2020212223_CI.nc"
#' va = "Relative_Humidity_2m_09h"
#' long = -3
#' lat = 5.4
#' y = 2022
#' apply_getva_multiyear(y)


apply_getva_multiyear = function(y){
  getva_yearmonth(
    path_data_clim_nc = path_data_clim_nc,
    va = va,
    lat = lat,
    long = long,
    year = y,
    month = NA)
}
