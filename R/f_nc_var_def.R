#' @description
#' @param
#' @return 
#' @export
#' @examples
#' 
nc_var_def = function (Path_data_raw, dossier_nc) {
  dossier_path <- paste(Path_data_raw, dossier_nc, sep = "")
  fichier_list = list.files(dossier_path)
  nc_file =
    nc_open(paste(dossier_path, "/", fichier_list[1], sep = ""))
  va = ncvar_def(
    nc_file$var[[1]]$name,
    nc_file$var[[1]]$units,
    dim_state,
    -1,
    longname = nc_file$var[[1]]$longname,
    prec = nc_file$var[[1]]$prec
  )
  nc_close(nc_file)
  return(va)
}
