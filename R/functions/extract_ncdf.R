library(ncdf4)

# Function to select a point into the netcdf file, according to latitude and longitude
# Params
# - ncfile: path to the NetCDF file
# - varname: name of the variable to extract
# - target_lat: latitude of the point to extract
# - target_lon: longitude of the point to extract
# Returns a data.frame with the time and the values of the variable at the specified coordinates
# Example : 
# on prend le chemin du fichier netcdf
#nc <- "../meteo_marseille_clustering/data/raw/ECMWF/ERA5land/t2m_20162022.nc"
# on extrait les données pour Marseille
#lcp_data <- extract_point_fromcoords(nc, "t2m", 43.25, 5.35)
#val_data <- extract_point_fromcoords(nc, "t2m", 43.25, 5.45)
extract_point_fromcoords <- function(ncfile, varname, target_lat, target_lon) {
  # Ouvrir le fichier NetCDF
  nc <- nc_open(ncfile)
  on.exit(nc_close(nc))  # S'assure de fermer le fichier en sortant
  
  # Lire les coordonnées
  lats <- ncvar_get(nc, "latitude")
  lons <- ncvar_get(nc, "longitude")

  # Trouver l'indice du point le plus proche
  lat_index <- which.min(abs(lats - target_lat))
  lon_index <- which.min(abs(lons - target_lon))
  
  print(paste("Target lat/lon:", target_lat, target_lon))
  print(paste("Closest lat:", lats[lat_index], "Closest lon:", lons[lon_index]))
  
  # Lire la variable
  var_dims <- ncvar_get(nc, varname, 
                        start = c(lon_index, lat_index, 1), 
                        count = c(1, 1, -1))  # -1 = toute la dimension "time"
  if (all(is.na(var_dims))) {
    warning("Toutes les valeurs sont NA à ce point. Peut-être un point sans données ?")
  }
  
  # Lire le temps
  time <- ncvar_get(nc, "valid_time")
  
  # Retourner un data.frame avec le temps et les valeurs
  return(data.frame(time = time, value = as.vector(var_dims)))
}