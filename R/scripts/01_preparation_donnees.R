library(ncdf4)
library(lubridate)
library(dplyr)
library(tidyr)
library(foehnix)
source("./R/functions/clustering_functions.R")
source("./R/functions/extract_ncdf.R")

# ce script permet de créer un dataset avec les données ERA5 à partir des fichiers netcdf

#### Chargement des données ERA5 -------
## On charge les fichiers
pathECMWF = "data/raw/ECMWF/atLCPcoords"
nc_windu <- nc_open(file.path(pathECMWF, "windu.nc"))
nc_windv <- nc_open(file.path(pathECMWF, "windv.nc"))
nc_t2m <- nc_open(file.path(pathECMWF, "temperature_2m.nc"))
nc_d2m <- nc_open(file.path(pathECMWF, "dewpoint_temperature.nc")) # point de rosée pour calculer l'humidité relative
nc_surfacepressure <- nc_open(file.path(pathECMWF, "surfacepressure.nc"))
nc_totalprecipitation <- nc_open(file.path(pathECMWF, "total_precipitation.nc"))
nc_radiation <- nc_open(file.path(pathECMWF, "surf_sol_rad_downwards.nc"))
nc_ablh <- nc_open(file.path(pathECMWF, "boundarylayerheight.nc"))

## On filtre les données par date pour éviter les problèmes de dimension
time <- ncvar_get(nc_t2m, "valid_time") 

# Définir la date de début (1970) pour la conversion de l'heure en date
start_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
date <- start_date + time

# Définir la date limite (début 2025) car quand on télécharge les données pour l'année en cours, la date de fin diffère
end_date <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")

# Filtrer les données jusqu'à cette date
valid_indices <- which(date <= end_date)

date <- date[valid_indices]
datejulian <- as.integer(trunc(julian.POSIXt(date)))
time <- as.numeric(difftime(date, min(date), units = "days")) / 365.25 # en années depuis le début des mesures
hour <- hour(date)
month <- month(date)
year <- year(date)
t2m <- ncvar_get(nc_t2m, "t2m")[valid_indices]
windu <- ncvar_get(nc_windu, "u10")[valid_indices]
windv <- ncvar_get(nc_windv, "v10")[valid_indices]
surfacepressure <- ncvar_get(nc_surfacepressure, "sp")[valid_indices] / 100 # hPa
totalprecipitation <- ncvar_get(nc_totalprecipitation, "tp")[valid_indices]*1000 # mm
d2m <- ncvar_get(nc_d2m, "d2m")[valid_indices]
ssrd <- ncvar_get(nc_radiation, "ssrd")[valid_indices]
ablh <- ncvar_get(nc_ablh, "blh")[valid_indices]

# on ferme les netcdf
rm(nc_windu, nc_windv, nc_t2m, nc_surfacepressure, nc_totalprecipitation, nc_d2m, nc_radiation, nc_ablh, valid_indices, start_date, end_date)

flag_dn <- case_when(
  hour(date) %in% 5:7 ~ "matin",
  hour(date) %in% 17:21 ~ "soir",
  TRUE ~ NA_character_
)
winter_peak <- case_when(
  hour(date) %in% 6:7 ~ TRUE,
  TRUE ~ FALSE
)
saison <- case_when(
  month == 12 | month == 1 | month == 2 ~ "DJF",
  month == 3 | month == 4 | month == 5 ~ "MAM",
  month == 6 | month == 7 | month == 8 ~ "JJA",
  month == 9 | month == 10 | month == 11 ~ "SON"
)

# on calcule l'humidité relative en utilisant la formule de Magnus
relative_humidity <- 100 * exp((17.625 * d2m) / (243.04 + d2m)) / exp((17.625 * t2m) / (243.04 + t2m))

# on convertit le vent pour avoir speed et direction
ddff <- uv2ddff(windu, windv)
wind_speed <- ddff$ff
wind_dir <- ddff$dd
rm(ddff)

# conversion en degrés celsius
t2m <- t2m - 273.15
d2m <- d2m - 273.15


dataset <- data.frame(
  date = date,
  datejulian = datejulian,
  time = time,
  hour = hour,
  month = month,
  year = year,
  flag_dn = flag_dn,
  winter_peak = winter_peak,
  saison = saison,
  windu = windu,
  windv = windv,
  ws = wind_speed,
  wd = wind_dir,
  t2m = t2m,
  d2m = d2m,
  surfacepressure = surfacepressure,
  totalprecipitation = totalprecipitation,
  ssrd = ssrd,
  ablh = ablh,
  relative_humidity = relative_humidity
) 

rm(date, datejulian, year, hour, t2m, windu, windv, surfacepressure, totalprecipitation, d2m, ssrd, ablh, flag_dn, month, time, winter_peak, saison, relative_humidity, wind_speed, wind_dir)

dataset$saison <- factor(dataset$saison, levels = c("DJF", "MAM", "JJA", "SON"))

# on écrit ça dans un csv
saveRDS(dataset, file = "./data/processed/ERA5_dataset.rds")

rm(pathECMWF)


#### Chargement du dataset ERA5 land
pathERA5land <- "data/raw/ECMWF/ERA5land"
# on définit les chemins pour chaque variable
ERA5land_windu <- file.path(pathERA5land, "windu_20162022.nc")
ERA5land_windv <- file.path(pathERA5land, "windv_20162022.nc")
ERA5land_t2m <- file.path(pathERA5land, "t2m_20162022.nc")
# on extrait les données au point (fin le nearest) d'intérêt (LCP)
lon <- 5.3950
lat <- 43.3059
windu <- extract_point_fromcoords(ERA5land_windu, "u10", lat, lon)
windv <- extract_point_fromcoords(ERA5land_windv, "v10", lat, lon)
t2m <- extract_point_fromcoords(ERA5land_t2m, "t2m", lat, lon)

# on ferme les netcdf
rm(ERA5land_windu, ERA5land_windv, ERA5land_t2m)

# on convertit le temps en date
start_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")

# on ordonne par date les trois df parce qu'il a pu y avoir des problèmes lors de la concaténation des netcdf
windu <- windu[order(windu$time), ]
windv <- windv[order(windv$time), ]
t2m <- t2m[order(t2m$time), ]

# on vérifie que les dates sont les mêmes
if (!all(windu$time == t2m$time) || !all(windu$time == windv$time)) {
  stop("Les dates des données ERA5 land ne correspondent pas.")
} else {
  message("Les dates des données ERA5 land correspondent.")
}

#### Création d'un dataset avec les données ERA5 land pour LCP -------
df_ERA5land <- data.frame(
  date = start_date + t2m$time,
  datejulian = as.integer(trunc(julian.POSIXt(windu$time))),
  time = as.numeric(difftime(windu$time, min(windu$time), units = "days")) / 365.25, # en années depuis le début des mesures
  windu = windu$value,
  windv = windv$value,
  t2m = t2m$value - 273.15 # conversion en degrés Celsius
)
# on plot la température pour vérifier que ça a du sens
plot(df_ERA5land$date, df_ERA5land$t2m, type = "l", main = "Température 2m ERA5 land", xlab = "Date", ylab = "Température (°C)")

# on télécharge les données de l'ERA5 land pour LCP
saveRDS(df_ERA5land, file = "./data/processed/ERA5land_df_LCP.rds")

#### Création d'un jeu de données en prenant la moyenne sur la plage horaire définie -------
df_matin <- dataset %>%
  filter(flag_dn == "matin") %>%
  group_by(datejulian) %>%
  summarise(
    date = first(date),
    month = first(month),
    time = first(time),
    saison = first(saison),
    year = first(year),
    windu = mean(windu, na.rm = TRUE),
    windv = mean(windv, na.rm = TRUE),
    ws = mean(ws, na.rm = TRUE),
    wd = mean(wd, na.rm = TRUE),
    t2m = mean(t2m, na.rm = TRUE),
    d2m = mean(d2m, na.rm = TRUE),
    surfacepressure = mean(surfacepressure, na.rm = TRUE),
    totalprecipitation = sum(totalprecipitation, na.rm = TRUE), # somme des précipitations
    ssrd = mean(ssrd, na.rm = TRUE),
    ablh = mean(ablh, na.rm = TRUE),
    relative_humidity = mean(relative_humidity, na.rm = TRUE)
  ) %>%
  ungroup()

df_matin <- df_matin %>%
  mutate(log10tp = log10(if_else(totalprecipitation == 0, 0.01, totalprecipitation)))
saveRDS(df_matin, file = "./data/processed/ERA5_df_matin.rds")


#### Création d'un jeu de données en prenant la moyenne sur la plage horaire définie -------
df_soir <- dataset %>%
  filter(flag_dn == "soir") %>%
  group_by(datejulian) %>%
  summarise(
    date = first(date),
    time = first(time),
    month = first(month),
    saison = first(saison),
    year = first(year),
    windu = mean(windu, na.rm = TRUE),
    windv = mean(windv, na.rm = TRUE),
    ws = mean(ws, na.rm = TRUE),
    wd = mean(wd, na.rm = TRUE),
    t2m = mean(t2m, na.rm = TRUE),
    d2m = mean(d2m, na.rm = TRUE),
    surfacepressure = mean(surfacepressure, na.rm = TRUE),
    totalprecipitation = sum(totalprecipitation, na.rm = TRUE), # somme des précipitations
    ssrd = mean(ssrd, na.rm = TRUE),
    ablh = mean(ablh, na.rm = TRUE),
    relative_humidity = mean(relative_humidity, na.rm = TRUE)
  ) %>%
  ungroup()

df_soir <- df_soir %>%
  mutate(log10tp = log10(if_else(totalprecipitation == 0, 0.01, totalprecipitation)))
saveRDS(df_soir, file = "./data/processed/ERA5_df_soir.rds")


#### Préparation des données pour le clustering (il faut faire une matrice qui contient seulement les variables que l'on veut garder et centrer réduire dans une autre matrice)
df_matin_clustering <- df_matin %>%
  select(date, t2m_modeled, surfacepressure_modeled, ssrd_modeled, ablh_modeled, relative_humidity_modeled, windu, windv) %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector())) # centrer et réduire

df_matinhiver_cl <- df_clustering %>%
  filter(saison == "DJF") %>%
  select(-saison)

saveRDS(df_matinhiver_cl, file = "./data/processed/ERA5_df_matinhiver_cl.rds")
saveRDS(df_matin_clustering, file = "./data/processed/ERA5_df_matin_clustering.rds")
