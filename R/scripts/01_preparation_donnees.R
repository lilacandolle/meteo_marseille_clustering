library(ncdf4)
library(lubridate)
library(dplyr)
library(tidyr)
library(foehnix)
source("./R/functions/clustering_functions.R")
source("./R/functions/extract_ncdf.R")

# ce script permet de créer un dataset avec les données ERA5 à partir des fichiers netcdf
# ATTENTION A MODIFIER POUR CHAQUE DATASET les plages horaires notamment si modifiées

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
date <- as.POSIXct(date, tz = "UTC")

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
rm(dataset)

#### ERA5 à MAR ------
pathERA5 <- "data/raw/ECMWF/MARforcomparison"
nc_windu <- nc_open(file.path(pathERA5, "20222023_MARcoords_ERA5windu.nc"))
nc_windv <- nc_open(file.path(pathERA5, "20222023_MARcoords_ERA5windv.nc"))
nc_t2m <- nc_open(file.path(pathERA5, "20222023_MARcoords_ERA5_t2m.nc"))
nc_tp <- nc_open(file.path(pathERA5, "20222023_MARcoords_ERA5_totalprecipitation.nc"))

## On filtre les données par date pour éviter les problèmes de dimension
time <- ncvar_get(nc_t2m, "valid_time") 

# Définir la date de début (1970) pour la conversion de l'heure en date
start_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
date <- start_date + time
date <- as.POSIXct(date, tz = "UTC")
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
totalprecipitation <- ncvar_get(nc_tp, "tp")[valid_indices]*1000 # mm

# on ferme les netcdf
rm(nc_windu, nc_windv, nc_t2m, nc_tp, valid_indices, start_date, end_date)
flag_dn <- case_when(
  hour(date) %in% 5:7 ~ "matin",
  hour(date) %in% 17:21 ~ "soir",
  TRUE ~ NA_character_
)
saison <- case_when(
  month == 12 | month == 1 | month == 2 ~ "DJF",
  month == 3 | month == 4 | month == 5 ~ "MAM",
  month == 6 | month == 7 | month == 8 ~ "JJA",
  month == 9 | month == 10 | month == 11 ~ "SON"
)

# on convertit le vent pour avoir speed et direction
ddff <- uv2ddff(windu, windv)
wind_speed <- ddff$ff
wind_dir <- ddff$dd
rm(ddff)
# conversion en degrés celsius
t2m <- t2m - 273.15
dataset_MAR <- data.frame(
  date = date,
  datejulian = datejulian,
  time = time,
  hour = hour,
  month = month,
  year = year,
  flag_dn = flag_dn,
  saison = saison,
  windu = windu,
  windv = windv,
  ws = wind_speed,
  wd = wind_dir,
  t2m = t2m,
  totalprecipitation = totalprecipitation
)
rm(date, datejulian, year, hour, t2m, windu, windv, totalprecipitation, flag_dn, month, time, wind_speed, wind_dir)
dataset_MAR$saison <- factor(dataset_MAR$saison, levels = c("DJF", "MAM", "JJA", "SON"))
# on écrit ça dans un csv
saveRDS(dataset_MAR, file = "./data/processed/ERA5_dataset_MAR.rds")
#### Chargement du dataset ERA5 land----------
pathERA5land <- "data/raw/ECMWF/ERA5land"
# on définit les chemins pour chaque variable
ERA5land_windu <- file.path(pathERA5land, "windu_20162022.nc")
ERA5land_windv <- file.path(pathERA5land, "windv_20162022.nc")
ERA5land_t2m <- file.path(pathERA5land, "t2m_20162022.nc")
ERA5land_ssrd <- file.path(pathERA5land, "ssrd_20162022.nc")
# on extrait les données au point (fin le nearest) d'intérêt (LCP)
lon <- 5.3950
lat <- 43.3059
windu <- extract_point_fromcoords(ERA5land_windu, "u10", lat, lon)
windv <- extract_point_fromcoords(ERA5land_windv, "v10", lat, lon)
t2m <- extract_point_fromcoords(ERA5land_t2m, "t2m", lat, lon)
ssrd <- extract_point_fromcoords(ERA5land_ssrd, "ssrd",lat, lon)

# on ferme les netcdf
rm(ERA5land_windu, ERA5land_windv, ERA5land_t2m, ERA5land_ssrd)

# on convertit le temps en date
start_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")

# on ordonne par date les trois df parce qu'il a pu y avoir des problèmes lors de la concaténation des netcdf
windu <- windu[order(windu$time), ]
windv <- windv[order(windv$time), ]
t2m <- t2m[order(t2m$time), ]
ssrd <- ssrd[order(ssrd$time), ]


# on vérifie que les dates sont les mêmes
if (!all(windu$time == t2m$time) || !all(windu$time == windv$time) || !all(windu$time == ssrd$time)) {
  stop("Les dates des données ERA5 land ne correspondent pas.")
} else {
  message("Les dates des données ERA5 land correspondent.")
}

# on met time en date pour ssrd
ssrd <- ssrd %>%
  mutate(time = start_date + time)

# quand l'heure est 1h, on divise ssrd par 3600, sinon on soustrait la valeur de l'heure d'avant puis on divise par 3600 (https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790)
ssrd <- ssrd %>%
  mutate(
    value = ifelse(
      hour(time) == 1,
      value / 3600,
      (value - lag(value)) / 3600
    )
  ) %>%
  select(time, value)
# on remplace la premiere valeur par 0
ssrd$value[1] <- 0

ddff <- uv2ddff(windu$value, windv$value)
wind_speed <- ddff$ff
wind_dir <- ddff$dd
rm(ddff)

#### Création d'un dataset avec les données ERA5 land pour LCP
df_ERA5land <- data.frame(
  date = start_date + t2m$time,
  datejulian = as.integer(trunc(julian.POSIXt(windu$time))),
  time = as.numeric(difftime(windu$time, min(windu$time), units = "days")) / 365.25, # en années depuis le début des mesures
  windu = windu$value,
  windv = windv$value,
  ws = wind_speed,
  wd = wind_dir,
  t2m = t2m$value - 273.15, # conversion en degrés Celsius
  ssrd = ssrd$value
)
rm(start_date, windu, windv, t2m, ssrd, wind_speed, wind_dir)

# on rajoute flag_dn
df_ERA5land$flag_dn <- case_when(
  hour(df_ERA5land$date) %in% 5:7 ~ "matin",
  hour(df_ERA5land$date) %in% 17:21 ~ "soir",
  TRUE ~ NA_character_
)

# on rajoute la saison
df_ERA5land$saison <- case_when(
  month(df_ERA5land$date) == 12 | month(df_ERA5land$date) == 1 | month(df_ERA5land$date) == 2 ~ "DJF",
  month(df_ERA5land$date) == 3 | month(df_ERA5land$date) == 4 | month(df_ERA5land$date) == 5 ~ "MAM",
  month(df_ERA5land$date) == 6 | month(df_ERA5land$date) == 7 | month(df_ERA5land$date) == 8 ~ "JJA",
  month(df_ERA5land$date) == 9 | month(df_ERA5land$date) == 10 | month(df_ERA5land$date) == 11 ~ "SON"
)

# on plot la température pour vérifier que ça a du sens
plot(df_ERA5land$date, df_ERA5land$t2m, type = "l", main = "Température 2m ERA5 land", xlab = "Date", ylab = "Température (°C)")

# on télécharge les données de ERA5 land pour LCP
saveRDS(df_ERA5land, file = "./data/processed/ERA5land_df_LCP.rds")


rm(df_ERA5land)

#### ERA5land others coord------
pathERA5land <- "data/raw/ECMWF/ERA5land"
# on définit les chemins pour chaque variable
ERA5land_windu <- file.path(pathERA5land, "windu_20162022.nc")
ERA5land_windv <- file.path(pathERA5land, "windv_20162022.nc")
ERA5land_t2m <- file.path(pathERA5land, "t2m_20162022.nc")
ERA5land_ssrd <- file.path(pathERA5land, "ssrd_20162022.nc")
# on extrait les données au point (fin le nearest) d'intérêt (LCP)
lon <- 5.21
lat <- 43.5
windu <- extract_point_fromcoords(ERA5land_windu, "u10", lat, lon)
windv <- extract_point_fromcoords(ERA5land_windv, "v10", lat, lon)
t2m <- extract_point_fromcoords(ERA5land_t2m, "t2m", lat, lon)
ssrd <- extract_point_fromcoords(ERA5land_ssrd, "ssrd",lat, lon)

# on ferme les netcdf
rm(ERA5land_windu, ERA5land_windv, ERA5land_t2m, ERA5land_ssrd)

# on convertit le temps en date
start_date <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")

# on ordonne par date les trois df parce qu'il a pu y avoir des problèmes lors de la concaténation des netcdf
windu <- windu[order(windu$time), ]
windv <- windv[order(windv$time), ]
t2m <- t2m[order(t2m$time), ]
ssrd <- ssrd[order(ssrd$time), ]


# on vérifie que les dates sont les mêmes
if (!all(windu$time == t2m$time) || !all(windu$time == windv$time) || !all(windu$time == ssrd$time)) {
  stop("Les dates des données ERA5 land ne correspondent pas.")
} else {
  message("Les dates des données ERA5 land correspondent.")
}

# on met time en date pour ssrd
ssrd <- ssrd %>%
  mutate(time = start_date + time)

# quand l'heure est 1h, on divise ssrd par 3600, sinon on soustrait la valeur de l'heure d'avant puis on divise par 3600 (https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790)
ssrd <- ssrd %>%
  mutate(
    value = ifelse(
      hour(time) == 1,
      value / 3600,
      (value - lag(value)) / 3600
    )
  ) %>%
  select(time, value)
# on remplace la premiere valeur par 0
ssrd$value[1] <- 0

ddff <- uv2ddff(windu$value, windv$value)
wind_speed <- ddff$ff
wind_dir <- ddff$dd
rm(ddff)

#### Création d'un dataset avec les données ERA5 land pour LCP
df_ERA5land <- data.frame(
  date = start_date + t2m$time,
  datejulian = as.integer(trunc(julian.POSIXt(windu$time))),
  time = as.numeric(difftime(windu$time, min(windu$time), units = "days")) / 365.25, # en années depuis le début des mesures
  windu = windu$value,
  windv = windv$value,
  ws = wind_speed,
  wd = wind_dir,
  t2m = t2m$value - 273.15, # conversion en degrés Celsius
  ssrd = ssrd$value
)
rm(start_date, windu, windv, t2m, ssrd, wind_speed, wind_dir)

# on rajoute flag_dn
df_ERA5land$flag_dn <- case_when(
  hour(df_ERA5land$date) %in% 5:7 ~ "matin",
  hour(df_ERA5land$date) %in% 17:21 ~ "soir",
  TRUE ~ NA_character_
)

# on rajoute la saison
df_ERA5land$saison <- case_when(
  month(df_ERA5land$date) == 12 | month(df_ERA5land$date) == 1 | month(df_ERA5land$date) == 2 ~ "DJF",
  month(df_ERA5land$date) == 3 | month(df_ERA5land$date) == 4 | month(df_ERA5land$date) == 5 ~ "MAM",
  month(df_ERA5land$date) == 6 | month(df_ERA5land$date) == 7 | month(df_ERA5land$date) == 8 ~ "JJA",
  month(df_ERA5land$date) == 9 | month(df_ERA5land$date) == 10 | month(df_ERA5land$date) == 11 ~ "SON"
)

# on télécharge les données de ERA5 land pour LCP
saveRDS(df_ERA5land, file = "./data/processed/ERA5land_df_435_52.rds")


rm(df_ERA5land)

#### Meteo France -------
# on charge les données de Météo France
df <- read.csv("data/raw/MeteoFrance/LCPMARVAL_20162025.csv", header = TRUE, sep = ",")
# changer la colonne AAAAMMJJHH en POSIXct
df$date <- as.POSIXct(strptime(as.character(df$AAAAMMJJHH), format = "%Y%m%d%H"), tz = "UTC")
df <- df %>%
  select(NUM_POSTE, NOM_USUEL, date, FF, DD, T, PSTAT, RR1, GLO, U)

# on transforme FF et DD en u et v
uv <- ddff2uv(df$DD, df$FF)
df$windu <- uv$u
df$windv <- uv$v
rm(uv)

# on rajoute la colonne flag_dn
df$flag_dn <- case_when(
  hour(df$date) %in% 5:7 ~ "matin",
  hour(df$date) %in% 17:21 ~ "soir",
  TRUE ~ NA_character_
)
# on rajoute la saison
df$saison <- case_when(
  month(df$date) == 12 | month(df$date) == 1 | month(df$date) == 2 ~ "DJF",
  month(df$date) == 3 | month(df$date) == 4 | month(df$date) == 5 ~ "MAM",
  month(df$date) == 6 | month(df$date) == 7 | month(df$date) == 8 ~ "JJA",
  month(df$date) == 9 | month(df$date) == 10 | month(df$date) == 11 ~ "SON"
)

df_VAL <- df %>%
  filter(NUM_POSTE == 13055029)
df_MAR <- df %>%
  filter(NUM_POSTE == 13054001)
df_LCP <- df %>%
  filter(NUM_POSTE == 13055001)
rm(df)

# on sauve les df
saveRDS(df_VAL, file = "./data/processed/VAL.rds")
saveRDS(df_MAR, file = "./data/processed/MAR.rds")
saveRDS(df_LCP, file = "./data/processed/LCP.rds")


