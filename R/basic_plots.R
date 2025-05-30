library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(openair)

savefig = TRUE
df <- read.csv("/Volumes/T7_Shield/_stageCO2/data/Meteo_France/LCPMARVAL_20162025.csv", header = TRUE, sep = ",")
figpath = "/Volumes/T7_Shield/_stageCO2/figures_meteofrance/"
# changer la colonne AAAAMMJJHH en POSIXct
df$DateTime <- as.POSIXct(strptime(as.character(df$AAAAMMJJHH), format = "%Y%m%d%H"), tz = "UTC")


df$station <- case_when(
  df$NUM_POSTE == 13055001 ~ "LCP",
  df$NUM_POSTE == 13054001 ~ "MAR",
  df$NUM_POSTE == 13055029 ~ "VAL",
  TRUE ~ NA_character_
)

df$flag_dn <- case_when(
  hour(df$DateTime) >= 0 & hour(df$DateTime) < 4 ~ "night",
  hour(df$DateTime) >= 12 & hour(df$DateTime) < 16 ~ "day",
  TRUE ~ NA_character_
)

df_VAL <- df %>%
  filter(station == "VAL")

df_LCP <- df %>%
  filter(station == "LCP")

df_MAR <- df %>%
  filter(station == "MAR")


# Filtrer les données pour l'année 2020
df_2020 <- df %>%
  filter(DateTime >= as.POSIXct("2020-01-01") & DateTime < as.POSIXct("2021-01-01"))


# Créer un graphique pour chaque station
ggplot(df_2020, aes(x = DateTime, y = T, color = as.factor(station))) +
  geom_line() +
  labs(title = "Température par station pour l'année 2020",
       x = "Date",
       y = "Température (°C)",
       color = "Station") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) +
  facet_wrap(~ station, ncol = 1) # Créer un graphique par station

if (savefig) {
  ggsave(paste0(figpath, "temperature_2020.png"), width = 15, height = 9, bg = "white")
}

# pareil pour la variable U
# Créer un graphique pour chaque station
ggplot(df_2020, aes(x = DateTime, y = U, color = as.factor(station))) +
  geom_line() +
  labs(title = "Humidité relative par station pour l'année 2020",
       x = "Date",
       y = "Humidité (%)",
       color = "Station") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) +
  facet_wrap(~ station, ncol = 1) # Créer un graphique par station
if (savefig) {
  ggsave(paste0(figpath, "humidity_2020.png"), width = 15, height = 9, bg = "white")
}


# plot des roses de vent
windRose <- windRose(df, ws = "FF", wd = "DD", type = 'station', paddle = F, border = T)

wind_MAR <- windRose(df_MAR, ws = "FF", wd = "DD", type = 'flag_dn', paddle = F, border = T,
         key.header = "MAR", key.footer = "Vitesse du vent (m/s)", 
         key.position = "bottom", key.width = 0.5, 
         key.height = 0.5, key.size = 1.5, key.text.size = 1.5, 
         key.text.angle = 0, key.text.color = "black", key.text.font = 2, 
         key.text.family = "sans", key.text.lineheight = 1.2,
         breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
         )

wind_LCP <- windRose(df_VAL, ws = "FF", wd = "DD", type = 'flag_dn', paddle = F, border = T,
         key.header = "VAL", key.footer = "Vitesse du vent (m/s)", 
         key.position = "bottom", key.width = 0.5, 
         key.height = 0.5, key.size = 1.5, key.text.size = 1.5, 
         key.text.angle = 0, key.text.color = "black", key.text.font = 2, 
         key.text.family = "sans", key.text.lineheight = 1.2,
         breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
         )

if (savefig) {
  png(paste0(figpath, "windrose_MAR.png"), width = 3000, height = 2500, res = 300)
  print(wind_MAR)
  dev.off()
  png(paste0(figpath, "windrose_LCP.png"), width = 3000, height = 2500, res = 300)
  print(wind_LCP)
  dev.off()
  png(paste0(figpath, "windRosetotal.png"), width = 3000, height = 2500, res = 300)
  print(windRose)
  dev.off()
  
}