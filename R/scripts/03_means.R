library(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)

# on charge les datasets
df_ERA5 <- readRDS("./data/processed/ERA5_dataset.rds")
df_ERA5land <- readRDS("./data/processed/ERA5land_df_LCP.rds")
df_MAR <- readRDS("./data/processed/MAR.rds")
df_VAL <- readRDS("./data/processed/VAL.rds")
dfs <- list(df_ERA5, df_MAR, df_VAL)

# on définit pour chaque df un vecteur des vars à moyenner et un de celles à sommer (juste les précipitations)
vars_to_mean <- list(
  df_ERA5 = c("windu", "windv", "ws", "wd", "t2m", "d2m", "surfacepressure", "ssrd", "ablh", "relative_humidity"),
  df_ERA5land = c("windu", "windv", "ws", "wd", "t2m", "ssrd"),
  df_MAR = c("windu", "windv", "FF", "DD", "T", "PSTAT", "GLO", "U"),
  df_VAL = c("windu", "windv", "FF", "DD", "T", "U")
  
)

vars_to_first <- list(
  df_ERA5 = c("date", "saison"),
  df_ERA5land = c("date", "saison"),
  df_MAR = c("date", "saison"),
  df_VAL = c("date", "saison")
)

vars_to_sum <- list(
  df_ERA5 = c("totalprecipitation"),
  df_ERA5land = c("totalprecipitation"),
  df_MAR = c("RR1"),
  df_VAL = c("RR1")
)

# Noms des dataframes d'origine (à utiliser pour récupérer les bonnes colonnes)
df_names <- c("df_ERA5", "df_MAR", "df_VAL")

# Boucle sur les datasets
for (i in seq_along(dfs)) {
  df <- dfs[[i]]
  name <- df_names[i]
  cols_to_avg <- vars_to_mean[[name]]
  cols_to_sum <- vars_to_sum[[name]]
  
  # ---- Version DAILY ----
  daily_df <- df %>%
    group_by(date) %>%
    summarise(
      across(all_of(cols_to_avg), ~mean(.x, na.rm = TRUE)),
      across(all_of(cols_to_sum), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  saveRDS(daily_df, file = paste0("./data/processed/mean/", name, "_daily.rds"))
  
  # ---- Si "flag_dn" existe, on fait aussi matin et soir ----
  if ("flag_dn" %in% colnames(df)) {
    for (moment in c("matin", "soir")) {
      temp_df <- df %>%
        filter(flag_dn == moment) %>%
        group_by(date) %>%
        summarise(
          across(all_of(cols_to_avg), ~mean(.x, na.rm = TRUE)),
          across(all_of(cols_to_sum), ~sum(.x, na.rm = TRUE)),
          .groups = "drop"
        )
      saveRDS(temp_df, file = paste0("./data/processed/mean/", name, "_", moment, ".rds"))
    }
  }
}
# clean up
rm(dfs, df_names, vars_to_mean, vars_to_first, vars_to_sum, daily_df, df, name, cols_to_avg, cols_to_sum, moment, temp_df, i)
rm(df_ERA5, df_MAR, df_VAL)
