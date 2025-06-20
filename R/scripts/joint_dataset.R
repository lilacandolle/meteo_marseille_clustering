# load data
ERA5land <- readRDS("data/processed/ERA5land_df_LCP.rds")
ERA5 <- readRDS("data/processed/ERA5_dataset.rds")
CO2 <- readRDS("data/processed/CO2_hour.rds")
CO2 <- CO2 %>%
  filter(minute(date) == 0 & second(date) == 0)

# on prend ERA5 pour year inf à 2023
ERA5 <- ERA5 %>%
  filter(year < 2023)
# on crée un dataset avec les 3
df_meteoCO2 <- ERA5land %>%
  select (date, flag_dn, saison, windu, windv, ws, wd, t2m, ssrd, surfacepressure, totalprecipitation) %>%
  left_join(CO2, by = "date") %>%
  mutate(saison = factor(saison, levels = c("DJF", "MAM", "JJA", "SON"))) %>% # pour ERA5 on joint que ablh et relative humidity
  left_join(ERA5 %>% select(date, relative_humidity, ablh), by = "date")

saveRDS(df_meteoCO2, "data/processed/df_meteoCO2_LCP.rds")