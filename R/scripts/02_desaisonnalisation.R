library(ggplot2)
library(dplyr)
library(lubridate)

# on charge les fichiers RDS
dataset <- readRDS("./data/processed/ERA5_dataset.rds")
df_matin <- readRDS("./data/processed/ERA5_df_matin.rds")
df_soir <- readRDS("./data/processed/ERA5_df_soir.rds")

#### Désaisonnalisation ----
# on désaisonnalise
vars_to_deseasonalize <- c("t2m", "surfacepressure", "ssrd", "ablh", "relative_humidity")
res <- fit_regression_multi(df_matin, vars_to_deseasonalize, trend = TRUE)
df_matin <- res$data

ggplot(df_matin, aes(x = date, y = t2m)) +
  geom_point(color = "blue") +  # Afficher les points des données
  geom_line(aes(y = res$models$t2m$coefficients[1] + res$models$t2m$coefficients[2] * time), color = "black") +  # Afficher la courbe de régression
  geom_line(aes(y = t2m_modeled), color = "red") +  # Afficher la courbe de régression
  labs(title = "Température à 2m le matin  et cycle saisonnier", x = "Temps (années)", y = "Température 2m (°C)") +
  theme_minimal()

ggplot(df_matin, aes(x = date, y = surfacepressure)) +
  geom_point(color = "blue") +  # Afficher les points des données
  geom_line(aes(y = res$models$surfacepressure$coefficients[1] + res$models$surfacepressure$coefficients[2] * time), color = "black") +  # Afficher la courbe de régression
  geom_line(aes(y = surfacepressure_modeled), color = "red") +  # Afficher la courbe de régression
  labs(title = "Pression de surface le matin et cycle saisonnier", x = "Temps (années)", y = "Pression de surface (hPa)") +
  theme_minimal()

ggplot(df_matin, aes(x = date, y = ssrd)) +
  geom_point(color = "blue") +  # Afficher les points des données
  geom_line(aes(y = res$models$ssrd$coefficients[1] + res$models$ssrd$coefficients[2] * time), color = "black") +  # Afficher la courbe de régression
  geom_line(aes(y = ssrd_modeled), color = "red") +  # Afficher la courbe de régression
  labs(title = "Radiation solaire downwards en surface le matin et cycle saisonnier", x = "Temps (années)", y = "Radiation solaire de surface (W/m²)") +
  theme_minimal()

saveRDS(df_matin, file = "./data/processed/ERA5_df_matin_desaisonnalise.rds")

#### Désaisonnalisation ----
res <- fit_regression_multi(df_soir, vars_to_deseasonalize, trend = TRUE)
df_soir <- res$data

saveRDS(df_soir, file = "./data/processed/ERA5_df_soir_desaisonnalise.rds")
rm(res, vars_to_deseasonalize)

#### Préparation des données pour le clustering (il faut faire une matrice qui contient seulement les variables que l'on veut garder et centrer réduire dans une autre matrice)
df_matin_clustering <- df_matin %>%
  select(date, t2m_modeled, surfacepressure_modeled, ssrd_modeled, ablh_modeled, relative_humidity_modeled, windu, windv) %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector())) # centrer et réduire

df_matinhiver_cl <- df_clustering %>%
  filter(saison == "DJF") %>%
  select(-saison)

saveRDS(df_matinhiver_cl, file = "./data/processed/ERA5_df_matinhiver_cl.rds")
saveRDS(df_matin_clustering, file = "./data/processed/ERA5_df_matin_clustering.rds")