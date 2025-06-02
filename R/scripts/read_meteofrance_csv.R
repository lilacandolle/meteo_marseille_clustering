### Ce script fermet de charger les données météo france, de les extraire pour la période à partir de 2016, et pour les trois stations d'intérêt, et de les combiner dans un fichier
###### Chargement des fichiers et sélection des données pour la station
dataframe <- read.csv("./data/raw/MeteoFrance/H_13_2010-2019.csv", header = TRUE, sep = ";")
# s"lectionner les lignes pour les quelles NUMPOSTE vaut 13055001 ou 13054001 ou 13055029
dataframe <- dataframe[dataframe$NUM_POSTE == 13055001 | dataframe$NUM_POSTE == 13054001 | dataframe$NUM_POSTE == 13055029, ]
# enlève les colonnes vides
dataframe <- dataframe[, colSums(is.na(dataframe)) != nrow(dataframe)]
# on reécrit le fichier
write.csv(dataframe, "./data/raw/MeteoFrance/LCPMARVAL_20102019.csv", row.names = FALSE)

dataframe <- read.csv("./data/raw/MeteoFrance/H_13_latest-2024-2025.csv", header = TRUE, sep = ";")
dataframe <- dataframe[dataframe$NUM_POSTE == 13055001 | dataframe$NUM_POSTE == 13054001 | dataframe$NUM_POSTE == 13055029, ]
dataframe <- dataframe[, colSums(is.na(dataframe)) != nrow(dataframe)]
# on reécrit le fichier
write.csv(dataframe, "./data/raw/MeteoFrance/LCPMARVAL_20242025.csv", row.names = FALSE)

dataframe <- read.csv("./data/raw/MeteoFrance/H_13_previous-2020-2023.csv", header = TRUE, sep = ";")
dataframe <- dataframe[dataframe$NUM_POSTE == 13055001 | dataframe$NUM_POSTE == 13054001 | dataframe$NUM_POSTE == 13055029, ]
dataframe <- dataframe[, colSums(is.na(dataframe)) != nrow(dataframe)]
# on reécrit le fichier
write.csv(dataframe, "./data/raw/MeteoFrance/LCPMARVAL_20202023.csv", row.names = FALSE)

# On charge les fichiers et on les fusionne, si les colonnes ont le même nom on les fusionne, et sinon on garde quand même
# les colonnes différentes
# On charge les fichiers
dataframe1 <- read.csv("./data/raw/MeteoFrance/LCPMARVAL_20102019.csv", header = TRUE, sep = ",")
# on prend qu'à partir de début 2016
dataframe1 <- dataframe1[dataframe1$AAAAMMJJHH >= 2016010100, ]
dataframe2 <- read.csv("./data/raw/MeteoFrance/LCPMARVAL_20242025.csv", header = TRUE, sep = ",")
dataframe3 <- read.csv("./data/raw/MeteoFrance/LCPMARVAL_20202023.csv", header = TRUE, sep = ",")

# on les joint sachant que les colonnes sont pas dans le même ordre
dataframe <- merge(dataframe1, dataframe2, all = TRUE)
dataframe <- merge(dataframe, dataframe3, all = TRUE)

# on réécrit le fichier
write.csv(dataframe, "./data/raw/MeteoFrance/LCPMARVAL_20162025.csv", row.names = FALSE)
