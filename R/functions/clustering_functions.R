#' Fonction pour effectuer la régression et calculer les anomalies
#' @param data Données à traiter
#' @param var Variable à désaisonnaliser
#' @param trend Indique si la tendance doit être incluse dans la régression. Par défaut, TRUE.
#' 
#' @return Liste contenant les données désaisonnalisées (deux nouvelles colonnes) et le modèle de régression
fit_regression <- function(data, var, trend = TRUE) {
  if (!trend) {
    formula <- as.formula(paste(var, "~ sin(2 * pi * time) + cos(2 * pi * time) +",
                                "sin(4 * pi * time) + cos(4 * pi * time) +",
                                "sin(6 * pi * time) + cos(6 * pi * time) +",
                                "sin(8 * pi * time) + cos(8 * pi * time)"))
  } else {
    formula <- as.formula(paste(var, "~ 1 + time +",
                                "sin(2 * pi * time) + cos(2 * pi * time) +",
                                "sin(4 * pi * time) + cos(4 * pi * time) +",
                                "sin(6 * pi * time) + cos(6 * pi * time) +",
                                "sin(8 * pi * time) + cos(8 * pi * time)"))
  }
  
  # Ajustement du modèle de régression
  model <- lm(formula, data = data)
  
  nom <- paste0(var, "_modeled")
  nomp <- paste0(var, "_ano")
  
  # Retourner les données désaisonnalisées
  data[[nom]] <- predict(model, newdata = data)
  data[[nomp]] <- data[[var]] - data[[nom]]
  
  return(list(data = data, model = model))
}

#' Plot clustering results. Attention, figpath doit etre défini dans l'environnement global.
#' 
#' @param data Data frame containing the data
#' @param clcolname Name of the column containing the cluster number
#' @param sub_dir Subdirectory where to save the plots
#' @param nom Name of the clustering method
#' @param windu Name of the column containing the u component of the wind
#' @param windv Name of the column containing the v component of the wind
#' @param vars Names of the columns containing the variables to plot
#' @param varnames Names of the variables to plot
#' @param savefig Logical indicating whether to save the plots
#' 
#' @return NULL
#' 
#' @example plot_clustering_results(mean_dayanddn, 'gmm_cluster', 'GMMboxplots_roses', 'GMM')
old_plot_clustering_results <- function(data, clcolname, sub_dir, nom, windu = windu, windv = windv,
                                    vars = c('t2m_ano', 'relative_humidity_ano', 'surfacepressure_ano', 'ssrd_ano', 'windu', 'windv', 'wd', 'ws', 'totalprecipitation'),
                                    varnames = c("Anomalies de température_a_2m", "Anomalies de humidité_relative", "Anomalies de Surface_pressure", "Anomalies de Solar_surface_radiation_downwards", 'vent_u', 'vent_v', 'wind dir', 'wind speed', 'Précipitations totales'),
                                    savefig = TRUE) {
  
  # Boxplots
  cluster_count <- as.data.frame(table(data[[clcolname]]))
  names(cluster_count) <- c("cluster", "count")
  cluster_count$label <- paste0("Cluster ", cluster_count$cluster, "\n(N=", cluster_count$count, ")")
  data$cluster_label <- factor(data[[clcolname]], 
                               levels = cluster_count$cluster, 
                               labels = cluster_count$label)
  for (i in 1:length(vars)) {
    plot_box <- ggplot(data, aes(x = cluster_label, y = !!sym(vars[i]), fill = as.factor(!!sym(clcolname)))) +
      geom_boxplot() +
      labs(title = paste(varnames[i], "par", clcolname), 
           x = clcolname, 
           y = varnames[i],
           fill = "Cluster") +
      theme_minimal() +
      scale_fill_viridis_d()
    
    # Afficher le plot à l'écran
    print(plot_box)
    
    # Sauvegarder le plot si savefig est TRUE
    if (savefig) {
      ggsave(file.path(figpath, sub_dir, paste("boxplot_ano_", varnames[i], "_clustering_", nom, ".png", sep = '')), 
             width = 10, height = 6, bg = "white")
    }
  }
  
  # Wind rose
  plot_rose <- ggplot(data, aes(x = !!sym("wd"), y = !!sym("ws"), color = as.factor(!!sym(clcolname)))) +
    geom_point() +
    coord_polar(start = 0) +  
    scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 45), labels = c("N", "NE", "E", "SE", "S", "SO", "O", "NO", "N")) +
    labs(x = "Direction", y = "Vitesse du vent", color = "Cluster") +
    theme_minimal() +
    facet_wrap(as.formula(paste("~", clcolname))) +
    scale_color_viridis_d()
  
  # Afficher le wind rose à l'écran
  print(plot_rose)
  
  # Sauvegarder le wind rose si savefig est TRUE
  if (savefig) {
    ggsave(file.path(figpath, sub_dir, paste(nom, "_rose_par_cluster.png", sep = '')), 
           width = 10, height = 6, bg = "white")
  }
  cluster_count <- table(data[[clcolname]])
  
  print(cluster_count)
}

plot_clustering_results <- function(data, clcolname, sub_dir, nom, 
                                    vars_dict = list(
                                      t2m_ano = "Anomalies de température_a_2m",
                                      relative_humidity_ano = "Anomalies de humidité_relative",
                                      surfacepressure_ano = "Anomalies de Surface_pressure",
                                      ssrd_ano = "Anomalies de Solar_surface_radiation_downwards",
                                      windu = "vent_u",
                                      windv = "vent_v",
                                      wd = "wind dir",
                                      ws = "wind speed",
                                      totalprecipitation = "Précipitations totales"
                                    ),
                                    savefig = TRUE,
                                    windf_col = "ws",
                                    windd_col = "wd") {
  
  # Calcul des labels de clusters
  cluster_count <- as.data.frame(table(data[[clcolname]]))
  names(cluster_count) <- c("cluster", "count")
  cluster_count$label <- paste0("Cluster ", cluster_count$cluster, "\n(N=", cluster_count$count, ")")
  data$cluster_label <- factor(data[[clcolname]], 
                               levels = cluster_count$cluster, 
                               labels = cluster_count$label)
  
  # Boucle sur les variables du dictionnaire
  for (var in names(vars_dict)) {
    nom_var <- vars_dict[[var]]
    
    plot_box <- ggplot(data, aes(x = cluster_label, y = !!sym(var), fill = as.factor(!!sym(clcolname)))) +
      geom_boxplot() +
      labs(title = paste(nom_var, "par", clcolname), 
           x = clcolname, 
           y = nom_var,
           fill = "Cluster") +
      theme_minimal() +
      scale_fill_viridis_d()
    
    print(plot_box)
    
    if (savefig) {
      ggsave(file.path(figpath, sub_dir, paste0("boxplot_ano_", nom_var, "_clustering_", nom, ".png")), 
             width = 10, height = 6, bg = "white")
    }
  }
  
  # Wind rose
  plot_rose <- ggplot(data, aes(x = !!sym(windd_col), y = !!sym(windf_col), color = as.factor(!!sym(clcolname)))) +
    geom_point() +
    coord_polar(start = 0) +  
    scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 45), 
                       labels = c("N", "NE", "E", "SE", "S", "SO", "O", "NO", "N")) +
    labs(x = "Direction", y = "Vitesse du vent", color = "Cluster") +
    theme_minimal() +
    facet_wrap(as.formula(paste("~", clcolname))) +
    scale_color_viridis_d()
  
  print(plot_rose)
  
  if (savefig) {
    ggsave(file.path(figpath, sub_dir, paste0(nom, "_rose_par_cluster.png")), 
           width = 10, height = 6, bg = "white")
  }
  
  print(table(data[[clcolname]]))
  
  invisible(NULL)  # pas de retour visible
}
