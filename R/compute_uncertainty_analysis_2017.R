#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title Plot uncertainty analysis
#' @param useful_objects_2017
#' @param wrangled_stinkbug_data_2017
#' @return 
#' @author franfram
#' @export
compute_uncertainty_analysis_2017 <- function(useful_objects_2017,
                                              wrangled_stinkbug_data_2017) {

  #Read data
  #make list and map read
  lista.datos.incertidumbre.bp <- read_csv(
    here("data", "ua", "lista.datos.uncertainty.EqNv.dat.SanVicente")
    )






  # Store useful objects
  ## Wrangled stinkbug data
  CCsemana_2017 <- wrangled_stinkbug_data_2017
  ## Localities string vector
  Localidades  <- useful_objects_2017[['for_jags']][['localities']] 
  semanasLoc <- useful_objects_2017[['for_jags']][['time_series_length']]  
  n.muestras <- wrangled_stinkbug_data_2017$Total.Muestras %>% unique


  datos.plot <- as_tibble(lista.datos.incertidumbre.bp) %>%
    filter(post.sample == c(1:5000)) %>%
    arrange(-cant.muestras, replica, post.sample)

  datos.plot$cant.muestras <- factor(datos.plot$cant.muestras,
    levels =
      c(180, 140, 120, 100, 80, 60, 40, 20, 10)
  )
  datos.plot$replica <- as.factor(datos.plot$replica)



  # Por ahora, el umbral está definido, por cada localidad, como el valor 
  # observado en la última semana
  umbral <- list()
  for (j in 7) {  ## umbral densidad de chinches
    umbral[[j]] <- filter(CCsemana_2017, Localidad == Localidades[j] &
                                      week == semanasLoc[j]) %>%
      select(EqNv.adulto.redondeado) %>%
      pull() / n.muestras 
  }



  prob.umbral <- list()
  for (j in 7) {  
    prob.umbral[[j]] <- lista.datos.incertidumbre.bp %>%
      filter(Localidad == Localidades[j] & semana == semanasLoc[j]) %>%
      mutate("sup.umbral" = post.Y.A.F1.EqNv.dat.planta > umbral[[j]]) %>%
      group_by(cant.muestras, replica) %>%
      summarise("prob.sup.umbral" = ((sum(sup.umbral) / n()) * 100),
                "Varianza" = var(post.Y.A.F1.EqNv.dat.planta),
                "Precision" = 1/(var(post.Y.A.F1.EqNv.dat.planta))) %>%
      ungroup() %>%
      mutate("Precision.relativa"= (Precision / (max(Precision))) * 100) %>%
      arrange(-cant.muestras)
    
    prob.umbral[[j]]$cant.muestras <- factor(prob.umbral[[j]]$`cant.muestras`, levels=
                                      c(180, 140, 120, 100, 80, 60, 40, 20, 10))
  }


  prob.umbral.mean <- list()
  for (j in 7) {
    prob.umbral.mean[[7]] <- prob.umbral[[7]] %>%
      group_by(cant.muestras) %>% 
      summarize(
        "prob.min" = min(prob.sup.umbral),
        "prob.prom" = round(mean(prob.sup.umbral), digits = 2),
        "prob.max" = max(prob.sup.umbral),
        "prec.rel.prom" = round(mean(Precision.relativa), digits = 2)
      ) 
  }





  prob.umbral.mean.tabla <- list()
  for (j in 7) {
    prob.umbral.mean.tabla[[7]] <- prob.umbral.mean[[7]] %>% 
      rename(
        "Probabilidad mínima" = prob.min,
        "Probabilidad promedio" = prob.prom,
        "Probabilidad máxima" = prob.max,
        "Precisión relativa promedio" = prec.rel.prom,
        "Cantidad de muestras" = cant.muestras
      )
  }

  #kable(prob.umbral.mean.tabla[[7]])

  

  textoPrec <- paste0(
    "Figura 6. Precisión relativa en función del Tamaño Muestral para la Localidad San Vicente. Esta variable",
    "\nse calcula respecto a la precisión máxima obtenida entre los distintos tamaños muestrales y réplicas"
  )


  textoLineplot <- paste0(
    "Figura 3. Simulaciones de dinámica poblacional de EqNv/planta utilizando distintos tamaños muestrales. Cada panel",
    "\nrepresenta un tamaño muestral distinto y, dentro de cada uno de estos, cada color representa una réplica distinta.",
    "\nSe grafican 80 simulaciones (líneas) por cada réplica"
  )

    
  ## tomamos 100 muestras de la posterior y luego reordenamos las rows 
  datos.plot <- as_tibble(lista.datos.incertidumbre.bp) %>%
    filter(post.sample == c(1:100)) %>%
    arrange(-cant.muestras, replica, post.sample)

  datos.plot$cant.muestras <- factor(datos.plot$cant.muestras, levels=
                                      c(180, 140, 120, 100, 80, 60, 40, 20, 10))
  datos.plot$replica <- as.factor(datos.plot$replica)


  labs_muestras <- c(
    '10' = "10 muestras",
    '20' = "20 muestras",
    '40' = "40 muestras",
    '60' = "60 muestras",
    '80' = "80 muestras",
    '100' = "100 muestras",
    '120' = "120 muestras",
    '140' = "140 muestras",
    '180' = "180 muestras"
  )

  labs_replicas <- c(
    '1' = "Réplica 1",
    '2' = "Réplica 2",
    '3' = "Réplica 3",
    '4' = "Réplica 4",
    '5' = "Réplica 5",
    '6' = "Réplica 6",
    '7' = "Réplica 7",
    '8' = "Réplica 8",
    '9' = "Réplica 9"
  )

  scatterplot.incertidumbre <- list()
  ridgeplot.incertidumbre <- list()
  boxplot.incertidumbre <- list()



  textoRidge <- paste0(
    "Figura 4. Incertidumbre en la predicción de EqNv/Planta en la última semana de los datos para la Localidad San Vicente.",
    "\nLa línea discontinua muestra el valor observado en la última semana y cada panel representa el resultado obtenido en una",
    "\nréplica distinta."
  )


  prob.umbral[[7]]$cant.muestras <- factor(prob.umbral[[7]]$`cant.muestras`, levels=
                                      c(10, 20, 40, 60, 80, 100, 120, 140, 180))


  for (j in 7) {
    ridgeplot.incertidumbre[[j]] <-
      ggplot(
        data = filter(datos.plot, Localidad == Localidades[j] & semana == semanasLoc[j]),
        # usamos semanasLoc[j] porque el forecast es sobre la última semana de los datos
        aes(x = post.Y.A.F1.EqNv.dat.planta, y = cant.muestras, fill = ..x..)
      ) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      scale_fill_viridis(option = "C") +
      labs(
        title = "Posterior de EqNv/Planta en la semana del Forecast según Tamaño Muestral",
        subtitle = Localidades[j],
        y = "Cantidad de Muestras", # "Cantidad \n de \n Muestras"
        x = "Incertidumbre en la predicción de EqNv/m lineal"#,
        #caption = textoRidge
      ) +
      geom_vline(xintercept = umbral[[j]], linetype = "dashed") +
      coord_cartesian(xlim = c(0, 5)) +
      # scale_x_reverse() +
      facet_wrap(~replica,
        labeller = labeller(replica = labs_replicas)
      ) +
      # theme_ipsum() +   # library(hrbrthemes) +
      theme(
        title = element_text(size = 10),
        legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(hjust = 0)
      )
  }



  lineplot.incertidumbre <- ggplot() +
    scale_x_continuous(breaks = seq(1, semanasLoc[7], 1)) +
    scale_y_continuous(breaks = seq(0, 7, 0.5)) +
    labs(y="Posterior de EqNv/m observado",
        x="Semana",
        #title = Localidades[[7]],
        caption = textoLineplot) +
    geom_segment(data = tibble("semanasLoc" = semanasLoc[[7]], 
                              "umbral" = umbral[[7]]),
                aes(x=(semanasLoc - 0.5),
                    xend=(semanasLoc + 0.5),
                    y=umbral,
                    yend=umbral)) +
    coord_cartesian(ylim = c(0, 2.5), xlim = c(1,10)) +
    theme(plot.caption = element_text(hjust = 0)) #+
    #theme_dark()
    #theme(panel.background = element_rect(fill = "gray60")) #+
    # theme(axis.title.x = element_text(#face = "bold",
    #                                   vjust = -0.2), 
    #       axis.title.y = element_text(#face = "bold",
    #                                   vjust = 2))


  for (i in 1:40) { # 80
    lineplot.incertidumbre <- lineplot.incertidumbre + 
      geom_line(
        data = filter(datos.plot, replica == 1 & post.sample == i), 
        aes(x = semana, 
            y = post.Y.A.F1.EqNv.dat.planta), 
        colour = "#762a83"
      ) +
      geom_line(
        data = filter(datos.plot, replica == 2 & post.sample == i),
        aes(x = semana,
            y = post.Y.A.F1.EqNv.dat.planta),
        colour = "#9970ab"
      ) +
      geom_line(
        data = filter(datos.plot, replica == 3 & post.sample == i),
        aes(x = semana,
            y = post.Y.A.F1.EqNv.dat.planta),
        colour = "#c2a5cf" # antes "#f7f7f7"
      ) +
      geom_line(
        data = filter(datos.plot, replica == 4 & post.sample == i),
        aes(x = semana,
            y = post.Y.A.F1.EqNv.dat.planta),
        colour = "#e7d4e8"
      ) +
      geom_line(
        data = filter(datos.plot, replica == 5 & post.sample == i),
        aes(x = semana,
            y = post.Y.A.F1.EqNv.dat.planta),
        colour = "#f7f7f7" # antes "#c2a5cf"
      ) +
      geom_line(
        data = filter(datos.plot, replica == 6 & post.sample == i),
        aes(x = semana,
            y = post.Y.A.F1.EqNv.dat.planta),
        colour = "#d9f0d3"
      ) +
      geom_line(
        data = filter(datos.plot, replica == 7 & post.sample == i),
        aes(x = semana,
            y = post.Y.A.F1.EqNv.dat.planta),
        colour = "#a6dba0"
      ) +
      geom_line(
        data = filter(datos.plot, replica == 8 & post.sample == i),
        aes(x = semana,
            y = post.Y.A.F1.EqNv.dat.planta),
        colour = "#5aae61"
      ) +
      geom_line(
        data = filter(datos.plot, replica == 9 & post.sample == i),
        aes(x = semana,
            y = post.Y.A.F1.EqNv.dat.planta),
        colour = "#1b7837"
      ) +
      facet_wrap(~cant.muestras,
                labeller = labeller(cant.muestras = labs_muestras))
    
  }


  datos.plot_incertidumbre <- datos.plot %>%
    filter(Localidad == Localidades[7], semana == semanasLoc[7]) %>%
    group_by(cant.muestras, replica) %>%
    summarize("var" = var(post.Y.A.F1.EqNv.dat.planta)) %>%
    ungroup()

  datos.plot_incertidumbre_mean <- datos.plot_incertidumbre %>%
    group_by(cant.muestras) %>%
    summarize("mean_var" = mean(var)) %>%
    mutate("mean_var_relativa" = mean_var / max(mean_var))


  datos.plot_incertidumbre_mean$cant.muestras <- factor(datos.plot_incertidumbre_mean$cant.muestras,
    levels =
      c(10, 20, 40, 60, 80, 100, 120, 140, 180)
  )


  # "falta agregarle HPDIs"
  plot_prec_bp <- list()
  for (j in 7) {
    plot_prec_bp[[j]] <- datos.plot_incertidumbre_mean %>%
      ggplot() +
      geom_line(aes(x = cant.muestras, y = mean_var_relativa, group = 1)) +
      #  scale_x_reverse() +
      labs(
        y = "Incertidumbre promedio relativa",
        x = "Tamaño Muestral utilizado" # ,
        # caption = textoPrec
      ) +
      theme(
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 11),
        plot.caption = element_text(
          hjust = 0,
          size = 9
        )
      )
  }



  return(
    list(
      'lineplot' = lineplot.incertidumbre, 
      'ridgeplot' = ridgeplot.incertidumbre[[7]],
      'uncertainty' = plot_prec_bp[[7]]
    )
  )


}
