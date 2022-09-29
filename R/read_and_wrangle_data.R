#' @title Stink bug data wrangling 2017
#' @description Read and wrangle data from 2017 stink bug data-set
#' @export
#' @return tidy `tibble`, ready for later merging with the other data-sets prior
#'   to creating JAGS's input data
#' @param raw_stinkbug_data_2017 Character, file path to the stink bug 2017 
#'   data file
read_and_wrangle_stinkbug_2017 <- function(raw_stinkbug_data_2017){
  # read and clean data file
  clean_stinkbug_data_2017 <- raw_stinkbug_data_2017 %>% 
    read.csv2( 
      skip = 4,
      header = TRUE,
      dec = "."
    ) %>%
    as_tibble() %>%
    filter(!is.na(Temp)) %>%
    mutate("Fecha" = as.Date(Fecha, format = "%d/%m/%Y"))    
  
  
  # start wrangling process
  
  ## define a few objects that will we used later
  ### Factores para Equivalente Nezara (solo adultos)
  factor.Nv <- 1
  factor.Pg <- 1.6 
  factor.Em <- 0.6
  factor.Df <- 0.3
  
  ## check that all the locations have the same amount of samples and pull
  ## that value
  n_muestras <- clean_stinkbug_data_2017 %>% 
    group_by(Localidad) %>% 
    summarise(
      "n" = max(Muestra)
    ) %>%
    pull(n) %>% 
    unique() 
  
  
  ## compute weekly data from daily data
  weekly_data_2017 <- clean_stinkbug_data_2017 %>%
    group_by(Localidad, Fecha) %>%
    ########################################################### explain stinkbug grouping. 
    summarise(
      "Fenologia" = names(which.max(table((Fenologia)))),
      "Temperatura" = mean(Temp),
      "Nv.Joven" = sum(Nezara.viri..Chica),
      "Nv.Adulto" = sum(Nezara.viri..Grande),
      "Pg.Joven" = sum(Piezod..Guil..Chica.),
      "Pg.Adulto" = sum(Piezod..Guil..Grande),
      "Em.Joven" = sum(Edessa.med..Chica.),
      "Em.Adulto" = sum(Edessa.med..Grande.),
      "Df.Joven" = sum(Dichelops.f..Chica),
      "Df.Adulto" = sum(Dichelops.f..Grande),
      "Total.Muestras" = max(Muestra),
      "Dist.linea" = unique(Dist.Lineas..m.)
    ) %>%
    mutate(
      "Total.Joven" = Nv.Joven + Pg.Joven + Em.Joven + Df.Joven,
      "Total.Adulto" = Nv.Adulto + Pg.Adulto + Em.Adulto + Df.Adulto,
      "Sup.muestreo" = 1 / Dist.linea
    )
  
  ## compute and add Equivalente Nezara (EqNv) values
  ## group phenologies
  
  weekly_data_2017 <- weekly_data_2017 %>%
    ## compute EqNv for adults (adults + nymphs from stages 4 and 5)
    mutate("EqNv.adulto" = Nv.Adulto * factor.Nv + Pg.Adulto * factor.Pg +
             Em.Adulto * factor.Em + Df.Adulto * factor.Df) %>%
    ## compute EqNv for nymphs (stages 1 to 3)
    mutate("EqNv.joven" = Nv.Joven * factor.Nv + Pg.Joven * factor.Pg +
             Em.Joven * factor.Em + Df.Joven * factor.Df) %>%
    ## add EqNv adult variants: rounded, by samples taken and by m2
    mutate(
      "EqNv.adulto.redondeado" = round(EqNv.adulto),
      "EqNv.adulto.planta" = EqNv.adulto / n_muestras,
      "EqNv.adulto.m2" = EqNv.adulto.planta / Sup.muestreo
    ) %>% 
    ## add EqNv joven variants: rounded, by samples taken and by m2
    mutate(
      "EqNv.joven.redondeado" = round(EqNv.joven),
      "EqNv.joven.planta" = EqNv.joven / n_muestras,
      "EqNv.joven.m2" = EqNv.joven.planta / Sup.muestreo
    ) %>% 
    ## group crop phenologies to "integer phenologies" (e.g. R5.5 to R5)
    mutate(Fenologia = 
             ifelse(
               Fenologia %in% 
                 c(
                   "R5.1",
                   "R5.2",
                   "R5.3",
                   "R5.4",
                   "R5.5",
                   "R5.6",
                   "R5.7",
                   "R5.8",
                   "R5.9"
                 ),  
               "R5", 
               Fenologia
             )
    ) %>%
    mutate(Fenologia = 
             ifelse(
               Fenologia %in% c("R7.2"), 
               "R7", 
               Fenologia
             )
    ) %>% 
    ################################################################################## check which plant phenology grouping you are not using for the manuscript. 
    ## add a crop phenology grouping variant for one of the models. This 
    ## variant has 7 group phenologies: 
    ## 1. Vegetative states (with available data): 8, 9 and 10 -> "V7.9.10"
    ## 2. Blooming reproductive states: R1 and R2 -> "R1.2"
    ## 3. Fruit forming reproductive states: R3 and R4 -> "R3.4" ################################################## "Fruit forming"??? probasebably not, check
    ## 4. Start of grain filling reproductive state: R5 -> "R5"
    ## 5. End of grain filling reproductive state: R6 -> "R6" 
    ## 6. Early maturity reproductive state: R7 -> "R7"
    ## 7. Full maturity reproductive state: R8 -> "R8"
    mutate(Fenologia2 = 
             recode(
               Fenologia,
               "V7" = "V7.9.10",
               "V9" = "V7.9.10",
               "V10" = "V7.9.10",
               "R1" = "R1.2",
               "R2" = "R1.2",
               "R3" = "R3.4",
               "R4" = "R3.4",
               "R5" = "R5",
               "R6" = "R6",
               "R7" = "R7",
               "R8" = "R8"
             ) 
    ) %>% 
    ## add another crop phenology grouping variant for another model. This 
    ## variant has only 3 group phenologies: 
    ## 1. Vegetative states + Reproductive states from R1 to R3 -> "VR1.2.3"
    ## 2. Reproductive states from R4 to R7 -> "R4.5.6.7"
    ## 3. Full maturity reproductive state R8 -> "R8"
    mutate(Fenologia3 = 
             recode(
               Fenologia,
               "V7" = "VR1.2.3",
               "V9" = "VR1.2.3",
               "V10" = "VR1.2.3",
               "R1" = "VR1.2.3",
               "R2" = "VR1.2.3",
               "R3" = "VR1.2.3",
               "R4" = "R4.5.6.7",
               "R5" = "R4.5.6.7",
               "R6" = "R4.5.6.7",
               "R7" = "R4.5.6.7",
               "R8" = "R8"
             )
    ) %>% 
    ## Add categorical crop phenology variable (ascending integers). 
    mutate("nFenologia" = 
             recode(
               Fenologia2,
               "V7.9.10" = 1,
               "R1.2" = 2,
               "R3.4" = 3,
               "R5" = 4,
               "R6" = 5,
               "R7" = 6,
               "R8" = 7
             )
    ) %>% 
    ## re-order a few columns  
    select( ############################################# if theres a bug it may be here inside the ungroup()
      #ungroup(weekly_data_2017), ############################################ check at the end if the tibble is the same as CCsemana_2017, removing this ungroup may do something
      "Localidad",
      "Fecha",
      "Total.Muestras",
      "Fenologia",
      "Fenologia2",
      "Fenologia3",
      "nFenologia",
      everything()
    ) %>%
    ## recode locations as ascending integers
    mutate("nLocalidad" =
             recode(
               Localidad,
               "America" = 1,
               "Diego de Alvear" = 2,
               "La Francia" = 3,
               "Leones" = 4,
               "Necochea" = 5,
               "Pehuajo" = 6,
               "San Vicente" = 7,
               "Zavalla" = 8
             )
    ) %>%
    ## Add crop phenology dummy variables.
    mutate("V7.9.10" = ifelse(Fenologia2 == "V7.9.10", 1, 0)) %>%
    mutate("R1.2" = ifelse(Fenologia2 == "R1.2", 1, 0)) %>%
    mutate("R3.4" = ifelse(Fenologia2 == "R3.4", 1, 0)) %>%
    mutate("R5" = ifelse(Fenologia2 == "R5", 1, 0)) %>%
    mutate("R6" = ifelse(Fenologia2 == "R6", 1, 0)) %>%
    mutate("R7" = ifelse(Fenologia2 == "R7", 1, 0)) %>%
    mutate("R8" = ifelse(Fenologia2 == "R8", 1, 0)) %>%
    mutate("VR1.2.3" = ifelse(Fenologia3 == "VR1.2.3", 1, 0)) %>%
    mutate("R4.5.6.7" = ifelse(Fenologia3 == "R4.5.6.7", 1, 0))  
  #add_column('n_muestras' = rep(n_muestras, nrow(weekly_data_2017))) ##### already added at the beggining. 'Total.Muestras' 
  
  
  ## add year var for dynamic branching by year
  weekly_data_2017 <- weekly_data_2017 %>%
    mutate(year = lubridate::year(Fecha))
  
  
  ## Add column with time series length
  ### compute value
  time_series_length <- weekly_data_2017 %>%
    group_by(Localidad) %>%
    tally() %>%
    rename("time_series_length" = n)
  
  ### add it to dataset
  weekly_data_2017 <- left_join(
    x = weekly_data_2017,
    y = time_series_length,
    by = c(
      "Localidad" = "Localidad"
    )
  )

  ## Add column with week number vector per locality. 
  ### Compute the vector
  week <- time_series_length %>% 
    pull(time_series_length) %>%
    map(~{c(1:.x)}) %>% 
    unlist()
  ### Add it to dataset
  weekly_data_2017 <- weekly_data_2017 %>%
    ungroup() %>% 
    mutate(week = week) %>% 
    #### re-order columns
    select(Localidad, Fecha, week, everything()) 



  return(weekly_data_2017)
    
  
  
}  
  # ## Cantidad de semanas que se midieron en cada Localidad ######################################## this should be a new var called "total weeks" summarise, max, etc. 
  # ###################### maybe make a useful_objects[] list and add all of these things. 
  # nsemana <- CCsemana_2017 %>%
  #   group_by(Localidad) %>%
  #   tally() %>%
  #   rename("Cantidad de Semanas" = n)
  # 
  # Localidades <- as.character(nsemana$Localidad)
  # 
  ## Categórica de Semana
  # Cantidad total de localidades
  # nLocalidades <- length(nsemana$Localidad)
  
  # # Cantidad de semanas por Localidad
  # semanasLoc <- rep(0, nLocalidades)
  # for (i in 1:nLocalidades) {
  #   semanasLoc[i] <- nrow(CCsemana_2017[CCsemana_2017$nLocalidad == i, ])
  # }
  # 
  # Categórica

#   time_series_week_vector <- time_series_length %>%
#     pull(time_series_length)
 
# week <- time_series_week_vector %>%
#  # Create list of vectors containing week time series
#  map(~{c(1:.x)}) %>% 
#  unlist()




#   Semana <- c(1:semanasLoc[1])
#   for (i in 2:nLocalidades) {
#     Semana <- c(Semana, (1:semanasLoc[i]))
#   }
  # 
  # # Control Threshold Variable
  # 
  # CCsemana_2017 <- CCsemana_2017 %>% 
  #   mutate("threshold_EqNv.adulto.m2" = recode(Fenologia, 
  #                                       "R3.4" = 1.4,
  #                                       "R5" = 2.9,
  #                                       "R6" = 7.1))
  # 
  # 
  # # Unimos Categórica al set de datos y ordenamos columnas
  # CCsemana_2017 <- CCsemana_2017 %>%
  #   ungroup() %>%
  #   mutate("Semana" = Semana) %>%
  #   select(
  #     "Localidad", 
  #     "nLocalidad", 
  #     "Fecha", 
  #     "Total.Muestras", 
  #     "Semana",
  #     "Fenologia", 
  #     "Fenologia2", 
  #     "nFenologia", 
  #     "V7.9.10", 
  #     "R1.2",
  #     "R3.4",
  #     "R5",
  #     "R6",
  #     "R7",
  #     "R8", 
  #     everything()
  #   )
  # 
  # 
  # col.chinches <- colnames(all_of(CCsemana_2017))
  # 
  # ### Las abundancias de chinches son por metro lineal. Las pasamos a metro cuadrado usando la distancia entre hileras que se encuentra en el excel.
  # 
  # ### Datos NOAA
  # "HACE FALTA CAMBIAR LOS NOMBRES DE LAS LOCALIDADES PARA QUE COINCIDAN CON ECMWF "
  # NOAAdata_2017 <- 
  #   bind_rows(
  #     NOAAdata_2017, # Esto es porque usamos la misma estación 2 veces
  #     mutate(NOAAdata_2017[NOAAdata_2017$NAME == "GENERAL PICO, AR", ],
  #            NAME = "GENERAL PICO, AR, 2"
  #     )
  #   ) %>%
  #   mutate("Localidad" = recode(NAME,
  #                               "GENERAL PICO, AR" = "America",
  #                               "MINISTRO PISTARINI, AR" = "San Vicente",
  #                               "MARCOS JUAREZ, AR" = "Leones",
  #                               "MAR DEL PLATA AERO, AR" = "Necochea",
  #                               "PILAR OBSERVATORIO, AR" = "La Francia",
  #                               "JUNIN, AR" = "Diego de Alvear",
  #                               "ROSARIO, AR" = "Zavalla",
  #                               "GENERAL PICO, AR, 2" = "Pehuajo"
  #   )) %>%
  #   # Promedio de temperatura semanal
  #   group_by(week = week(DATE)) %>%
  #   mutate("NOAA.air.temp.weekly.avg" = mean(TAVG)) %>%
  #   ungroup()
  # 
  # 
  # 
  # 
  # 
  
  
  
  
  
  
  
  ############################################# dont forget to select the new columns, such as n_semana 
  
  
  
  
  
# }


