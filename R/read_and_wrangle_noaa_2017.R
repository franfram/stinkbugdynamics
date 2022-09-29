#' @title NOAA data wrangling 2017
#' @description Read and wrangle 2017 data from NOAA data-set, includes only
#'   temperature values
#' @export
#' @return tidy `tibble`, ready for later merging with the other data-sets prior
#'   to creating JAGS's input data. 
#' @param raw_noaa_data_2017 Character, file path to the NOAA 2017 
#'   data file
read_and_wrangle_noaa_2017 <- function(raw_noaa_data_2017){
  # read
  noaa_data_2017 <- raw_noaa_data_2017 %>% 
    read_csv() %>% 
    as_tibble()
  
  # wrangle
  noaa_data_2017 <- 
    ## same station is used for two different locations, so we duplicate 
    ## one of them
    bind_rows(
      noaa_data_2017, # Esto es porque usamos la misma estación 2 veces
      mutate(noaa_data_2017[noaa_data_2017$NAME == "GENERAL PICO, AR", ],
             NAME = "GENERAL PICO, AR, 2"
      )
    ) %>%
    ## recode from station name to location name
    mutate("Localidad" = recode(NAME,
                                "GENERAL PICO, AR" = "America",
                                "MINISTRO PISTARINI, AR" = "San Vicente",
                                "MARCOS JUAREZ, AR" = "Leones",
                                "MAR DEL PLATA AERO, AR" = "Necochea",
                                "PILAR OBSERVATORIO, AR" = "La Francia",
                                "JUNIN, AR" = "Diego de Alvear",
                                "ROSARIO, AR" = "Zavalla",
                                "GENERAL PICO, AR, 2" = "Pehuajo"
    )) %>%
    ## compute weekly mean temperature
    group_by(week = week(DATE)) %>%
    mutate("NOAA.air.temp.weekly.avg_2017" = mean(TAVG)) %>%
    ungroup()
    
    # return wrangled tibble
    noaa_data_2017
}

