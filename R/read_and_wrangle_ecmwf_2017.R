#' @title ECMWF data wrangling 2017
#' @description Read and wrangle 2017 data from European Centre for Medium-Range
#'   Weather Forecasts data-set. 
#'   ECMWF model: ERA5-land reanalysis data-set
#'   Vars retrieved: 2m temperature, 
#'                   2m dewpoint temperature, and 
#'                   total precipitation
#' @export
#' @return tidy `tibble`, ready for later merging with the other data-sets prior
#'   to creating JAGS's input data. 
#' @param raw_ecmwf_data_2017 Character, file path to the ECMWF 2017 
#'   data file  
read_and_wrangle_ecmwf_2017 <- function(raw_ecmwf_data_2017){
  metdata_2017_raw <- raw_ecmwf_data_2017 %>% 
    # read
    read.csv() %>% 
    # change format of vars
    mutate(
      'valid_time' = ymd_hms(valid_time),
      'Date' = date(valid_time),
      'Time' = format(valid_time, format = "%H:%M:%S"),
      'Day' = day(valid_time)
    ) %>% 
    # Kelvin to °C
    mutate(  
      't2m' = t2m - 273.15,
      'd2m' = d2m - 273.15
    ) %>% 
    # remove (unintentionally) downloaded data from an year other than 2017
    filter(year(Date) == '2017') %>% 
    select('station', 'lat', 'lon', 'Date', 'Day', 'Time', 't2m', 'tp', 'd2m') %>% 
    ungroup()
  
  
  
  # Compute daily means 
  metdata_2017_daily <- metdata_2017_raw %>%  
    group_by(station, lat, lon, Date, Day) %>% 
    summarise( # compute daily means
      't2m_dmean' = mean(t2m), 
      'tp_dmean' = mean(tp), 
      'd2m_dmean' = mean(d2m)
    ) %>% 
    select('station', 'lat', 'lon', 'Date', 't2m_dmean', 'tp_dmean', 'd2m_dmean') %>% 
    ungroup()
  
  # Compute weekly means from daily means    
  metdata_2017_weekly <- metdata_2017_daily %>% 
    group_by(station, week = week(Date)) %>% 
    mutate(
      't2m_wmean' = mean(t2m_dmean), 
      'tp_wmean' = mean(tp_dmean), 
      'd2m_wmean' = mean(d2m_dmean)
    ) %>% 
    ungroup() %>% 
    na.omit() %>% 
    # add location name to each station
    mutate("Localidad" = recode(
      station,
      "GENERAL_PICO" = "America",
      "MINISTRO_PISTARINI" = "San Vicente",
      "MARCOS_JUAREZ" = "Leones",
      "MAR DEL PLATA AERO" = "Necochea",
      "PILAR OBSERVATORIO" = "La Francia",
      "JUNIN" = "Diego de Alvear",
      "ROSARIO" = "Zavalla",
      "GENERAL_PICO_2" = "Pehuajo"
      )
    )     
  
  # useful objects for plots 
  
  plot_labeling_data <- metdata_2017_weekly %>%
    # select variables of interest for plotting 
    select(station, lat, lon, Date, week, t2m_wmean, tp_wmean, d2m_wmean) %>%
    # store data only from each sunday of each week 
    group_by(station, week) %>%
    slice_head() 
  
  # pull a dates vector from a location (does't matter which location)
  plot_dates <- plot_labeling_data %>% 
    filter(station == "GENERAL_PICO") %>% 
    pull(Date) %>% 
    as.character()
  
  
  # return ecmwf weekly dataset for target
  metdata_2017_weekly

}    

