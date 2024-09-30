# -------------------------------------------------------------------------
#
# Script name: 03_process_climate_data.R
#
# Purpose of script: Process climate data from esarchive (BSC ES Dept internal storage)
# Author(s): Bruno M. Carvalho, Alba Llabrés-Brustenga
# Date Created: 2023-08-29
# Email: bruno.carvalho@bsc.es
#
# -------------------------------------------------------------------------

# Load packages
library(lubridate)
library(startR)
library(zeallot)
library(raster)
library(sf)
#library(tidyverse)
library(exactextractr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(data.table)

# packages <- c()
# install.packages(setdiff(packages, rownames(installed.packages())))
# lapply(packages, require, character.only = TRUE)

# -------------------------------------------------------------------------


# Base shapefile ----------------------------------------------------------

# file path for 2022 Brazil municipality shapefile from IBGE
fp <- ("/esarchive/shapefiles/IBGE/BR_Municipios_2022.shp")

# read shapefile
br_shp <- st_read(fp) %>% 
  st_transform(br_shp, crs = 'EPSG:4326') %>% 
  # remove lagoons and Noronha island
  filter(CD_MUN != "4300001" & CD_MUN != "4300002" & CD_MUN != "2605459")

# list of states (to extract climate data by state)
states <- sort(unique(br_shp$SIGLA_UF))
#states <- states[1]

# Load data with startR ---------------------------------------------------
start.month <- 1
end.month <- 12
start.year <- 2001
end.year <- 2021

# obtain dates in the format of netcdf in /esarchive
for(mm in start.month:end.month){
  if (mm == start.month){
    dates <- as.Date(paste0(start.year:end.year, '-', start.month, '-1'))
  } else {
    dates <- append(dates, as.Date(paste0(start.year:end.year, '-', mm, '-1')))
  }
}
dates <- dates[order(dates)]
dates_reanalysis <- array(substr(gsub('-', '', as.character(dates)),1,6), 
                          dim = c(time = (end.month - start.month + 1), 
                                  syear = (end.year - start.year + 1)))

# era5land ----------------------------------------------------------------

# select variable:
variables <- c('tas', 'tasmax', 'tasmin')
dataset <- 'era5land' # options: 'era5land', 'era5', 'seas51', 'chirps' 

era5land_data <- list()
for(w in 1:length(states)){
  
  shp <- filter(br_shp, SIGLA_UF == states[w])
  
  # shapefile bounding box
  lons.min <- st_bbox(shp)["xmin"] - 1
  lons.max <- st_bbox(shp)["xmax"] + 1
  lats.min <- st_bbox(shp)["ymin"] - 1
  lats.max <- st_bbox(shp)["ymax"] + 1
  
  for(var in variables){
    # obtain path:
    if (dataset == 'era5land'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr' | var == 'tas' | var == 'tdps'){'f1h'} else {0}
      path_dataset <- paste0('/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_', ff, '/$var$_$sdate$.nc')
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    } else if (dataset == 'era5'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr' | var == 'tas' | var == 'tdps'){'f1h'} else {0}
      path_dataset <- paste0('/esarchive/recon/ecmwf/era5/monthly_mean/$var$_', ff, '-r1440x721cds', '/$var$_$sdate$.nc')
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    }else if (dataset == 'seas51'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr'){'s0-24h'} else if (var == 'tas' | var == 'tdps'){'f6h'} else {0}
      path_dataset <- paste0('/esarchive/exp/ecmwf/system51c3s/monthly_mean/$var$_', ff, '/$var$_$sdate$.nc')
      selected_dates <- dates_seasonalforecast
      forecast <- TRUE
    } else if (dataset == 'chirps'){
      path_dataset <- '/esarchive/obs/ucsb/chirps-v2/monthly_mean/$var$/$var$_$sdate$.nc'
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    } else {
      stop('unknown dataset')
    }
    
    # load data:
    if (forecast){
      rm(data)
      data <- Start(dat = path_dataset,
                    var = var,
                    sdate = selected_dates,
                    time = 1:6,
                    ensemble = 'all',
                    split_multiselected_dims = TRUE,
                    latitude = startR::values(list(lats.min, lats.max)),
                    latitude_reorder = Sort(decreasing = TRUE),
                    longitude = startR::values(list(lons.min, lons.max)),
                    longitude_reorder = CircularSort(-180, 180),
                    synonims = list(latitude = c('lat', 'latitude'),
                                    longitude = c('lon', 'longitude')),
                    return_vars = list(latitude = 'dat',
                                       longitude = 'dat',
                                       time = c('sdate')),
                    retrieve = TRUE)
    } else {
      rm(data)
      data <- Start(dat = path_dataset,
                    var = var,
                    sdate = selected_dates,
                    split_multiselected_dims = TRUE,
                    latitude = startR::values(list(lats.min, lats.max)),
                    latitude_reorder = Sort(decreasing = TRUE),
                    longitude = startR::values(list(lons.min, lons.max)),
                    longitude_reorder = CircularSort(-180, 180),
                    synonims = list(latitude = c('lat', 'latitude'),
                                    longitude = c('lon', 'longitude')),
                    return_vars = list(latitude = 'dat',
                                       longitude = 'dat',
                                       time = c('sdate')),
                    retrieve = TRUE)
    }
    
    # transform units
    if (var =='tas'){
      tas <- data - 273.15
      attr(tas, 'Variables')$common$tas$units <- "C"
      tas <- ClimProjDiags::Subset(tas, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'tasmax'){
      tasmax <- data - 273.15
      attr(tasmax, 'Variables')$common$tasmax$units <- "C"
      tasmax <- ClimProjDiags::Subset(tasmax, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'tasmin'){
      tasmin <- data - 273.15
      attr(tasmin, 'Variables')$common$tasmin$units <- "C"
      tasmin <- ClimProjDiags::Subset(tasmin, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'prlr'){
      prlr <- data * 3600 * 24 * 30.44 * 1000
      attr(prlr, 'Variables')$common$prlr$units <- "mm"
      prlr <- ClimProjDiags::Subset(prlr, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    }
  }
  
  # Extract summary statistics
  #vars <- list(tas, tasmax, tasmin, prlr)
  vars <- list(tas, tasmax, tasmin)
  #lbs <- c("tas", "tasmax", "tasmin", "prlr")
  lbs <- c("tas", "tasmax", "tasmin")
  dt.out <- NULL
  for(i in 1:length(vars)){
    
    # Extract variable of interest
    var <- vars[[i]]
    
    # Assign latitude and longitude coordinates from data
    lon <- attr(var, "Variables")$dat1$longitude
    lat <- attr(var, "Variables")$dat1$latitude
    
    # Set time variables
    nyears <- end.year - start.year + 1
    nmonths <- 12
    ntimes <- nyears * nmonths
    
    # Transform multidimensional array into list of rasters
    out <- NULL
    for (x in 1:(ntimes)) {
      
      # Calculate i and j from x
      a <- ((x - 1) %/% nmonths) + 1
      b <- ((x - 1) %% nmonths) + 1
      
      m <-  var[b,a,,]
      d <- as.character(attr(var, "Variables")[["common"]][["time"]][[x]])
      
      out[[d]] <- raster::raster(m, 
                                 xmn=min(lon), xmx=max(lon), 
                                 ymn=min(lat), ymx=max(lat))
      crs(out[[d]]) <- "+init=epsg:4326"
      
    }
    
    # Convert list of rasters into stack
    out <- raster::stack(out)
    
    # Extract summary statistics from stack and create dataframe
    dt.out[[i]] <- exact_extract(out, shp, fun="mean") %>% 
      # bind_cols(admin1=shp[["SIGLA_UF"]]) %>%
      bind_cols(admin2=shp[["CD_MUN"]]) %>%
      pivot_longer(all_of(names(.)[-ncol(.)]), 
                   names_to="date", values_to="vals") %>% 
      mutate(date=str_sub(date, start=7, end=16),
             date=as.Date(date, format="%Y.%m.%d"),
             year=year(date),
             month=month(date),
             admin1=states[w]) %>% 
      dplyr::select(year, month, admin1, admin2, vals) %>% 
      rename(!!lbs[i] :=vals) #%>% 
      #add.lags(., lbs[i], "admin2", k=1:4)
  }
  
  era5land_data[[w]] <- dt.out %>% reduce(left_join)
  
}

# combine era5land data
tas_data <- data.table::rbindlist(era5land_data)


# chirps ------------------------------------------------------------------

# select variable:
variables <- 'prlr'
dataset <- 'chirps' # options: 'era5land', 'era5', 'seas51', 'chirps' 

chirps_data <- list()
for(w in 1:length(states)){
  
  shp <- filter(br_shp, SIGLA_UF == states[w])
  
  # shapefile bounding box
  lons.min <- st_bbox(shp)["xmin"] - 1
  lons.max <- st_bbox(shp)["xmax"] + 1
  lats.min <- st_bbox(shp)["ymin"] - 1
  lats.max <- st_bbox(shp)["ymax"] + 1
  
  for(var in variables){
    # obtain path:
    if (dataset == 'era5land'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr' | var == 'tas' | var == 'tdps'){'f1h'} else {0}
      path_dataset <- paste0('/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_', ff, '/$var$_$sdate$.nc')
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    } else if (dataset == 'era5'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr' | var == 'tas' | var == 'tdps'){'f1h'} else {0}
      path_dataset <- paste0('/esarchive/recon/ecmwf/era5/monthly_mean/$var$_', ff, '-r1440x721cds', '/$var$_$sdate$.nc')
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    }else if (dataset == 'seas51'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr'){'s0-24h'} else if (var == 'tas' | var == 'tdps'){'f6h'} else {0}
      path_dataset <- paste0('/esarchive/exp/ecmwf/system51c3s/monthly_mean/$var$_', ff, '/$var$_$sdate$.nc')
      selected_dates <- dates_seasonalforecast
      forecast <- TRUE
    } else if (dataset == 'chirps'){
      path_dataset <- '/esarchive/obs/ucsb/chirps-v2/monthly_mean/$var$/$var$_$sdate$.nc'
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    } else {
      stop('unknown dataset')
    }
    
    # load data:
    if (forecast){
      rm(data)
      data <- Start(dat = path_dataset,
                    var = var,
                    sdate = selected_dates,
                    time = 1:6,
                    ensemble = 'all',
                    split_multiselected_dims = TRUE,
                    latitude = startR::values(list(lats.min, lats.max)),
                    latitude_reorder = Sort(decreasing = TRUE),
                    longitude = startR::values(list(lons.min, lons.max)),
                    longitude_reorder = CircularSort(-180, 180),
                    synonims = list(latitude = c('lat', 'latitude'),
                                    longitude = c('lon', 'longitude')),
                    return_vars = list(latitude = 'dat',
                                       longitude = 'dat',
                                       time = c('sdate')),
                    retrieve = TRUE)
    } else {
      rm(data)
      data <- Start(dat = path_dataset,
                    var = var,
                    sdate = selected_dates,
                    split_multiselected_dims = TRUE,
                    latitude = startR::values(list(lats.min, lats.max)),
                    latitude_reorder = Sort(decreasing = TRUE),
                    longitude = startR::values(list(lons.min, lons.max)),
                    longitude_reorder = CircularSort(-180, 180),
                    synonims = list(latitude = c('lat', 'latitude'),
                                    longitude = c('lon', 'longitude')),
                    return_vars = list(latitude = 'dat',
                                       longitude = 'dat',
                                       time = c('sdate')),
                    retrieve = TRUE)
    }
    
    # transform units
    if (var =='tas'){
      tas <- data - 273.15
      attr(tas, 'Variables')$common$tas$units <- "C"
      tas <- ClimProjDiags::Subset(tas, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'tasmax'){
      tasmax <- data - 273.15
      attr(tasmax, 'Variables')$common$tasmax$units <- "C"
      tasmax <- ClimProjDiags::Subset(tasmax, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'tasmin'){
      tasmin <- data - 273.15
      attr(tasmin, 'Variables')$common$tasmin$units <- "C"
      tasmin <- ClimProjDiags::Subset(tasmin, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'prlr'){
      prlr <- data * 3600 * 24 * 30.44 * 1000
      attr(prlr, 'Variables')$common$prlr$units <- "mm"
      prlr <- ClimProjDiags::Subset(prlr, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    }
  }
  
  # Extract summary statistics
  #vars <- list(tas, tasmax, tasmin, prlr)
  vars <- list(prlr)
  #lbs <- c("tas", "tasmax", "tasmin", "prlr")
  lbs <- c("prlr")
  dt.out <- NULL
  for(i in 1:length(vars)){
    
    # Extract variable of interest
    var <- vars[[i]]
    
    # Assign latitude and longitude coordinates from data
    lon <- attr(var, "Variables")$dat1$longitude
    lat <- attr(var, "Variables")$dat1$latitude
    
    # Set time variables
    nyears <- end.year - start.year + 1
    nmonths <- 12
    ntimes <- nyears * nmonths
    
    # Transform multidimensional array into list of rasters
    out <- NULL
    for (x in 1:(ntimes)) {
      
      # Calculate i and j from x
      a <- ((x - 1) %/% nmonths) + 1
      b <- ((x - 1) %% nmonths) + 1
      
      m <-  var[b,a,,]
      d <- as.character(attr(var, "Variables")[["common"]][["time"]][[x]])
      
      out[[d]] <- raster::raster(m, 
                                 xmn=min(lon), xmx=max(lon), 
                                 ymn=min(lat), ymx=max(lat))
      crs(out[[d]]) <- "+init=epsg:4326"
      
    }
    
    # Convert list of rasters into stack
    out <- raster::stack(out)
    
    # Extract summary statistics from stack and create dataframe
    dt.out[[i]] <- exact_extract(out, shp, fun="mean") %>% 
      # bind_cols(admin1=shp[["SIGLA_UF"]]) %>%
      bind_cols(admin2=shp[["CD_MUN"]]) %>%
      pivot_longer(all_of(names(.)[-ncol(.)]), 
                   names_to="date", values_to="vals") %>% 
      mutate(date=str_sub(date, start=7, end=16),
             date=as.Date(date, format="%Y.%m.%d"),
             year=year(date),
             month=month(date),
             admin1=states[w]) %>% 
      dplyr::select(year, month, admin1, admin2, vals) %>% 
      rename(!!lbs[i] :=vals) #%>% 
    #add.lags(., lbs[i], "admin2", k=1:4)
  }
  
  chirps_data[[w]] <- dt.out %>% reduce(left_join)
  
}

# combine chirps data
prlr_data <- data.table::rbindlist(chirps_data)


# join tas and prlr -------------------------------------------------------
if(dir.exists("./output") == FALSE){
  dir.create("./output")  
}

all_states_data <- tas_data %>% 
  left_join(prlr_data, by = c("year", "month", "admin1", "admin2"))

# save output table
write_csv(all_states_data, "./output/03_climate_data.csv")

# Checking NAs in municipalities
table(is.na(all_states_data$tas))

missing_mun <- all_states_data %>%
  filter(is.na(tas)) %>%
  select(admin2) %>%
  distinct() %>% 
  pull()

# There was no overlapping ERA5-land grid cells in the following municipalities:
# "2916104" Itaparica BA
# "2919926" Madre de Deus BA
# "2109452" Raposa MA  
# "2503209" Cabedelo PB
# "4310330" Imbé RS
# For CHIRPS data was ok.

# try running again for each of these municipalities
# by adding a 10 km buffer to each municipality

# select variable:
variables <- c('tas', 'tasmax', 'tasmin')
dataset <- 'era5land' # options: 'era5land', 'era5', 'seas51', 'chirps' 

remaining_data <- list()
for(w in 1:length(missing_mun)){
  
  shp <- filter(br_shp, CD_MUN == missing_mun[w])
  shp <- st_buffer(shp, dist = 10000)
  
  # shapefile bounding box
  lons.min <- st_bbox(shp)["xmin"] - 0.5
  lons.max <- st_bbox(shp)["xmax"] + 0.5
  lats.min <- st_bbox(shp)["ymin"] - 0.5
  lats.max <- st_bbox(shp)["ymax"] + 0.5
  
  for(var in variables){
    # obtain path:
    if (dataset == 'era5land'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr' | var == 'tas' | var == 'tdps'){'f1h'} else {0}
      path_dataset <- paste0('/esarchive/recon/ecmwf/era5land/monthly_mean/$var$_', ff, '/$var$_$sdate$.nc')
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    } else if (dataset == 'era5'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr' | var == 'tas' | var == 'tdps'){'f1h'} else {0}
      path_dataset <- paste0('/esarchive/recon/ecmwf/era5/monthly_mean/$var$_', ff, '-r1440x721cds', '/$var$_$sdate$.nc')
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    }else if (dataset == 'seas51'){
      ff <- if (var == 'tasmax' | var == 'tasmin'){'f24h'} else if (var == 'prlr'){'s0-24h'} else if (var == 'tas' | var == 'tdps'){'f6h'} else {0}
      path_dataset <- paste0('/esarchive/exp/ecmwf/system51c3s/monthly_mean/$var$_', ff, '/$var$_$sdate$.nc')
      selected_dates <- dates_seasonalforecast
      forecast <- TRUE
    } else if (dataset == 'chirps'){
      path_dataset <- '/esarchive/obs/ucsb/chirps-v2/monthly_mean/$var$/$var$_$sdate$.nc'
      selected_dates <- dates_reanalysis
      forecast <- FALSE
    } else {
      stop('unknown dataset')
    }
    
    # load data:
    if (forecast){
      rm(data)
      data <- Start(dat = path_dataset,
                    var = var,
                    sdate = selected_dates,
                    time = 1:6,
                    ensemble = 'all',
                    split_multiselected_dims = TRUE,
                    latitude = startR::values(list(lats.min, lats.max)),
                    latitude_reorder = Sort(decreasing = TRUE),
                    longitude = startR::values(list(lons.min, lons.max)),
                    longitude_reorder = CircularSort(-180, 180),
                    synonims = list(latitude = c('lat', 'latitude'),
                                    longitude = c('lon', 'longitude')),
                    return_vars = list(latitude = 'dat',
                                       longitude = 'dat',
                                       time = c('sdate')),
                    retrieve = TRUE)
    } else {
      rm(data)
      data <- Start(dat = path_dataset,
                    var = var,
                    sdate = selected_dates,
                    split_multiselected_dims = TRUE,
                    latitude = startR::values(list(lats.min, lats.max)),
                    latitude_reorder = Sort(decreasing = TRUE),
                    longitude = startR::values(list(lons.min, lons.max)),
                    longitude_reorder = CircularSort(-180, 180),
                    synonims = list(latitude = c('lat', 'latitude'),
                                    longitude = c('lon', 'longitude')),
                    return_vars = list(latitude = 'dat',
                                       longitude = 'dat',
                                       time = c('sdate')),
                    retrieve = TRUE)
    }
    
    # transform units
    if (var =='tas'){
      tas <- data - 273.15
      attr(tas, 'Variables')$common$tas$units <- "C"
      tas <- ClimProjDiags::Subset(tas, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'tasmax'){
      tasmax <- data - 273.15
      attr(tasmax, 'Variables')$common$tasmax$units <- "C"
      tasmax <- ClimProjDiags::Subset(tasmax, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'tasmin'){
      tasmin <- data - 273.15
      attr(tasmin, 'Variables')$common$tasmin$units <- "C"
      tasmin <- ClimProjDiags::Subset(tasmin, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    } else if (var == 'prlr'){
      prlr <- data * 3600 * 24 * 30.44 * 1000
      attr(prlr, 'Variables')$common$prlr$units <- "mm"
      prlr <- ClimProjDiags::Subset(prlr, along = c('dat', 'var'), indices = list(1,1), drop = 'selected')
    }
  }
  
  # Extract summary statistics
  vars <- list(tas, tasmax, tasmin)
  lbs <- c("tas", "tasmax", "tasmin")
  dt.out <- NULL
  for(i in 1:length(vars)){
    
    # Extract variable of interest
    var <- vars[[i]]
    
    # Assign latitude and longitude coordinates from data
    lon <- attr(var, "Variables")$dat1$longitude
    lat <- attr(var, "Variables")$dat1$latitude
    
    # Set time variables
    nyears <- end.year - start.year + 1
    nmonths <- 12
    ntimes <- nyears * nmonths
    
    # Transform multidimensional array into list of rasters
    out <- NULL
    for (x in 1:(ntimes)) {
      
      # Calculate i and j from x
      a <- ((x - 1) %/% nmonths) + 1
      b <- ((x - 1) %% nmonths) + 1
      
      m <-  var[b,a,,]
      d <- as.character(attr(var, "Variables")[["common"]][["time"]][[x]])
      
      out[[d]] <- raster::raster(m, 
                                 xmn=min(lon), xmx=max(lon), 
                                 ymn=min(lat), ymx=max(lat))
      crs(out[[d]]) <- "+init=epsg:4326"
      
    }
    
    # Convert list of rasters into stack
    out <- raster::stack(out)
    
    # Extract summary statistics from stack and create dataframe
    dt.out[[i]] <- exact_extract(out, shp, fun="mean") %>% 
      bind_cols(admin1=shp[["SIGLA_UF"]]) %>%
      bind_cols(admin2=shp[["CD_MUN"]]) %>%
      pivot_longer(all_of(names(.)[-ncol(.)]), 
                   names_to="date", values_to="vals") %>% 
      mutate(date=str_sub(date, start=7, end=16),
             date=as.Date(date, format="%Y.%m.%d"),
             year=year(date),
             month=month(date)) %>% 
      dplyr::select(year, month, admin1, admin2, vals) %>% 
      rename(!!lbs[i] :=vals) #%>% 
    #add.lags(., lbs[i], "admin2", k=1:4)
  }
  
  remaining_data[[w]] <- dt.out %>% reduce(left_join)
  
}

# combine era5land data for remaining municipalities
tas_data2 <- data.table::rbindlist(remaining_data)

# merge remaining data into full dataset
final <- tas_data %>%
  filter(is.na(tas) == FALSE) %>% 
  bind_rows(tas_data2) %>% 
  arrange(year, month, admin2) %>% 
  left_join(prlr_data, by = c("year", "month", "admin1", "admin2"))

glimpse(final)
table(is.na(final))
# all good!

# save output table
write_csv(final, "./output/03_climate_data.csv")

# sample for visual check in QGIS
# sample <- final %>%
#   filter(year == 2021,
#          month == 12) %>%
#   select(admin1, admin2, tas, tasmax, tasmin, prlr)
# 
# write_csv(sample, "./output/03_temp_climate_sample.csv")

# found an additional error:
# tasmax and tasmin values for month 12/2021 are the same

test <- final %>% 
  mutate(diff = tasmax-tasmin) %>% 
  filter(diff == 0)

View(test)
