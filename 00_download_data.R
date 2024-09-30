# -------------------------------------------------------------------------
#
# Script name: 00_download_data.R
#
# Purpose of script: Download data direcly from source
# Author(s): Bruno M. Carvalho
# Date Created: 2023-08-23
# Email: bruno.carvalho@bsc.es
#
# -------------------------------------------------------------------------


# Load packages
#library(tidyverse) # review later
library(KrigR)

# packages <- c()
# install.packages(setdiff(packages, rownames(installed.packages())))
# lapply(packages, require, character.only = TRUE)

# increase max download timeout
options(timeout = max(1000, getOption("timeout")))


# Base shapefiles ---------------------------------------------------------
if(dir.exists("./data/shp") == FALSE){
  dir.create("./data/shp")  
}

# URL for 2022 Brazil municipality shapefile from IBGE
shp_url <- paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/",
             "malhas_territoriais/malhas_municipais/municipio_2022/",
             "Brasil/BR/BR_Municipios_2022.zip")

# URL for 2022 Brazil outline shapefile from IBGE
br_url <- paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/",
                  "malhas_territoriais/malhas_municipais/municipio_2022/",
                  "Brasil/BR/BR_Pais_2022.zip")

# download zip files
download.file(shp_url, destfile = "./data/temp_br.zip", 
              mode = "wb", method = "libcurl")

download.file(br_url, destfile = "./data/temp_br2.zip", 
              mode = "wb", method = "libcurl")

# unzip file and save shapefile
unzip(zipfile = "./data/temp_br.zip", exdir = "./data/shp")
unzip(zipfile = "./data/temp_br2.zip", exdir = "./data/shp")

# delete temporary zip file
file.remove("./data/temp_br.zip")
file.remove("./data/temp_br2.zip")


# Epidemiological data ----------------------------------------------------
if(dir.exists("./data/den") == FALSE){
  dir.create("./data/den")  
}

# create vectors of URLs and file paths
den_url <- sprintf(paste0("ftp://ftp.datasus.gov.br/dissemin/",
                          "publicos/SINAN/DADOS/FINAIS/",
                          "DENGBR%02d.dbc"), 1:21)
den_fp <- sprintf("./data/den/DENGBR%02d.DBC", 1:21)

# WARNING: you are about to download very large files (~ 840 MB)
# Split into smaller vectors if necessary
# den_url1 = den_url[1:10]
# den_fp1 = den_fp[1:10]
den_url1 = den_url[11:21]
den_fp1 = den_fp[11:21]
download.file(den_url1, destfile = den_fp1, mode = "wb", method = "libcurl")


# Socioeconomic data ------------------------------------------------------
if(dir.exists("./data/pop") == FALSE){
  dir.create("./data/pop")  
}

pop_url <- sprintf("ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPTCU/POPTBR%02d.zip", 1:21)
pop_fp <- sprintf("./data/pop/temp/POPTBR%02d.zip", 1:21)

# download files
download.file(pop_url, destfile = pop_fp, mode = "wb", method = "libcurl")

# unzip
lapply(pop_fp, unzip, exdir = "./data/pop/temp")
unzip(zipfile = "./data/temp_br.zip", exdir = "./data/shp")

# copy relevant files
pop_fn <- list.files(path = "./data/pop/temp", pattern = ".dbf|.DBF")

file.copy(from = paste0("./data/pop/temp/", pop_fn),
          to = paste0("./data/pop/", pop_fn))

# delete temporary files
unlink("./data/pop/temp/",recursive=TRUE)


# Climate data ------------------------------------------------------------
if(dir.exists("./data/clim") == FALSE){
  dir.create("./data/clim")  
}

# CDS credentials
cds <- read.csv("./data/CDS_credentials.csv", header = TRUE)
API_user <- cds$API_user
API_key <- cds$API_key

# directory to save download files
clim_fp <- "./data/clim"

# set coordinate limits of Brazil
# (previously extracted from shapefile)
# lim <- c(-73.99045, -28.84764, -33.75118, 5.271841)

# read shapefile 
br <- sf::st_read("./data/shp/BR_Pais_2022.shp")

# download all
# WARNING
temp <- download_ERA(
  Variable = "2m_temperature",
  DataSet = "era5-land",
  DateStart = "2001-01-01",
  DateStop = "2021-12-31",
  TResolution = "month",
  TStep = 1,
  Extent = extent(br),
  Dir = clim_fp,
  FileName = "2mt",
  API_User = API_user,
  API_Key = API_key
)
