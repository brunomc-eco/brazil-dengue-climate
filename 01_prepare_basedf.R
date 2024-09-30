# -------------------------------------------------------------------------
#
# Script name: 01_prepare_basedf.R
#
# Purpose of script: Prepare a base data frame to join all variables
# Author(s): Bruno M. Carvalho
# Date Created: 2023-09-20
# Email: bruno.carvalho@bsc.es
#
# -------------------------------------------------------------------------

# Load packages
packages <- c("sf", "dplyr", "tidyr", "readr")
#install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, require, character.only = TRUE)

# -------------------------------------------------------------------------

# Define paths to data ----------------------------------------------------

## from direct download
shp_fp <- "./data/shp/BR_Municipios_2022.shp"

## from esarchive
#shp_fp <- "/esarchive/shapefiles/IBGE/BR_Municipios_2022.shp"

# Prepare base data frame -------------------------------------------------

# define time steps
year <- as.character(seq(2001, 2021, 1))
month <- as.character(seq(1, 12, 1))

# read shapefile
shp <- st_read(shp_fp)

# clean shapefile for data frame
spatial_df <- shp %>% 
  st_drop_geometry() %>%
  # remove lagoons and Noronha island
  filter(CD_MUN != "4300001" & CD_MUN != "4300002" & CD_MUN != "2605459") %>% 
  # fix variables
  mutate(admin2_id = as.character(CD_MUN),
         admin2 = as.character(NM_MUN),
         admin1 = as.character(SIGLA_UF)) %>% 
  select(admin2_id, admin2, admin1)

# base df
df <- expand_grid(admin2_id = spatial_df$admin2_id, year, month) %>% 
  left_join(spatial_df, by = "admin2_id") %>% 
  select(admin2_id, admin2, admin1, year, month)


# Save output table -------------------------------------------------------

write_csv(df, "./output/01_basedf.csv")
