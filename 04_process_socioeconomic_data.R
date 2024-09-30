# -------------------------------------------------------------------------
#
# Script name: 04_process_socioeconomic_data.R
#
# Purpose of script: Pre-process socioeconomic data
# Author(s): Bruno M. Carvalho
# Date Created: 2023-08-29
# Email: bruno.carvalho@bsc.es
#
# -------------------------------------------------------------------------

# Load packages

packages <- c("foreign", "data.table", "dplyr", "stringr", "readr")
#install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, require, character.only = TRUE)


# Define paths to data ----------------------------------------------------

## from direct download
pop_fp <- "./data/pop"


# Read and process data ---------------------------------------------------

filenames <- list.files(path = pop_fp, full.names = TRUE)

pops <- lapply(filenames, read.dbf)

# fixing number of columns in 2018 data
pops[[18]] <- select(pops[[18]], 1:3) 

pop <- rbindlist(pops) %>% 
  mutate(admin2_id = as.character(MUNIC_RES),
         admin2_id_sinan = str_sub(admin2_id, end = 6),
         year = as.character(ANO),
         pop = POPULACAO) %>% 
  select(admin2_id_sinan, year, pop) %>% 
  arrange(admin2_id_sinan, year)


# Save output -------------------------------------------------------------

write_csv(pop, "./output/04_pop_data.csv")
