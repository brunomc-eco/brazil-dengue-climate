# -------------------------------------------------------------------------
#
# Script name: 02_process_epi_data.R
#
# Purpose of script: Pre-process epidemiological data
# Author(s): Bruno M. Carvalho
# Date Created: 2023-08-29
# Email: bruno.carvalho@bsc.es
#
# -------------------------------------------------------------------------

# Load packages

packages <- c("read.dbc", "dplyr", "data.table", 
              "lubridate", "stringr", "tidyr", "readr")
# install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, require, character.only = TRUE)


# Define paths to data ----------------------------------------------------

## from direct download
den_fp <- "./data/den"
  
## from esarchive
#den_fp <- "/esarchive/obs/datasus/sinan/original_files/DENGBR/FTP"


# Dengue data 2001-2006 ---------------------------------------------------
# essential variables from each file for this period
vars <- c("CON_CLASSI", # confirmed dengue case
          "CON_CRITER", # criteria for confirmation (includes under investigation)
          "ID_MN_RESI", # municipality of residence
          "DT_NOTIFIC") # date of notification

filenames <- list.files(path = den_fp, full.names = TRUE)[1:6]

den1 <- list()
for(i in 1:length(filenames)){
  temp <- read.dbc(filenames[i])
  den1[[i]] <- dplyr::select(temp, all_of(vars))
  rm(temp)
}

# clean data
# need more info about the data (CON_CRITER), come back later
deng1 <- rbindlist(den1) %>%
  filter(CON_CLASSI %in% c(1, 2, 3, 4)) %>% # only confirmed cases
  #filter(CON_CRITER %in% c(1, 2)) %>% # confirmation criteria, review later
  mutate(admin2_id = as.character(ID_MN_RESI),
         admin2_id_sinan = str_sub(admin2_id, end = 6), # not all admin2_id have 7 digits
         year = as.character(year(DT_NOTIFIC)),
         month = as.character(month(DT_NOTIFIC))) %>%
  drop_na(admin2_id) %>% # removing records without municipality id
  group_by(admin2_id_sinan, year, month) %>%
  summarise(cases = n())


# Dengue data 2007-2021 ---------------------------------------------------
# essential variables from each file for this period
vars <- c("CLASSI_FIN", # confirmed dengue case
          "CRITERIO", # criteria for confirmation (includes under investigation)
          "ID_MN_RESI", # municipality of residence
          "DT_NOTIFIC") # date of notification

filenames <- list.files(path = den_fp, full.names = TRUE)[7:21]
# The file for 2008 is not reading properly, 
# so skipping this year entirely for now
filenames <- filenames[-2]

den2 <- list()
for(i in 1:length(filenames)){
  temp <- read.dbc(filenames[i])
  den2[[i]] <- dplyr::select(temp, all_of(vars))
  rm(temp)
}

deng2 <- data.table::rbindlist(den2) %>%
  filter(CLASSI_FIN %in% c(1, 2, 3, 4, 10, 11, 12)) %>% # only confirmed cases
  #filter(CRITERIO %in% c(1, 2)) %>% # confirmation criteria, review later
  mutate(admin2_id = as.character(ID_MN_RESI),
         admin2_id_sinan = str_sub(admin2_id, end = 6),
         year = as.character(year(DT_NOTIFIC)),
         month = as.character(month(DT_NOTIFIC))) %>%
  drop_na(admin2_id) %>% # removing records without municipality id
  group_by(admin2_id_sinan, year, month) %>%
  summarise(cases = n())


# Merge data and save output ----------------------------------------------

den_full <- bind_rows(deng1, deng2) %>% 
  arrange(admin2_id_sinan, year, month, cases)

# save output table
write_csv(den_full, "./output/02_dengue_data.csv")
