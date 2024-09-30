# -------------------------------------------------------------------------
#
# Script name: 05_harmonize_dataset.R
#
# Purpose of script: Merge and harmonize data from previous scripts
# Author(s): Bruno M. Carvalho
# Date Created: 2023-09-20
# Email: bruno.carvalho@bsc.es
#
# -------------------------------------------------------------------------

# Load packages
packages <- c("readr", "dplyr", "stringr", "tidyr")
#install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, require, character.only = TRUE)

# -------------------------------------------------------------------------

# Load data

base <- read_csv("./output/01_basedf.csv")
den <- read_csv("./output/02_dengue_data.csv")
clim <- read_csv("./output/03_climate_data.csv")
pop <- read_csv("./output/04_pop_data.csv")


# Merge and clean data ----------------------------------------------------

df <- base %>%
  # create admin2 id with 6 digits for datasus data
  mutate(admin2_id_sinan = as.numeric(str_sub(admin2_id, end = 6))) %>% 
  # merge dengue data
  left_join(den, by = c("admin2_id_sinan", "year", "month")) %>%
  # replacing NAs for zeroes in dengue data
  replace_na(list(cases = 0)) %>% 
  # merge pop data
  left_join(pop, by = c("admin2_id_sinan", "year")) %>% 
  # remove admin2_id_sinan column
  select(-admin2_id_sinan) %>% 
  # merge climate data
  left_join(clim, by = join_by("admin2_id" == "admin2", 
                               "admin1", "year", "month"))


# Check for errors and data completeness ----------------------------------

# Unique number of municipalities
## Should be 5569 (5570 municipalities except Fernando de Noronha island)
df %>% 
  select(admin2_id) %>% 
  distinct() %>% 
  nrow()

# Checking length of admin2_id (all must have 7 digits)
## This is needed because of the multiple conversions from 6 to 7-digit formats
df %>% 
  select(admin2_id) %>% 
  distinct() %>% 
  pull() %>% 
  str_length() %>% 
  table()

# missing data in dengue cases (should be 0)
table(is.na(df$cases))

# Difference between total dengue cases in original and harmonized data
## These will be different, because the original data from SINAN contains
## unspecified municipalities and cases registered only at the state (admin1)
## level, which were excluded from the harmonized dataset
sum(den$cases)
sum(df$cases)

## Which admin2_ids from sinan dengue data are not on the harmonized dataset?
den %>% 
  anti_join(mutate(df, admin2_id_sinan = as.numeric(str_sub(admin2_id, end = 6))), 
            by = "admin2_id_sinan") %>%
  select(admin2_id_sinan) %>% 
  distinct() %>% 
  pull()

## How many dengue cases from sinan are not on the harmonized dataset?
den %>% 
  anti_join(mutate(df, admin2_id_sinan = as.numeric(str_sub(admin2_id, end = 6))), 
            by = "admin2_id_sinan") %>%
  pull(cases) %>% 
  sum()

# summary of dengue data (detect awkward values)
summary(df$cases)

# missing data in population - some are present
table(is.na(df$pop))

# Which municipalities have no pop data? Do they have dengue case records?
df %>% 
  filter(is.na(pop)) %>% 
  select(admin2, admin1, cases) %>% 
  distinct()

# summary of pop data (detect awkward values)
summary(df$pop)

# missing data in climate variables
table(is.na(df$tas))
table(is.na(df$tasmax))
table(is.na(df$tasmin))
table(is.na(df$prlr))

# summaries of climate variables (detect awkward values)
summary(df$tas)
summary(df$tasmax)
summary(df$tasmin)
summary(df$prlr)


# Small dataset for testing -----------------------------------------------
# Creating a small dataset to test other modelling scripts
# Only RR state, which has only 15 municipalities
# Only 2010 year, all months

df_test <- df %>% 
  filter(admin1 == "RR", year == 2010)


# Save output -------------------------------------------------------------

# save full df
#write_csv(df, "./output/05_harmonized_df.csv")

# save test df
write_csv(df_test, "./output/05_harmonized_df_test.csv")
