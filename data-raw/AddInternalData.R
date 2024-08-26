### Author: Ashlynn Wimer (@bucketteOfIvy)
### Date: 8/22/2024
### About: Load data dictionaries, FIPS reference tables into R/sysdata.rda.

library(purrr)
library(readxl)
library(dplyr)
library(sf)

base_url <- 'https://raw.githubusercontent.com/GeoDaCenter/opioid-policy-scan/main/data_final/dictionaries/'
file_names <- c('C_Dict.xlsx', 'S_Dict.xlsx', 'T_Dict.xlsx', 'Z_Dict.xlsx')

temps <- map(file_names, tempfile, fileext='.xlsx')

file_names |> 
  map(~ paste0(base_url, .x)) |>
  walk2(.y=temps, ~ download.file(url=.x, destfile=.y, mode='wb'))

tables <- map(temps, read_xlsx)

# column which should only be used internally
tables[[1]]['scale'] <- 'county'
tables[[2]]['scale'] <- 'state'
tables[[3]]['scale'] <- 'tract'
tables[[4]]['scale'] <- 'zcta'

# clean tables to safely bind_rows
tables[[2]] <- rename(tables[[2]], "OEPSv1"="OEPS v1 Table")
tables[[3]] <- select(tables[[3]], "Analysis", everything())
tables[[4]] <- mutate(tables[[4]], '1980'=NA, '1990'=NA, '2000'=NA, '2010'=NA)

data_dictionary <- bind_rows(tables)

temps |> walk(unlink)

## Add FIPS crosswalk

county_id_table <- tigris::counties() |>
  select(STATEFP, COUNTYFP, GEOID, NAME) |>
  mutate(NAME=tolower(NAME)) |>
  st_drop_geometry()

states_id_table <- tigris::states() |>
  select(STATEFP, NAME, ABBR=STUSPS) |>
  mutate(NAME=tolower(NAME), ABBR=tolower(ABBR)) |>
  st_drop_geometry()

usethis::use_data(data_dictionary, county_id_table, states_id_table, internal=TRUE, overwrite=TRUE)