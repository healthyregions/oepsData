### Author: Ashlynn Wimer (@bucketteOfIvy)
### Date: 7/17/2024
### About: Load finalized data dictionaries into R/sysdata.rda.

library(purrr)
library(readxl)
library(dplyr)
library(usethis)

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
tables[[2]] <- rename(tables[[2]], "OEPSv1"="OEPS V1 Table")
tables[[3]] <- select(tables[[3]], "Metadata Location"="...11", -"Analysis", everything())
tables[[4]] <- mutate(tables[[4]], '1980'=NA, '1990'=NA, '2000'=NA, '2010'=NA)

data_dictionary <- bind_rows(tables)

usethis::use_data(data_dictionary, internal=TRUE)

temps |> walk(unlink)