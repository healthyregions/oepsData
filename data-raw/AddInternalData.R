### Author: Ashlynn Wimer (@bucketteOfIvy)
### Date: 8/22/2024
### About: Load data dictionaries, FIPS reference tables into R/sysdata.rda.

library(purrr)
library(readxl)
library(dplyr)
library(sf)

#' Concatenate two string lists pairwise when both elements are present
#'
#' Given two lists of strings A and B of equal length, paste together 
#' every element of index i in A with every element of index i in B, using
#' some seperator, iff they both exist. If one element is NA, attempt
#' to use whichever is not NA, and if they are both NA, return for that element.
#' 
#' @param A a list of strings
#' @param B a list of strings
#' @param sep the seperator for pasting. Defaults ''
#' 
#' @returns A list of length `length(a)` which contains the pairwise pasted strings.
combine_if_present <- function(A, B, sep=", ") {
  # this is not a very paradigmatic solution for R
  # but it works soooo
  return_list <- 1:length(A)
  for (row in 1:length(A)) {
    if (is.na(A[row]) & is.na(B[row])) {
      return_list[row] <- NA
    } else if (!is.na(B[row]) & !is.na(A[row])) {
      return_list[row] <- paste(A[row], B[row], sep=sep)
    } else if (!is.na(A[row])) {
      return_list[row] <- B[row]
    } else {
      return_list[row] <- B[row]
    }
  }
  return(return_list)
}


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
tables[[2]] <- rename(tables[[2]])
tables[[3]] <- select(tables[[3]], "Analysis", everything())
tables[[4]] <- mutate(tables[[4]], '1980'=NA, '1990'=NA, '2000'=NA, '2010'=NA)

data_dictionary <- bind_rows(tables)

data_dictionary['Latest'] <- ifelse(data_dictionary['Latest'] == 'x', '2018', '')
data_dictionary['2010'] <- ifelse(data_dictionary['2010'] == 'x', '2010', '')
data_dictionary['2000'] <- ifelse(data_dictionary['2000'] == 'x', '2000', '')
data_dictionary['1990'] <- ifelse(data_dictionary['1990'] == 'x', '1990', '')
data_dictionary['1980'] <- ifelse(data_dictionary['1980'] == 'x', '1980', '')
data_dictionary["Year"] <- combine_if_present(data_dictionary[['Latest']], data_dictionary[['2010']])
data_dictionary["Year"] <- combine_if_present(data_dictionary[["Year"]], data_dictionary[["2000"]])
data_dictionary["Year"] <- combine_if_present(data_dictionary[["Year"]], data_dictionary[["1990"]])
data_dictionary["Year"] <- combine_if_present(data_dictionary[["Year"]], data_dictionary[["1980"]])

data_dictionary <- select(data_dictionary, 'Variable', "Release Year"='Year', 'Theme', everything())
data_dictionary <- select(data_dictionary, -'1980', -'1990', -'2000', -'2010', -'Latest')

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