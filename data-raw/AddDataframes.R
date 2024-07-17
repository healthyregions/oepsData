### Author: Ashlynn Wimer (@bucketteOfIvy)
### Date: 7/15/2024
### About: R script which loads attribute data into the package directly.

library(tidyverse)
library(here)
library(RCurl)
library(usethis)

BASE_URL <- 'https://raw.githubusercontent.com/GeoDaCenter/opioid-policy-scan/main/data_final/full_tables/'

# lists are used so that future maintainers can expand this more easily
# (just be sure to edit the base url as well if needed!)
county <- c('C_Latest', 'C_2010', 
            'C_2000',  'C_1990', 
            'C_1980')

state <- c('S_Latest', 'S_2010',
           'S_2000',   'S_1990',
           'S_1980')

tract <- c('T_Latest', 'T_2010',
           'T_2000',   'T_1990',
           'T_1980')

zcta <- c('Z_Latest')

# Multiple helper functions are defined so that we can just map over our lists
# to retrieve and clean our data.
grabDataFromGithub <- function(file_name, base_url, file_type = 'csv') {
  data <- getURL(paste(base_url, file_name, '.', file_type, sep=''))
  data <- read.csv(text=data)
  return(as_tibble(data))
}

padGeoids <- function(df, size=5) {
  df <- mutate(df, GEOID=str_pad(GEOID, size, 'left', '0'))
  return(df)
}

counties <- county |> 
  map(grabDataFromGithub, base_url=BASE_URL) |>
  map(padGeoids, size=5)

# `use_data` only accepts named objects
C_Latest <- counties[[1]]
C_2010   <- counties[[2]]
C_2000   <- counties[[3]]
C_1990   <- counties[[4]]
C_1980   <- counties[[5]]

use_data(C_Latest, compress='xz') 
use_data(C_2010,   compress='xz')   
use_data(C_2000,   compress='xz')   
use_data(C_1990,   compress='xz')   
use_data(C_1980,   compress='xz')   

states <- state |> 
  map(grabDataFromGithub, base_url=BASE_URL) |>
  map(padGeoids, size=2) 

S_Latest <- states[[1]]
S_2010   <- states[[2]]
S_2000   <- states[[3]]
S_1990   <- states[[4]]
S_1980   <- states[[5]]

use_data(S_Latest, compress='xz') 
use_data(S_2010,   compress='xz')   
use_data(S_2000,   compress='xz')   
use_data(S_1990,   compress='xz')   
use_data(S_1980,   compress='xz')   

tracts <- tract |> 
  map(grabDataFromGithub, base_url=BASE_URL) |>
  map(padGeoids, size=11)

T_Latest <- tracts[[1]]
T_2010   <- tracts[[2]]
T_2000   <- tracts[[3]]
T_1990   <- tracts[[4]]
T_1980   <- tracts[[5]]

use_data(T_Latest, compress='xz') 
use_data(T_2010,   compress='xz')   
use_data(T_2000,   compress='xz')   
use_data(T_1990,   compress='xz')   
use_data(T_1980,   compress='xz')   

zctas <- zcta |> 
  map(grabDataFromGithub, base_url=BASE_URL) |>
  map(padGeoids, size=5)

Z_Latest <- zctas[[1]]
use_data(Z_Latest, compress='xz')