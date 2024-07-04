### Author: Ashlynn Wimer
### Date: 7/4/2024
### About: R Script which does very little.

library(dplyr)
library(readr)
library(here)

state_latest <- read_csv(here::here("data-raw", "S_Latest.csv"))

usethis::use_data(state_latest, overwrite = TRUE)