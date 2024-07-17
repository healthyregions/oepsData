#' @export
oeps_dictionary <- function(scale) {
  stopifnot(grepl("state|county|counties|tract|zcta", scale, ignore.case=TRUE))

  if (grepl('state', scale)) {
    return_data <- data_dictionary |>
      filter(scale=='state') |>
      select(-scale)
  }
  
  if (grepl('county|counties', scale)) {
    return_data <- data_dictionary |>
      filter(scale=='county') |>
      select(-scale)
  }
  
  if (grepl('tract', scale)) {
    return_data <- data_dictionary |>
      filter(scale=='tract') |>
      select(-scale)
  }
  
  if (grepl('zcta', scale)) {
    return_data <- data_dictionary |>
      filter(scale=='zcta') |>
      select(-scale)
  }
  
  return(return_data)
  
}