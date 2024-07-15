#' @export 
load_oeps <- function(year, scale, geography=FALSE) {

  validate_request(year, scale)
  
  return(eval(parse(text=make_file_name(year, scale))))
}

make_file_name <- function(year, scale) {
  geog_letter <- toupper(substr(scale, 1, 1))
  year_tag <- paste0(toupper(substr(year, 1, 1)), substr(year, 2, 7))
  return(paste(geog_letter, year_tag, sep="_"))
}

validate_request <- function(year, scale) {
  valid_years = c("1980", "1990", "2000", "2010", "latest")
  valid_scales = c("county", "state", "zcta'", "tract")
  
  stopifnot(
    tolower(as.character(year)) %in% valid_years,
    tolower(scale) %in% valid_scales
  )
}