#' @export 
load_oeps <- function(scale, year, geometry=FALSE) {

  stopifnot(
    grepl('state|tract|county|zcta|counties', scale, ignore.case=T),
    grepl('1980|1990|2000|2010|latest', year, ignore.case=T)
  )
  
  attribute_data <- eval(parse(text=make_file_name(scale, year)))
  
  if (!geometry) {
    return(attribute_data)
  }
  
  geometry <- retrieve_geometry(scale, quiet=TRUE)
  
  data <- merge(attribute_data, geometry, on='HEROP_ID')
  
  return(data)
}

make_file_name <- function(scale, year) {
  scale <- toupper(substr(scale, 1, 1))
  year <- paste0(toupper(substr(year, 1, 1)), substr(year, 2, 7))
  return(paste(scale, year, sep="_"))
}

retrieve_geometry <- function(scale, quiet=FALSE) {
  shape_url <- '/vsicurl/https://raw.githubusercontent.com/GeoDaCenter/opioid-policy-scan/main/data_final/geometryFiles/'

  if (grepl('state', scale)) {
    shape_url <- paste0(shape_url, 'state/states2010.shp')
  }
  
  if (grepl('tract', scale)) {
    shape_url <- paste0(shape_url, 'tract/tracts2010.shp')
  }
  
  if (grepl('county|counties', scale)) {
    shape_url <- paste0(shape_url, 'county/counties2010.shp')
  }
  
  if (grepl('zcta', scale)) {
    shape_url <- paste0(shape_url, 'zcta/zctas2010.shp')
  }
  
  return(st_read(shape_url, quiet=quiet))
}